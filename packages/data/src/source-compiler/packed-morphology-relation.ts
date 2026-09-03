import { createWriteStream } from 'node:fs';
import { once } from 'node:events';
import {
  MORPHOLOGY_SECTION_ID,
  openMorphology,
  openPack,
  SURFACE_INDEX_SECTION_ID,
  openSurfaceIndex,
  surfaceRoute
} from '@ichiran/core/compiler';
import {
  canonicalConjugationRelationKey,
  packedRelationKey
} from './conjugation-relation-proof.js';

const HEADER_BYTES = 64;
const STATE_BYTES = 8;
const EDGE_BYTES = 4;
const MORPHOLOGY_TERMINAL = 0x8000_0000;
const RELATION_WRITE_ROWS = 4_096;

interface Frame {
  readonly state: number;
  edge: number;
  readonly end: number;
  emitted: boolean;
}

function decodeUtf8(bytes: readonly number[]): string {
  return Buffer.from(bytes).toString('utf8');
}

/** Independently walks every morphology terminal in the packed surface DAG. */
export function* packedMorphologySurfaces(surfaceBytes: Uint8Array): Generator<string> {
  const validated = openSurfaceIndex(surfaceBytes);
  const view = new DataView(surfaceBytes.buffer, surfaceBytes.byteOffset, surfaceBytes.byteLength);
  const stateCount = view.getUint32(16, true);
  const morphologyCount = view.getUint32(32, true);
  const root = view.getUint32(44, true);
  const statesOffset = view.getUint32(48, true);
  const edgesOffset = view.getUint32(52, true);
  if (statesOffset !== HEADER_BYTES || root !== stateCount - 1
    || validated.manifest.morphologyCount !== morphologyCount) {
    throw new Error('Surface index header disagrees with its validated manifest');
  }
  const firstEdge = (state: number): number =>
    view.getUint32(statesOffset + state * STATE_BYTES, true);
  const terminal = (state: number): boolean =>
    (view.getUint32(statesOffset + state * STATE_BYTES + 4, true) & MORPHOLOGY_TERMINAL) !== 0;
  const edgeLabel = (edge: number): number => surfaceBytes[edgesOffset + edge * EDGE_BYTES]!;
  const edgeTarget = (edge: number): number => {
    const at = edgesOffset + edge * EDGE_BYTES + 1;
    return surfaceBytes[at]! | (surfaceBytes[at + 1]! << 8) | (surfaceBytes[at + 2]! << 16);
  };

  const bytes: number[] = [];
  const stack: Frame[] = [{ state: root, edge: firstEdge(root), end: firstEdge(root + 1), emitted: false }];
  let emitted = 0;
  while (stack.length > 0) {
    const frame = stack[stack.length - 1]!;
    if (!frame.emitted) {
      frame.emitted = true;
      if (terminal(frame.state)) {
        emitted++;
        yield decodeUtf8(bytes);
      }
    }
    if (frame.edge < frame.end) {
      const edge = frame.edge++;
      const target = edgeTarget(edge);
      bytes.push(edgeLabel(edge));
      stack.push({ state: target, edge: firstEdge(target), end: firstEdge(target + 1), emitted: false });
    } else {
      stack.pop();
      if (stack.length > 0) bytes.pop();
    }
  }
  if (emitted !== morphologyCount) {
    throw new Error(`Packed surface walk found ${emitted} morphology terminals, expected ${morphologyCount}`);
  }
}

export async function writePackedMorphologyRelation(
  hotPack: Uint8Array,
  destination: string,
  options: {
    readonly surfaceLimit?: number;
    readonly onProgress?: (surfaces: number, candidates: number) => void;
  } = {}
): Promise<{
  readonly surfaces: number;
  readonly candidates: number;
  readonly emptySurfaces: number;
  readonly complete: boolean;
}> {
  const pack = openPack(hotPack);
  const surfaceBytes = pack.getSection(SURFACE_INDEX_SECTION_ID);
  const morphology = openMorphology(pack.getSection(MORPHOLOGY_SECTION_ID));
  const output = createWriteStream(destination, { flags: 'wx' });
  const lines: string[] = [];
  let surfaces = 0;
  let candidates = 0;
  let emptySurfaces = 0;
  let complete = true;
  try {
    for (const surface of packedMorphologySurfaces(surfaceBytes)) {
      if (options.surfaceLimit !== undefined && surfaces >= options.surfaceLimit) {
        complete = false;
        break;
      }
      surfaces++;
      const values = morphology.lookup(surface, surfaceRoute(surface));
      if (values.length === 0) emptySurfaces++;
      for (const candidate of values) {
        lines.push(canonicalConjugationRelationKey(packedRelationKey(candidate)));
        candidates++;
        if (lines.length === RELATION_WRITE_ROWS) {
          const chunk = `${lines.join('\n')}\n`;
          lines.length = 0;
          if (!output.write(chunk)) await once(output, 'drain');
        }
      }
      if (surfaces % 100_000 === 0) options.onProgress?.(surfaces, candidates);
    }
  } finally {
    if (lines.length > 0) {
      const chunk = `${lines.join('\n')}\n`;
      lines.length = 0;
      if (!output.write(chunk)) await once(output, 'drain');
    }
    const closed = once(output, 'close');
    output.end();
    await closed;
  }
  return { surfaces, candidates, emptySurfaces, complete };
}
