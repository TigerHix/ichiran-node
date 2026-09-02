import type { SplitDeclaration } from './analyzer-support-annotation-model.js';
import { SEGMENT_SPLIT_DECLARATIONS } from './analyzer-support-segsplit-declarations.js';
import { addLegacySplitDeclarationsA } from './analyzer-support-split-declarations-a.js';
import { addLegacySplitDeclarationsB } from './analyzer-support-split-declarations-b.js';

function uniqueDeclarations(
  values: readonly SplitDeclaration[],
  label: string
): readonly SplitDeclaration[] {
  const output = new Map<number, SplitDeclaration>();
  for (const value of values) {
    if (output.has(value.seq)) throw new Error(`Duplicate ${label} declaration ${value.seq}`);
    output.set(value.seq, value);
  }
  return [...output.values()];
}

const legacy: SplitDeclaration[] = [];
addLegacySplitDeclarationsA(value => legacy.push(value));
addLegacySplitDeclarationsB(value => legacy.push(value));

/** The exact 172 legacy split definitions active in the qualified reference. */
export const LEGACY_SPLIT_DECLARATIONS = uniqueDeclarations(legacy, 'legacy split');

/**
 * Reviewed behavior additions in ichiran ea958336 on top of the frozen
 * reference declarations. They are ordinary source declarations here.
 */
export const UPSTREAM_260118_SPLIT_DECLARATIONS: readonly SplitDeclaration[] = [
  {
    seq: 1_774_820,
    score: -5,
    parts: [
      {
        type: 'guard',
        condition: (_length, _text, candidate) => candidate.route === 'kana'
      },
      { type: 'part', seqs: 1_002_980, lengthFn: () => 2 },
      { type: 'part', seqs: 1_277_450, lengthFn: () => null }
    ]
  },
  {
    seq: 1_362_970,
    score: 100,
    parts: [
      { type: 'part', seqs: ['申し', 1_363_090], lengthFn: () => 2, conjP: true },
      { type: 'part', seqs: 1_589_040, lengthFn: () => null, conjP: true }
    ]
  }
];

/** Complete qualified split declaration set, with current overlays replacing by seq. */
export const SPLIT_DECLARATIONS = uniqueDeclarations([
  ...LEGACY_SPLIT_DECLARATIONS.filter(value =>
    !UPSTREAM_260118_SPLIT_DECLARATIONS.some(overlay => overlay.seq === value.seq)),
  ...UPSTREAM_260118_SPLIT_DECLARATIONS
], 'split');

export { SEGMENT_SPLIT_DECLARATIONS };
