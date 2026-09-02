import { expect, test } from 'bun:test';
import { gzipSync } from 'node:zlib';
import postgres from 'postgres';
import {
  buildRootPayload,
  compareRootPayloadText,
  isRootPayloadKanaSurface
} from '../src/browser-pack/root-payload.js';
import { loadRootPayloadSource } from '../src/browser-pack/root-payload-oracle.js';
import { openRootPayload } from '../../core/src/root-payload.js';

const RUN_POSTGRES_TEST = process.env.RUN_ROOT_PAYLOAD_POSTGRES === 'true';

function assertSame(actual: unknown, expected: unknown, label: string): void {
  if (actual !== expected) {
    throw new Error(`${label}: expected ${JSON.stringify(expected)}, got ${JSON.stringify(actual)}`);
  }
}

function assertStringList(actual: readonly string[], expected: readonly string[], label: string): void {
  if (actual.length !== expected.length) {
    throw new Error(`${label}: expected ${expected.length} values, got ${actual.length}`);
  }
  for (let index = 0; index < actual.length; index++) {
    assertSame(actual[index], expected[index], `${label}[${index}]`);
  }
}

function assertBytesEqual(actual: Uint8Array, expected: Uint8Array): void {
  assertSame(actual.byteLength, expected.byteLength, 'deterministic byte length');
  for (let index = 0; index < actual.byteLength; index++) {
    if (actual[index] !== expected[index]) {
      throw new Error(`deterministic rebuild differs at byte ${index}`);
    }
  }
}

test.skipIf(!RUN_POSTGRES_TEST)(
  'root payload exhaustively equals canonical ichiran_test projections',
  async () => {
    const sql = postgres({
      host: process.env.ROOT_PAYLOAD_DATABASE_HOST ?? '/var/run/postgresql',
      database: process.env.ROOT_PAYLOAD_DATABASE_NAME ?? 'ichiran_test',
      user: process.env.ROOT_PAYLOAD_DATABASE_USER ?? 'tiger',
      password: process.env.ROOT_PAYLOAD_DATABASE_PASSWORD ?? '',
      max: 4
    });

    try {
      const started = performance.now();
      const source = await loadRootPayloadSource(sql);
      console.log(`root payload source loaded in ${Math.round(performance.now() - started)}ms`);

      // Core issues this query without ORDER BY and prepends every returned
      // row. On the pinned PostgreSQL snapshot, btree, bitmap-heap, and
      // sequential plans all visit equal text keys by heap tuple order; the
      // semantic projection freezes the observable reverse order, never CTID.
      const witnessText = '心の音は風のように的を射る';
      const witnessKeys = new Set<string>();
      for (let start = 0; start < witnessText.length; start++) {
        for (let end = start + 1; end <= witnessText.length; end++) {
          const value = witnessText.slice(start, end);
          if (!isRootPayloadKanaSurface(value)) witnessKeys.add(value);
        }
      }
      const legacyRows = await sql<{ text: string; seq: number }[]>`
        SELECT * FROM kanji_text WHERE text IN ${sql([...witnessKeys])}
      `;
      const rootSeqs = new Set(source.entries.map(entry => entry.seq));
      const legacyOrder = new Map<string, number[]>();
      for (const row of legacyRows) {
        if (!rootSeqs.has(row.seq)) continue;
        const values = legacyOrder.get(row.text) ?? [];
        values.unshift(row.seq);
        legacyOrder.set(row.text, values);
      }
      const directWitnesses = new Map<string, readonly number[]>([
        ['心', [1_360_480, 1_595_125]],
        ['的', [1_437_260, 1_437_270]],
        ['音', [2_859_161, 2_859_162, 1_576_900]],
        ['風', [1_499_730, 1_499_720, 1_361_130]]
      ]);
      for (const [surface, expected] of directWitnesses) {
        const projected = source.forms
          .filter(form => form.surface === surface)
          .map(form => form.seq);
        expect(projected).toEqual(expected);
        expect(legacyOrder.get(surface)).toEqual(expected);
        expect(source.forms
          .filter(form => form.surface === surface)
          .map(form => form.lookupOrder)).toEqual(expected.map((_, index) => index));
      }

      const build = buildRootPayload(source);
      console.log(`root payload built in ${Math.round(performance.now() - started)}ms total`);
      const rebuilt = buildRootPayload(source);
      console.log(`root payload rebuilt in ${Math.round(performance.now() - started)}ms total`);
      assertBytesEqual(rebuilt.bytes, build.bytes);

      const reader = openRootPayload(build.bytes);
      console.log(`root payload opened in ${Math.round(performance.now() - started)}ms total`);
      expect(reader.entryCount).toBe(213_732);
      expect(reader.formCount).toBe(464_802);
      expect(reader.surfaceCount).toBe(432_664);
      expect(reader.restrictionCount).toBe(6_732);

      const surfaces: string[] = [];
      let previousSurface: string | undefined;
      for (const form of source.forms) {
        if (form.surface !== previousSurface) {
          if (previousSurface !== undefined) {
            if (compareRootPayloadText(previousSurface, form.surface) >= 0) {
              throw new Error('PostgreSQL direct surfaces are not in strict UTF-8 byte order');
            }
          }
          surfaces.push(form.surface);
          previousSurface = form.surface;
        }
      }
      expect(surfaces.length).toBe(reader.surfaceCount);
      console.log(`root payload surfaces checked in ${Math.round(performance.now() - started)}ms total`);

      for (let entry = 0; entry < source.entries.length; entry++) {
        const expected = source.entries[entry]!;
        assertSame(reader.entrySeq(entry), expected.seq, `entry ${entry} seq`);
        assertSame(reader.findEntryIndex(expected.seq), entry, `entry ${entry} binary search`);
        assertSame(reader.entryNKanji(entry), expected.nKanji, `entry ${entry} nKanji`);
        assertSame(reader.entryNKana(entry), expected.nKana, `entry ${entry} nKana`);
        assertSame(reader.entryPrimaryNokanji(entry), expected.primaryNokanji, `entry ${entry} primaryNokanji`);
        assertSame(reader.entryArchived(entry), expected.archived, `entry ${entry} archived`);
        assertSame(reader.entryPreferKana(entry), expected.preferKana, `entry ${entry} preferKana`);
        assertSame(
          reader.entryPreferKanaOnOrdinalZero(entry),
          expected.preferKanaOnOrdinalZero,
          `entry ${entry} preferKanaOnOrdinalZero`
        );

        const actualPos: string[] = [];
        for (let position = 0; position < reader.entryPosCount(entry); position++) {
          actualPos.push(reader.string(reader.entryPosStringIdAt(entry, position)));
        }
        assertStringList(actualPos, expected.pos, `entry ${entry} POS`);
      }
      console.log(`root payload entries checked in ${Math.round(performance.now() - started)}ms total`);

      let expectedFormIndex = 0;
      for (let rank = 0; rank < surfaces.length; rank++) {
        const start = reader.surfaceFormStart(rank);
        const count = reader.surfaceFormCount(rank);
        assertSame(start, expectedFormIndex, `surface ${rank} first form`);
        for (let within = 0; within < count; within++) {
          const form = start + within;
          const expected = source.forms[expectedFormIndex]!;
          assertSame(expected.surface, surfaces[rank], `form ${form} surface`);
          assertSame(reader.entrySeq(reader.formEntryIndex(form)), expected.seq, `form ${form} seq`);
          assertSame(reader.formOrdinal(form), expected.ord, `form ${form} ord`);
          assertSame(reader.formCommon(form), expected.common, `form ${form} common`);
          assertSame(
            reader.string(reader.formCommonTagStringId(form)),
            expected.commonTags,
            `form ${form} commonTags`
          );
          assertSame(reader.formRoute(form), expected.route, `form ${form} route`);
          assertSame(reader.formConjugatable(form), expected.conjugatable, `form ${form} conjugatable`);
          assertSame(reader.formNokanji(form), expected.nokanji, `form ${form} nokanji`);
          assertSame(
            reader.resolveSurfaceReference(
              reader.formBestReference(form),
              (surfaceRank) => surfaces[surfaceRank]!
            ),
            expected.best,
            `form ${form} best`
          );
          expectedFormIndex++;
        }
      }
      expect(expectedFormIndex).toBe(source.forms.length);
      console.log(`root payload forms checked in ${Math.round(performance.now() - started)}ms total`);

      for (let restriction = 0; restriction < source.restrictions.length; restriction++) {
        const expected = source.restrictions[restriction]!;
        assertSame(
          reader.entrySeq(reader.restrictionEntryIndex(restriction)),
          expected.seq,
          `restriction ${restriction} seq`
        );
        assertSame(
          reader.resolveSurfaceReference(
            reader.restrictionReadingReference(restriction),
            (surfaceRank) => surfaces[surfaceRank]!
          ),
          expected.reading,
          `restriction ${restriction} reading`
        );
        assertSame(
          reader.resolveSurfaceReference(
            reader.restrictionWrittenReference(restriction),
            (surfaceRank) => surfaces[surfaceRank]!
          ),
          expected.written,
          `restriction ${restriction} written`
        );
      }
      console.log(`root payload restrictions checked in ${Math.round(performance.now() - started)}ms total`);

      const gzipBytes = gzipSync(build.bytes, { level: 9 }).byteLength;
      console.log(JSON.stringify({
        rootPayload: build.stats,
        compression: {
          rawBytes: build.bytes.byteLength,
          gzip9Bytes: gzipBytes
        }
      }, null, 2));
    } finally {
      await sql.end();
    }
  },
  300_000
);
