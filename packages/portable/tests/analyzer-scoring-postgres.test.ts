import { expect, test } from 'bun:test';
import { getConnection, setConnection } from '../../core/src/conn.js';
import { testWord } from '../../core/src/characters.js';
import { getOriginalText, wordConjData } from '../../core/src/dict/conjugation.js';
import { calcScore as currentCalcScore } from '../../core/src/dict/scoring.js';
import { getSplitImpl } from '../../core/src/dict/splitQueries.js';
// Populates the current analyzer's split maps before querying them.
import '../../core/src/dict/splitDefinitions.js';
import type { ConjData, KanaText, KanjiText, Reading } from '../../core/src/types.js';
import { scoreAnalyzerCandidate } from '../src/analyzer-scoring.js';
import {
  ANALYZER_SCORE_FLAG_COMMON,
  ANALYZER_SCORE_FLAG_LONG,
  ANALYZER_SCORE_FLAG_PRIMARY,
  ANALYZER_SCORE_FLAG_STRONG,
  type AnalyzerConjugation,
  type AnalyzerSequenceFacts,
  type AnalyzerWordScoreFacts
} from '../src/analyzer-types.js';

const RUN_POSTGRES_TEST = process.env.RUN_ANALYZER_SCORING_POSTGRES === 'true';

interface FormRow {
  route: 'kanji' | 'kana';
  id: number;
  seq: number;
  text: string;
  ord: number;
  common: number | null;
  commonTags: string;
  conjugateP: boolean;
  nokanji: boolean;
  best: string | null;
}

interface EntryRow {
  rootP: boolean;
  nKanji: number;
  primaryNokanji: boolean;
}

function selectedConjugations(values: readonly ConjData[]): readonly ConjData[] {
  const allVia = values.length > 0 && values.every((value) => value.via !== null);
  return allVia ? values : values.filter((value) => value.via === null);
}

function portableConjugation(value: ConjData): AnalyzerConjugation {
  return {
    seq: value.seq,
    from: value.from,
    via: value.via,
    property: {
      pos: value.prop.pos,
      type: value.prop.conjType,
      negative: value.prop.neg,
      formal: value.prop.fml
    }
  };
}

function flags(kpcl: readonly boolean[]): number {
  return (kpcl[0] ? ANALYZER_SCORE_FLAG_STRONG : 0)
    | (kpcl[1] ? ANALYZER_SCORE_FLAG_PRIMARY : 0)
    | (kpcl[2] ? ANALYZER_SCORE_FLAG_COMMON : 0)
    | (kpcl[3] ? ANALYZER_SCORE_FLAG_LONG : 0);
}

test.skipIf(!RUN_POSTGRES_TEST)(
  'portable materialized scorer matches current PostgreSQL scoring',
  async () => {
    setConnection({
      host: process.env.ANALYZER_SCORING_DATABASE_HOST ?? '/var/run/postgresql',
      database: process.env.ANALYZER_SCORING_DATABASE_NAME ?? 'ichiran_test',
      user: process.env.ANALYZER_SCORING_DATABASE_USER ?? 'tiger',
      password: process.env.ANALYZER_SCORING_DATABASE_PASSWORD ?? ''
    });
    const sql = getConnection();

    const archivedRows = await sql.unsafe<Array<{ seq: number }>>(`
      WITH fully_archived AS (
        SELECT s.seq
        FROM sense s
        LEFT JOIN sense_prop sp
          ON sp.sense_id = s.id
          AND sp.tag = 'misc'
          AND sp.text IN ('arch', 'obsc', 'rare')
        GROUP BY s.seq
        HAVING EVERY(sp.id IS NOT NULL)
      )
      SELECT seq FROM fully_archived
      UNION
      SELECT DISTINCT c.seq
      FROM conjugation c
      JOIN fully_archived a ON a.seq = c."from"
    `);
    const archived = new Set(archivedRows.map((row) => row.seq));

    const rootForms = await sql.unsafe<FormRow[]>(`
      SELECT * FROM (
        SELECT 'kanji'::text AS route, k.id, k.seq, k.text, k.ord, k.common,
               COALESCE(k.common_tags, '') AS "commonTags",
               k.conjugate_p AS "conjugateP", k.nokanji, k.best_kana AS best
        FROM kanji_text k JOIN entry e USING (seq)
        WHERE e.root_p
        UNION ALL
        SELECT 'kana'::text AS route, r.id, r.seq, r.text, r.ord, r.common,
               COALESCE(r.common_tags, '') AS "commonTags",
               r.conjugate_p AS "conjugateP", r.nokanji, r.best_kanji AS best
        FROM kana_text r JOIN entry e USING (seq)
        WHERE e.root_p
      ) candidates
      ORDER BY hashint4(seq), route COLLATE "C", text COLLATE "C", ord
      LIMIT 48
    `);
    const generatedSeqRows = await sql.unsafe<Array<{ seq: number }>>(`
      SELECT seq FROM conjugation
      GROUP BY seq
      ORDER BY hashint4(seq)
      LIMIT 24
    `);
    const generatedSeqs = generatedSeqRows.map((row) => row.seq);
    const generatedRows = await sql<FormRow[]>`
      SELECT * FROM (
        SELECT 'kanji'::text AS route, k.id, k.seq, k.text, k.ord, k.common,
               COALESCE(k.common_tags, '') AS "commonTags",
               k.conjugate_p AS "conjugateP", k.nokanji, k.best_kana AS best
        FROM kanji_text k WHERE k.seq IN ${sql(generatedSeqs)}
        UNION ALL
        SELECT 'kana'::text AS route, r.id, r.seq, r.text, r.ord, r.common,
               COALESCE(r.common_tags, '') AS "commonTags",
               r.conjugate_p AS "conjugateP", r.nokanji, r.best_kanji AS best
        FROM kana_text r WHERE r.seq IN ${sql(generatedSeqs)}
      ) generated
      ORDER BY seq, route, text, ord
    `;
    const firstGenerated = new Map<number, FormRow>();
    for (const row of generatedRows) {
      if (!firstGenerated.has(row.seq)) firstGenerated.set(row.seq, row);
    }
    const forms = [...rootForms, ...firstGenerated.values()];

    async function sequenceFacts(seqs: readonly number[]): Promise<AnalyzerSequenceFacts> {
      if (seqs.length === 0) {
        return { allArchived: false, preferKana: false, preferKanaOnOrdinalZero: false };
      }
      const rows = await sql<Array<{ ordinalZero: boolean }>>`
        SELECT (s.ord = 0) AS "ordinalZero"
        FROM sense_prop sp
        JOIN sense s ON s.id = sp.sense_id
        WHERE sp.seq IN ${sql([...seqs])} AND sp.tag = 'misc' AND sp.text = 'uk'
      `;
      return {
        allArchived: seqs.every((seq) => archived.has(seq)),
        preferKana: rows.length > 0,
        preferKanaOnOrdinalZero: rows.some((row) => row.ordinalZero)
      };
    }

    let compared = 0;
    for (const row of forms) {
      const reading: Reading = row.route === 'kanji'
        ? ({
            id: row.id, seq: row.seq, text: row.text, ord: row.ord,
            common: row.common, commonTags: row.commonTags,
            conjugateP: row.conjugateP, nokanji: row.nokanji, bestKana: row.best
          } satisfies KanjiText)
        : ({
            id: row.id, seq: row.seq, text: row.text, ord: row.ord,
            common: row.common, commonTags: row.commonTags,
            conjugateP: row.conjugateP, nokanji: row.nokanji, bestKanji: row.best
          } satisfies KanaText);
      const allConjugations = await wordConjData(reading);
      const conjugations = selectedConjugations(allConjugations);
      const from = conjugations.map((value) => value.from);
      if (await getSplitImpl(reading, from)) continue;

      const [entry] = await sql<EntryRow[]>`
        SELECT root_p AS "rootP", n_kanji AS "nKanji",
               primary_nokanji AS "primaryNokanji"
        FROM entry WHERE seq = ${row.seq}
      `;
      if (!entry) throw new Error(`Missing entry ${row.seq}`);
      const lineage = [row.seq, ...from];
      const positions = await sql<Array<{ text: string }>>`
        SELECT DISTINCT sp1.text
        FROM sense_prop sp1
        LEFT JOIN sense_prop sp2
          ON sp1.sense_id = sp2.sense_id
          AND sp2.tag = 'misc'
          AND sp2.text IN ('arch', 'obsc', 'rare')
        WHERE sp1.seq IN ${sql(lineage)} AND sp1.tag = 'pos' AND sp2.id IS NULL
      `;

      let inheritedCommon: number | null = null;
      let inheritedOrd: number | null = null;
      if (conjugations.length > 0) {
        const originals = await getOriginalText(conjugations, row.text);
        const inherited: Array<{ common: number | null; ord: number }> = [];
        for (const [text, seq] of originals) {
          const table = testWord(text, 'kana') ? 'kana_text' : 'kanji_text';
          const source = await sql.unsafe<Array<{ common: number | null; ord: number }>>(
            `SELECT common, ord FROM ${table} WHERE seq = $1 AND text = $2`,
            [seq, text]
          );
          if (source[0]) inherited.push(source[0]);
        }
        const commons = inherited
          .map((value) => value.common)
          .filter((value): value is number => value !== null)
          .sort((left, right) => left === 0 ? -1 : right === 0 ? 1 : left - right);
        inheritedCommon = commons[0] ?? null;
        inheritedOrd = inherited.length > 0
          ? Math.min(...inherited.map((value) => value.ord))
          : null;
      }

      const portable: AnalyzerWordScoreFacts = {
        kind: 'word',
        text: row.text,
        trueText: row.text,
        trueTextFollowsText: true,
        route: row.route,
        seq: row.seq,
        ord: row.ord,
        common: row.common,
        nokanji: row.nokanji,
        entry: {
          root: entry.rootP,
          nKanji: entry.nKanji,
          primaryNokanji: entry.primaryNokanji
        },
        conjugationOnly: false,
        conjugations: allConjugations.map(portableConjugation),
        positions: positions.map((value) => value.text),
        self: await sequenceFacts([row.seq]),
        lineage: await sequenceFacts(lineage),
        inheritedCommon,
        inheritedOrd,
        split: null,
        suruBreak: null
      };

      for (const options of [
        {},
        { final: true },
        { useLength: Math.max(1, row.text.length + 2) }
      ] as const) {
        const current = await currentCalcScore(reading, options);
        const actual = scoreAnalyzerCandidate(portable, options);
        expect(actual.score, `${row.seq}/${row.text} ${JSON.stringify(options)}`).toBe(current[0]);
        expect([...actual.info.positions].sort()).toEqual([...current[1].posi].sort());
        expect(actual.info.seqSet).toEqual(current[1].seqSet);
        expect(actual.info.common).toBe(current[1].common);
        expect(actual.info.breakdown).toEqual({
          propertyScore: current[1].scoreInfo[0],
          kanjiBreak: current[1].scoreInfo[1],
          useLengthBonus: current[1].scoreInfo[2],
          split: current[1].scoreInfo[3]
        });
        expect(actual.info.flags).toBe(flags(current[1].kpcl));
        compared++;
      }
    }
    expect(compared).toBeGreaterThanOrEqual(180);
    await sql.end();
  },
  120_000
);
