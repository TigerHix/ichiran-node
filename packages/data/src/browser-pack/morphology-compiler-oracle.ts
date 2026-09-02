import { getConnection } from '@ichiran/reference-postgres';

import {
  buildMorphology,
  type MorphologyCompileResult,
  type MorphologyManualPatchSource,
  type MorphologyRootFormSource,
  type MorphologyRootSource
} from './morphology-compiler.js';

type Sql = ReturnType<typeof getConnection>;

async function loadRootRows(sql: Sql): Promise<MorphologyRootSource[]> {
  return sql<MorphologyRootSource[]>`
    WITH root_pos AS (
      SELECT DISTINCT c."from" AS seq, cp.pos
      FROM conjugation c
      JOIN conj_prop cp ON cp.conj_id = c.id
      WHERE c.via IS NULL AND cp.pos <> 'exp'
    )
    SELECT * FROM (
      SELECT rp.seq, rp.pos, 'kana'::text AS route, r.text, r.ord, r.common,
             r.best_kanji AS counterpart
      FROM root_pos rp
      JOIN kana_text r USING (seq)
      WHERE r.conjugate_p
        AND r.text ~ '^[ァ-ヺヽヾーぁ-ゔゝゞー]+$'
      UNION ALL
      SELECT rp.seq, rp.pos, 'kanji'::text AS route, k.text, k.ord, k.common,
             k.best_kana AS counterpart
      FROM root_pos rp
      JOIN kanji_text k USING (seq)
      WHERE k.conjugate_p
        AND k.text !~ '^[ァ-ヺヽヾーぁ-ゔゝゞー]+$'
    ) rows
    ORDER BY route COLLATE "C", pos COLLATE "C", text COLLATE "C", seq, ord
  `;
}

async function loadRootForms(sql: Sql): Promise<MorphologyRootFormSource[]> {
  return sql<MorphologyRootFormSource[]>`
    WITH roots AS (SELECT DISTINCT "from" AS seq FROM conjugation)
    SELECT * FROM (
      SELECT k.seq, k.text FROM roots JOIN kanji_text k USING (seq)
      UNION
      SELECT r.seq, r.text FROM roots JOIN kana_text r USING (seq)
    ) rows
    ORDER BY seq, text COLLATE "C"
  `;
}

async function loadManualPatches(sql: Sql): Promise<MorphologyManualPatchSource[]> {
  return sql<MorphologyManualPatchSource[]>`
    WITH selected AS (
      SELECT c.seq, c."from" AS root_seq, cp.pos, cp.conj_type,
             cp.neg AS negative, cp.fml AS formal, csr.text AS surface,
             csr.source_text
      FROM conjugation c
      JOIN conj_prop cp ON cp.conj_id = c.id
      JOIN conj_source_reading csr ON csr.conj_id = c.id
      WHERE
        (c."from" = 2089020 AND csr.text LIKE 'じゃ%')
        OR (c."from" IN (1612690, 2253080) AND cp.pos = 'exp')
    )
    SELECT * FROM (
      SELECT 'kana'::text AS route, s.surface, s.root_seq AS "rootSeq",
             s.pos, s.conj_type AS "conjType", s.negative, s.formal,
             s.source_text AS "sourceText",
             src.best_kanji AS "sourceCounterpart",
             target.best_kanji AS "targetCounterpart",
             src.ord, src.common
      FROM selected s
      JOIN kana_text target ON target.seq = s.seq AND target.text = s.surface
      JOIN kana_text src ON src.seq = s.root_seq AND src.text = s.source_text
      UNION ALL
      SELECT 'kanji'::text AS route, s.surface, s.root_seq AS "rootSeq",
             s.pos, s.conj_type AS "conjType", s.negative, s.formal,
             s.source_text AS "sourceText",
             src.best_kana AS "sourceCounterpart",
             target.best_kana AS "targetCounterpart",
             src.ord, src.common
      FROM selected s
      JOIN kanji_text target ON target.seq = s.seq AND target.text = s.surface
      JOIN kanji_text src ON src.seq = s.root_seq AND src.text = s.source_text
    ) rows
    ORDER BY route COLLATE "C", surface COLLATE "C", "rootSeq", "sourceText" COLLATE "C",
             pos COLLATE "C", "conjType", negative NULLS FIRST, formal NULLS FIRST
  `;
}

/** Build the qualified morphology projection from its read-only migration oracle. */
export async function compileMorphology(options: {
  sql?: Sql;
  dataPath?: string;
} = {}): Promise<MorphologyCompileResult> {
  const sql = options.sql ?? getConnection();
  const [rootRows, rootForms, manualRows] = await Promise.all([
    loadRootRows(sql),
    loadRootForms(sql),
    loadManualPatches(sql)
  ]);
  return buildMorphology({ roots: rootRows, rootForms, manualPatches: manualRows }, {
    dataPath: options.dataPath
  });
}
