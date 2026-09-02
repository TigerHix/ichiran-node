import type postgres from 'postgres';

import {
  buildRootPayload,
  RootPayloadEncodingError,
  type RootPayloadBuild,
  type RootPayloadEntrySource,
  type RootPayloadFormSource,
  type RootPayloadRoute,
  type RootPayloadSource
} from './root-payload.js';

interface EntryQueryRow {
  seq: number;
  nKanji: number;
  nKana: number;
  primaryNokanji: boolean;
  archived: boolean;
  preferKana: boolean;
  preferKanaOnOrdinalZero: boolean;
}

interface PosQueryRow {
  seq: number;
  pos: string;
}

interface FormQueryRow extends RootPayloadFormSource {}

interface LegacyFormOrderRow {
  surface: string;
  route: RootPayloadRoute;
  seq: number;
}

interface RestrictionQueryRow {
  seq: number;
  reading: string;
  written: string;
}

export async function loadRootPayloadSource(sql: postgres.Sql): Promise<RootPayloadSource> {
  const [entryRows, posRows, formRows, restrictionRows] = await Promise.all([
    sql.unsafe<EntryQueryRow[]>(`
      WITH archived AS (
        SELECT s.seq
        FROM sense s
        LEFT JOIN sense_prop sp
          ON sp.sense_id = s.id
          AND sp.tag = 'misc'
          AND sp.text IN ('arch', 'obsc', 'rare')
        GROUP BY s.seq
        HAVING EVERY(sp.id IS NOT NULL)
      ), prefer_kana AS (
        SELECT sp.seq,
               TRUE AS "preferKana",
               BOOL_OR(s.ord = 0) AS "preferKanaOnOrdinalZero"
        FROM sense_prop sp
        LEFT JOIN sense s ON s.id = sp.sense_id
        WHERE sp.tag = 'misc' AND sp.text = 'uk'
        GROUP BY sp.seq
      )
      SELECT e.seq,
             e.n_kanji AS "nKanji",
             e.n_kana AS "nKana",
             e.primary_nokanji AS "primaryNokanji",
             (a.seq IS NOT NULL) AS archived,
             COALESCE(pk."preferKana", FALSE) AS "preferKana",
             COALESCE(pk."preferKanaOnOrdinalZero", FALSE) AS "preferKanaOnOrdinalZero"
      FROM entry e
      LEFT JOIN archived a ON a.seq = e.seq
      LEFT JOIN prefer_kana pk ON pk.seq = e.seq
      WHERE e.root_p = TRUE
      ORDER BY e.seq
    `),
    sql.unsafe<PosQueryRow[]>(`
      SELECT sp1.seq, sp1.text AS pos
      FROM sense_prop sp1
      JOIN entry e ON e.seq = sp1.seq AND e.root_p = TRUE
      LEFT JOIN sense_prop sp2
        ON sp1.sense_id = sp2.sense_id
        AND sp2.tag = 'misc'
        AND sp2.text IN ('arch', 'obsc', 'rare')
      WHERE sp1.tag = 'pos' AND sp2.id IS NULL
      GROUP BY sp1.seq, sp1.text
      ORDER BY sp1.seq, sp1.text COLLATE "C"
    `),
    sql.unsafe<FormQueryRow[]>(`
      SELECT * FROM (
        SELECT kt.text AS surface,
               'kanji'::text AS route,
               kt.seq,
               (ROW_NUMBER() OVER (
                 PARTITION BY kt.text
                 ORDER BY kt.ctid DESC
               ) - 1)::integer AS "lookupOrder",
               kt.ord,
               kt.common,
               COALESCE(kt.common_tags, '') AS "commonTags",
               kt.conjugate_p AS conjugatable,
               kt.nokanji,
               kt.best_kana AS best
        FROM kanji_text kt
        JOIN entry e USING (seq)
        WHERE e.root_p = TRUE
          AND NOT (kt.text ~ '^[ァ-ヺヽヾーぁ-ゔゝゞ]+$')

        UNION ALL

        SELECT rt.text AS surface,
               'kana'::text AS route,
               rt.seq,
               (ROW_NUMBER() OVER (
                 PARTITION BY rt.text
                 ORDER BY rt.ctid DESC
               ) - 1)::integer AS "lookupOrder",
               rt.ord,
               rt.common,
               COALESCE(rt.common_tags, '') AS "commonTags",
               rt.conjugate_p AS conjugatable,
               rt.nokanji,
               rt.best_kanji AS best
        FROM kana_text rt
        JOIN entry e USING (seq)
        WHERE e.root_p = TRUE
          AND rt.text ~ '^[ァ-ヺヽヾーぁ-ゔゝゞ]+$'
      ) forms
      ORDER BY surface COLLATE "C", route, "lookupOrder"
    `),
    sql.unsafe<RestrictionQueryRow[]>(`
      SELECT rr.seq, rr.reading, rr.text AS written
      FROM restricted_readings rr
      JOIN entry e USING (seq)
      WHERE e.root_p = TRUE
      ORDER BY rr.seq, rr.reading COLLATE "C", rr.text COLLATE "C"
    `)
  ]);

  const requestedKanjiSurfaces: string[] = [];
  const requestedKanaSurfaces: string[] = [];
  let requestedSurface: string | undefined;
  for (const form of formRows) {
    if (form.surface === requestedSurface) continue;
    (form.route === 'kana' ? requestedKanaSurfaces : requestedKanjiSurfaces).push(form.surface);
    requestedSurface = form.surface;
  }

  // Production proof for the otherwise-observable unordered-query behavior in
  // core findSubstringWords(). OFFSET 0 keeps one parameterized text-index
  // scan per requested surface, matching core's no-ORDER-BY lookup. PostgreSQL
  // visits equal index keys by ascending heap tuple, and core's `unshift`
  // reverses that stream. Only route/surface/root seq leave SQL; CTID is used
  // solely by the canonical projection above and is never emitted or hashed.
  const legacyFormRows = await sql.unsafe<LegacyFormOrderRow[]>(`
    SELECT requested.surface, 'kanji'::text AS route, found.seq
    FROM unnest($1::text[]) requested(surface)
    CROSS JOIN LATERAL (
      SELECT kt.seq
      FROM kanji_text kt
      JOIN entry e USING (seq)
      WHERE kt.text = requested.surface AND e.root_p = TRUE
      OFFSET 0
    ) found

    UNION ALL

    SELECT requested.surface, 'kana'::text AS route, found.seq
    FROM unnest($2::text[]) requested(surface)
    CROSS JOIN LATERAL (
      SELECT rt.seq
      FROM kana_text rt
      JOIN entry e USING (seq)
      WHERE rt.text = requested.surface AND e.root_p = TRUE
      OFFSET 0
    ) found
  `, [requestedKanjiSurfaces, requestedKanaSurfaces]);
  const legacyOrder = new Map<string, number[]>();
  for (const row of legacyFormRows) {
    const key = `${row.route}\u0000${row.surface}`;
    const values = legacyOrder.get(key) ?? [];
    values.unshift(row.seq);
    legacyOrder.set(key, values);
  }
  if (legacyFormRows.length !== formRows.length) {
    throw new RootPayloadEncodingError(
      `Legacy direct-order proof covered ${legacyFormRows.length} of ${formRows.length} forms`
    );
  }
  let proofIndex = 0;
  while (proofIndex < formRows.length) {
    const first = formRows[proofIndex]!;
    let proofEnd = proofIndex + 1;
    while (proofEnd < formRows.length && formRows[proofEnd]!.surface === first.surface) proofEnd++;
    const projected = formRows.slice(proofIndex, proofEnd).map(form => form.seq);
    const observed = legacyOrder.get(`${first.route}\u0000${first.surface}`);
    if (
      observed === undefined
      || observed.length !== projected.length
      || observed.some((seq, index) => seq !== projected[index])
    ) {
      throw new RootPayloadEncodingError(
        `Legacy direct-order proof differs for ${JSON.stringify(first.surface)}`
      );
    }
    legacyOrder.delete(`${first.route}\u0000${first.surface}`);
    proofIndex = proofEnd;
  }
  if (legacyOrder.size !== 0) {
    throw new RootPayloadEncodingError('Legacy direct-order proof contains unprojected forms');
  }

  const entries: RootPayloadEntrySource[] = [];
  let posIndex = 0;
  for (const row of entryRows) {
    const pos: string[] = [];
    while (posIndex < posRows.length && posRows[posIndex]!.seq === row.seq) {
      pos.push(posRows[posIndex]!.pos);
      posIndex++;
    }
    if (posIndex < posRows.length && posRows[posIndex]!.seq < row.seq) {
      throw new RootPayloadEncodingError(`POS row has no root entry: ${posRows[posIndex]!.seq}`);
    }
    entries.push({ ...row, pos });
  }
  if (posIndex !== posRows.length) {
    throw new RootPayloadEncodingError(`POS row has no root entry: ${posRows[posIndex]!.seq}`);
  }

  return {
    entries,
    forms: formRows,
    restrictions: restrictionRows
  };
}

export async function compileRootPayload(sql: postgres.Sql): Promise<RootPayloadBuild> {
  return buildRootPayload(await loadRootPayloadSource(sql));
}
