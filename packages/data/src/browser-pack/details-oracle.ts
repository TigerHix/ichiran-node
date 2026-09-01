import type postgres from 'postgres';

import {
  DETAIL_PROPERTY_TAGS,
  DetailStoreEncodingError,
  type DetailEntrySource,
  type DetailFormSource,
  type DetailGlossSource,
  type DetailPropertySource,
  type DetailPropertyTag,
  type DetailSenseSource
} from './details.js';

interface SenseRow {
  id: number;
  seq: number;
  ord: number;
}
interface GlossRow {
  senseId: number;
  ord: number;
  text: string;
}

interface PropertyRow {
  senseId: number;
  tag: string;
  ord: number;
  text: string;
}

interface FormRow extends DetailFormSource {
  seq: number;
}

export async function loadDetailEntries(sql: postgres.Sql): Promise<DetailEntrySource[]> {
  const [entryRows, formRows, senseRows, glossRows, propertyRows] = await Promise.all([
    sql.unsafe<Array<{ seq: number }>>(`
      SELECT seq FROM entry WHERE root_p = TRUE ORDER BY seq
    `),
    sql.unsafe<FormRow[]>(`
      SELECT forms.* FROM (
        SELECT kt.seq,
               'kanji'::text AS route,
               kt.text,
               kt.ord,
               kt.common,
               COALESCE(kt.common_tags, '') AS "commonTags",
               kt.conjugate_p AS conjugatable,
               kt.nokanji,
               kt.best_kana AS best
        FROM kanji_text kt
        JOIN entry e USING (seq)
        WHERE e.root_p = TRUE
        UNION ALL
        SELECT rt.seq,
               'kana'::text AS route,
               rt.text,
               rt.ord,
               rt.common,
               COALESCE(rt.common_tags, '') AS "commonTags",
               rt.conjugate_p AS conjugatable,
               rt.nokanji,
               rt.best_kanji AS best
        FROM kana_text rt
        JOIN entry e USING (seq)
        WHERE e.root_p = TRUE
      ) forms
      ORDER BY seq, route DESC, ord, text COLLATE "C"
    `),
    sql.unsafe<SenseRow[]>(`
      SELECT s.id, s.seq, s.ord
      FROM sense s
      JOIN entry e USING (seq)
      WHERE e.root_p = TRUE
      ORDER BY s.seq, s.ord
    `),
    sql.unsafe<GlossRow[]>(`
      SELECT g.sense_id AS "senseId", g.ord, g.text
      FROM gloss g
      JOIN sense s ON s.id = g.sense_id
      JOIN entry e USING (seq)
      WHERE e.root_p = TRUE
      ORDER BY s.seq, s.ord, g.ord
    `),
    sql.unsafe<PropertyRow[]>(`
      -- Core orders properties by sense/tag/ordinal. PostgreSQL returns equal-
      -- ordinal rows in their physical insertion order on the pinned snapshot;
      -- the stable id tie-break preserves that observable legacy order.
      SELECT sp.sense_id AS "senseId", sp.tag, sp.ord, sp.text
      FROM sense_prop sp
      JOIN sense s ON s.id = sp.sense_id
      JOIN entry e ON e.seq = s.seq
      WHERE e.root_p = TRUE
      ORDER BY s.seq, s.ord, sp.tag COLLATE "C", sp.ord, sp.id
    `)
  ]);

  const forms = new Map<number, DetailFormSource[]>();
  for (const { seq, ...form } of formRows) {
    const values = forms.get(seq);
    if (values) values.push(form);
    else forms.set(seq, [form]);
  }

  const glosses = new Map<number, DetailGlossSource[]>();
  for (const row of glossRows) {
    const values = glosses.get(row.senseId);
    const gloss = { ord: row.ord, text: row.text };
    if (values) values.push(gloss);
    else glosses.set(row.senseId, [gloss]);
  }
  const properties = new Map<number, DetailPropertySource[]>();
  for (const row of propertyRows) {
    if (!DETAIL_PROPERTY_TAGS.includes(row.tag as DetailPropertyTag)) {
      throw new DetailStoreEncodingError(`Unknown database sense-property tag ${row.tag}`);
    }
    const values = properties.get(row.senseId);
    const property = {
      tag: row.tag as DetailPropertyTag,
      ord: row.ord,
      text: row.text
    };
    if (values) values.push(property);
    else properties.set(row.senseId, [property]);
  }

  const senses = new Map<number, DetailSenseSource[]>();
  for (const row of senseRows) {
    const values = senses.get(row.seq);
    const sense = {
      ord: row.ord,
      glosses: glosses.get(row.id) ?? [],
      properties: properties.get(row.id) ?? []
    };
    if (values) values.push(sense);
    else senses.set(row.seq, [sense]);
    glosses.delete(row.id);
    properties.delete(row.id);
  }
  if (glosses.size !== 0 || properties.size !== 0) {
    throw new DetailStoreEncodingError('Gloss or property references a missing root sense');
  }
  const entries = entryRows.map(({ seq }) => ({
    seq,
    forms: forms.get(seq) ?? [],
    senses: senses.get(seq) ?? []
  }));
  for (const entry of entries) forms.delete(entry.seq);
  if (forms.size !== 0) throw new DetailStoreEncodingError('Form references a missing root entry');
  return entries;
}
