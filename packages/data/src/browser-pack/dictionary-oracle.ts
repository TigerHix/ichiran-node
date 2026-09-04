import type postgres from 'postgres';

import {
  LEXICON_PROPERTY_TAGS,
  LexiconStoreEncodingError,
  type LexiconEntrySource,
  type LexiconFormSource,
  type LexiconPropertySource,
  type LexiconPropertyTag,
  type LexiconSenseSource
} from './lexicon.js';
import type {
  LocaleGlossEntrySource,
  LocaleGlossTextSource
} from './locale-gloss.js';

interface SenseRow { readonly id: number; readonly seq: number; readonly ord: number; }
interface GlossRow { readonly senseId: number; readonly ord: number; readonly text: string; }
interface PropertyRow {
  readonly senseId: number;
  readonly tag: string;
  readonly ord: number;
  readonly text: string;
}
interface FormRow extends LexiconFormSource { readonly seq: number; }

export interface DictionaryOracleEntries {
  readonly lexicon: readonly LexiconEntrySource[];
  readonly english: readonly LocaleGlossEntrySource[];
}

/** Qualification-only PostgreSQL projection into the clean split stores. */
export async function loadDictionaryEntries(
  sql: postgres.Sql
): Promise<DictionaryOracleEntries> {
  const [entryRows, formRows, senseRows, glossRows, propertyRows] = await Promise.all([
    sql.unsafe<Array<{ seq: number }>>(`
      SELECT seq FROM entry WHERE root_p = TRUE ORDER BY seq
    `),
    sql.unsafe<FormRow[]>(`
      SELECT forms.* FROM (
        SELECT kt.seq, 'kanji'::text AS route, kt.text, kt.ord, kt.common,
               COALESCE(kt.common_tags, '') AS "commonTags", kt.conjugate_p AS conjugatable,
               kt.nokanji, kt.best_kana AS best
        FROM kanji_text kt JOIN entry e USING (seq) WHERE e.root_p = TRUE
        UNION ALL
        SELECT rt.seq, 'kana'::text AS route, rt.text, rt.ord, rt.common,
               COALESCE(rt.common_tags, '') AS "commonTags", rt.conjugate_p AS conjugatable,
               rt.nokanji, rt.best_kanji AS best
        FROM kana_text rt JOIN entry e USING (seq) WHERE e.root_p = TRUE
      ) forms
      ORDER BY seq, route DESC, ord, text COLLATE "C"
    `),
    sql.unsafe<SenseRow[]>(`
      SELECT s.id, s.seq, s.ord FROM sense s JOIN entry e USING (seq)
      WHERE e.root_p = TRUE ORDER BY s.seq, s.ord
    `),
    sql.unsafe<GlossRow[]>(`
      SELECT g.sense_id AS "senseId", g.ord, g.text
      FROM gloss g JOIN sense s ON s.id = g.sense_id JOIN entry e USING (seq)
      WHERE e.root_p = TRUE ORDER BY s.seq, s.ord, g.ord
    `),
    sql.unsafe<PropertyRow[]>(`
      SELECT sp.sense_id AS "senseId", sp.tag, sp.ord, sp.text
      FROM sense_prop sp JOIN sense s ON s.id = sp.sense_id JOIN entry e ON e.seq = s.seq
      WHERE e.root_p = TRUE
      ORDER BY s.seq, s.ord, sp.tag COLLATE "C", sp.ord, sp.id
    `)
  ]);

  const forms = new Map<number, LexiconFormSource[]>();
  for (const { seq, ...form } of formRows) {
    const values = forms.get(seq) ?? [];
    values.push(form);
    forms.set(seq, values);
  }
  const glosses = new Map<number, LocaleGlossTextSource[]>();
  for (const { senseId, ...gloss } of glossRows) {
    const values = glosses.get(senseId) ?? [];
    values.push(gloss);
    glosses.set(senseId, values);
  }
  const properties = new Map<number, PropertyRow[]>();
  for (const property of propertyRows) {
    if (property.tag !== 's_inf'
      && !LEXICON_PROPERTY_TAGS.includes(property.tag as LexiconPropertyTag)) {
      throw new LexiconStoreEncodingError(
        `Unknown database sense-property tag ${property.tag}`
      );
    }
    const values = properties.get(property.senseId) ?? [];
    values.push(property);
    properties.set(property.senseId, values);
  }

  const lexicalSenses = new Map<number, LexiconSenseSource[]>();
  const englishGroups = new Map<number, LocaleGlossEntrySource['groups'][number][]>();
  for (const sense of senseRows) {
    const values = properties.get(sense.id) ?? [];
    const lexical = lexicalSenses.get(sense.seq) ?? [];
    lexical.push({
      ord: sense.ord,
      properties: values.filter(value => value.tag !== 's_inf').map((value): LexiconPropertySource => ({
        tag: value.tag as LexiconPropertyTag,
        ord: value.ord,
        text: value.text
      }))
    });
    lexicalSenses.set(sense.seq, lexical);
    const english = englishGroups.get(sense.seq) ?? [];
    english.push({
      targets: [sense.ord],
      glosses: glosses.get(sense.id) ?? [],
      info: values.filter(value => value.tag === 's_inf').map(value => ({
        ord: value.ord,
        text: value.text
      }))
    });
    englishGroups.set(sense.seq, english);
    glosses.delete(sense.id);
    properties.delete(sense.id);
  }
  if (glosses.size !== 0 || properties.size !== 0) {
    throw new LexiconStoreEncodingError('Gloss or property references a missing root sense');
  }

  const lexicon = entryRows.map(({ seq }) => ({
    seq,
    forms: forms.get(seq) ?? [],
    senses: lexicalSenses.get(seq) ?? []
  }));
  const english = entryRows.map(({ seq }) => ({
    seq,
    groups: englishGroups.get(seq) ?? []
  }));
  for (const entry of lexicon) forms.delete(entry.seq);
  if (forms.size !== 0) throw new LexiconStoreEncodingError('Form references a missing root entry');
  return { lexicon, english };
}
