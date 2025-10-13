/**
 * Entry loading functions for JMDict XML processing
 * Ported from ~/ichiran/dict-load.lisp lines 15-159
 */

import { XMLParser } from 'fast-xml-parser';
import type postgres from 'postgres';
import { getConnection } from '@ichiran/core';
import { conjugateEntryOuter, loadSecondaryConjugations } from './conjugate.js';
import { POS_WITH_CONJ_RULES } from './conj-rules.js';

/**
 * XML parser configuration
 */
const xmlParser = new XMLParser({
  ignoreAttributes: false,
  attributeNamePrefix: "@_",
  textNodeName: "#text",
  parseTagValue: false,
  trimValues: true,
  ignoreDeclaration: true,
  processEntities: true
});

/**
 * Helper to extract text content from XML node
 * Ported from dict-load.lisp:19-28 node-text
 */
function nodeText(node: any): string {
  if (!node) return '';
  if (typeof node === 'string') return node;
  if (node['#text']) return String(node['#text']);

  // Handle arrays (multiple child nodes)
  if (Array.isArray(node)) {
    return node.map(n => nodeText(n)).join('');
  }

  // Handle object nodes - extract text from children
  if (typeof node === 'object') {
    const values: string[] = [];
    for (const key in node) {
      if (key === '#text') {
        values.push(String(node[key]));
      } else if (key !== '@_type' && !key.startsWith('@_')) {
        values.push(nodeText(node[key]));
      }
    }
    return values.join('');
  }

  return '';
}

/**
 * Helper to normalize node to array
 */
function asArray<T>(node: T | T[] | undefined): T[] {
  if (!node) return [];
  return Array.isArray(node) ? node : [node];
}

/**
 * Inserts readings (kanji or kana) for an entry
 * Ported from dict-load.lisp:34-66 insert-readings
 *
 * @param nodes - Array of k_ele or r_ele nodes from XML
 * @param tag - 'keb' for kanji or 'reb' for kana
 * @param table - 'kanji_text' or 'kana_text'
 * @param seq - Entry sequence number
 * @param priTag - 'ke_pri' or 're_pri'
 * @param sql - Database connection
 */
async function insertReadings(
  nodes: any[],
  tag: string,
  table: 'kanji_text' | 'kana_text',
  seq: number,
  priTag: string,
  sql: postgres.Sql
): Promise<void> {
  const toAdd: Array<{ text: string; common: number | null; nokanji: boolean; priTags: string }> = [];
  const restrictions: Array<{ seq: number; reading: string; text: string }> = [];
  let primaryNokanji = false;

  for (let ord = 0; ord < nodes.length; ord++) {
    const node = nodes[ord];
    const readingNode = node[tag];
    const readingText = nodeText(readingNode);

    let common: number | null = null;
    let skip = false;
    let nokanji = false;
    const priTags: string[] = [];

    // Check for "ok" in re_inf (okurigana) - skip these
    const reInf = asArray(node.re_inf);
    for (const inf of reInf) {
      if (nodeText(inf) === 'ok') {
        skip = true;
      }
    }

    if (skip) continue;

    // Check for re_nokanji flag
    if (node.re_nokanji !== undefined) {
      nokanji = true;
    }

    // Collect reading restrictions (will be batched later)
    const reRestr = asArray(node.re_restr);
    for (const restr of reRestr) {
      const restrText = nodeText(restr);
      restrictions.push({ seq, reading: readingText, text: restrText });
    }

    // Process priority tags (ke_pri or re_pri)
    const priNodes = asArray(node[priTag]);
    for (const pri of priNodes) {
      const priTagText = nodeText(pri);
      priTags.push(priTagText);

      if (common === null) {
        common = 0;
      }

      // Extract commonality from "nfXX" tags
      if (priTagText.startsWith('nf')) {
        const nfValue = parseInt(priTagText.substring(2));
        if (!isNaN(nfValue)) {
          common = nfValue;
        }
      }
    }

    const priTagsStr = priTags.map(t => `[${t}]`).join('');
    toAdd.push({ text: readingText, common, nokanji, priTags: priTagsStr });

    if (nokanji) {
      primaryNokanji = true;
    }
  }

  // Batch insert all restrictions
  if (restrictions.length > 0) {
    await sql`
      INSERT INTO restricted_readings ${sql(restrictions, 'seq', 'reading', 'text')}
    `;
  }

  // Batch insert all readings
  if (toAdd.length > 0) {
    const readingsWithSeq = toAdd.map((r, ord) => ({
      seq,
      text: r.text,
      ord,
      common: r.common,
      nokanji: r.nokanji,
      commonTags: r.priTags
    }));

    await sql`
      INSERT INTO ${sql(table)} ${sql(readingsWithSeq, 'seq', 'text', 'ord', 'common', 'nokanji', 'commonTags')}
    `;
  }

  // Update entry statistics
  const countField = table === 'kanji_text' ? 'n_kanji' : 'n_kana';
  await sql`
    UPDATE entry
    SET primary_nokanji = ${primaryNokanji},
        ${sql(countField)} = ${toAdd.length}
    WHERE seq = ${seq}
  `;
}

/**
 * Collects sense properties (pos, misc, dial, field, s_inf, stagk, stagr)
 * Ported from dict-load.lisp:69-71 insert-sense-traits
 */
function collectSenseTraits(
  senseNode: any,
  tag: string,
  senseId: number,
  seq: number
): Array<{ senseId: number; tag: string; text: string; ord: number; seq: number }> {
  const nodes = asArray(senseNode[tag]);
  const traits: Array<{ senseId: number; tag: string; text: string; ord: number; seq: number }> = [];

  for (let ord = 0; ord < nodes.length; ord++) {
    let text = nodeText(nodes[ord]);
    // Strip XML entity notation (&foo; -> foo) to match Lisp behavior
    if (text.startsWith('&') && text.endsWith(';')) {
      text = text.slice(1, -1);
    }
    traits.push({ senseId, tag, text, ord, seq });
  }

  return traits;
}

/**
 * Inserts senses (meanings) for an entry
 * Ported from dict-load.lisp:73-80 insert-senses
 */
async function insertSenses(
  senseNodes: any[],
  seq: number,
  sql: postgres.Sql
): Promise<void> {
  if (senseNodes.length === 0) return;

  // Batch insert all senses first using unnest
  const seqs = senseNodes.map(() => seq);
  const ords = senseNodes.map((_, ord) => ord);
  const senses = await sql<Array<{ id: number }>>`
    INSERT INTO sense (seq, ord)
    SELECT * FROM unnest(
      ${sql.array(seqs)}::int[],
      ${sql.array(ords)}::int[]
    ) AS t(seq, ord)
    RETURNING id
  `;

  // Collect all glosses and sense props for batch insert
  const allGlosses: Array<{ senseId: number; text: string; ord: number }> = [];
  const allSenseProps: Array<{ senseId: number; tag: string; text: string; ord: number; seq: number }> = [];

  for (let ord = 0; ord < senseNodes.length; ord++) {
    const senseNode = senseNodes[ord];
    const senseId = senses[ord].id;

    // Collect glosses (English definitions)
    const glossNodes = asArray(senseNode.gloss);
    for (let gord = 0; gord < glossNodes.length; gord++) {
      const glossText = nodeText(glossNodes[gord]);
      allGlosses.push({ senseId, text: glossText, ord: gord });
    }

    // Collect sense properties
    const tags = ['pos', 'misc', 'dial', 'field', 's_inf', 'stagk', 'stagr'];
    for (const tag of tags) {
      const traits = collectSenseTraits(senseNode, tag, senseId, seq);
      allSenseProps.push(...traits);
    }
  }

  // Batch insert all glosses
  if (allGlosses.length > 0) {
    await sql`
      INSERT INTO gloss ${sql(allGlosses, 'senseId', 'text', 'ord')}
    `;
  }

  // Batch insert all sense properties
  if (allSenseProps.length > 0) {
    await sql`
      INSERT INTO sense_prop ${sql(allSenseProps, 'senseId', 'tag', 'text', 'ord', 'seq')}
    `;
  }
}

/**
 * Gets the next available sequence number
 * Ported from dict-load.lisp:105-106 next-seq
 */
async function nextSeq(sql: postgres.Sql): Promise<number> {
  const result = await sql<Array<{ max: number | null }>>`
    SELECT MAX(seq) as max FROM entry
  `;
  const maxSeq = result[0].max;
  return maxSeq ? maxSeq + 1 : 1;
}

export interface LoadEntryOptions {
  /** What to do if entry already exists */
  ifExists?: 'skip' | 'overwrite';
  /** Override sequence number (can be number or reading string) */
  seq?: number | string;
  /** Whether to generate conjugations after loading */
  conjugateP?: boolean;
  /** Upstream entry check [seq, text] - skip if exists with same text */
  upstream?: [number, string];
}

/**
 * Loads a single JMDict entry into the database
 * Ported from dict-load.lisp:115-159 load-entry
 *
 * @param content - XML string containing a single <entry> element
 * @param options - Loading options
 * @returns The sequence number of the loaded entry, or undefined if skipped
 */
export async function loadEntry(
  content: string,
  options: LoadEntryOptions = {}
): Promise<number | undefined> {
  const sql = getConnection();

  // Parse XML content
  const parsed = xmlParser.parse(content);
  const entry = parsed.entry;

  if (!entry) {
    throw new Error('No <entry> element found in XML content');
  }

  // Determine sequence number
  let seq: number;

  if (options.seq !== undefined) {
    if (typeof options.seq === 'string') {
      // If reading exists, use its seq; otherwise choose next available seq
      // TODO: Implement findWord lookup
      seq = await nextSeq(sql);
    } else {
      seq = options.seq;
    }
  } else {
    // Extract from ent_seq element
    const entSeqNode = entry.ent_seq;
    const parsedSeq = parseInt(nodeText(entSeqNode));
    seq = Number.isFinite(parsedSeq) ? parsedSeq : await nextSeq(sql);
  }

  // Check upstream condition
  if (options.upstream) {
    const [upstreamSeq, _upstreamText] = options.upstream;
    const existingEntry = await sql<Array<{ seq: number; content: string }>>`
      SELECT seq, content FROM entry WHERE seq = ${upstreamSeq}
    `;

    if (existingEntry.length > 0) {
      // TODO: Implement get-text to compare with upstreamText
      // For now, skip if entry exists
      return undefined;
    }
  }

  // Handle ifExists option
  if (options.ifExists === 'overwrite') {
    await sql`DELETE FROM entry WHERE seq = ${seq}`;
  }
  // Note: 'skip' is handled by ON CONFLICT below (more efficient than separate SELECT)

  // Create main entry record (idempotent - uses ON CONFLICT for reloads)
  const insertResult = await sql`
    INSERT INTO entry (seq, content, root_p, n_kanji, n_kana, primary_nokanji)
    VALUES (${seq}, ${content}, true, 0, 0, false)
    ON CONFLICT (seq) DO NOTHING
    RETURNING seq
  `;
  
  // If nothing was inserted (conflict), entry already exists
  if (insertResult.length === 0) {
    return undefined; // Handles ifExists='skip' case
  }

  // Extract and insert readings
  const kanjiNodes = asArray(entry.k_ele);
  const kanaNodes = asArray(entry.r_ele);
  const senseNodes = asArray(entry.sense);

  await insertReadings(kanjiNodes, 'keb', 'kanji_text', seq, 'ke_pri', sql);
  await insertReadings(kanaNodes, 'reb', 'kana_text', seq, 're_pri', sql);
  await insertSenses(senseNodes, seq, sql);

  // Note: Entry statistics (n_kanji, n_kana) are recalculated in bulk at the end
  // of load-jmdict and apply-errata via recalcEntryStatsAll() - no need for per-entry recalc

  // Conjugate entry if requested (dict-load.lisp:151-158)
  if (options.conjugateP) {
    const posi = await sql<{ text: string }[]>`
      SELECT DISTINCT text FROM sense_prop
      WHERE seq = ${seq} AND tag = 'pos'
        AND text = ANY(${sql.array(POS_WITH_CONJ_RULES)})
    `;

    if (posi.length > 0) {
      await conjugateEntryOuter(seq, { asPosi: posi.map(p => p.text) });
      await loadSecondaryConjugations({ from: [seq] });
    }
  }

  return seq;
}

/**
 * Recalculates entry statistics (n_kanji, n_kana) for specific entries
 * Ported from dict-load.lisp:52-55 recalc-entry-stats
 */
export async function recalcEntryStats(
  seqs: number[],
  sql?: postgres.Sql
): Promise<void> {
  const conn = sql || getConnection();

  await conn`
    UPDATE entry
    SET n_kanji = (SELECT COUNT(id) FROM kanji_text WHERE kanji_text.seq = entry.seq),
        n_kana = (SELECT COUNT(id) FROM kana_text WHERE kana_text.seq = entry.seq)
    WHERE seq = ANY(${seqs})
  `;
}

/**
 * Recalculates entry statistics for all entries
 * Ported from dict-load.lisp:57-60 recalc-entry-stats-all
 */
export async function recalcEntryStatsAll(sql?: postgres.Sql): Promise<void> {
  const conn = sql || getConnection();

  await conn`
    UPDATE entry
    SET n_kanji = (SELECT COUNT(id) FROM kanji_text WHERE kanji_text.seq = entry.seq),
        n_kana = (SELECT COUNT(id) FROM kana_text WHERE kana_text.seq = entry.seq)
  `;
}
