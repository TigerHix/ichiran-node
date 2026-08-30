// ichiran/dict/conjugation - Conjugation logic extracted from dict.ts
// Handles word conjugation data, caching, and parent queries

import { getConnection, defineCache } from '../conn.js';
import { trueText } from './utils.js';
import { getCalcScoreCache } from './cache.js';
import type {
  AnyWord,
  SimpleWord,
  ProxyText,
  Entry,
  ConjData,
  Conjugation,
  ConjProp,
  ConjSourceReading
} from '../types.js';
import { isCompoundText, isProxyText, isSimpleWord, isSimpleText } from '../types.js';

// Line 654-667: defgeneric word-conj-data
export async function wordConjData(word: AnyWord): Promise<ConjData[]> {
  if (!word) return [];
  if (isCompoundText(word)) {
    // Compound text - get conj data from last word
    return wordConjData(word.words[word.words.length - 1]);
  }
  // ProxyText - delegate to source (Lisp generic method pattern)
  if (isProxyText(word) && word.source) {
    return wordConjData(word.source);
  }
  // SimpleWord (KanjiText/KanaText) or CompoundText with seq
  if (isSimpleWord(word)) {
    const conjugations = isSimpleText(word) ? word.conjugations : undefined;
    return getConjData(word.seq, conjugations ?? undefined, trueText(word));
  }
  if (isCompoundText(word)) {
    // CompoundText has seq array - use first seq
    const seq = word.seq[0];
    return getConjData(seq, undefined, trueText(word));
  }
  return [];
}

// Line 669-672: defgeneric score-base (only defined for compound-text in Lisp)
export function scoreBase(word: AnyWord): SimpleWord | ProxyText {
  // In Lisp, score-base is only defined for compound-text
  // Calling it on non-compound words would throw an error
  if (!isCompoundText(word)) {
    throw new Error('scoreBase called on non-compound word');
  }
  // CompoundText has optional scoreBase property
  if (word.scoreBase) {
    return word.scoreBase;
  }
  return word.primary;
}

// Line 160: defmethod common
export async function getCommon(obj: Entry | { seq: number } | any): Promise<number | null> {
  if (isProxyText(obj)) {
    return getCommon(obj.source);
  }

  const sql = getConnection();
  const seq = obj.seq;

  const result = await sql`
    SELECT MAX(common) as max_common FROM (
      SELECT common FROM kanji_text WHERE seq = ${seq}
      UNION
      SELECT common FROM kana_text WHERE seq = ${seq}
    ) tmp
  `;

  return result[0]?.max_common ?? null;
}

// Line 329-340: defcache :no-conj-data
const noConjDataCache = defineCache<Set<number>>(':no-conj-data', async () => {
  const sql = getConnection();
  const rows = await sql<{ seq: number }[]>`
    SELECT entry.seq FROM entry
    LEFT JOIN conjugation c ON entry.seq = c.seq
    WHERE c.seq IS NULL
  `;
  return new Set(rows.map(r => r.seq));
});

// Line 339-340: defun no-conj-data
async function noConjData(seq: number): Promise<boolean> {
  const cache = await noConjDataCache();
  return cache.has(seq);
}

// Line 342-369: defun get-conj-data
export async function getConjData(
  seq: number,
  fromConjIds?: number[] | number | ':root',
  texts?: string | string[]
): Promise<ConjData[]> {
  if (fromConjIds === ':root' || await noConjData(seq)) {
    return [];
  }

  const sql = getConnection();
  const cache = getCalcScoreCache();
  const textList = texts ? (Array.isArray(texts) ? texts : [texts]) : null;

  const getConjugations = async () => {
    if (fromConjIds === null || fromConjIds === undefined) {
      return await sql<Conjugation[]>`SELECT * FROM conjugation WHERE seq = ${seq} ORDER BY id`;
    } else if (Array.isArray(fromConjIds)) {
      const validIds = fromConjIds.filter((id): id is number => id !== null && id !== undefined);
      if (validIds.length === 0) {
        return await sql<Conjugation[]>`SELECT * FROM conjugation WHERE seq = ${seq} ORDER BY id`;
      }
      return await sql<Conjugation[]>`SELECT * FROM conjugation WHERE seq = ${seq} AND id IN ${sql(validIds)} ORDER BY id`;
    } else {
      return await sql<Conjugation[]>`SELECT * FROM conjugation WHERE seq = ${seq} AND "from" = ${fromConjIds} ORDER BY id`;
    }
  };

  const shouldCacheConj = fromConjIds === null || fromConjIds === undefined;
  const conjs = shouldCacheConj
    ? await cache.getOrFetch('conj', seq, getConjugations) as Conjugation[]
    : await getConjugations();
  const results: ConjData[] = [];

  if (conjs.length === 0) {
    return results;
  }

  // Batch fetch all source readings and props for all conjugations
  const conjIds = conjs.map(c => c.id);

  // Fetch all source readings in one query
  const fetchSources = async (ids: number[], filter?: string[]) => {
    if (filter && filter.length > 0) {
      return sql<(ConjSourceReading & { conjId: number })[]>`
        SELECT text, source_text, conj_id FROM conj_source_reading
        WHERE conj_id IN ${sql(ids)} AND text IN ${sql(filter)}
      `;
    }
    return sql<(ConjSourceReading & { conjId: number })[]>`
      SELECT text, source_text, conj_id FROM conj_source_reading
      WHERE conj_id IN ${sql(ids)}
    `;
  };

  let allSrcMapRows: (ConjSourceReading & { conjId: number })[];
  if (textList && textList.length > 0) {
    const cached = cache.getConjSourcesFiltered(conjIds, textList);
    if (cached && cached.length > 0) {
      allSrcMapRows = cached as (ConjSourceReading & { conjId: number })[];
    } else {
      const fetchedSources = await cache.getOrFetch('conjSource', { conjIds, texts: textList }, () => fetchSources(conjIds, textList));
      allSrcMapRows = fetchedSources as (ConjSourceReading & { conjId: number })[];
      cache.setConjSources(conjIds, allSrcMapRows, textList);
    }
  } else {
    const cached = conjIds.flatMap(id => cache.getConjSources(id) || []);
    if (cached.length > 0) {
      allSrcMapRows = cached as (ConjSourceReading & { conjId: number })[];
    } else {
      const fetchedSources = await cache.getOrFetch('conjSource', { conjIds }, () => fetchSources(conjIds));
      allSrcMapRows = fetchedSources as (ConjSourceReading & { conjId: number })[];
      cache.setConjSources(conjIds, allSrcMapRows);
    }
  }

  // Fetch all props in one query
  const fetchProps = async (ids: number[]) => {
    return sql<(ConjProp & { conjId: number })[]>`
      SELECT * FROM conj_prop WHERE conj_id IN ${sql(ids)}
    `;
  };

  let allProps = conjIds.flatMap(id => cache.getConjProps(id) || []);
  if (allProps.length === 0) {
    allProps = await cache.getOrFetch('conjProps', conjIds, async () => fetchProps(conjIds)) as (ConjProp & { conjId: number })[];
  }

  // Group source readings by conj_id
  const srcMapByConjId = new Map<number, [string, string][]>();
  for (const row of allSrcMapRows) {
    if (!srcMapByConjId.has(row.conjId)) {
      srcMapByConjId.set(row.conjId, []);
    }
    srcMapByConjId.get(row.conjId)!.push([row.text, row.sourceText]);
  }

  // Group props by conj_id
  const propsByConjId = new Map<number, ConjProp[]>();
  for (const prop of allProps) {
    if (!propsByConjId.has(prop.conjId)) {
      propsByConjId.set(prop.conjId, []);
    }
    propsByConjId.get(prop.conjId)!.push(prop);
  }

  // Build results
  for (const conj of conjs) {
    const srcMap = srcMapByConjId.get(conj.id) || [];

    if (!textList || srcMap.length > 0) {
      const props = propsByConjId.get(conj.id) || [];

      for (const prop of props) {
        results.push({
          seq: conj.seq,
          from: conj.from,
          via: conj.via,
          prop,
          srcMap
        });
      }
    }
  }

  return results;
}

// Line 371-378: defun get-original-text-once
export async function getOriginalTextOnce(conjDatas: ConjData | ConjData[], texts: string | string[]): Promise<string[]> {
  const textList = Array.isArray(texts) ? texts : [texts];
  const conjDataList = Array.isArray(conjDatas) ? conjDatas : [conjDatas];

  const results: string[] = [];
  for (const conjData of conjDataList) {
    for (const [txt, srcTxt] of conjData.srcMap) {
      if (textList.includes(txt)) {
        results.push(srcTxt);
      }
    }
  }
  return results;
}

// Line 380-392: defun get-original-text*
export async function getOriginalText(conjDatas: ConjData | ConjData[], texts: string | string[]): Promise<[string, number][]> {
  const textList = Array.isArray(texts) ? texts : [texts];
  const conjDataList = Array.isArray(conjDatas) ? conjDatas : [conjDatas];

  const results: [string, number][] = [];
  for (const conjData of conjDataList) {
    const srcTexts: string[] = [];
    for (const [txt, srcTxt] of conjData.srcMap) {
      if (textList.includes(txt)) {
        srcTexts.push(srcTxt);
      }
    }

    if (conjData.via === null) {
      for (const txt of srcTexts) {
        results.push([txt, conjData.from]);
      }
    } else {
      const newCd = await getConjData(conjData.via, conjData.from);
      const nestedResults = await getOriginalText(newCd, srcTexts);
      results.push(...nestedResults);
    }
  }
  return results;
}

// Line 404-415: defprepared query-parents-kanji
export async function queryParentsKanji(seq: number, text: string): Promise<Array<[number, number]>> {
  const sql = getConnection();
  const results = await sql<Array<{ id: number; conjId: number }>>`
    SELECT kt.id, conj.id as conj_id
    FROM kanji_text kt, conj_source_reading csr, conjugation conj
    WHERE conj.seq = ${seq}
      AND conj.id = csr.conj_id
      AND csr.text = ${text}
      AND kt.seq = CASE WHEN conj.via IS NOT NULL THEN conj.via ELSE conj.from END
      AND kt.text = csr.source_text
  `;
  return results.map(r => [r.id, r.conjId]);
}

// Line 417-428: defprepared query-parents-kana
export async function queryParentsKana(seq: number, text: string): Promise<Array<[number, number]>> {
  const sql = getConnection();
  const results = await sql<Array<{ id: number; conjId: number }>>`
    SELECT kt.id, conj.id as conj_id
    FROM kana_text kt, conj_source_reading csr, conjugation conj
    WHERE conj.seq = ${seq}
      AND conj.id = csr.conj_id
      AND csr.text = ${text}
      AND kt.seq = CASE WHEN conj.via IS NOT NULL THEN conj.via ELSE conj.from END
      AND kt.text = csr.source_text
  `;
  return results.map(r => [r.id, r.conjId]);
}
