// dict-cache.ts - Caching infrastructure for dict.ts

import { LRUCache } from 'lru-cache';
import type { Entry, SenseProp, ConjProp, Conjugation, ConjSourceReading } from './types.js';
import type { Sql } from 'postgres';

// =============================================================================
// CALCSCORE RESULT MEMOIZATION
// =============================================================================

/**
 * Result type for calcScore memoization
 */
export interface CalcScoreResult {
  score: number;
  info: any;
}

/**
 * Memoizer for calcScore results - caches based on reading + options
 * Supports all reading types: SimpleText, ProxyText, CounterText, CompoundText
 */
export class CalcScoreMemoizer {
  private cache = new LRUCache<string, CalcScoreResult>({ max: 50000 });

  /**
   * Helper to serialize a word object for cache key
   * Captures all fields that affect calcScore result
   * Limits depth to prevent infinite recursion
   */
  private serializeWord(word: any, depth: number = 0): any {
    if (!word) return null;
    if (typeof word !== 'object') return word;
    if (depth > 3) return { _deep: word.text || 'deep' }; // Safety limit

    const base: any = {
      text: word.text,
      seq: word.seq,
      ord: word.ord,
      common: word.common,
      kana: word.kana,
      conjugations: word.conjugations,
    };

    // KanjiText/KanaText specific
    if ('bestKana' in word) base.bestKana = word.bestKana;
    if ('bestKanji' in word) base.bestKanji = word.bestKanji;
    if ('nokanji' in word) base.nokanji = word.nokanji;

    // Handle nested source (ProxyText, CounterText)
    if ('source' in word && word.source) {
      base.source = this.serializeWord(word.source, depth + 1);
    }

    // CounterText specific - call getText() to get full "1倍" not just "倍"
    if ('valueString' in word) {
      base.valueString = word.valueString;
      base.counter = word.counter;
      if (typeof word.getText === 'function') {
        base.fullText = word.getText();
      }
    }

    return base;
  }

  /**
   * Generate cache key for a reading + options combination
   * Returns unique key if cacheable, or null for uncacheable cases
   */
  getKey(reading: any, args: {
    final?: boolean;
    useLength?: number;
    kanjiBreak?: number[];
    scoreMod?: number | number[] | ((s: number) => number);
  }): string | null {
    // For non-simple scoreMod (arrays or functions), don't cache
    if (args.scoreMod !== undefined && typeof args.scoreMod !== 'number') {
      return null; // Skip caching entirely
    }

    const { final = false, useLength, kanjiBreak = [] } = args;

    const cacheKey: any = {
      f: final,
      ul: useLength,
      kb: kanjiBreak,
      sm: typeof args.scoreMod === 'number' ? args.scoreMod : null,
    };

    // Serialize by reading type (use 'as any' for runtime type checks)
    const r = reading as any;

    if ('words' in r && 'primary' in r) {
      // CompoundText - special code path at line 1528-1545
      cacheKey.t = 'c'; // type: compound
      cacheKey.txt = r.text;
      cacheKey.k = r.kana;
      cacheKey.csm = r.scoreMod; // compound's scoreMod
      cacheKey.p = this.serializeWord(r.primary, 0);
      cacheKey.sb = r.scoreBase ? this.serializeWord(r.scoreBase, 0) : null;
      // wordConjData uses last word (line 619-621)
      if (r.words && r.words.length > 0) {
        cacheKey.lw = this.serializeWord(r.words[r.words.length - 1], 0);
      }
    } else if ('valueString' in r) {
      // CounterText
      cacheKey.t = 'ctr'; // type: counter
      cacheKey.vs = r.valueString;
      cacheKey.ctr = r.counter;
      // CRITICAL: Call getText() to get full text like "1倍", not just "倍"
      cacheKey.txt = typeof r.getText === 'function' ? r.getText() : r.text;
      cacheKey.src = this.serializeWord(r.source, 0);
    } else if ('source' in r) {
      // ProxyText - delegates to source (lines 1596, 1568-1573, 1675-1677, 1715)
      cacheKey.t = 'p'; // type: proxy
      cacheKey.txt = r.text;
      cacheKey.k = r.kana;
      cacheKey.conj = r.conjugations;
      cacheKey.nk = r.nokanji;
      // CRITICAL: Serialize full source, not just source.text
      cacheKey.src = this.serializeWord(r.source, 0);
    } else {
      // Simple types (KanaText, KanjiText)
      cacheKey.t = 's'; // type: simple
      cacheKey.txt = r.text;
      cacheKey.seq = r.seq;
      cacheKey.ord = r.ord;
      cacheKey.com = r.common;
      cacheKey.k = r.kana;
      cacheKey.conj = r.conjugations;
      if ('bestKana' in r) cacheKey.bk = r.bestKana;
      if ('bestKanji' in r) cacheKey.bkj = r.bestKanji;
    }

    return JSON.stringify(cacheKey);
  }

  /**
   * Deep clone info object to prevent mutation of cached data
   */
  private cloneInfo(info: any): any {
    if (info === null || info === undefined) return info;
    if (typeof info !== 'object') return info;

    if (Array.isArray(info)) {
      return info.map(item => this.cloneInfo(item));
    }

    const cloned: any = {};
    for (const key in info) {
      if (Object.prototype.hasOwnProperty.call(info, key)) {
        cloned[key] = this.cloneInfo(info[key]);
      }
    }
    return cloned;
  }

  get(key: string | null): CalcScoreResult | undefined {
    if (key === null) return undefined;
    const cached = this.cache.get(key);
    if (!cached) return undefined;

    // CRITICAL: Deep clone info to prevent mutations from affecting cache
    return {
      score: cached.score,
      info: this.cloneInfo(cached.info)
    };
  }

  set(key: string | null, result: CalcScoreResult) {
    if (key === null) return; // Skip caching for uncacheable cases
    this.cache.set(key, result);
  }

  reset() {
    this.cache.clear();
  }
}

// =============================================================================
// REQUEST-SCOPED DATABASE QUERY CACHE
// =============================================================================

/**
 * Request-scoped cache for batching database queries in calcScore
 * Caches entries, prefer-kana sense_props, and non-arch posi for multiple seq values
 *
 * Design: Uses getOrFetch pattern to eliminate repetitive check-fetch-store code
 */
export class CalcScoreDataCache {
  // Single-seq caches
  private entryCache = new Map<number, Entry | null>();

  private conjCache = new Map<number, Conjugation[]>();
  private conjPropCache = new Map<number, ConjProp[]>();
  private conjSourceCache = new Map<number, ConjSourceReading[]>();
  private conjSourceFilterCache = new Map<string, Map<number, ConjSourceReading[]>>();

  // Multi-seq caches (key = sorted seq array as string)
  private preferKanaCache = new Map<string, Map<number, SenseProp[]>>();
  private nonArchPosiCache = new Map<string, string[]>();

  // Track what we've already fetched
  private fetchedEntries = new Set<number>();
  private fetchedPreferKana = new Set<string>();
  private fetchedNonArchPosi = new Set<string>();

  /**
   * Generic getOrFetch pattern - eliminates repetitive cache boilerplate
   *
   * @param type - Cache type ('entry', 'preferKana', 'nonArchPosi')
   * @param key - Cache key (seq for entry, seq array for others)
   * @param fetcher - Async function to fetch data if not cached
   * @returns Cached or fetched data
   */
  async getOrFetch<T>(
    type: 'entry',
    key: number,
    fetcher: (sql: Sql) => Promise<T>
  ): Promise<T>;
  async getOrFetch<T>(
    type: 'conj',
    key: number,
    fetcher: (sql: Sql) => Promise<T>
  ): Promise<T>;
  async getOrFetch<T>(
    type: 'conjProps',
    key: number[],
    fetcher: (sql: Sql) => Promise<T>
  ): Promise<T>;
  async getOrFetch<T>(
    type: 'conjSource',
    key: { conjIds: number[]; texts?: string[] },
    fetcher: (sql: Sql) => Promise<T>
  ): Promise<T>;
  async getOrFetch<T>(
    type: 'preferKana' | 'nonArchPosi',
    key: number[],
    fetcher: (sql: Sql) => Promise<T>
  ): Promise<T>;
  async getOrFetch<T>(
    type: 'entry' | 'conj' | 'conjProps' | 'conjSource' | 'preferKana' | 'nonArchPosi',
    key: number | number[] | { conjIds: number[]; texts?: string[] },
    fetcher: (sql: Sql) => Promise<T>
  ): Promise<T> {
    // Import sql connection here to avoid circular dependency
    const { getConnection } = await import('./conn.js');
    const sql = getConnection();

    if (type === 'entry') {
      const seq = key as number;
      const cached = this.entryCache.get(seq);
      if (cached !== undefined) return cached as T;

      const result = await fetcher(sql);
      this.entryCache.set(seq, result as any);
      this.fetchedEntries.add(seq);
      return result;
    } else if (type === 'conj') {
      const seq = key as number;
      const cached = this.conjCache.get(seq);
      if (cached !== undefined) return cached as T;

      const result = await fetcher(sql) as Conjugation[];
      this.conjCache.set(seq, result as any);
      return result as T;
    } else if (type === 'conjProps') {
      const conjIds = Array.isArray(key) ? key as number[] : [key as number];
      const missing = conjIds.filter(id => !this.conjPropCache.has(id));
      if (missing.length === 0) {
        const flattened = conjIds.flatMap(id => this.conjPropCache.get(id) || []);
        return flattened as T;
      }

      const result = await fetcher(sql) as ConjProp[];
      const byConj = new Map<number, ConjProp[]>();
      for (const prop of result) {
        if (!byConj.has(prop.conjId)) byConj.set(prop.conjId, []);
        byConj.get(prop.conjId)!.push(prop);
      }
      for (const id of conjIds) {
        this.conjPropCache.set(id, byConj.get(id) || []);
      }
      const flattened = conjIds.flatMap(id => this.conjPropCache.get(id) || []);
      return flattened as T;
    } else if (type === 'conjSource') {
      const { conjIds, texts } = key as { conjIds: number[]; texts?: string[] };
      const sortedIds = [...new Set(conjIds)].sort((a, b) => a - b);
      const textListKey = texts && texts.length > 0 ? JSON.stringify([...new Set(texts)].sort()) : null;
      const filterKey = textListKey ? `${sortedIds.join(',')}|${textListKey}` : null;

      if (filterKey) {
        const cachedFiltered = this.conjSourceFilterCache.get(filterKey);
        if (cachedFiltered !== undefined) {
          const flattened = sortedIds.flatMap(id => cachedFiltered.get(id) || []);
          if (flattened.length > 0) return flattened as T;
        }
      } else {
        const allCached = sortedIds.flatMap(id => this.conjSourceCache.get(id) || []);
        if (allCached.length > 0) {
          return allCached as T;
        }
      }

      const result = await fetcher(sql) as ConjSourceReading[];
      const byConj = new Map<number, ConjSourceReading[]>();
      for (const row of result) {
        if (!byConj.has(row.conjId)) byConj.set(row.conjId, []);
        byConj.get(row.conjId)!.push(row);
      }

      if (filterKey) {
        const byMap = new Map<number, ConjSourceReading[]>();
        for (const id of sortedIds) {
          byMap.set(id, byConj.get(id) || []);
        }
        this.conjSourceFilterCache.set(filterKey, byMap);
      } else {
        for (const id of sortedIds) {
          this.conjSourceCache.set(id, byConj.get(id) || []);
        }
      }

      return result as T;
    } else if (type === 'preferKana') {
      const seqs = key as number[];
      if (seqs.length === 0) return [] as T;

      const cacheKey = JSON.stringify([...seqs].sort());
      const cachedBySeq = this.preferKanaCache.get(cacheKey);
      if (cachedBySeq !== undefined) {
        return seqs.flatMap(seq => cachedBySeq.get(seq) || []) as T;
      }

      const result = await fetcher(sql) as SenseProp[];
      const bySeq = new Map<number, SenseProp[]>();
      for (const row of result) {
        if (!bySeq.has(row.seq)) bySeq.set(row.seq, []);
        bySeq.get(row.seq)!.push(row);
      }
      this.preferKanaCache.set(cacheKey, bySeq);
      this.fetchedPreferKana.add(cacheKey);
      return result as T;
    } else {
      // nonArchPosi
      const seqs = key as number[];
      if (seqs.length === 0) return [] as T;

      const cacheKey = JSON.stringify([...seqs].sort());
      const cached = this.nonArchPosiCache.get(cacheKey);
      if (cached !== undefined) return cached as T;

      const result = await fetcher(sql);
      this.nonArchPosiCache.set(cacheKey, result as any);
      this.fetchedNonArchPosi.add(cacheKey);
      return result;
    }
  }

  /**
   * Prefetch all data needed for a batch of seq values
   * Optimizes by batching database queries
   */
  async prefetchSeqs(seqs: number[]) {
    if (seqs.length === 0) return;

    const { getConnection } = await import('./conn.js');
    const sql = getConnection();
    const uniqueSeqs = [...new Set(seqs)];

    // Batch fetch entries
    const unfetchedSeqs = uniqueSeqs.filter(s => !this.fetchedEntries.has(s));
    if (unfetchedSeqs.length > 0) {
      const entries = await sql<Entry[]>`
        SELECT * FROM entry WHERE seq IN ${sql(unfetchedSeqs)}
      `;
      for (const seq of unfetchedSeqs) {
        const entry = entries.find(e => e.seq === seq) || null;
        this.entryCache.set(seq, entry);
        this.fetchedEntries.add(seq);
      }
    }

    // Batch fetch prefer-kana sense_prop data
    const key = JSON.stringify([...uniqueSeqs].sort());
    if (!this.fetchedPreferKana.has(key)) {
      const preferKana = await sql<SenseProp[]>`
        SELECT id, tag, sense_id AS "senseId", text, ord, seq
        FROM sense_prop
        WHERE seq IN ${sql(uniqueSeqs)}
          AND tag = 'misc'
          AND text = 'uk'
      `;

      // Group by seq
      const bySeq = new Map<number, SenseProp[]>();
      for (const row of preferKana) {
        if (!bySeq.has(row.seq)) bySeq.set(row.seq, []);
        bySeq.get(row.seq)!.push(row);
      }
      this.preferKanaCache.set(key, bySeq);
      this.fetchedPreferKana.add(key);
    }

    // Batch fetch non-arch posi
    if (!this.fetchedNonArchPosi.has(key)) {
      const result = await sql<{ text: string }[]>`
        SELECT DISTINCT sp1.text
        FROM sense_prop sp1
        LEFT JOIN sense_prop sp2
          ON sp1.sense_id = sp2.sense_id
          AND sp2.tag = 'misc'
          AND sp2.text IN ('arch', 'obsc', 'rare')
        WHERE sp1.seq IN ${sql(uniqueSeqs)}
          AND sp1.tag = 'pos'
          AND sp2.id IS NULL
      `;
      this.nonArchPosiCache.set(key, result.map(r => r.text));
      this.fetchedNonArchPosi.add(key);
    }
  }

  setEntry(seq: number, entry: Entry | null) {
    this.entryCache.set(seq, entry);
    this.fetchedEntries.add(seq);
  }

  getPreferKana(seqs: number[]): SenseProp[] | undefined {
    if (seqs.length === 0) return [];
    const key = JSON.stringify([...seqs].sort());
    const bySeq = this.preferKanaCache.get(key);
    if (!bySeq) return undefined;

    return seqs.flatMap(seq => bySeq.get(seq) || []);
  }

  setPreferKana(seqs: number[], data: SenseProp[]) {
    const key = JSON.stringify([...seqs].sort());
    const bySeq = new Map<number, SenseProp[]>();
    for (const row of data) {
      if (!bySeq.has(row.seq)) bySeq.set(row.seq, []);
      bySeq.get(row.seq)!.push(row);
    }
    this.preferKanaCache.set(key, bySeq);
    this.fetchedPreferKana.add(key);
  }

  getNonArchPosi(seqs: number[]): string[] | undefined {
    if (seqs.length === 0) return [];
    const key = JSON.stringify([...seqs].sort());
    return this.nonArchPosiCache.get(key);
  }

  setNonArchPosi(seqs: number[], posi: string[]) {
    const key = JSON.stringify([...seqs].sort());
    this.nonArchPosiCache.set(key, posi);
    this.fetchedNonArchPosi.add(key);
  }

  getConjugations(seq: number): Conjugation[] | undefined {
    return this.conjCache.get(seq);
  }

  setConjugations(seq: number, conjs: Conjugation[]) {
    this.conjCache.set(seq, conjs);
  }

  getConjProps(conjId: number): ConjProp[] | undefined {
    return this.conjPropCache.get(conjId);
  }

  setConjProps(conjId: number, props: ConjProp[]) {
    this.conjPropCache.set(conjId, props);
  }

  getConjSources(conjId: number): ConjSourceReading[] | undefined {
    return this.conjSourceCache.get(conjId);
  }

  getConjSourcesFiltered(conjIds: number[], texts: string[]): ConjSourceReading[] | undefined {
    const key = `${[...new Set(conjIds)].sort((a, b) => a - b).join(',')}|${JSON.stringify([...new Set(texts)].sort())}`;
    const map = this.conjSourceFilterCache.get(key);
    if (!map) return undefined;
    return conjIds.flatMap(id => map.get(id) || []);
  }

  setConjSources(conjIds: number[], rows: ConjSourceReading[], texts?: string[]) {
    const byConj = new Map<number, ConjSourceReading[]>();
    for (const row of rows) {
      if (!byConj.has(row.conjId)) byConj.set(row.conjId, []);
      byConj.get(row.conjId)!.push(row);
    }

    if (texts && texts.length > 0) {
      const key = `${[...new Set(conjIds)].sort((a, b) => a - b).join(',')}|${JSON.stringify([...new Set(texts)].sort())}`;
      this.conjSourceFilterCache.set(key, byConj);
    } else {
      for (const id of conjIds) {
        this.conjSourceCache.set(id, byConj.get(id) || []);
      }
    }
  }

  /**
   * Reset all caches - useful for testing or when DB is updated
   */
  reset() {
    this.entryCache.clear();
    this.conjCache.clear();
    this.conjPropCache.clear();
    this.conjSourceCache.clear();
    this.conjSourceFilterCache.clear();
    this.preferKanaCache.clear();
    this.nonArchPosiCache.clear();
    this.fetchedEntries.clear();
    this.fetchedPreferKana.clear();
    this.fetchedNonArchPosi.clear();
  }
}

// =============================================================================
// GLOBAL CACHE INSTANCES
// =============================================================================

export const calcScoreMemo = new CalcScoreMemoizer();

// Global persistent cache instance - persists across all romanize() calls
// Since the DB doesn't change, we can cache indefinitely for better performance
const calcScoreDataCache = new CalcScoreDataCache();

export function getCalcScoreCache(): CalcScoreDataCache {
  return calcScoreDataCache;
}

// Reset both caches - useful for testing or when DB is updated
export function resetCalcScoreCache() {
  calcScoreMemo.reset();
  calcScoreDataCache.reset();
}
