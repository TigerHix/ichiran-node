// ichiran/dict/readings - Reading extraction functions
// Extracted from dict.lisp lines 80-476

import { AsyncLocalStorage } from 'async_hooks';
import { getConnection } from '../conn.js';
import { kanjiRegex, kanjiMatch, kanjiCrossMatch, testWord } from '../characters.js';
import type {
  KanjiText,
  KanaText,
  AnyWord,
  Segment,
  Reading,
  Entry
} from '../types.js';
import {
  isSegment,
  isKanjiText,
  isKanaText,
  isProxyText,
  isSimpleWord,
  isWordWithText,
  isWordWithKana,
  isCounterText
} from '../types.js';
import { queryParentsKanji, queryParentsKana } from './conjugation.js';
import { getHintImpl as getHint } from './splitQueries.js';

// Re-export for backward compatibility
export { getHint };

// =============================================================================
// ASYNC LOCAL STORAGE CONTEXT FOR HINTS
// =============================================================================

// Line 82: defparameter *disable-hints* - feature flag to prevent infinite recursion
// In Lisp, this is a dynamic variable with let-binding. In TypeScript, we use AsyncLocalStorage
// to provide async-safe context isolation (prevents race conditions between concurrent calls)
const disableHintsStorage = new AsyncLocalStorage<boolean>();

function getDisableHints(): boolean {
  return disableHintsStorage.getStore() ?? false;
}

function withDisableHints<T>(value: boolean, fn: () => Promise<T>): Promise<T> {
  return disableHintsStorage.run(value, fn);
}

// =============================================================================
// HELPER FUNCTIONS
// =============================================================================

// Line 117-124: defun get-kanji-kana-old (fallback when best-kana-conj returns :null)
async function getKanjiKanaOld(obj: KanjiText): Promise<string> {
  const regex = kanjiRegex(obj.text);
  const sql = getConnection();
  const kts = await sql<KanaText[]>`
    SELECT * FROM kana_text WHERE seq = ${obj.seq} ORDER BY ord
  `;

  for (const kt of kts) {
    if (regex.test(kt.text)) {
      return kt.text;
    }
  }

  return kts[0]?.text || obj.text;
}

// =============================================================================
// MAIN READING FUNCTIONS
// =============================================================================

// Line 80-84, 111-115, 150-151: defmethod get-kana with :around method for hints
export async function getKana(obj: AnyWord | Segment | null | undefined): Promise<string> {
  if (!obj) return '';

  // Handle Segment objects
  if (isSegment(obj)) {
    // This is a Segment - get kana from the word (recursive)
    return getKana(obj.word);
  }

  // Line 80-84: :around method - try get-hint first for simple-text
  // Only apply hints to SimpleWord types (KanjiText/KanaText) that aren't already hinted
  if (isSimpleWord(obj) && !getDisableHints() && !(obj.hintedp)) {
    const hint = await withDisableHints(true, () => getHint(obj));
    if (hint) return hint;
  }
  // Handle WordInfo objects that have seq array (from compound words)
  else if (!isSimpleWord(obj) && isWordWithText(obj) && 'seq' in obj && !getDisableHints()) {
    const seqValue = (obj as any).seq;
    if (Array.isArray(seqValue) && seqValue.length > 0 && !('hintedp' in obj && (obj as any).hintedp)) {
      // BUG FIX: WordInfo can have array seq, but getHint expects single number
      const reading = { ...obj, seq: seqValue[0] } as unknown as Reading;
      const hint = await withDisableHints(true, () => getHint(reading));
      if (hint) return hint;
    }
  }

  // Line 111-115: kanji-text method - call best-kana-conj
  if (isKanjiText(obj)) {
    const bk = await bestKanaConj(obj);
    if (bk === ':null') {
      return getKanjiKanaOld(obj);
    }
    return bk;
  }

  // Handle objects with getKana method (e.g. CounterText)
  // CounterText.getKana() adds suffix, so prefer it over getKanaBase
  if (isCounterText(obj) && typeof (obj as any).getKana === 'function') {
    return (obj as any).getKana();
  }

  // Handle objects with getKanaBase method (e.g. NumberText)
  // NumberText extends CounterText, so this is also CounterText
  if (isCounterText(obj) && typeof (obj as any).getKanaBase === 'function') {
    return (obj as any).getKanaBase();
  }

  // Line 150-151: kana-text method - return text directly
  if (isWordWithKana(obj)) {
    if (typeof obj.kana === 'string') return obj.kana;
    if (Array.isArray(obj.kana)) return obj.kana as any; // For alternative words
  }
  if (isWordWithText(obj)) return obj.text;
  return '';
}

// Line 108-109, 153-155: defmethod get-kanji
export async function getKanji(obj: AnyWord | Segment | null | undefined): Promise<string | null> {
  if (!obj) return null;

  // Handle Segment objects
  if (isSegment(obj)) {
    // This is a Segment - get kanji from the word (recursive)
    return getKanji(obj.word);
  }

  // Line 108-109: kanji-text method - return text directly
  if (isKanjiText(obj) && !testWord(obj.text, 'kana')) {
    return obj.text;
  }

  // Line 153-155: kana-text method - call best-kanji-conj
  if (isKanaText(obj)) {
    const bk = await bestKanjiConj(obj);
    if (bk !== ':null') {
      return bk;
    }
    return null;
  }

  // Default: check if text contains kanji
  if (isWordWithText(obj) && !testWord(obj.text, 'kana')) {
    return obj.text;
  }

  return null;
}

export async function trueKana(obj: AnyWord): Promise<string> {
  if (isProxyText(obj)) {
    return trueKana(obj.source);
  }
  return getKana(obj);
}

export async function trueKanji(obj: AnyWord): Promise<string | null> {
  if (isProxyText(obj)) {
    return trueKanji(obj.source);
  }
  return getKanji(obj);
}

// =============================================================================
// BEST CONJUGATION READING FUNCTIONS
// =============================================================================

// Line 430-455: defun best-kana-conj
export async function bestKanaConj(obj: KanjiText): Promise<string | ':null'> {
  const wc = obj.conjugations;

  // If not conjugated or is root, return best-kana directly
  if ((!wc || wc === ':root') && obj.bestKana && obj.bestKana !== ':null') {
    return obj.bestKana;
  }

  const parents = await queryParentsKanji(obj.seq, obj.text);
  if (parents.length === 0) return ':null';

  const sql = getConnection();

  // Batch fetch all parent kanji_text rows
  const parentIds = parents.map(([pid]) => pid);
  const parentKtRows = await sql<KanjiText[]>`
    SELECT * FROM kanji_text WHERE id = ANY(${parentIds})
  `;
  const parentKtMap = new Map(parentKtRows.map(kt => [kt.id, kt]));

  // Batch fetch all conj_source_reading records for all conjugation IDs
  const conjIds = parents.map(([, cid]) => cid);
  const allReadings = await sql<Array<{ conjId: number; text: string; sourceText: string }>>`
    SELECT conj_id as conj_id, text, source_text as source_text
    FROM conj_source_reading
    WHERE conj_id = ANY(${conjIds})
  `;

  // Build a map: conjId -> sourceText -> reading texts
  const readingsMap = new Map<number, Map<string, string[]>>();
  for (const r of allReadings) {
    if (!readingsMap.has(r.conjId)) {
      readingsMap.set(r.conjId, new Map());
    }
    const sourceMap = readingsMap.get(r.conjId)!;
    if (!sourceMap.has(r.sourceText)) {
      sourceMap.set(r.sourceText, []);
    }
    sourceMap.get(r.sourceText)!.push(r.text);
  }

  for (const [pid, cid] of parents) {
    // Get parent kanji-text from cache
    const parentKt = parentKtMap.get(pid);
    if (!parentKt) continue;

    // Recursively get best-kana from parent
    const parentBk = await bestKanaConj(parentKt);

    // Skip if parent has no best-kana or if conjugation filter doesn't match
    if (parentBk === ':null') continue;
    if (wc && wc !== ':root' && Array.isArray(wc) && !wc.includes(cid)) continue;

    // Get readings for this conjugation from parent's best-kana (from cache)
    const readings = readingsMap.get(cid)?.get(parentBk) || [];

    if (readings.length === 0) continue;

    if (readings.length === 1) {
      return readings[0];
    }

    // Multiple readings - try to match using kanji-cross-match
    const km = kanjiCrossMatch(parentKt.text, parentBk, obj.text);
    if (km) {
      const exactMatch = readings.find(r => r === km);
      if (exactMatch) {
        return exactMatch;
      }

      // Try regex matching, preferring similar length
      const regex = kanjiRegex(obj.text);
      const lenKm = km.length;
      const sortedReadings = [...readings].sort((a, b) => Math.abs(a.length - lenKm) - Math.abs(b.length - lenKm));

      for (const rd of sortedReadings) {
        if (regex.test(rd)) {
          return rd;
        }
      }
    }

    // Fallback to first reading
    return readings[0];
  }

  return ':null';
}

// Line 457-476: defun best-kanji-conj
export async function bestKanjiConj(obj: KanaText): Promise<string | ':null'> {
  const wc = obj.conjugations;

  // If not conjugated or is root, return best-kanji directly
  if ((!wc || wc === ':root') && obj.bestKanji && obj.bestKanji !== ':null') {
    return obj.bestKanji;
  }

  // If entry has no kanji, return :null
  const sql = getConnection();
  const entryRows = await sql<Entry[]>`SELECT * FROM entry WHERE seq = ${obj.seq}`;
  if (obj.nokanji || (entryRows.length > 0 && entryRows[0].nKanji === 0)) {
    return ':null';
  }

  const parents = await queryParentsKana(obj.seq, obj.text);
  if (parents.length === 0) return ':null';

  // Batch fetch all parent kana_text rows
  const parentIds = parents.map(([pid]) => pid);
  const parentKanaRows = await sql<KanaText[]>`
    SELECT * FROM kana_text WHERE id = ANY(${parentIds})
  `;
  const parentKanaMap = new Map(parentKanaRows.map(kt => [kt.id, kt]));

  // Batch fetch all conj_source_reading records for all conjugation IDs
  const conjIds = parents.map(([, cid]) => cid);
  const allReadings = await sql<Array<{ conjId: number; text: string; sourceText: string }>>`
    SELECT conj_id as conj_id, text, source_text as source_text
    FROM conj_source_reading
    WHERE conj_id = ANY(${conjIds})
  `;

  // Build a map: conjId -> sourceText -> reading texts
  const readingsMap = new Map<number, Map<string, string[]>>();
  for (const r of allReadings) {
    if (!readingsMap.has(r.conjId)) {
      readingsMap.set(r.conjId, new Map());
    }
    const sourceMap = readingsMap.get(r.conjId)!;
    if (!sourceMap.has(r.sourceText)) {
      sourceMap.set(r.sourceText, []);
    }
    sourceMap.get(r.sourceText)!.push(r.text);
  }

  for (const [pid, cid] of parents) {
    // Get parent kana-text from cache
    const parentKanaText = parentKanaMap.get(pid);
    if (!parentKanaText) continue;

    // Recursively get best-kanji from parent
    const parentBk = await bestKanjiConj(parentKanaText);

    // Skip if parent has no best-kanji or if conjugation filter doesn't match
    if (parentBk === ':null') continue;
    if (wc && wc !== ':root' && Array.isArray(wc) && !wc.includes(cid)) continue;

    // Get readings for this conjugation from parent's best-kanji (from cache)
    const readings = readingsMap.get(cid)?.get(parentBk) || [];

    // Find matching reading using kanji-match
    for (const reading of readings) {
      if (kanjiMatch(reading, obj.text)) {
        return reading;
      }
    }
  }

  return ':null';
}
