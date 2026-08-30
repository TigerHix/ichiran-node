// ichiran/grammar/suffixCache - Suffix cache initialization and management
// Extracted from dict-grammar.ts Lines 4-10, 54-56, 72-74, 130-146, 497-698

import type { SuffixCacheEntry, SuffixValue } from './types.js';
import type { KanaText } from '../types.js';
// Phase 3: Import from suffixKanaHelpers instead of suffixHelpers to break circular dependency
import { getKanaForms, getKanaForm, findWordConjOf } from '../dict/suffixKanaHelpers.js';

// ============================================================================
// SUFFIX DESCRIPTION (Lines 76-124)
// ============================================================================

export const SUFFIX_DESCRIPTION: Record<string | number, string> = {
  ':chau': 'indicates completion (to finish ...)',
  ':ha': 'topic marker particle',
  ':tai': 'want to... / would like to...',
  ':iru': 'indicates continuing action (to be ...ing)',
  ':oru': 'indicates continuing action (to be ...ing) (humble)',
  ':aru': 'indicates completion / finished action',
  ':kuru': 'indicates action that had been continuing up till now / came to be ',
  ':oku': 'to do in advance / to leave in the current state expecting a later change',
  ':kureru': '(asking) to do something for one',
  ':morau': '(asking) to get somebody to do something',
  ':itadaku': '(asking) to get somebody to do something (polite)',
  ':iku': 'is becoming / action starting now and continuing',
  ':suru': 'makes a verb from a noun',
  ':itasu': 'makes a verb from a noun (humble)',
  ':sareru': 'makes a verb from a noun (honorific or passive)',
  ':saseru': 'let/make someone/something do ...',
  ':rou': 'probably / it seems that... / I guess ...',
  ':ii': "it's ok if ... / is it ok if ...?",
  ':mo': 'even if ...',
  ':sugiru': 'to be too (much) ...',
  ':nikui': 'difficult to...',
  ':sa': '-ness (degree or condition of adjective)',
  ':tsutsu': 'while ... / in the process of ...',
  ':tsutsuaru': 'to be doing ... / to be in the process of doing ...',
  ':uru': 'can ... / to be able to ...',
  ':sou': 'looking like ... / seeming ...',
  ':nai': 'negative suffix',
  ':ra': 'pluralizing suffix (not polite)',
  ':kudasai': 'please do ...',
  ':yagaru': 'indicates disdain or contempt',
  ':naru': 'to become ...',
  ':desu': 'formal copula',
  ':desho': "it seems/perhaps/don't you think?",
  ':tosuru': 'to try to .../to be about to...',
  ':garu': 'to feel .../have a ... impression of someone',
  ':me': 'somewhat/-ish',
  ':gai': 'worth it to ...',
  ':tasou': 'seem to want to... (tai+sou)',
  // For splitsegs
  2826528: 'polite prefix', // お
  2028980: 'at / in / by', // で
  2028970: 'or / questioning particle', // か
  2028990: 'to / at / in', // に
  2029010: 'indicates direct object of action', // を
  1469800: "indicates possessive (...'s)", // の
  2086960: 'quoting particle', // と
  1002980: 'from / because', // から
};

// ============================================================================
// CACHE INFRASTRUCTURE (Lines 4-10, 54-74)
// ============================================================================

// Line 5-6: def-conn-var *suffix-cache* and *suffix-class*
// These are lazily initialized mutable caches (like Lisp's def-conn-var)
let suffixCache: Map<string, SuffixCacheEntry> | null = null;
let suffixClass: Map<number, string> | null = null;

// Line 72: defvar *init-suffixes-lock*
let initSuffixesLock = false;

// Line 8-10: defun init-suffix-hashtables
function initSuffixHashtables(): void {
  suffixCache = new Map<string, SuffixCacheEntry>();
  suffixClass = new Map<number, string>();
}

// Line 159-160: defun get-suffix-description
export function getSuffixDescription(seq: number): string | null {
  const cls = suffixClass?.get(seq);
  return SUFFIX_DESCRIPTION[cls || seq] || null;
}

// Line 164-166: defun init-suffixes-running-p
function initSuffixesRunningP(): boolean {
  return !suffixCache || initSuffixesLock;
}

// ============================================================================
// SUFFIX INITIALIZATION (Lines 168-316)
// ============================================================================

// Line 168-316: defun init-suffixes-thread
async function initSuffixesThread(): Promise<void> {
  if (!suffixCache || !suffixClass) {
    throw new Error('Suffix caches not initialized');
  }

  const cache = suffixCache;
  const classMap = suffixClass;

  // Line 171-182: update-suffix-cache
  function updateSuffixCache(text: string, newVal: SuffixValue, options: { join?: boolean } = {}): void {
    const old = cache.get(text);

    if (!old) {
      cache.set(text, newVal);
    } else if (options.join) {
      if (Array.isArray(old) && Array.isArray(old[0])) {
        // Already an array of values
        cache.set(text, [newVal, ...(old as SuffixValue[])]);
      } else {
        // Single value, convert to array
        cache.set(text, [newVal, old as SuffixValue]);
      }
    } else {
      // Overwrite
      cache.set(text, newVal);
    }
  }

  // Line 183-185: load-kf
  function loadKf(key: string, kf: KanaText | null, options: { class?: string; text?: string; join?: boolean } = {}): void {
    const text = options.text || (kf ? kf.text : '');
    updateSuffixCache(text, [key, kf], { join: options.join });
    if (kf) {
      classMap.set(kf.seq, options.class || key);
    }
  }

  // Line 186-188: load-conjs
  async function loadConjs(key: string, seq: number, cls?: string, join?: boolean): Promise<void> {
    const forms = await getKanaForms(seq);
    for (const kf of forms) {
      loadKf(key, kf, { class: cls, join });
    }
  }

  // Line 189-190: load-abbr
  function loadAbbr(key: string, text: string, options: { join?: boolean } = {}): void {
    updateSuffixCache(text, [key, null], { join: options.join });
  }

  // Line 192-315: Load all suffixes
  await loadConjs(':chau', 2013800); // ちゃう
  await loadConjs(':chau', 2210750); // ちまう
  loadKf(':chau', await getKanaForm(2028920, 'は'), { class: ':ha', text: 'ちゃ' });
  loadKf(':chau', await getKanaForm(2028920, 'は'), { class: ':ha', text: 'じゃ' });

  await loadConjs(':tai', 2017560);

  // Because suffix た is used up by いる this combination cannot occur, so add separately
  loadKf(':tai', await getKanaForm(900000, 'たそう'), { class: ':tasou' });

  await loadConjs(':ren-', 2772730, ':nikui');

  await loadConjs(':te', 1577985, ':oru'); // おる
  await loadConjs(':te', 1296400, ':aru'); // ある

  // Line 208-213: Special handling for いる (る)
  const iruForms = await getKanaForms(1577980);
  for (const kf of iruForms) {
    const tkf = kf.text;
    cache.set(tkf, [tkf.length > 1 ? ':teiru+' : ':teiru', kf]);
    classMap.set(kf.seq, ':iru');
    if (tkf.length > 1) {
      cache.set(tkf.substring(1), [':teiru', kf]);
    }
  }

  await loadConjs(':te', 1547720, ':kuru'); // くる
  await loadConjs(':te', 1421850, ':oku'); // おく
  await loadConjs(':to', 2108590, ':oku'); // とく
  await loadConjs(':te', 1305380, ':chau'); // しまう
  await loadConjs(':te+space', 1269130, ':kureru'); // くれる
  await loadConjs(':te+space', 1535910, ':morau'); // もらう
  await loadConjs(':te+space', 1587290, ':itadaku'); // いただく

  // Line 226-234: Special handling for いく / く
  const ikuForms = await getKanaForms(1578850);
  for (const kf of ikuForms) {
    const tkf = kf.text;
    const tkfShort = tkf.substring(1);
    const val: SuffixValue = [':te', kf];

    if (tkf.charCodeAt(0) === 0x3044) { // HIRAGANA_LETTER_I
      cache.set(tkf, val);
      classMap.set(kf.seq, ':iku');
      if (!cache.has(tkfShort)) {
        cache.set(tkfShort, val);
      }
    }
  }

  loadKf(':teii', await getKanaForm(2820690, 'いい'), { class: ':ii' });
  loadKf(':teii', await getKanaForm(900001, 'もいい'), { class: ':ii', text: 'もいい' });
  loadKf(':te', await getKanaForm(2028940, 'も'), { class: ':mo' });

  loadKf(':kudasai', await getKanaForm(1184270, 'ください', ':root'));

  await loadConjs(':suru', 1157170); // する
  await loadConjs(':suru', 1421900, ':itasu'); // いたす
  await loadConjs(':suru', 2269820, ':sareru'); // される
  await loadConjs(':suru', 1005160, ':saseru'); // させる

  await loadConjs(':sou', 1006610); // そう
  await loadConjs(':sou+', 2141080); // そうにない

  loadKf(':rou', await getKanaForm(1928670, 'だろう'), { text: 'ろう' });

  await loadConjs(':sugiru', 1195970); // すぎる

  loadKf(':sa', await getKanaForm(2029120, 'さ'));

  loadKf(':ren', await getKanaForm(1008120, 'つつ'), { class: ':tsutsu' });
  await loadConjs(':ren', 2027910, ':tsutsuaru');

  loadKf(':ren', await getKanaForm(1454500, 'うる'), { class: ':uru' });
  const nakuWords = await findWordConjOf('なく', 1529520);
  if (nakuWords.length > 0) {
    // nakuWords can be KanaText or KanjiText, but we only use KanaText
    const nakuKana = nakuWords[0] as KanaText;
    loadKf(':neg', nakuKana, { class: ':nai' });
  }

  await loadConjs(':adv', 1375610, ':naru'); // なる
  await loadConjs(':teren', 1012740, ':yagaru');

  loadKf(':ra', await getKanaForm(2067770, 'ら'));
  await loadConjs(':rashii', 1013240); // らしい

  loadKf(':desu', await getKanaForm(1628500, 'です'));
  loadKf(':desho', await getKanaForm(1008420, 'でしょう'));
  loadKf(':desho', await getKanaForm(1008420, 'でしょ'));

  await loadConjs(':tosuru', 2136890); // とする

  loadKf(':kurai', await getKanaForm(1154340, 'くらい'));
  loadKf(':kurai', await getKanaForm(1154340, 'ぐらい'));

  await loadConjs(':garu', 1631750); // がる

  loadKf(':ren', await getKanaForm(2016470, 'がち'), { class: ':gachi' });
  loadKf(':iadj', await getKanaForm(2006580, 'げ'));
  loadKf(':iadj', await getKanaForm(1604890, 'め'), { class: ':me' });

  loadKf(':ren-', await getKanaForm(2606690, 'がい'), { class: ':gai' });

  // Abbreviations
  loadAbbr(':nai', 'ねえ');
  loadAbbr(':nai', 'ねぇ');
  loadAbbr(':nai', 'ねー');
  loadAbbr(':nai-x', 'ず');
  loadAbbr(':nai-x', 'ざる');
  loadAbbr(':nai-x', 'ぬ');
  loadAbbr(':nai-n', 'ん');

  loadAbbr(':nakereba', 'なきゃ');
  loadAbbr(':nakereba', 'なくちゃ');

  // loadAbbr(':eba', 'や'); // Conflicts with noun + や
  loadAbbr(':teba', 'ちゃ', { join: true }); // つ
  loadAbbr(':reba', 'りゃ'); // る
  loadAbbr(':keba', 'きゃ'); // く
  loadAbbr(':geba', 'ぎゃ'); // ぐ
  loadAbbr(':neba', 'にゃ'); // ぬ
  loadAbbr(':beba', 'びゃ'); // ぶ
  loadAbbr(':meba', 'みゃ'); // む
  loadAbbr(':seba', 'しゃ'); // す

  loadAbbr(':shimashou', 'しましょ');
  loadAbbr(':dewanai', 'じゃない');
  loadAbbr(':ii', 'ええ');
}

// Line 318-324: defun init-suffixes
export async function initSuffixes(options: { blocking?: boolean; reset?: boolean } = {}): Promise<boolean> {
  if (options.reset || !suffixCache) {
    initSuffixHashtables();

    if (initSuffixesLock && !options.blocking) {
      // Already initializing
      return true;
    }

    initSuffixesLock = true;

    try {
      await initSuffixesThread();
    } finally {
      initSuffixesLock = false;
    }
  }

  return initSuffixesRunningP();
}

// ============================================================================
// CACHE ACCESSORS
// ============================================================================

export function getSuffixCache(): Map<string, SuffixCacheEntry> | null {
  return suffixCache;
}

export function getSuffixClass(): Map<number, string> | null {
  return suffixClass;
}
