import type { PredicateContext, Token } from './types.js';
import type { IchiranPos } from './pos.js';
import { PosHelpers } from './pos.js';
import { findWordWithPos } from '@ichiran/core';

export type AsyncPredicateFn = (token: Token, ctx: PredicateContext) => Promise<boolean>;

function normalizeKana(kana: string | string[] | undefined | null): string[] {
  if (!kana) return [];
  return Array.isArray(kana) ? kana : [kana];
}

function makeRegex(pattern: string): RegExp {
  if (pattern.startsWith('/') && pattern.endsWith('/')) {
    return new RegExp(pattern.slice(1, -1));
  }
  return new RegExp(`^${pattern}$`);
}

function toArray<T>(value: T | T[] | undefined): T[] {
  if (value === undefined || value === null) return [];
  return Array.isArray(value) ? value : [value];
}

/**
 * Extract the lemma (dictionary/base form) from a token.
 * Handles various tokenization patterns and normalizes kanji forms to hiragana.
 * 
 * Priority order:
 * 1. grammarInfo.conjugations[0].word (conjugated forms like します, した)
 * 2. grammarInfo.components[0].conjugations[0].word (compound auxiliaries like してください)
 * 3. grammarInfo.word (root forms and compounds)
 * 4. wordInfo.components[0].text (compound structures)
 * 5. wordInfo.text (dictionary entries)
 * 6. token.text (fallback)
 * 
 * Normalization:
 * - 為る → する (kanji suru to hiragana)
 * - 有る → ある (kanji aru to hiragana)
 * - 成る → なる (kanji naru to hiragana)
 * - 来る → くる (kanji kuru to hiragana)
 */
function getLemmaFromToken(token: Token): string | undefined {
  let lemma: string | undefined;
  
  const gi = token.grammarInfo;
  
  // 1. Check conjugations array (most reliable for conjugated forms)
  if (gi && Array.isArray(gi.conjugations) && gi.conjugations.length > 0) {
    const base = gi.conjugations[0];
    if (base?.word && typeof base.word === 'string' && base.word.length > 0) {
      lemma = base.word;
    }
  }
  
  // 2. Check nested components for compound auxiliaries (e.g., してください)
  if (!lemma && gi && Array.isArray(gi.components) && gi.components.length > 0) {
    const comp0 = gi.components[0];
    if (comp0 && Array.isArray(comp0.conjugations) && comp0.conjugations.length > 0) {
      const compBase = comp0.conjugations[0];
      if (compBase?.word && typeof compBase.word === 'string' && compBase.word.length > 0) {
        lemma = compBase.word;
      }
    }
  }
  
  // 3. Fallback to grammarInfo.word (root forms)
  if (!lemma && gi?.word && typeof gi.word === 'string' && gi.word.length > 0) {
    lemma = gi.word;
  }
  
  // 4. Fallback to wordInfo.components[0].text
  if (!lemma) {
    const comp0 = token.wordInfo?.components?.[0]?.text;
    if (comp0 && comp0.length > 0) {
      lemma = comp0;
    }
  }
  
  // 5. Fallback to wordInfo.text
  if (!lemma) {
    const dictText = token.wordInfo?.text;
    if (dictText && dictText.length > 0) {
      lemma = dictText;
    }
  }
  
  // 6. Final fallback to token text
  if (!lemma) {
    lemma = token.text;
  }
  
  // Normalize common kanji forms to hiragana for consistent matching
  // This handles cases where ichiran returns kanji lemmas (為る, 有る, etc.)
  return normalizeLemma(lemma);
}

/**
 * Normalize kanji verb forms to their hiragana equivalents.
 * This ensures consistent lemma matching regardless of whether
 * ichiran returns 為る or する, 有る or ある, etc.
 * 
 * Exported for use in pattern matching and other modules.
 */
export function normalizeLemma(lemma: string): string {
  const kanjiToKana: Record<string, string> = {
    '為る': 'する',
    '有る': 'ある',
    '在る': 'ある',
    '成る': 'なる',
    '来る': 'くる',
    '無い': 'ない',
    '居る': 'いる',
    '呉れる': 'くれる',
    '下さる': 'くださる',
    '御座る': 'ござる',
    // Normalize いい/よい/良い variations to 'いい'
    'よい': 'いい',
    '良い': 'いい',
  };
  
  return kanjiToKana[lemma] || lemma;
}

const predicateFactories: Record<string, (arg?: string) => AsyncPredicateFn> = {
  // NOTE: "not" is also an available predicate, just not defined here
  text: (value) => {
    if (!value) return async () => false;
    const regex = makeRegex(value);
    return async (token) => regex.test(token.text);
  },
  isCaseParticle: () => async (token) => {
    // Core case/topic particles used in NPCase
    // Keep aligned with compiler's NPCase macro intent
    // Includes comparative particles より/よりも for patterns like AよりB
    const pos = toArray(token.grammarInfo?.partOfSpeech);
    if (!pos.some(p => PosHelpers.isParticle(p))) return false;
    const text = token.text;
    // Same set previously enumerated in NPCase, plus comparative particles
    const CASE_PARTICLES = new Set(['は', 'が', 'を', 'に', 'で', 'も', 'と', 'へ', 'から', 'まで', 'より', 'よりも']);
    return CASE_PARTICLES.has(text);
  },
  kana: (pattern) => {
    if (!pattern) return async () => false;
    const regex = makeRegex(pattern);
    return async (token) => normalizeKana(token.wordInfo?.kana).some(k => regex.test(k));
  },
  pos: (value) => {
    if (!value) return async () => false;
    return async (token) => toArray(token.grammarInfo?.partOfSpeech).some((p: IchiranPos | string) => p === value);
  },
  lemma: (value) => {
    if (!value) return async () => false;
    const normalized = normalizeLemma(value.trim());
    return async (token) => getLemmaFromToken(token) === normalized;
  },
  lemmaNeg: (value) => {
    // Matches lemma AND checks that it has negative polarity (neg=true)
    // Example: lemmaNeg:ある matches ありません, ありませんでした but NOT あります
    if (!value) return async () => false;
    const normalized = normalizeLemma(value.trim());
    return async (token) => {
      if (getLemmaFromToken(token) !== normalized) return false;
      
      // Check if this has negative polarity
      const conjInfo = token.grammarInfo?.conjugations;
      if (!conjInfo || !Array.isArray(conjInfo) || conjInfo.length === 0) return false;
      
      return conjInfo.some(c => c.neg === true);
    };
  },
  isNegative: () => async (token) => {
    // Check if token has negative polarity (neg=true)
    // Works for any conjugated form that includes negation
    const conjInfo = token.grammarInfo?.conjugations;
    if (!conjInfo || !Array.isArray(conjInfo) || conjInfo.length === 0) return false;
    
    return conjInfo.some(c => c.neg === true);
  },
  conj: (value) => {
    if (!value) return async () => false;
    return async (token) => {
      const conj = token.wordInfo?.conjugations;
      if (!conj) return false;
      if (conj === ':root') return value === ':root';
      const num = Number(value);
      return Array.isArray(conj) ? conj.includes(num) : false;
    };
  },
  type: (value) => {
    if (!value) return async () => false;
    return async (token) => token.wordInfo?.type === value;
  },
  isTopicMarker: () => async (token) => {
    // Matches は and compound topic markers like には, では, etc.
    // Also matches って (colloquial topic marker)
    // These are expressions that end in は and function as topic markers
    const pos = toArray(token.grammarInfo?.partOfSpeech);
    return (token.text === 'は' && pos.some(p => PosHelpers.isParticle(p))) ||
           (token.text === 'って' && pos.some(p => PosHelpers.isParticle(p))) ||
           (token.text.endsWith('は') && pos.some(p => p === 'exp' || PosHelpers.isParticle(p)));
  },
  hasVerbalConjugation: () => async (token) => {
    // Check if POS includes a verb tag
    const pos = toArray(token.grammarInfo?.partOfSpeech);
    if (pos.some(p => PosHelpers.isVerb(p))) return true;

    // Also match tokens with verbal conjugations but empty/missing POS
    // Example: できなかった has conj:[5] (negative past) but may have empty POS
    const conj = token.wordInfo?.conjugations;
    if (conj && conj !== ':root' && Array.isArray(conj) && conj.length > 0) {
      // Has conjugation data, so it's a verbal form
      return true;
    }

    return false;
  },
  isAdjective: () => async (token) => toArray(token.grammarInfo?.partOfSpeech).some(p => PosHelpers.isAdjective(p)),
  isAdverb: () => async (token) => toArray(token.grammarInfo?.partOfSpeech).some(p => PosHelpers.isAdverb(p)),
  isAdverbModifier: () => async (token) => {
    // Adverb modifiers are adverbs or i-adjectives in their ku-form (conj 13)
    const pos = toArray(token.grammarInfo?.partOfSpeech);
    if (pos.some(p => PosHelpers.isAdverb(p))) return true;
    
    // Check for ku-form of i-adjectives (conjugation 13)
    const conj = token.wordInfo?.conjugations;
    if (Array.isArray(conj) && conj.includes(13)) {
      return pos.some(p => PosHelpers.isIAdjective(p));
    }
    return false;
  },
  isAuxiliary: () => async (token) => toArray(token.grammarInfo?.partOfSpeech).some(p => PosHelpers.isAuxiliary(p)),
  isIAdjective: () => async (token) => {
    // Exclude な - it's tagged as adj-i but is actually the na-adjective auxiliary particle
    if (token.text === 'な') return false;
    return toArray(token.grammarInfo?.partOfSpeech).some(p => PosHelpers.isIAdjective(p));
  },
  isNaAdjective: () => async (token) => toArray(token.grammarInfo?.partOfSpeech).some(p => PosHelpers.isNaAdjective(p)),
  isPredicateHead: () => async (token) => {
    // Predicate heads are verbs, adjectives, or copulas in their root form
    const pos = toArray(token.grammarInfo?.partOfSpeech);
    const isPredicatePos = pos.some(p =>
      PosHelpers.isVerb(p) ||
      PosHelpers.isAdjective(p) ||
      PosHelpers.isCopula(p)
    );
    if (!isPredicatePos) return false;

    // Check if in root form
    const conj = token.wordInfo?.conjugations;
    return conj === ':root' || conj === undefined || conj === null;
  },
  isPredicate: () => async (token) => {
    // Predicates are verbs, adjectives, auxiliaries, or adverbs in ANY conjugation form
    // Unlike isPredicateHead, this accepts conjugated forms (polite, past, negative, etc.)
    // This is used for matching full predicate phrases including auxiliaries and adverbial forms
    const pos = toArray(token.grammarInfo?.partOfSpeech);
    return pos.some(p =>
      PosHelpers.isVerb(p) ||
      PosHelpers.isAdjective(p) ||
      PosHelpers.isCopula(p) ||
      PosHelpers.isAuxiliary(p) ||  // Include auxiliaries like です, だ, た, etc.
      PosHelpers.isAdverb(p)  // Include adverbs like 早く (ku-form of adjectives)
    );
  },
  isNominalHead: () => async (token) => {
    // Tokens that can function as the HEAD of a noun phrase
    // - Nouns, pronouns, adj-no
    // - Temporal expressions (tagged as n,adv like 今日, 明日, 今年)
    const pos = toArray(token.grammarInfo?.partOfSpeech);
    return pos.some(p =>
      PosHelpers.isNoun(p) ||
      PosHelpers.isPronoun(p) ||
      p === 'adj-no'
    );
  },
  isNounModifier: () => async (token) => {
    // Tokens that can MODIFY nouns within a noun phrase
    // - Adjectives (i-adjectives, na-adjectives)
    // - Prenominal adjectives (連体詞 - この, その, あの, etc.)
    // - Numerals and counters
    // - adj-no when used attributively
    const pos = toArray(token.grammarInfo?.partOfSpeech);
    return pos.some(p =>
      PosHelpers.isIAdjective(p) ||
      PosHelpers.isNaAdjective(p) ||
      PosHelpers.isPrenounAdjective(p) ||
      p === 'num' ||
      p === 'ctr' ||
      p === 'adj-no'
    );
  },
  isNounConnector: () => async (token) => {
    // の particle that connects nouns in compound phrases (会社の代表番号)
    return token.text === 'の' && toArray(token.grammarInfo?.partOfSpeech).some(p => PosHelpers.isParticle(p));
  },
  isNounPhrase: () => async (token, context) => {
    // Tokens that can function as a complete, standalone noun phrase
    // 1. Nominal heads (nouns, pronouns, adj-no)
    // 2. Pronominal の (の acting as pronoun substitute: 弟の = brother's [one])
    
    const pos = toArray(token.grammarInfo?.partOfSpeech);
    
    // Check if it's a nominal head
    const isHead = pos.some(p =>
      PosHelpers.isNoun(p) ||
      PosHelpers.isPronoun(p) ||
      p === 'adj-no'
    );
    
    if (isHead) return true;
    
    // Check if it's pronominal の
    // Pronominal の appears when の is followed by clause-level markers
    if (token.text === 'の' && pos.some(p => PosHelpers.isParticle(p))) {
      const nextToken = context.tokens[context.index + 1];
      if (!nextToken) return true; // End of sentence
      
      // Clause-level markers that can follow pronominal の
      const clauseMarkers = ['は', 'が', 'を', 'に', 'で', 'より', 'よりも', 'か', 'も', 'と', 'や', 'へ', 'から', 'まで', 'だ', 'です', 'だった', 'でした'];
      if (clauseMarkers.includes(nextToken.text)) return true;
      
      // Punctuation or end markers
      if (!nextToken.wordInfo || nextToken.text.match(/^[。、！？\.\,\!\?]$/)) return true;
    }
    
    return false;
  },
  isParticle: () => async (token) => toArray(token.grammarInfo?.partOfSpeech).some(p => PosHelpers.isParticle(p)),
  isQuestionParticle: () => async (token) => {
    // か is the question particle
    return token.text === 'か' && toArray(token.grammarInfo?.partOfSpeech).some(p => PosHelpers.isParticle(p));
  },
  isCounter: () => async (token) => {
    // Counters have POS tag 'ctr'
    return toArray(token.grammarInfo?.partOfSpeech).some(p => p === 'ctr');
  },
  isOrdinal: () => async (token) => {
    // Ordinals are typically numeric with 番目 (banme) suffix or counter
    return token.text.includes('番目') || 
           (token.text.includes('第') && toArray(token.grammarInfo?.partOfSpeech).some(p => p === 'num' || p === 'pref'));
  },
  isIAdjKuForm: () => async (token) => {
    // Ku-form of i-adjectives (adverbial form ending in く)
    // These are stored as separate dictionary entries, not as conjugations
    // They can have POS: adj-i, adj-ix (irregular like よく), or adv

    // Check if token ends with く
    const text = token.text;
    const kana = normalizeKana(token.wordInfo?.kana);
    const endsWithKu = text.endsWith('く') || kana.some(k => k.endsWith('く'));
    if (!endsWithKu) return false;

    // Check if POS is i-adjective or adverb (ku-forms are sometimes stored as adverbs)
    const pos = toArray(token.grammarInfo?.partOfSpeech);
    return pos.some(p => PosHelpers.isIAdjective(p) || PosHelpers.isAdverb(p));
  },
  isIAdjKunaiForm: () => async (token) => {
    // Negative form of i-adjectives
    // Check if this is a conjugation with neg=true flag
    const conjInfo = token.grammarInfo?.conjugations;
    if (!conjInfo || !Array.isArray(conjInfo) || conjInfo.length === 0) return false;
    
    // Check if any conjugation has neg=true and base is i-adjective
    return conjInfo.some(c => 
      c.neg === true &&
      toArray(c.partOfSpeech).some(p => PosHelpers.isIAdjective(p))
    );
  },
  isIAdjKuteForm: () => async (token) => {
    // Te-form of i-adjectives (Conjunctive ~te)
    // Check if this token IS a conjugation with type "Conjunctive (~te)"
    const conjInfo = token.grammarInfo?.conjugations;
    if (!conjInfo || !Array.isArray(conjInfo) || conjInfo.length === 0) return false;
    
    // Check if any conjugation has "Conjunctive" type and base is i-adjective
    return conjInfo.some(c => 
      c.conjugationTypes?.some(type => type.includes('Conjunctive')) &&
      toArray(c.partOfSpeech).some(p => PosHelpers.isIAdjective(p))
    );
  },
  isCopulaDe: () => async (token) => {
    // Check if token is で as the conjunctive (~te) form of the copula だ
    // Used after な-adjectives and nouns to form connectives
    const conjInfo = token.grammarInfo?.conjugations;
    if (!conjInfo || !Array.isArray(conjInfo) || conjInfo.length === 0) return false;
    
    return conjInfo.some(c => 
      c.conjugationTypes?.some(type => type.includes('Conjunctive')) &&
      toArray(c.partOfSpeech).some(p => PosHelpers.isCopula(p))
    );
  },
  isSuruVerb: () => async (token) => {
    // Suru verbs (vs, vs-i, vs-s, vs-c)
    // Also includes conjugated forms of suru that start with し (shi)
    // like したい, しない, しすぎる, した, して, します, etc.
    const pos = toArray(token.grammarInfo?.partOfSpeech);

    // Direct suru verb POS
    if (pos.some(p => p === 'vs' || p === 'vs-i' || p === 'vs-s' || p === 'vs-c')) {
      return true;
    }

    // Conjugated forms of suru (start with し)
    const text = token.text;
    const kana = normalizeKana(token.wordInfo?.kana);
    const startsWithShi = text.startsWith('し') || kana.some(k => k.startsWith('し'));

    if (startsWithShi) {
      // Must be a verb, auxiliary, or adjective (for -tai forms)
      return pos.some(p =>
        PosHelpers.isVerb(p) ||
        PosHelpers.isAuxiliary(p) ||
        PosHelpers.isAdjective(p)  // For したい which is marked as adj-i
      );
    }

    return false;
  },
  isIAdjKuVerbCompound: () => async (token) => {
    // Compound verb tokens that contain ku-form + verb (くなる, くする, etc.)
    // Examples: 長くなる, 早くなった, よくする, 明るくした, etc.
    // EXCLUDES: くない (ku-nai negative) which is adjective negation, not verb modification
    const text = token.text;
    const kana = normalizeKana(token.wordInfo?.kana);
    const pos = toArray(token.grammarInfo?.partOfSpeech);

    // Must be a verb or adjective (some compounds are tagged as adjectives)
    const isVerbLike = pos.some(p =>
      PosHelpers.isVerb(p) ||
      PosHelpers.isAdjective(p) ||
      PosHelpers.isAuxiliary(p)
    );
    if (!isVerbLike) return false;

    // Exclude くない pattern (negative adjective, not verb modification)
    // Examples: 高くない, 暗くなかった
    const containsKuNai = text.includes('くない') || kana.some(k => k.includes('くない'));
    if (containsKuNai) return false;

    // Check if text contains くな pattern (ku + naru)
    // Examples: 長くなる, 暗くなった, 涼しくなりました
    // Must check AFTER excluding くない to avoid false positives
    const containsKuNa = text.includes('くな') || kana.some(k => k.includes('くな'));
    if (containsKuNa) return true;

    // Check if text contains くし pattern (ku + suru)
    // Examples: 明るくする, よくした, 大きくしない
    // Also: くなくし pattern (double negative: make NOT adjective)
    const containsKuShi = text.includes('くし') || kana.some(k => k.includes('くし'));
    if (containsKuShi) return true;

    return false;
  },
  isIAdjKuSuruCompound: () => async (token) => {
    // Specialized predicate for い-adjective + する compounds only
    // This is a subset of isIAdjKuVerbCompound but specifically for する verbs
    // Examples: 優しくする, よくする, くなくする (double negative)
    // Used in patterns that specifically match the causative くする construction
    const text = token.text;
    const kana = normalizeKana(token.wordInfo?.kana);
    const pos = toArray(token.grammarInfo?.partOfSpeech);
    const conjugations = toArray(token.grammarInfo?.conjugations);

    // Must be a verb-like token
    const isVerbLike = pos.some(p =>
      PosHelpers.isVerb(p) ||
      PosHelpers.isAdjective(p) ||
      PosHelpers.isAuxiliary(p)
    );
    if (!isVerbLike) return false;

    // Check for くし pattern (ku + shi from suru)
    // OR くなくし pattern (ku + naku + shi for double negative)
    const containsKuShi = text.includes('くし') || kana.some(k => k.includes('くし'));
    if (!containsKuShi) return false;

    // Additional check: must have conjugation data that links to する
    // This prevents false positives like 拍手 (はくしゅ) which contains くし but is not a くする form
    if (conjugations.length === 0) {
      // No conjugation data - reject unless this looks like a compound that ends with する conjugation
      // Check if text ends with する-like patterns (した, して, します, etc.)
      const endsWithSuruForm = /[くく](?:し|す|せ|さ)[あいうえおんたてますだでだったれろよかっきけくなどもば]/.test(text);
      if (!endsWithSuruForm) return false;
    }

    // If we have conjugation data, verify at least one has lemma する
    if (conjugations.length > 0) {
      const hasSuruLemma = conjugations.some(c => c.word === 'する');
      if (!hasSuruLemma) return false;
    }

    return true;
  },
  isVerbStemOrAuxiliary: () => async (token) => {
    // Matches verb stems (masu-stem), verb suffixes, noun+suru compounds, OR auxiliaries
    // Verb stems are often tagged as nouns (like 切り from 切る)
    // This allows matching patterns like: 薄く + 切り + にくい OR よく + 勉強しました
    const text = token.text;
    const kana = normalizeKana(token.wordInfo?.kana);
    const pos = toArray(token.grammarInfo?.partOfSpeech);

    // Check if it's an auxiliary
    if (pos.some(p => PosHelpers.isAuxiliary(p))) return true;

    // Check if it's a verb in any form
    if (pos.some(p => PosHelpers.isVerb(p))) return true;

    // Check for common verb suffixes by text (these can be tagged as suf or adj-i)
    // にくい = hard to do, やすい = easy to do, がたい = hard to do (formal),
    // づらい = hard to do, すぎる = too much, たい = want to
    const verbSuffixes = ['にくい', 'やすい', 'がたい', 'づらい', 'すぎる', 'たい'];
    if (verbSuffixes.some(s => text === s || kana.some(k => k === s))) {
      return true;
    }

    // Check for noun+suru compounds (tagged as noun but contain suru conjugations)
    // Examples: 勉強しました, 勉強します, 勉強した, 勉強して, etc.
    // These contain specific し patterns like しま, した, して, しな, etc.
    if (pos.some(p => p === 'n')) {
      // More specific patterns for suru conjugations
      const suruPatterns = ['しま', 'した', 'して', 'しな', 'しよ', 'しろ', 'せよ'];
      const containsSuruPattern = suruPatterns.some(p =>
        text.includes(p) || kana.some(k => k.includes(p))
      );
      if (containsSuruPattern) {
        // Likely a noun+suru compound
        return true;
      }

      // Check for masu-stem nouns
      // Masu-stems are verb stems that end in -i/-ri/-ki/-shi/-chi/-ni/-bi/-mi sounds
      // They're often tagged as nouns but function as verb stems
      // Examples: 切り (kiri from 切る), 読み (yomi from 読む), 話し (hanashi from 話す)
      const endsWithMasuStem = text.endsWith('り') || text.endsWith('い') ||
                                text.endsWith('き') || text.endsWith('し') ||
                                text.endsWith('ち') || text.endsWith('に') ||
                                text.endsWith('び') || text.endsWith('み');
      if (endsWithMasuStem && kana.length > 0) {
        // Additional check: kana should end with appropriate hiragana
        const lastKana = kana[0].slice(-1);
        const masuStemEndings = ['り', 'い', 'き', 'し', 'ち', 'に', 'び', 'み'];
        if (masuStemEndings.includes(lastKana)) {
          return true;
        }
      }
    }

    return false;
  },
  isNaruVerb: () => async (token) => {
    // Check if this is the verb なる (naru) in any conjugated form
    // First check if it's a verb
    const pos = toArray(token.grammarInfo?.partOfSpeech);
    if (!pos.some(p => PosHelpers.isVerb(p))) return false;
    
    // Check conjugation base if available
    const conjInfo = token.grammarInfo?.conjugations;
    if (conjInfo && Array.isArray(conjInfo) && conjInfo.length > 0) {
      // Check if base word is なる/成る
      const hasNaruBase = conjInfo.some(c => {
        const reading = c.reading || '';
        return c.word === 'なる' || c.word === '成る' || reading === 'なる';
      });
      if (hasNaruBase) return true;
    }
    
    // Fallback: check if text contains なる or なり pattern
    const text = token.text;
    const kana = normalizeKana(token.wordInfo?.kana);
    return text.includes('なる') || text.includes('なり') ||
           kana.some(k => k.includes('なる') || k.includes('なり'));
  },
  isSpatialNoun: () => async (token) => {
    // Nouns that denote absolute spatial positions/directions (not gradable)
    // These are typically used with 一番 to mean "the very front/top/etc" (non-superlative)
    // rather than "the most ..." (superlative)
    const spatialNouns = [
      // Basic directions
      '前', '後', '上', '下', '外', '内', '奥', '手前', '端', '先', '横', '隣', '側',
      // Left/right, sides
      '右', '左', '右辺', '左辺', '右側', '左側', '向こう側', '両側',
      // Cardinal directions
      '東', '西', '南', '北', '東側', '西側', '南側', '北側',
      // Parts/sections
      '前部', '後部', '内側', '外側', '内部', '外部', '中心', '周り', '隅', '縁',
      // Faces/surfaces
      '面', '前面', '後面', '正面', '背面', '表面', '裏面', '側面', '裏',
      // Directions/orientations
      '前方', '後方', '方', '方向',
      // Proximity
      '元', '足元', '手元', '近く', '遠く', '辺', '辺り', 'そば', 'かたわら', '所',
      // Demonstratives (spatial)
      'こちら', 'そちら', 'あちら', 'どちら', '向こう',
    ];
    const pos = toArray(token.grammarInfo?.partOfSpeech);
    return spatialNouns.includes(token.text) && pos.some(p => PosHelpers.isNoun(p));
  },
  isGradableElement: () => async (token) => {
    // Elements that can be modified by degree adverbs like 一番
    // This includes adjectives, verbs, adverbs, and predicates
    // Excludes spatial nouns which don't have degrees
    const pos = toArray(token.grammarInfo?.partOfSpeech);
    
    // Check for gradable POS
    const isGradablePos = pos.some(p => 
      PosHelpers.isAdjective(p) || 
      PosHelpers.isVerb(p) || 
      PosHelpers.isAdverb(p)
    );
    
    if (isGradablePos) return true;
    
    // Check for adverbial modifiers (ku-form, ni-form)
    const conj = token.wordInfo?.conjugations;
    if (Array.isArray(conj)) {
      // Ku-form (13) or Ni-form (50) are adverbial/gradable
      if (conj.includes(13) || conj.includes(50)) return true;
    }
    
    // Auxiliaries and predicates are gradable
    if (pos.some(p => PosHelpers.isAuxiliary(p))) return true;
    
    return false;
  },
  prefixHasPos: (arg) => {
    if (!arg) return async () => false;
    
    // Parse format: suffix:pos1,pos2,pos3
    const parts = arg.split(':');
    if (parts.length !== 2) {
      throw new Error(`prefixHasPos requires format 'suffix:pos', got: ${arg}`);
    }
    
    const [suffix, posString] = parts;
    const posList = posString.split(',');
    return async (token) => checkPrefixHasPos(token, suffix, posList);
  },
};

// Cache for prefix lookups to avoid repeated database queries
const prefixPosCache = new Map<string, boolean>();

/**
 * Async predicate: Check if the prefix before a suffix has any of the specified POS tags.
 * Format: prefixHasPos:suffix:pos1,pos2,pos3
 * Example: prefixHasPos:になる:adj-na or prefixHasPos:んです:v,adj-i,aux
 * 
 * This strips the suffix from the token's kana, looks up the prefix in the dictionary,
 * and checks if it has any of the specified parts of speech.
 */
async function checkPrefixHasPos(token: Token, suffix: string, posList: string[]): Promise<boolean> {
  // Get the base/dictionary form of the word
  // If it's a conjugation, use the base form from conjugations[0].word
  // Otherwise use the token text directly
  const baseForm = token.grammarInfo?.conjugations?.[0]?.word || token.text;
  const baseKana = token.grammarInfo?.conjugations?.[0]?.reading || normalizeKana(token.wordInfo?.kana)[0];
  
  // Check if the base form ends with the suffix (e.g., "好きになる" ends with "になる")
  if (!baseKana?.endsWith(suffix)) {
    return false;
  }
  
  // Extract the prefix from the text/kanji form (e.g., "好きになる" - "になる" = "好き")
  // We use the text form (not kana) because kanji like 気 vs 好き have different meanings
  if (!baseForm.endsWith(suffix)) {
    return false;
  }
  
  const prefix = baseForm.substring(0, baseForm.length - suffix.length);
  if (prefix.length === 0) return false;
  
  // Check if the prefix has any of the specified POS tags
  for (const pos of posList) {
    // Check cache first
    const cacheKey = `${prefix}:${pos}`;
    if (prefixPosCache.has(cacheKey)) {
      if (prefixPosCache.get(cacheKey)) {
        return true;
      }
      continue;
    }
    
    // Look up the prefix in the dictionary
    try {
      const results = await findWordWithPos(prefix, pos);
      const found = results.length > 0;
      prefixPosCache.set(cacheKey, found);
      if (found) {
        return true;
      }
    } catch (error) {
      prefixPosCache.set(cacheKey, false);
    }
  }
  
  return false;
}

export function resolvePredicate(spec: string): AsyncPredicateFn {
  // Split only on the first colon to handle cases like "conj::root" or "prefixHasPos:になる:adj-na"
  const colonIndex = spec.indexOf(':');
  const name = colonIndex === -1 ? spec : spec.slice(0, colonIndex);
  const arg = colonIndex === -1 ? undefined : spec.slice(colonIndex + 1);
  
  const factory = predicateFactories[name];
  if (!factory) {
    throw new Error(`Unknown predicate: ${spec}`);
  }
  return factory(arg);
}

export async function evaluatePredicates(token: Token, ctx: PredicateContext, predicates: AsyncPredicateFn[]): Promise<boolean> {
  for (const fn of predicates) {
    const passed = await fn(token, ctx);
    if (!passed) return false;
  }
  return true;
}
