// ichiran/characters - Character classification and kana/kanji utilities
// Port of characters.lisp

// Line 3: defparameter *sokuon-characters*
export const SOKUON_CHARACTERS = { sokuon: "っッ" };
// Line 5: defparameter *iteration-characters*
export const ITERATION_CHARACTERS = { iter: "ゝヽ", iterV: "ゞヾ" };

// Line 7-9: defparameter *modifier-characters*
export const MODIFIER_CHARACTERS = {
  "+a": "ぁァ", "+i": "ぃィ", "+u": "ぅゥ", "+e": "ぇェ", "+o": "ぉォ",
  "+ya": "ゃャ", "+yu": "ゅュ", "+yo": "ょョ", "+wa": "ゎヮ",
  longVowel: "ー"
};

// Line 11-30: defparameter *kana-characters*
export const KANA_CHARACTERS = {
  a: "あア", i: "いイ", u: "うウ", e: "えエ", o: "おオ",
  ka: "かカ", ki: "きキ", ku: "くク", ke: "けケ", ko: "こコ",
  sa: "さサ", shi: "しシ", su: "すス", se: "せセ", so: "そソ",
  ta: "たタ", chi: "ちチ", tsu: "つツ", te: "てテ", to: "とト",
  na: "なナ", ni: "にニ", nu: "ぬヌ", ne: "ねネ", no: "のノ",
  ha: "はハ", hi: "ひヒ", fu: "ふフ", he: "へヘ", ho: "ほホ",
  ma: "まマ", mi: "みミ", mu: "むム", me: "めメ", mo: "もモ",
  ya: "やヤ", yu: "ゆユ", yo: "よヨ",
  ra: "らラ", ri: "りリ", ru: "るル", re: "れレ", ro: "ろロ",
  wa: "わワ", wi: "ゐヰ", we: "ゑヱ", wo: "をヲ",
  n: "んン",
  ga: "がガ", gi: "ぎギ", gu: "ぐグ", ge: "げゲ", go: "ごゴ",
  za: "ざザ", ji: "じジ", zu: "ずズ", ze: "ぜゼ", zo: "ぞゾ",
  da: "だダ", dji: "ぢヂ", dzu: "づヅ", de: "でデ", do: "どド",
  ba: "ばバ", bi: "びビ", bu: "ぶブ", be: "べベ", bo: "ぼボ",
  pa: "ぱパ", pi: "ぴピ", pu: "ぷプ", pe: "ぺペ", po: "ぽポ",
  vu: "ゔヴ"
};

// Line 32-35: defparameter *all-characters*
export const ALL_CHARACTERS = {
  ...SOKUON_CHARACTERS,
  ...ITERATION_CHARACTERS,
  ...MODIFIER_CHARACTERS,
  ...KANA_CHARACTERS
};

// Line 37-42: defparameter *char-class-hash*
export const CHAR_CLASS_HASH = new Map<string, string>();
for (const [charClass, chars] of Object.entries(ALL_CHARACTERS)) {
  for (const char of chars) {
    CHAR_CLASS_HASH.set(char, charClass);
  }
}

// Line 44-45: defun get-char-class
export function getCharClass(char: string): string {
  return CHAR_CLASS_HASH.get(char) ?? char;
}

// Line 47-53: defun long-vowel-modifier-p
export function longVowelModifierP(modifier: string, prevChar: string): boolean {
  const vowelMap: Record<string, string> = {
    "+a": "A", "+i": "I", "+u": "U", "+e": "E", "+o": "O"
  };
  const vowel = vowelMap[modifier];
  if (!vowel) return false;

  const charClass = getCharClass(prevChar);
  if (typeof charClass !== 'string' || charClass === prevChar) return false;

  return vowel === charClass[charClass.length - 1].toUpperCase();
}

// Line 63-68: hash-from-list *dakuten-hash*
export const DAKUTEN_HASH = new Map<string, string>([
  ["ka", "ga"], ["ki", "gi"], ["ku", "gu"], ["ke", "ge"], ["ko", "go"],
  ["sa", "za"], ["shi", "ji"], ["su", "zu"], ["se", "ze"], ["so", "zo"],
  ["ta", "da"], ["chi", "dji"], ["tsu", "dzu"], ["te", "de"], ["to", "do"],
  ["ha", "ba"], ["hi", "bi"], ["fu", "bu"], ["he", "be"], ["ho", "bo"],
  ["u", "vu"]
]);

// Line 70-71: hash-from-list *handakuten-hash*
export const HANDAKUTEN_HASH = new Map<string, string>([
  ["ha", "pa"], ["hi", "pi"], ["fu", "pu"], ["he", "pe"], ["ho", "po"]
]);

// Line 73-79: hash-from-list *undakuten-hash*
export const UNDAKUTEN_HASH = new Map<string, string>([
  ["ga", "ka"], ["gi", "ki"], ["gu", "ku"], ["ge", "ke"], ["go", "ko"],
  ["za", "sa"], ["ji", "shi"], ["zu", "su"], ["ze", "se"], ["zo", "so"],
  ["da", "ta"], ["dji", "chi"], ["dzu", "tsu"], ["de", "te"], ["do", "to"],
  ["ba", "ha"], ["bi", "hi"], ["bu", "fu"], ["be", "he"], ["bo", "ho"],
  ["pa", "ha"], ["pi", "hi"], ["pu", "fu"], ["pe", "he"], ["po", "ho"],
  ["vu", "u"]
]);

// Line 81-83: defun voice-char
export function voiceChar(cc: string): string {
  return DAKUTEN_HASH.get(cc) ?? cc;
}

// Line 85-91: defparameter *punctuation-marks*
export const PUNCTUATION_MARKS: [string, string][] = [
  ["【", " ["], ["】", "] "],
  ["、", ", "], ["，", ", "],
  ["。", ". "], ["・・・", "... "], ["・", " "], ["　", " "],
  ["「", ' "'], ["」", '" '], ["゛", '"'],
  ["『", " «"], ["』", "» "],
  ["〜", " - "], ["：", ": "], ["！", "! "], ["？", "? "], ["；", "; "]
];

// Line 93-101: defun dakuten-join
function dakutenJoin(dakutenHash: Map<string, string>, char: string): [string, string][] {
  const result: [string, string][] = [];
  for (const [cc, ccd] of dakutenHash.entries()) {
    let kc = (KANA_CHARACTERS as any)[cc];
    let kcd = (KANA_CHARACTERS as any)[ccd];
    if (!kc || !kcd) continue;

    const offset = kc.length - kcd.length;
    if (offset > 0) kc = kc.slice(offset);

    for (let idx = 0; idx < kc.length; idx++) {
      result.push([kc[idx] + char, kcd[idx]]);
    }
  }
  return result;
}

// Line 103-104: defparameter *dakuten-join*
export const DAKUTEN_JOIN = [
  ...dakutenJoin(DAKUTEN_HASH, "゛"),
  ...dakutenJoin(HANDAKUTEN_HASH, "゜")
];

// Line 106-107: defparameter *half-width-kana* and *full-width-kana*
export const HALF_WIDTH_KANA = "･ｦｧｨｩｪｫｬｭｮｯｰｱｲｳｴｵｶｷｸｹｺｻｼｽｾｿﾀﾁﾂﾃﾄﾅﾆﾇﾈﾉﾊﾋﾌﾍﾎﾏﾐﾑﾒﾓﾔﾕﾖﾗﾘﾙﾚﾛﾜﾝﾞﾟ";
export const FULL_WIDTH_KANA = "・ヲァィゥェォャュョッーアイウエオカキクケコサシスセソタチツテトナニヌネノハヒフヘホマミムメモヤユヨラリルレロワン゛゜";

// Line 109-112: defparameter *abnormal-chars*
export const ABNORMAL_CHARS =
  "０１２３４５６７８９ａｂｃｄｅｆｇｈｉｊｋｌｍｎｏｐｑｒｓｔｕｖｗｘｙｚＡＢＣＤＥＦＧＨＩＪＫＬＭＮＯＰＱＲＳＴＵＶＷＸＹＺ＃＄％＆（）＊＋／〈＝〉？＠［］＾＿'｛｜｝～" +
  HALF_WIDTH_KANA;

// Line 114-116: defparameter *normal-chars*
export const NORMAL_CHARS =
  "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ#$%&()*+/<=>?@[]^_`{|}~" +
  FULL_WIDTH_KANA;

// Line 118-129: Regex patterns
export const KATAKANA_REGEX = /[ァ-ヺヽヾー]/;
export const KATAKANA_UNIQ_REGEX = /[ァ-ヺヽヾ]/;
export const HIRAGANA_REGEX = /[ぁ-ゔゝゞー]/;
export const KANJI_REGEX = /[々ヶ〆一-龯]/;
export const KANJI_CHAR_REGEX = /[一-龯]/;
export const NONWORD_REGEX = /[^々ヶ〆一-龯ァ-ヺヽヾぁ-ゔゝゞー〇]/;
export const NUMERIC_REGEX = /[0-9０-９〇一二三四五六七八九零壱弐参拾十百千万億兆京]/;
export const NUM_WORD_REGEX = /[0-9０-９〇々ヶ〆一-龯ァ-ヺヽヾぁ-ゔゝゞー]/;
export const WORD_REGEX = /[々ヶ〆一-龯ァ-ヺヽヾぁ-ゔゝゞー〇]/;
export const DIGIT_REGEX = /[0-9０-９〇]/;
export const DECIMAL_POINT_REGEX = /[.,]/;

// Line 131-134: defparameter *basic-split-regex*
export const BASIC_SPLIT_REGEX = /(((?<![.,]|[0-9０-９〇])[0-9０-９〇]+|[々ヶ〆一-龯ァ-ヺヽヾぁ-ゔゝゞー〇])[0-9０-９〇々ヶ〆一-龯ァ-ヺヽヾぁ-ゔゝゞー]*[々ヶ〆一-龯ァ-ヺヽヾぁ-ゔゝゞー〇]|[々ヶ〆一-龯ァ-ヺヽヾぁ-ゔゝゞー〇])/g;

export type CharClass =
  | "katakana" | "katakana-uniq" | "hiragana" | "kanji" | "kanji-char"
  | "kana" | "traditional" | "nonword" | "number";

// Line 136-145: defparameter *char-class-regex-mapping*
export const CHAR_CLASS_REGEX_MAPPING: Record<CharClass, RegExp> = {
  "katakana": /[ァ-ヺヽヾー]/,
  "katakana-uniq": /[ァ-ヺヽヾ]/,
  "hiragana": /[ぁ-ゔゝゞー]/,
  "kanji": /[々ヶ〆一-龯]/,
  "kanji-char": /[一-龯]/,
  "kana": /[ァ-ヺヽヾーぁ-ゔゝゞー]/,
  "traditional": /[ぁ-ゔゝゞー々ヶ〆一-龯]/,
  "nonword": /[^々ヶ〆一-龯ァ-ヺヽヾぁ-ゔゝゞー〇]/,
  "number": /[0-9０-９〇一二三四五六七八九零壱弐参拾十百千万億兆京]/
};

// Line 151-153: defparameter *char-scanners*
export const CHAR_SCANNERS = new Map<CharClass, RegExp>(
  (Object.keys(CHAR_CLASS_REGEX_MAPPING) as CharClass[]).map(cc => [
    cc,
    new RegExp(`^${CHAR_CLASS_REGEX_MAPPING[cc].source}+$`)
  ])
);

// Line 160-163: defun test-word
export function testWord(word: string, charClass: CharClass): boolean {
  const regex = CHAR_SCANNERS.get(charClass);
  return regex ? regex.test(word) : false;
}

// Line 165-170: defun count-char-class
export function countCharClass(word: string, charClass: CharClass): number {
  const regex = CHAR_CLASS_REGEX_MAPPING[charClass];
  if (!regex) return 0;
  const matches = word.match(new RegExp(regex.source, 'g'));
  return matches ? matches.length : 0;
}

// Line 172-177: defun collect-char-class
export function collectCharClass(word: string, charClass: CharClass): string[] {
  const regex = CHAR_CLASS_REGEX_MAPPING[charClass];
  if (!regex) return [];
  const matches = word.match(new RegExp(regex.source, 'g'));
  return matches ?? [];
}

// Line 179-183: defun sequential-kanji-positions
export function sequentialKanjiPositions(word: string, offset = 0): number[] {
  const positions: number[] = [];
  const regex = /(?=[々一-龯][々一-龯])/g;
  let match: RegExpExecArray | null;
  while ((match = regex.exec(word)) !== null) {
    positions.push(match.index + 1 + offset);
    // CRITICAL: Lookahead is zero-width, must manually advance to avoid infinite loop
    // Without this, regex.lastIndex doesn't advance and we loop forever at same position
    if (regex.lastIndex === match.index) {
      regex.lastIndex++;
    }
  }
  return positions;
}

// Line 185-188: defun kanji-mask
export function kanjiMask(word: string): string {
  return word.replace(/[々ヶ〆一-龯]+/g, '%');
}

// Line 190-198: defun kanji-regex
export function kanjiRegex(word: string): RegExp {
  const mask = kanjiMask(word);
  const pattern = mask.split('').map(char =>
    char === '%' ? '.+' : char.replace(/[.*+?^${}()|[\]\\]/g, '\\$&')
  ).join('');
  return new RegExp(`^${pattern}$`);
}

// Line 200-201: defun kanji-match
export function kanjiMatch(word: string, reading: string): boolean {
  return kanjiRegex(word).test(reading);
}

// Line 203-208: defun kanji-cross-match
export function kanjiCrossMatch(word: string, reading: string, newWord: string): string | null {
  let m = 0;
  while (m < word.length && m < newWord.length && word[m] === newWord[m]) {
    m++;
  }

  const rCut = m + (reading.length - word.length);
  if (m > 0 && rCut >= 0 && rCut <= reading.length) {
    const readingHead = reading.slice(0, rCut);
    return readingHead + newWord.slice(m);
  }
  return null;
}

// Line 210-217: defun simplify-ngrams
export function simplifyNgrams(str: string, map: [string, string][]): string {
  let result = str;
  for (const [from, to] of map) {
    result = result.replace(new RegExp(from.replace(/[.*+?^${}()|[\]\\]/g, '\\$&'), 'g'), to);
  }
  return result;
}

// Line 219-222: defun to-normal-char
function toNormalChar(char: string, context?: 'kana'): string | null {
  const source = context === 'kana' ? HALF_WIDTH_KANA : ABNORMAL_CHARS;
  const target = context === 'kana' ? FULL_WIDTH_KANA : NORMAL_CHARS;
  const pos = source.indexOf(char);
  return pos >= 0 ? target[pos] : null;
}

// Line 224-232: defun normalize
export function normalize(str: string, context?: 'kana', skipPunctuation: boolean = false): string {
  let result = str.split('').map(char => toNormalChar(char, context) ?? char).join('');
  const replacements = context === 'kana' 
    ? DAKUTEN_JOIN 
    : skipPunctuation 
      ? DAKUTEN_JOIN  // Skip punctuation normalization, only apply dakuten joining
      : [...PUNCTUATION_MARKS, ...DAKUTEN_JOIN];
  result = simplifyNgrams(result, replacements);
  return result;
}

// Line 234-236: defun split-by-regex
export function splitByRegex(regex: RegExp, str: string): string[] {
  return str.split(regex).filter(seg => seg.length > 0);
}

export interface BasicSplitSegment {
  type: 'word' | 'misc';
  text: string;
}

// Line 238-243: defun basic-split
// Lisp: (defun basic-split (str)
//         (let* ((split1 (split-by-regex *basic-split-regex* str)))
//           (loop for segment in split1
//                for misc = (test-word segment :nonword) then (not misc)
//                collect (cons (if misc :misc :word) segment))))
// The key is split-by-regex uses ppcre:split with :with-registers-p t,
// which includes both matches and non-matches in the result array
export function basicSplit(str: string): BasicSplitSegment[] {
  // Split by regex, including both matches and non-matches
  // This mimics ppcre:split with :with-registers-p t
  const segments: string[] = [];
  let lastIndex = 0;
  const regex = new RegExp(BASIC_SPLIT_REGEX.source, 'g');
  let match: RegExpExecArray | null;

  while ((match = regex.exec(str)) !== null) {
    // Add non-match before this match (if any)
    if (match.index > lastIndex) {
      segments.push(str.slice(lastIndex, match.index));
    }
    // Add the match itself
    segments.push(match[0]);
    lastIndex = regex.lastIndex;
  }

  // Add remaining non-match after last match (if any)
  if (lastIndex < str.length) {
    segments.push(str.slice(lastIndex));
  }

  // Filter out empty segments (like Lisp's remove-if)
  const nonEmptySegments = segments.filter(seg => seg.length > 0);

  // Alternate between types: first is tested, then alternates
  // In Lisp: (loop for segment in split1
  //               for misc = (test-word segment :nonword) then (not misc)
  //               collect (cons (if misc :misc :word) segment))
  const result: BasicSplitSegment[] = [];
  let isMisc: boolean | undefined;

  for (let i = 0; i < nonEmptySegments.length; i++) {
    const segment = nonEmptySegments[i];

    if (i === 0) {
      // First segment: test with test-word :nonword
      isMisc = testWord(segment, 'nonword');
    } else {
      // Subsequent segments: alternate (not misc)
      isMisc = !isMisc;
    }

    result.push({
      type: isMisc ? 'misc' : 'word',
      text: segment
    });
  }

  return result;
}

// Line 245-249: defun mora-length
export function moraLength(str: string): number {
  return str.split('').filter(char =>
    !"っッぁァぃィぅゥぇェぉォゃャゅュょョー".includes(char)
  ).length;
}

// Line 251-260: defun as-hiragana
export function asHiragana(str: string): string {
  return str.split('').map(char => {
    const normalized = toNormalChar(char) ?? char;
    const charClass = CHAR_CLASS_HASH.get(normalized);
    if (charClass) {
      const chars = (ALL_CHARACTERS as any)[charClass];
      return chars ? chars[0] : char;
    }
    return char;
  }).join('');
}

// Line 262-271: defun as-katakana
export function asKatakana(str: string): string {
  return str.split('').map(char => {
    const normalized = toNormalChar(char) ?? char;
    const charClass = CHAR_CLASS_HASH.get(normalized);
    if (charClass) {
      const chars = (ALL_CHARACTERS as any)[charClass];
      return chars ? chars[chars.length - 1] : char;
    }
    return char;
  }).join('');
}

// Line 273-278: defun consecutive-char-groups
export function consecutiveCharGroups(
  charClass: CharClass,
  str: string,
  start = 0,
  end = str.length
): [number, number][] {
  const result: [number, number][] = [];
  const regex = new RegExp(CHAR_CLASS_REGEX_MAPPING[charClass].source, 'g');
  const substr = str.slice(start, end);

  let match: RegExpExecArray | null;
  while ((match = regex.exec(substr)) !== null) {
    const matchStart = start + match.index;
    let matchEnd = matchStart + match[0].length;

    // Continue matching consecutive characters
    while (matchEnd < end && CHAR_CLASS_REGEX_MAPPING[charClass].test(str[matchEnd])) {
      matchEnd++;
    }

    result.push([matchStart, matchEnd]);
    regex.lastIndex = matchEnd - start;
  }

  return result;
}

// Line 280-284: defun kanji-prefix
export function kanjiPrefix(word: string): string {
  const match = word.match(/^.*[々ヶ〆一-龯]/);
  return match ? match[0] : "";
}

// Line 286-296: defun unrendaku
export function unrendaku(txt: string, fresh = false): string {
  let result = fresh ? txt : txt;
  if (result.length === 0) return result;

  const firstChar = result[0];
  const cc = CHAR_CLASS_HASH.get(firstChar);
  if (!cc) return result;

  const unvoiced = UNDAKUTEN_HASH.get(cc);
  if (!unvoiced) return result;

  const kanaChars = (KANA_CHARACTERS as any)[cc];
  const unvoicedChars = (KANA_CHARACTERS as any)[unvoiced];
  if (!kanaChars || !unvoicedChars) return result;

  const pos = kanaChars.indexOf(firstChar);
  if (pos < 0) return result;

  const newChar = unvoicedChars[pos];
  return newChar + result.slice(1);
}

// Line 298-309: defun rendaku
export function rendaku(txt: string, fresh = false, handakuten = false): string {
  let result = fresh ? txt : txt;
  if (result.length === 0) return result;

  const firstChar = result[0];
  const cc = CHAR_CLASS_HASH.get(firstChar);
  if (!cc) return result;

  const useHash = handakuten ? HANDAKUTEN_HASH : DAKUTEN_HASH;
  const voiced = useHash.get(cc);
  if (!voiced) return result;

  const kanaChars = (KANA_CHARACTERS as any)[cc];
  const voicedChars = (KANA_CHARACTERS as any)[voiced];
  if (!kanaChars || !voicedChars) return result;

  const pos = kanaChars.indexOf(firstChar);
  if (pos < 0) return result;

  const newChar = voicedChars[pos];
  return newChar + result.slice(1);
}

// Line 311-314: defun geminate
export function geminate(txt: string, fresh = false): string {
  const result = fresh ? txt : txt;
  if (result.length === 0) return result;
  return result.slice(0, -1) + 'っ';
}

// Line 316-324: defun destem
export function destem(word: string, stem: number, charClass: CharClass = 'kana'): string {
  if (stem === 0) return word;

  const regex = CHAR_CLASS_REGEX_MAPPING[charClass];
  const positions: number[] = [];

  let match: RegExpExecArray | null;
  const globalRegex = new RegExp(regex.source, 'g');
  while ((match = globalRegex.exec(word)) !== null) {
    positions.push(match.index);
  }

  if (positions.length >= stem) {
    return word.slice(0, positions[positions.length - stem]);
  }
  return "";
}

// Line 326-357: defun match-diff
export function matchDiff(s1: string, s2: string): [any[], number] | null {
  const l1 = s1.length;
  const l2 = s2.length;

  if (l1 === 0 || l2 === 0) return null;

  let m = 0;
  while (m < l1 && m < l2 && s1[m] === s2[m]) {
    m++;
  }

  if (m === l1 && m === l2) {
    return [[s1], l1];
  }

  if (l1 === 1 || l2 === 1) {
    return [[[s1, s2]], 0];
  }

  if (m === 0) {
    let bestMatch: any[] | null = null;
    let bestMatchValue = -1;

    for (let i = 1; i < l1; i++) {
      for (let j = 1; j < l2; j++) {
        if (s1[i] === s2[j]) {
          const subResult = matchDiff(s1.slice(i), s2.slice(j));
          if (subResult && subResult[1] > bestMatchValue) {
            bestMatch = [[s1.slice(0, i), s2.slice(0, j)], ...subResult[0]];
            bestMatchValue = subResult[1];
          }
        }
      }
    }

    return bestMatch ? [bestMatch, bestMatchValue] : null;
  }

  if (m === l1) {
    return [[s1.slice(0, l1 - 1), [s1[l1 - 1], s2[l1 - 1]]], l1 - 1];
  }

  if (m === l2) {
    return [[s2.slice(0, l2 - 1), [s1[l2 - 1], s2[l2 - 1]]], l2 - 1];
  }

  const subResult = matchDiff(s1.slice(m), s2.slice(m));
  if (subResult) {
    return [[s1.slice(0, m), ...subResult[0]], subResult[1] + m];
  }

  return null;
}

// Line 359-363: defun safe-subseq
export function safeSubseq(sequence: string, start: number, end?: number): string | null {
  const len = sequence.length;
  if (start < 0 || start > len) return null;
  if (end !== undefined && (end < start || end > len)) return null;
  return sequence.slice(start, end);
}

// Line 365-370: defun join
export function join(separator: string, list: any[], key?: (obj: any) => string): string {
  return list.map(obj => key ? key(obj) : obj).join(separator);
}