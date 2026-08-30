/** Browser-safe character classification used by the portable analyzer. */

export const SOKUON_CHARACTERS = { sokuon: 'っッ' } as const;
export const ITERATION_CHARACTERS = { iter: 'ゝヽ', iterV: 'ゞヾ' } as const;
export const MODIFIER_CHARACTERS = {
  '+a': 'ぁァ', '+i': 'ぃィ', '+u': 'ぅゥ', '+e': 'ぇェ', '+o': 'ぉォ',
  '+ya': 'ゃャ', '+yu': 'ゅュ', '+yo': 'ょョ', '+wa': 'ゎヮ',
  longVowel: 'ー'
} as const;

export const KANA_CHARACTERS = {
  a: 'あア', i: 'いイ', u: 'うウ', e: 'えエ', o: 'おオ',
  ka: 'かカ', ki: 'きキ', ku: 'くク', ke: 'けケ', ko: 'こコ',
  sa: 'さサ', shi: 'しシ', su: 'すス', se: 'せセ', so: 'そソ',
  ta: 'たタ', chi: 'ちチ', tsu: 'つツ', te: 'てテ', to: 'とト',
  na: 'なナ', ni: 'にニ', nu: 'ぬヌ', ne: 'ねネ', no: 'のノ',
  ha: 'はハ', hi: 'ひヒ', fu: 'ふフ', he: 'へヘ', ho: 'ほホ',
  ma: 'まマ', mi: 'みミ', mu: 'むム', me: 'めメ', mo: 'もモ',
  ya: 'やヤ', yu: 'ゆユ', yo: 'よヨ',
  ra: 'らラ', ri: 'りリ', ru: 'るル', re: 'れレ', ro: 'ろロ',
  wa: 'わワ', wi: 'ゐヰ', we: 'ゑヱ', wo: 'をヲ', n: 'んン',
  ga: 'がガ', gi: 'ぎギ', gu: 'ぐグ', ge: 'げゲ', go: 'ごゴ',
  za: 'ざザ', ji: 'じジ', zu: 'ずズ', ze: 'ぜゼ', zo: 'ぞゾ',
  da: 'だダ', dji: 'ぢヂ', dzu: 'づヅ', de: 'でデ', do: 'どド',
  ba: 'ばバ', bi: 'びビ', bu: 'ぶブ', be: 'べベ', bo: 'ぼボ',
  pa: 'ぱパ', pi: 'ぴピ', pu: 'ぷプ', pe: 'ぺペ', po: 'ぽポ',
  vu: 'ゔヴ'
} as const;

export const ALL_CHARACTERS = {
  ...SOKUON_CHARACTERS,
  ...ITERATION_CHARACTERS,
  ...MODIFIER_CHARACTERS,
  ...KANA_CHARACTERS
} as const;

export const CHAR_CLASS_HASH = new Map<string, string>();
for (const [charClass, characters] of Object.entries(ALL_CHARACTERS)) {
  for (const character of characters) CHAR_CLASS_HASH.set(character, charClass);
}

export const DAKUTEN_HASH = new Map<string, string>([
  ['ka', 'ga'], ['ki', 'gi'], ['ku', 'gu'], ['ke', 'ge'], ['ko', 'go'],
  ['sa', 'za'], ['shi', 'ji'], ['su', 'zu'], ['se', 'ze'], ['so', 'zo'],
  ['ta', 'da'], ['chi', 'dji'], ['tsu', 'dzu'], ['te', 'de'], ['to', 'do'],
  ['ha', 'ba'], ['hi', 'bi'], ['fu', 'bu'], ['he', 'be'], ['ho', 'bo'],
  ['u', 'vu']
]);

export const HANDAKUTEN_HASH = new Map<string, string>([
  ['ha', 'pa'], ['hi', 'pi'], ['fu', 'pu'], ['he', 'pe'], ['ho', 'po']
]);

export const UNDAKUTEN_HASH = new Map<string, string>([
  ['ga', 'ka'], ['gi', 'ki'], ['gu', 'ku'], ['ge', 'ke'], ['go', 'ko'],
  ['za', 'sa'], ['ji', 'shi'], ['zu', 'su'], ['ze', 'se'], ['zo', 'so'],
  ['da', 'ta'], ['dji', 'chi'], ['dzu', 'tsu'], ['de', 'te'], ['do', 'to'],
  ['ba', 'ha'], ['bi', 'hi'], ['bu', 'fu'], ['be', 'he'], ['bo', 'ho'],
  ['pa', 'ha'], ['pi', 'hi'], ['pu', 'fu'], ['pe', 'he'], ['po', 'ho'],
  ['vu', 'u']
]);

export const PUNCTUATION_MARKS: readonly (readonly [string, string])[] = [
  ['【', ' ['], ['】', '] '], ['、', ', '], ['，', ', '], ['。', '. '],
  ['・・・', '... '], ['・', ' '], ['　', ' '], ['「', ' "'], ['」', '" '],
  ['゛', '"'], ['『', ' «'], ['』', '» '], ['〜', ' - '], ['：', ': '],
  ['！', '! '], ['？', '? '], ['；', '; ']
];

export const HALF_WIDTH_KANA = '･ｦｧｨｩｪｫｬｭｮｯｰｱｲｳｴｵｶｷｸｹｺｻｼｽｾｿﾀﾁﾂﾃﾄﾅﾆﾇﾈﾉﾊﾋﾌﾍﾎﾏﾐﾑﾒﾓﾔﾕﾖﾗﾘﾙﾚﾛﾜﾝﾞﾟ';
export const FULL_WIDTH_KANA = '・ヲァィゥェォャュョッーアイウエオカキクケコサシスセソタチツテトナニヌネノハヒフヘホマミムメモヤユヨラリルレロワン゛゜';

export const ABNORMAL_CHARS =
  '０１２３４５６７８９ａｂｃｄｅｆｇｈｉｊｋｌｍｎｏｐｑｒｓｔｕｖｗｘｙｚＡＢＣＤＥＦＧＨＩＪＫＬＭＮＯＰＱＲＳＴＵＶＷＸＹＺ＃＄％＆（）＊＋／〈＝〉？＠［］＾＿\'｛｜｝～'
  + HALF_WIDTH_KANA;
export const NORMAL_CHARS =
  '0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ#$%&()*+/<=>?@[]^_`{|}~'
  + FULL_WIDTH_KANA;

function dakutenJoin(table: ReadonlyMap<string, string>, mark: string): [string, string][] {
  const result: [string, string][] = [];
  for (const [plainClass, voicedClass] of table) {
    let plain: string | undefined = KANA_CHARACTERS[plainClass as keyof typeof KANA_CHARACTERS];
    const voiced: string | undefined = KANA_CHARACTERS[voicedClass as keyof typeof KANA_CHARACTERS];
    if (!plain || !voiced) continue;
    if (plain.length > voiced.length) plain = plain.slice(plain.length - voiced.length);
    for (let index = 0; index < plain.length; index++) {
      result.push([plain[index]! + mark, voiced[index]!]);
    }
  }
  return result;
}

export const DAKUTEN_JOIN: readonly (readonly [string, string])[] = [
  ...dakutenJoin(DAKUTEN_HASH, '゛'),
  ...dakutenJoin(HANDAKUTEN_HASH, '゜')
];

export type CharClass =
  | 'katakana' | 'katakana-uniq' | 'hiragana' | 'kanji' | 'kanji-char'
  | 'kana' | 'traditional' | 'nonword' | 'number';

export const CHAR_CLASS_REGEX: Readonly<Record<CharClass, RegExp>> = {
  katakana: /[ァ-ヺヽヾー]/,
  'katakana-uniq': /[ァ-ヺヽヾ]/,
  hiragana: /[ぁ-ゔゝゞー]/,
  kanji: /[々ヶ〆一-龯]/,
  'kanji-char': /[一-龯]/,
  kana: /[ァ-ヺヽヾーぁ-ゔゝゞー]/,
  traditional: /[ぁ-ゔゝゞー々ヶ〆一-龯]/,
  nonword: /[^々ヶ〆一-龯ァ-ヺヽヾぁ-ゔゝゞー〇]/,
  number: /[0-9０-９〇一二三四五六七八九零壱弐参拾十百千万億兆京]/
};

const CHAR_SCANNERS = new Map<CharClass, RegExp>(
  (Object.keys(CHAR_CLASS_REGEX) as CharClass[]).map((charClass) => [
    charClass,
    new RegExp(`^${CHAR_CLASS_REGEX[charClass].source}+$`)
  ])
);

const BASIC_SPLIT_REGEX = /(((?<![.,]|[0-9０-９〇])[0-9０-９〇]+|[々ヶ〆一-龯ァ-ヺヽヾぁ-ゔゝゞー〇])[0-9０-９〇々ヶ〆一-龯ァ-ヺヽヾぁ-ゔゝゞー]*[々ヶ〆一-龯ァ-ヺヽヾぁ-ゔゝゞー〇]|[々ヶ〆一-龯ァ-ヺヽヾぁ-ゔゝゞー〇])/g;

export interface BasicSplitSegment {
  readonly type: 'word' | 'misc';
  readonly text: string;
}

export function getCharClass(character: string): string {
  return CHAR_CLASS_HASH.get(character) ?? character;
}

export function voiceChar(charClass: string): string {
  return DAKUTEN_HASH.get(charClass) ?? charClass;
}

export function longVowelModifierP(modifier: string, previousCharacter: string): boolean {
  const vowel = ({ '+a': 'A', '+i': 'I', '+u': 'U', '+e': 'E', '+o': 'O' } as const)[
    modifier as '+a' | '+i' | '+u' | '+e' | '+o'
  ];
  if (!vowel) return false;
  const charClass = getCharClass(previousCharacter);
  return charClass !== previousCharacter
    && vowel === charClass[charClass.length - 1]!.toUpperCase();
}

export function testWord(word: string, charClass: CharClass): boolean {
  return CHAR_SCANNERS.get(charClass)!.test(word);
}

export function countCharClass(word: string, charClass: CharClass): number {
  return word.match(new RegExp(CHAR_CLASS_REGEX[charClass].source, 'g'))?.length ?? 0;
}

export function collectCharClass(word: string, charClass: CharClass): string[] {
  return word.match(new RegExp(CHAR_CLASS_REGEX[charClass].source, 'g')) ?? [];
}

export function sequentialKanjiPositions(word: string, offset = 0): number[] {
  const positions: number[] = [];
  const regex = /(?=[々一-龯][々一-龯])/g;
  let match: RegExpExecArray | null;
  while ((match = regex.exec(word)) !== null) {
    positions.push(match.index + 1 + offset);
    regex.lastIndex++;
  }
  return positions;
}

export function simplifyNgrams(
  input: string,
  replacements: readonly (readonly [string, string])[]
): string {
  let result = input;
  for (const [from, to] of replacements) {
    const escaped = from.replace(/[.*+?^${}()|[\]\\]/g, '\\$&');
    result = result.replace(new RegExp(escaped, 'g'), to);
  }
  return result;
}

function toNormalChar(character: string, context?: 'kana'): string | null {
  const source = context === 'kana' ? HALF_WIDTH_KANA : ABNORMAL_CHARS;
  const target = context === 'kana' ? FULL_WIDTH_KANA : NORMAL_CHARS;
  const index = source.indexOf(character);
  return index < 0 ? null : target[index]!;
}

export function normalize(
  input: string,
  context?: 'kana',
  skipPunctuation = false
): string {
  let result = Array.from(input, (character) => toNormalChar(character, context) ?? character).join('');
  const replacements = context === 'kana' || skipPunctuation
    ? DAKUTEN_JOIN
    : [...PUNCTUATION_MARKS, ...DAKUTEN_JOIN];
  result = simplifyNgrams(result, replacements);
  return result;
}

export function basicSplit(input: string): BasicSplitSegment[] {
  const pieces: string[] = [];
  const regex = new RegExp(BASIC_SPLIT_REGEX.source, 'g');
  let lastIndex = 0;
  let match: RegExpExecArray | null;
  while ((match = regex.exec(input)) !== null) {
    if (match.index > lastIndex) pieces.push(input.slice(lastIndex, match.index));
    pieces.push(match[0]);
    lastIndex = regex.lastIndex;
  }
  if (lastIndex < input.length) pieces.push(input.slice(lastIndex));

  let misc: boolean | undefined;
  return pieces.filter(Boolean).map((text, index) => {
    misc = index === 0 ? testWord(text, 'nonword') : !misc;
    return { type: misc ? 'misc' : 'word', text };
  });
}

export function moraLength(input: string): number {
  let length = 0;
  for (const character of input) {
    if (!'っッぁァぃィぅゥぇェぉォゃャゅュょョー'.includes(character)) length++;
  }
  return length;
}

export function asHiragana(input: string): string {
  return Array.from(input, (character) => {
    const normalized = toNormalChar(character) ?? character;
    const charClass = CHAR_CLASS_HASH.get(normalized) as keyof typeof ALL_CHARACTERS | undefined;
    return charClass ? ALL_CHARACTERS[charClass][0]! : character;
  }).join('');
}

export function asKatakana(input: string): string {
  return Array.from(input, (character) => {
    const normalized = toNormalChar(character) ?? character;
    const charClass = CHAR_CLASS_HASH.get(normalized) as keyof typeof ALL_CHARACTERS | undefined;
    const characters = charClass ? ALL_CHARACTERS[charClass] : undefined;
    return characters ? characters[characters.length - 1]! : character;
  }).join('');
}

export function consecutiveCharGroups(
  charClass: CharClass,
  input: string,
  start = 0,
  end = input.length
): [number, number][] {
  const groups: [number, number][] = [];
  const regex = new RegExp(CHAR_CLASS_REGEX[charClass].source, 'g');
  const substring = input.slice(start, end);
  let match: RegExpExecArray | null;
  while ((match = regex.exec(substring)) !== null) {
    const groupStart = start + match.index;
    let groupEnd = groupStart + match[0].length;
    while (groupEnd < end && CHAR_CLASS_REGEX[charClass].test(input[groupEnd]!)) groupEnd++;
    groups.push([groupStart, groupEnd]);
    regex.lastIndex = groupEnd - start;
  }
  return groups;
}

export function unrendaku(input: string, _fresh = false): string {
  return replaceInitialKana(input, UNDAKUTEN_HASH);
}

export function rendaku(input: string, _fresh = false, handakuten = false): string {
  return replaceInitialKana(input, handakuten ? HANDAKUTEN_HASH : DAKUTEN_HASH);
}

function replaceInitialKana(input: string, table: ReadonlyMap<string, string>): string {
  if (input.length === 0) return input;
  const sourceClass = CHAR_CLASS_HASH.get(input[0]!);
  const targetClass = sourceClass ? table.get(sourceClass) : undefined;
  if (!sourceClass || !targetClass) return input;
  const source = KANA_CHARACTERS[sourceClass as keyof typeof KANA_CHARACTERS];
  const target = KANA_CHARACTERS[targetClass as keyof typeof KANA_CHARACTERS];
  if (!source || !target) return input;
  const index = source.indexOf(input[0]!);
  return index < 0 ? input : target[index]! + input.slice(1);
}

export function geminate(input: string, _fresh = false): string {
  return input.length === 0 ? input : input.slice(0, -1) + 'っ';
}

export function destem(word: string, stem: number, charClass: CharClass = 'kana'): string {
  if (stem === 0) return word;
  const positions: number[] = [];
  const regex = new RegExp(CHAR_CLASS_REGEX[charClass].source, 'g');
  let match: RegExpExecArray | null;
  while ((match = regex.exec(word)) !== null) positions.push(match.index);
  return positions.length < stem ? '' : word.slice(0, positions[positions.length - stem]!);
}
