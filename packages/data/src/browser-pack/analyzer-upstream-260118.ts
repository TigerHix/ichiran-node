/**
 * Analyzer-only rules added after the frozen PostgreSQL reference.
 *
 * These definitions mirror ichiran ea958336 against the 260118 database. They
 * live in the browser-pack compiler so the reference analyzer can remain pinned
 * while newly compiled portable packs receive the current behavior.
 */
import { matchDiff } from '@ichiran/reference-postgres/src/characters.js';
import {
  getKana,
  trueKana,
  trueKanji
} from '@ichiran/reference-postgres/src/dict/readings.js';
import { insertHints } from '@ichiran/reference-postgres/src/dict/splitDefinitions.js';
import type {
  AsyncSplitFunction,
  HintFunction
} from '@ichiran/reference-postgres/src/dict/splitMaps.js';
import {
  findWordConjOf,
  findWordSeq,
  getKanaForms
} from '@ichiran/reference-postgres/src/dict/suffixHelpers.js';
import { getWordType, trueText } from '@ichiran/reference-postgres/src/dict/utils.js';
import { matchReadings } from '@ichiran/reference-postgres/src/kanji.js';
import type { KanaText, Reading } from '@ichiran/reference-postgres/src/types.js';

type HintSpec = readonly [keyword: string, position: number];

export const UPSTREAM_260118_SKIP_WORD_ADDED = 2_827_357;
export const UPSTREAM_260118_SKIP_WORD_REMOVED = 2_458_040;

export const UPSTREAM_260118_GATAI_SEQ = 2_867_504;
export const UPSTREAM_260118_GATAI_KEYWORD = ':ren-';
export const UPSTREAM_260118_GATAI_CLASS = ':gatai';
export const UPSTREAM_260118_NEBA_ABBREVIATION = Object.freeze({
  text: 'ねば',
  keyword: ':nakereba'
});

/** Load the generated forms that upstream registers as the :gatai suffix. */
export async function loadUpstream260118GataiForms(): Promise<readonly KanaText[]> {
  const forms = await getKanaForms(UPSTREAM_260118_GATAI_SEQ);
  if (forms.length === 0) {
    throw new Error(
      `The 260118 analyzer pack requires kana forms for sequence ${UPSTREAM_260118_GATAI_SEQ}`
    );
  }
  return forms;
}

const karasukiSplit: AsyncSplitFunction = async (reading: Reading) => {
  if (getWordType(reading) !== 'kana') return null;
  const text = trueText(reading);
  const [kara, suki] = await Promise.all([
    findWordSeq(text.slice(0, 2), 1_002_980),
    findWordSeq(text.slice(2), 1_277_450)
  ]);
  if (!kara[0] || !suki[0]) return null;
  return [[kara[0], suki[0]], -5];
};

const moushiokureruSplit: AsyncSplitFunction = async (reading: Reading) => {
  const text = trueText(reading);
  const source = await findWordConjOf('申し', 1_363_090);
  if (!source[0]) return null;
  const [moushi, okureru] = await Promise.all([
    findWordSeq(text.slice(0, 2), source[0].seq),
    findWordConjOf(text.slice(2), 1_589_040)
  ]);
  if (!moushi[0] || !okureru[0]) return null;
  return [[moushi[0], okureru[0]], 100];
};

export const upstream260118SplitMap: ReadonlyMap<number, AsyncSplitFunction> = new Map([
  [1_774_820, karasukiSplit],
  [1_362_970, moushiokureruSplit]
]);

function translateHintPosition(match: readonly unknown[], position: number): number | null {
  let offset = 0;
  let remaining = position;
  for (const value of match) {
    if (typeof value === 'string') {
      if (remaining <= value.length) return offset + remaining;
      remaining -= value.length;
      offset += value.length;
      continue;
    }

    const part = value as readonly [string, string];
    const sourceLength = part[0].length;
    const targetLength = part[1].length;
    if (remaining < sourceLength) {
      return offset + Math.min(1, Math.max(targetLength, remaining));
    }
    if (remaining === sourceLength) return offset + targetLength;
    remaining -= sourceLength;
    offset += targetLength;
  }
  return null;
}

function translateHints(
  match: readonly unknown[],
  hints: readonly HintSpec[]
): HintSpec[] {
  const output: HintSpec[] = [];
  for (const [keyword, position] of hints) {
    const translated = translateHintPosition(match, position);
    if (translated !== null) output.push([keyword, translated]);
  }
  return output;
}

function easyHint(kanjiSplit: string): HintFunction {
  const parts = kanjiSplit.split(' ');
  const text = parts.join('');
  const hints: HintSpec[] = [];
  let position = 0;
  for (let index = 0; index < parts.length; index++) {
    const part = parts[index]!;
    if (index > 0) hints.push(['space', position]);
    if (['は', 'へ', 'には', 'とは'].includes(part)) {
      hints.push(['mod', position + part.length - 1]);
    }
    position += part.length;
  }

  return async (reading: Reading): Promise<string | null> => {
    if (getWordType(reading) !== 'kanji') return null;
    const readingText = await trueKanji(reading);
    if (!readingText) return null;
    const match = matchDiff(text, readingText);
    if (!match) return null;
    const kana = await trueKana(reading);
    const kanjiReadings = await matchReadings(readingText, kana);
    if (!kanjiReadings) return null;
    return insertHints(
      await getKana(reading),
      translateHints(kanjiReadings, translateHints(match[0], hints)) as [string, number][]
    );
  };
}

const goSentenceHint: HintFunction = async (reading: Reading) => {
  const kana = await trueKana(reading);
  const ha = kana.indexOf('は');
  if (ha < 0) return null;
  return insertHints(await getKana(reading), [
    ['space', ha],
    ['mod', ha],
    ['space', ha + 1]
  ]);
};

export const UPSTREAM_260118_EASY_HINTS = Object.freeze([
  [2_865_369, '世間 は 張り物'],
  [2_867_221, '武士 は 相身互い'],
  [2_868_635, '止まない 雨 は ない'],
  [2_864_666, '予定 は 未定'],
  [2_865_149, '画像 は イメージ です'],
  [2_863_602, '余り物 には 福 が ある'],
  [2_867_148, '敵 の 急所 は 我が 急所'],
  [2_864_960, '言い方 は 悪いです が'],
  [2_868_513, '人 は パン のみ にて 生くる に 非ず'],
  [2_864_443, 'に 至って は'],
  [1_586_550, '後 へ 引く']
] as const);

const hintEntries: Array<readonly [number, HintFunction]> = [
  [2_867_144, goSentenceHint],
  [2_867_149, goSentenceHint],
  ...UPSTREAM_260118_EASY_HINTS.map(
    ([seq, split]) => [seq, easyHint(split)] as const
  )
];

export const upstream260118HintMap: ReadonlyMap<number, HintFunction> = new Map(hintEntries);
