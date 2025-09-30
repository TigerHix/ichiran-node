// ichiran/dict-split - Word splitting definitions
// Port of dict-split.lisp

import { trueText, trueKana, trueKanji, getKana, getText, getWordType, calcScore, wordConjData } from './dict.js';
import { findWordSeq, findWordConjOf } from './dict-grammar.js';
import { unrendaku, safeSubseq, matchDiff } from './characters.js';
import { matchReadings } from './kanji.js';
// Import shared types from types.ts (single source of truth)
import type { KanjiText, KanaText, CompoundText, Segment, ConjData, Reading } from './types.js';

// Type for split functions: (reading) => [parts, attrs] or Promise<[parts, attrs]>
// attrs can be:
//   - a number (simple score)
//   - an object with { score, primary?, connector?, root? }
// Note: Regular splits are sync, but segsplits may be async (they call async findWordSeq)
type SplitAttrs = number | { score: number; primary?: number; connector?: string; root?: number[] };
type SplitFunction = (reading: Reading) => [any[], SplitAttrs] | null;
type AsyncSplitFunction = (reading: Reading) => Promise<[any[], SplitAttrs] | null>;

// Type for hint functions: (reading) => string | null
type HintFunction = (reading: Reading) => Promise<string | null>;

// Line 5: defparameter *split-map*
const splitMap = new Map<number, AsyncSplitFunction>();

// Line 7-11: defmacro defsplit
// In TypeScript, we directly register functions instead of using a macro
function defsplit(seq: number, fn: AsyncSplitFunction): void {
  splitMap.set(seq, fn);
}

// Helper: safe-subseq equivalent (already in characters.ts as safeSubseq)
// Helper: unrendaku equivalent (already in characters.ts)

// Line 13-67: defmacro def-simple-split
// This is a complex macro that generates split functions.
// In TypeScript, we create a builder function that returns a SplitFunction

type PartDef =
  | { type: 'test', condition: (len: number, txt: string, reading: Reading) => boolean, newScore?: number, pushOnFail?: ':score' | ':pscore' }
  | { type: 'marker', marker: ':score' | ':pscore' }
  | { type: 'part', seqs: number | number[] | [string, ...number[]], lengthFn: (len: number, txt: string, reading: Reading) => number | null, conjP?: boolean, modify?: boolean | ((txt: string) => string) };

interface DefSimpleSplitOptions {
  name?: string;
  seq: number;
  score: SplitAttrs;
  parts: PartDef[];
}

function defSimpleSplit(options: DefSimpleSplitOptions): void {
  const { seq, score: initialScore, parts } = options;

  const splitFn: AsyncSplitFunction = async (reading: Reading) => {
    const txt = trueText(reading);
    const len = txt.length;
    let offset = 0;
    const resultParts: any[] = [];
    let score: SplitAttrs = initialScore;

    for (const partDef of parts) {
      // Handle :test (conditional early exit)
      if (partDef.type === 'test') {
        if (!partDef.condition(len, txt, reading)) {
          if (partDef.newScore !== undefined) {
            // Update the score value
            if (typeof score === 'number') {
              score = partDef.newScore;
            } else {
              score = { ...score, score: partDef.newScore };
            }
          }
          if (partDef.pushOnFail) {
            resultParts.push(partDef.pushOnFail);
          }
          // Early exit - go to :end
          break;
        }
        continue;
      }

      // Handle :score or :pscore markers
      if (partDef.type === 'marker') {
        resultParts.push(partDef.marker);
        continue;
      }

      // Handle actual word parts
      if (partDef.type === 'part') {
        // Resolve seqs
        let pseqs: number[];
        if (typeof partDef.seqs === 'number') {
          pseqs = [partDef.seqs];
        } else if (Array.isArray(partDef.seqs) && typeof partDef.seqs[0] === 'string') {
          // String-based lookup: (("text" seq1 seq2 ...))
          // Need to call find-word-conj-of with the string and seqs
          const [searchText, ...searchSeqs] = partDef.seqs as [string, ...number[]];
          const found = await findWordConjOf(searchText, ...searchSeqs);
          if (found.length > 0 && 'seq' in found[0]) {
            pseqs = [found[0].seq];
          } else {
            pseqs = [];
          }
        } else {
          pseqs = partDef.seqs as number[];
        }

        // Calculate part length
        const partLength = partDef.lengthFn(len, txt, reading);

        // Extract part text
        const partTxt = partLength !== null
          ? safeSubseq(txt, offset, offset + partLength)
          : safeSubseq(txt, offset, undefined);

        // Avoid recursion
        if (pseqs.includes(seq)) {
          console.warn(`Recursive split on ${seq}`);
          resultParts.push(null);
        } else if (partTxt) {
          // Apply modification if needed
          let searchTxt = partTxt;
          if (partDef.modify === true) {
            searchTxt = unrendaku(partTxt);
          } else if (typeof partDef.modify === 'function') {
            searchTxt = partDef.modify(partTxt);
          }

          // Find word
          const findFn = partDef.conjP ? findWordConjOf : findWordSeq;
          const foundWords = await findFn(searchTxt, ...pseqs);
          resultParts.push(foundWords.length > 0 ? foundWords[0] : null);
        } else {
          resultParts.push(null);
        }

        // Advance offset
        if (partLength !== null) {
          offset += partLength;
        }
      }
    }

    // Return parts and score
    return [resultParts, score];
  };

  defsplit(seq, splitFn);
}

// Line 69-75: defun get-split*
async function getSplit_(reading: Reading, conjOf: number[] = []): Promise<[any[], SplitAttrs] | null> {
  const readingSeq = 'seq' in reading ? reading.seq : null;

  if (readingSeq !== null) {
    const splitFn = splitMap.get(readingSeq);
    if (splitFn) {
      return await splitFn(reading);
    }
  }

  // Try conjugation ancestors
  for (const seq of conjOf) {
    const splitFn = splitMap.get(seq);
    if (splitFn) {
      return await splitFn(reading);
    }
  }

  return null;
}

// Line 77-81: defun get-split
export async function getSplit(reading: Reading, conjOf: number[] = []): Promise<[any[], SplitAttrs] | null> {
  const result = await getSplit_(reading, conjOf);
  if (result) {
    const [split, score] = result;
    // Safety check: ensure all split parts exist (no nulls)
    if (split && split.every(part => part !== null)) {
      return [split, score];
    }
  }
  return null;
}

// ============================================================================
// SPLIT DEFINITIONS
// ============================================================================

// Line 96-100: defmacro def-de-split
// Helper function to create de-split definitions
function defDeSplit(seq: number, seqA: number, score: number = 20): void {
  defSimpleSplit({
    seq,
    score,
    parts: [
      { type: 'part', seqs: seqA, lengthFn: (len) => len - 1 },
      { type: 'part', seqs: 2028980, lengthFn: () => 1 }  // で particle
    ]
  });
}

// Line 102-133: def-de-split instances
defDeSplit(1163700, 1576150); // 一人で
defDeSplit(1611020, 1577100); // 何で
defDeSplit(1004800, 1628530); // これで
defDeSplit(2810720, 1004820); // 此れまでで
defDeSplit(1006840, 1006880); // その上で
defDeSplit(1530610, 1530600); // 無断で
defDeSplit(1245390, 1245290); // 空で
defDeSplit(2719270, 1445430); // 土足で
defDeSplit(1189420, 2416780); // 何用で
defDeSplit(1272220, 1592990); // 交代で
defDeSplit(1311360, 1311350); // 私費で
defDeSplit(1368500, 1368490); // 人前で
defDeSplit(1395670, 1395660); // 全体で
defDeSplit(1417790, 1417780); // 単独で
defDeSplit(1454270, 1454260); // 道理で
defDeSplit(1479100, 1679020); // 半眼で
defDeSplit(1510140, 1680900); // 別封で
defDeSplit(1518550, 1529560); // 無しで
defDeSplit(1531420, 1531410); // 名義で
defDeSplit(1597400, 1585205); // 力尽くで
defDeSplit(1679990, 2582460); // 抜き足で
defDeSplit(1682060, 2085340); // 金ずくで
defDeSplit(1736650, 1611710); // 水入らずで
defDeSplit(1865020, 1590150); // 陰で
defDeSplit(1878880, 2423450); // 差しで
defDeSplit(2126220, 1802920); // 捩じり鉢巻きで
defDeSplit(2136520, 2005870); // もう少しで
defDeSplit(2513590, 2513650); // 詰め開きで
defDeSplit(2771850, 2563780); // 気にしないで
defDeSplit(2810800, 1587590); // 今までで
defDeSplit(1343110, 1343100); // ところで
defDeSplit(1270210, 1001640); // お陰で

// Line 135-137: でございます special case
defSimpleSplit({
  seq: 2253080,
  score: 20,
  parts: [
    { type: 'part', seqs: 2028980, lengthFn: () => 1 },  // で
    { type: 'part', seqs: 1612690, lengthFn: () => null, conjP: true }  // ございます
  ]
});

// Line 139-144: defmacro def-toori-split
// Helper function for 通り (toori) compounds
function defTooriSplit(seq: number, seqA: number, seqB: number = 1432930, score: number = 50): void {
  defSimpleSplit({
    seq,
    score,
    parts: [
      {
        type: 'test',
        condition: (len, txt, reading) => getWordType(reading) === 'kanji'
      },
      { type: 'part', seqs: seqA, lengthFn: (len) => len - 2 },
      { type: 'part', seqs: seqB, lengthFn: () => 2 }
    ]
  });
}

// Line 146-169: def-toori-split instances
defTooriSplit(1260990, 1260670); // 元通り
defTooriSplit(1414570, 2082450); // 大通り
defTooriSplit(1424950, 1620400); // 中通り [ちゅう通り]
defTooriSplit(1424960, 1423310); // 中通り [なか通り]
defTooriSplit(1820790, 1250090); // 型通り
defTooriSplit(1489800, 1489340); // 表通り
defTooriSplit(1523010, 1522150); // 本通り
defTooriSplit(1808080, 1604890); // 目通り
defTooriSplit(1368820, 1580640); // 人通り
defTooriSplit(1550490, 1550190); // 裏通り
defTooriSplit(1619440, 2069220); // 素通り
defTooriSplit(1164910, 2821500, 1432920); // 一通り
defTooriSplit(1462720, 1461140, 1432920); // 二通り

// Line 171-175: defmacro def-do-split
// Helper function for ど (do) prefix compounds
function defDoSplit(seq: number, seqB: number, seqA: number = 2252690, score: number = 30): void {
  defSimpleSplit({
    seq,
    score,
    parts: [
      { type: 'part', seqs: seqA, lengthFn: () => 1 },
      { type: 'part', seqs: seqB, lengthFn: () => null }
    ]
  });
}

// Line 177-183: def-do-split instances
defDoSplit(2142710, 1185200); // ど下手
defDoSplit(2803190, 1595630); // どすけべ
defDoSplit(2142680, 1290210); // ど根性
defDoSplit(2523480, 1442750); // ど田舎

// Line 194-198: defmacro def-shi-split
// Helper function for し (shi) stem + verb compounds
function defShiSplit(seq: number, seqB: number, seqA: [string, ...number[]] = ["し", 1157170], score: number = 30): void {
  defSimpleSplit({
    seq,
    score,
    parts: [
      { type: 'part', seqs: seqA, lengthFn: () => 1 },
      { type: 'part', seqs: seqB, lengthFn: () => null, conjP: true }
    ]
  });
}

// Line 200-219: def-shi-split instances
defShiSplit(1005700, 1156990); // し易い
defShiSplit(1005830, 1370760); // し吹く
defShiSplit(1157200, 2772730); // し難い
defShiSplit(1157220, 1195970); // し過ぎる
defShiSplit(1157230, 1284430); // し合う
defShiSplit(1157280, 1370090); // し尽す
defShiSplit(1157310, 1405800); // し続ける
defShiSplit(1304890, 1256520); // し兼ねる
defShiSplit(1304960, 1307550); // し始める
defShiSplit(1305110, 1338180); // し出す
defShiSplit(1305280, 1599390); // し直す
defShiSplit(1305290, 1212670); // し慣れる
defShiSplit(1594300, 1596510); // し損なう
defShiSplit(1594310, 1406680); // し損じる
defShiSplit(1594460, 1372620); // し遂げる
defShiSplit(1594580, 1277100); // し向ける
defShiSplit(2518250, 1332760); // し終える
defShiSplit(1157240, 1600260); // し残す
defShiSplit(1304820, 1207610); // し掛ける
defShiSplit(2858937, 1406690); // し損ねる

// ============================================================================
// Individual def-simple-split definitions (Lines 221-800)
// ============================================================================

// Line 223-225: split-nakunaru - 無くなる
defSimpleSplit({
  seq: 1529550,
  score: 30,
  parts: [
    { type: 'part', seqs: ["無く", 1529520], lengthFn: () => 2 },
    { type: 'part', seqs: 1375610, lengthFn: () => null, conjP: true }
  ]
});

// Line 227-230: split-nakunaru2 - 亡くなる
defSimpleSplit({
  seq: 1518540,
  score: 10,
  parts: [
    { type: 'test', condition: (len, txt, r) => getWordType(r) === 'kana' },
    { type: 'part', seqs: ["亡く", 1518450], lengthFn: () => 2 },
    { type: 'part', seqs: 1375610, lengthFn: () => null, conjP: true }
  ]
});

// Line 234-237: split-tegakakaru - 手が掛かる
defSimpleSplit({
  seq: 2089710,
  score: 10,
  parts: [
    { type: 'part', seqs: 1327190, lengthFn: () => 1 },  // 手
    { type: 'part', seqs: 2028930, lengthFn: () => 1 },  // が
    { type: 'part', seqs: 1207590, lengthFn: () => null, conjP: true }
  ]
});

// Line 240-242: split-kawaribae - 代わり映え
defSimpleSplit({
  seq: 1411570,
  score: 10,
  parts: [
    { type: 'part', seqs: [1590770, 1510720], lengthFn: (len, txt) => txt.indexOf('り') + 1 },
    { type: 'part', seqs: ["映え", 1600620], lengthFn: () => 2 }
  ]
});

// Line 244-247: split-hayaimonode - 早いもので
defSimpleSplit({
  seq: 2815260,
  score: 100,
  parts: [
    { type: 'part', seqs: 1404975, lengthFn: (len, txt) => txt.indexOf('い') + 1 },
    { type: 'part', seqs: 1502390, lengthFn: (len, txt) => txt.includes('物') ? 1 : 2 },
    { type: 'part', seqs: 2028980, lengthFn: () => 1 }
  ]
});

// Line 249-252: split-dogatsukeru - ドが付ける
defSimpleSplit({
  seq: 2800540,
  score: 30,
  parts: [
    { type: 'part', seqs: 2252690, lengthFn: () => 1 },
    { type: 'part', seqs: 2028930, lengthFn: () => 1 },
    { type: 'part', seqs: 1495740, lengthFn: () => null, conjP: true }
  ]
});

// Line 254-256: split-janaika - じゃないか
defSimpleSplit({
  seq: 2819990,
  score: 20,
  parts: [
    { type: 'part', seqs: ["じゃない", 2089020], lengthFn: () => 4 },
    { type: 'part', seqs: 2028970, lengthFn: () => 1 }
  ]
});

// Line 258-261: split-kaasan - 母さん
defSimpleSplit({
  seq: 1609470,
  score: 50,
  parts: [
    { type: 'test', condition: (len, txt, r) => getWordType(r) === 'kanji' },
    { type: 'part', seqs: 1514990, lengthFn: () => 1 },
    { type: 'part', seqs: 1005340, lengthFn: () => 2 }
  ]
});

// Line 263-265: split-souda - そうだ
defSimpleSplit({
  seq: 1006650,
  score: 5,
  parts: [
    { type: 'part', seqs: 2137720, lengthFn: () => 2 },
    { type: 'part', seqs: 2089020, lengthFn: () => null }
  ]
});

// Line 267-269: split-soudesu - そうです
defSimpleSplit({
  seq: 2837492,
  score: 5,
  parts: [
    { type: 'part', seqs: 2137720, lengthFn: () => 2 },
    { type: 'part', seqs: 1628500, lengthFn: () => null }
  ]
});

// Line 271-274: split-kinosei - 気のせい
defSimpleSplit({
  seq: 1221750,
  score: 100,
  parts: [
    { type: 'part', seqs: 1221520, lengthFn: () => 1 },
    { type: 'part', seqs: 1469800, lengthFn: () => 1 },
    { type: 'part', seqs: 1610040, lengthFn: () => 2 }
  ]
});

// Line 276-279: split-kigatsuku - 気がつく
defSimpleSplit({
  seq: 1591050,
  score: 100,
  parts: [
    { type: 'part', seqs: 1221520, lengthFn: () => 1 },
    { type: 'part', seqs: 2028930, lengthFn: () => 1 },
    { type: 'part', seqs: 1495740, lengthFn: () => null, conjP: true }
  ]
});

// Line 281-283: split-nanimokamo - なにもかも
defSimpleSplit({
  seq: 1599590,
  score: 20,
  parts: [
    { type: 'part', seqs: 1188490, lengthFn: (len) => len - 2 },
    { type: 'part', seqs: 2143350, lengthFn: () => 2 }
  ]
});

// Line 285-288: split-katawonaraberu - 肩を並べる
defSimpleSplit({
  seq: 2102910,
  score: 20,
  parts: [
    { type: 'part', seqs: 1258950, lengthFn: (len, txt) => txt.indexOf('を') },
    { type: 'part', seqs: 2029010, lengthFn: () => 1 },
    { type: 'part', seqs: 1508390, lengthFn: () => null, conjP: true }
  ]
});

// Line 290-292: split-moushiwakenasasou - 申し訳なさそう
defSimpleSplit({
  seq: 2057340,
  score: 300,
  parts: [
    { type: 'part', seqs: 1363050, lengthFn: (len, txt) => txt.indexOf('な') },
    { type: 'part', seqs: 2246510, lengthFn: () => null }
  ]
});

// Line 294-295: split-kimatte - 決まって
defSimpleSplit({
  seq: 1951150,
  score: 50,
  parts: [
    { type: 'part', seqs: ["決まって", 1591420], lengthFn: () => null }
  ]
});

// Line 297-299: split-osoreiru - 恐れ入る
defSimpleSplit({
  seq: 1236680,
  score: 100,
  parts: [
    { type: 'part', seqs: 1236660, lengthFn: (len, txt) => txt.indexOf('れ') + 1 },
    { type: 'part', seqs: 1465580, lengthFn: () => null, conjP: true }
  ]
});

// Line 301-303: split-nantokanaru - なんとかなる
defSimpleSplit({
  seq: 2104540,
  score: 20,
  parts: [
    { type: 'part', seqs: 1188420, lengthFn: (len, txt) => txt.indexOf('か') + 1 },
    { type: 'part', seqs: 1375610, lengthFn: () => null, conjP: true }
  ]
});

// Line 305-307: split-hajiketobu - 弾け飛ぶ
defSimpleSplit({
  seq: 2610760,
  score: 50,
  parts: [
    { type: 'part', seqs: ["弾け", 1419380], lengthFn: (len, txt) => txt.indexOf('け') + 1 },
    { type: 'part', seqs: 1429700, lengthFn: () => null, conjP: true }
  ]
});

// Line 309-311: split-motteiku - 持って行く
defSimpleSplit({
  seq: 1315700,
  score: 50,
  parts: [
    { type: 'part', seqs: ["持って", 1315720], lengthFn: (len, txt) => txt.indexOf('て') + 1 },
    { type: 'part', seqs: 1578850, lengthFn: () => null, conjP: true }
  ]
});

// Line 313-316: split-hairikomeru - 入り込める
defSimpleSplit({
  seq: 1465460,
  score: 100,
  parts: [
    { type: 'test', condition: (len, txt, r) => getWordType(r) === 'kanji' },
    { type: 'part', seqs: ["入り", 1465590], lengthFn: (len, txt) => txt.indexOf('り') + 1 },
    { type: 'part', seqs: 1288790, lengthFn: () => null, conjP: true }
  ]
});

// Line 318-321: split-shinikakaru - 死に掛かる
defSimpleSplit({
  seq: 1881080,
  score: 30,
  parts: [
    { type: 'part', seqs: 1310720, lengthFn: () => 1 },
    { type: 'part', seqs: 2028990, lengthFn: () => 1 },
    { type: 'part', seqs: 1207590, lengthFn: () => null, conjP: true }
  ]
});

// Line 323-326: split-hisshininatte - 必死になって
defSimpleSplit({
  seq: 1903910,
  score: 50,
  parts: [
    { type: 'part', seqs: 1601890, lengthFn: (len, txt) => txt.indexOf('に') },
    { type: 'part', seqs: 2028990, lengthFn: () => 1 },
    { type: 'part', seqs: ["なって", 1375610], lengthFn: () => null }
  ]
});

// Line 328-330: split-nitotte - にとって
defSimpleSplit({
  seq: 1009600,
  score: 50,
  parts: [
    { type: 'part', seqs: 2028990, lengthFn: () => 1 },
    { type: 'part', seqs: ["取って", 1326980], lengthFn: () => null }
  ]
});

// Line 332-333: i don't remember why this was here
// defSimpleSplit({
//   seq: 1009610,
//   score: 0,  // split-nimo
//   parts: []
// });

// Line 335-338: split-kotonisuru - 事にする
defSimpleSplit({
  seq: 2215340,
  score: 100,
  parts: [
    { type: 'part', seqs: 1313580, lengthFn: (len, txt) => txt.indexOf('に') },
    { type: 'part', seqs: 2028990, lengthFn: () => 1 },
    { type: 'part', seqs: 1157170, lengthFn: () => null, conjP: true }
  ]
});

// Line 340-342: split-hajikidasu - 弾き出す
defSimpleSplit({
  seq: 1419350,
  score: 100,
  parts: [
    { type: 'part', seqs: 1901710, lengthFn: (len, txt) => txt.indexOf('き') + 1 },
    { type: 'part', seqs: 1338180, lengthFn: () => null, conjP: true }
  ]
});

// Line 344-346: split-hitotachi - 人たち
defSimpleSplit({
  seq: 1368740,
  score: 100,
  parts: [
    { type: 'part', seqs: 1580640, lengthFn: (len, txt) => txt.indexOf('人') >= 0 ? 1 : 2 },
    { type: 'part', seqs: 1416220, lengthFn: (len, txt) => txt.indexOf('達') >= 0 ? 1 : 2 }
  ]
});

// Line 348-350: split-desura - でさえ/ですら
defSimpleSplit({
  seq: 2034520,
  score: 30,
  parts: [
    { type: 'part', seqs: 2028980, lengthFn: () => 1 },
    { type: 'part', seqs: [2827091], lengthFn: () => null }
  ]
});

// Line 352-354: split-gotoni - ごとに
defSimpleSplit({
  seq: 1524660,
  score: 50,
  parts: [
    { type: 'part', seqs: 1524640, lengthFn: (len, txt) => txt.indexOf('に') },
    { type: 'part', seqs: 2028990, lengthFn: () => 1 }
  ]
});

// Line 356-358: split-osagari - お下がり
defSimpleSplit({
  seq: 1693800,
  score: 50,
  parts: [
    { type: 'part', seqs: 2826528, lengthFn: () => 1 },
    { type: 'part', seqs: 1609810, lengthFn: () => null }
  ]
});

// Line 360-362: split-kaisasae - 買い支え
defSimpleSplit({
  seq: 1752860,
  score: 50,
  parts: [
    { type: 'part', seqs: 1636070, lengthFn: () => 2 },
    { type: 'part', seqs: ["支え", 1310090], lengthFn: () => null }
  ]
});

// Line 364-366: split-toiu - という
defSimpleSplit({
  seq: 1922760,
  score: 20,
  parts: [
    { type: 'part', seqs: 1008490, lengthFn: () => 1 },
    { type: 'part', seqs: 1587040, lengthFn: () => null, conjP: true }
  ]
});

// Line 368-371: split-toiukotoda - ということだ
defSimpleSplit({
  seq: 2612990,
  score: 30,
  parts: [
    { type: 'part', seqs: 1922760, lengthFn: () => 3 },
    { type: 'part', seqs: 1313580, lengthFn: (len) => len - 4 },
    { type: 'part', seqs: 2089020, lengthFn: () => null }
  ]
});

// Line 373-375: split-tonattara - となったら
defSimpleSplit({
  seq: 2100770,
  score: 50,
  parts: [
    { type: 'part', seqs: 1008490, lengthFn: () => 1 },
    { type: 'part', seqs: ["なったら", 1375610], lengthFn: () => null }
  ]
});

// Line 377-379: split-tonaru - となる
defSimpleSplit({
  seq: 2100900,
  score: 10,
  parts: [
    { type: 'part', seqs: 1008490, lengthFn: () => 1 },
    { type: 'part', seqs: 1375610, lengthFn: () => null, conjP: true }
  ]
});

// Line 381-384: 手に入る
defSimpleSplit({
  seq: 1327220,
  score: 50,
  parts: [
    { type: 'part', seqs: 1327190, lengthFn: () => 1 },
    { type: 'part', seqs: 2028990, lengthFn: () => 1 },
    { type: 'part', seqs: 1465590, lengthFn: () => null, conjP: true }
  ]
});

// Line 386-389: 手に入れる
defSimpleSplit({
  seq: 1327230,
  score: 50,
  parts: [
    { type: 'part', seqs: 1327190, lengthFn: () => 1 },
    { type: 'part', seqs: 2028990, lengthFn: () => 1 },
    { type: 'part', seqs: 1465610, lengthFn: () => null, conjP: true }
  ]
});

// Line 391-393: そうなんです
defSimpleSplit({
  seq: 2433760,
  score: 50,
  parts: [
    { type: 'part', seqs: 1006610, lengthFn: () => 2 },
    { type: 'part', seqs: 2683060, lengthFn: () => null }
  ]
});

// Line 395-397: 良さげ
defSimpleSplit({
  seq: 2088480,
  score: 20,
  parts: [
    { type: 'part', seqs: 1634130, lengthFn: () => 2 },
    { type: 'part', seqs: 2006580, lengthFn: () => 1 }
  ]
});

// Line 399-402: のせいで
defSimpleSplit({
  seq: 2724560,
  score: 30,
  parts: [
    { type: 'part', seqs: 1469800, lengthFn: () => 1 },
    { type: 'part', seqs: 1610040, lengthFn: (len) => len - 2 },
    { type: 'part', seqs: 2028980, lengthFn: () => 1 }
  ]
});

// Line 404-405: 少なくない
defSimpleSplit({
  seq: 2666360,
  score: 30,
  parts: [
    { type: 'part', seqs: ["少なくない", 1348910], lengthFn: () => null }
  ]
});

// Line 407-409: split-janai - じゃない
defSimpleSplit({
  seq: 2755350,
  score: 10,
  parts: [
    { type: 'part', seqs: 2089020, lengthFn: () => 2 },
    { type: 'part', seqs: 1529520, lengthFn: () => null, conjP: true }
  ]
});

// Line 411-413: split-jan - じゃん
defSimpleSplit({
  seq: 2135280,
  score: 10,
  parts: [
    { type: 'part', seqs: 2089020, lengthFn: () => 2 },
    { type: 'part', seqs: 2139720, lengthFn: () => 1 }
  ]
});

// Line 415-419: はないか
defSimpleSplit({
  seq: 2771940,
  score: -5,
  parts: [
    { type: 'test', condition: (len, txt) => txt === "はないか" },
    { type: 'part', seqs: 2028920, lengthFn: () => 1 },
    { type: 'part', seqs: 1529520, lengthFn: () => 2 },
    { type: 'part', seqs: 2028970, lengthFn: () => 1 }
  ]
});

// Line 421-422: split-nara - なら
defSimpleSplit({
  seq: 1009470,
  score: 1,
  parts: [
    { type: 'part', seqs: ["なら", 2089020], lengthFn: () => null }
  ]
});

// Line 424-427: ならん
defSimpleSplit({
  seq: 2083990,
  score: 20,
  parts: [
    { type: 'test', condition: (len, txt) => txt === "ならん" },
    { type: 'part', seqs: 1009470, lengthFn: () => 2 },
    { type: 'part', seqs: 2139720, lengthFn: () => 1 }
  ]
});

// Line 429-430: ならんで
defSimpleSplit({
  seq: 2762260,
  score: 0,
  parts: [
    { type: 'part', seqs: ["ならんで", 1508380], lengthFn: () => null }
  ]
});

// Line 432-435: ならんで (kana version)
defSimpleSplit({
  seq: 1508380,
  score: 10,
  parts: [
    { type: 'test', condition: (len, txt, r) => getWordType(r) === 'kana' },
    { type: 'part', seqs: 2083990, lengthFn: () => 3 },
    { type: 'part', seqs: 2028980, lengthFn: () => 1 }
  ]
});

// Line 438-440: 中でも
defSimpleSplit({
  seq: 2009290,
  score: 100,
  parts: [
    { type: 'part', seqs: 1423310, lengthFn: (len) => len - 2 },
    { type: 'part', seqs: 1008460, lengthFn: () => null }
  ]
});

// Line 442-444: 物好き
defSimpleSplit({
  seq: 1502500,
  score: 100,
  parts: [
    { type: 'part', seqs: 1502390, lengthFn: (len) => len - 2 },
    { type: 'part', seqs: 1277450, lengthFn: () => 2, conjP: true }
  ]
});

// Line 446-449: かもしれない
defSimpleSplit({
  seq: 1002970,
  score: 600,
  parts: [
    { type: 'test', condition: (len, txt, r) => getWordType(r) === 'kanji' },
    { type: 'part', seqs: 2143350, lengthFn: () => 2 },
    { type: 'part', seqs: ["知れない", 1420490], lengthFn: () => null }
  ]
});

// Line 451-452: しまった
defSimpleSplit({
  seq: 1005600,
  score: -10,
  parts: [
    { type: 'part', seqs: ["しまった", 1305380], lengthFn: () => null }
  ]
});

// Line 454-455: やった
defSimpleSplit({
  seq: 2016840,
  score: -5,
  parts: [
    { type: 'part', seqs: ["やった", 1012980], lengthFn: () => null }
  ]
});

// Line 457-458: あの
defSimpleSplit({
  seq: 1000430,
  score: -5,
  parts: [
    { type: 'part', seqs: 1000420, lengthFn: () => null }
  ]
});

// Line 460-462: あのね
defSimpleSplit({
  seq: 1612640,
  score: 5,
  parts: [
    { type: 'part', seqs: 1000420, lengthFn: () => 2 },
    { type: 'part', seqs: [2029080, 2029120, 1005110], lengthFn: () => null }
  ]
});

// Line 464-466: に+ない
defSimpleSplit({
  seq: 1314600,
  score: -5,
  parts: [
    { type: 'part', seqs: 2028990, lengthFn: () => 1 },
    { type: 'part', seqs: 1529520, lengthFn: () => null, conjP: true }
  ]
});

// Line 468-470: に+ない
defSimpleSplit({
  seq: 1322540,
  score: -5,
  parts: [
    { type: 'part', seqs: 2028990, lengthFn: () => 1 },
    { type: 'part', seqs: 1529520, lengthFn: () => null, conjP: true }
  ]
});

// Line 472-475: 気にします
defSimpleSplit({
  seq: 1221680,
  score: 50,
  parts: [
    { type: 'part', seqs: 1221520, lengthFn: () => 1 },
    { type: 'part', seqs: 2028990, lengthFn: () => 1 },
    { type: 'part', seqs: 1157170, lengthFn: () => null, conjP: true }
  ]
});

// Line 477-480: わけがわからない
defSimpleSplit({
  seq: 1538340,
  score: 50,
  parts: [
    { type: 'part', seqs: 1538330, lengthFn: (len, txt) => txt.indexOf('が') },
    { type: 'part', seqs: 2028930, lengthFn: () => 1 },
    { type: 'part', seqs: 1606560, lengthFn: () => null, conjP: true }
  ]
});

// Line 482-485: わけのわからない
defSimpleSplit({
  seq: 2757500,
  score: 50,
  parts: [
    { type: 'part', seqs: 1538330, lengthFn: (len, txt) => txt.indexOf('の') },
    { type: 'part', seqs: 1469800, lengthFn: () => 1 },
    { type: 'part', seqs: 1606560, lengthFn: () => null, conjP: true }
  ]
});

// Line 487-489: 見たところ (commented out)
// defSimpleSplit({
//   seq: 1715710,
//   score: 10,
//   parts: [
//     { type: 'part', seqs: ["見た", 1259290], lengthFn: () => 2 },
//     { type: 'part', seqs: 1343100, lengthFn: () => null }
//   ]
// });

// Line 491-493: 時には
defSimpleSplit({
  seq: 1315860,
  score: 20,
  parts: [
    { type: 'part', seqs: 1315840, lengthFn: (len) => len - 2 },
    { type: 'part', seqs: 2215430, lengthFn: () => 2 }
  ]
});

// Line 495-498: 這います/います
defSimpleSplit({
  seq: 1474200,
  score: -10,
  parts: [
    { type: 'test', condition: (len, txt, r) => getWordType(r) === 'kana' },
    { type: 'part', seqs: 2028920, lengthFn: () => 1 },
    { type: 'part', seqs: 1577980, lengthFn: () => null, conjP: true }
  ]
});

// Line 500-502: 尽くし
defSimpleSplit({
  seq: 2276360,
  score: 10,
  parts: [
    { type: 'part', seqs: 2436480, lengthFn: (len) => len - 1 },
    { type: 'part', seqs: 2086640, lengthFn: () => 1 }
  ]
});

// Line 504-507: ことし
defSimpleSplit({
  seq: 1579130,
  score: -1,
  parts: [
    { type: 'test', condition: (len, txt) => txt === "ことし" },
    { type: 'part', seqs: 1313580, lengthFn: () => 2 },
    { type: 'part', seqs: 2086640, lengthFn: () => 1 }
  ]
});

// Line 509-512: 汗を流す
defSimpleSplit({
  seq: 2668400,
  score: 50,
  parts: [
    { type: 'part', seqs: 1213060, lengthFn: (len, txt) => txt.indexOf('を') },
    { type: 'part', seqs: 2029010, lengthFn: () => 1 },
    { type: 'part', seqs: 1552120, lengthFn: () => null, conjP: true }
  ]
});

// Line 514-517: 気がつく (duplicate - already defined at line 276-279, skip)

// Line 519-521: 折りたたみ式
defSimpleSplit({
  seq: 2835890,
  score: 50,
  parts: [
    { type: 'part', seqs: 1385860, lengthFn: () => 5 },
    { type: 'part', seqs: 1319060, lengthFn: () => 1 }
  ]
});

// Line 523-527: Helper function optprefix
function optprefix(prefix: string): (txt: string) => string {
  return (txt: string) => {
    if (txt.startsWith(prefix)) {
      return txt;
    }
    return prefix + txt;
  };
}

// Line 529-533: ついてる
defSimpleSplit({
  seq: 1894260,
  score: 50,
  parts: [
    { type: 'test', condition: (len) => len > 3 },
    { type: 'part', seqs: ["付いて", 1894260], lengthFn: () => 3 },
    { type: 'part', seqs: 1577980, lengthFn: () => null, conjP: true, modify: optprefix("い") }
  ]
});

// Line 535-536: 付いて
defSimpleSplit({
  seq: 1854750,
  score: 20,
  parts: [
    { type: 'part', seqs: ["付いて", 1495740], lengthFn: () => null }
  ]
});

// Line 538-540: にしろ
defSimpleSplit({
  seq: 2526850,
  score: 10,
  parts: [
    { type: 'part', seqs: 2028990, lengthFn: () => 1 },
    { type: 'part', seqs: ["しろ", 1157170], lengthFn: () => null }
  ]
});

// Line 542-544: にせよ
defSimpleSplit({
  seq: 2026650,
  score: 10,
  parts: [
    { type: 'part', seqs: 2028990, lengthFn: () => 1 },
    { type: 'part', seqs: ["せよ", 1157170], lengthFn: () => null }
  ]
});

// Line 546-548: 普段着
defSimpleSplit({
  seq: 1602740,
  score: 50,
  parts: [
    { type: 'part', seqs: 1497180, lengthFn: (len) => len - 1 },
    { type: 'part', seqs: 2093780, lengthFn: () => null }
  ]
});

// Line 550-552: なお
defSimpleSplit({
  seq: 1349300,
  score: 5,
  parts: [
    { type: 'part', seqs: 2029110, lengthFn: () => 1 },
    { type: 'part', seqs: 2826528, lengthFn: () => null }
  ]
});

// Line 554-557: 気がある
defSimpleSplit({
  seq: 1221530,
  score: 50,
  parts: [
    { type: 'part', seqs: 1221520, lengthFn: () => 1 },
    { type: 'part', seqs: 2028930, lengthFn: () => 1 },
    { type: 'part', seqs: 1296400, lengthFn: () => null, conjP: true }
  ]
});

// Line 559-562: 気がない
defSimpleSplit({
  seq: 2272780,
  score: 50,
  parts: [
    { type: 'part', seqs: 1221520, lengthFn: () => 1 },
    { type: 'part', seqs: 2028930, lengthFn: () => 1 },
    { type: 'part', seqs: 1529520, lengthFn: () => null, conjP: true }
  ]
});

// Line 564-567: 気はない
defSimpleSplit({
  seq: 2846470,
  score: 50,
  parts: [
    { type: 'part', seqs: 1221520, lengthFn: () => 1 },
    { type: 'part', seqs: 2028920, lengthFn: () => 1 },
    { type: 'part', seqs: 1529520, lengthFn: () => null, conjP: true }
  ]
});

// Line 569-573: 気を使う/気を遣う
defSimpleSplit({
  seq: 1591980,
  score: 50,
  parts: [
    { type: 'part', seqs: 1221520, lengthFn: () => 1 },
    { type: 'part', seqs: 2029010, lengthFn: () => 1 },
    { type: 'part', seqs: 1305990, lengthFn: () => null, conjP: true }
  ]
});

// Line 575-577: 立ちすくむ
defSimpleSplit({
  seq: 1551500,
  score: 50,
  parts: [
    { type: 'part', seqs: ["立ち", 1597040], lengthFn: () => 2 },
    { type: 'part', seqs: 1570220, lengthFn: () => null, conjP: true }
  ]
});

// Line 579-581: 零れ落ちる
defSimpleSplit({
  seq: 2002270,
  score: 50,
  parts: [
    { type: 'part', seqs: ["零れ", 1557650], lengthFn: (len, txt) => txt.indexOf('れ') + 1 },
    { type: 'part', seqs: 1548550, lengthFn: () => null, conjP: true }
  ]
});

// Line 583-586: につく
defSimpleSplit({
  seq: 1314770,
  score: -10,
  parts: [
    { type: 'test', condition: (len, txt, r) => getWordType(r) === 'kana' },
    { type: 'part', seqs: 2028990, lengthFn: () => 1 },
    { type: 'part', seqs: 1495740, lengthFn: () => null, conjP: true }
  ]
});

// Line 588-589: つい (with :score marker)
defSimpleSplit({
  seq: 1008030,
  score: -10,
  parts: [
    { type: 'marker', marker: ':score' }
  ]
});

// Line 591-594: ついたて
defSimpleSplit({
  seq: 1597740,
  score: 5,
  parts: [
    { type: 'test', condition: (len, txt, r) => getWordType(r) === 'kana' },
    { type: 'part', seqs: 1008030, lengthFn: () => 2 },
    { type: 'part', seqs: 2081610, lengthFn: () => null }
  ]
});

// Line 596-601: 雪がない (complex with dual :test)
defSimpleSplit({
  seq: 1581550,
  score: 10,
  parts: [
    { type: 'test', condition: (len, txt) => txt.startsWith("雪") },
    { type: 'part', seqs: 1386500, lengthFn: () => 1 },
    { type: 'part', seqs: 2028930, lengthFn: () => 1 },
    { type: 'test', condition: (len) => len > 2, newScore: -2, pushOnFail: ':pscore' },
    { type: 'part', seqs: 1529520, lengthFn: () => null, conjP: true }
  ]
});

// Line 603-605: はやめる
defSimpleSplit({
  seq: 1601080,
  score: -5,
  parts: [
    { type: 'part', seqs: 2028920, lengthFn: () => 1 },
    { type: 'part', seqs: 1310680, lengthFn: () => null, conjP: true }
  ]
});

// Line 607-609: 者ども
defSimpleSplit({
  seq: 2529050,
  score: 30,
  parts: [
    { type: 'part', seqs: 1322990, lengthFn: (len, txt) => txt.startsWith("もの") ? 2 : 1 },
    { type: 'part', seqs: 1234250, lengthFn: () => null }
  ]
});

// Line 611-613: すると
defSimpleSplit({
  seq: 1006280,
  score: 30,
  parts: [
    { type: 'part', seqs: 1157170, lengthFn: () => 2 },
    { type: 'part', seqs: 1008490, lengthFn: () => 1 }
  ]
});

// Line 615-617: 出しな
defSimpleSplit({
  seq: 2757540,
  score: 90,
  parts: [
    { type: 'part', seqs: 1896380, lengthFn: () => 1 },
    { type: 'part', seqs: 2728200, lengthFn: () => null }
  ]
});

// Line 619-621: わかりきる
defSimpleSplit({
  seq: 1606530,
  score: 100,
  parts: [
    { type: 'part', seqs: ["分かり", 1606560], lengthFn: () => 3 },
    { type: 'part', seqs: 1384830, lengthFn: () => null, conjP: true }
  ]
});

// Line 623-625: 落ちこぼれる
defSimpleSplit({
  seq: 2007500,
  score: 100,
  parts: [
    { type: 'part', seqs: ["落ち", 1548550], lengthFn: () => 2 },
    { type: 'part', seqs: 1557650, lengthFn: () => null, conjP: true }
  ]
});

// Line 627-629: あけましておめでとうございます
defSimpleSplit({
  seq: 1532270,
  score: 100,
  parts: [
    { type: 'part', seqs: ["あけまして", 1202450], lengthFn: () => 5 },
    { type: 'part', seqs: 1001540, lengthFn: () => null }
  ]
});

// Line 631-633: よろしくおねがいします
defSimpleSplit({
  seq: 2133750,
  score: 100,
  parts: [
    { type: 'part', seqs: 1224890, lengthFn: (len, txt) => txt.indexOf('く') + 1 },
    { type: 'part', seqs: 1001720, lengthFn: () => null }
  ]
});

// Line 635-638: 俺たち
defSimpleSplit({
  seq: 1863230,
  score: 15,
  parts: [
    { type: 'test', condition: (len, txt, r) => getWordType(r) === 'kana' },
    { type: 'part', seqs: 1576870, lengthFn: () => 2 },
    { type: 'part', seqs: 1416220, lengthFn: () => null }
  ]
});

// Line 640-643: お前たち
defSimpleSplit({
  seq: 2834051,
  score: 15,
  parts: [
    { type: 'test', condition: (len, txt, r) => getWordType(r) === 'kana' },
    { type: 'part', seqs: 1002290, lengthFn: () => 3 },
    { type: 'part', seqs: 1416220, lengthFn: () => null }
  ]
});

// Line 646-648: 割り
defSimpleSplit({
  seq: 1606800,
  score: 10,
  parts: [
    { type: 'test', condition: (len) => len === 2 },
    { type: 'part', seqs: ["割り", 1208000], lengthFn: () => null }
  ]
});

// Line 650-652: 割り切れる
defSimpleSplit({
  seq: 1207840,
  score: 50,
  parts: [
    { type: 'part', seqs: ["割り", 1208000], lengthFn: () => 2 },
    { type: 'part', seqs: 1384860, lengthFn: () => null, conjP: true }
  ]
});

// Line 654-656: あり得ない
defSimpleSplit({
  seq: 2109610,
  score: 50,
  parts: [
    { type: 'part', seqs: ["有り", 1296400], lengthFn: () => 2 },
    { type: 'part', seqs: 1588760, lengthFn: () => null, conjP: true }
  ]
});

// Line 658-660: なので (commented out)
// defSimpleSplit({
//   seq: 2827864,
//   score: 100,
//   parts: [
//     { type: 'part', seqs: 2029110, lengthFn: () => 1 },
//     { type: 'part', seqs: 1009970, lengthFn: () => 2 }
//   ]
// });

// Line 662-665: につまる
defSimpleSplit({
  seq: 1322560,
  score: -10,
  parts: [
    { type: 'test', condition: (len, txt, r) => getWordType(r) === 'kana' },
    { type: 'part', seqs: 2028990, lengthFn: () => 1 },
    { type: 'part', seqs: 1226480, lengthFn: () => null, conjP: true }
  ]
});

// Line 667-669: その上
defSimpleSplit({
  seq: 1006880,
  score: 50,
  parts: [
    { type: 'part', seqs: 1006830, lengthFn: () => 2 },
    { type: 'part', seqs: 1352130, lengthFn: () => null }
  ]
});

// Line 671-673: はね上がる
defSimpleSplit({
  seq: 1601010,
  score: 50,
  parts: [
    { type: 'part', seqs: ["跳ね", 1429620], lengthFn: () => 2 },
    { type: 'part', seqs: 1352290, lengthFn: () => null, conjP: true }
  ]
});

// Line 675-678: 写真を撮る
defSimpleSplit({
  seq: 1881690,
  score: 50,
  parts: [
    { type: 'part', seqs: 1321900, lengthFn: (len, txt) => txt.indexOf('を') },
    { type: 'part', seqs: 2029010, lengthFn: () => 1 },
    { type: 'part', seqs: 1298790, lengthFn: () => null, conjP: true }
  ]
});

// Line 680-681: 取り留め
defSimpleSplit({
  seq: 2834732,
  score: -10,
  parts: [
    { type: 'part', seqs: 1707770, lengthFn: () => null, conjP: true }
  ]
});

// Line 685-688: 取り留めのない
defSimpleSplit({
  seq: 1855670,
  score: 50,
  parts: [
    { type: 'part', seqs: ["取り留め", 1707770], lengthFn: (len, txt) => txt.indexOf('の') },
    { type: 'part', seqs: 1469800, lengthFn: () => 1 },
    { type: 'part', seqs: 1529520, lengthFn: () => null }
  ]
});

// Line 690-693: 取り留めもない
defSimpleSplit({
  seq: 2855921,
  score: 50,
  parts: [
    { type: 'part', seqs: ["取り留め", 1707770], lengthFn: (len, txt) => txt.indexOf('も') },
    { type: 'part', seqs: 2028940, lengthFn: () => 1 },
    { type: 'part', seqs: 1529520, lengthFn: () => null }
  ]
});

// ============================================================================
// SEGMENT SPLITS (Lines 696-802)
// These allow expanding one segment into several (e.g., "ところが" → "ところ+が")
// ============================================================================

// Line 698: defparameter *segsplit-map*
const segsplitMap = new Map<number, AsyncSplitFunction>();

// Helper to temporarily use segsplitMap instead of splitMap
// In Lisp, they use (let ((*split-map* *segsplit-map*)) ...)
// We'll define these splits manually with segsplitMap

// Line 701-703: split-tokoroga - ところが
segsplitMap.set(1008570, async (reading: Reading) => {
  const txt = trueText(reading);
  const len = txt.length;
  const parts: any[] = [];

  const word1 = (await findWordSeq(safeSubseq(txt, 0, len - 1)!, 1343100))[0] || null;
  const word2 = (await findWordSeq(safeSubseq(txt, len - 1, undefined)!, 2028930))[0] || null;

  parts.push(word1);
  parts.push(word2);

  return [parts, -10];
});

// Line 705-707: split-tokorode - ところで
segsplitMap.set(1343110, async (reading: Reading) => {
  const txt = trueText(reading);
  const len = txt.length;
  const parts: any[] = [];

  const word1 = (await findWordSeq(safeSubseq(txt, 0, len - 1)!, 1343100))[0] || null;
  const word2 = (await findWordSeq(safeSubseq(txt, len - 1, undefined)!, 2028980))[0] || null;

  parts.push(word1);
  parts.push(word2);

  // '(-10 :root (1))
  return [parts, { score: -10, root: [1] }];
});

// Line 709-711: split-dokoroka - 所か
segsplitMap.set(2009220, async (reading: Reading) => {
  const txt = trueText(reading);
  const len = txt.length;
  const parts: any[] = [];

  const word1 = (await findWordSeq(safeSubseq(txt, 0, len - 1)!, 1343100))[0] || null;
  const word2 = (await findWordSeq(safeSubseq(txt, len - 1, undefined)!, 2028970))[0] || null;

  parts.push(word1);
  parts.push(word2);

  return [parts, -10];
});

// Line 713-715: split-tokoroe - ところへ
segsplitMap.set(2097010, async (reading: Reading) => {
  const txt = trueText(reading);
  const len = txt.length;
  const parts: any[] = [];

  const word1 = (await findWordSeq(safeSubseq(txt, 0, len - 1)!, 1343100))[0] || null;
  const word2 = (await findWordSeq(safeSubseq(txt, len - 1, undefined)!, 2029000))[0] || null;

  parts.push(word1);
  parts.push(word2);

  return [parts, -10];
});

// Line 717-719: split-tokorowo - ところを
segsplitMap.set(2136660, async (reading: Reading) => {
  const txt = trueText(reading);
  const len = txt.length;
  const parts: any[] = [];

  const word1 = (await findWordSeq(safeSubseq(txt, 0, len - 1)!, 1343100))[0] || null;
  const word2 = (await findWordSeq(safeSubseq(txt, len - 1, undefined)!, 2029010))[0] || null;

  parts.push(word1);
  parts.push(word2);

  return [parts, -10];
});

// Line 721-724: split-tokorodewa - ところでは
segsplitMap.set(1897510, async (reading: Reading) => {
  const txt = trueText(reading);
  const len = txt.length;
  const parts: any[] = [];

  const word1 = (await findWordSeq(safeSubseq(txt, 0, len - 2)!, 1343100))[0] || null;
  const word2 = (await findWordSeq(safeSubseq(txt, len - 2, len - 1)!, 2028980))[0] || null;
  const word3 = (await findWordSeq(safeSubseq(txt, len - 1, undefined)!, 2028920))[0] || null;

  parts.push(word1);
  parts.push(word2);
  parts.push(word3);

  return [parts, -10];
});

// Line 726-728: split-omise - お店
segsplitMap.set(2409240, async (reading: Reading) => {
  const txt = trueText(reading);
  const parts: any[] = [];

  const word1 = (await findWordSeq(safeSubseq(txt, 0, 1)!, 2826528))[0] || null;
  const word2 = (await findWordSeq(safeSubseq(txt, 1, undefined)!, 1582120))[0] || null;

  parts.push(word1);
  parts.push(word2);

  // '(20 :primary 1 :connector "")
  return [parts, { score: 20, primary: 1, connector: "" }];
});

// Line 730-732: split-hitorashii - 人らしい
segsplitMap.set(1366490, async (reading: Reading) => {
  const txt = trueText(reading);
  const len = txt.length;
  const parts: any[] = [];

  const word1 = (await findWordSeq(safeSubseq(txt, 0, len - 3)!, 1580640))[0] || null;
  const word2 = (await findWordSeq(safeSubseq(txt, len - 3, undefined)!, 1013240))[0] || null;

  parts.push(word1);
  parts.push(word2);

  // '(-10 :connector "")
  return [parts, { score: -10, connector: "" }];
});

// Line 734-736: split-toha - とは
segsplitMap.set(2028950, async (reading: Reading) => {
  const txt = trueText(reading);
  const parts: any[] = [];

  const word1 = (await findWordSeq(safeSubseq(txt, 0, 1)!, 1008490))[0] || null;
  const word2 = (await findWordSeq(safeSubseq(txt, 1, undefined)!, 2028920))[0] || null;

  parts.push(word1);
  parts.push(word2);

  return [parts, -5];
});

// Line 738-740: split-deha - では
segsplitMap.set(1008450, async (reading: Reading) => {
  const txt = trueText(reading);
  const parts: any[] = [];

  const word1 = (await findWordSeq(safeSubseq(txt, 0, 1)!, 2028980))[0] || null;
  const word2 = (await findWordSeq(safeSubseq(txt, 1, undefined)!, 2028920))[0] || null;

  parts.push(word1);
  parts.push(word2);

  return [parts, -5];
});

// Line 742-744: split-naito - ないと
segsplitMap.set(2394710, async (reading: Reading) => {
  const txt = trueText(reading);
  const parts: any[] = [];

  const word1 = (await findWordSeq(safeSubseq(txt, 0, 2)!, 1529520))[0] || null;
  const word2 = (await findWordSeq(safeSubseq(txt, 2, undefined)!, 1008490))[0] || null;

  parts.push(word1);
  parts.push(word2);

  return [parts, -5];
});

// Line 746-748: split-honno - ほんの
segsplitMap.set(1011740, async (reading: Reading) => {
  const txt = trueText(reading);
  const len = txt.length;
  const parts: any[] = [];

  const word1 = (await findWordSeq(safeSubseq(txt, 0, len - 1)!, 1522150))[0] || null;
  const word2 = (await findWordSeq(safeSubseq(txt, len - 1, undefined)!, 1469800))[0] || null;

  parts.push(word1);
  parts.push(word2);

  return [parts, -5];
});

// Line 750-753: split-kanatte - かなって
segsplitMap.set(1208870, async (reading: Reading) => {
  const txt = trueText(reading);

  if (txt !== "かなって") {
    return null;
  }

  const parts: any[] = [];
  const word1 = (await findWordSeq(safeSubseq(txt, 0, 2)!, 1002940))[0] || null;
  const word2 = (await findWordSeq(safeSubseq(txt, 2, undefined)!, 2086960))[0] || null;

  parts.push(word1);
  parts.push(word2);

  return [parts, 5];
});

// Line 755-757: split-dakara - だから
segsplitMap.set(1007310, async (reading: Reading) => {
  const txt = trueText(reading);
  const parts: any[] = [];

  const word1 = (await findWordSeq(safeSubseq(txt, 0, 1)!, 2089020))[0] || null;
  const word2 = (await findWordSeq(safeSubseq(txt, 1, undefined)!, 1002980))[0] || null;

  parts.push(word1);
  parts.push(word2);

  return [parts, -5];
});

// Line 759-761: から元気
segsplitMap.set(1675330, async (reading: Reading) => {
  const txt = trueText(reading);
  const parts: any[] = [];

  const word1 = (await findWordSeq(safeSubseq(txt, 0, 2)!, 1002980))[0] || null;
  const word2 = (await findWordSeq(safeSubseq(txt, 2, undefined)!, 1260720))[0] || null;

  parts.push(word1);
  parts.push(word2);

  // '(10 :primary 1)
  return [parts, { score: 10, primary: 1 }];
});

// Line 763-765: からって
segsplitMap.set(2841254, async (reading: Reading) => {
  const txt = trueText(reading);
  const parts: any[] = [];

  const word1 = (await findWordSeq(safeSubseq(txt, 0, 2)!, 1002980))[0] || null;
  const word2 = (await findWordSeq(safeSubseq(txt, 2, undefined)!, 2086960))[0] || null;

  parts.push(word1);
  parts.push(word2);

  return [parts, 5];
});

// Line 767-770: もんだ
segsplitMap.set(1567610, async (reading: Reading) => {
  const txt = trueText(reading);

  if (txt !== "もんだ") {
    return null;
  }

  const parts: any[] = [];
  const word1 = (await findWordSeq(safeSubseq(txt, 0, 2)!, 1502390))[0] || null;
  const word2 = (await findWordSeq(safeSubseq(txt, 2, undefined)!, 2089020))[0] || null;

  parts.push(word1);
  parts.push(word2);

  return [parts, 5];
});

// Line 772-775: はぐったり
segsplitMap.set(1010105, async (reading: Reading) => {
  const txt = trueText(reading);

  if (txt !== "はぐったり") {
    return null;
  }

  const parts: any[] = [];
  const word1 = (await findWordSeq(safeSubseq(txt, 0, 1)!, 2028920))[0] || null;
  const word2 = (await findWordSeq(safeSubseq(txt, 1, undefined)!, 1004070))[0] || null;

  parts.push(word1);
  parts.push(word2);

  return [parts, 5];
});

// Line 778-802: defun get-segsplit
export async function getSegsplit(segment: Segment): Promise<Segment | null> {
  const word = segment.word;

  // Check if word is simple-text (not compound)
  if (!word || !('text' in word) || ('words' in word)) {
    return null;
  }

  // Try to get split using segsplitMap
  const conjOf = segment.info?.seqSet ? segment.info.seqSet.slice(1) : [];
  const result = await getSegsplit_(word, conjOf);

  if (!result) {
    return null;
  }

  const [split, attrs] = result;

  // Safety check: all parts must exist
  if (!split || !split.every((p: any) => p !== null)) {
    return null;
  }

  // Parse attrs (can be number or object)
  const score = typeof attrs === 'number' ? attrs : attrs.score;
  const primary = typeof attrs === 'number' ? 0 : (attrs.primary ?? 0);
  const connector = typeof attrs === 'number' ? " " : (attrs.connector ?? " ");
  const root = typeof attrs === 'number' ? undefined : attrs.root;

  // Build compound-text
  // Extract seq arrays from all words
  const getSeqs = (word: any): number[] => {
    if ('seq' in word) {
      return Array.isArray(word.seq) ? word.seq : [word.seq];
    }
    return [];
  };

  const compoundWord: CompoundText = {
    text: split.map(w => getText(w)).join(""),
    kana: (await Promise.all(split.map(w => getKana(w)))).join(connector),
    primary: split[primary],
    words: split,
    seq: split.flatMap(w => getSeqs(w)),
    scoreMod: score
  };

  // Set conjugations to :root for specified positions
  if (root) {
    for (let i = 0; i < split.length; i++) {
      if (root.includes(i)) {
        split[i].conjugations = ':root';
      }
    }
  }

  // Create new segment (copy)
  const newSeg: Segment = {
    ...segment,
    word: compoundWord,
    text: compoundWord.text,
    score: (segment.score ?? 0) + score
  };

  // Recalculate info using calc-score on the primary word
  const [, info] = await calcScore(compoundWord.primary);
  newSeg.info = info;

  // Add conjugation data
  const conjData = await wordConjData(compoundWord);
  if (newSeg.info) {
    newSeg.info.conj = conjData;
  } else {
    newSeg.info = { conj: conjData };
  }

  return newSeg;
}

// Helper function: Like getSplit_() but looks in segsplitMap instead of splitMap
// In Lisp, this is handled by (let ((*split-map* *segsplit-map*)) ...) dynamic rebinding
async function getSegsplit_(reading: Reading, conjOf: number[] = []): Promise<[any[], SplitAttrs] | null> {
  const readingSeq = 'seq' in reading ? reading.seq : null;

  if (readingSeq !== null) {
    const splitFn = segsplitMap.get(readingSeq);
    if (splitFn) {
      return await splitFn(reading);
    }
  }

  // Try conjugation ancestors
  for (const seq of conjOf) {
    const splitFn = segsplitMap.get(seq);
    if (splitFn) {
      return await splitFn(reading);
    }
  }

  return null;
}

// End of segment splits section

// ============================================================================
// KANA HINTS SYSTEM
// Lines 805-1829 in dict-split.lisp
// ============================================================================

// Line 807-808: Kana hint modifier characters
// U+200C = ZERO WIDTH NON-JOINER
// U+200B = ZERO WIDTH SPACE
const KANA_HINT_MOD = '\u200c';
export const KANA_HINT_SPACE = '\u200b';

// Line 810: *hint-char-map*
const hintCharMap: Record<string, string> = {
  space: KANA_HINT_SPACE,
  mod: KANA_HINT_MOD,
};

// Line 812-818: *hint-simplify-map*
const hintSimplifyMap: [string, string][] = [
  [KANA_HINT_SPACE, ' '],
  [KANA_HINT_MOD + 'は', 'わ'],
  [KANA_HINT_MOD + 'ハ', 'ワ'],
  [KANA_HINT_MOD + 'へ', 'え'],
  [KANA_HINT_MOD + 'ヘ', 'エ'],
  [KANA_HINT_MOD, ''],
];

// Line 820-821: process-hints
// Note: simplify-ngrams is defined in characters.ts
import { simplifyNgrams } from './characters.js';

export function processHints(word: string): string {
  return simplifyNgrams(word, hintSimplifyMap);
}

// Line 823-824: strip-hints
export function stripHints(word: string): string {
  const hintChars = new Set(Object.values(hintCharMap));
  return Array.from(word).filter(c => !hintChars.has(c)).join('');
}

// Line 826: *kana-hint-map* (for testing - not used in main system)
// const kanaHintMap = new Map<number, SplitFunction>();

// Line 828-842: insert-hints
// hints are [[character-kw, position], ...]
type HintSpec = [string, number]; // [character-kw, position]

export function insertHints(str: string, hints: HintSpec[] | null): string {
  if (!hints || hints.length === 0) {
    return str;
  }

  const len = str.length;
  const positions: string[][] = Array(len + 1).fill(null).map(() => []);

  for (const [characterKw, position] of hints) {
    const char = hintCharMap[characterKw];
    if (char && position >= 0 && position <= len) {
      // Use unshift to match Lisp's push behavior (adds to front of list)
      positions[position].unshift(char);
    }
  }

  let result = '';
  for (let i = 0; i <= len; i++) {
    // Reverse to match Lisp: (loop for char in (reverse (aref positions i)))
    // unshift adds to front (like Lisp push), then we reverse to get correct order
    const reversed = positions[i].slice().reverse();
    for (const char of reversed) {
      result += char;
    }
    if (i < len) {
      result += str[i];
    }
  }

  return result;
}

// Line 844: *hint-map*
const hintMap = new Map<number, HintFunction>();

// Line 846-852: defhint macro
// (defmacro defhint (seqs (reading-var) &body body)
//   (unless (listp seqs) (setf seqs (list seqs)))
//   `(let ((,fn (lambda (,reading-var) ,@body)))
//      ,@(loop for seq in seqs collect `(setf (gethash ,seq *hint-map*) ,fn))))
// In TypeScript: direct function that registers hint functions in the map
function defhint(seqs: number | number[], fn: HintFunction): void {
  const seqList = Array.isArray(seqs) ? seqs : [seqs];
  for (const seq of seqList) {
    hintMap.set(seq, fn);
  }
}

// Line 854-874: def-simple-hint macro
// (defmacro def-simple-hint (seqs (&optional length-var kana-var reading-var) &body hints-def ...)
//   `(defhint ,seqs (,reading-var)
//      (block hint
//        (let* ((,kana-var (true-kana ,reading-var))
//               (,length-var (length ,kana-var))
//               ,@(loop for (var value) in hints-def ...))
//          (insert-hints (get-kana ,reading-var) (list ,@(loop for pair in hints-def ...)))))))
// In TypeScript: function that builds hint functions with variable bindings and tests
type HintDef =
  | { type: 'test', value: (k: string, l: number) => any }
  | { type: 'hint', keyword: string, position: (k: string, l: number, vars: any) => number | null };

interface DefSimpleHintOptions {
  seqs: number | number[];
  hints: HintDef[];
}

function defSimpleHint(options: DefSimpleHintOptions): void {
  const { seqs, hints } = options;

  const fn: HintFunction = async (reading: Reading) => {
    const k = await trueKana(reading); // Line 862: (true-kana reading-var)
    const l = k.length; // Line 863: (length kana-var)
    const vars: any = {};

    // Line 864-868: Process all test and variable assignments
    for (const hint of hints) {
      if (hint.type === 'test') {
        const result = hint.value(k, l);
        if (result === null || result === false) {
          return null; // Line 868: (return-from hint nil)
        }
        // Store test result in vars if it returned a value
        if (result !== true) {
          Object.assign(vars, result);
        }
      }
    }

    // Line 870-873: Collect hint specifications
    const hintSpecs: HintSpec[] = [];
    for (const hint of hints) {
      if (hint.type === 'hint') {
        const pos = hint.position(k, l, vars);
        if (pos !== null && pos !== undefined) {
          hintSpecs.push([hint.keyword, pos]);
        }
      }
    }

    // Line 870: (insert-hints (get-kana reading-var) (list ...))
    return insertHints(await getKana(reading), hintSpecs);
  };

  defhint(seqs, fn);
}

// Line 876-891: translate-hint-position
// Helper for def-easy-hint
function translateHintPosition(match: any[], position: number): number | null {
  let off = 0;
  let rem = position;

  for (const part of match) {
    if (typeof part === 'string') {
      // Simple string part
      const len = part.length;
      if (rem <= len) {
        return off + rem;
      }
      rem -= len;
      off += len;
    } else {
      // Array: [original, replacement]
      const len = part[0].length;
      const clen = part[1].length;

      if (rem < len) {
        return off + Math.min(1, Math.max(clen, rem));
      }
      if (rem === len) {
        return off + clen;
      }
      rem -= len;
      off += clen;
    }
  }

  return null;
}

// Line 893-896: translate-hints
function translateHints(match: any[], hints: HintSpec[]): HintSpec[] {
  const result: HintSpec[] = [];
  for (const [hint, pos] of hints) {
    const newPos = translateHintPosition(match, pos);
    if (newPos !== null) {
      result.push([hint, newPos]);
    }
  }
  return result;
}

// Line 898-908: *easy-hints-seqs* and check-easy-hints
// These are for testing only - we'll skip implementation for now
const easyHintsSeqs: number[] = [];

// Line 910-930: def-easy-hint macro
// (defmacro def-easy-hint (seq kanji-split)
//   (let* ((parts (split-sequence #\Space kanji-split))
//          (text (remove #\Space kanji-split))
//          (hints (loop with pos = 0
//                    for part in parts
//                    unless (zerop pos) collect (list :space pos)
//                    and if (find part '("は" "へ" "には" "とは") :test 'equal)
//                        collect (list :mod (+ pos (length part) -1))
//                    do (incf pos (length part)))))
//     `(progn
//        (push ,seq *easy-hints-seqs*)
//        (defhint (,seq) (,reading-var)
//          (when (typep ,reading-var 'simple-text) ; Line 925: check for kanji-text
//            (let* ((,rtext (true-kanji ,reading-var))  ; Line 926
//                   (,match (match-diff ,text ,rtext))   ; Line 927
//                   (,kr (match-readings ,rtext (true-kana ,reading-var)))) ; Line 928
//              (when (and ,match ,kr) ; Line 929
//                (insert-hints (get-kana ,reading-var) ; Line 930
//                             (translate-hints ,kr (translate-hints ,match ',hints))))))))))
// In TypeScript: function that generates hints from space-separated kanji splits
function defEasyHint(seq: number, kanjiSplit: string): void {
  // Line 911-919: Parse kanji split and calculate hint positions
  const parts = kanjiSplit.split(' ');
  const text = parts.join('');

  const hints: HintSpec[] = [];
  let pos = 0;

  for (let i = 0; i < parts.length; i++) {
    const part = parts[i];

    if (i > 0) {
      hints.push(['space', pos]); // Line 916: (:space pos)
    }

    if (['は', 'へ', 'には', 'とは'].includes(part)) {
      hints.push(['mod', pos + part.length - 1]); // Line 918: (:mod (+ pos (length part) -1))
    }

    pos += part.length;
  }

  easyHintsSeqs.push(seq); // Line 923: (push seq *easy-hints-seqs*)

  const fn: HintFunction = async (reading: Reading) => {
    // Line 925: (typep reading-var 'simple-text)
    // In Lisp: checks for simple-text (base class), but relies on nil checks
    // In TypeScript: more strict - check for 'kanji' type upfront
    if (getWordType(reading) !== 'kanji') {
      return null;
    }

    const rtext = await trueKanji(reading); // Line 926: (true-kanji reading-var)
    if (!rtext) return null; // Extra null safety

    const match = matchDiff(text, rtext); // Line 927: (match-diff text rtext)
    if (!match) {
      return null; // Line 929: (when (and match kr) ...)
    }

    const kana = await trueKana(reading); // Line 928: (true-kana reading-var)
    if (!kana) return null; // Extra null safety

    const kr = await matchReadings(rtext, kana); // Line 928: (match-readings rtext kana) - now async
    if (!kr) {
      return null; // Line 929: (when (and match kr) ...)
    }

    // Line 930: (insert-hints (get-kana reading-var) (translate-hints kr (translate-hints match hints)))
    return insertHints(await getKana(reading), translateHints(kr, translateHints(match, hints)));
  };

  defhint(seq, fn); // Line 924: (defhint (seq) ...)
}

// Line 932-939: get-hint
// (defun get-hint (reading)
//   (let ((hint-fn (gethash (seq reading) *hint-map*))
//         (conj-of (mapcar #'conj-data-from (word-conj-data reading))))
//     (if hint-fn
//         (funcall hint-fn reading)
//         (loop for seq in conj-of
//            for hint-fn = (gethash seq *hint-map*)
//            when hint-fn do (return (funcall hint-fn reading))))))
// In TypeScript: async version (wordConjData is async)
export async function getHint(reading: Reading): Promise<string | null> {
  const seq = reading.seq; // Line 933: (seq reading)
  const hintFn = hintMap.get(seq); // Line 933: (gethash (seq reading) *hint-map*)

  if (hintFn) {
    return await hintFn(reading); // Line 935: (funcall hint-fn reading) - now async
  }

  // Line 934-938: Try conjugation ancestors
  const conjData = await wordConjData(reading); // Line 934: (word-conj-data reading)
  if (conjData) {
    for (const data of conjData) { // Line 937: (loop for seq in conj-of ...)
      const conjSeq = data.from; // conj-data-from
      const conjHintFn = hintMap.get(conjSeq); // Line 937: (gethash seq *hint-map*)
      if (conjHintFn) {
        return await conjHintFn(reading); // Line 938: (return (funcall hint-fn reading)) - now async
      }
    }
  }

  return null;
}

// ============================================================================
// SIMPLE HINT DEFINITIONS (Lines 1006-1364)
// Manual definitions for complex hint patterns
// ============================================================================

// Lines 1006-1011: expressions ending with は/へ
defSimpleHint({ seqs: [2028920, 2029000], hints: [{ type: 'hint', keyword: 'mod', position: (k, l) => l - 1 }] });

// Lines 1013-1022: no space - expressions ending with は
defSimpleHint({ seqs: [1289480, 1289400, 1008450, 2215430, 2028950], hints: [{ type: 'test', value: (k) => k.endsWith('は') ? {} : null }, { type: 'hint', keyword: 'mod', position: (k, l) => l - 1 }] });

// Lines 1024-1088: with space - expressions ending with は
defSimpleHint({ seqs: [1006660, 1008500, 1307530, 1320830, 1324320, 1524990, 1586850, 1877880, 1897510, 1907300, 1912570, 2034440, 2098160, 2105820, 2134680, 2136300, 2176280, 2177410, 2177420, 2177430, 2177440, 2177450, 2256430, 2428890, 2523450, 2557290, 2673120, 2691570, 2702090, 2717440, 2717510, 2828541, 1217970, 1331520, 1907290, 1914670, 1950430, 2136680, 2181810, 2181730, 2576840, 1331510, 1010470, 2008290, 2136690, 2829815, 2830216, 2840063, 2841096, 2841959, 2844687, 2844836, 2850535, 2861249], hints: [{ type: 'test', value: (k) => k.endsWith('は') ? {} : null }, { type: 'hint', keyword: 'space', position: (k, l) => l - 1 }, { type: 'hint', keyword: 'mod', position: (k, l) => l - 1 }] });

// Lines 1090-1094: へと
defSimpleHint({ seqs: [2844416], hints: [{ type: 'hint', keyword: 'space', position: (k, l) => l - 1 }, { type: 'hint', keyword: 'mod', position: () => 0 }] });

// Lines 1096-1102: ところへ, 何方へ
defSimpleHint({ seqs: [2097010, 1009150], hints: [{ type: 'hint', keyword: 'space', position: (k, l) => l - 1 }, { type: 'hint', keyword: 'mod', position: (k, l) => l - 1 }] });

// Lines 1104-1112: それはそれは
defSimpleHint({ seqs: [2261800], hints: [{ type: 'hint', keyword: 'space', position: () => 2 }, { type: 'hint', keyword: 'mod', position: () => 2 }, { type: 'hint', keyword: 'space', position: () => 3 }, { type: 'hint', keyword: 'space', position: (k, l) => l - 1 }, { type: 'hint', keyword: 'mod', position: (k, l) => l - 1 }] });

// Lines 1114-1136: では/には ending
defSimpleHint({ seqs: [1009480, 1315860, 1406050, 2026610, 2061740, 2097310, 2101020, 2119920, 2134700, 2200100, 2407650, 2553140, 2762790, 1288910, 1423320, 2099850, 1006890], hints: [{ type: 'test', value: (k) => k.endsWith('は') ? {} : null }, { type: 'hint', keyword: 'space', position: (k, l) => l - 2 }, { type: 'hint', keyword: 'mod', position: (k, l) => l - 1 }] });

// Lines 1140-1152: では expressions
defSimpleHint({ seqs: [2089020, 2823770, 2098240, 2027020, 2135480, 2397760, 2724540, 2757720], hints: [{ type: 'test', value: (k) => { const deha = k.lastIndexOf('では'); return deha >= 0 ? { deha } : null; } }, { type: 'hint', keyword: 'mod', position: (k, l, vars) => vars.deha + 1 }] });

// Lines 1154-1188: ends with ではない
defSimpleHint({ seqs: [2027080, 2126160, 2126140, 2131120, 2136640, 2214830, 2221680, 2416950, 2419210, 2664520, 2682500, 2775790, 1343120, 2112270, 2404260, 2758400, 2827556, 2057560, 2841318, 2088970, 2833095, 2835662, 2841608, 2841609, 2845739, 2849457, 2850045, 2854412], hints: [{ type: 'test', value: (k) => { const deha = k.lastIndexOf('では'); return deha >= 0 ? { deha } : null; } }, { type: 'hint', keyword: 'space', position: (k, l, vars) => vars.deha }, { type: 'hint', keyword: 'mod', position: (k, l, vars) => vars.deha + 1 }] });

// Lines 1191-1203: では in the middle
defSimpleHint({ seqs: [2037860, 2694350, 2111220, 2694360, 2182700, 2142010], hints: [{ type: 'test', value: (k) => { const deha = k.lastIndexOf('では'); return deha >= 0 ? { deha } : null; } }, { type: 'hint', keyword: 'space', position: (k, l, vars) => vars.deha }, { type: 'hint', keyword: 'mod', position: (k, l, vars) => vars.deha + 1 }, { type: 'hint', keyword: 'space', position: (k, l, vars) => vars.deha + 2 }] });

// Lines 1206-1235: には in the middle
defSimpleHint({ seqs: [2057580, 2067990, 2103020, 2105980, 2152700, 2416920, 2418030, 2792210, 2792420, 2417920, 2598720, 2420170, 2597190, 2597800, 2057570, 2419360, 2121480, 2646440, 2740880, 2416860, 2156910, 2182690, 2848157], hints: [{ type: 'test', value: (k) => { const niha = k.lastIndexOf('には'); return niha >= 0 ? { niha } : null; } }, { type: 'hint', keyword: 'space', position: (k, l, vars) => vars.niha }, { type: 'hint', keyword: 'mod', position: (k, l, vars) => vars.niha + 1 }, { type: 'hint', keyword: 'space', position: (k, l, vars) => vars.niha + 2 }] });

// Lines 1239-1248: starts with には/とは
defSimpleHint({ seqs: [2181860, 2037320, 2125460, 2128060, 2070730], hints: [{ type: 'hint', keyword: 'mod', position: () => 1 }, { type: 'hint', keyword: 'space', position: () => 2 }] });

// Lines 1250-1258: 目には目を
defSimpleHint({ seqs: [2832044], hints: [{ type: 'test', value: (k) => { const niha = k.lastIndexOf('には'); return niha >= 0 ? { niha } : null; } }, { type: 'hint', keyword: 'space', position: (k, l, vars) => vars.niha }, { type: 'hint', keyword: 'mod', position: (k, l, vars) => vars.niha + 1 }, { type: 'hint', keyword: 'space', position: (k, l, vars) => vars.niha + 2 }, { type: 'hint', keyword: 'space', position: (k, l) => l - 1 }] });

// Lines 1261-1338: は in the middle
defSimpleHint({ seqs: [1008970, 1188440, 1193090, 1394290, 1855940, 1949380, 1981600, 1982230, 2018320, 2062980, 2078930, 2089520, 2089620, 2098150, 2108910, 2115570, 2118120, 2118430, 2118440, 2134480, 2135530, 2136710, 2141360, 2168360, 2173880, 2174570, 2176450, 2177240, 2200690, 2210960, 2213470, 2255320, 2275900, 2403520, 2408680, 2416870, 2416930, 2417040, 2417150, 2417980, 2418090, 2418280, 2418800, 2418920, 2419030, 2419350, 2419390, 2419590, 2419600, 2419610, 2419620, 2420180, 2583560, 2585230, 2593830, 2600530, 2618920, 2618990, 2708230, 2716900, 2737650, 2741810, 2744840, 2827754, 2831359, 2833597, 2839953, 2844002, 2858918, 2862330], hints: [{ type: 'test', value: (k) => { const ha = k.lastIndexOf('は'); return ha >= 0 ? { ha } : null; } }, { type: 'hint', keyword: 'space', position: (k, l, vars) => vars.ha }, { type: 'hint', keyword: 'mod', position: (k, l, vars) => vars.ha }, { type: 'hint', keyword: 'space', position: (k, l, vars) => vars.ha + 1 }] });

// Lines 1342-1350: そうはイカのキンタマ
defSimpleHint({ seqs: [2716860], hints: [{ type: 'test', value: (k) => { const ha = k.indexOf('は'); const no = k.lastIndexOf('の'); return ha >= 0 && no >= 0 ? { ha, no } : null; } }, { type: 'hint', keyword: 'space', position: (k, l, vars) => vars.ha }, { type: 'hint', keyword: 'mod', position: (k, l, vars) => vars.ha }, { type: 'hint', keyword: 'space', position: (k, l, vars) => vars.ha + 1 }, { type: 'hint', keyword: 'space', position: (k, l, vars) => vars.no }, { type: 'hint', keyword: 'space', position: (k, l, vars) => vars.no + 1 }] });

// Lines 1352-1363: 他所は他所うちはうち
defSimpleHint({ seqs: [2845260], hints: [{ type: 'test', value: (k) => { const ha1 = k.indexOf('は'); const ha2 = k.lastIndexOf('は'); const uu = k.indexOf('う'); return ha1 >= 0 && ha2 >= 0 && uu >= 0 ? { ha1, ha2, uu } : null; } }, { type: 'hint', keyword: 'space', position: (k, l, vars) => vars.ha1 }, { type: 'hint', keyword: 'mod', position: (k, l, vars) => vars.ha1 }, { type: 'hint', keyword: 'space', position: (k, l, vars) => vars.ha1 + 1 }, { type: 'hint', keyword: 'space', position: (k, l, vars) => vars.uu }, { type: 'hint', keyword: 'space', position: (k, l, vars) => vars.ha2 }, { type: 'hint', keyword: 'mod', position: (k, l, vars) => vars.ha2 }, { type: 'hint', keyword: 'space', position: (k, l, vars) => vars.ha2 + 1 }] });

// ============================================================================
// EASY HINT DEFINITIONS (Lines 1366-1829)
// 420 definitions for expressions with は/へ particles
// ============================================================================

defEasyHint(1238480, "郷 に 入って は 郷 に 従え");
defEasyHint(1338260, "出る 釘 は 打たれる");
defEasyHint(1471680, "馬鹿 は 死ななきゃ 治らない");
defEasyHint(1566340, "屁 と 火事 は もと から 騒ぐ");
defEasyHint(1638390, "用心する に 如く は ない");
defEasyHint(1985430, "出る 杭 は 打たれる");
defEasyHint(2078950, "背 に 腹 は かえられない");
defEasyHint(2083430, "瓜 の 蔓 に 茄子 は ならぬ");
defEasyHint(2093380, "敵 は 本能寺 に あり");
defEasyHint(2101500, "無い 袖 は 振れぬ");
defEasyHint(2109530, "継続 は 力 なり");
defEasyHint(2111700, "事実 は 小説 より 奇 なり");
defEasyHint(2111710, "事実 は 小説 よりも 奇 なり");
defEasyHint(2113730, "縁 は 異なもの 味なもの");
defEasyHint(2113740, "縁は異なもの");
defEasyHint(2125770, "その手 は 食わない");
defEasyHint(2130410, "すまじき もの は 宮仕え");
defEasyHint(2140350, "時 は 金 なり");
defEasyHint(2140990, "青 は 藍 より 出でて 藍 より 青し");
defEasyHint(2152710, "渡る 世間 に 鬼 は ない");
defEasyHint(2152850, "病 は 気 から");
defEasyHint(2152960, "甲 の 薬 は 乙 の 毒");
defEasyHint(2159970, "好奇心 は 猫 をも 殺す");
defEasyHint(2159990, "甘い物 は 別腹");
defEasyHint(2168350, "ペン は 剣 よりも 強し");
defEasyHint(2176630, "千金 の 裘 は 一狐 の 腋 に 非ず");
defEasyHint(2177220, "宝 さかって 入る 時 は さかって 出る");
defEasyHint(2178680, "出す こと は 舌を出す も 嫌い");
defEasyHint(2195810, "に 越した こと は ない");
defEasyHint(2209690, "男 に 二言 は ない");
defEasyHint(2216540, "背 に 腹 は 替えられぬ");
defEasyHint(2219580, "寝言 は 寝て から 言え");
defEasyHint(2411180, "用心する に 越した こと は ない");
defEasyHint(2416580, "悪い 事 は 出来ぬ もの");
defEasyHint(2416650, "衣 ばかり で 和尚 は できぬ");
defEasyHint(2416680, "一銭 を 笑う 者 は 一銭 に 泣く");
defEasyHint(2417140, "血 は 水 よりも 濃い");
defEasyHint(2417180, "見る と 聞く とは 大違い");
defEasyHint(2417270, "後 は 野となれ 山となれ");
defEasyHint(2417540, "昨日 の 友 は 今日 の 敵");
defEasyHint(2417580, "山椒 は 小粒 でも ぴりりと 辛い");
defEasyHint(2417750, "蒔かぬ 種 は 生えぬ");
defEasyHint(2417760, "鹿 を 追う 者 は 山 を 見ず");
defEasyHint(2418060, "色事 は 思案 の 外");
defEasyHint(2418100, "寝る ほど 楽 は なかりけり");
defEasyHint(2418220, "人 の 口 に 戸 は 立てられず");
defEasyHint(2418250, "人 は パン のみ にて 生くる 者 に 非ず");
defEasyHint(2418270, "人 は 見かけ に よらぬ もの");
defEasyHint(2418490, "世間 の 口 に 戸 は 立てられぬ");
defEasyHint(2418500, "世間 は 広い 様 で 狭い");
defEasyHint(2418600, "生ある 者 は 必ず 死あり");
defEasyHint(2418700, "栴檀 は 双葉 より 芳し");
defEasyHint(2418720, "前車 の 覆る は 後車 の 戒め");
defEasyHint(2418900, "タダ より 高い もの は ない");
defEasyHint(2418970, "知らぬ は 亭主 ばかり なり");
defEasyHint(2419060, "釣り落とした 魚 は 大きい");
defEasyHint(2419120, "転んでも ただ は 起きぬ");
defEasyHint(2419190, "逃がした 魚 は 大きい");
defEasyHint(2419260, "二兎 を 追う 者 は 一兎 をも 得ず");
defEasyHint(2419270, "二度ある こと は 三度ある");
defEasyHint(2419410, "馬鹿 と はさみ は 使いよう");
defEasyHint(2419420, "馬鹿 に つける 薬 は ない");
defEasyHint(2419450, "板子 一枚 下 は 地獄");
defEasyHint(2419530, "夫婦喧嘩 は 犬 も 食わない");
defEasyHint(2419690, "文 は 人 なり");
defEasyHint(2419710, "聞く と 見る とは 大違い");
defEasyHint(2419730, "便り の ない の は よい 便り");
defEasyHint(2419800, "名の無い 星 は 宵 から 出る");
defEasyHint(2419970, "余 の 辞書 に 不可能 という 文字 は ない");
defEasyHint(2420010, "用心 に 越した こと は ない");
defEasyHint(2420110, "例外 の ない 規則 は ない");
defEasyHint(2420120, "歴史 は 繰り返す");
defEasyHint(2420190, "驕る 平家 は 久しからず");
defEasyHint(2424500, "目的 の ために は 手段 を 選ばない");
defEasyHint(2442180, "命 に 別条 は ない");
defEasyHint(2570900, "藁 で 束ねても 男 は 男");
defEasyHint(2580730, "知 は 力 なり");
defEasyHint(2582770, "既往 は 咎めず");
defEasyHint(2641030, "相手 にとって 不足 は ない");
defEasyHint(2694370, "転んでも ただ は 起きない");
defEasyHint(2738830, "お客様 は 神様 です");
defEasyHint(2757560, "宵越し の 銭 は 持たない");
defEasyHint(2758920, "立っている もの は 親 でも 使え");
defEasyHint(2761670, "先 の こと は 分からない");
defEasyHint(2776820, "理屈 と 膏薬 は どこ へ でも つく");
defEasyHint(2783090, "疑わしき は 罰せず");
defEasyHint(2784400, "蟹 は 甲羅 に 似せて 穴 を 掘る");
defEasyHint(2789240, "嘘 と 坊主 の 頭 は ゆった ことがない");
defEasyHint(2792090, "正気 とは 思えない");
defEasyHint(2797740, "一円 を 笑う 者 は 一円 に 泣く");
defEasyHint(2798610, "お神酒 上がらぬ 神 は ない");
defEasyHint(2826812, "悪い こと は 言わない");
defEasyHint(2828308, "吐いた 唾 は 飲めぬ");
defEasyHint(2830029, "明けない 夜 は ない");
defEasyHint(2833956, "山 より 大きな 猪 は 出ぬ");
defEasyHint(2833957, "老兵 は 死なず ただ 消え去る のみ");
defEasyHint(2833961, "梅 は 食う とも 核 食う な 中 に 天神 寝てござる");
defEasyHint(2833986, "悪 に 強い は 善 にも 強い");
defEasyHint(2108440, "過ちて は 則ち 改むる に 憚る こと 勿れ");
defEasyHint(2417420, "降り懸かる 火の粉 は 払わねば ならぬ");
defEasyHint(2418640, "静かに 流れる 川 は 深い");
defEasyHint(2835355, "無い 物 は 無い");
defEasyHint(2835504, "気 は 確か");
defEasyHint(2835673, "見る は 法楽");
defEasyHint(2836181, "持つべき もの は 友");
defEasyHint(2836183, "持つべき もの は 友人");
defEasyHint(2836500, "経験者 は 語る");
defEasyHint(2741060, "本日 は 晴天 なり");
defEasyHint(2836784, "物 か は");
defEasyHint(2837023, "明日 は 我が身");
defEasyHint(2837133, "右 に 出る 者 は ない");
defEasyHint(2839180, "便り が ない の は よい 便り");
defEasyHint(2839838, "無理 は 禁物");
defEasyHint(2839934, "元 は と言えば");
defEasyHint(2840462, "すべて 世 は こと も なし");
defEasyHint(2840493, "体 は 正直");
defEasyHint(2840733, "話し上手 は 聞き上手");
defEasyHint(2840752, "色男 金 と 力 は なかりけり");
defEasyHint(2841085, "話 は 別");
defEasyHint(2841164, "愛 は 盲目");
defEasyHint(2841165, "恋 は 闇");
defEasyHint(2841585, "礼 は はずむ");
defEasyHint(2842361, "失敗 は 成功 の 母");
defEasyHint(2843805, "細工 は 流々 仕上げ を 御覧じろ");
defEasyHint(2843453, "立てば 芍薬 座れば 牡丹 歩く姿 は 百合 の 花");
defEasyHint(2843281, "九層 の 台 は 累土 より 起こる");
defEasyHint(2844718, "老いて は 益々壮ん なるべし");
defEasyHint(2844721, "若い時 は 二度ない");
defEasyHint(2844870, "習わぬ 経 は 読めぬ");
defEasyHint(2844963, "避けて は 通れない");
defEasyHint(2844990, "戴くもの は 夏 も 小袖");
defEasyHint(2845002, "魚 は 頭 から 腐る");
defEasyHint(2845919, "人 は 見目 より ただ 心");
defEasyHint(2845920, "人 に 善言 を 与うる は 布帛 よりも 煖かなり");
defEasyHint(2846470, "気 は 無い");
defEasyHint(2847076, "虎 は 千里 往って 千里 還る");
defEasyHint(2848309, "それ は ない");
defEasyHint(2847626, "話 は 早い");
defEasyHint(2848813, "美人 は 三日 で 飽きる");
defEasyHint(2849042, "過去 は 過去");
defEasyHint(2849859, "寝言 は 寝て 言え");
defEasyHint(2851317, "困った 時 は お互い様");
defEasyHint(2855884, "逃げた 魚 は 大きい");
defEasyHint(2855675, "鯛 も 一人 は うまからず");
defEasyHint(2856828, "ごめん で 済む なら 警察 は いらない");
defEasyHint(2857339, "他 は 無い");
defEasyHint(2857468, "無 から は 何も 生じない");
defEasyHint(2861000, "今 は 亡き");
defEasyHint(2861001, "今 は 無き");
defEasyHint(2861231, "言って は なんです が");
defEasyHint(2862670, "夫婦 は 合わせ鏡");
defEasyHint(2863444, "然 は 然りながら");
defEasyHint(2863557, "人生 は 一度きり");
defEasyHint(2416600, "悪人 は 畳 の 上 で は 死ねない");
defEasyHint(2767400, "鬼 は 外 福 は 内");
defEasyHint(2418260, "人 は 一代 名 は 末代");
defEasyHint(2828341, "花 は 桜木 人 は 武士");
defEasyHint(2086560, "鶴 は 千年 亀 は 万年");
defEasyHint(2152790, "楽 は 苦 の 種 苦 は 楽 の 種");
defEasyHint(2154700, "旅 は 道連れ 世 は 情け");
defEasyHint(2158840, "男 は 度胸 女 は 愛敬");
defEasyHint(2168380, "沈黙 は 金 雄弁 は 銀");
defEasyHint(2417120, "芸術 は 長く 人生 は 短し");
defEasyHint(2417230, "言う は 易く 行う は 難し");
defEasyHint(2417500, "今日 は 人の身 明日 は 我が身");
defEasyHint(2417930, "女 は 弱し されど 母 は 強し");
defEasyHint(2418180, "親 は 無くても 子 は 育つ");
defEasyHint(2418550, "生 は 難く 死 は 易し");
defEasyHint(2418630, "声 は すれども 姿 は 見えず");
defEasyHint(2418650, "昔 は 昔 今 は 今");
defEasyHint(2418740, "創業 は 易く 守成 は 難し");
defEasyHint(2419150, "東 は 東 西 は 西");
defEasyHint(2419950, "雄弁 は 銀 沈黙 は 金");
defEasyHint(2419960, "夕焼け は 晴れ 朝焼け は 雨");
defEasyHint(2420080, "旅 は 心 世 は 情け");
defEasyHint(2424520, "去る者 は 追わず 来たる者 は 拒まず");
defEasyHint(2558710, "遠き は 花 の 香 近き は 糞 の 香");
defEasyHint(2719710, "フグ は 食いたし 命 は 惜しし");
defEasyHint(2790690, "弓 は 袋 に 太刀 は 鞘");
defEasyHint(2828900, "山中 の 賊 を 破る は 易く 心中 の 賊 を 破る は 難し");
defEasyHint(2833976, "君子 は 周して 比せず 小人 は 比して 周せず");
defEasyHint(2833959, "知る者 は 言わず 言う者 は 知らず");
defEasyHint(2833900, "虎 は 死して 皮 を 留め 人 は 死して 名 を 残す");
defEasyHint(2570040, "朝焼け は 雨 夕焼け は 晴れ");
defEasyHint(2419570, "腹 が 減って は 戦 は 出来ぬ");
defEasyHint(2255410, "浜 の 真砂 は 尽きるとも 世 に 盗人 の 種 は 尽きまじ");
defEasyHint(2419720, "聞く は 一時 の 恥 聞かぬ は 末代 の 恥");
defEasyHint(2419910, "問う は 一旦 の 恥 問わぬ は 末代 の 恥");
defEasyHint(2757120, "問う は 一度 の 恥 問わぬ は 末代 の 恥");
defEasyHint(2834642, "柳 は 緑 花 は 紅");
defEasyHint(2836571, "聞く は 一時 の 恥 聞かぬ は 一生 の 恥");
defEasyHint(2836731, "男 は 松 女 は 藤");
defEasyHint(2839233, "転がる 石 に は 苔 は 付かない");
defEasyHint(2835297, "此れ は 此れ は");
defEasyHint(2845254, "上戸 は 毒 を 知らず 下戸 は 薬 を 知らず");
defEasyHint(2845255, "文 は やりたし 書く手 は 持たぬ");
defEasyHint(2845915, "旅 は 情け 人 は 心");
defEasyHint(2845916, "人 は 人 我 は 我");
defEasyHint(2847494, "行き は 良い良い 帰り は 怖い");
defEasyHint(2848603, "始め は 処女 の 如く 後 は 脱兎 の 如し");
defEasyHint(2153170, "目 には 目 を 歯 には 歯 を");
defEasyHint(2422970, "人 には 添うて見よ 馬 には 乗って見よ");
defEasyHint(2833500, "馬 には 乗って見よ 人 には 添うて見よ");
defEasyHint(2857020, "居候 三杯目 には そっと出し");
defEasyHint(2862061, "無下 には できない");
defEasyHint(2863521, "上 には 上 が いる");
defEasyHint(2863544, "右に出る 者 は いない");
defEasyHint(1008660, "隣 の 芝生 は 青い");
defEasyHint(1204760, "蛙 の 子 は 蛙");
defEasyHint(2113380, "金 は 天下 の 回り物");
defEasyHint(2141020, "秋の日 は 釣瓶落とし");
defEasyHint(2144050, "秋 の 鹿 は 笛 に 寄る");
defEasyHint(2152870, "柳 の 下 に いつも 泥鰌 は おらぬ");
defEasyHint(2152930, "一年 の 計 は 元旦 に あり");
defEasyHint(2158240, "若い 時 の 苦労 は 買うてもせよ");
defEasyHint(2202070, "勝負 は 時 の 運");
defEasyHint(2227110, "カエサル の もの は カエサル に");
defEasyHint(2417800, "蛇 の 道 は 蛇");
defEasyHint(2418170, "親 の 光 は 七光り");
defEasyHint(2420070, "旅 の 恥 は 掻き捨て");
defEasyHint(2420100, "隣 の 花 は 赤い");
defEasyHint(2582990, "狐 の 子 は 頬白");
defEasyHint(2697510, "君父 の 讐 は 倶に 天 を 戴かず");
defEasyHint(2827732, "若い 時 の 苦労 は 買ってもせよ");
defEasyHint(2835925, "煩悩 の 犬 は 追えども 去らず");
defEasyHint(2174750, "己 の 欲せざる 所 は 人 に 施す 勿れ");
defEasyHint(2838865, "だけ の 事 は ある");
defEasyHint(2838606, "今日 の ところ は");
defEasyHint(2838426, "木 の 実 は 本 へ 落つ");
defEasyHint(2845252, "下戸 の 建てた 蔵 は ない");
defEasyHint(2858678, "自分 の こと は 棚 に 上げる");
defEasyHint(2859764, "他人 の 不幸 は 蜜 の 味");
defEasyHint(2860668, "親 の 恩 は 子 で 送る");
defEasyHint(1487700, "必要 は 発明 の 母");
defEasyHint(1320150, "失敗 は 成功 の もと");
defEasyHint(2126750, "悪妻 は 百年 の 不作");
defEasyHint(2141010, "逢う は 別れ の 始め");
defEasyHint(2144040, "商い は 牛 の よだれ");
defEasyHint(2152780, "苦 は 楽 の 種");
defEasyHint(2211780, "情け は 人 の 為 ならず");
defEasyHint(2416720, "嘘つき は 泥棒 の 始まり");
defEasyHint(2416970, "教うる は 学ぶ の 半ば");
defEasyHint(2417350, "口 は 禍 の 元");
defEasyHint(2417610, "子 は 三界 の 首枷");
defEasyHint(2417810, "弱き者 よ 汝 の 名 は 女 也");
defEasyHint(2418340, "人間 は 万物 の 霊長");
defEasyHint(2418470, "据え膳 食わぬ は 男 の 恥");
defEasyHint(2418540, "正直 は 一生 の 宝");
defEasyHint(2418590, "生兵法 は 大怪我 の もと");
defEasyHint(2419020, "朝起き は 三文 の 徳");
defEasyHint(2420140, "恋 は 思案 の 外");
defEasyHint(2550210, "幸運 の 女神 は 前髪 しかない");
defEasyHint(2591070, "火事 と 喧嘩 は 江戸 の 華");
defEasyHint(2796370, "禍福 は 糾える 縄 の ごとし");
defEasyHint(2833968, "人間 は 万物 の 尺度 である");
defEasyHint(2833958, "言葉 は 身 の 文");
defEasyHint(2832652, "挨拶 は 時 の 氏神");
defEasyHint(2111130, "早起き は 三文 の 徳");
defEasyHint(2417830, "酒 は 百薬 の 長");
defEasyHint(2837015, "落ち武者 は 薄 の 穂 に 怖ず");
defEasyHint(2837756, "風邪 は 万病 の 元");
defEasyHint(2842831, "口 は 災い の 門");
defEasyHint(2843962, "生 は 死 の 始め");
defEasyHint(2853754, "正直 は 最善 の 策");
defEasyHint(2860665, "子 は 親 の 鏡");
defEasyHint(2860666, "子供 は 親 の 鏡");
defEasyHint(2860667, "子供 は 風 の 子");
defEasyHint(2860677, "兄弟 は 他人 の 始まり");
defEasyHint(1213500, "甘言 は 偶人 を 喜ばす");
defEasyHint(1470130, "能 ある 鷹 は 爪 を 隠す");
defEasyHint(1929200, "悪貨 は 良貨 を 駆逐する");
defEasyHint(2077530, "類 は 友 を 呼ぶ");
defEasyHint(2079030, "大 は 小 を 兼ねる");
defEasyHint(2089460, "鳴く 猫 は 鼠 を 捕らぬ");
defEasyHint(2102600, "おぼれる 者 は わら をも つかむ");
defEasyHint(2168340, "天 は 自ら 助くる 者 を 助く");
defEasyHint(2416900, "急いて は 事 を 仕損ずる");
defEasyHint(2417110, "芸 は 身 を 助く");
defEasyHint(2419100, "天 は 二物 を 与えず");
defEasyHint(2419810, "名 は 体 を 表す");
defEasyHint(2520680, "義 を 見てせざる は 勇なきなり");
defEasyHint(2627320, "急いて は 事 を 仕損じる");
defEasyHint(2686140, "大人 は 赤子 の 心 を 失わず");
defEasyHint(2833952, "足る を 知る 者 は 富む");
defEasyHint(2832631, "井蛙 は 以って 海 を 語る 可からず");
defEasyHint(2832604, "良禽 は 木 を 択んで棲む");
defEasyHint(2757650, "日光 を 見ない 中 は 結構 と言う な");
defEasyHint(2419440, "敗軍 の 将 は 兵 を 語らず");
defEasyHint(2834645, "飢えたる 犬 は 棒 を 恐れず");
defEasyHint(2836094, "満 は 損 を 招く");
defEasyHint(2844015, "大徳 は 小怨 を 滅す");
defEasyHint(2844292, "氷 は 水 より 出でて 水 よりも 寒し");
defEasyHint(2845250, "芸 は 身 を 助ける");
defEasyHint(2845917, "我 は 仮説 を 作らず");
defEasyHint(2845918, "歌人 は 居ながらにして 名所 を 知る");
defEasyHint(2846531, "老いたる 馬 は 道 を 忘れず");
defEasyHint(2847018, "名人 は 人 を 謗らず");
defEasyHint(2850060, "赤き は 酒 の 咎");
defEasyHint(2850189, "君子 の 交わり は 淡きこと 水 の ごとし");
defEasyHint(2855699, "謀 は 密なる を 良しとす");
defEasyHint(2859193, "百里 を 行く 者 は 九十 を 半ばとす");
defEasyHint(2859070, "獅子 は 我が子 を 千尋 の 谷 に 落とす");
defEasyHint(2860664, "子供 は 親 の 背中 を 見て 育つ");
defEasyHint(2095170, "天才 と 狂人 は 紙一重");
defEasyHint(2237240, "女房 と 畳 は 新しい 方がいい");
defEasyHint(2835775, "今 となって は");
defEasyHint(2847205, "バカ と 煙 は 高い 所 へ 上る");
defEasyHint(2847632, "下戸 と 化け物 は ない");
defEasyHint(2124980, "そう は 問屋 が 卸さない");
defEasyHint(2395080, "過ぎたる は 及ばざる が ごとし");
defEasyHint(2395090, "過ぎたる は 猶及ばざる が 如し");
defEasyHint(2417400, "慌てる 乞食 は もらい が 少ない");
defEasyHint(2419860, "明日 は 明日 の 風 が 吹く");
defEasyHint(2852239, "犬 が 西 向きゃ 尾 は 東");
defEasyHint(2852243, "雨 の 降る 日 は 天気 が 悪い");
defEasyHint(2862433, "商人 は 損して いつか 倉 が 建つ");
defEasyHint(2863524, "まさか とは 思う が");
defEasyHint(2138600, "百聞 は 一見 に しかず");
defEasyHint(2153120, "良薬 は 口 に 苦し");
defEasyHint(2153130, "目 は 口 ほど に 物を言う");
defEasyHint(2168390, "鉄 は 熱い うち に 打て");
defEasyHint(2171910, "去る者 は 日々 に 疎し");
defEasyHint(2416560, "ローマ は 一日 に して 成らず");
defEasyHint(2416730, "運 は 天 に 在り");
defEasyHint(2416800, "火 の ない ところ に 煙 は 立たない");
defEasyHint(2417160, "健全なる 精神 は 健全なる 身体 に 宿る");
defEasyHint(2417390, "孝行 の したい 時分 に 親 は なし");
defEasyHint(2418120, "新しい 酒 は 古い 革袋 に 入れる");
defEasyHint(2418130, "深い 川 は 静かに 流れる");
defEasyHint(2418450, "水 は 低きに 流る");
defEasyHint(2418750, "すべて の 道 は ローマ に 通ず");
defEasyHint(2419080, "鉄 は 熱い うち に 鍛え よ");
defEasyHint(2420150, "老いて は 子 に 従え");
defEasyHint(2566010, "秋茄子 は 嫁 に 食わす な");
defEasyHint(2832573, "巧詐 は 拙誠 に 如かず");
defEasyHint(2704850, "花泥棒 は 罪 に ならない");
defEasyHint(2837518, "文 は 武 に 勝る");
defEasyHint(2837552, "巧遅 は 拙速 に 如かず");
defEasyHint(2842829, "悪名 は 無名 に 勝る");
defEasyHint(2845256, "志 は 松 の 葉 に 包め");
defEasyHint(2845443, "天災 は 忘れた頃 に やってくる");
defEasyHint(2846430, "凝って は 思案 に 能わず");
defEasyHint(2851107, "女 は 三界 に 家 なし");
defEasyHint(2855268, "秋刀魚 は 目黒 に 限る");
defEasyHint(2859282, "子供 は 三歳 までに 一生分 の 親孝行 を する");
defEasyHint(2854538, "志ある者 は 事 竟に 成る");
defEasyHint(2418150, "親 に 似ぬ 子 は 鬼子");
defEasyHint(2419940, "柳 の 下 に 何時も 泥鰌 は 居ない");
defEasyHint(2832738, "身体髪膚 これ を 父母 に 受くあえて 毀傷せざる は 孝 の 始めなり");
defEasyHint(2834655, "親 の 意見 と 茄子 の 花 は 千 に 一つ も 無駄 は ない");
defEasyHint(2830412, "他 に 方法 は 無い");
defEasyHint(2666530, "墓 に 布団 は 着せられぬ");
defEasyHint(2847238, "知らん が 為に 我 は 信ず");
defEasyHint(2854601, "事 と 次第 によって は");
defEasyHint(2855600, "武士 に 二言 は ない");
defEasyHint(2863450, "人の口 に 戸 は 立てられぬ");
defEasyHint(2204530, "ヘブライ人 へ の 手紙");
defEasyHint(2813120, "ヘブル人 へ の 手紙");
defEasyHint(2839843, "上 を 下 へ");
defEasyHint(2839846, "上 や 下 へ の 大騒ぎ");
defEasyHint(2841303, "足下 へ も 寄りつけない");
defEasyHint(1151370, "悪 の 道 へ 誘う");
defEasyHint(1171020, "右 から 左 へ");
defEasyHint(1898770, "中 へ 入る");
defEasyHint(2125750, "そこ へ 持ってきて");
defEasyHint(2129780, "目 から 鼻 へ 抜ける");
defEasyHint(2177720, "棚 へ 上げる");
defEasyHint(2402730, "故郷 へ 錦 を 飾る");
defEasyHint(2431220, "への字 に 結んだ 口");
defEasyHint(2515280, "力 へ の 意志");
defEasyHint(2515290, "権力 へ の 意志");
defEasyHint(2716340, "平均 へ の 回帰");
defEasyHint(2738180, "右 へ 倣え");
defEasyHint(2826689, "東 へ 東 へ");
defEasyHint(2831475, "脇 へ それる");
defEasyHint(2219570, "元 へ");
defEasyHint(2017030, "次 から 次 へ と");
defEasyHint(2102190, "上 を 下 へ の 大騒ぎ");
defEasyHint(2845308, "寺 から 里 へ");
defEasyHint(2849371, "何処 へ やら");
defEasyHint(2204790, "コリント人 へ の 手紙");
defEasyHint(2204800, "ガラテヤ人 へ の 手紙");
defEasyHint(2204840, "ローマ人 へ の 手紙");
defEasyHint(2859067, "そこ へ 行く と");
defEasyHint(2834606, "親 は 無く とも 子 は 育つ");
defEasyHint(2834583, "病 は 口 より 入り 禍 は 口 より 出ず");
defEasyHint(2834582, "安物 は 高物");
defEasyHint(2834576, "目 は 心 の 鏡");
defEasyHint(2834575, "目 は 心 の 窓");
defEasyHint(2834651, "冷や酒 と 親 の 意見 は 後 で きく");
defEasyHint(2834564, "火 の 無い ところ に 煙 は 立たなぬ");
defEasyHint(2834563, "徳 は 孤 ならず 必ず 隣 あり");
defEasyHint(2834560, "君子 は 器 ならず");
defEasyHint(2834416, "馬鹿 は 風邪 を 引かない");
defEasyHint(2834363, "墨 は 餓鬼 に 磨らせ 筆 は 鬼 に 持たせよ");
defEasyHint(2834360, "信 は 荘厳 より 起こる");
defEasyHint(2834321, "父母 の 恩 は 山 よりも 高く 海 よりも 深し");
defEasyHint(2834318, "二人 は 伴侶 三人 は 仲間割れ");
defEasyHint(2834316, "人 の 花 は 赤い");
defEasyHint(2834313, "紅 は 園生 に 植えても 隠れなし");
defEasyHint(2834310, "地獄 は 壁一重");
defEasyHint(2834309, "人 は 死して 名 を 留む");
defEasyHint(2834308, "浮世 は 夢");
defEasyHint(2834287, "隣 の 芝 は 青い");
defEasyHint(2834263, "弱き者 汝 の 名 は 女 なり");
defEasyHint(2834244, "知識 は 力 なり");
defEasyHint(2834239, "武士 は 食わねど 高楊枝");
defEasyHint(2834233, "貧 は 世界 の 福 の 神");
defEasyHint(2834232, "光 は 東 から");
defEasyHint(2834228, "馬 に 乗る まで は 牛 に 乗れ");
defEasyHint(2834227, "言わぬ は 言う に 優る");
defEasyHint(2834224, "合わせ物 は 離れ物");
defEasyHint(2834220, "悪 は 延べよ");
defEasyHint(2833980, "牛 は 牛連れ 馬 は 馬連れ");
defEasyHint(2833979, "馬鹿 と 天才 は 紙一重");
defEasyHint(2833939, "長い物 には 巻かれ よ");
defEasyHint(2833938, "長い物 には 巻かれろ");
defEasyHint(2842906, "ない 訳 には 行かない");
defEasyHint(2835463, "人目 も はばからず");
defEasyHint(2849004, "ギョエテ とは 俺 の こと か と ゲーテ いい");
