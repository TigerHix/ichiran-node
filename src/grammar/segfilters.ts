// Grammar segfilters module
// Extracted from dict-grammar.ts (Lines 2005-2250)

import type { Segment, SegmentList, ConjData } from '../types.js';
import { isCompoundText, isSimpleWord } from '../types.js';
import type { SegfilterFunction } from './types.js';
import { getText } from '../dict/utils.js';
import { CONJ_ADJECTIVE_LITERARY } from '../dict/errata.js';

// Line 1021: defparameter *segfilter-list*
const segfilterList: SegfilterFunction[] = [];

// ============================================================================
// HELPER FUNCTIONS
// ============================================================================

// Line 1023-1027: defmacro defsegfilter
function defsegfilter(_name: string, fn: SegfilterFunction): void {
  if (!segfilterList.includes(fn)) {
    segfilterList.push(fn);
  }
}

// Line 1029-1034: defun classify
function classify<T>(filter: (element: T) => boolean, list: T[]): [T[], T[]] {
  const yep: T[] = [];
  const nope: T[] = [];

  for (const element of list) {
    if (filter(element)) {
      yep.push(element);
    } else {
      nope.push(element);
    }
  }

  return [yep, nope];
}

// Helper: make segment list from old list and new segments
function makeSegmentListFrom(oldSegmentList: SegmentList, segments: Segment[]): SegmentList {
  return {
    ...oldSegmentList,
    segments
  };
}

// Helper: filter segments by seq set
function filterInSeqSet(...seqs: number[]): (segment: Segment) => boolean {
  return (segment: Segment) => {
    const info = segment.info;
    if (!info) return false;

    const seqSet = info.seqSet || [];
    return seqs.some(seq => seqSet.includes(seq));
  };
}

// Helper: filter segments by seq set (simple words only)
function filterInSeqSetSimple(...seqs: number[]): (segment: Segment) => boolean {
  return (segment: Segment) => {
    const word = segment.word;
    if (!word || !isSimpleWord(word)) return false;
    const seq = word.seq;

    // Check that word is not compound (seq is not a list)
    if (Array.isArray(seq)) return false;

    const info = segment.info;
    if (!info) return false;

    const seqSet = info.seqSet || [];
    return seqs.some(s => seqSet.includes(s));
  };
}

// Helper: filter by conjugation type
function filterIsConjugation(conjType: number): (segment: Segment) => boolean {
  return (segment: Segment) => {
    const info = segment.info;
    if (!info) return false;

    const conj = info.conj || [];
    return conj.some((cdata: ConjData) => cdata.prop.conjType === conjType);
  };
}

// Helper: filter compound words by last word seq
function filterIsCompoundEnd(...seqs: number[]): (segment: Segment) => boolean {
  return (segment: Segment) => {
    const word = segment.word;
    if (!word || !isCompoundText(word)) return false;

    // word is a CompoundText
    const words = word.words;
    const lastWord = words[words.length - 1];
    // lastWord can be SimpleWord (has seq) or ProxyText (no direct seq)
    const lastSeq = isSimpleWord(lastWord) ? lastWord.seq : undefined;
    return lastSeq !== undefined && seqs.includes(lastSeq);
  };
}

// Helper: filter compound words by last word text
function filterIsCompoundEndText(...texts: string[]): (segment: Segment) => boolean {
  return (segment: Segment) => {
    const word = segment.word;
    if (!word || !isCompoundText(word)) return false;

    // word is a CompoundText
    const words = word.words;
    const lastWord = words[words.length - 1];
    const lastText = getText(lastWord);
    return texts.includes(lastText);
  };
}

// Noun particles constant (from synergies)
const NOUN_PARTICLES = [
  2028920, // は
  2028930, // が
  2028990, // に
  2028980, // で
  2029000, // へ
  1007340, // だけ
  1579080, // ごろ
  1525680, // まで
  2028940, // も
  1582300, // など
  2215430, // には
  1469800, // の
  1009990, // のみ
  2029010, // を
  1005120, // さえ
  2034520, // でさえ
  1005120, // すら
  1008490, // と
  1008530, // とか
  1008590, // として
  2028950, // とは
  2028960, // や
  1009600, // にとって
];

// Line 1036-1066: defmacro def-segfilter-must-follow
// This segfilter is for when segments that satisfy filter-right MUST follow segments that satisfy filter-left
function defSegfilterMustFollow(
  name: string,
  filterLeft: (segment: Segment) => boolean,
  filterRight: (segment: Segment) => boolean,
  options: { allowFirst?: boolean } = {}
): void {
  const { allowFirst = false } = options;

  const segfilterFn: SegfilterFunction = (segmentListLeft, segmentListRight) => {
    // Line 1042-1043: (multiple-value-bind (,satisfies-right ,contradicts-right) (classify ...))
    const [satisfiesRight, contradictsRight] = classify(filterRight, segmentListRight.segments);

    // Line 1044-1046: ((or (not ,satisfies-right) (and ,allow-first (not ,segment-list-left))) ...)
    if (satisfiesRight.length === 0 || (allowFirst && !segmentListLeft)) {
      return [[segmentListLeft, segmentListRight]];
    }

    // Line 1047-1051: ((or (not ,segment-list-left) (/= ...)) ...)
    if (!segmentListLeft || segmentListLeft.end !== segmentListRight.start) {
      if (contradictsRight.length > 0) {
        return [[segmentListLeft, makeSegmentListFrom(segmentListRight, contradictsRight)]];
      }
      return [];
    }

    // Line 1052-1066: (t (multiple-value-bind (,satisfies-left ,contradicts-left) ...))
    const [satisfiesLeft, contradictsLeft] = classify(filterLeft, segmentListLeft.segments);

    if (contradictsLeft.length > 0) {
      const result: Array<[SegmentList | null, SegmentList]> = [];

      // Line 1056-1059: (let ((,result (when ,contradicts-right ...)))
      if (contradictsRight.length > 0) {
        result.push([segmentListLeft, makeSegmentListFrom(segmentListRight, contradictsRight)]);
      }

      // Line 1060-1064: (when ,satisfies-left (push ...))
      if (satisfiesLeft.length > 0) {
        result.push([
          makeSegmentListFrom(segmentListLeft, satisfiesLeft),
          makeSegmentListFrom(segmentListRight, satisfiesRight)
        ]);
      }

      return result;
    } else {
      // Line 1066: (list (list ,segment-list-left ,segment-list-right))
      return [[segmentListLeft, segmentListRight]];
    }
  };

  defsegfilter(name, segfilterFn);
}

// ============================================================================
// SEGFILTER DEFINITIONS
// ============================================================================

// Line 1069-1072: defparameter *aux-verbs*
const AUX_VERBS = [
  1342560, // 初める/そめる
  // 2141080, // そうにない
];

// Line 1074-1076: def-segfilter-must-follow segfilter-aux-verb
defSegfilterMustFollow(
  'segfilter-aux-verb',
  filterIsConjugation(13),
  filterInSeqSet(...AUX_VERBS)
);

// Line 1078-1081: def-segfilter-must-follow segfilter-tsu-iru
defSegfilterMustFollow(
  'segfilter-tsu-iru',
  (segment: Segment) => !filterInSeqSet(2221640)(segment),
  filterInSeqSet(1577980),
  { allowFirst: true }
);

// Line 1083-1086: def-segfilter-must-follow segfilter-n
defSegfilterMustFollow(
  'segfilter-n',
  (segment: Segment) => !filterInSeqSetSimple(...NOUN_PARTICLES)(segment),
  filterInSeqSet(2139720, 2849370, 2849387), // ん んだ
  { allowFirst: true }
);

// Line 1088-1090: def-segfilter-must-follow segfilter-wokarasu
defSegfilterMustFollow(
  'segfilter-wokarasu',
  filterInSeqSet(2029010),
  filterInSeqSet(2087020)
);

// Line 1092-1094: def-segfilter-must-follow segfilter-badend
defSegfilterMustFollow(
  'segfilter-badend',
  (_: Segment) => false,
  filterIsCompoundEndText('ちゃい', 'いか', 'とか', 'とき', 'い')
);

// Line 1096-1102: def-segfilter-must-follow segfilter-sukiyoki
// Some of adj-ix words end with 好い which produces a confusing 好き conjugation; this should disable it
defSegfilterMustFollow(
  'segfilter-sukiyoki',
  (_: Segment) => false,
  (segment: Segment) => {
    return filterIsConjugation(CONJ_ADJECTIVE_LITERARY)(segment) &&
           getText(segment).endsWith('好き');
  }
);

// Line 1104-1107: Commented out segfilter-itsu
// (def-segfilter-must-follow segfilter-itsu (l r)
//   (complement (filter-is-compound-end-text "い"))
//   (filter-in-seq-set 2221640 1013250)
//   :allow-first t)

// Line 1109-1112: def-segfilter-must-follow segfilter-roku
defSegfilterMustFollow(
  'segfilter-roku',
  (segment: Segment) => !filterIsCompoundEndText('いろ')(segment),
  (segment: Segment) => getText(segment).startsWith('く'),
  { allowFirst: true }
);

// Line 1114-1117: def-segfilter-must-follow segfilter-sae
defSegfilterMustFollow(
  'segfilter-sae',
  (segment: Segment) => !filterIsCompoundEnd(2029120)(segment),
  (segment: Segment) => getText(segment).startsWith('え'),
  { allowFirst: true }
);

// Line 1119-1122: def-segfilter-must-follow segfilter-janai
defSegfilterMustFollow(
  'segfilter-janai',
  (segment: Segment) => !filterIsCompoundEnd(2028920)(segment),
  filterInSeqSet(1529520, 1296400, 2139720),
  { allowFirst: true }
);

// Line 1124-1127: def-segfilter-must-follow segfilter-nohayamete
defSegfilterMustFollow(
  'segfilter-nohayamete',
  (segment: Segment) => !filterInSeqSet(1469800)(segment),
  filterInSeqSet(1601080),
  { allowFirst: true }
);

// Line 1129-1133: def-segfilter-must-follow segfilter-toomou
// Split と before 思う 言う
defSegfilterMustFollow(
  'segfilter-toomou',
  (segment: Segment) => !filterInSeqSet(2837117)(segment), // 何だと
  filterInSeqSet(1589350, 1587040),
  { allowFirst: true }
);

// Line 1135-1138: def-segfilter-must-follow segfilter-totte
defSegfilterMustFollow(
  'segfilter-totte',
  (segment: Segment) => !filterInSeqSet(1008490)(segment),
  filterInSeqSet(2086960),
  { allowFirst: true }
);

// Line 1140-1145: def-segfilter-must-follow segfilter-dashi
defSegfilterMustFollow(
  'segfilter-dashi',
  (segment: Segment) => {
    const info = (segment as any).info || {};
    const seqSet: number[] = info.seqSet || [];
    // Line 1142-1143: (or (not (find 2089020 seq-set)) (find 2028980 seq-set))
    return !seqSet.includes(2089020) || seqSet.includes(2028980); // だ or で
  },
  filterInSeqSet(1157170, 2424740, 1305070), // する して
  { allowFirst: true }
);

// Line 1147-1151: def-segfilter-must-follow segfilter-dekiru
// 出 followed by 来る or 来てる
defSegfilterMustFollow(
  'segfilter-dekiru',
  (segment: Segment) => !filterInSeqSet(1896380, 2422860)(segment),
  filterInSeqSet(2830009, 1547720),
  { allowFirst: true }
);

// Line 1153-1156: defparameter *honorifics*
const HONORIFICS = [
  1247260, // 君
];

// Line 1157-1159: def-segfilter-must-follow segfilter-honorific
defSegfilterMustFollow(
  'segfilter-honorific',
  (segment: Segment) => !filterInSeqSetSimple(...NOUN_PARTICLES)(segment),
  filterInSeqSet(...HONORIFICS)
);

// ============================================================================
// PUBLIC API
// ============================================================================

// Line 1162-1168: defun apply-segfilters
export function applySegfilters(segLeft: SegmentList | null, segRight: SegmentList): Array<[SegmentList | null, SegmentList]> {
  // Line 1163: (loop with splits = (list (list seg-left seg-right)) ...)
  let splits: Array<[SegmentList | null, SegmentList]> = [[segLeft, segRight]];

  // Line 1164-1167: for segfilter in *segfilter-list* do (setf splits ...)
  for (const segfilter of segfilterList) {
    const newSplits: Array<[SegmentList | null, SegmentList]> = [];
    for (const [left, right] of splits) {
      newSplits.push(...segfilter(left, right));
    }
    splits = newSplits;
  }

  // Line 1168: finally (return splits)
  return splits;
}
