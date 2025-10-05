// Grammar synergies module
// Extracted from dict-grammar.ts (Lines 1466-1894)

import type { Segment, SegmentList, ConjData } from '../types.js';
import { isCompoundText, isCounterText, isSimpleWord } from '../types.js';
import type { Synergy, SynergyFunction } from './types.js';
import { getText } from '../dict/utils.js';

// Line 720: defparameter *synergy-list*
const synergyList: SynergyFunction[] = [];

// ============================================================================
// HELPER FUNCTIONS
// ============================================================================

// Line 715-718: defun make-segment-list-from
function makeSegmentListFrom(oldSegmentList: SegmentList, segments: Segment[]): SegmentList {
  return {
    ...oldSegmentList,
    segments
  };
}

// Line 722-726: defmacro defsynergy
function defsynergy(_name: string, fn: SynergyFunction): void {
  synergyList.push(fn);
}

// Line 728-743: defmacro def-generic-synergy
function defGenericSynergy(
  name: string,
  filterLeft: (segment: Segment) => boolean,
  filterRight: (segment: Segment) => boolean,
  options: {
    description?: string;
    connector: string;
    score: number | ((l: SegmentList, r: SegmentList) => number);
  }
): void {
  const synergyFn: SynergyFunction = async (segmentListLeft: SegmentList, segmentListRight: SegmentList) => {
    const start = segmentListLeft.end;
    const end = segmentListRight.start;

    // Line 734: Only combine adjacent segments
    if (start !== end) return null;

    // Line 735-736: Filter segments
    const left = segmentListLeft.segments.filter(filterLeft);
    const right = segmentListRight.segments.filter(filterRight);

    // Line 737-743: Create synergy if both sides match
    if (left.length > 0 && right.length > 0) {
      const score = typeof options.score === 'function'
        ? options.score(segmentListLeft, segmentListRight)
        : options.score;

      return [[
        makeSegmentListFrom(segmentListRight, right),
        {
          start,
          end,
          description: options.description || '',
          connector: options.connector,
          score
        },
        makeSegmentListFrom(segmentListLeft, left)
      ]];
    }

    return null;
  };

  defsynergy(name, synergyFn);
}

// ============================================================================
// FILTER FUNCTIONS (Lines 745-797)
// ============================================================================

// Line 745-752: defun filter-is-noun
function filterIsNoun(segment: Segment): boolean {
  const info = segment.info;
  if (!info) return false;

  const kpcl = info.kpcl || [null, null, null, null];
  const [k, p, c, l] = kpcl;
  const posi = info.posi || [];

  // Check KPCL and POS
  if ((l || k || (p && c)) &&
      ['n', 'n-adv', 'n-t', 'adj-na', 'n-suf', 'pn'].some(pos => posi.includes(pos))) {
    return true;
  }

  // Check if counter-text with seq-set
  // Counters have 'valueString' method (from CounterText class)
  // Note: in Lisp, empty seq-set is nil (falsy), so we check length > 0
  if (segment.word && isCounterText(segment.word) && info.seqSet && info.seqSet.length > 0) {
    return true;
  }

  return false;
}

// Line 754-761: defmacro filter-is-pos
function filterIsPos(posList: string[], kpclTest: (k: any, p: any, c: any, l: any) => boolean): (segment: Segment) => boolean {
  return (segment: Segment) => {
    const info = segment.info;
    if (!info) return false;

    const kpcl = info.kpcl || [null, null, null, null];
    const [k, p, c, l] = kpcl;
    const posi = info.posi || [];

    if (!kpclTest(k, p, c, l)) return false;
    return posList.some(pos => posi.includes(pos));
  };
}

// Line 763-766: defun filter-in-seq-set
function filterInSeqSet(...seqs: number[]): (segment: Segment) => boolean {
  return (segment: Segment) => {
    const info = segment.info;
    if (!info) return false;

    const seqSet = info.seqSet || [];
    return seqs.some(seq => seqSet.includes(seq));
  };
}

// Line 768-774: defun filter-in-seq-set-simple
export function filterInSeqSetSimple(...seqs: number[]): (segment: Segment) => boolean {
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

// Line 776-780: defun filter-is-conjugation
export function filterIsConjugation(conjType: number): (segment: Segment) => boolean {
  return (segment: Segment) => {
    const info = segment.info;
    if (!info) return false;

    const conj = info.conj || [];
    return conj.some((cdata: ConjData) => cdata.prop.conjType === conjType);
  };
}

// Line 782-788: defun filter-is-compound-end
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

// Line 790-796: defun filter-is-compound-end-text
export function filterIsCompoundEndText(...texts: string[]): (segment: Segment) => boolean {
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

// Line 798-822: defparameter *noun-particles*
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

// ============================================================================
// SYNERGY DEFINITIONS (Lines 824-952)
// ============================================================================

// Line 824-829: def-generic-synergy synergy-noun-particle
defGenericSynergy('synergy-noun-particle',
  filterIsNoun,
  filterInSeqSet(...NOUN_PARTICLES),
  {
    description: 'noun+prt',
    score: (_l, r) => 10 + 4 * (r.end - r.start),
    connector: ' '
  }
);

// Line 831-836: synergy-suru-verb (commented out in Lisp)
// defGenericSynergy('synergy-suru-verb',
//   filterIsPos(['vs'], (k, p, c, l) => k || l || (p && c)),
//   filterInSeqSet(1157170), // する
//   {
//     description: 'noun+suru',
//     score: 10,
//     connector: ''
//   }
// );

// Line 838-843: def-generic-synergy synergy-noun-da
defGenericSynergy('synergy-noun-da',
  filterIsNoun,
  filterInSeqSet(2089020), // だ
  {
    description: 'noun+da',
    score: 10,
    connector: ' '
  }
);

// Line 845-850: def-generic-synergy synergy-no-da
defGenericSynergy('synergy-no-da',
  filterInSeqSet(1469800, 2139720), // の, ん
  filterInSeqSet(2089020, 1007370, 1928670), // だ, です, だろう
  {
    description: 'no da/desu',
    score: 15,
    connector: ' '
  }
);

// Line 852-858: def-generic-synergy synergy-sou-nanda (TODO: remove hack)
defGenericSynergy('synergy-sou-nanda',
  filterInSeqSet(2137720),
  filterInSeqSet(2140410),
  {
    description: 'sou na n da',
    score: 50,
    connector: ' '
  }
);

// Line 860-865: def-generic-synergy synergy-no-adjectives
defGenericSynergy('synergy-no-adjectives',
  filterIsPos(['adj-no'], (k, p, c, l) => k || l || (p && c)),
  filterInSeqSet(1469800), // の
  {
    description: 'no-adjective',
    score: 15,
    connector: ' '
  }
);

// Line 867-872: def-generic-synergy synergy-na-adjectives
defGenericSynergy('synergy-na-adjectives',
  filterIsPos(['adj-na'], (k, p, c, l) => k || l || (p && c)),
  filterInSeqSet(2029110, 2028990), // な, に
  {
    description: 'na-adjective',
    score: 15,
    connector: ' '
  }
);

// Line 874-879: def-generic-synergy synergy-to-adverbs
defGenericSynergy('synergy-to-adverbs',
  filterIsPos(['adv-to'], (k, p, _c, l) => k || l || p),
  filterInSeqSet(1008490), // と
  {
    description: 'to-adverb',
    score: (l, _r) => 10 + 10 * (l.end - l.start),
    connector: ' '
  }
);

// Line 881-886: def-generic-synergy synergy-suffix-chu
defGenericSynergy('synergy-suffix-chu',
  filterIsNoun,
  filterInSeqSet(1620400, 2083570), // 中
  {
    description: 'suffix-chu',
    score: 12,
    connector: '-'
  }
);

// Line 888-893: def-generic-synergy synergy-suffix-tachi
defGenericSynergy('synergy-suffix-tachi',
  filterIsNoun,
  filterInSeqSet(1416220), // 達
  {
    description: 'suffix-tachi',
    score: 10,
    connector: '-'
  }
);

// Line 895-900: def-generic-synergy synergy-suffix-buri
defGenericSynergy('synergy-suffix-buri',
  filterIsNoun,
  filterInSeqSet(1361140), // 振り
  {
    description: 'suffix-buri',
    score: 40,
    connector: ''
  }
);

// Line 902-908: def-generic-synergy synergy-suffix-sei
defGenericSynergy('synergy-suffix-sei',
  filterIsNoun,
  filterInSeqSet(1375260), // 性
  {
    description: 'suffix-sei',
    score: 12,
    connector: ''
  }
);

// Line 910-915: def-generic-synergy synergy-o-prefix
defGenericSynergy('synergy-o-prefix',
  filterInSeqSet(1270190), // お
  filterIsPos(['n'], (k, _p, _c, l) => k || l),
  {
    description: 'o+noun',
    score: 10,
    connector: ''
  }
);

// Line 917-922: def-generic-synergy synergy-kanji-prefix
defGenericSynergy('synergy-kanji-prefix',
  filterInSeqSet(2242840, 1922780, 2423740), // 未, 不
  filterIsPos(['n'], (k, _p, _c, _l) => k),
  {
    description: 'kanji prefix+noun',
    score: 15,
    connector: ''
  }
);

// Line 924-929: def-generic-synergy synergy-shicha-ikenai
defGenericSynergy('synergy-shicha-ikenai',
  filterIsCompoundEnd(2028920), // は
  filterInSeqSet(1000730, 1612750, 1409110, 2829697, 1587610), // いけない, いけません, だめ, いかん, いや
  {
    description: 'shicha ikenai',
    score: 50,
    connector: ' '
  }
);

// Line 931-939: def-generic-synergy synergy-shika-negative
defGenericSynergy('synergy-shika-negative',
  filterInSeqSet(1005460), // しか
  (segment: Segment) => {
    const info = segment.info;
    if (!info) return false;

    const conj = info.conj || [];
    return conj.some((cdata: ConjData) => cdata.prop.neg !== false);
  },
  {
    description: 'shika+neg',
    score: 50,
    connector: ' '
  }
);

// Line 941-946: def-generic-synergy synergy-no-toori
defGenericSynergy('synergy-no-toori',
  filterInSeqSet(1469800), // の
  filterInSeqSet(1432920), // 通り
  {
    description: 'no toori',
    score: 50,
    connector: ' '
  }
);

// Line 948-952: def-generic-synergy synergy-oki
defGenericSynergy('synergy-oki',
  filterIsPos(['ctr'], (_k, _p, _c, _l) => true),
  filterInSeqSet(2854117, 2084550), // 置き
  {
    score: 20,
    connector: ''
  } as any // description is optional
);

// ============================================================================
// PUBLIC API
// ============================================================================

// Line 954-956: defun get-synergies
export async function getSynergies(segmentListLeft: SegmentList, segmentListRight: SegmentList): Promise<Array<[SegmentList, Synergy, SegmentList]>> {
  const results: Array<[SegmentList, Synergy, SegmentList]> = [];

  for (const fn of synergyList) {
    const result = await fn(segmentListLeft, segmentListRight);
    if (result) {
      results.push(...result);
    }
  }

  return results;
}
