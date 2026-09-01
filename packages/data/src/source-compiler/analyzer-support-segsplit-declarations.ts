import type { SplitDeclaration } from './analyzer-support-annotation-model.js';

const part = (
  seqs: number,
  lengthFn: (length: number, text: string) => number | null
): SplitDeclaration['parts'][number] => ({ type: 'part', seqs, lengthFn });

/** The complete 18-rule segment-split ledger from upstream dict-split.lisp. */
export const SEGMENT_SPLIT_DECLARATIONS: readonly SplitDeclaration[] = [
  { seq: 1_008_570, score: -10, parts: [part(1_343_100, l => l - 1), part(2_028_930, () => 1)] },
  { seq: 1_343_110, score: { score: -10, root: [1] }, parts: [part(1_343_100, l => l - 1), part(2_028_980, () => 1)] },
  { seq: 2_009_220, score: -10, parts: [part(1_343_100, l => l - 1), part(2_028_970, () => 1)] },
  { seq: 2_097_010, score: -10, parts: [part(1_343_100, l => l - 1), part(2_029_000, () => 1)] },
  { seq: 2_136_660, score: -10, parts: [part(1_343_100, l => l - 1), part(2_029_010, () => 1)] },
  {
    seq: 1_897_510,
    score: -10,
    parts: [part(1_343_100, l => l - 2), part(2_028_980, () => 1), part(2_028_920, () => 1)]
  },
  { seq: 2_409_240, score: { score: 20, primary: 1, connector: '' }, parts: [part(2_826_528, () => 1), part(1_582_120, () => null)] },
  { seq: 1_366_490, score: { score: -10, connector: '' }, parts: [part(1_580_640, l => l - 3), part(1_013_240, () => 3)] },
  { seq: 2_028_950, score: -5, parts: [part(1_008_490, () => 1), part(2_028_920, () => 1)] },
  { seq: 1_008_450, score: -5, parts: [part(2_028_980, () => 1), part(2_028_920, () => 1)] },
  { seq: 2_394_710, score: -5, parts: [part(1_529_520, () => 2), part(1_008_490, () => null)] },
  { seq: 1_011_740, score: -5, parts: [part(1_522_150, l => l - 1), part(1_469_800, () => 1)] },
  {
    seq: 1_208_870,
    score: 5,
    parts: [
      { type: 'guard', condition: (_length, text) => text === 'かなって' },
      part(1_002_940, () => 2),
      part(2_086_960, () => null)
    ]
  },
  { seq: 1_007_310, score: -5, parts: [part(2_089_020, () => 1), part(1_002_980, () => null)] },
  { seq: 1_675_330, score: { score: 10, primary: 1 }, parts: [part(1_002_980, () => 2), part(1_260_720, () => null)] },
  { seq: 2_841_254, score: 5, parts: [part(1_002_980, () => 2), part(2_086_960, () => null)] },
  {
    seq: 1_567_610,
    score: 5,
    parts: [
      { type: 'guard', condition: (_length, text) => text === 'もんだ' },
      part(1_502_390, () => 2),
      part(2_089_020, () => null)
    ]
  },
  {
    seq: 1_010_105,
    score: 5,
    parts: [
      { type: 'guard', condition: (_length, text) => text === 'はぐったり' },
      part(2_028_920, () => 1),
      part(1_004_070, () => null)
    ]
  }
];
