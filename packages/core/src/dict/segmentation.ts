// ichiran/dict/segmentation - Segmentation and path-finding algorithms
// Extracted from dict.ts as part of Phase 1b refactoring
// Contains main segmentation logic: building candidates, finding best paths, filling segments

import { consecutiveCharGroups, sequentialKanjiPositions } from '../characters.js';
import { getSuffixMap } from '../grammar/suffixMatcher.js';
import { applySegfilters } from '../grammar/segfilters.js';
import { getSynergies } from '../grammar/synergies.js';
import { getPenalties } from '../grammar/penalties.js';
import type { Synergy } from '../grammar/types.js';
import type {
  Segment,
  SegmentList,
  AnyWord
} from '../types.js';
import { TopArray, isCompoundText, isProxyText, isSegmentList, isCounterText, isSimpleWord, isSimpleText } from '../types.js';

// Import from other dict modules
import {
  findStickyPositions,
  genScore,
  gapPenalty,
  cullSegments,
  getSegmentScore,
  SCORE_CUTOFF,
  SEGMENT_SCORE_CUTOFF
} from './scoring.js';

import {
  findSubstringWords,
  withSubstringHash,
  findWordFull,
  MAX_WORD_LENGTH
} from './lookup.js';

import { withSuffixContext } from './suffixContext.js';

import { getText, getWordType, getSeq, trueText } from './utils.js';
import { getKana } from './readings.js';

import { startTimer } from './profiling.js';
import { WordInfo, processWordInfo } from './wordInfo.js';

import { getCalcScoreCache } from './cache.js';
import { getSegsplit } from './splitDefinitions.js';
import { FORCE_KANJI_BREAK } from './errata.js';

// Line 1071-1109: defun join-substring-words*
async function joinSubstringWords(str: string): Promise<[Array<[number, number, Segment[]]>, number[]]> {
  const endTimer = startTimer('joinSubstringWords');

  const sticky = findStickyPositions(str);
  const substringHash = await findSubstringWords(str, sticky);

  // Set substring hash context for find-word to use
  return await withSubstringHash(substringHash, async () => {
    const katakanaGroups = consecutiveCharGroups('katakana', str);
    const numberGroups = consecutiveCharGroups('number', str);
    const endGetSuffixMap = startTimer('joinSubstringWords_getSuffixMap');
    const suffixMap = await getSuffixMap(str);
    endGetSuffixMap();

    let kanjiBreak: number[] = [];
    const ends: number[] = [];
    const result: Array<[number, number, Segment[]]> = [];

  for (let start = 0; start < str.length; start++) {
    if (sticky.includes(start)) continue;

    const katakanaGroupEnd = katakanaGroups.find(g => g[0] === start)?.[1];
    const numberGroupEnd = numberGroups.find(g => g[0] === start)?.[1];

    for (let end = start + 1; end <= Math.min(str.length, start + MAX_WORD_LENGTH); end++) {
      if (sticky.includes(end)) continue;

      const part = str.slice(start, end);

      // Set up suffix context for findWordFull
      // Uses AsyncLocalStorage to pass suffix map (matches Lisp's dynamic variables)
      const endFindWord = startTimer('joinSubstringWords_findWord');
      const words = await withSuffixContext(
        { suffixMap, suffixNextEnd: end },
        () => findWordFull(part, {
          asHiragana: katakanaGroupEnd === end,
          counter: numberGroupEnd && numberGroupEnd <= end ?
            Math.min(numberGroupEnd - start, 20) : undefined
        })
      );
      endFindWord();

      if (words.length > 0) {
        const segments = words.map(word => ({
          start,
          end,
          word
        }));

        if (start === 0 || ends.includes(start)) {
          if (FORCE_KANJI_BREAK.includes(part)) {
            for (let i = start + 1; i < end; i++) {
              kanjiBreak.push(i);
            }
          } else {
            const positions = sequentialKanjiPositions(part, start);
            kanjiBreak.push(...positions);
          }
        }

        if (!ends.includes(end)) {
          ends.push(end);
        }

        result.push([start, end, segments]);
      }
    }
  }

    endTimer();
    return [result, [...new Set(kanjiBreak)]];
  }); // Close withSubstringHash
}

function collectSeqsForPrefetch(
  word: AnyWord | null | undefined,
  target: Set<number>,
  seen: WeakSet<object> = new WeakSet()
): void {
  if (!word || typeof word !== 'object') return;
  const obj = word as object;
  if (seen.has(obj)) return;
  seen.add(obj);

  const addSeqValue = (value: unknown) => {
    if (value === null || value === undefined) return;
    if (Array.isArray(value)) {
      for (const item of value) {
        if (typeof item === 'number' && Number.isFinite(item)) {
          target.add(item);
        }
      }
    } else if (typeof value === 'number' && Number.isFinite(value)) {
      target.add(value);
    }
  };

  // CounterText has getSeq() method
  if (isCounterText(word) && typeof (word as any).getSeq === 'function') {
    addSeqValue((word as any).getSeq());
  }
  // SimpleWord (KanjiText/KanaText) has seq property
  else if (isSimpleWord(word)) {
    addSeqValue(word.seq);
  }
  // CompoundText has seq array
  else if (isCompoundText(word)) {
    addSeqValue(word.seq);
  }

  if (isCompoundText(word) && word.primary) {
    collectSeqsForPrefetch(word.primary, target, seen);
  }

  if (isCompoundText(word)) {
    for (const child of word.words) {
      collectSeqsForPrefetch(child, target, seen);
    }
  }

  if (isProxyText(word) && word.source) {
    collectSeqsForPrefetch(word.source, target, seen);
  }
}

// Line 1111-1127: defun join-substring-words
export async function joinSubstringWords2(str: string): Promise<SegmentList[]> {
  const [result, kanjiBreak] = await joinSubstringWords(str);
  const seqsForPrefetch = new Set<number>();
  for (const [, , segments] of result) {
    for (const segment of segments) {
      collectSeqsForPrefetch(segment.word as AnyWord, seqsForPrefetch);
    }
  }

  if (seqsForPrefetch.size > 0) {
    const cache = getCalcScoreCache();
    await cache.prefetchSeqs([...seqsForPrefetch]);
  }

  const endsWithLw = str.endsWith('ー');
  const segmentLists: SegmentList[] = [];

  for (const [start, end, segments] of result) {
    const kb = [start, end].filter(n => kanjiBreak.includes(n)).map(n => n - start);

    const scoredSegments: Segment[] = [];
    for (const segment of segments) {
      await genScore(segment, {
        final: segment.end === str.length || (endsWithLw && segment.end === str.length - 1),
        kanjiBreak: kb
      });


      if ((segment.score ?? 0) >= SCORE_CUTOFF) {
        scoredSegments.push(segment);
      }
    }

    if (scoredSegments.length > 0) {
      segmentLists.push({
        segments: cullSegments(scoredSegments),
        start,
        end,
        matches: segments.length
      });
    }
  }

  return segmentLists;
}

// Line 1169-1171: defun get-seg-initial
function getSegInitial(seg: Segment | SegmentList): any[] {
  const splits = applySegfilters(null, seg as SegmentList);
  return splits.map((split: any) => split[1]);
}

// Line 1173-1176: defun get-seg-splits
async function getSegSplits(segLeft: Segment | SegmentList, segRight: Segment | SegmentList): Promise<Array<[SegmentList, Synergy, SegmentList] | [SegmentList, SegmentList]>> {
  const splits = applySegfilters(segLeft as SegmentList | null, segRight as SegmentList);
  const results: Array<[SegmentList, Synergy, SegmentList] | [SegmentList, SegmentList]> = [];

  for (const split of splits) {
    const [left, right] = split;
    if (left === null) continue; // Skip splits with null left segment
    const penalties = await getPenalties(left, right);
    const synergies = await getSynergies(left, right);

    results.push(penalties, ...synergies);
  }

  return results;
}

// Line 1178-1186: defun expand-segment-list
async function expandSegmentList(segmentList: SegmentList): Promise<void> {
  const expanded: Segment[] = [...segmentList.segments];

  for (const segment of segmentList.segments) {
    const segsplit = await getSegsplit(segment);
    if (segsplit) {
      expanded.push(segsplit);
      segmentList.matches++;
    }
  }

  // Sort by score descending
  expanded.sort((a, b) => (b.score ?? 0) - (a.score ?? 0));
  segmentList.segments = expanded;
}

// Line 1188-1231: defun find-best-path
export async function findBestPath(
  segmentLists: SegmentList[],
  strLength: number,
  options: { limit?: number } = {}
): Promise<Array<[any[], number]>> {
  const endTimer = startTimer('findBestPath');
  const limit = options.limit ?? 5;
  const top = new TopArray(limit);

  top.registerItem(gapPenalty(0, strLength), []);

  // Expand all segment lists and initialize their tops
  for (const segmentList of segmentLists) {
    await expandSegmentList(segmentList);
    segmentList.top = new TopArray(limit);
  }

  // Process segment lists
  for (let i = 0; i < segmentLists.length; i++) {
    const seg1 = segmentLists[i];
    const gapLeft = gapPenalty(0, seg1.start);
    const gapRight = gapPenalty(seg1.end, strLength);

    // Initial segments
    const initialSegs = getSegInitial(seg1);
    for (const seg of initialSegs) {
      const score1 = getSegmentScore(seg);
      seg1.top!.registerItem(gapLeft + score1, [seg]);
      top.registerItem(gapLeft + score1 + gapRight, [seg]);
    }

    // Connect to following segments
    for (let j = i + 1; j < segmentLists.length; j++) {
      const seg2 = segmentLists[j];

      if (seg2.start >= seg1.end) {
        const score2 = getSegmentScore(seg2);
        const gapLeft2 = gapPenalty(seg1.end, seg2.start);
        const gapRight2 = gapPenalty(seg2.end, strLength);

        for (const tai of seg1.top!.getArray()) {
          const payload = tai.payload;
          const segLeft = payload[0];
          const score3 = getSegmentScore(segLeft);
          const scoreTail = tai.score - score3;

          const splits = await getSegSplits(segLeft, seg2);
          for (const split of splits) {
            const splitArray = Array.isArray(split) ? split : [split];
            // Line 1241: (reduce #'+ split :key #'get-segment-score)
            const splitScore = splitArray.reduce((sum, s) => sum + getSegmentScore(s), 0);
            const accum = gapLeft2 + Math.max(splitScore, score3 + 1, score2 + 1) + scoreTail;
            const path = [...splitArray, ...payload.slice(1)] as any as Segment[];

            seg2.top!.registerItem(accum, path);
            top.registerItem(accum + gapRight2, path);
          }
        }
      }
    }
  }

  // Clear segment-list tops
  for (const segment of segmentLists) {
    segment.top = null;
  }

  // Return results
  endTimer();
  return top.getArray().map(tai => [tai.payload.reverse(), tai.score]);
}

// Line 1388-1405: defun fill-segment-path
export async function fillSegmentPath(str: string, path: (SegmentList | any)[]): Promise<WordInfo[]> {
  const endTimer = startTimer('fillSegmentPath');

  const result: WordInfo[] = [];
  let idx = 0;

  for (const segmentList of path) {
    if (isSegmentList(segmentList)) {
      // It's a SegmentList
      const sl = segmentList;

      if (sl.start > idx) {
        // Add gap
        const substr = str.slice(idx, sl.start);
        result.push(new WordInfo({
          type: 'gap',
          text: substr,
          kana: substr,
          start: idx,
          end: sl.start
        }));
      }

      result.push(await wordInfoFromSegmentList(sl));
      idx = sl.end;
    }
  }

  // Final gap if needed
  if (idx < str.length) {
    const substr = str.slice(idx);
    result.push(new WordInfo({
      type: 'gap',
      text: substr,
      kana: substr,
      start: idx,
      end: str.length
    }));
  }

  endTimer();
  // Line 1405: Lisp uses (nreverse result) before processing
  // In Lisp, push adds to front, so result is built in reverse order, then nreverse fixes it.
  // In TypeScript, push adds to back, so result is already in correct order - no reversal needed.
  return processWordInfo(result);
}

// Line 1325-1347: defun word-info-from-segment
export async function wordInfoFromSegment(segment: Segment): Promise<WordInfo> {
  const word = segment.word;
  const wordType = getWordType(word);

  const data: Partial<WordInfo> = {
    type: wordType,
    text: getText(segment),
    kana: await getKana(word),
    seq: getSeq(word),
    conjugations: isSimpleText(word) ? word.conjugations : undefined,
    trueText: trueText(word),
    score: segment.score ?? 0,
    start: segment.start,
    end: segment.end
  };

  // Handle compound words
  if (isCompoundText(word)) {
    const primarySeq = getSeq(word.primary);
    data.components = await Promise.all(word.words.map(async (wrd: any) => new WordInfo({
      type: getWordType(wrd),
      text: getText(wrd),
      kana: await getKana(wrd),
      seq: getSeq(wrd),
      conjugations: wrd.conjugations,
      trueText: trueText(wrd),
      primary: getSeq(wrd) === primarySeq
    })));
  }

  // Handle counter words
  if (isCounterText(word) && 'ordinalp' in word) {
    data.counter = [word.valueString(), word.ordinalp];
  }

  return new WordInfo(data as any);
}

// Line 1351-1378: defun word-info-from-segment-list
export async function wordInfoFromSegmentList(segmentList: SegmentList): Promise<WordInfo> {
  const segments = segmentList.segments;
  const wiList = await Promise.all(segments.map(wordInfoFromSegment));

  if (wiList.length === 0) {
    return new WordInfo({
      type: 'gap',
      text: '',
      kana: ''
    });
  }

  const wi1 = wiList[0];
  const maxScore = wi1.score;

  // Filter by score cutoff
  const filteredWiList = wiList.filter(wi =>
    wi.score >= maxScore * SEGMENT_SCORE_CUTOFF
  );

  const matches = segmentList.matches;

  if (filteredWiList.length === 1) {
    wi1.skipped = matches - 1;
    return wi1;
  }

  // Multiple alternatives
  const kanaList = filteredWiList.map(wi => wi.kana);
  const seqList = filteredWiList.map(wi => wi.seq).filter((s): s is number | number[] => s !== null && s !== undefined);

  return new WordInfo({
    type: wi1.type,
    text: wi1.text,
    kana: [...new Set(kanaList.flat())],
    seq: seqList.length > 0 ? seqList.flat() : null,
    components: filteredWiList,
    alternative: true,
    score: wi1.score,
    start: segmentList.start,
    end: segmentList.end,
    skipped: matches - filteredWiList.length
  });
}
