/**
 * Historical addConjReading root substitution. The declaration replaces the
 * non-shared prefix of each existing target's ordinal-zero form while keeping
 * its conjugated suffix unchanged.
 */
export function replayConjugationReading(
  baseText: string,
  reading: string,
  targetText: string
): string {
  let base = baseText.length - 1;
  let added = reading.length - 1;
  while (base >= 0 && added >= 0 && baseText[base] === reading[added]) {
    base--;
    added--;
  }
  if (base < 0 || added < 0) {
    const baseCut = reading.length >= baseText.length ? 0 : baseText.length - reading.length;
    const readingCut = reading.length >= baseText.length ? reading.length - baseText.length : 0;
    return reading.slice(0, readingCut) + targetText.slice(baseCut);
  }
  return reading.slice(0, added + 1) + targetText.slice(base + 1);
}
