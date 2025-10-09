import type { Capture, MatchSegment, Token } from './types.js';

export function buildSegmentsFromTokens(
  unfilteredTokens: Token[],
  captures: Capture[]
): MatchSegment[] {
  if (captures.length === 0) {
    return [];
  }

  const sortedCaptures = [...captures].sort((a, b) => a.start - b.start);
  const segments: MatchSegment[] = [];
  let currentPos = 0;

  for (const capture of sortedCaptures) {
    const captureStart = capture.start;
    const captureEnd = capture.end;
    if (captureStart < 0 || captureEnd > unfilteredTokens.length) {
      continue;
    }

    if (currentPos < captureStart) {
      const rawText = unfilteredTokens.slice(currentPos, captureStart).map(t => t.text).join('');
      if (rawText) {
        segments.push({ type: 'raw', text: rawText });
      }
    }

    segments.push({ type: 'capture', text: capture.text, label: capture.label });
    currentPos = captureEnd;
  }

  if (currentPos < unfilteredTokens.length) {
    const rawText = unfilteredTokens.slice(currentPos).map(t => t.text).join('');
    if (rawText) {
      segments.push({ type: 'raw', text: rawText });
    }
  }

  return segments;
}


