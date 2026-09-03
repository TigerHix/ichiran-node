import type { AnalyzeOptions } from '@ichiran/core';

export const MAX_ENTITY_SPEC_LENGTH = 2048;
export const MAX_ENTITY_HINTS = 64;

export function parseEntityHints(
  value: string,
  textLength: number
): NonNullable<AnalyzeOptions['entities']> {
  if (value.length > MAX_ENTITY_SPEC_LENGTH) {
    throw new Error(`Entity spans must contain at most ${MAX_ENTITY_SPEC_LENGTH} text units.`);
  }
  if (!value.trim()) return [];
  const parts = value.split(/[\s,]+/).filter(Boolean);
  if (parts.length > MAX_ENTITY_HINTS) {
    throw new Error(`Entity spans must contain at most ${MAX_ENTITY_HINTS} hints.`);
  }
  return parts.map(part => {
    const match = /^(\d+):(\d+)(?::(-?\d+))?$/.exec(part);
    if (!match) throw new Error(`Entity span “${part}” must be start:end or start:end:boost.`);
    const start = Number(match[1]);
    const end = Number(match[2]);
    const boost = match[3] === undefined ? undefined : Number(match[3]);
    if (start >= end || end > textLength) {
      throw new Error(`Entity span “${part}” is outside this ${textLength}-unit input.`);
    }
    return boost === undefined ? { start, end } : { start, end, boost };
  });
}
