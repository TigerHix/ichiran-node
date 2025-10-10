/**
 * Conservative minimum token counts for built-in macros.
 * Used for optimization to skip matching when insufficient tokens remain.
 */
export const MACRO_MIN_TOKENS: Record<string, number> = {
  NP: 1,
  NPCase: 2,
  NPTopic: 2,
  ClauseAdjunct: 1,
  PrePredicateAdjuncts: 0,
  AttributiveClause: 1,
  IAdjKu: 1,
  IAdjKute: 1,
  IAdjKunai: 1,
  IAdjKunakute: 2,
  IAdjPoliteNegative: 1,
  IAdjKuSuru: 2,
  IAdjKuNaru: 1,
  NaAdjDe: 2,
  NaAdjDeshite: 2,
  NaAdjNegativeConnective: 2,
  NaAdjNi: 2,
  NaAdjNiNaru: 2,
  NaAdjNiSuru: 2,
  NPDeWaTopic: 2,
  CopulaNegative: 1,
  DeWaFrame: 2,
};

