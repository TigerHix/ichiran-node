import type { GinzaSentence } from '../ginza/types.js';
import type { Clause, CompiledRule, RuleSpec, Trigger, CaptureValue, CaptureSpec, TokPred } from './dsl.js';
import { tokenMatchesPreds } from './dsl.js';

type Binding = Map<string, number>; // var -> token index

type EdgeClause = Extract<Clause, { kind: 'edge' }>;
type BeforeClause = Extract<Clause, { kind: 'before' }>;
type NextClause = Extract<Clause, { kind: 'next' }>;

/** Precomputed clause lookups to avoid repeated filtering at match time */
type CompiledSpec = {
  spec: RuleSpec;
  orderedVars: string[];
  sortedCaptureNames: string[];
  varToPredsFlat: Map<string, TokPred[]>;
  varToEdgesAsChild: Map<string, EdgeClause[]>;
  varToEdgesAsHead: Map<string, EdgeClause[]>;
  varToBeforeAsA: Map<string, BeforeClause[]>;
  varToBeforeAsB: Map<string, BeforeClause[]>;
  varToNextAsA: Map<string, NextClause[]>;
  varToNextAsB: Map<string, NextClause[]>;
};

function extractTriggersFromClauses(clauses: Clause[], out: Trigger[]): void {
  for (const c of clauses) {
    if (c.kind === 'node') {
      for (const p of c.preds) {
        if (p.kind === 'lemma') out.push({ kind: 'lemma', value: p.value });
        if (p.kind === 'text') out.push({ kind: 'text', value: p.value });
        if (p.kind === 'lemmaOneOf') {
          for (const v of p.value) out.push({ kind: 'lemma', value: v });
        }
        if (p.kind === 'textOneOf') {
          for (const v of p.value) out.push({ kind: 'text', value: v });
        }
      }
    } else if (c.kind === 'either') {
      // Extract triggers from all branches
      for (const branch of c.branches) {
        extractTriggersFromClauses(branch.clauses, out);
      }
    }
  }
}

export function deriveTriggers(spec: RuleSpec): Trigger[] {
  const out: Trigger[] = [];
  extractTriggersFromClauses(spec.where, out);
  // Dedup
  const seen = new Set<string>();
  const deduped: Trigger[] = [];
  for (const t of out) {
    const k = `${t.kind}:${t.value}`;
    if (seen.has(k)) continue;
    seen.add(k);
    deduped.push(t);
  }
  // Prefer lemma triggers first.
  deduped.sort((a, b) => (a.kind === b.kind ? 0 : a.kind === 'lemma' ? -1 : 1));
  return deduped;
}

export type SentenceIndex = {
  all: number[]; // [0..n)
  byLemma: Map<string, number[]>;
  byText: Map<string, number[]>;
  byPos: Map<string, number[]>;
  byDep: Map<string, number[]>;
  // head index -> children indices
  childrenOf: Map<number, number[]>;
  // head index -> dep -> children indices
  childrenOfByDep: Map<number, Map<string, number[]>>;
};

function pushMapArr<K>(m: Map<K, number[]>, k: K, v: number): void {
  const arr = m.get(k);
  if (arr) arr.push(v);
  else m.set(k, [v]);
}

export function buildSentenceIndex(sent: GinzaSentence): SentenceIndex {
  const byLemma = new Map<string, number[]>();
  const byText = new Map<string, number[]>();
  const byPos = new Map<string, number[]>();
  const byDep = new Map<string, number[]>();
  const childrenOf = new Map<number, number[]>();
  const childrenOfByDep = new Map<number, Map<string, number[]>>();
  const all: number[] = [];

  for (let i = 0; i < sent.tokens.length; i++) {
    const t = sent.tokens[i]!;
    all.push(i);
    pushMapArr(byLemma, t.lemma, i);
    pushMapArr(byText, t.text, i);
    pushMapArr(byPos, t.pos, i);
    pushMapArr(byDep, t.dep, i);

    if (t.head >= 0) {
      pushMapArr(childrenOf, t.head, i);
      let depMap = childrenOfByDep.get(t.head);
      if (!depMap) {
        depMap = new Map<string, number[]>();
        childrenOfByDep.set(t.head, depMap);
      }
      pushMapArr(depMap, t.dep, i);
    }
  }

  return { all, byLemma, byText, byPos, byDep, childrenOf, childrenOfByDep };
}

function intersect(a: number[], b: number[]): number[] {
  if (a.length === 0 || b.length === 0) return [];
  // intersect by marking the smaller side
  const [small, big] = a.length <= b.length ? [a, b] : [b, a];
  const s = new Set(small);
  const out: number[] = [];
  for (const x of big) if (s.has(x)) out.push(x);
  return out;
}

function windowCandidates(n: number, startExclusive: number, endInclusive: number): number[] {
  const out: number[] = [];
  const lo = Math.max(0, startExclusive + 1);
  const hi = Math.min(n - 1, endInclusive);
  for (let i = lo; i <= hi; i++) out.push(i);
  return out;
}

/** Get variables referenced by a single clause */
function getClauseVars(c: Clause): string[] {
  if (c.kind === 'node') return [c.node.v];
  if (c.kind === 'edge') return [c.child.v, c.head.v];
  if (c.kind === 'next' || c.kind === 'before') return [c.a.v, c.b.v];
  if (c.kind === 'not') return getClauseVars(c.clause);
  if (c.kind === 'optional') return c.clauses.flatMap(getClauseVars);
  if (c.kind === 'either') return c.branches.flatMap((b) => b.clauses.flatMap(getClauseVars));
  return [];
}

/**
 * Check if a clause holds given current bindings.
 *
 * Constraint checking returns true when variables are unbound.
 * This implements "optimistic" partial matching during DFS:
 * constraints are only enforced once all referenced vars are bound.
 */
function clauseHolds(clause: Clause, sent: GinzaSentence, bind: Binding): boolean {
  if (clause.kind === 'node') {
    const idx = bind.get(clause.node.v);
    if (idx === undefined) return true; // not yet bound - defer check
    const tok = sent.tokens[idx];
    if (!tok) return false;
    return tokenMatchesPreds(tok, clause.preds);
  }
  if (clause.kind === 'edge') {
    const cIdx = bind.get(clause.child.v);
    const hIdx = bind.get(clause.head.v);
    if (cIdx === undefined || hIdx === undefined) return true;
    const tok = sent.tokens[cIdx];
    if (!tok) return false;
    if (tok.head !== hIdx) return false;
    if (clause.dep && tok.dep !== clause.dep) return false;
    return true;
  }
  if (clause.kind === 'next') {
    const a = bind.get(clause.a.v);
    const b = bind.get(clause.b.v);
    if (a === undefined || b === undefined) return true;
    return b === a + 1;
  }
  if (clause.kind === 'before') {
    const a = bind.get(clause.a.v);
    const b = bind.get(clause.b.v);
    if (a === undefined || b === undefined) return true;
    if (a >= b) return false;
    if (clause.maxDistance !== undefined && b - a > clause.maxDistance) return false;
    return true;
  }
  if (clause.kind === 'not') {
    // Check if inner clause references unbound vars - if so, defer
    const innerVars = getClauseVars(clause.clause);
    if (innerVars.some((v) => !bind.has(v))) return true; // defer until bound
    return !clauseHolds(clause.clause, sent, bind);
  }
  if (clause.kind === 'optional') {
    // Optional clauses never cause failure; they're "nice to have"
    return true;
  }
  if (clause.kind === 'either') {
    // In explainMatch context, check if any branch holds
    // (In normal matching, either clauses are expanded at compile time)
    return clause.branches.some((branch) => allClausesHold(branch.clauses, sent, bind));
  }
  // Exhaustive check
  const _exhaustive: never = clause;
  throw new Error(`[grammar] Unknown clause kind: ${(_exhaustive as Clause).kind}`);
}

function allClausesHold(clauses: Clause[], sent: GinzaSentence, bind: Binding): boolean {
  for (const c of clauses) {
    if (!clauseHolds(c, sent, bind)) return false;
  }
  return true;
}

function extractVarsFromClauses(clauses: Clause[], vs: Set<string>): void {
  for (const c of clauses) {
    if (c.kind === 'node') vs.add(c.node.v);
    else if (c.kind === 'edge') {
      vs.add(c.child.v);
      vs.add(c.head.v);
    } else if (c.kind === 'next' || c.kind === 'before') {
      vs.add(c.a.v);
      vs.add(c.b.v);
    } else if (c.kind === 'either') {
      // Extract vars from all branches
      for (const branch of c.branches) {
        extractVarsFromClauses(branch.clauses, vs);
      }
    }
    // 'not' and 'optional' - ignore nested vars for simplicity
  }
}

function varsInSpec(spec: RuleSpec): string[] {
  const vs = new Set<string>();
  extractVarsFromClauses(spec.where, vs);
  return [...vs];
}

function findAnchorInClauses(clauses: Clause[]): string | null {
  for (const c of clauses) {
    if (c.kind === 'node') {
      if (c.preds.some((p) => p.kind === 'lemma' || p.kind === 'text')) return c.node.v;
    } else if (c.kind === 'either') {
      // Check all branches for an anchor
      for (const branch of c.branches) {
        const anchor = findAnchorInClauses(branch.clauses);
        if (anchor) return anchor;
      }
    }
  }
  return null;
}

function anchorVar(spec: RuleSpec): string | null {
  // Pick the first node var that has a literal lemma/text predicate; this is our anchor.
  return findAnchorInClauses(spec.where);
}

function buildCompiledSpec(spec: RuleSpec): CompiledSpec {
  const vars = varsInSpec(spec);
  const anchor = anchorVar(spec);
  // Deterministic ordering: anchor first, then sorted alphabetically
  const orderedVars = anchor
    ? [anchor, ...vars.filter((v) => v !== anchor).sort()]
    : [...vars].sort();

  const varToPredsFlat = new Map<string, TokPred[]>();
  const varToEdgesAsChild = new Map<string, EdgeClause[]>();
  const varToEdgesAsHead = new Map<string, EdgeClause[]>();
  const varToBeforeAsA = new Map<string, BeforeClause[]>();
  const varToBeforeAsB = new Map<string, BeforeClause[]>();
  const varToNextAsA = new Map<string, NextClause[]>();
  const varToNextAsB = new Map<string, NextClause[]>();

  // Recursively index clauses (including nested optional/either)
  function indexClauses(clauses: Clause[]): void {
    for (const c of clauses) {
      if (c.kind === 'node') {
        const preds = varToPredsFlat.get(c.node.v) ?? [];
        preds.push(...c.preds);
        varToPredsFlat.set(c.node.v, preds);
      } else if (c.kind === 'edge') {
        const childArr = varToEdgesAsChild.get(c.child.v) ?? [];
        childArr.push(c);
        varToEdgesAsChild.set(c.child.v, childArr);
        const headArr = varToEdgesAsHead.get(c.head.v) ?? [];
        headArr.push(c);
        varToEdgesAsHead.set(c.head.v, headArr);
      } else if (c.kind === 'before') {
        const aArr = varToBeforeAsA.get(c.a.v) ?? [];
        aArr.push(c);
        varToBeforeAsA.set(c.a.v, aArr);
        const bArr = varToBeforeAsB.get(c.b.v) ?? [];
        bArr.push(c);
        varToBeforeAsB.set(c.b.v, bArr);
      } else if (c.kind === 'next') {
        const aArr = varToNextAsA.get(c.a.v) ?? [];
        aArr.push(c);
        varToNextAsA.set(c.a.v, aArr);
        const bArr = varToNextAsB.get(c.b.v) ?? [];
        bArr.push(c);
        varToNextAsB.set(c.b.v, bArr);
      } else if (c.kind === 'optional') {
        indexClauses(c.clauses);
      } else if (c.kind === 'either') {
        for (const branch of c.branches) indexClauses(branch.clauses);
      }
      // 'not' clauses intentionally not indexed - they're negative constraints
    }
  }

  indexClauses(spec.where);

  // Pre-sort capture names for dedup key generation
  const sortedCaptureNames = spec.captures.map((c) => c.name).sort();

  return {
    spec,
    orderedVars,
    sortedCaptureNames,
    varToPredsFlat,
    varToEdgesAsChild,
    varToEdgesAsHead,
    varToBeforeAsA,
    varToBeforeAsB,
    varToNextAsA,
    varToNextAsB,
  };
}

function possibleBindingsForVar(
  sent: GinzaSentence,
  idx: SentenceIndex,
  varName: string,
  compiled: CompiledSpec,
  bind: Binding
): number[] {
  // Use precomputed preds
  const preds = compiled.varToPredsFlat.get(varName) ?? [];

  // Start with an indexed candidate set if possible (prefer lemma/text literals, then pos/dep).
  let candidates: number[] | null = null;
  for (const p of preds) {
    if (p.kind === 'lemma') candidates = candidates ? intersect(candidates, idx.byLemma.get(p.value) ?? []) : idx.byLemma.get(p.value) ?? [];
    else if (p.kind === 'text')
      candidates = candidates ? intersect(candidates, idx.byText.get(p.value) ?? []) : idx.byText.get(p.value) ?? [];
    else if (p.kind === 'pos')
      candidates = candidates ? intersect(candidates, idx.byPos.get(p.value) ?? []) : idx.byPos.get(p.value) ?? [];
    else if (p.kind === 'dep')
      candidates = candidates ? intersect(candidates, idx.byDep.get(p.value) ?? []) : idx.byDep.get(p.value) ?? [];
  }
  if (!candidates) candidates = idx.all;

  // Edge-based pruning: varName as child
  for (const c of compiled.varToEdgesAsChild.get(varName) ?? []) {
    const hIdx = bind.get(c.head.v);
    if (hIdx !== undefined) {
      const fromHead =
        c.dep !== undefined
          ? idx.childrenOfByDep.get(hIdx)?.get(c.dep) ?? []
          : idx.childrenOf.get(hIdx) ?? [];
      candidates = intersect(candidates, fromHead);
    }
  }

  // Edge-based pruning: varName as head
  for (const c of compiled.varToEdgesAsHead.get(varName) ?? []) {
    const cIdx = bind.get(c.child.v);
    if (cIdx !== undefined) {
      const tok = sent.tokens[cIdx];
      if (!tok) return [];
      if (c.dep !== undefined && tok.dep !== c.dep) return [];
      const h = tok.head;
      candidates = intersect(candidates, h >= 0 ? [h] : []);
    }
  }

  // Surface-order pruning: before clauses where varName is B (after A)
  for (const c of compiled.varToBeforeAsB.get(varName) ?? []) {
    const aIdx = bind.get(c.a.v);
    if (aIdx !== undefined) {
      const max = c.maxDistance !== undefined ? aIdx + c.maxDistance : sent.tokens.length - 1;
      candidates = intersect(candidates, windowCandidates(sent.tokens.length, aIdx, max));
    }
  }

  // Surface-order pruning: before clauses where varName is A (before B)
  for (const c of compiled.varToBeforeAsA.get(varName) ?? []) {
    const bIdx = bind.get(c.b.v);
    if (bIdx !== undefined) {
      const min = c.maxDistance !== undefined ? bIdx - c.maxDistance : 0;
      candidates = intersect(candidates, windowCandidates(sent.tokens.length, min - 1, bIdx - 1));
    }
  }

  // Surface-order pruning: next clauses where varName is B
  for (const c of compiled.varToNextAsB.get(varName) ?? []) {
    const aIdx = bind.get(c.a.v);
    if (aIdx !== undefined) candidates = intersect(candidates, aIdx + 1 < sent.tokens.length ? [aIdx + 1] : []);
  }

  // Surface-order pruning: next clauses where varName is A
  for (const c of compiled.varToNextAsA.get(varName) ?? []) {
    const bIdx = bind.get(c.b.v);
    if (bIdx !== undefined) candidates = intersect(candidates, bIdx - 1 >= 0 ? [bIdx - 1] : []);
  }

  // Final predicate filter (handles regex + inflection forms + conjugation class).
  // No need for unique() - intersect already dedupes, and index lookups return unique indices
  const out: number[] = [];
  for (const i of candidates) {
    const tok = sent.tokens[i];
    if (!tok) continue;
    if (preds.length === 0 || tokenMatchesPreds(tok, preds)) out.push(i);
  }
  return out;
}

function applyCaptureSpecs(
  captureSpecs: CaptureSpec[],
  sent: GinzaSentence,
  bind: Binding,
  sourceText: string,
  captures: Record<string, CaptureValue>
): boolean {
  for (const cap of captureSpecs) {
    if (cap.kind === 'token') {
      const tokIdx = bind.get(cap.var.v);
      if (tokIdx === undefined) {
        throw new Error(`[grammar] Capture '${cap.name}' references unbound var '${cap.var.v}'`);
      }
      const tok = sent.tokens[tokIdx];
      if (!tok) return false;
      captures[cap.name] = {
        start: tok.start,
        end: tok.end,
        text: sourceText.slice(tok.start, tok.end),
      };
    } else if (cap.kind === 'span') {
      const fromIdx = bind.get(cap.from.v);
      const toIdx = bind.get(cap.to.v);
      if (fromIdx === undefined) {
        throw new Error(`[grammar] Capture '${cap.name}' references unbound var '${cap.from.v}'`);
      }
      if (toIdx === undefined) {
        throw new Error(`[grammar] Capture '${cap.name}' references unbound var '${cap.to.v}'`);
      }
      const fromTok = sent.tokens[fromIdx];
      const toTok = sent.tokens[toIdx];
      if (!fromTok || !toTok) return false;
      const start = Math.min(fromTok.start, toTok.start);
      const end = Math.max(fromTok.end, toTok.end);
      captures[cap.name] = {
        start,
        end,
        text: sourceText.slice(start, end),
      };
    }
  }
  return true;
}

function buildCaptures(
  spec: RuleSpec,
  sent: GinzaSentence,
  bind: Binding,
  sourceText: string
): Record<string, CaptureValue> | null {
  const captures: Record<string, CaptureValue> = {};
  // either() branches are expanded at compile time, so captures are already merged into spec.captures
  if (!applyCaptureSpecs(spec.captures, sent, bind, sourceText, captures)) {
    return null;
  }
  return captures;
}

function findMatches(compiled: CompiledSpec, sent: GinzaSentence, sourceText: string, idx: SentenceIndex): Array<Record<string, CaptureValue>> {
  const { spec, orderedVars, sortedCaptureNames } = compiled;
  const matches: Array<Record<string, CaptureValue>> = [];
  const bind: Binding = new Map();

  function dfs(k: number): void {
    if (k === orderedVars.length) {
      // All clauses already checked at each step - just build captures
      const captures = buildCaptures(spec, sent, bind, sourceText);
      if (captures) matches.push(captures);
      return;
    }

    const v = orderedVars[k]!;
    for (const cand of possibleBindingsForVar(sent, idx, v, compiled, bind)) {
      bind.set(v, cand);
      if (allClausesHold(spec.where, sent, bind)) dfs(k + 1);
      bind.delete(v);
    }
  }

  dfs(0);
  // Dedup by capture span coordinates (pre-sorted names, no allocation)
  const seen = new Set<string>();
  return matches.filter((m) => {
    const key = sortedCaptureNames.map((k) => `${k}:${m[k]!.start}-${m[k]!.end}`).join('|');
    if (seen.has(key)) return false;
    seen.add(key);
    return true;
  });
}

function compileSingleRule(spec: RuleSpec): CompiledRule {
  const triggers = deriveTriggers(spec);
  if (triggers.length === 0) {
    throw new Error(
      `[grammar] Rule '${spec.id}' has no literal lemma/text triggers. ` +
        `Rules must include at least one exact { lemma: "..." } or { text: "..." } predicate ` +
        `so the trigger-indexed dispatch can consider them.`
    );
  }
  // Precompute clause lookups at compile time
  const compiled = buildCompiledSpec(spec);
  return {
    id: spec.id,
    triggers,
    details: spec.details,
    match: (sent: GinzaSentence, sourceText: string, idx?: SentenceIndex) => {
      const index = idx ?? buildSentenceIndex(sent);
      return findMatches(compiled, sent, sourceText, index);
    },
  };
}

/**
 * Compile a rule spec into one or more CompiledRules.
 * If the spec uses either(), it's expanded into N separate rules (one per branch).
 * Handles nested either() by recursively compiling branch specs.
 */
export function compileRule(spec: RuleSpec): CompiledRule[] {
  // Find either clause (if any)
  const eitherClause = spec.where.find((c): c is Extract<Clause, { kind: 'either' }> => c.kind === 'either');

  if (!eitherClause) {
    // No either - compile as single rule
    return [compileSingleRule(spec)];
  }

  // Expand either into N rules (one per branch)
  // Each branch becomes its own rule with the same id
  const otherClauses = spec.where.filter((c) => c.kind !== 'either');

  // Recursively compile each branch to handle nested either()
  const results: CompiledRule[] = [];
  for (const branch of eitherClause.branches) {
    const branchSpec: RuleSpec = {
      id: spec.id,
      where: [...otherClauses, ...branch.clauses],
      captures: [...spec.captures, ...branch.captures],
    };
    // Recursive call to handle nested either() in branch.clauses
    const compiled = compileRule(branchSpec);
    results.push(...compiled);
  }
  return results;
}

// ─────────────────────────────────────────────────────────────────────────────
// Explain Mode
// ─────────────────────────────────────────────────────────────────────────────

export type ExplainSuccess = {
  matched: true;
  captures: Record<string, CaptureValue>;
};

export type ExplainFailure = {
  matched: false;
  reason: string;
  failedClause?: Clause;
  partialBinding: Record<string, { tokenIdx: number; text: string }>;
  triedCandidates: Record<string, number[]>;
};

export type ExplainResult = ExplainSuccess | ExplainFailure;

function describeClause(c: Clause): string {
  switch (c.kind) {
    case 'node': {
      const preds = c.preds.map((p) => {
        if (p.kind === 'text') return `text="${p.value}"`;
        if (p.kind === 'textOneOf') return `text∈[${p.value.join(',')}]`;
        if (p.kind === 'lemma') return `lemma="${p.value}"`;
        if (p.kind === 'lemmaOneOf') return `lemma∈[${p.value.join(',')}]`;
        if (p.kind === 'pos') return `pos=${p.value}`;
        if (p.kind === 'dep') return `dep=${p.value}`;
        if (p.kind === 'inflectionForm') return `inflForm=${p.value}`;
        if (p.kind === 'conjugationClass') return `conjClass=${p.value}`;
        return `${p.kind}=...`;
      });
      return `node(${c.node.v}: ${preds.join(', ')})`;
    }
    case 'edge':
      return `edge(${c.child.v} --${c.dep ?? '*'}--> ${c.head.v})`;
    case 'before': {
      const dist = c.maxDistance !== undefined ? `≤${c.maxDistance}` : '';
      return `before(${c.a.v} < ${c.b.v}${dist})`;
    }
    case 'next':
      return `next(${c.a.v}, ${c.b.v})`;
    case 'not':
      return `not(${describeClause(c.clause)})`;
    case 'optional':
      return `optional(${c.clauses.length} clauses)`;
    case 'either':
      return `either(${c.branches.length} branches)`;
    default: {
      const _exhaustive: never = c;
      throw new Error(`Unknown clause kind: ${(_exhaustive as Clause).kind}`);
    }
  }
}

export function explainMatch(spec: RuleSpec, sent: GinzaSentence, sourceText: string): ExplainResult {
  // Handle either() by trying each branch separately
  const eitherClause = spec.where.find((c): c is Extract<Clause, { kind: 'either' }> => c.kind === 'either');
  
  if (eitherClause) {
    const otherClauses = spec.where.filter((c) => c.kind !== 'either');
    let bestFailure: ExplainFailure | null = null;
    
    for (const branch of eitherClause.branches) {
      const branchSpec: RuleSpec = {
        id: spec.id,
        where: [...otherClauses, ...branch.clauses],
        captures: [...spec.captures, ...branch.captures],
      };
      const result = explainMatch(branchSpec, sent, sourceText);
      if (result.matched) return result;
      // Track deepest failure across branches
      if (!bestFailure || Object.keys(result.partialBinding).length > Object.keys(bestFailure.partialBinding).length) {
        bestFailure = result;
      }
    }
    return bestFailure ?? { matched: false, reason: 'No branches matched', partialBinding: {}, triedCandidates: {} };
  }

  const idx = buildSentenceIndex(sent);
  const compiled = buildCompiledSpec(spec);
  const { orderedVars } = compiled;

  const bind: Binding = new Map();
  const triedCandidates: Record<string, number[]> = {};

  // Track the "deepest" failure point
  let deepestFailure: ExplainFailure | null = null;

  /** Helper to create and track failure records */
  function recordFailure(reason: string, clause?: Clause): void {
    const failure: ExplainFailure = {
      matched: false,
      reason,
      failedClause: clause,
      partialBinding: Object.fromEntries(
        [...bind.entries()].map(([v, i]) => [v, { tokenIdx: i, text: sent.tokens[i]?.text ?? '' }])
      ),
      triedCandidates: { ...triedCandidates },
    };
    const depth = Object.keys(failure.partialBinding).length;
    const deepestDepth = deepestFailure ? Object.keys(deepestFailure.partialBinding).length : -1;
    if (depth >= deepestDepth) {
      deepestFailure = failure;
    }
  }

  function dfs(k: number): Record<string, CaptureValue> | null {
    if (k === orderedVars.length) {
      // Check all clauses one final time and find the first failure
      for (const c of spec.where) {
        if (!clauseHolds(c, sent, bind)) {
          recordFailure(`Clause failed: ${describeClause(c)}`, c);
          return null;
        }
      }
      return buildCaptures(spec, sent, bind, sourceText);
    }

    const v = orderedVars[k]!;
    const candidates = possibleBindingsForVar(sent, idx, v, compiled, bind);
    triedCandidates[v] = candidates;

    if (candidates.length === 0) {
      recordFailure(`No candidates for variable '${v}'`);
      return null;
    }

    for (const cand of candidates) {
      bind.set(v, cand);
      if (allClausesHold(spec.where, sent, bind)) {
        const result = dfs(k + 1);
        if (result) return result;
      } else {
        // Track this failure if it's the deepest
        for (const c of spec.where) {
          if (!clauseHolds(c, sent, bind)) {
            recordFailure(`Clause failed: ${describeClause(c)}`, c);
            break;
          }
        }
      }
      bind.delete(v);
    }

    return null;
  }

  const captures = dfs(0);
  if (captures) {
    return { matched: true, captures };
  }

  return deepestFailure ?? {
    matched: false,
    reason: 'No matches found',
    partialBinding: {},
    triedCandidates,
  };
}

