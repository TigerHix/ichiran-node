import type { RuleSpec, CompiledRule, CaptureValue } from './engine/index.js';
import { compileRule, buildSentenceIndex } from './engine/index.js';
import type { GinzaSentence, GinzaDoc } from './ginza/types.js';

/** A ruleset groups rules under a common id (e.g., "bunpro.jlpt1") */
export type Ruleset = {
  id: string;
  rules: RuleSpec[];
};

export type CompiledRuleset = {
  id: string;
  rules: CompiledRule[];
};

export type MatchHit = {
  ruleId: string;
  rulesetId: string;
  captures: Record<string, CaptureValue>;
};

type Trigger = { kind: 'lemma' | 'text'; value: string };

function triggerKey(t: Trigger): string {
  return `${t.kind}:${t.value}`;
}

export type CompiledProgram = {
  rulesets: CompiledRuleset[];
  // Dispatch: trigger key -> [rulesetIdx, ruleIdx][]
  dispatch: Map<string, Array<[rulesetIdx: number, ruleIdx: number]>>;
};

export function compileRuleset(rs: Ruleset): CompiledRuleset {
  return {
    id: rs.id,
    rules: rs.rules.flatMap(compileRule),  // flatMap since compileRule returns array (either expansion)
  };
}

export function buildProgram(rulesets: Ruleset[]): CompiledProgram {
  const compiled = rulesets.map(compileRuleset);
  const dispatch = new Map<string, Array<[number, number]>>();

  for (let rsIdx = 0; rsIdx < compiled.length; rsIdx++) {
    const rs = compiled[rsIdx]!;
    for (let rIdx = 0; rIdx < rs.rules.length; rIdx++) {
      const rule = rs.rules[rIdx]!;
      for (const t of rule.triggers) {
        const k = triggerKey(t);
        const arr = dispatch.get(k) ?? [];
        arr.push([rsIdx, rIdx]);
        dispatch.set(k, arr);
      }
    }
  }

  return { rulesets: compiled, dispatch };
}

function sentenceTokenTriggerKeys(sent: GinzaSentence): string[] {
  const keys: string[] = [];
  for (const tok of sent.tokens) {
    keys.push(`lemma:${tok.lemma}`);
    keys.push(`text:${tok.text}`);
  }
  return keys;
}

export type MatchOptions = {
  /** If provided, only match rules from these ruleset ids */
  rulesetIds?: string[];
};

export function matchSentence(
  program: CompiledProgram,
  sent: GinzaSentence,
  sourceText: string,
  opts: MatchOptions = {}
): MatchHit[] {
  const enabledRulesets = opts.rulesetIds
    ? new Set(opts.rulesetIds)
    : null;

  // Collect candidate [rsIdx, rIdx] pairs
  const candidates = new Set<string>();
  for (const k of sentenceTokenTriggerKeys(sent)) {
    const pairs = program.dispatch.get(k);
    if (!pairs) continue;
    for (const [rsIdx, rIdx] of pairs) {
      if (enabledRulesets && !enabledRulesets.has(program.rulesets[rsIdx]!.id)) continue;
      candidates.add(`${rsIdx}:${rIdx}`);
    }
  }

  // Build index once, reuse for all rule matches
  const idx = buildSentenceIndex(sent);

  const hits: MatchHit[] = [];
  for (const key of candidates) {
    const [rsIdxStr, rIdxStr] = key.split(':');
    const rsIdx = Number(rsIdxStr);
    const rIdx = Number(rIdxStr);
    const rs = program.rulesets[rsIdx]!;
    const rule = rs.rules[rIdx]!;
    const capturesList = rule.match(sent, sourceText, idx);
    for (const captures of capturesList) {
      hits.push({ ruleId: rule.id, rulesetId: rs.id, captures });
    }
  }
  return hits;
}

export function matchDoc(
  program: CompiledProgram,
  doc: GinzaDoc,
  sourceText: string,
  opts: MatchOptions = {}
): MatchHit[] {
  const hits: MatchHit[] = [];
  for (const sent of doc.sentences) {
    hits.push(...matchSentence(program, sent, sourceText, opts));
  }
  return hits;
}

