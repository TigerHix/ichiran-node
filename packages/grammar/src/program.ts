import { GinzaClient } from './ginza/client.js';
import type { GinzaClientOptions } from './ginza/client.js';
import type { GinzaDoc } from './ginza/types.js';
import { explainMatch as explainMatchInternal } from './engine/compiler.js';
import type { ExplainResult } from './engine/compiler.js';
import type { RuleSpec } from './engine/dsl.js';
import { buildProgram, matchDoc, type MatchHit, type CompiledProgram, type Ruleset, type MatchOptions } from './ruleset.js';

export type GrammarEngineOptions = {
  ginza?: GinzaClientOptions;
  /** Use an existing GinzaClient instead of creating a new one */
  client?: GinzaClient;
};

export type RuleDetails = {
  ruleId: string;
  rulesetId: string;
  name?: string;
  description?: string;
};

export class GrammarEngine {
  private client: GinzaClient;
  private program: CompiledProgram;
  private ruleSpecs: Map<string, RuleSpec>;
  private ruleToRuleset: Map<string, string>;

  private constructor(client: GinzaClient, program: CompiledProgram, ruleSpecs: Map<string, RuleSpec>, ruleToRuleset: Map<string, string>) {
    this.client = client;
    this.program = program;
    this.ruleSpecs = ruleSpecs;
    this.ruleToRuleset = ruleToRuleset;
  }

  static async create(rulesets: Ruleset[], opts: GrammarEngineOptions = {}): Promise<GrammarEngine> {
    const program = buildProgram(rulesets);
    const specsMap = new Map<string, RuleSpec>();
    const ruleToRuleset = new Map<string, string>();
    for (const rs of rulesets) {
      for (const r of rs.rules) {
        specsMap.set(r.id, r);
        ruleToRuleset.set(r.id, rs.id);
      }
    }
    const client = opts.client ?? new GinzaClient(opts.ginza);
    if (!opts.client) {
      await client.start();
    }
    return new GrammarEngine(client, program, specsMap, ruleToRuleset);
  }

  async close(): Promise<void> {
    await this.client.stop();
  }

  getRulesetIds(): string[] {
    return this.program.rulesets.map((rs) => rs.id);
  }

  getRuleIds(): string[] {
    return this.program.rulesets.flatMap((rs) => rs.rules.map((r) => r.id));
  }

  getRuleDetails(ruleId: string): RuleDetails | null {
    const spec = this.ruleSpecs.get(ruleId);
    if (!spec) return null;
    const rulesetId = this.ruleToRuleset.get(ruleId);
    if (!rulesetId) return null;
    const raw = spec.details;
    return {
      ruleId,
      rulesetId,
      name: raw?.data?.attributes?.title,
      description: raw?.data?.attributes?.meaning,
    };
  }

  async match(text: string, opts: MatchOptions = {}): Promise<MatchHit[]> {
    const [doc] = await this.client.analyze([text]);
    if (!doc) return [];
    return matchDoc(this.program, doc, text, opts);
  }

  async analyze(text: string): Promise<GinzaDoc | null> {
    const [doc] = await this.client.analyze([text]);
    return doc ?? null;
  }

  /** Match a pre-parsed doc (for benchmarking without GiNZA overhead). */
  matchDoc(doc: GinzaDoc, sourceText: string, opts: MatchOptions = {}): MatchHit[] {
    return matchDoc(this.program, doc, sourceText, opts);
  }

  /** Explain why a specific rule did or did not match. */
  async explainMatch(text: string, ruleId: string): Promise<ExplainResult> {
    const spec = this.ruleSpecs.get(ruleId);
    if (!spec) {
      return { matched: false, reason: `Unknown rule: ${ruleId}`, partialBinding: {}, triedCandidates: {} };
    }
    const [doc] = await this.client.analyze([text]);
    if (!doc || doc.sentences.length === 0) {
      return { matched: false, reason: 'No sentences parsed', partialBinding: {}, triedCandidates: {} };
    }
    // Explain on first sentence (could extend to all sentences later)
    return explainMatchInternal(spec, doc.sentences[0]!, text);
  }
}

export type { MatchHit, MatchOptions, Ruleset, ExplainResult };
