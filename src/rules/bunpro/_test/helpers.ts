/**
 * Test helpers for bunpro rule tests.
 */
import { describe, it, expect } from 'bun:test';
import { existsSync } from 'node:fs';
import { join } from 'node:path';
import { fileURLToPath } from 'node:url';
import type { BunproGrammarItem, BunproLevel } from '@grammar/data/bunpro/types.js';
import { loadBunproGrammarItemWithOptions } from '@grammar/data/bunpro/loader.js';
import type { GrammarEngine } from '../../../program.js';
import type { RuleSpec } from '../../../engine/dsl.js';

const DATA_ROOT = fileURLToPath(new URL('../../../../data/bunpro', import.meta.url));

export function loadTestItem(ruleId: string, level: BunproLevel): BunproGrammarItem {
  const filePath = join(DATA_ROOT, level, `${ruleId}.json`);
  if (!existsSync(filePath)) {
    throw new Error(`Missing test data: ${filePath} for rule '${ruleId}'`);
  }
  const item = loadBunproGrammarItemWithOptions(filePath, level, { allowTrivialSlug: true });
  if (!item) {
    throw new Error(`Failed to load bunpro item: ${filePath}`);
  }
  return item;
}

export type RuleTestOptions = {
  /** Sentences that should NOT match (false positive tests) */
  negatives?: string[];
  /** Positive sentences to skip (known GiNZA parsing limitations) */
  skipPositives?: string[];
};

/**
 * Describe tests for a single rule with test sentences from bunpro data.
 */
export function describeRule(
  rule: RuleSpec,
  level: BunproLevel,
  rulesetId: string,
  getEngine: () => GrammarEngine,
  opts: RuleTestOptions = {}
) {
  describe(rule.id, () => {
    const item = loadTestItem(rule.id, level);

    describe('positives', () => {
      const { skipPositives = [] } = opts;
      for (const { sentence } of item.sentences) {
        if (skipPositives.includes(sentence)) {
          it.skip(`✓ ${sentence.slice(0, 40)} (GiNZA limitation)`, () => {});
          continue;
        }
        it(`✓ ${sentence.slice(0, 40)}`, async () => {
          const engine = getEngine();
          const hits = await engine.match(sentence);
          const hit = hits.find((h) => h.ruleId === rule.id);

          if (!hit) {
            const explain = await engine.explainMatch(sentence, rule.id);
            if (!explain.matched) {
              console.log(`\n❌ Rule '${rule.id}' failed on: ${sentence}`);
              console.log(`   Reason: ${explain.reason}`);
              if (explain.failedClause) {
                console.log(`   Failed clause: ${explain.failedClause.kind}`);
              }
              if (explain.partialBinding && Object.keys(explain.partialBinding).length > 0) {
                console.log(`   Partial bindings: ${JSON.stringify(explain.partialBinding)}`);
              }
            }
          }

          expect(hit).toBeDefined();
          expect(hit!.rulesetId).toBe(rulesetId);

          for (const [, cap] of Object.entries(hit!.captures)) {
            expect(typeof cap.start).toBe('number');
            expect(typeof cap.end).toBe('number');
            expect(cap.text.length).toBeGreaterThan(0);
          }
        });
      }
    });

    const { negatives } = opts;
    if (negatives && negatives.length > 0) {
      describe('negatives', () => {
        for (const sentence of negatives) {
          it(`✗ ${sentence.slice(0, 40)}`, async () => {
            const engine = getEngine();
            const hits = await engine.match(sentence);
            const hit = hits.find((h) => h.ruleId === rule.id);

            if (hit) {
              console.log(`\n❌ FALSE POSITIVE: Rule '${rule.id}' matched: ${sentence}`);
              console.log(`   Captured: ${JSON.stringify(hit.captures)}`);
            }

            expect(hit).toBeUndefined();
          });
        }
      });
    }
  });
}

