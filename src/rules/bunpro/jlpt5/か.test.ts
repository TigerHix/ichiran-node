import { describe, it, expect } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import rule from './か.js';
import { BUNPRO_JLPT5 } from './index.js';

// Manual test data for か since it's filtered by isNonTrivialSlug()
const positives = [
  'あなたのアパートは大きいですか。',
  'これはコップですか。',
  '大丈夫か？',
  '大丈夫ですか？',
  'これは何か分からない。',
  '美味しいか分からない。',
  'これはペンですか。',
  'これは本ですか。',
  'これはあなたの本ですか。',
  'これもあなたの本ですか。',
  'これはいいですか。',
  '明日もいいですか。',
  'これは、ユウキさんのお母さんですか。',
  'これはゾンビですか。',
  'お元気ですか。',
  '映画はいいですか。',
  'これはよくないですか。',
  'これは、必要ですか。',
];

describe('bunpro.jlpt5 - か', () => {
  const engine = useSharedEngine([BUNPRO_JLPT5]);
  
  describe('positives', () => {
    for (const sentence of positives) {
      it(`✓ ${sentence.slice(0, 40)}`, async () => {
        const e = engine.get();
        const hits = await e.match(sentence);
        const hit = hits.find((h) => h.ruleId === rule.id);

        if (!hit) {
          const explain = await e.explainMatch(sentence, rule.id);
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
        expect(hit!.rulesetId).toBe(BUNPRO_JLPT5.id);

        for (const [, cap] of Object.entries(hit!.captures)) {
          expect(typeof cap.start).toBe('number');
          expect(typeof cap.end).toBe('number');
          expect(cap.text.length).toBeGreaterThan(0);
        }
      });
    }
  });
});
