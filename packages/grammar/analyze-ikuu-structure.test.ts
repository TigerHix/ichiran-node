import { describe, test } from 'bun:test';
import { useSharedEngine } from './src/rules/bunpro/_test/engine.js';
import { BUNPRO_JLPT4 } from './src/rules/bunpro/jlpt4/index.js';

describe('DEBUG: Compare ていく vs standalone いく', () => {
  const engine = useSharedEngine([BUNPRO_JLPT4]);

  test('analyze structures', async () => {
    const e = engine.get();
    
    const sentences = [
      { sent: '持っていく', desc: 'verb-te + いく' },
      { sent: '食べていく', desc: 'verb-te + いく' },
      { sent: '京都へいく', desc: 'place + へ + いく (standalone)' },
      { sent: '学校にいく', desc: 'place + に + いく (standalone)' },
    ];
    
    for (const { sent, desc } of sentences) {
      console.log('\n=== ' + sent + ' (' + desc + ') ===');
      const doc = await e.analyze(sent);
      if (doc && doc.sentences.length > 0) {
        const tokens = doc.sentences[0].tokens;
        tokens.forEach((tok, i) => {
          const hasTeChild = tokens.some(t => t.head === i && t.lemma === 'て');
          console.log(`  ${i}: ${tok.text} [${tok.pos}] lemma=${tok.lemma} dep=${tok.dep} head=${tok.head} hasTeChild=${hasTeChild}`);
        });
      }
    }
  });
});
