import { describe, test } from 'bun:test';
import { useSharedEngine } from './src/rules/bunpro/_test/engine.js';
import { BUNPRO_JLPT4 } from './src/rules/bunpro/jlpt4/index.js';

describe('DEBUG: Check token before iku', () => {
  const engine = useSharedEngine([BUNPRO_JLPT4]);

  test('analyze tokens before iku', async () => {
    const e = engine.get();
    
    const sentences = [
      '持っていく',
      '食べていく',
      '買っていく',
      '京都へいく',
      '学校にいく',
    ];
    
    for (const sent of sentences) {
      console.log('\n=== ' + sent + ' ===');
      const doc = await e.analyze(sent);
      if (doc && doc.sentences.length > 0) {
        const tokens = doc.sentences[0].tokens;
        const ikuIdx = tokens.findIndex(t => t.lemma === 'いく');
        if (ikuIdx >= 0) {
          const iku = tokens[ikuIdx];
          console.log('iku at index ' + ikuIdx);
          
          if (ikuIdx > 0) {
            const prevTok = tokens[ikuIdx - 1];
            console.log('  previous token: ' + prevTok.text + ' [lemma=' + prevTok.lemma + ', pos=' + prevTok.pos + ']');
          }
          
          if (ikuIdx > 1) {
            const prev2Tok = tokens[ikuIdx - 2];
            console.log('  2 tokens back: ' + prev2Tok.text + ' [lemma=' + prev2Tok.lemma + ', pos=' + prev2Tok.pos + ']');
          }
        }
      }
    }
  });
});
