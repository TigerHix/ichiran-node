import { describe, test } from 'bun:test';
import { useSharedEngine } from './src/rules/bunpro/_test/engine.js';
import { BUNPRO_JLPT4 } from './src/rules/bunpro/jlpt4/index.js';

describe('DEBUG: Check te relationship', () => {
  const engine = useSharedEngine([BUNPRO_JLPT4]);

  test('analyze te connection to iku', async () => {
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
          console.log('iku at index ' + ikuIdx + ': dep=' + iku.dep + ', head=' + iku.head);
          
          if (iku.head >= 0 && iku.head < tokens.length) {
            const headTok = tokens[iku.head];
            console.log('  -> head: ' + headTok.text + ' [lemma=' + headTok.lemma + ', pos=' + headTok.pos + ']');
          }
          
          const tePointingToIku = tokens.find(t => t.lemma === 'て' && t.head === ikuIdx);
          console.log('  te pointing to iku: ' + (tePointingToIku ? 'YES (index ' + tokens.indexOf(tePointingToIku) + ')' : 'NO'));
        }
      }
    }
  });
});
