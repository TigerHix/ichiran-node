import { describe, it } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './それなら.js';
import { BUNPRO_JLPT2 } from './index.js';

// Negative test cases - sentences that should NOT match the それなら grammar rule
const negatives = [
  // なら (nara) alone - general conditional without "that" reference
  '行くなら、私も行きたい。',
  '雨なら行かない。',
  'できるならやってみて。',
  'そんなに嫌なら、やめればいい。',

  // それ (sore) alone - demonstrative pronoun
  'それは私の本です。',
  'それを見せてください。',
  'それを食べました。',

  // で + は as separate particles (locative では, not conjunction)
  '東京では雨が降っている。',
  '日本では桜が有名です。',
  'ここでは喫煙禁止です。',

  // Similar conjunctions with different meanings
  // では・それでは (dewa/soredewa) - "well then, in that case" (broader usage)
  'それでは、始めましょう。',
  'では、また明日。',

  // その場合なら (sono baai nara) - "in that specific case" (more formal)
  'その場合なら、別の方法を考えましょう。',

  // そうすれば (sureba) - "if (you) do that" (conditional verb form)
  'そうすればうまくいくと思います。',
  'そうすれば問題は解決するだろう。',

  // それなのに (sore noni) - "even though that, despite that" (concessive)
  'それなのに、彼は来なかった。',
  '頑張ったのに、それなのだ。',

  // それにしても (sore ni shitemo) - "even so, be that as it may" (adverbial)
  'それにしても、彼は遅いな。',

  // それから (sorekara) - "and then, after that" (temporal sequence)
  '朝ごはんを食べた。それから学校に行った。',

  // それでも (soredemo) - "even so, nevertheless" (concessive)
  '雨が降っている。それでも出かけます。',

  // それとも (soretomo) - "or, or else" (alternative question)
  'コーヒーにしますか？それとも紅茶にしますか？',
];

describe('bunpro.jlpt2', () => {
  const engine = useSharedEngine([BUNPRO_JLPT2]);
  describeRule(rule, 'JLPT2', BUNPRO_JLPT2.id, engine.get, { negatives });
});

// Debug: test specific sentences separately
describe('bunpro.jlpt2 debug', () => {
  const engine = useSharedEngine([BUNPRO_JLPT2]);

  it('debug: だったら sentence', async () => {
    const e = engine.get();
    const sent = '一人で食べに行くの？だったら僕も一緒に行ってもいい？';
    const hits = await e.match(sent);
    console.log('\n=== DEBUG:', sent, '===');
    console.log('All hits:', hits.map(h => ({ruleId: h.ruleId, captures: h.captures})));
    const hit = hits.find(h => h.ruleId === 'それなら');
    console.log('Hit for それなら:', hit);

    // Analyze the sentence
    const doc = await e.analyze(sent);
    console.log('\nDoc keys:', Object.keys(doc || {}));
    console.log('Number of sentences:', doc?.sentences?.length);
    if (doc && doc.sentences) {
      for (let i = 0; i < doc.sentences.length; i++) {
        console.log(`\nSentence ${i} tokens:`);
        for (const token of doc.sentences[i].tokens) {
          console.log(`  ${token.text}\tPOS=${token.pos}\tLEMMA=${token.lemma || 'N/A'}`);
        }
      }
    }
  });
});
