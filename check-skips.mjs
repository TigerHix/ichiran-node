import { analyzeGrammar } from './packages/grammar/src/engine/index.js';

const tests = [
  ['おおい parsing issue', '今週は仕事がおおい。'],
  ['つまらない parsing issue', 'この本はつまらないです。'],
  ['ある→ない as ADJ', '紙がなかった。'],
  ['ある→ない as ADJ 2', 'お金はない。'],
  ['Noun+で lemma issue 1', '彼は漫画家です'],
  ['Noun+で lemma issue 2', 'クエンティンさんは映画監督です'],
];

for (const [label, text] of tests) {
  console.log('\n' + '='.repeat(60));
  console.log(label + ': ' + text);
  console.log('='.repeat(60));
  const result = analyzeGrammar(text);
  result.tokens.forEach(t => {
    if (t.pos === 'ADJ' || t.pos === 'AUX' || t.pos === 'VERB' || t.pos === 'SCONJ' || t.pos === 'ADP' || t.lemma === 'おおい' || t.lemma === 'つまらない' || t.text === 'です' || t.text === '監督') {
      console.log('  "' + t.text + '" lemma=' + t.lemma + ' pos=' + t.pos + ' tag=' + (t.tag || '(none)') + ' conj=' + (t.conjugationClass || '(none)'));
    }
  });
}
