import { createGinzaClient } from './packages/grammar/src/ginza/client.ts';

const client = await createGinzaClient();
const sent = 'そんな歩き方をしていては、ペンギンかと思われますよ。';
console.log('=== ' + sent + ' ===');
const doc = await client.analyze(sent);
console.log(JSON.stringify(doc.tokens.map(t => ({
  id: t.id,
  text: t.text,
  lemma: t.lemma,
  pos: t.pos,
  head: t.head,
  dep: t.dep
})), null, 2));

const sent2 = '毎日お菓子ばかりを食べていては、いつまで経っても痩せませんよ。';
console.log('\n=== ' + sent2 + ' ===');
const doc2 = await client.analyze(sent2);
console.log(JSON.stringify(doc2.tokens.map(t => ({
  id: t.id,
  text: t.text,
  lemma: t.lemma,
  pos: t.pos,
  head: t.head,
  dep: t.dep
})), null, 2));
