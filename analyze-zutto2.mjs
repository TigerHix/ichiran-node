import { GrammarEngine } from './packages/grammar/src/index.js';

async function debug() {
  const engine = await GrammarEngine.create([], {
    ginza: { python: 'python3' },
  });

  // Positive examples from Bunpro data (what should match)
  const positives = [
    'さっき見た鳥よりずっと大きかったよ！',
    'この車は私のよりずっとよく見える。',
    '太陽のほうが地球よりずっと大きいです。',
    '「でも勇者さまはずっと強いし...」',
    'さかもとくんは他のクラスメイトたちよりも、ずっと大人らしいね。',
    'それよりずっと昔だよ。',
    '僕よりもずっと年上だということ、信じられないよ。',
    'この建物はここから、思ったよりもずっと大きく見えます。',
    '漫画の主人公：「これから、ずっとずっと強くなるぞ。」',
    'コンピュータは以前に比べてずっと使いやすくなった。',
    'アムステルダムに比べて東京のほうがずっと混雑している都市です。',
  ];

  // Negative examples (what should NOT match)
  const negatives = [
    'ずっと待っていた。',
    'ずっと好きです。',
    'ずっと一緒にいる。',
    'ずっと住んでいます。',
    'ずっと勉強している。',
  ];

  console.log('=== POSITIVES (should match) ===');
  for (const sentence of positives) {
    console.log('\n' + sentence);
    const doc = await engine.analyze(sentence);
    const zuttoIdx = doc.sentences[0].tokens.findIndex(t => t.text === 'ずっと');
    if (zuttoIdx >= 0) {
      // Show next few tokens after ずっと
      for (let i = zuttoIdx; i < Math.min(zuttoIdx + 5, doc.sentences[0].tokens.length); i++) {
        const t = doc.sentences[0].tokens[i];
        console.log(`  ${t.text}\t${t.pos}\t${t.tag}\t${t.lemma}`);
      }
    }
  }

  console.log('\n\n=== NEGATIVES (should NOT match) ===');
  for (const sentence of negatives) {
    console.log('\n' + sentence);
    const doc = await engine.analyze(sentence);
    const zuttoIdx = doc.sentences[0].tokens.findIndex(t => t.text === 'ずっと');
    if (zuttoIdx >= 0) {
      // Show next few tokens after ずっと
      for (let i = zuttoIdx; i < Math.min(zuttoIdx + 5, doc.sentences[0].tokens.length); i++) {
        const t = doc.sentences[0].tokens[i];
        console.log(`  ${t.text}\t${t.pos}\t${t.tag}\t${t.lemma}`);
      }
    }
  }

  await engine.close();
}

debug().catch(console.error);
