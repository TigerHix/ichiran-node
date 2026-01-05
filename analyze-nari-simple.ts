import { GiNZA, initializeGiNZA } from './packages/grammar/src/ginza/mod.js';

const sentences = [
  'いらないなら売るなり捨てるなりしてもらっても結構ですので。',
  'お姉ちゃんなりお兄ちゃんなりに質問して！',
  'ご飯にするなり運動するなり好きにすればいいけど、早く寝なさいよ。',
  '俺になり母さんになり電話くれればよかったのに。',
  '大なり小なり欠点はあるものだ。',
  '両親なり友達なり、心から信頼できる人はいますか？',
  'フランスなりイタリアなり、料理が美味しいところに行きたい。',
  '電話になりメールなりするのが社会人としての常識じゃないのか？！',
  '俺を煮るなり焼くなり好きにしていい！',
];

async function main() {
  await initializeGiNZA();
  const ginza = new GiNZA();

  for (const sentence of sentences) {
    console.log(`\n=== ${sentence} ===`);
    const doc = await ginza.parseDetailed(sentence);
    for (const token of doc.tokens) {
      console.log(`${token.surface}\t${token.pos}\tlemma=${token.lemma}\tdep=${token.dep}\thead=${token.head}`);
    }
  }

  await ginza.close();
}

main().catch(console.error);
