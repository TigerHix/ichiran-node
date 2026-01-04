import { linguisticRule } from '../../../engine/lang.js';

export default linguisticRule('のがへた', (r) => {
  // Verb (dictionary form) + の + が + 下手(へた) = "poor at doing, unskilled at"
  // Examples: 歌うのが下手 (poor at singing), 漢字を書くのが下手 (poor at writing kanji)
  //
  // Structure:
  // - Verb in dictionary form (辞書形)
  // - の (nominalizer)
  // - が (subject marker)
  // - 下手/へた (na-adjective meaning unskilled/poor at)
  //
  // This is the antonym of のが上手 (good at) and similar structure to のが好き
  //
  // GiNZA parsing notes:
  // - The verb can be suru-verb: 料理する, 勉強する, 運転する
  // - Or regular verb with object: 漢字を書く, パンを作る
  // - The nominalized phrase (verb+の) becomes the subject marked by が
  // - 下手 can be written in kanji or hiragana

  r.either(
    // Branch 1: 下手/へた (basic form)
    (branch) => {
      const verb = branch.verb({}, 'verb');
      const no = branch.particle('の', 'no');
      const ga = branch.particle('が', 'ga');
      const heta = branch.tok({
        lemmaOneOf: ['下手', 'へた'],
        posOneOf: ['NOUN', 'ADJ', 'VERB'],
      }, 'heta');

      branch.inOrder(verb, no, 3);
      branch.inOrder(no, ga, 1);
      branch.inOrder(ga, heta, 1);
      branch.captureSpan('のがへた', verb, heta);
    },

    // Branch 2: 大下手/大へた (intensified form - "very poor at")
    (branch) => {
      const verb = branch.verb({}, 'verb');
      const no = branch.particle('の', 'no');
      const ga = branch.particle('が', 'ga');
      const heta = branch.tok({
        lemmaOneOf: ['大下手', '大へた'],
        posOneOf: ['NOUN', 'ADJ'],
      }, 'heta');

      branch.inOrder(verb, no, 3);
      branch.inOrder(no, ga, 1);
      branch.inOrder(ga, heta, 1);
      branch.captureSpan('のがへた', verb, heta);
    }
  );
});
