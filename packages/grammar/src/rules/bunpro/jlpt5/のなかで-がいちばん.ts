import { linguisticRule } from '../../../engine/lang.js';

export default linguisticRule('のなかで-がいちばん', (r) => {
  // Pattern: [Noun/この/その] のなかで/中で + [noun + が/は] + 一番/いちばん
  // Meaning: "Out of (group), (item) is the most (adjective)"
  //
  // This rule handles multiple sentence patterns:
  // 1. Noun + の + 中で + noun + が + 一番 + adj
  // 2. この/その + 中で + 一番 + adj + noun
  // 3. Noun + の + 中で + 一番 + adj (no intermediate noun + が/は)
  // 4. Variations with は instead of が

  r.either(
    // Pattern 1: Noun + の + 中/なか + で + itemNoun + が + ichiban
    (branch1) => {
      const groupNoun = branch1.tok({ posOneOf: ['NOUN', 'PRON', 'PROPN', 'DET', 'NUM'] }, 'groupNoun');
      const no = branch1.particle('の', 'no');
      const naka = branch1.tok({ textOneOf: ['中', 'なか'], posOneOf: ['NOUN', 'PROPN'] }, 'naka');
      const de = branch1.particle('で', 'de');
      const itemNoun = branch1.tok({ posOneOf: ['NOUN', 'PRON', 'PROPN', 'DET', 'NUM'] }, 'itemNoun');
      const subjectMarker = branch1.particle('が', 'subjectMarker');
      const ichiban = branch1.tok({ textOneOf: ['一番', 'いちばん'], posOneOf: ['NOUN', 'ADV'] }, 'ichiban');

      branch1.caseMarker(groupNoun, no);
      branch1.inOrder(no, naka, 3);
      branch1.caseMarker(naka, de);
      branch1.caseMarker(itemNoun, subjectMarker);
      branch1.inOrder(groupNoun, no, 1);
      branch1.inOrder(no, naka, 1);
      branch1.inOrder(naka, de, 1);
      branch1.inOrder(de, itemNoun);
      branch1.inOrder(itemNoun, subjectMarker, 1);
      branch1.inOrder(subjectMarker, ichiban, 1);
      branch1.captureSpan('のなかで-がいちばん', groupNoun, ichiban);
    },
    // Pattern 2: この/その + 中/なか + で + ichiban (no intermediate noun)
    (branch2) => {
      const groupNoun = branch2.tok({ textOneOf: ['この', 'その', 'あの'], pos: 'DET' }, 'groupNoun');
      const naka = branch2.tok({ textOneOf: ['中', 'なか'], posOneOf: ['NOUN', 'PROPN'] }, 'naka');
      const de = branch2.particle('で', 'de');
      const ichiban = branch2.tok({ textOneOf: ['一番', 'いちばん'], posOneOf: ['NOUN', 'ADV'] }, 'ichiban');

      branch2.inOrder(groupNoun, naka, 1);
      branch2.caseMarker(naka, de);
      branch2.inOrder(naka, de, 1);
      branch2.inOrder(de, ichiban);
      branch2.captureSpan('のなかで-がいちばん', groupNoun, ichiban);
    },
    // Pattern 3: Noun + の + 中/なか + で + ichiban (no intermediate noun + が/は)
    (branch3) => {
      const groupNoun = branch3.tok({ posOneOf: ['NOUN', 'PRON', 'PROPN', 'DET', 'NUM'] }, 'groupNoun');
      const no = branch3.particle('の', 'no');
      const naka = branch3.tok({ textOneOf: ['中', 'なか'], posOneOf: ['NOUN', 'PROPN'] }, 'naka');
      const de = branch3.particle('で', 'de');
      const ichiban = branch3.tok({ textOneOf: ['一番', 'いちばん'], posOneOf: ['NOUN', 'ADV'] }, 'ichiban');

      branch3.caseMarker(groupNoun, no);
      branch3.inOrder(no, naka, 3);
      branch3.caseMarker(naka, de);
      branch3.inOrder(groupNoun, no, 1);
      branch3.inOrder(no, naka, 1);
      branch3.inOrder(naka, de, 1);
      branch3.inOrder(de, ichiban);
      branch3.captureSpan('のなかで-がいちばん', groupNoun, ichiban);
    },
    // Pattern 4: Noun + の + 中で/なかで (compound word) + ichiban
    (branch4) => {
      const groupNoun = branch4.tok({ posOneOf: ['NOUN', 'PRON', 'PROPN', 'DET', 'NUM'] }, 'groupNoun');
      const no = branch4.particle('の', 'no');
      const nakade = branch4.tok({ textOneOf: ['中で', 'なかで'], posOneOf: ['NOUN', 'ADV', 'ADP'] }, 'nakade');
      const ichiban = branch4.tok({ textOneOf: ['一番', 'いちばん'], posOneOf: ['NOUN', 'ADV'] }, 'ichiban');

      branch4.caseMarker(groupNoun, no);
      branch4.inOrder(no, nakade, 3);
      branch4.inOrder(nakade, ichiban);
      branch4.captureSpan('のなかで-がいちばん', groupNoun, ichiban);
    },
    // Pattern 5: Noun + の + 中/なか + で + itemNoun + は + ichiban (topic marker は)
    (branch5) => {
      const groupNoun = branch5.tok({ posOneOf: ['NOUN', 'PRON', 'PROPN', 'DET', 'NUM'] }, 'groupNoun');
      const no = branch5.particle('の', 'no');
      const naka = branch5.tok({ textOneOf: ['中', 'なか'], posOneOf: ['NOUN', 'PROPN'] }, 'naka');
      const de = branch5.particle('で', 'de');
      const itemNoun = branch5.tok({ posOneOf: ['NOUN', 'PRON', 'PROPN', 'DET'] }, 'itemNoun');
      const topicMarker = branch5.particle('は', 'topicMarker');
      const ichiban = branch5.tok({ textOneOf: ['一番', 'いちばん'], posOneOf: ['NOUN', 'ADV'] }, 'ichiban');

      branch5.caseMarker(groupNoun, no);
      branch5.inOrder(no, naka, 3);
      branch5.caseMarker(naka, de);
      branch5.inOrder(groupNoun, no, 1);
      branch5.inOrder(no, naka, 1);
      branch5.inOrder(naka, de, 1);
      branch5.inOrder(de, itemNoun);
      branch5.inOrder(itemNoun, topicMarker, 1);
      branch5.inOrder(topicMarker, ichiban, 1);
      branch5.captureSpan('のなかで-がいちばん', groupNoun, ichiban);
    },
    // Pattern 6: Multiple nouns connected with と (e.g., AとBとCのなかで)
    (branch6) => {
      const groupNoun1 = branch6.tok({ posOneOf: ['NOUN', 'PROPN'] }, 'groupNoun1');
      const to1 = branch6.particle('と', 'to1');
      const groupNoun2 = branch6.tok({ posOneOf: ['NOUN', 'PROPN'] }, 'groupNoun2');
      const to2 = branch6.particle('と', 'to2');
      const groupNoun3 = branch6.tok({ posOneOf: ['NOUN', 'PROPN'] }, 'groupNoun3');
      const no = branch6.particle('の', 'no');
      const naka = branch6.tok({ textOneOf: ['中', 'なか'], posOneOf: ['NOUN', 'PROPN'] }, 'naka');
      const de = branch6.particle('で', 'de');
      const ichiban = branch6.tok({ textOneOf: ['一番', 'いちばん'], posOneOf: ['NOUN', 'ADV'] }, 'ichiban');

      branch6.inOrder(groupNoun1, to1, 1);
      branch6.inOrder(to1, groupNoun2, 1);
      branch6.inOrder(groupNoun2, to2, 1);
      branch6.inOrder(to2, groupNoun3, 1);
      branch6.inOrder(groupNoun3, no, 1);
      branch6.inOrder(no, naka, 3);
      branch6.caseMarker(naka, de);
      branch6.inOrder(naka, de, 1);
      branch6.inOrder(de, ichiban);
      branch6.captureSpan('のなかで-がいちばん', groupNoun1, ichiban);
    },
    // Pattern 7: Reverse order: ichiban ... この/その + 中/なか + で (conversational pattern)
    (branch7) => {
      const ichiban = branch7.tok({ textOneOf: ['一番', 'いちばん'], posOneOf: ['NOUN', 'ADV'] }, 'ichiban');
      const groupNoun = branch7.tok({ textOneOf: ['この', 'その', 'あの'], pos: 'DET' }, 'groupNoun');
      const naka = branch7.tok({ textOneOf: ['中', 'なか'], posOneOf: ['NOUN', 'PROPN'] }, 'naka');
      const de = branch7.particle('で', 'de');

      branch7.inOrder(ichiban, groupNoun);
      branch7.inOrder(groupNoun, naka, 1);
      branch7.caseMarker(naka, de);
      branch7.inOrder(naka, de, 1);
      branch7.captureSpan('のなかで-がいちばん', groupNoun, de);
    }
  );
});
