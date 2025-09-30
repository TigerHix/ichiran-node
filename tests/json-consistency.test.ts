// JSON Consistency tests - ported from tests.lisp
import { describe, test, expect } from 'bun:test';
import { wordInfoGlossJson, wordInfoFromText } from '../src/dict.js';
import { setupTests } from './test-setup.js';

setupTests();

describe('JSON Consistency Tests', () => {
  const testWords = [
    "の", "赤かったろう", "書いてきてる", "捩じり鉢巻きで",
    "夕べ", "さくや", "建ち並ばなきゃ", "建ち並びましてる",
    "どおりで", "十万三", "5万100", "1234",
    "1日", "2期", "三羽", "1万500円", "オレら"
  ];

  for (const word of testWords) {
    test(`"${word}" - gloss JSON consistent through serialization round-trip`, async () => {
      // Faithful port of Lisp test (lines 606-614 in tests.lisp):
      // (loop for word in '(...)
      //   for word-info = (word-info-from-text word)
      //   for word-info-json = (word-info-json word-info)
      //   do (assert-equal (word-info-gloss-json word-info) (word-info-gloss-json word-info-json)))

      // 1. Get WordInfo from text (single best segmentation)
      const wordInfo = await wordInfoFromText(word);

      // 2. Serialize to basic JSON (like Lisp's word-info-json)
      const wordInfoJson = wordInfo.toJSON();

      // 3. Get gloss JSON from original WordInfo object
      const glossFromObject = await wordInfoGlossJson(wordInfo);

      // 4. Get gloss JSON from plain JSON object (duck-typed)
      const glossFromJson = await wordInfoGlossJson(wordInfoJson);

      // 5. Both should produce identical gloss data
      expect(glossFromJson).toEqual(glossFromObject);
    });
  }
});