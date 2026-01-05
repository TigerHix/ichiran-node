import { bunproLinguisticRule } from '../../../engine/lang.js';

/**
 * JLPT2: て頂戴 (te choudai) - "please do..." / humble "to receive"
 *
 * A casual/familiar request form used among family members, OR a humble form
 * of "to receive" when used in suru-verb compounds.
 *
 * As request: Verb-te form + 頂戴 (ちょうだい, choudai)
 * As humble: 頂戴します (I humbly receive)
 *
 * 頂戴 is a humble form of "to receive" (もらう), but when attached to
 * the te-form of a verb, it functions as a casual request similar to
 * てください but more intimate and sometimes considered more feminine.
 *
 * Structures:
 * - Verb［て-form］+ 頂戴（ちょうだい）- request form
 * - 頂戴（ちょうだい）+ します/しました - humble receive form
 *
 * Examples:
 * - 開けてちょうだい。 (Please open it.)
 * - 買ってきてちょうだい。 (Please buy it and come back.)
 * - とってちょうだい。 (Please get it.)
 * - お時間を頂戴しました。 (I received your time - humble)
 *
 * Key discriminators:
 * - Matches verb-te form + 頂載/ちょうだい
 * - Also matches standalone 頂載/ちょうだい when followed by する/します/しました
 * - Different from てください (more polite)
 * - Excludes 頂戴いたします (humble with いたします, not included in test data)
 *
 * GiNZA parse structure:
 * - 頂戴 can be NOUN or VERB with lemma=頂戴 or ちょうだい
 * - て form can be AUX with text=て or VERB with text ending in て
 */
export default bunproLinguisticRule('て頂戴', (r) => {
  r.either(
    // Branch 1: Te form as AUX particle + 頂戴
    (b) => {
      const te = b.aux({ text: 'て' }, 'te');
      const choudai = b.tok({
        textOneOf: ['頂戴', 'ちょうだい'],
        posOneOf: ['NOUN', 'VERB'],
      }, 'choudai');
      b.inOrder(te, choudai, 3);
      b.captureSpan('て頂戴', te, choudai);
    },

    // Branch 2: Te form as VERB (連用形) + 頂戴
    (b) => {
      const te = b.verb({
        inflectionForm: '連用形-一般',
      }, 'te');
      const choudai = b.tok({
        textOneOf: ['頂戴', 'ちょうだい'],
        posOneOf: ['NOUN', 'VERB'],
      }, 'choudai');
      b.inOrder(te, choudai, 5);
      b.captureSpan('て頂戴', te, choudai);
    },

    // Branch 3: Any token ending in て + 頂戴 (catch-all)
    (b) => {
      const te = b.tok({
        text: /て$/,
        posOneOf: ['VERB', 'AUX'],
      }, 'te');
      const choudai = b.tok({
        textOneOf: ['頂戴', 'ちょうだい'],
        posOneOf: ['NOUN', 'VERB'],
      }, 'choudai');
      b.inOrder(te, choudai, 5);
      b.captureSpan('て頂戴', te, choudai);
    },

    // Branch 4: Standalone 頂戴 + し/した/します/いたします (humble form)
    // Matches: 頂戴しました, 頂戴します, 頂戴した, 頂戴いたします
    (b) => {
      const choudai = b.tok({
        textOneOf: ['頂戴', 'ちょうだい'],
        posOneOf: ['NOUN', 'VERB'],
      }, 'choudai');
      const suru = b.aux({
        lemmaOneOf: ['する', 'いたす'],
        textOneOf: ['し', 'した', 'します', 'しまして', 'いたし', 'いたします'],
      }, 'suru');
      b.inOrder(choudai, suru, 3);
      b.capture(choudai);
    },

    // Branch 5: Standalone 頂戴 at sentence end (like "グミちょうだい！")
    // This must be followed by punctuation or be the last token
    (b) => {
      const choudai = b.tok({
        textOneOf: ['頂戴', 'ちょうだい'],
        posOneOf: ['NOUN', 'VERB'],
      }, 'choudai');
      // Can be followed by PUNCT (sentence end) or SYM (symbols like ♡)
      const end = b.tok({
        posOneOf: ['PUNCT', 'SYM'],
      }, 'end');
      b.inOrder(choudai, end, 2);
      b.capture(choudai);
    }
  );
});
