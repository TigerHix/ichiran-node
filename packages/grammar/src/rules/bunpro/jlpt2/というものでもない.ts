import { linguisticRule } from '../../../engine/lang.js';

/**
 * JLPT2: というものでもない (to iu mono demo nai) - Not necessarily the case
 *
 * A grammar pattern meaning "not necessarily" or "not always the case".
 * It expresses that while something might appear to be true or is often
 * thought to be true, it's not an absolute rule. This is a formal way
 * to say "there's no guarantee that..." or "it's not necessarily true that...".
 *
 * Structure:
 * - Verb + というものでもない
 * - I-adjective + というものでもない
 * - Na-adjective + (だ) + というものでもない
 * - Noun + (だ) + というものでもない
 *
 * The negative can also be:
 * - というものではない (slightly less formal)
 * - というものではありません (polite)
 * - というものでもありません (polite)
 * - というものじゃない (casual)
 *
 * Examples:
 * - 図鑑にはすべての生き物が載っているというものでもない。
 *   (It's not necessarily true that all living creatures are listed in the encyclopedia.)
 * - 洋服は高ければいいというものでもない。
 *   (It's not necessarily true that clothes are better if they're expensive.)
 * - いい大学を出たからといって、いい会社に入社できるというものでもない。
 *   (Just because you graduated from a good university doesn't necessarily mean you can get into a good company.)
 *
 * Key discriminators:
 * - The pattern という + もの + でも + ない must appear together
 * - という is the quotative particle combination
 * - もの is a noun meaning "thing/abstract concept"
 * - でも is a particle meaning "even/not even"
 * - ない (or ありません/じゃない) is the negation
 * - The entire pattern attaches to the preceding predicate
 *
 * Different from:
 * - というわけではない (different nuance - counters an assumption)
 * - わけではない (direct denial of assumption)
 * - とは限らない (not necessarily - less formal)
 * - ことにはならない (doesn't mean that - weaker)
 *
 * GiNZA parse structure:
 * - と as ADP (助詞-格助詞)
 * - いう as VERB (動詞-一般)
 * - もの as NOUN (名詞-普通名詞-サ変可能)
 * - で as AUX (助動詞, lemma=だ)
 * - も as ADP (助詞-係助詞)
 * - ない as ADJ (形容詞-非自立可能)
 */
export default linguisticRule('というものでもない', (r) => {
  r.either(
    // Pattern 1: と (ADP) + いう (VERB) + もの (NOUN) + で (AUX) + も (ADP) + ない (ADJ)
    // Standard GiNZA tokenization for というものでもない
    (b1) => {
      const to = b1.tok({ text: 'と' }, 'to');
      const iu = b1.tok({ text: 'いう' }, 'iu');
      const mono = b1.tok({ text: 'もの' }, 'mono');
      const de = b1.tok({ text: 'で' }, 'de');
      const mo = b1.tok({ text: 'も' }, 'mo');
      const nai = b1.tok({ text: 'ない' }, 'nai');

      b1.inOrder(to, iu, 1);
      b1.inOrder(iu, mono, 2);
      b1.inOrder(mono, de, 2);
      b1.inOrder(de, mo, 1);
      b1.inOrder(mo, nai, 1);

      b1.captureSpan('というものでもない', to, nai);
    },

    // Pattern 2: と + いう + もの + で + も + なかった (past tense)
    (b2) => {
      const to = b2.tok({ text: 'と' }, 'to');
      const iu = b2.tok({ text: 'いう' }, 'iu');
      const mono = b2.tok({ text: 'もの' }, 'mono');
      const de = b2.tok({ text: 'で' }, 'de');
      const mo = b2.tok({ text: 'も' }, 'mo');
      const nakat = b2.tok({ text: 'なかっ' }, 'nakat');
      const ta = b2.tok({ text: 'た' }, 'ta');

      b2.inOrder(to, iu, 1);
      b2.inOrder(iu, mono, 2);
      b2.inOrder(mono, de, 2);
      b2.inOrder(de, mo, 1);
      b2.inOrder(mo, nakat, 1);
      b2.inOrder(nakat, ta, 1);

      b2.captureSpan('というものでもない', to, ta);
    },

    // Pattern 3: Combined という + もの + で + も + ない
    // When と and いう are combined (less common)
    (b3) => {
      const toiu = b3.tok({ text: 'という' }, 'toiu');
      const mono = b3.tok({ text: 'もの' }, 'mono');
      const de = b3.tok({ text: 'で' }, 'de');
      const mo = b3.tok({ text: 'も' }, 'mo');
      const nai = b3.tok({ text: 'ない' }, 'nai');

      b3.inOrder(toiu, mono, 2);
      b3.inOrder(mono, de, 2);
      b3.inOrder(de, mo, 1);
      b3.inOrder(mo, nai, 1);

      b3.captureSpan('というものでもない', toiu, nai);
    },

    // Pattern 4: Full combined token というものでもない
    // When the entire phrase is tokenized as one unit
    (b4) => {
      const pattern = b4.tok({
        textOneOf: [
          'というものでもない',
          'というものではない',
          'というものじゃない'
        ]
      }, 'pattern');

      b4.capture(pattern);
    },

    // Pattern 5: Permissive - allows POS variations
    (b5) => {
      const to = b5.tok({ text: 'と' }, 'to');
      const iu = b5.tok({ text: 'いう' }, 'iu');
      const mono = b5.tok({ text: 'もの' }, 'mono');
      const de = b5.tok({ text: 'で' }, 'de');
      const mo = b5.tok({ text: 'も' }, 'mo');
      const nai = b5.tok({ textOneOf: ['ない', 'なかっ'] }, 'nai');

      b5.inOrder(to, iu, 2);
      b5.inOrder(iu, mono, 3);
      b5.inOrder(mono, de, 3);
      b5.inOrder(de, mo, 2);
      b5.inOrder(mo, nai, 2);

      b5.captureSpan('というものでもない', to, nai);
    }
  );
});
