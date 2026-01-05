import { linguisticRule } from '../../../engine/lang.js';

/**
 * JLPT2: てからでないと (te kara denai to) - Not until... cannot
 *
 * A grammar pattern meaning "not until (A) is done, (B) cannot happen" or
 * "unless (A) is done, (B) is impossible". It emphasizes that action (B)
 * cannot occur until action (A) is completed.
 *
 * Structure:
 * - Verb-te form + から + で + ない + と (conditional)
 * - Verb-te form + から + で + ない + ば (conditional variant)
 *
 * Variants:
 * - てからでないと (most common)
 * - てからでなければ (conditional ba-form)
 *
 * Examples:
 * - 書類が揃ってからでないと、申し込みができません。
 *   (You can't apply unless all the documents are ready.)
 * - アプリをインストールしてからでないと、音楽は聴けません。
 *   (You can't listen to music unless you install the app.)
 * - 冷めてからでないと飲めない。
 *   (You can't drink it unless it cools down.)
 * - 資格をとってからでなければ、面接が受けられません。
 *   (You can't be interviewed unless you get the qualification.)
 *
 * Key discriminators:
 * - Follows verb-te form (連用形 with て)
 * - から is the source particle (ADP, case dep)
 * - で is auxiliary form of だ (AUX, lemma=だ, aux dep)
 * - ない is negative auxiliary (AUX, lemma=ない, fixed dep)
 * - と is conditional particle (SCONJ/ADP, mark dep)
 * - ば is conditional particle (SCONJ, mark dep)
 *
 * GiNZA parse structure:
 * - Verb in te-form (連用形-促音便 or 連用形-一般)
 * - て as SCONJ (mark dep)
 * - から as ADP (case dep)
 * - で as AUX with lemma=だ (aux or cop dep)
 * - ない as AUX with lemma=ない (fixed dep)
 * - と as SCONJ or ADP (mark dep) - for でないと
 * - ば as SCONJ (mark dep) - for でなければ
 *
 * Different from:
 * - てから (after doing) - without でないと/でなければ
 * - ないと (if not) - without てからで
 * - からして (judging from) - different pattern
 * - からすると (from the standpoint of) - different pattern
 */
export default linguisticRule('てからでないと', (r) => {
  r.either(
    // Pattern 1: Verb-te + から + で + ない + と (full pattern, split tokens)
    // e.g., 揃ってからでないと、してからでないと、入ってからでないと
    (b1) => {
      const verb = b1.verb({}, 'verb');
      const te = b1.tok({ text: 'て', pos: 'SCONJ' }, 'te');
      const kara = b1.particle('から', 'kara');
      const de = b1.aux({ lemma: 'だ' }, 'de');
      const nai = b1.aux({ lemma: 'ない' }, 'nai');
      const to = b1.tok({ text: 'と', posOneOf: ['SCONJ', 'ADP'] }, 'to');

      b1.inOrder(verb, te, 5);
      b1.inOrder(te, kara, 2);
      b1.inOrder(kara, de, 2);
      b1.inOrder(de, nai, 2);
      b1.inOrder(nai, to, 2);

      b1.captureSpan('てからでないと', verb, to);
    },

    // Pattern 2: Verb-te + から + で + ない + ば (conditional ba-form)
    // e.g., とってからでなければ、なってからでなければ
    (b2) => {
      const verb = b2.verb({}, 'verb');
      const te = b2.tok({ text: 'て', pos: 'SCONJ' }, 'te');
      const kara = b2.particle('から', 'kara');
      const de = b2.aux({ lemma: 'だ' }, 'de');
      const nakere = b2.tok({ lemma: 'ない', inflectionForm: '仮定形-一般' }, 'nakere');
      const ba = b2.tok({ text: 'ば', pos: 'SCONJ' }, 'ba');

      b2.inOrder(verb, te, 5);
      b2.inOrder(te, kara, 2);
      b2.inOrder(kara, de, 2);
      b2.inOrder(de, nakere, 2);
      b2.inOrder(nakere, ba, 2);

      b2.captureSpan('てからでないと', verb, ba);
    },

    // Pattern 3: More flexible - verb + te + kara + de/nai + to/ba (handles variations)
    // Allows different POS tags and dependencies
    (b3) => {
      const verb = b3.verb({}, 'verb');
      const te = b3.tok({ text: 'て' }, 'te');
      const kara = b3.tok({ text: 'から' }, 'kara');
      const de = b3.aux({ lemma: 'だ' }, 'de');
      const nai = b3.aux({ lemma: 'ない' }, 'nai');
      const final = b3.tok({ textOneOf: ['と', 'ば'] }, 'final');

      b3.inOrder(verb, te, 5);
      b3.inOrder(te, kara, 3);
      b3.inOrder(kara, de, 3);
      b3.inOrder(de, nai, 3);
      b3.inOrder(nai, final, 3);

      b3.captureSpan('てからでないと', verb, final);
    },

    // Pattern 4: Even more flexible - handles edge cases in tokenization
    (b4) => {
      const verb = b4.tok({ pos: 'VERB' }, 'verb');
      const te = b4.tok({ text: 'て' }, 'te');
      const kara = b4.tok({ text: 'から' }, 'kara');
      const de = b4.tok({ text: 'で', lemmaOneOf: ['だ', 'ある'] }, 'de');
      const nai = b4.tok({ text: 'ない' }, 'nai');
      const final = b4.tok({ textOneOf: ['と', 'ば'] }, 'final');

      b4.inOrder(verb, te, 6);
      b4.inOrder(te, kara, 4);
      b4.inOrder(kara, de, 4);
      b4.inOrder(de, nai, 4);
      b4.inOrder(nai, final, 4);

      b4.captureSpan('てからでないと', verb, final);
    }
  );
});
