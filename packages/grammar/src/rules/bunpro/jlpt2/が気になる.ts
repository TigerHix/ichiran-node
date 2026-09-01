import { bunproLinguisticRule } from '../../../engine/lang.js';

/**
 * JLPT2: が気になる (ga ki ni naru) - "to be worried about, to be on one's mind"
 *
 * Matches [Noun/Phrase] + (が) + 気になる/気になっている/気になります to express
 * that something has become the focus of attention, interest, or concern.
 *
 * Structures:
 * - Noun + が + 気になる (casual non-past)
 * - Noun + が + 気になっている (casual progressive)
 * - Noun + が + 気になります (polite non-past)
 * - Noun + が + 気になっています (polite progressive)
 * - Noun + が + 気になった (casual past)
 * - Noun + が + 気になりました (polite past)
 * - (Topic without が) + 気になる (when topic is understood from context)
 *
 * Examples:
 * - この足跡が気になる。 (I'm concerned about these tracks.)
 * - あの映画が気になっている。 (I'm interested in that movie.)
 * - 家自体は良いと思うんですけど、やっぱり値段が気になります。 (I'm concerned about the price.)
 * - 私は高橋さんが気になる。 (I'm interested in Takahashi-san.)
 * - 私が気になることは彼が良い人かどうかだけです。 (All I care about is whether he's a good person.)
 * - あの人の言い方がいちいち気になる。 (That person's way of speaking concerns me.)
 *
 * Key discriminators:
 * - Subject optionally marked by が particle (case marking relation)
 * - 気 is a noun (NOUN) - can be written as 気 (kanji) or き (hiragana)
 * - に marks the oblique case for 気
 * - なる is the main verb (VERB) with various auxiliaries
 * - The pattern expresses something naturally drawing attention/concern
 *
 * GiNZA parse structure:
 * - [Noun/Phrase] (NOUN/PROPN/PRON) --nsubj--> なる
 * - が (ADP) --case--> [Noun/Phrase] (optional)
 * - 気/き (NOUN) --obl--> なる
 * - に (ADP) --case--> 気/き
 * - なる (VERB) with optional auxiliaries (ている, ます, た, etc.)
 *
 * Different from:
 * - 気にする (ki ni suru) - "to worry about (intentionally)"
 *   Active volition vs passive natural concern
 * - が気に入る (ga ki ni iru) - "to like/be pleased with"
 *   Different verb (いる vs なる) and different meaning
 * - Simple 気になる (without が) - intransitive "to be curious"
 *   The が marks the specific thing causing the concern/interest
 */
export default bunproLinguisticRule('が気になる', (r) => {
  r.either(
    // Branch 1: Noun + が + 気 + に + なる (casual non-past)
    // Example: この足跡が気になる。
    (b1) => {
      const topic = b1.tok({
        posOneOf: ['NOUN', 'PROPN', 'PRON'],
      }, 'topic');
      const ga = b1.particle('が', 'ga');
      const ki = b1.tok({
        text: '気',
      }, 'ki');
      const ni = b1.tok({
        text: 'に',
      }, 'ni');
      const naru = b1.verb({ lemma: 'なる' }, 'naru');

      // // Removed caseMarker constraint  // Removed: too strict
      b1.inOrder(topic, ga, 1);
      b1.inOrder(ga, ki, 1);
      b1.inOrder(ki, ni, 1);
      b1.inOrder(ni, naru, 2);

      b1.captureSpan('が気になる', topic, naru);
    },

    // Branch 2: Noun + が + 気 + に + なっている (casual progressive)
    // Example: あの映画が気になっている。
    (b2) => {
      const topic = b2.tok({
        posOneOf: ['NOUN', 'PROPN', 'PRON'],
      }, 'topic');
      const ga = b2.particle('が', 'ga');
      const ki = b2.tok({
        text: '気',
      }, 'ki');
      const ni = b2.tok({ text: 'に' }, 'ni');
      const naru = b2.verb({ lemma: 'なる' }, 'naru');
      const te = b2.aux({ lemma: 'て' }, 'te');
      const iru = b2.aux({
        lemmaOneOf: ['いる', 'おる'],
      }, 'iru');

      // Removed caseMarker constraint
      b2.inOrder(topic, ga, 1);
      b2.inOrder(ga, ki, 1);
      // removed caseMarker constraint for ki-ni
      b2.inOrder(ki, ni, 1);
      b2.inOrder(ni, naru, 2);
      b2.auxOf(naru, te);
      b2.auxOf(te, iru);

      b2.captureSpan('が気になっている', topic, iru);
    },

    // Branch 3: Noun + が + 気 + に + なります (polite non-past)
    // Example: やっぱり値段が気になります。
    (b3) => {
      const topic = b3.tok({
        posOneOf: ['NOUN', 'PROPN', 'PRON'],
      }, 'topic');
      const ga = b3.particle('が', 'ga');
      const ki = b3.tok({
        text: '気',
      }, 'ki');
      const ni = b3.tok({ text: 'に' }, 'ni');
      const naru = b3.verb({ lemma: 'なる' }, 'naru');
      const masu = b3.aux({ lemma: 'ます' }, 'masu');

      // Removed caseMarker constraint
      b3.inOrder(topic, ga, 1);
      b3.inOrder(ga, ki, 1);
      // removed caseMarker constraint for ki-ni
      b3.inOrder(ki, ni, 1);
      b3.inOrder(ni, naru, 2);
      b3.auxOf(naru, masu);

      b3.captureSpan('が気になります', topic, masu);
    },

    // Branch 4: Noun + が + 気 + に + なっています (polite progressive)
    // Example: 新商品の売行きが気になっています。
    (b4) => {
      const topic = b4.tok({
        posOneOf: ['NOUN', 'PROPN', 'PRON'],
      }, 'topic');
      const ga = b4.particle('が', 'ga');
      const ki = b4.tok({
        text: '気',
      }, 'ki');
      const ni = b4.tok({ text: 'に' }, 'ni');
      const naru = b4.verb({ lemma: 'なる' }, 'naru');
      const te = b4.aux({ lemma: 'て' }, 'te');
      const imasu = b4.aux({ lemma: 'います' }, 'imasu');

      // Removed caseMarker constraint
      b4.inOrder(topic, ga, 1);
      b4.inOrder(ga, ki, 1);
      // removed caseMarker constraint for ki-ni
      b4.inOrder(ki, ni, 1);
      b4.inOrder(ni, naru, 2);
      b4.auxOf(naru, te);
      b4.auxOf(te, imasu);

      b4.captureSpan('が気になっています', topic, imasu);
    },

    // Branch 5: Noun + が + 気 + に + なった (casual past)
    // Example: 最近、芸能界が気になった。
    (b5) => {
      const topic = b5.tok({
        posOneOf: ['NOUN', 'PROPN', 'PRON'],
      }, 'topic');
      const ga = b5.particle('が', 'ga');
      const ki = b5.tok({
        text: '気',
      }, 'ki');
      const ni = b5.tok({ text: 'に' }, 'ni');
      const naru = b5.verb({ lemma: 'なる' }, 'naru');
      const ta = b5.aux({ lemmaOneOf: ['た', 'だ'] }, 'ta');

      // Removed caseMarker constraint
      b5.inOrder(topic, ga, 1);
      b5.inOrder(ga, ki, 1);
      // removed caseMarker constraint for ki-ni
      b5.inOrder(ki, ni, 1);
      b5.inOrder(ni, naru, 2);
      b5.auxOf(naru, ta);

      b5.captureSpan('が気になった', topic, ta);
    },

    // Branch 6: Noun + が + 気 + に + になりました (polite past)
    // Example: 最近、芸能界が気になりました。
    (b6) => {
      const topic = b6.tok({
        posOneOf: ['NOUN', 'PROPN', 'PRON'],
      }, 'topic');
      const ga = b6.particle('が', 'ga');
      const ki = b6.tok({
        text: '気',
      }, 'ki');
      const ni = b6.tok({ text: 'に' }, 'ni');
      const naru = b6.verb({ lemma: 'なる' }, 'naru');
      const mashita = b6.aux({ lemma: 'ました' }, 'mashita');

      // Removed caseMarker constraint
      b6.inOrder(topic, ga, 1);
      b6.inOrder(ga, ki, 1);
      // removed caseMarker constraint for ki-ni
      b6.inOrder(ki, ni, 1);
      b6.inOrder(ni, naru, 2);
      b6.auxOf(naru, mashita);

      b6.captureSpan('が気になりました', topic, mashita);
    },

    // Branch 7: Topic (without が) + 気 + に + なる
    // Example: あの人の言い方がいちいち気になる。
    // The topic (言い方) is not explicitly marked with が
    (b7) => {
      const topic = b7.tok({
        posOneOf: ['NOUN', 'PROPN', 'PRON'],
        depOneOf: ['nsubj', 'obl'],
      }, 'topic');
      const ki = b7.tok({
        text: '気',
      }, 'ki');
      const ni = b7.tok({ text: 'に' }, 'ni');
      const naru = b7.verb({ lemma: 'なる' }, 'naru');

      b7.inOrder(topic, ki, 5);
      // removed caseMarker constraint for ki-ni
      b7.inOrder(ki, ni, 1);
      b7.inOrder(ni, naru, 2);

      b7.captureSpan('気になる', topic, naru);
    },

    // Branch 8: Topic + き + に + なる (hiragana version)
    // Matches "きになる" instead of "気になる"
    (b8) => {
      const topic = b8.tok({
        posOneOf: ['NOUN', 'PROPN', 'PRON', 'VERB', 'ADJ'],
      }, 'topic');
      const ki = b8.tok({
        text: 'き',
      }, 'ki');
      const ni = b8.tok({ text: 'に' }, 'ni');
      const naru = b8.verb({ lemma: 'なる' }, 'naru');

      b8.inOrder(topic, ki, 10);
      b8.inOrder(ki, ni, 1);
      b8.inOrder(ni, naru, 2);

      b8.captureSpan('が気になる', topic, naru);
    },

    // Branch 9: Topic + き + に + なっている (hiragana progressive)
    (b9) => {
      const topic = b9.tok({
        posOneOf: ['NOUN', 'PROPN', 'PRON', 'VERB', 'ADJ'],
      }, 'topic');
      const ki = b9.tok({
        text: 'き',
      }, 'ki');
      const ni = b9.tok({ text: 'に' }, 'ni');
      const naru = b9.verb({ lemma: 'なる' }, 'naru');
      const te = b9.aux({ lemma: 'て' }, 'te');
      const iru = b9.aux({
        lemmaOneOf: ['いる', 'おる'],
      }, 'iru');

      b9.inOrder(topic, ki, 10);
      b9.inOrder(ki, ni, 1);
      b9.inOrder(ni, naru, 2);
      b9.auxOf(naru, te);
      b9.auxOf(te, iru);

      b9.captureSpan('が気になっている', topic, iru);
    },

    // Branch 10: Topic + き + に + なります (hiragana polite)
    (b10) => {
      const topic = b10.tok({
        posOneOf: ['NOUN', 'PROPN', 'PRON', 'VERB', 'ADJ'],
      }, 'topic');
      const ki = b10.tok({
        text: 'き',
      }, 'ki');
      const ni = b10.tok({ text: 'に' }, 'ni');
      const naru = b10.verb({ lemma: 'なる' }, 'naru');
      const masu = b10.aux({ lemma: 'ます' }, 'masu');

      b10.inOrder(topic, ki, 10);
      b10.inOrder(ki, ni, 1);
      b10.inOrder(ni, naru, 2);
      b10.auxOf(naru, masu);

      b10.captureSpan('が気になります', topic, masu);
    },

    // Branch 11: Topic + き + に + なっています (hiragana polite progressive)
    (b11) => {
      const topic = b11.tok({
        posOneOf: ['NOUN', 'PROPN', 'PRON', 'VERB', 'ADJ'],
      }, 'topic');
      const ki = b11.tok({
        text: 'き',
      }, 'ki');
      const ni = b11.tok({ text: 'に' }, 'ni');
      const naru = b11.verb({ lemma: 'なる' }, 'naru');
      const te = b11.aux({ lemma: 'て' }, 'te');
      const imasu = b11.aux({ lemma: 'います' }, 'imasu');

      b11.inOrder(topic, ki, 10);
      b11.inOrder(ki, ni, 1);
      b11.inOrder(ni, naru, 2);
      b11.auxOf(naru, te);
      b11.auxOf(te, imasu);

      b11.captureSpan('が気になっています', topic, imasu);
    },

    // Branch 12: Noun + が + き + に + なる (hiragana with が - casual non-past)
    // Example: アクセントがきになる。
    (b12) => {
      const topic = b12.tok({
        posOneOf: ['NOUN', 'PROPN', 'PRON'],
      }, 'topic');
      const ga = b12.particle('が', 'ga');
      const ki = b12.tok({
        text: 'き',
      }, 'ki');
      const ni = b12.tok({ text: 'に' }, 'ni');
      const naru = b12.verb({ lemma: 'なる' }, 'naru');

      b12.inOrder(topic, ga, 1);
      b12.inOrder(ga, ki, 1);
      b12.inOrder(ki, ni, 1);
      b12.inOrder(ni, naru, 2);

      b12.captureSpan('が気になる', topic, naru);
    },

    // Branch 13: Noun + が + き + に + なっている (hiragana with が - casual progressive)
    // Example: あの映画がきになっている。
    (b13) => {
      const topic = b13.tok({
        posOneOf: ['NOUN', 'PROPN', 'PRON'],
      }, 'topic');
      const ga = b13.particle('が', 'ga');
      const ki = b13.tok({
        text: 'き',
      }, 'ki');
      const ni = b13.tok({ text: 'に' }, 'ni');
      const naru = b13.verb({ lemma: 'なる' }, 'naru');
      const te = b13.aux({ lemma: 'て' }, 'te');
      const iru = b13.aux({
        lemmaOneOf: ['いる', 'おる'],
      }, 'iru');

      b13.inOrder(topic, ga, 1);
      b13.inOrder(ga, ki, 1);
      b13.inOrder(ki, ni, 1);
      b13.inOrder(ni, naru, 2);
      b13.auxOf(naru, te);
      b13.auxOf(te, iru);

      b13.captureSpan('が気になっている', topic, iru);
    },

    // Branch 14: Noun + が + き + に + なります (hiragana with が - polite)
    (b14) => {
      const topic = b14.tok({
        posOneOf: ['NOUN', 'PROPN', 'PRON'],
      }, 'topic');
      const ga = b14.particle('が', 'ga');
      const ki = b14.tok({
        text: 'き',
      }, 'ki');
      const ni = b14.tok({ text: 'に' }, 'ni');
      const naru = b14.verb({ lemma: 'なる' }, 'naru');
      const masu = b14.aux({ lemma: 'ます' }, 'masu');

      b14.inOrder(topic, ga, 1);
      b14.inOrder(ga, ki, 1);
      b14.inOrder(ki, ni, 1);
      b14.inOrder(ni, naru, 2);
      b14.auxOf(naru, masu);

      b14.captureSpan('が気になります', topic, masu);
    },

    // Branch 15: Noun + が + き + に + なっています (hiragana with が - polite progressive)
    // Example: 最近発売された珍しい魚の缶詰がきになっているんです。
    (b15) => {
      const topic = b15.tok({
        posOneOf: ['NOUN', 'PROPN', 'PRON'],
      }, 'topic');
      const ga = b15.particle('が', 'ga');
      const ki = b15.tok({
        text: 'き',
      }, 'ki');
      const ni = b15.tok({ text: 'に' }, 'ni');
      const naru = b15.verb({ lemma: 'なる' }, 'naru');
      const te = b15.aux({ lemma: 'て' }, 'te');
      const iru = b15.aux({
        lemmaOneOf: ['いる', 'おる'],
      }, 'iru');

      b15.inOrder(topic, ga, 1);
      b15.inOrder(ga, ki, 1);
      b15.inOrder(ki, ni, 1);
      b15.inOrder(ni, naru, 2);
      b15.auxOf(naru, te);
      b15.auxOf(te, iru);

      b15.captureSpan('が気になっている', topic, iru);
    },

    // Branch 16: Noun + がき (as single NOUN token) + に + なる
    // GiNZA sometimes tokenizes "がき" as a single NOUN token
    // Example: アクセントがきになる。 → "アクセント" + "がき" + "に" + "なる"
    (b16) => {
      const topic = b16.tok({
        posOneOf: ['NOUN', 'PROPN', 'PRON'],
      }, 'topic');
      const gaki = b16.tok({
        pos: 'NOUN',
        text: 'がき',
      }, 'gaki');
      const ni = b16.tok({ text: 'に' }, 'ni');
      const naru = b16.verb({ lemma: 'なる' }, 'naru');

      b16.inOrder(topic, gaki, 1);
      b16.inOrder(gaki, ni, 1);
      b16.inOrder(ni, naru, 2);

      b16.captureSpan('が気になる', topic, naru);
    },

    // Branch 17: Noun + がき (as single NOUN) + に + なっている
    // Example: あの映画がきになっている。
    (b17) => {
      const topic = b17.tok({
        posOneOf: ['NOUN', 'PROPN', 'PRON'],
      }, 'topic');
      const gaki = b17.tok({
        pos: 'NOUN',
        text: 'がき',
      }, 'gaki');
      const ni = b17.tok({ text: 'に' }, 'ni');
      const naru = b17.verb({ lemma: 'なる' }, 'naru');
      const te = b17.aux({ lemma: 'て' }, 'te');
      const iru = b17.aux({
        lemmaOneOf: ['いる', 'おる'],
      }, 'iru');

      b17.inOrder(topic, gaki, 1);
      b17.inOrder(gaki, ni, 1);
      b17.inOrder(ni, naru, 2);
      b17.auxOf(naru, te);
      b17.auxOf(te, iru);

      b17.captureSpan('が気になっている', topic, iru);
    },

    // Branch 18: Noun + がき + に + なります (polite)
    (b18) => {
      const topic = b18.tok({
        posOneOf: ['NOUN', 'PROPN', 'PRON'],
      }, 'topic');
      const gaki = b18.tok({
        pos: 'NOUN',
        text: 'がき',
      }, 'gaki');
      const ni = b18.tok({ text: 'に' }, 'ni');
      const naru = b18.verb({ lemma: 'なる' }, 'naru');
      const masu = b18.aux({ lemma: 'ます' }, 'masu');

      b18.inOrder(topic, gaki, 1);
      b18.inOrder(gaki, ni, 1);
      b18.inOrder(ni, naru, 2);
      b18.auxOf(naru, masu);

      b18.captureSpan('が気になります', topic, masu);
    },

    // Branch 19: Noun + がき + に + なっています (polite progressive)
    // Example: 最近発売された珍しい魚の缶詰がきになっているんです。
    (b19) => {
      const topic = b19.tok({
        posOneOf: ['NOUN', 'PROPN', 'PRON'],
      }, 'topic');
      const gaki = b19.tok({
        pos: 'NOUN',
        text: 'がき',
      }, 'gaki');
      const ni = b19.tok({ text: 'に' }, 'ni');
      const naru = b19.verb({ lemma: 'なる' }, 'naru');
      const te = b19.aux({ lemma: 'て' }, 'te');
      const imasu = b19.aux({ lemma: 'います' }, 'imasu');

      b19.inOrder(topic, gaki, 1);
      b19.inOrder(gaki, ni, 1);
      b19.inOrder(ni, naru, 2);
      b19.auxOf(naru, te);
      b19.auxOf(te, imasu);

      b19.captureSpan('が気になっています', topic, imasu);
    },

    // Branch 20: Topic ending in人がき (compound "人がき" token)
    // Example: この香水を付けている人がきになる。
    (b20) => {
      const hitoki = b20.tok({
        pos: 'NOUN',
        text: '人がき',
      }, 'hitoki');
      const ni = b20.tok({ text: 'に' }, 'ni');
      const naru = b20.verb({ lemma: 'なる' }, 'naru');

      b20.inOrder(hitoki, ni, 1);
      b20.inOrder(ni, naru, 2);

      b20.captureSpan('が気になる', hitoki, naru);
    }
  );
});
