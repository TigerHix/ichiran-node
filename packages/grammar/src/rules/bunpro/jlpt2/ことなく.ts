import { bunproLinguisticRule } from '../../../engine/lang.js';

/**
 * JLPT2: ことなく (koto naku) - "without doing, never doing"
 *
 * Verb dictionary form + こと + なく = "without doing X"
 *
 * This is a formal/archaic negative form, similar to ずに (zu ni).
 * The こと nominalizes the preceding verb, and なく is the adverbial
 * form of ない (nai - negative).
 *
 * Structure:
 * - Verb (dictionary form) + こと + なく
 *
 * Formation:
 * - よむ (yomu - to read) → よむことなく (without reading)
 * - する (suru - to do) → することなく (without doing)
 * - くる (kuru - to come) → くることなく (without coming)
 * - たべる (taberu - to eat) → たべることなく (without eating)
 *
 * Examples:
 * - 犯人が捕まることなく１０年が経つ。
 *   (Ten years pass without the culprit being caught.)
 * - 遅刻することなく、職場に着いた。
 *   (I was able to arrive at work without being late.)
 * - 彼は社長に何も言うことなく会社を辞めた。
 *   (He quit his job without telling the boss.)
 * - 真実を知ることなく終わる。
 *   (It will end without knowing the truth.)
 * - 遅れることなく到着した。
 *   (Arrived without being late.)
 * - 耕すことなく種を植えても、うまくいくはずがない。
 *   (Without tilling, even if you plant seeds, there is no way it will turn out well.)
 * - うちの子には、散らかすことなく遊ぶという特技があります。
 *   (Our child has a special skill where they can play without leaving things untidy.)
 * - 対立することなく話し合いを終えることができました。
 *   (The discussion was able to come to a close without confrontation.)
 * - 挨拶をしたのに、友人は立ち止まることなく行ってしまった。
 *   (Despite saying hello, my friend went without stopping.)
 * - あの選手は、準備体操をすることなく試合に出るらしい。
 *   (I heard that athlete plays without doing warming up exercises.)
 * - 貴乃花は一門に属することなく引退してしまった。
 *   (Takanohana retired without ever joining an ichimon.)
 * - 耕すことなく種を植えても、うまくいくはずがない。
 *   (Without tilling, even if you plant seeds, there is no way it will turn out well.)
 * - この洗剤を使えば、セーターが縮むことなく洗えるからおすすめですよ。
 *   (If you use this detergent, you can wash a sweater without it shrinking, so I recommend it.)
 * - 彼の長所を知ることなく、別れの時がきてしまった。
 *   (It has come time for us to part ways without knowing his strong points.)
 * - ネットショップなら、直接話すことなく注文できて楽だ。
 *   (If you shop online, you can comfortably order without speaking to anyone directly.)
 * - 何ごとも理解されることなく終わる。
 *   (It will end without understanding anything.)
 *
 * Nuance:
 * - More formal than ないで (naide) or ずに (zuni)
 * - Often used in written language or formal speech
 * - Expresses "without X happening" or "never having done X"
 * - Can express contrast between two actions or states
 * - Similar to ずに but with different grammatical structure
 *
 * Related grammar:
 * - ずに (zuni) - "without doing" (more common, classical auxiliary)
 * - ないで (naide) - "without doing" (casual, modern negative te-form)
 * - ことなしに (kotonashini) - variant with なし instead of なく
 * - こともなく (kotomonaku) - variant with も (also/even) particle
 *
 * GiNZA parse structure:
 * - Verb (any form) + こと(NOUN) + なく(ADV/AUX)
 * - こと is a noun (NOUN) that nominalizes the verb
 * - なく is an adverb (ADV) or auxiliary (AUX) - adverbial form of ない
 * - Various dependency relations (compound, advcl, obl, mark)
 *
 * Key discriminators:
 * - Follows verb in dictionary form (or other forms)
 * - こと is a noun (NOUN) meaning "thing, matter, abstract concept"
 * - なく is the adverbial form of ない (negative)
 * - Expresses negative circumstance or condition
 * - More formal/literary than ないで or ずに
 *
 * Different from:
 * - ないで (naide) - casual negative te-form "without doing"
 * - ずに (zuni) - classical negative auxiliary "without doing"
 * - ことなしに (kotonashini) - same meaning but different form
 */
export default bunproLinguisticRule('ことなく', (r) => {
  r.either(
    // Pattern 1: Verb + こと + なく (most common pattern)
    // なく as adverb (ADV)
    (b1) => {
      const verb = b1.verb({}, 'verb');
      const koto = b1.noun({ text: 'こと', lemma: 'こと' }, 'koto');
      const naku = b1.tok({ text: 'なく', pos: 'ADV' }, 'naku');

      b1.inOrder(verb, koto, 2);
      b1.inOrder(koto, naku, 1);

      b1.captureSpan('ことなく', verb, naku);
    },

    // Pattern 2: Verb + こと + なく (なく as AUX)
    // GiNZA may parse なく as auxiliary in some contexts
    (b2) => {
      const verb = b2.verb({}, 'verb');
      const koto = b2.noun({ text: 'こと', lemma: 'こと' }, 'koto');
      const naku = b2.aux({ text: 'なく', lemma: 'ない' }, 'naku');

      b2.inOrder(verb, koto, 2);
      b2.inOrder(koto, naku, 1);

      b2.captureSpan('ことなく', verb, naku);
    },

    // Pattern 3: Verb + こと + なく (looser, any POS for なく)
    // Catch-all for unexpected GiNZA parsings
    (b3) => {
      const verb = b3.verb({}, 'verb');
      const koto = b3.noun({ text: 'こと', lemma: 'こと' }, 'koto');
      const naku = b3.tok({ text: 'なく' }, 'naku');

      b3.inOrder(verb, koto, 2);
      b3.inOrder(koto, naku, 1);

      b3.captureSpan('ことなく', verb, naku);
    },

    // Pattern 4: Verb + こと + も + なく (with も particle)
    // Variant: ことなく (kotomonaku) - "also without doing, even without doing"
    (b4) => {
      const verb = b4.verb({}, 'verb');
      const koto = b4.noun({ text: 'こと', lemma: 'こと' }, 'koto');
      const mo = b4.particle('も', 'mo');
      const naku = b4.tok({ text: 'なく' }, 'naku');

      b4.inOrder(verb, koto, 2);
      b4.inOrder(koto, mo, 1);
      b4.inOrder(mo, naku, 1);

      b4.captureSpan('ことなく', verb, naku);
    },

    // Pattern 5: Any token + こと + なく (very loose pattern)
    // For cases where the preceding element isn't clearly a verb
    (b5) => {
      const prev = b5.tok({}, 'prev');
      const koto = b5.noun({ text: 'こと', lemma: 'こと' }, 'koto');
      const naku = b5.tok({ text: 'なく' }, 'naku');

      b5.inOrder(prev, koto, 2);
      b5.inOrder(koto, naku, 1);

      b5.captureSpan('ことなく', prev, naku);
    }
  );
});
