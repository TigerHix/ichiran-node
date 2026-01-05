import { linguisticRule } from '../../../engine/lang.js';

/**
 * JLPT2: という点から考えると (to iu ten kara kangaeru to) - From the standpoint of
 *
 * A grammar pattern meaning "from the perspective of" or "when considered from the
 * point of view of". It indicates that something is being considered from a specific
 * aspect or standpoint.
 *
 * Structure:
 * - Noun + という + 点 + から + 考える + と
 * - Noun + の + 点 + から + 考える + と
 * - (Modifier) + 点 + から + 考える + と
 *
 * Examples:
 * - 日本のデジタル化が遅れているという点から考えると、デジタル人材の育成が必要だ。
 *   (From the standpoint of Japan's digitalization lagging behind, digital workforce development is needed.)
 * - 子供の教育という点から考えると、楽しく勉強をさせる事が重要です。
 *   (From the standpoint of educating children, it is important to make learning fun.)
 * - 健康の点から考えると休む時はちゃんと休まなければ身体を壊す可能性が高まります。
 *   (From the standpoint of health, if you don't rest when you need to rest, the possibility of physical damage increases.)
 * - そうした点から考えると正しいスペルや文法を学ぶことが必要ではないと見える。
 *   (From such a perspective, it seems that there is no need to learn proper spelling or grammar.)
 *
 * Key discriminators:
 * - Follows a noun phrase or clause
 * - という (toiu) is the quotative/attributive form of 言う
 * - 点 (ten) means "point" or "aspect" - can be kanji or hiragana (てん)
 * - から (kara) is the source/starting point marker
 * - 考える (kangaeru) is the verb "to think/consider" - can be kanji or hiragana (かんがえる)
 *   - GiNZA uses lemma=かんがえる (hiragana) for both forms
 * - と (to) is the conditional particle
 * - The entire phrase forms an adverbial clause
 *
 * GiNZA parse structure:
 * - Various tokenizations of という (single token or split)
 * - 点/てん as NOUN with lemma=点
 * - から as ADP/particle
 * - 考える/かんがえる as VERB with lemma=かんがえる
 * - と as ADP/particle with dep=mark
 *
 * Different from:
 * - から alone (because/from)
 * - と言う (called/say - simple quotation)
 * - 点 alone (point/dot)
 * - 考える alone (to think)
 */
export default linguisticRule('という点から考えると', (r) => {
  r.either(
    // Pattern 1: Noun + という + 点 + から + 考える + と
    // e.g., デジタル化が遅れているという点から考えると
    //       子供の教育という点から考えると
    (b1) => {
      const noun = b1.tok({ posOneOf: ['NOUN', 'PROPN', 'PRON', 'VERB', 'ADJ'] }, 'noun');
      const toiu = b1.tok({ textOneOf: ['という', 'と言う'] }, 'toiu');
      const ten = b1.tok({ textOneOf: ['点', 'てん'] }, 'ten');
      const kara = b1.particle('から', 'kara');
      const kangaeru = b1.tok({ textOneOf: ['考える', 'かんがえる'] }, 'kangaeru');
      const to = b1.particle('と', 'to');

      b1.inOrder(noun, toiu, 5);
      b1.inOrder(toiu, ten, 1);
      b1.inOrder(ten, kara, 1);
      b1.inOrder(kara, kangaeru, 1);
      b1.inOrder(kangaeru, to, 1);

      b1.captureSpan('という点から考えると', noun, to);
    },

    // Pattern 2: Noun + の + 点 + から + 考える + と
    // e.g., 健康の点から考えると
    //       日本の教師は１週間に働く時間の点から考えると
    (b2) => {
      const noun = b2.tok({ posOneOf: ['NOUN', 'PROPN', 'PRON'] }, 'noun');
      const no = b2.particle('の', 'no');
      const ten = b2.tok({ textOneOf: ['点', 'てん'] }, 'ten');
      const kara = b2.particle('から', 'kara');
      const kangaeru = b2.tok({ textOneOf: ['考える', 'かんがえる'] }, 'kangaeru');
      const to = b2.particle('と', 'to');

      b2.inOrder(noun, no, 1);
      b2.inOrder(no, ten, 1);
      b2.inOrder(ten, kara, 1);
      b2.inOrder(kara, kangaeru, 1);
      b2.inOrder(kangaeru, to, 1);

      b2.captureSpan('という点から考えると', noun, to);
    },

    // Pattern 3: (Modifier) + 点 + から + 考える + と
    // e.g., そうした点から考えると
    //       ３点から考えると
    (b3) => {
      const modifier = b3.tok({ posOneOf: ['NOUN', 'PROPN', 'PRON', 'VERB', 'ADJ', 'NUM', 'ADV'] }, 'modifier');
      const ten = b3.tok({ textOneOf: ['点', 'てん'] }, 'ten');
      const kara = b3.particle('から', 'kara');
      const kangaeru = b3.tok({ textOneOf: ['考える', 'かんがえる'] }, 'kangaeru');
      const to = b3.particle('と', 'to');

      b3.inOrder(modifier, ten, 3);
      b3.inOrder(ten, kara, 1);
      b3.inOrder(kara, kangaeru, 1);
      b3.inOrder(kangaeru, to, 1);

      b3.captureSpan('という点から考えると', modifier, to);
    },

    // Pattern 3b: Specific pattern for "そうした" + 点 + から + 考える + と
    // e.g., そうした点から考えると、そうしたてんからかんがえると
    (b3b) => {
      const soshita = b3b.tok({ textOneOf: ['そうした'] }, 'soshita');
      const ten = b3b.tok({ textOneOf: ['点', 'てん'] }, 'ten');
      const kara = b3b.particle('から', 'kara');
      const kangaeru = b3b.tok({ textOneOf: ['考える', 'かんがえる'] }, 'kangaeru');
      const to = b3b.particle('と', 'to');

      b3b.inOrder(soshita, ten, 1);
      b3b.inOrder(ten, kara, 1);
      b3b.inOrder(kara, kangaeru, 1);
      b3b.inOrder(kangaeru, to, 1);

      b3b.captureSpan('という点から考えると', soshita, to);
    },

    // Pattern 3c: "そう" + "したてん" + から + 考える + と (if tokenized differently)
    (b3c) => {
      const so = b3c.tok({ text: 'そう' }, 'so');
      const shitaTen = b3c.tok({ textOneOf: ['した点', 'したてん'] }, 'shitaTen');
      const kara = b3c.particle('から', 'kara');
      const kangaeru = b3c.tok({ textOneOf: ['考える', 'かんがえる'] }, 'kangaeru');
      const to = b3c.particle('と', 'to');

      b3c.inOrder(so, shitaTen, 1);
      b3c.inOrder(shitaTen, kara, 1);
      b3c.inOrder(kara, kangaeru, 1);
      b3c.inOrder(kangaeru, to, 1);

      b3c.captureSpan('という点から考えると', so, to);
    },

    // Pattern 3d: "そうした" + "てんから考えると" (combined tokens)
    (b3d) => {
      const soshita = b3d.tok({ textOneOf: ['そうした'] }, 'soshita');
      const tenKaraKangaeruTo = b3d.tok({ textOneOf: ['てんからかんがえると', 'てんから考えると'] }, 'tenKaraKangaeruTo');

      b3d.inOrder(soshita, tenKaraKangaeruTo, 1);
      b3d.captureSpan('という点から考えると', soshita, tenKaraKangaeruTo);
    },

    // Pattern 4: Loose pattern to handle various tokenizations
    // Matches any noun/verb/adj + (optional: という/の) + 点 + から + 考える + と
    // Must have 考える (not just だ or other verbs) after 点から
    (b4) => {
      const start = b4.tok({ posOneOf: ['NOUN', 'PROPN', 'PRON', 'VERB', 'ADJ', 'NUM', 'ADV'] }, 'start');
      const ten = b4.tok({ textOneOf: ['点', 'てん'] }, 'ten');
      const kara = b4.particle('から', 'kara');
      const kangaeru = b4.tok({ textOneOf: ['考える', 'かんがえる'] }, 'kangaeru');
      const to = b4.particle('と', 'to');

      // Make sure 考える comes shortly after から
      b4.inOrder(start, ten, 8);
      b4.inOrder(ten, kara, 3);
      b4.inOrder(kara, kangaeru, 2);
      b4.inOrder(kangaeru, to, 1);

      b4.captureSpan('という点から考えると', start, to);
    }
  );
});
