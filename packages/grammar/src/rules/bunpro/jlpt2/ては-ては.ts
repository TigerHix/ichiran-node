import { linguisticRule } from '../../../engine/lang.js';

/**
 * JLPT2: ては-ては (te wa te wa) - Repeatedly doing X and Y
 *
 * A grammar pattern expressing repeated alternating actions. It indicates that
 * actions (A) and (B) are repeated in sequence, often leading to result (C).
 *
 * Structure:
 * - Verb[te] + は + ( intervening content ) + Verb[te] + は
 * - Can appear as ては or では (after verbs ending in む/ぶ/ぬ)
 * - Colloquial forms: ちゃ and じゃ (contracted from ては and では)
 *
 * Examples:
 * - 食べては寝て、食べては寝ての繰り返しだから太ってきた。
 *   (Eating and sleeping, eating and sleeping repeatedly, so I gained weight.)
 * - 接続しては切断して、接続しては切断して。
 *   (Connecting and disconnecting, connecting and disconnecting.)
 * - 酒を飲んでは暴れ、暴れては物を壊す。
 *   (Drinking and raging, raging and breaking things.)
 * - 運動しないで食っちゃ寝食っちゃ寝ばかりしていると牛になるよ！
 *   (If you eat and sleep, eat and sleep without exercising, you'll turn into a cow!)
 *
 * Key discriminators:
 * - Requires two occurrences of verb-te + は (or ちゃ/じゃ)
 * - Verbs must be in te-form (ending in て or で)
 * - Pattern indicates repeated alternating actions
 * - Usually appears with words indicating repetition (繰り返し, etc.)
 *
 * Different from:
 * - Single ては (conditional - "if/when doing X")
 * - Regular te-form without repetition
 * - Other te-form patterns (てしまう, ておく, etc.)
 */
export default linguisticRule('ては-ては', (r) => {
  r.either(
    // Pattern 1: Full form ては + (content) + ては
    // Structure: verb1-te-wa1 + (any content including verb2) + verb3-te-wa2
    // e.g., 受け取っては誰かに渡し、渡されては受け取る
    //      verb1=受け取っ+て+は, verb2=渡し, verb3=渡され+て+は
    //      圧縮しては送信し、圧縮しては送信し
    //      verb1=圧縮し+て+は, verb2=送信し, verb3=圧縮し+て+は
    (b1) => {
      const verb1 = b1.verb({}, 'verb1');
      const te1 = b1.tok({ text: 'て' }, 'te1');
      const wa1 = b1.particle('は', 'wa1');

      const verb3 = b1.verb({}, 'verb3');
      const te3 = b1.tok({ text: 'て' }, 'te3');
      const wa2 = b1.particle('は', 'wa2');

      // Structure: verb1-te-wa1 ... verb3-te-wa2
      b1.inOrder(verb1, te1, 1);
      b1.inOrder(te1, wa1, 1);
      b1.inOrder(wa1, verb3, 5);
      b1.inOrder(verb3, te3, 1);
      b1.inOrder(te3, wa2, 1);

      b1.captureSpan('ては-ては', verb1, wa2);
    },

    // Pattern 2: Full form では ... では (after む/ぶ/ぬ verbs)
    (b2) => {
      const verb1 = b2.verb({}, 'verb1');
      const de1 = b2.tok({ text: 'で' }, 'de1');
      const wa1 = b2.particle('は', 'wa1');

      const verb3 = b2.verb({}, 'verb3');
      const de3 = b2.tok({ text: 'で' }, 'de3');
      const wa2 = b2.particle('は', 'wa2');

      b2.inOrder(verb1, de1, 1);
      b2.inOrder(de1, wa1, 1);
      b2.inOrder(wa1, verb3, 5);
      b2.inOrder(verb3, de3, 1);
      b2.inOrder(de3, wa2, 1);

      b2.captureSpan('ては-ては', verb1, wa2);
    },

    // Pattern 3: Mixed ては and では
    (b3) => {
      const verb1 = b3.verb({}, 'verb1');
      const te1 = b3.tok({ textOneOf: ['て', 'で'] }, 'te1');
      const wa1 = b3.particle('は', 'wa1');

      const verb3 = b3.verb({}, 'verb3');
      const te3 = b3.tok({ textOneOf: ['て', 'で'] }, 'te3');
      const wa2 = b3.particle('は', 'wa2');

      b3.inOrder(verb1, te1, 1);
      b3.inOrder(te1, wa1, 1);
      b3.inOrder(wa1, verb3, 5);
      b3.inOrder(verb3, te3, 1);
      b3.inOrder(te3, wa2, 1);

      b3.captureSpan('ては-ては', verb1, wa2);
    },

    // Pattern 4: Colloquial form ちゃ ... ちゃ
    (b4) => {
      const verb1 = b4.verb({}, 'verb1');
      const cha1 = b4.tok({ text: 'ちゃ' }, 'cha1');

      const verb3 = b4.verb({}, 'verb3');
      const cha3 = b4.tok({ text: 'ちゃ' }, 'cha3');

      b4.inOrder(verb1, cha1, 1);
      b4.inOrder(cha1, verb3, 5);
      b4.inOrder(verb3, cha3, 1);

      b4.captureSpan('ては-ては', verb1, cha3);
    },

    // Pattern 5: Colloquial form じゃ ... じゃ
    (b5) => {
      const verb1 = b5.verb({}, 'verb1');
      const ja1 = b5.tok({ text: 'じゃ' }, 'ja1');

      const verb3 = b5.verb({}, 'verb3');
      const ja3 = b5.tok({ text: 'じゃ' }, 'ja3');

      b5.inOrder(verb1, ja1, 1);
      b5.inOrder(ja1, verb3, 5);
      b5.inOrder(verb3, ja3, 1);

      b5.captureSpan('ては-ては', verb1, ja3);
    },

    // Pattern 6: Mixed colloquial ちゃ and じゃ
    (b6) => {
      const verb1 = b6.verb({}, 'verb1');
      const contracted1 = b6.tok({ textOneOf: ['ちゃ', 'じゃ'] }, 'contracted1');

      const verb3 = b6.verb({}, 'verb3');
      const contracted3 = b6.tok({ textOneOf: ['ちゃ', 'じゃ'] }, 'contracted3');

      b6.inOrder(verb1, contracted1, 1);
      b6.inOrder(contracted1, verb3, 5);
      b6.inOrder(verb3, contracted3, 1);

      b6.captureSpan('ては-ては', verb1, contracted3);
    },

    // Pattern 7: Combined form with single token for ては/では
    (b7) => {
      const verb1 = b7.verb({}, 'verb1');
      const tewa1 = b7.tok({ textOneOf: ['ては', 'では'] }, 'tewa1');

      const verb3 = b7.verb({}, 'verb3');
      const tewa3 = b7.tok({ textOneOf: ['ては', 'では'] }, 'tewa3');

      b7.inOrder(verb1, tewa1, 1);
      b7.inOrder(tewa1, verb3, 5);
      b7.inOrder(verb3, tewa3, 1);

      b7.captureSpan('ては-ては', verb1, tewa3);
    }
  );
});
