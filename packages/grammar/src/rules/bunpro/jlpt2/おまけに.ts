import { linguisticRule } from '../../../engine/lang.js';

/**
 * JLPT2: おまけに (お負けに) - On top of that, In addition, To make matters worse
 *
 * Matches おまけに, a conjunctive adverb meaning "on top of that", "in addition",
 * or "to make matters worse". This is a casual expression often used in negative
 * contexts, but can be used for positive situations as well.
 *
 * Structure:
 * - おまけに (conjunctive adverb)
 *
 * Examples:
 * - 今日は仕事に遅刻して部長に怒られたし、おまけに取引先の人も怒らせちゃったから、
 *   今日は最悪の日だったよ。 (Today was the worst because I was late to work and
 *   got yelled at by my boss, and to make matters worse, I upset a client.)
 * - 昨日は彼氏に美味しいご飯をご馳走してもらって、おまけにプレゼントまでもらった。
 *   (Yesterday my boyfriend treated me to a delicious meal, and on top of that,
 *   I even got a present.)
 * - パソコンが全然立ち上がらない。おまけにスマホの充電がないから仕事が全然できない。
 *   (My computer won't turn on. To make matters worse, my phone is out of battery,
 *   so I can't work at all.)
 *
 * Key discriminators:
 * - POS is ADV or CCONJ (can be parsed as either)
 * - This distinguishes from other conjunctions like その上, それに, さらに, etc.
 *
 * GiNZA parse structure:
 * - おまけに: text=おまけに, pos=ADV or CCONJ
 */
export default linguisticRule('おまけに', (r) => {
  const omakeni = r.tok({
    text: 'おまけに',
    posOneOf: ['ADV', 'CCONJ'],
  }, 'omakeni');
  r.capture(omakeni);
});
