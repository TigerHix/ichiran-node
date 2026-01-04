import { linguisticRule } from '../../../engine/lang.js';

/**
 * JLPT4: みたい (mitai) - Looks like, Seems like, Similar to
 *
 * Casual version of ようだ (you da). Expresses that something looks like or
 * seems like something else, based on visual observation or reliable information.
 * Functions grammatically as a na-adjective.
 *
 * Structures:
 * - Noun + みたい + だ/です (looks like X)
 * - Verb + みたい (seems like X happened)
 * - ［い］Adjective + みたい (seems X)
 * - ［な］Adjective + みたい (seems X)
 * - Noun + みたいな + Noun (X-like Y)
 *
 * Examples:
 * - この犬は熊みたいです (This dog looks like a bear)
 * - あの雲は馬みたい (That cloud looks like a horse)
 * - 明日は雪が降るみたいだ (It seems like it will snow tomorrow)
 * - 彼は地下鉄が嫌いみたいだ (He seems to dislike subways)
 * - 先輩は今日は来ないみたい (It seems senpai won't come today)
 * - 彼はスポーツ選手みたいな体をしている (He has a body like an athlete)
 *
 * Key discriminators:
 * - みたい is the casual conversational form, less formal than ようだ
 * - Functions as AUX/ADJ depending on context
 * - Attaches to nouns, verbs, and adjectives
 *
 * GiNZA parse structure:
 * - 熊みたい: 熊(NOUN) + みたい(AUX/ADJ, lemma=みたい)
 * - 降るみたい: 降る(VERB) + みたい(AUX/ADJ, lemma=みたい)
 * - 嫌いみたい: 嫌い(ADJ) + みたい(AUX/ADJ, lemma=みたい)
 * - みたいな: みたい(ADJ) + な(PART/AUX)
 */
export default linguisticRule('みたい', (r) => {
  r.either(
    // Branch 1: Noun + みたい
    // Example: 熊みたい, パソコンみたいだ, 有名人みたい
    // Must be NOUN pos to avoid matching verbs/adjectives
    (b) => {
      const noun = b.tok({
        pos: 'NOUN',
      }, 'noun');
      const mitai = b.tok({
        lemma: 'みたい',
      }, 'mitai');
      b.inOrder(noun, mitai, 1);
      b.captureSpan('みたい', noun, mitai);
    },

    // Branch 2: Verb (any form) + みたい
    // Example: 降るみたいだ, 来ないみたい, 出て行ったみたい, 遊んでもいいみたい
    // Captures the entire verb phrase before みたい
    (b) => {
      const verb = b.verb({}, 'verb');
      const mitai = b.tok({
        lemma: 'みたい',
      }, 'mitai');
      b.inOrder(verb, mitai);
      b.captureSpan('みたい', verb, mitai);
    },

    // Branch 3: I-adjective + みたい
    // Example: 浅いみたい, 楽しみたい (rare but possible)
    (b) => {
      const adj = b.adj({
        pos: 'ADJ',
      }, 'adj');
      const mitai = b.tok({
        lemma: 'みたい',
      }, 'mitai');
      b.inOrder(adj, mitai, 1);
      b.captureSpan('みたい', adj, mitai);
    },

    // Branch 4: Na-adjective + みたい
    // Example: 嫌いみたい, 好きみたい
    // Na-adjectives can be ADJ or ADV depending on context
    (b) => {
      const adj = b.tok({
        tag: '形状詞-一般',
      }, 'adj');
      const mitai = b.tok({
        lemma: 'みたい',
      }, 'mitai');
      b.inOrder(adj, mitai, 1);
      b.captureSpan('みたい', adj, mitai);
    },

    // Branch 5: Noun + みたいな (attributive form modifying another noun)
    // Example: スポーツ選手みたいな体, 春みたいな天気
    (b) => {
      const noun = b.tok({
        pos: 'NOUN',
      }, 'noun');
      const mitai = b.tok({
        lemma: 'みたい',
      }, 'mitai');
      const na = b.aux({
        text: 'な',
      }, 'na');
      b.inOrder(noun, mitai, 1);
      b.inOrder(mitai, na, 1);
      b.captureSpan('みたい', noun, na);
    }
  );
});
