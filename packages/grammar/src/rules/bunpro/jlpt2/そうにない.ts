import { linguisticRule } from '../../../engine/lang.js';

/**
 * JLPT2: そうにない (souninai) - Unlikely to, showing no signs of
 *
 * Matches verb stem + そうに(も)ない to express that something is unlikely to happen.
 * This is a weaker version of そうもない, indicating something doesn't seem likely.
 *
 * Structure: Verb［stem］+ そうに(も)ない
 *
 * Examples:
 * - 上がれそうにない (unlikely to be able to finish work/leave)
 * - 行けそうにない (unlikely to be able to go)
 * - 降りそうにない (unlikely to rain)
 * - できそうにない (unlikely to be able to do)
 * - 追い越せそうにない (unlikely to be able to surpass)
 * - 暖まりそうにない (showing no signs of warming up)
 *
 * Key discriminators:
 * - そう has tag=形状詞-助動詞語幹 (appearance), not 名詞-助動詞語幹 (hearsay)
 * - The particle に (ADP) attaches as case marker to the verb stem
 * - Optional particle も can follow に (そうにもない variant)
 * - ない (ADJ, lemma=ない) is the main predicate
 *
 * GiNZA parse patterns:
 * 1. Normal: stem(VERB, 連用形-一般) + そう(ADV, dep=advmod, head=stem) + に(ADP, dep=case, head=stem) + (も) + ない
 * 2. Alternative: stem(VERB, 連用形-一般) + そう(AUX, dep=aux, head=stem) + に(ADP, dep=case, head=stem) + (も) + ない
 * 3. Potential verbs: stem(VERB, 命令形/仮定形-一般) + そう(AUX/ADV) + に + (も) + ない
 * 4. Passive/potential with auxiliary: stem(VERB, 未然形-一般) + auxiliary(AUX) + そう(AUX, dep=aux, head=stem) + に + (も) + ない
 * 5. Verbs parsed as ADJ (うかる type): stem(ADJ, tag=動詞-一般, 連用形-一般) + そう(AUX, dep=aux) + に + (も) + ない
 * 6. イ音便 verbs (きく type): stem(VERB, 連用形-イ音便) + そう(AUX, dep=aux) + に + (も) + ない
 * 7. サ変 compounds (noun + aux verb): noun(VERB, tag=名詞-普通名詞-サ変可能) + aux(AUX, lemma=できる) + そう(AUX, dep=aux) + に + (も) + ない
 * 8. Irregular きそう (single NOUN token) + に + (も) + ない
 *
 * Note: This is the negative conjecture form, weaker than そうもない due to the particle に instead of も.
 * The variant そうにもない is slightly stronger but still weaker than そうもない.
 */
export default linguisticRule('そうにない', (r) => {
  r.either(
    // Branch 1: Verb stem (連用形-一般) + そう (ADV, dep=advmod) + に + (も) + ない
    // Example: 遊べそうにない (遊べ is VERB, 連用形-一般; そう is ADV, dep=advmod)
    (b) => {
      const stem = b.verb({}, 'stem');
      const sou = b.tok({
        lemma: 'そう',
        tag: '形状詞-助動詞語幹',
        pos: 'ADV',
        dep: 'advmod',
      }, 'sou');
      b.headChild(stem, sou, 'advmod');
      const ni = b.particle('に', 'ni');
      b.caseMarker(stem, ni);
      b.optional((ob) => {
        const mo = ob.particle('も', 'mo');
        ob.inOrder(ni, mo, 1);
      });
      const nai = b.adj({
        lemma: 'ない',
      }, 'nai');
      b.captureSpan('そうにない', stem, nai);
    },

    // Branch 2: Verb stem (連用形-一般) + そう (AUX, dep=aux) + に + (も) + ない
    // Example: できそうにない (でき is VERB, 連用形-一般; そう is AUX, dep=aux)
    (b) => {
      const stem = b.verb({
        // inflectionForm: '連用形-一般',
      }, 'stem');
      const sou = b.tok({
        lemma: 'そう',
        tag: '形状詞-助動詞語幹',
        pos: 'AUX',
        dep: 'aux',
      }, 'sou');
      b.auxOf(stem, sou);
      const ni = b.particle('に', 'ni');
      b.caseMarker(stem, ni);
      b.optional((ob) => {
        const mo = ob.particle('も', 'mo');
        ob.inOrder(ni, mo, 1);
      });
      const nai = b.adj({
        lemma: 'ない',
      }, 'nai');
      b.captureSpan('そうにない', stem, nai);
    },

    // Branch 3: Verb stem (命令形) + そう + に + (も) + ない
    (b) => {
      const stem = b.verb({
        // inflectionForm: '命令形',
      }, 'stem');
      const sou = b.tok({
        lemma: 'そう',
        tag: '形状詞-助動詞語幹',
      }, 'sou');
      b.headChild(stem, sou, 'advmod');
      const ni = b.particle('に', 'ni');
      b.caseMarker(stem, ni);
      b.optional((ob) => {
        const mo = ob.particle('も', 'mo');
        ob.inOrder(ni, mo, 1);
      });
      const nai = b.adj({
        lemma: 'ない',
      }, 'nai');
      b.captureSpan('そうにない', stem, nai);
    },

    // Branch 4: Verb stem (仮定形-一般) + そう (AUX, dep=aux) + に + (も) + ない
    // Example: はこべそうにない (はこべ is VERB, 仮定形-一般)
    (b) => {
      const stem = b.verb({
        // inflectionForm: '仮定形-一般',
      }, 'stem');
      const sou = b.tok({
        lemma: 'そう',
        tag: '形状詞-助動詞語幹',
        pos: 'AUX',
        dep: 'aux',
      }, 'sou');
      b.auxOf(stem, sou);
      const ni = b.particle('に', 'ni');
      b.caseMarker(stem, ni);
      b.optional((ob) => {
        const mo = ob.particle('も', 'mo');
        ob.inOrder(ni, mo, 1);
      });
      const nai = b.adj({
        lemma: 'ない',
      }, 'nai');
      b.captureSpan('そうにない', stem, nai);
    },

    // Branch 5: Verb stem (未然形-一般) + auxiliary (られる) + そう (AUX, dep=aux) + に + (も) + ない
    // Example: 食べられそうにない, 受け取れそうにない
    (b) => {
      const stem = b.verb({
        // inflectionForm: '未然形-一般',
      }, 'stem');
      const rareru = b.aux({
        lemma: 'られる',
      }, 'rareru');
      b.auxOf(stem, rareru);
      const sou = b.tok({
        lemma: 'そう',
        tag: '形状詞-助動詞語幹',
        pos: 'AUX',
        dep: 'aux',
      }, 'sou');
      b.auxOf(stem, sou);
      const ni = b.particle('に', 'ni');
      b.caseMarker(stem, ni);
      b.optional((ob) => {
        const mo = ob.particle('も', 'mo');
        ob.inOrder(ni, mo, 1);
      });
      const nai = b.adj({
        lemma: 'ない',
      }, 'nai');
      b.captureSpan('そうにない', stem, nai);
    },

    // Branch 6: Verb stem (連用形-イ音便) + そう (AUX, dep=aux) + に + (も) + ない
    // Example: きいてくれそうにない (きい is VERB, 連用形-イ音便)
    // Verbs like きく (to listen) conjugate to きいて in 連用形-イ音便
    (b) => {
      const stem = b.verb({
        // inflectionForm: '連用形-イ音便',
      }, 'stem');
      const sou = b.tok({
        lemma: 'そう',
        tag: '形状詞-助動詞語幹',
        pos: 'AUX',
        dep: 'aux',
      }, 'sou');
      b.auxOf(stem, sou);
      const ni = b.particle('に', 'ni');
      b.caseMarker(stem, ni);
      b.optional((ob) => {
        const mo = ob.particle('も', 'mo');
        ob.inOrder(ni, mo, 1);
      });
      const nai = b.adj({
        lemma: 'ない',
      }, 'nai');
      b.captureSpan('そうにない', stem, nai);
    },

    // Branch 7: Verb parsed as ADJ (うかる type) + そう (AUX, dep=aux) + に + (も) + ない
    // Example: うかりそうにない (うかり is ADJ, tag=動詞-一般, 連用形-一般)
    // Some verbs like うかる (to pass) are parsed as ADJ by GiNZA
    (b) => {
      const stem = b.adj({
        tag: '動詞-一般',
        // inflectionForm: '連用形-一般',
      }, 'stem');
      const sou = b.tok({
        lemma: 'そう',
        tag: '形状詞-助動詞語幹',
        pos: 'AUX',
        dep: 'aux',
      }, 'sou');
      b.auxOf(stem, sou);
      const ni = b.particle('に', 'ni');
      b.caseMarker(stem, ni);
      b.optional((ob) => {
        const mo = ob.particle('も', 'mo');
        ob.inOrder(ni, mo, 1);
      });
      const nai = b.adj({
        lemma: 'ない',
      }, 'nai');
      b.captureSpan('そうにない', stem, nai);
    },

    // Branch 8: Noun (サ変可能) + auxiliary verb (できる) + そう (AUX, dep=aux) + に + (も) + ない
    // Example: 合格できそうにない (合格 is VERB/NOUN, tag=名詞-普通名詞-サ変可能; でき is AUX, lemma=できる)
    (b) => {
      const noun = b.tok({
        tag: '名詞-普通名詞-サ変可能',
      }, 'noun');
      const deki = b.aux({
        lemma: 'できる',
        // inflectionForm: '連用形-一般',
      }, 'deki');
      b.auxOf(noun, deki);
      const sou = b.tok({
        lemma: 'そう',
        tag: '形状詞-助動詞語幹',
        pos: 'AUX',
        dep: 'aux',
      }, 'sou');
      b.auxOf(noun, sou);
      const ni = b.particle('に', 'ni');
      b.caseMarker(noun, ni);
      b.optional((ob) => {
        const mo = ob.particle('も', 'mo');
        ob.inOrder(ni, mo, 1);
      });
      const nai = b.adj({
        lemma: 'ない',
      }, 'nai');
      b.captureSpan('そうにない', noun, nai);
    },

    // Branch 9: Irregular きそう (single NOUN token) + に + (も) + ない
    // Example: きそうにない (きそう is single NOUN token, lemma=きそう, tag=名詞-普通名詞-一般)
    // GiNZA sometimes parses "来+そう" as a single noun token
    (b) => {
      const kisou = b.tok({
        pos: 'NOUN',
        tag: '名詞-普通名詞-一般',
      }, 'kisou');
      const ni = b.particle('に', 'ni');
      b.caseMarker(kisou, ni);
      b.optional((ob) => {
        const mo = ob.particle('も', 'mo');
        ob.inOrder(ni, mo, 1);
      });
      const nai = b.adj({
        lemma: 'ない',
      }, 'nai');
      b.inOrder(kisou, ni, 1);
      b.inOrder(ni, nai, 3);
      b.captureSpan('そうにない', kisou, nai);
    }
  );
});
