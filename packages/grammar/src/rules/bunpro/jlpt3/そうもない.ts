import { linguisticRule } from '../../../engine/lang.js';

/**
 * JLPT3: そうもない - Very unlikely to / doesn't seem like
 *
 * Matches verb stem + そうもない to express that something is very unlikely to happen.
 * This is a stronger version of そうにない, indicating that something doesn't even appear likely.
 *
 * Structure: Verb［stem］+ そうもない
 *
 * Examples:
 * - 遊べそうもない (very unlikely to be able to play/hang out)
 * - 運べそうもない (very unlikely to be able to carry)
 * - できそうもない (very unlikely to be able to do)
 * - 降りそうもない (doesn't look like it will rain)
 * - 来そうもない (doesn't look like [someone] will come)
 * - 食べられそうもない (doesn't look like [someone] can eat)
 *
 * Key discriminators:
 * - そう has tag=形状詞-助動詞語幹 (appearance), not 名詞-助動詞語幹 (hearsay)
 * - The particle も (ADP) attaches as case marker
 * - ない (ADJ, lemma=ない) is the main predicate
 *
 * GiNZA parse patterns:
 * 1. Normal: stem(VERB, 連用形-一般) + そう(ADV, dep=advmod, head=stem) + も(ADP, dep=case, head=そう) + ない
 * 2. Alternative: stem(VERB, 連用形-一般) + そう(AUX, dep=aux, head=stem) + も(ADP, dep=case, head=stem) + ない
 * 3. Potential verbs: stem(VERB, 命令形/仮定形-一般) + そう(AUX/ADV) + も + ない
 * 4. Passive/potential with auxiliary: stem(VERB, 未然形-一般) + auxiliary(AUX) + そう(AUX, dep=aux, head=stem) + も + ない
 * 5. Verbs parsed as ADJ (うかる type): stem(ADJ, tag=動詞-一般, 連用形-一般) + そう(AUX, dep=aux) + も + ない
 * 6. イ音便 verbs (きく type): stem(VERB, 連用形-イ音便) + そう(AUX, dep=aux) + も + ない
 * 7. サ変 compounds (noun + aux verb): noun(VERB, tag=名詞-普通名詞-サ変可能) + aux(AUX, lemma=できる) + そう(AUX, dep=aux) + も + ない
 * 8. Irregular きそう (single NOUN token) + も + ない
 *
 * Note: This is the negative conjecture form, stronger than そうにない due to the emphatic particle も.
 */
export default linguisticRule('そうもない', (r) => {
  r.either(
    // Branch 1: Verb stem (連用形-一般) + そう (ADV, dep=advmod) + も + ない
    // Example: 遊べそうもない (遊べ is VERB, 連用形-一般; そう is ADV, dep=advmod)
    (b) => {
      const stem = b.verb({
        inflectionForm: '連用形-一般',
      }, 'stem');
      const sou = b.tok({
        lemma: 'そう',
        tag: '形状詞-助動詞語幹',
        pos: 'ADV',
        dep: 'advmod',
      }, 'sou');
      b.headChild(stem, sou, 'advmod');
      const mo = b.particle('も', 'mo');
      b.caseMarker(sou, mo);
      const nai = b.adj({
        lemma: 'ない',
      }, 'nai');
      b.captureSpan('そうもない', stem, nai);
    },

    // Branch 2: Verb stem (連用形-一般) + そう (AUX, dep=aux) + も + ない
    // Example: できそうもない (でき is VERB, 連用形-一般; そう is AUX, dep=aux)
    (b) => {
      const stem = b.verb({
        inflectionForm: '連用形-一般',
      }, 'stem');
      const sou = b.tok({
        lemma: 'そう',
        tag: '形状詞-助動詞語幹',
        pos: 'AUX',
        dep: 'aux',
      }, 'sou');
      b.auxOf(stem, sou);
      const mo = b.particle('も', 'mo');
      b.caseMarker(stem, mo);
      const nai = b.adj({
        lemma: 'ない',
      }, 'nai');
      b.captureSpan('そうもない', stem, nai);
    },

    // Branch 3: Verb stem (命令形) + そう + も + ない
    (b) => {
      const stem = b.verb({
        inflectionForm: '命令形',
      }, 'stem');
      const sou = b.tok({
        lemma: 'そう',
        tag: '形状詞-助動詞語幹',
      }, 'sou');
      b.headChild(stem, sou, 'advmod');
      const mo = b.particle('も', 'mo');
      b.caseMarker(stem, mo);
      const nai = b.adj({
        lemma: 'ない',
      }, 'nai');
      b.captureSpan('そうもない', stem, nai);
    },

    // Branch 4: Verb stem (仮定形-一般) + そう (AUX, dep=aux) + も + ない
    // Example: はこべそうもない (はこべ is VERB, 仮定形-一般)
    (b) => {
      const stem = b.verb({
        inflectionForm: '仮定形-一般',
      }, 'stem');
      const sou = b.tok({
        lemma: 'そう',
        tag: '形状詞-助動詞語幹',
        pos: 'AUX',
        dep: 'aux',
      }, 'sou');
      b.auxOf(stem, sou);
      const mo = b.particle('も', 'mo');
      b.caseMarker(stem, mo);
      const nai = b.adj({
        lemma: 'ない',
      }, 'nai');
      b.captureSpan('そうもない', stem, nai);
    },

    // Branch 5: Verb stem (未然形-一般) + auxiliary (られる) + そう (AUX, dep=aux) + も + ない
    // Example: 食べられそうもない, 見られそうもない
    (b) => {
      const stem = b.verb({
        inflectionForm: '未然形-一般',
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
      const mo = b.particle('も', 'mo');
      b.caseMarker(stem, mo);
      const nai = b.adj({
        lemma: 'ない',
      }, 'nai');
      b.captureSpan('そうもない', stem, nai);
    },

    // Branch 6: Verb stem (連用形-イ音便) + そう (AUX, dep=aux) + も + ない
    // Example: きいてくれそうもない (きい is VERB, 連用形-イ音便)
    // Verbs like きく (to listen) conjugate to きいて in 連用形-イ音便
    (b) => {
      const stem = b.verb({
        inflectionForm: '連用形-イ音便',
      }, 'stem');
      const sou = b.tok({
        lemma: 'そう',
        tag: '形状詞-助動詞語幹',
        pos: 'AUX',
        dep: 'aux',
      }, 'sou');
      b.auxOf(stem, sou);
      const mo = b.particle('も', 'mo');
      b.caseMarker(stem, mo);
      const nai = b.adj({
        lemma: 'ない',
      }, 'nai');
      b.captureSpan('そうもない', stem, nai);
    },

    // Branch 7: Verb parsed as ADJ (うかる type) + そう (AUX, dep=aux) + も + ない
    // Example: うかりそうもない (うかり is ADJ, tag=動詞-一般, 連用形-一般)
    // Some verbs like うかる (to pass) are parsed as ADJ by GiNZA
    (b) => {
      const stem = b.adj({
        tag: '動詞-一般',
        inflectionForm: '連用形-一般',
      }, 'stem');
      const sou = b.tok({
        lemma: 'そう',
        tag: '形状詞-助動詞語幹',
        pos: 'AUX',
        dep: 'aux',
      }, 'sou');
      b.auxOf(stem, sou);
      const mo = b.particle('も', 'mo');
      b.caseMarker(stem, mo);
      const nai = b.adj({
        lemma: 'ない',
      }, 'nai');
      b.captureSpan('そうもない', stem, nai);
    },

    // Branch 8: Noun (サ変可能) + auxiliary verb (できる) + そう (AUX, dep=aux) + も + ない
    // Example: 実現できそうもない (実現 is VERB/NOUN, tag=名詞-普通名詞-サ変可能; でき is AUX, lemma=できる)
    (b) => {
      const noun = b.tok({
        tag: '名詞-普通名詞-サ変可能',
      }, 'noun');
      const deki = b.aux({
        lemma: 'できる',
        inflectionForm: '連用形-一般',
      }, 'deki');
      b.auxOf(noun, deki);
      const sou = b.tok({
        lemma: 'そう',
        tag: '形状詞-助動詞語幹',
        pos: 'AUX',
        dep: 'aux',
      }, 'sou');
      b.auxOf(noun, sou);
      const mo = b.particle('も', 'mo');
      b.caseMarker(noun, mo);
      const nai = b.adj({
        lemma: 'ない',
      }, 'nai');
      b.captureSpan('そうもない', noun, nai);
    },

    // Branch 9: Irregular きそう (single NOUN token) + も + ない
    // Example: きそうもない (きそう is single NOUN token, lemma=きそう, tag=名詞-普通名詞-一般)
    // GiNZA sometimes parses "来+そう" as a single noun token
    (b) => {
      const kisou = b.tok({
        pos: 'NOUN',
        tag: '名詞-普通名詞-一般',
      }, 'kisou');
      const mo = b.particle('も', 'mo');
      b.caseMarker(kisou, mo);
      const nai = b.adj({
        lemma: 'ない',
      }, 'nai');
      b.inOrder(kisou, mo, 1);
      b.inOrder(mo, nai, 1);
      b.captureSpan('そうもない', kisou, nai);
    }
  );
});
