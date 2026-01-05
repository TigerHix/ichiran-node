import { bunproLinguisticRule } from '../../../engine/lang.js';

export default bunproLinguisticRule('と言っても', (r) => {
  // と言っても (to itte mo) - "even if I say / although I say"
  // Concessive expression: "although (A) might be said, (B) is more accurate"
  //
  // Patterns:
  // 1. Quotational と/って + いっ/言っ (verb stem of いう) + て + も
  // 2. Can follow verbs, i-adjectives, na-adjectives, or nouns
  //
  // GiNZA parsing:
  // - といっても: と (ADP/case) + いっ (VERB/lemma=いう) + て (SCONJ) + も (ADP)
  // - っていっても: って (ADP/case) + いっ (VERB/lemma=いう) + て (SCONJ) + も (ADP)
  // - って言っても: って (ADP/case) + 言っ (VERB/lemma=言う) + て (SCONJ) + も (ADP)
  //
  // Dependency labels vary (fixed, advcl, mark, case) - don't constrain dep
  //
  // Examples:
  // - 毎日走るといっても (although I say I run every day)
  // - 重いといっても (although I say it's heavy - i-adj)
  // - 簡単だといっても (although I say it's easy - na-adj + だ)
  // - 先生って言っても (although you say "teacher" - casual)
  // - 有名だといっても (although one says it's famous - noun + だ)

  r.either(
    // Pattern 1: といっても (formal quotative と)
    // 毎日走ると(quot) + いっ(verb/いう) + て(te-form) + も(particle)
    // 重いと(quot) + いっ(verb/いう) + て(te-form) + も(particle)
    // 簡単だと(quot) + いっ(verb/いう) + て(te-form) + も(particle)
    (b) => {
      const toQuot = b.particle('と', 'toQuot');
      const iu = b.verb({
        lemma: 'いう',
        textOneOf: ['いっ', '言っ', 'ゆっ'],
      }, 'iu');
      const te = b.tok({
        text: 'て',
        pos: 'SCONJ',
      }, 'te');
      const mo = b.particle('も', 'mo');

      b.inOrder(toQuot, iu, 1);
      b.inOrder(iu, te, 1);
      b.inOrder(te, mo, 1);
      b.captureSpan('と言っても', toQuot, mo);
    },

    // Pattern 2: っていっても or って言っても (casual quotative って)
    // 先生って(quot) + 言っ(verb/言う) + て(te-form) + も(particle)
    // 楽しいって(quot) + いっ(verb/いう) + て(te-form) + も(particle)
    (b) => {
      const tteQuot = b.particle('って', 'tteQuot');
      const iu = b.verb({
        lemmaOneOf: ['いう', '言う'],
        textOneOf: ['いっ', '言っ', 'ゆっ'],
      }, 'iu');
      const te = b.tok({
        text: 'て',
        pos: 'SCONJ',
      }, 'te');
      const mo = b.particle('も', 'mo');

      b.inOrder(tteQuot, iu, 1);
      b.inOrder(iu, te, 1);
      b.inOrder(te, mo, 1);
      b.captureSpan('と言っても', tteQuot, mo);
    }
  );
});
