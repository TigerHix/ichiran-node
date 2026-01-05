import { bunproLinguisticRule } from '../../../engine/lang.js';

export default bunproLinguisticRule('なければいけない', (r) => {
  r.either(
    // Pattern 1: Standard casual form - verb[nai-mizen] + れば + いけない
    // e.g., 行かなければいけない, しなければいけない, 飲まなければいけない
    // GiNZA parses: なけれ (AUX, lemma=ない, infl=仮定形-一般) + ば (SCONJ)
    (b) => {
      // Negative auxiliary in conditional form: なけれ (from ない)
      const nakere = b.aux({
        text: 'なけれ',
        lemma: 'ない',
        inflectionForm: '仮定形-一般',
      }, 'nakere');

      // Conditional particle: ば
      const ba = b.tok({
        text: 'ば',
        pos: 'SCONJ',
      }, 'ba');

      // Potential form of iku in negative form: いけ
      const ike = b.verb({
        lemma: 'いける',
        inflectionForm: '未然形-一般',
      }, 'ike');

      // Negative auxiliary: ない
      const nai = b.tok({
        text: 'ない',
        pos: 'AUX',
        lemma: 'ない',
      }, 'nai');

      // Structure constraints
      b.inOrder(nakere, ba, 1);
      b.inOrder(ba, ike, 2);
      b.inOrder(ike, nai, 1);

      // Capture from nakere through ikenai
      b.captureSpan('なければいけない', nakere, nai);
    },

    // Pattern 2: Standard polite form - verb[nai-mizen] + れば + いけません
    // e.g., 行かなければいけません, しなければいけません
    (b) => {
      // Negative auxiliary in conditional form: なけれ
      const nakere = b.aux({
        text: 'なけれ',
        lemma: 'ない',
        inflectionForm: '仮定形-一般',
      }, 'nakere');

      // Conditional particle: ば
      const ba = b.tok({
        text: 'ば',
        pos: 'SCONJ',
      }, 'ba');

      // Potential form of iku in polite form: いけ (連用形 before ません)
      const ike = b.verb({
        lemma: 'いける',
        inflectionForm: '連用形-一般',
      }, 'ike');

      // Polite negative auxiliary: ませ (first part of ません)
      const mase = b.aux({
        text: 'ませ',
        lemma: 'ます',
        inflectionForm: '未然形-一般',
      }, 'mase');

      // Structure constraints
      b.inOrder(nakere, ba, 1);
      b.inOrder(ba, ike, 2);
      b.auxOf(ike, mase);

      // Capture from nakere through ikemase(n)
      b.captureSpan('なければいけない', nakere, mase);
    },

    // Pattern 3: Contracted form with ikenai - verb[nai-mizen] + なきゃ + いけない
    // e.g., 返さなきゃいけない, 行かなきゃいけない
    // GiNZA: 返さ[VERB,未然形] + なきゃ[AUX,仮定形-融合] + いけ[VERB,未然形] + ない[AUX,終止形]
    (b) => {
      // Verb in irrealis form (未然形)
      const verb = b.tok({
        inflectionForm: '未然形-一般',
        posOneOf: ['VERB', 'AUX'],
      }, 'verb');

      // Contraction auxiliary: なきゃ (仮定形-融合 of ない)
      const nakya = b.aux({
        lemma: 'ない',
        text: 'なきゃ',
        inflectionForm: '仮定形-融合',
      }, 'nakya');

      // Potential form of iku in negative form: いけ
      const ike = b.verb({
        lemma: 'いける',
        inflectionForm: '未然形-一般',
      }, 'ike');

      // Negative auxiliary: ない
      const nai = b.tok({
        text: 'ない',
        pos: 'AUX',
        lemma: 'ない',
      }, 'nai');

      // Structure constraints: verb → nakya (auxOf or inOrder)
      b.either(
        (branch) => {
          branch.auxOf(verb, nakya);
          branch.inOrder(nakya, ike, 2);
        },
        (branch) => {
          branch.inOrder(verb, nakya, 1);
          branch.inOrder(nakya, ike, 2);
        }
      );
      b.inOrder(ike, nai, 1);

      // Capture from verb through ikenai
      b.captureSpan('なければいけない', verb, nai);
    },

    // Pattern 4: Contracted without ikenai - verb[nai-mizen] + なきゃ
    // e.g., 行かなきゃ, しなきゃ (casual speech, ikenai implied)
    // GiNZA: 入ら[VERB,未然形] + なきゃ[AUX,仮定形-融合]
    (b) => {
      // Verb in irrealis form (未然形)
      const verb = b.tok({
        inflectionForm: '未然形-一般',
        posOneOf: ['VERB', 'AUX'],
      }, 'verb');

      // Contraction auxiliary: なきゃ (仮定形-融合 of ない)
      const nakya = b.aux({
        lemma: 'ない',
        text: 'なきゃ',
        inflectionForm: '仮定形-融合',
      }, 'nakya');

      // Structure constraints: verb → nakya (auxOf or inOrder)
      b.either(
        (branch) => branch.auxOf(verb, nakya),
        (branch) => branch.inOrder(verb, nakya, 1)
      );

      // Capture from verb through nakya
      b.captureSpan('なければいけない', verb, nakya);
    },

    // Pattern 5: Past tense form - verb[nai-mizen] + れば + いけなかった
    // e.g., のらなければいけなかった, 行かなければいけなかった
    // GiNZA: なけれ[AUX,仮定形] + ば[SCONJ] + いけ[VERB,未然形] + なかっ[AUX,連用形促音便] + た[AUX]
    (b) => {
      // Negative auxiliary in conditional form: なけれ (from ない)
      const nakere = b.aux({
        text: 'なけれ',
        lemma: 'ない',
        inflectionForm: '仮定形-一般',
      }, 'nakere');

      // Conditional particle: ば
      const ba = b.tok({
        text: 'ば',
        pos: 'SCONJ',
      }, 'ba');

      // Potential form of iku in irrealis form: いけ (before なかった)
      // Note: GiNZA uses lemma=いける (potential form) but inflection=未然形
      const ike = b.tok({
        text: 'いけ',
        pos: 'VERB',
        inflectionForm: '未然形-一般',
      }, 'ike');

      // Past negative auxiliary: なかっ (促音便 form of ない before た)
      const nakatta = b.aux({
        lemma: 'ない',
        inflectionForm: '連用形-促音便',
      }, 'nakatta');

      // Past tense auxiliary: た
      const ta = b.aux({
        lemma: 'た',
      }, 'ta');

      // Structure constraints
      b.inOrder(nakere, ba, 1);
      b.inOrder(ba, ike, 2);
      b.inOrder(ike, nakatta, 1);
      b.inOrder(nakatta, ta, 1);

      // Capture from nakere through ta
      b.captureSpan('なければいけない', nakere, ta);
    }
  );
});
