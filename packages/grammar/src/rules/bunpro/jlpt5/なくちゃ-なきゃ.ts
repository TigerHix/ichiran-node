import { bunproLinguisticRule } from '../../../engine/lang.js';

export default bunproLinguisticRule('なくちゃ-なきゃ', (r) => {
  r.either(
    // Pattern 1a: なくちゃ (verb[irrealis] + なく + ちゃ) - with ikenai
    // e.g., 食べなくちゃいけない, 勉強しなくちゃいけない
    // GiNZA: 食べ[VERB,未然形] + なく[AUX,連用形] + ちゃ[SCONJ] + いけ[VERB,未然形] + ない[AUX,終止形]
    //      勉強[VERB] + し[AUX,未然形] + なく[AUX,連用形] + ちゃ[SCONJ] (suru-verb case)
    (b) => {
      // Verb in irrealis form (未然形), or auxiliary する for suru-verbs
      const verb = b.tok({
        inflectionForm: '未然形-一般',
        posOneOf: ['VERB', 'AUX'],
      }, 'verb');

      // Negative auxiliary: なく (連用形 of ない)
      const naku = b.aux({ lemma: 'ない', inflectionForm: '連用形-一般' }, 'naku');

      // Contraction marker: ちゃ
      const cha = b.tok({ text: 'ちゃ', pos: 'SCONJ' }, 'cha');

      // Potential form of iku in negative form: いけ
      const ike = b.verb(
        { lemma: 'いける', inflectionForm: '未然形-一般' },
        'ike'
      );

      // Negative auxiliary: ない
      const nai = b.tok({ text: 'ない', pos: 'AUX', lemma: 'ない' }, 'nai');

      // Structure constraints: verb → naku (aux) or verb → naku (inOrder for suru-verbs)
      // Use either auxOf OR inOrder to handle both regular verbs and suru-verbs
      b.either(
        (branch) => {
          branch.auxOf(verb, naku);
          branch.inOrder(naku, cha, 1);
        },
        (branch) => {
          branch.inOrder(verb, naku, 1);
          branch.inOrder(naku, cha, 1);
        }
      );
      b.inOrder(cha, ike, 2);
      b.inOrder(ike, nai, 1);

      // Capture from verb through ikenai
      b.captureSpan('なくちゃ-なきゃ', verb, nai);
    },

    // Pattern 1b: なきゃ (verb[irrealis] + なきゃ) - with ikenai
    // e.g., 返さなきゃいけない, 行かなきゃいけない
    // GiNZA: 返さ[VERB,未然形] + なきゃ[AUX,仮定形-融合] + いけ[VERB,未然形] + ない[AUX,終止形]
    (b) => {
      // Verb in irrealis form (未然形), or auxiliary する for suru-verbs
      const verb = b.tok({
        inflectionForm: '未然形-一般',
        posOneOf: ['VERB', 'AUX'],
      }, 'verb');

      // Contraction auxiliary: なきゃ (仮定形-融合 of ない)
      const nakya = b.aux({ lemma: 'ない', text: 'なきゃ', inflectionForm: '仮定形-融合' }, 'nakya');

      // Potential form of iku in negative form: いけ
      const ike = b.verb(
        { lemma: 'いける', inflectionForm: '未然形-一般' },
        'ike'
      );

      // Negative auxiliary: ない
      const nai = b.tok({ text: 'ない', pos: 'AUX', lemma: 'ない' }, 'nai');

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
      b.captureSpan('なくちゃ-なきゃ', verb, nai);
    },

    // Pattern 1c: なけりゃ (verb[irrealis] + なけりゃ) - with ikenai
    // e.g., やらなけりゃいけない (alternative contraction form)
    // GiNZA: やら[VERB,未然形] + なけりゃ[AUX,仮定形-融合] + いけ[VERB,未然形] + ない[AUX,終止形]
    (b) => {
      // Verb in irrealis form (未然形), or auxiliary する for suru-verbs
      const verb = b.tok({
        inflectionForm: '未然形-一般',
        posOneOf: ['VERB', 'AUX'],
      }, 'verb');

      // Contraction auxiliary: なけりゃ (仮定形-融合 of ない)
      const nakerya = b.aux({ lemma: 'ない', text: 'なけりゃ', inflectionForm: '仮定形-融合' }, 'nakerya');

      // Potential form of iku in negative form: いけ
      const ike = b.verb(
        { lemma: 'いける', inflectionForm: '未然形-一般' },
        'ike'
      );

      // Negative auxiliary: ない
      const nai = b.tok({ text: 'ない', pos: 'AUX', lemma: 'ない' }, 'nai');

      // Structure constraints: verb → nakerya (auxOf or inOrder)
      b.either(
        (branch) => {
          branch.auxOf(verb, nakerya);
          branch.inOrder(nakerya, ike, 2);
        },
        (branch) => {
          branch.inOrder(verb, nakerya, 1);
          branch.inOrder(nakerya, ike, 2);
        }
      );
      b.inOrder(ike, nai, 1);

      // Capture from verb through ikenai
      b.captureSpan('なくちゃ-なきゃ', verb, nai);
    },

    // Pattern 2a: なくちゃ (verb[irrealis] + なく + ちゃ) - short form (implicit obligation)
    // e.g., 洗濯をしなくちゃ, 宿題をしなくちゃ
    // GiNZA: し[VERB,未然形] + なく[AUX,連用形] + ちゃ[SCONJ]
    (b) => {
      // Verb in irrealis form (未然形), or auxiliary する for suru-verbs
      const verb = b.tok({
        inflectionForm: '未然形-一般',
        posOneOf: ['VERB', 'AUX'],
      }, 'verb');

      // Negative auxiliary: なく (連用形 of ない)
      const naku = b.aux({ lemma: 'ない', inflectionForm: '連用形-一般' }, 'naku');

      // Contraction marker: ちゃ (end of sentence/phrase)
      const cha = b.tok({ text: 'ちゃ', pos: 'SCONJ' }, 'cha');

      // Structure constraints: verb → naku (auxOf or inOrder)
      b.either(
        (branch) => {
          branch.auxOf(verb, naku);
          branch.inOrder(naku, cha, 1);
        },
        (branch) => {
          branch.inOrder(verb, naku, 1);
          branch.inOrder(naku, cha, 1);
        }
      );

      // Capture from verb through cha
      b.captureSpan('なくちゃ-なきゃ', verb, cha);
    },

    // Pattern 2b: なきゃ (verb[irrealis] + なきゃ) - short form (implicit obligation)
    // e.g., お風呂に入らなきゃ, 買いに行かなきゃ
    // GiNZA: 入ら[VERB,未然形] + なきゃ[AUX,仮定形-融合]
    (b) => {
      // Verb in irrealis form (未然形), or auxiliary する for suru-verbs
      const verb = b.tok({
        inflectionForm: '未然形-一般',
        posOneOf: ['VERB', 'AUX'],
      }, 'verb');

      // Contraction auxiliary: なきゃ (仮定形-融合 of ない)
      const nakya = b.aux({ lemma: 'ない', text: 'なきゃ', inflectionForm: '仮定形-融合' }, 'nakya');

      // Structure constraints: verb → nakya (auxOf or inOrder)
      b.either(
        (branch) => branch.auxOf(verb, nakya),
        (branch) => branch.inOrder(verb, nakya, 1)
      );

      // Capture from verb through nakya
      b.captureSpan('なくちゃ-なきゃ', verb, nakya);
    },

    // Pattern 2c: なけりゃ (verb[irrealis] + なけりゃ) - short form
    // e.g., 勉強しなけりゃ
    // GiNZA: し[VERB,未然形] + なけりゃ[AUX,仮定形-融合]
    (b) => {
      // Verb in irrealis form (未然形), or auxiliary する for suru-verbs
      const verb = b.tok({
        inflectionForm: '未然形-一般',
        posOneOf: ['VERB', 'AUX'],
      }, 'verb');

      // Contraction auxiliary: なけりゃ (仮定形-融合 of ない)
      const nakerya = b.aux({ lemma: 'ない', text: 'なけりゃ', inflectionForm: '仮定形-融合' }, 'nakerya');

      // Structure constraints: verb → nakerya (auxOf or inOrder)
      b.either(
        (branch) => branch.auxOf(verb, nakerya),
        (branch) => branch.inOrder(verb, nakerya, 1)
      );

      // Capture from verb through nakerya
      b.captureSpan('なくちゃ-なきゃ', verb, nakerya);
    }
  );
});
