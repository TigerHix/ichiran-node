import { linguisticRule } from '../../../engine/lang.js';

export default linguisticRule('そんな-こんな-あんな-どんな', (r) => {
  // そんな, こんな, あんな, どんな are casual pre-noun adjectivals (連体詞)
  // meaning "such/this/that/what kind of"
  // They are abbreviations of そのような/このような/あのような/どのような
  //
  // GiNZA parses these as:
  // - pos=PRON (pronoun/demonstrative)
  // - dep varies: nmod, obl, or dep (depends on context)
  // - head points to the noun they modify (or to a verb in clauses)
  //
  // The determiner and noun may have intervening adjectives (e.g., こんな高いもの)
  // Or may be followed by a nominalizer の (e.g., こんな高いの)
  //
  // Formal variants (このような, そのような, etc.) are sometimes parsed as:
  // - Single token: pos=DET, lemma=このような (rare)
  // - More commonly as: この(DET) + よう(NOUN) + な(AUX)
  //
  // Examples from Bunpro:
  // - そんな言い方してはいけません。 (such a way of speaking)
  // - こんな高いものはもらえないよ。 (this expensive thing)
  // - あんな大人にはなりたくない。 (that kind of adult)
  // - どんな靴が欲しい？ (what kind of shoes)
  // - このような場合にはどうすればいいですか。 (formal)

  r.either(
    // Branch 1: そんな (casual)
    (branch) => {
      const sonna = branch.tok({ textOneOf: ['そんな', '其んな'], pos: 'PRON', depOneOf: ['nmod', 'obl', 'dep'] }, 'det');
      const noun = branch.tok({ posOneOf: ['NOUN', 'SCONJ'] }, 'noun');
      branch.inOrder(sonna, noun); // No maxDistance - allow adjectives between
      branch.captureAs('そんな', sonna);
    },
    // Branch 2: こんな (casual)
    (branch) => {
      const konna = branch.tok({ textOneOf: ['こんな', '此んな'], pos: 'PRON', depOneOf: ['nmod', 'obl', 'dep'] }, 'det');
      const noun = branch.tok({ posOneOf: ['NOUN', 'SCONJ'] }, 'noun');
      branch.inOrder(konna, noun);
      branch.captureAs('こんな', konna);
    },
    // Branch 3: あんな (casual)
    (branch) => {
      const anna = branch.tok({ textOneOf: ['あんな', '彼んな'], pos: 'PRON', depOneOf: ['nmod', 'obl', 'dep'] }, 'det');
      const noun = branch.tok({ posOneOf: ['NOUN', 'SCONJ'] }, 'noun');
      branch.inOrder(anna, noun);
      branch.captureAs('あんな', anna);
    },
    // Branch 4: どんな (casual)
    (branch) => {
      const donna = branch.tok({ textOneOf: ['どんな', '何んな'], pos: 'PRON', depOneOf: ['nmod', 'obl', 'dep'] }, 'det');
      const noun = branch.tok({ posOneOf: ['NOUN', 'SCONJ'] }, 'noun');
      branch.inOrder(donna, noun);
      branch.captureAs('どんな', donna);
    },
    // Branch 5: このような (formal - split: この + よう + な)
    (branch) => {
      const kono = branch.tok({ text: 'この', pos: 'DET', dep: 'det' }, 'det1');
      const you = branch.noun({ lemma: 'よう' }, 'you');
      branch.headChild(you, kono, 'det');
      const na = branch.aux({ lemma: 'だ' }, 'na');
      branch.copulaOf(you, na);
      const noun = branch.tok({ posOneOf: ['NOUN', 'SCONJ'] }, 'noun');
      branch.inOrder(kono, noun);
      branch.captureSpan('こんな', kono, na);
    },
    // Branch 6: そのような (formal - split)
    (branch) => {
      const sono = branch.tok({ text: 'その', pos: 'DET', dep: 'det' }, 'det1');
      const you = branch.noun({ lemma: 'よう' }, 'you');
      branch.headChild(you, sono, 'det');
      const na = branch.aux({ lemma: 'だ' }, 'na');
      branch.copulaOf(you, na);
      const noun = branch.tok({ posOneOf: ['NOUN', 'SCONJ'] }, 'noun');
      branch.inOrder(sono, noun);
      branch.captureSpan('そんな', sono, na);
    },
    // Branch 7: あのような (formal - split)
    (branch) => {
      const ano = branch.tok({ text: 'あの', pos: 'DET', dep: 'det' }, 'det1');
      const you = branch.noun({ lemma: 'よう' }, 'you');
      branch.headChild(you, ano, 'det');
      const na = branch.aux({ lemma: 'だ' }, 'na');
      branch.copulaOf(you, na);
      const noun = branch.tok({ posOneOf: ['NOUN', 'SCONJ'] }, 'noun');
      branch.inOrder(ano, noun);
      branch.captureSpan('あんな', ano, na);
    },
    // Branch 8: どのような (formal - single token, or split)
    (branch) => {
      const donoyouna = branch.tok({ text: 'どのような', pos: 'DET', dep: 'acl' }, 'det');
      const noun = branch.tok({ posOneOf: ['NOUN', 'SCONJ'] }, 'noun');
      branch.inOrder(donoyouna, noun);
      branch.captureAs('どんな', donoyouna);
    },
    // Branch 9: どのような (formal - split variant)
    (branch) => {
      const dono = branch.tok({ text: 'どの', pos: 'DET', dep: 'det' }, 'det1');
      const you = branch.noun({ lemma: 'よう' }, 'you');
      branch.headChild(you, dono, 'det');
      const na = branch.aux({ lemma: 'だ' }, 'na');
      branch.copulaOf(you, na);
      const noun = branch.tok({ posOneOf: ['NOUN', 'SCONJ'] }, 'noun');
      branch.inOrder(dono, noun);
      branch.captureSpan('どんな', dono, na);
    }
  );
});
