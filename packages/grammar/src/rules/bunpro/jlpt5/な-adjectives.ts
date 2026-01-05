import { bunproLinguisticRule } from '../../../engine/lang.js';

export default bunproLinguisticRule('な-adjectives', (r) => {
  // Na-adjectives (形容動詞): Adjectival nouns that conjugate like nouns
  //
  // Examples:
  // - 綺麗な絵 (na-adj + な + noun): attributive form
  // - 静かだ (na-adj + だ): predicate form (casual)
  // - 静かです (na-adj + です): predicate form (polite)
  //
  // GiNZA parsing notes:
  // - Na-adjectives are typically tagged as ADJ or NOUN depending on the word
  // - When followed by な, the auxiliary has lemma=だ (copula), pos=AUX
  // - We identify them by specific lemmas that are grammatically na-adjectives
  // - Some na-adjectives like きれい/綺麗 end in い but are grammatically na-adjectives
  // - Both kanji and hiragana lemmas need to be included (e.g., 大好き and だいすき)

  const naAdjLemmas = [
    'きれい', '綺麗',  // kirei (pretty)
    'たいへん', '大変',  // taihen (awful/tough)
    'しずか', '静か',  // shizuka (quiet)
    'ひま', '暇',  // hima (free)
    'たいせつ', '大切',  // taisetsu (important)
    'だいすき', '大好き',  // daisuki (love)
    'りっぱ', '立派',  // rippa (fine/splendid)
    'べんり', '便利',  // benri (convenient)
    'にぎやか', '賑やか',  // nigiyaka (lively)
    'ゆうめい', '有名',  // yuumei (famous)
    'じょうず', '上手',  // jouzu (skillful)
    'へた', '下手',  // heta (unskillful)
    'すき', '好き',  // suki (like)
    'きらい', '嫌い',  // kirai (hate)
    'だいじょうぶ', '大丈夫',  // daijoubu (okay)
    'かっこいい',  // kakkoii (cool)
    'げんき', '元気',  // genki (healthy/energetic)
    'しあわせ', '幸せ',  // shiawase (happy)
  ];

  r.either(
    // Branch 1: Na-adjective + な + noun (attributive)
    // Examples: 綺麗な絵, 大好きな野菜, 大変な授業
    // GiNZA: na-adj (pos=ADJ) + な (pos=AUX, lemma=だ)
    (b) => {
      const naAdj = b.tok({
        lemmaOneOf: naAdjLemmas,
        posOneOf: ['ADJ', 'NOUN'],
      }, 'naAdj');
      const na = b.aux({
        lemma: 'だ',
        text: 'な',
      }, 'na');
      // Require adjacency: na-adj immediately followed by な
      b.inOrder(naAdj, na, 1);
      b.captureSpan('な-adjectives', naAdj, na);
    },

    // Branch 2: Na-adjective + だ (predicate, casual)
    // Examples: 静かだ, 暇だ, 大好きだ
    // GiNZA: na-adj (pos=ADJ) + だ (pos=AUX, lemma=だ)
    (b) => {
      const naAdj = b.tok({
        lemmaOneOf: naAdjLemmas,
        posOneOf: ['ADJ', 'NOUN'],
      }, 'naAdj');
      const da = b.aux({
        lemma: 'だ',
        text: 'だ',
      }, 'da');
      // Require adjacency: na-adj immediately followed by だ
      b.inOrder(naAdj, da, 1);
      b.captureSpan('な-adjectives', naAdj, da);
    },

    // Branch 3: Na-adjective + です (predicate, polite)
    // Examples: 静かです, 綺麗です, 大変です
    // GiNZA: na-adj (pos=ADJ) + です (pos=AUX, lemma=です)
    (b) => {
      const naAdj = b.tok({
        lemmaOneOf: naAdjLemmas,
        posOneOf: ['ADJ', 'NOUN'],
      }, 'naAdj');
      const desu = b.aux({
        lemma: 'です',
        text: 'です',
      }, 'desu');
      // Require adjacency: na-adj immediately followed by です
      b.inOrder(naAdj, desu, 1);
      b.captureSpan('な-adjectives', naAdj, desu);
    }
  );
});
