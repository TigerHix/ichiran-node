/**
 * All Part-of-Speech tags used by ichiran/JMdict
 * These are the actual tags from the sense_prop table
 */
export type IchiranPos =
  // Adjectives
  | 'adj-f'      // noun or verb acting prenominally
  | 'adj-i'      // adjective (keiyoushi)
  | 'adj-ix'     // adjective (keiyoushi) - yoi/ii class
  | 'adj-ku'     // `ku' adjective (archaic)
  | 'adj-na'     // adjectival nouns or quasi-adjectives (keiyoudoushi)
  | 'adj-nari'   // archaic/formal form of na-adjective
  | 'adj-no'     // nouns which may take the genitive case particle `no'
  | 'adj-pn'     // pre-noun adjectival (rentaishi)
  | 'adj-shiku'  // `shiku' adjective (archaic)
  | 'adj-t'      // `taru' adjective
  // Adverbs
  | 'adv'        // adverb (fukushi)
  | 'adv-to'     // adverb taking the `to' particle
  // Auxiliary
  | 'aux'        // auxiliary
  | 'aux-adj'    // auxiliary adjective
  | 'aux-v'      // auxiliary verb
  // Conjunction
  | 'conj'       // conjunction
  // Copula
  | 'cop'        // copula
  | 'cop-da'     // copula da
  // Counter
  | 'ctr'        // counter
  // Expression
  | 'exp'        // expressions (phrases, clauses, etc.)
  // Interjection
  | 'int'        // interjection (kandoushi)
  // Nouns
  | 'n'          // noun (common) (futsuumeishi)
  | 'n-pref'     // noun, used as a prefix
  | 'n-suf'      // noun, used as a suffix
  // Numeric
  | 'num'        // numeric
  // Pronoun
  | 'pn'         // pronoun
  // Prefix
  | 'pref'       // prefix
  // Particle
  | 'prt'        // particle
  // Suffix
  | 'suf'        // suffix
  // Unclassified
  | 'unc'        // unclassified
  // Verbs - Ichidan
  | 'v1'         // Ichidan verb
  | 'v1-s'       // Ichidan verb - kureru special class
  // Verbs - Nidan (archaic)
  | 'v2a-s'      // Nidan verb with 'u' ending (archaic)
  | 'v2b-k'      // Nidan verb (upper class) with `bu' ending (archaic)
  | 'v2d-s'      // Nidan verb (lower class) with `dzu' ending (archaic)
  | 'v2g-k'      // Nidan verb (upper class) with `gu' ending (archaic)
  | 'v2g-s'      // Nidan verb (lower class) with `gu' ending (archaic)
  | 'v2h-k'      // Nidan verb (upper class) with `hu/fu' ending (archaic)
  | 'v2h-s'      // Nidan verb (lower class) with `hu/fu' ending (archaic)
  | 'v2k-k'      // Nidan verb (upper class) with `ku' ending (archaic)
  | 'v2k-s'      // Nidan verb (lower class) with `ku' ending (archaic)
  | 'v2m-s'      // Nidan verb (lower class) with `mu' ending (archaic)
  | 'v2n-s'      // Nidan verb (lower class) with `nu' ending (archaic)
  | 'v2r-k'      // Nidan verb (upper class) with `ru' ending (archaic)
  | 'v2r-s'      // Nidan verb (lower class) with `ru' ending (archaic)
  | 'v2s-s'      // Nidan verb (lower class) with `su' ending (archaic)
  | 'v2t-k'      // Nidan verb (upper class) with `tsu' ending (archaic)
  | 'v2t-s'      // Nidan verb (lower class) with `tsu' ending (archaic)
  | 'v2w-s'      // Nidan verb (lower class) with `uwe' ending (archaic)
  | 'v2y-k'      // Nidan verb (upper class) with `yu' ending (archaic)
  | 'v2y-s'      // Nidan verb (lower class) with `yu' ending (archaic)
  | 'v2z-s'      // Nidan verb (lower class) with `zu' ending (archaic)
  // Verbs - Yodan (archaic)
  | 'v4b'        // Yodan verb with `bu' ending (archaic)
  | 'v4g'        // Yodan verb with `gu' ending (archaic)
  | 'v4h'        // Yodan verb with `hu/fu' ending (archaic)
  | 'v4k'        // Yodan verb with `ku' ending (archaic)
  | 'v4m'        // Yodan verb with `mu' ending (archaic)
  | 'v4r'        // Yodan verb with `ru' ending (archaic)
  | 'v4s'        // Yodan verb with `su' ending (archaic)
  | 'v4t'        // Yodan verb with `tsu' ending (archaic)
  // Verbs - Godan
  | 'v5aru'      // Godan verb - -aru special class
  | 'v5b'        // Godan verb with `bu' ending
  | 'v5g'        // Godan verb with `gu' ending
  | 'v5k'        // Godan verb with `ku' ending
  | 'v5k-s'      // Godan verb - Iku/Yuku special class
  | 'v5m'        // Godan verb with `mu' ending
  | 'v5n'        // Godan verb with `nu' ending
  | 'v5r'        // Godan verb with `ru' ending
  | 'v5r-i'      // Godan verb with `ru' ending (irregular verb)
  | 'v5s'        // Godan verb with `su' ending
  | 'v5t'        // Godan verb with `tsu' ending
  | 'v5u'        // Godan verb with `u' ending
  | 'v5u-s'      // Godan verb with `u' ending (special class)
  // Verbs - other types
  | 'vi'         // intransitive verb
  | 'vk'         // Kuru verb - special class
  | 'vn'         // irregular nu verb
  | 'vr'         // irregular ru verb, plain form ends with -ri
  | 'vs'         // noun or participle which takes the aux. verb suru
  | 'vs-c'       // su verb - precursor to the modern suru
  | 'vs-i'       // suru verb - included
  | 'vs-s'       // suru verb - special class
  | 'vt'         // transitive verb
  | 'vz'         // Ichidan verb - zuru verb (alternative form of -jiru verbs);

/**
 * Helper functions to categorize POS tags
 */
export const PosHelpers = {
  isNoun: (pos: IchiranPos | 'unknown'): boolean => {
    return pos === 'n' || pos === 'pn' || pos === 'n-suf' || pos === 'n-pref';
  },
  
  isVerb: (pos: IchiranPos | 'unknown'): boolean => {
    return pos.startsWith('v') || pos === 'vi' || pos === 'vt';
  },
  
  isAdjective: (pos: IchiranPos | 'unknown'): boolean => {
    // Exclude adj-pn (pronominal/pre-noun adjectives like 何の, この, etc.)
    // These are determiners, not predicate adjectives
    return pos.startsWith('adj-') && pos !== 'adj-pn';
  },
  
  isIAdjective: (pos: IchiranPos | 'unknown'): boolean => {
    return pos === 'adj-i' || pos === 'adj-ix';
  },
  
  isNaAdjective: (pos: IchiranPos | 'unknown'): boolean => {
    return pos === 'adj-na';
  },
  
  isAdverb: (pos: IchiranPos | 'unknown'): boolean => {
    return pos === 'adv' || pos === 'adv-to';
  },
  
  isParticle: (pos: IchiranPos | 'unknown'): boolean => {
    return pos === 'prt';
  },
  
  isAuxiliary: (pos: IchiranPos | 'unknown'): boolean => {
    return pos === 'aux' || pos === 'aux-v' || pos === 'aux-adj' || pos === 'cop' || pos === 'cop-da';
  },
  
  isCopula: (pos: IchiranPos | 'unknown'): boolean => {
    return pos === 'cop' || pos === 'cop-da';
  },
  
  isPrenounAdjective: (pos: IchiranPos | 'unknown'): boolean => {
    return pos === 'adj-pn';
  },
  
  isDeterminer: (pos: IchiranPos | 'unknown'): boolean => {
    // Pre-noun adjectives (この, その, あの, etc.) act as determiners
    return pos === 'adj-pn';
  },

  isPronoun: (pos: IchiranPos | 'unknown'): boolean => {
    return pos === 'pn';
  },
};

