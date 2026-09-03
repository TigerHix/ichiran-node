export type PartOfSpeechCategory =
  | 'noun'
  | 'verb'
  | 'adjective'
  | 'adverb'
  | 'particle'
  | 'auxiliary'
  | 'conjunction'
  | 'pronoun'
  | 'copula'
  | 'interjection'
  | 'counter'
  | 'expression'
  | 'numeric'
  | 'prefix-suffix'
  | 'other';

// Keep these labels in lockstep with Komi's learner-facing vocabulary.
const PART_OF_SPEECH_LABELS: Readonly<Record<string, string>> = {
  'adj-i': 'I-Adjective',
  'adj-ix': 'I-Adjective (Archaic)',
  'adj-na': 'Na-Adjective',
  'adj-no': 'No-Adjective',
  'adj-pn': 'Pre-noun Adjective',
  'adj-t': 'Taru-Adjective',
  'adj-f': 'Prenominal',
  adv: 'Adverb',
  'adv-to': 'To-Adverb',
  aux: 'Auxiliary',
  'aux-v': 'Auxiliary Verb',
  'aux-adj': 'Auxiliary Adjective',
  conj: 'Conjunction',
  cop: 'Copula',
  'cop-da': 'Copula (だ)',
  ctr: 'Counter',
  exp: 'Expression',
  int: 'Interjection',
  n: 'Noun',
  'n-adv': 'Adverbial Noun',
  'n-suf': 'Noun Suffix',
  'n-pref': 'Noun Prefix',
  'n-t': 'Temporal Noun',
  'n-pr': 'Proper Noun',
  num: 'Number',
  pn: 'Pronoun',
  prt: 'Particle',
  pref: 'Prefix',
  suf: 'Suffix',
  v1: 'Ichidan Verb (-ru)',
  'v1-s': 'Ichidan Verb (-ru Special)',
  v5aru: 'Godan Verb (-aru)',
  v5b: 'Godan Verb (-bu)',
  v5g: 'Godan Verb (-gu)',
  v5k: 'Godan Verb (-ku)',
  'v5k-s': 'Godan Verb (-ku Special)',
  v5m: 'Godan Verb (-mu)',
  v5n: 'Godan Verb (-nu)',
  v5r: 'Godan Verb (-ru)',
  'v5r-i': 'Godan Verb (-ru Irregular)',
  v5s: 'Godan Verb (-su)',
  v5t: 'Godan Verb (-tsu)',
  v5u: 'Godan Verb (-u)',
  'v5u-s': 'Godan Verb (-u Special)',
  vk: 'Kuru Verb',
  vs: 'Suru Verb',
  'vs-i': 'Suru Verb (Included)',
  'vs-s': 'Suru Verb (Special)',
  vt: 'Transitive Verb',
  vi: 'Intransitive Verb',
  vz: 'Zuru Verb',
  v5uru: 'Godan Verb (-uru)',
  'on-mim': 'Onomatopoeia',
  unc: 'Unclassified',

  // JMdict also contains uncommon and historical classes. These are not in
  // Komi's usual result set, but they should still never leak opaque codes.
  'adj-kari': 'Kari-Adjective (Archaic)',
  'adj-ku': 'Ku-Adjective (Archaic)',
  'adj-shiku': 'Shiku-Adjective (Archaic)',
  'adj-nari': 'Na-Adjective (Archaic)',
  'v-unspec': 'Verb (Unspecified)',
  vn: 'Nu Verb (Irregular)',
  vr: 'Ru Verb (Irregular)',
  'vs-c': 'Su Verb (Archaic)',
  v4b: 'Yodan Verb (-bu, Archaic)',
  v4g: 'Yodan Verb (-gu, Archaic)',
  v4h: 'Yodan Verb (-fu, Archaic)',
  v4k: 'Yodan Verb (-ku, Archaic)',
  v4m: 'Yodan Verb (-mu, Archaic)',
  v4n: 'Yodan Verb (-nu, Archaic)',
  v4r: 'Yodan Verb (-ru, Archaic)',
  v4s: 'Yodan Verb (-su, Archaic)',
  v4t: 'Yodan Verb (-tsu, Archaic)',
  'v2a-s': 'Nidan Verb (-u, Archaic)',
  'v2b-k': 'Upper Nidan Verb (-bu, Archaic)',
  'v2b-s': 'Lower Nidan Verb (-bu, Archaic)',
  'v2d-k': 'Upper Nidan Verb (-dzu, Archaic)',
  'v2d-s': 'Lower Nidan Verb (-dzu, Archaic)',
  'v2g-k': 'Upper Nidan Verb (-gu, Archaic)',
  'v2g-s': 'Lower Nidan Verb (-gu, Archaic)',
  'v2h-k': 'Upper Nidan Verb (-fu, Archaic)',
  'v2h-s': 'Lower Nidan Verb (-fu, Archaic)',
  'v2k-k': 'Upper Nidan Verb (-ku, Archaic)',
  'v2k-s': 'Lower Nidan Verb (-ku, Archaic)',
  'v2m-k': 'Upper Nidan Verb (-mu, Archaic)',
  'v2m-s': 'Lower Nidan Verb (-mu, Archaic)',
  'v2n-s': 'Lower Nidan Verb (-nu, Archaic)',
  'v2r-k': 'Upper Nidan Verb (-ru, Archaic)',
  'v2r-s': 'Lower Nidan Verb (-ru, Archaic)',
  'v2s-s': 'Lower Nidan Verb (-su, Archaic)',
  'v2t-k': 'Upper Nidan Verb (-tsu, Archaic)',
  'v2t-s': 'Lower Nidan Verb (-tsu, Archaic)',
  'v2w-s': 'Lower Nidan Verb (-u, Archaic)',
  'v2y-k': 'Upper Nidan Verb (-yu, Archaic)',
  'v2y-s': 'Lower Nidan Verb (-yu, Archaic)',
  'v2z-s': 'Lower Nidan Verb (-zu, Archaic)'
};

const CONJUGATION_LABELS: Readonly<Record<number, string>> = {
  1: 'Non-past',
  2: 'Past (~ta)',
  3: 'Conjunctive (~te)',
  4: 'Provisional (~eba)',
  5: 'Potential',
  6: 'Passive',
  7: 'Causative',
  8: 'Causative-Passive',
  9: 'Volitional',
  10: 'Imperative',
  11: 'Conditional (~tara)',
  12: 'Alternative (~tari)',
  13: 'Continuative (~i)',
  50: 'Adverbial',
  51: 'Adjective Stem',
  52: 'Negative Stem',
  53: 'Causative (~su)',
  54: 'Old/literary form'
};

export function partOfSpeechLabel(value: string): string {
  return PART_OF_SPEECH_LABELS[value] ?? value
    .split('-')
    .filter(Boolean)
    .map(part => part.charAt(0).toUpperCase() + part.slice(1))
    .join(' ');
}

export function partOfSpeechCategory(value: string): PartOfSpeechCategory {
  if (value === 'n' || value.startsWith('n-')) return 'noun';
  if (value.startsWith('v') || value === 'vi' || value === 'vt') return 'verb';
  if (value.startsWith('adj')) return 'adjective';
  if (value.startsWith('adv')) return 'adverb';
  if (value === 'prt') return 'particle';
  if (value.startsWith('aux')) return 'auxiliary';
  if (value === 'conj') return 'conjunction';
  if (value === 'pn') return 'pronoun';
  if (value.startsWith('cop')) return 'copula';
  if (value === 'int') return 'interjection';
  if (value === 'ctr') return 'counter';
  if (value === 'exp') return 'expression';
  if (value === 'num') return 'numeric';
  if (value === 'pref' || value === 'suf') return 'prefix-suffix';
  return 'other';
}

export function conjugationLabel(type: number): string {
  return CONJUGATION_LABELS[type] ?? `Conjugation ${type}`;
}
