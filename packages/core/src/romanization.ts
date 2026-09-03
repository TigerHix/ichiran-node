import {
  CHAR_CLASS_HASH,
  MODIFIER_CHARACTERS,
  normalize,
  simplifyNgrams,
  voiceChar
} from './characters.js';
import type { RomanizationName } from './romanization-contract.js';

export { joinRomanizedParts, type RomanizationName } from './romanization-contract.js';

type KanaClass = string;
type KanaTree = (KanaClass | KanaTree | undefined)[];

const KANA_CLASS_NAMES = new Set(CHAR_CLASS_HASH.values());
const HINT_MODIFIER = '\u200c';
const HINT_SPACE = '\u200b';

export interface RomanizationMethod {
  base(item: KanaClass): string;
  apply(modifier: KanaClass, tree: KanaTree): string;
  simplify(input: string): string;
  special(input: string): string | null;
}

export function processHints(input: string): string {
  return simplifyNgrams(input, [
    [HINT_SPACE, ' '],
    [HINT_MODIFIER + 'は', 'わ'],
    [HINT_MODIFIER + 'ハ', 'ワ'],
    [HINT_MODIFIER + 'へ', 'え'],
    [HINT_MODIFIER + 'ヘ', 'エ'],
    [HINT_MODIFIER, '']
  ]);
}

export function stripHints(input: string): string {
  return Array.from(input, (character) => (
    character === HINT_MODIFIER || character === HINT_SPACE ? '' : character
  )).join('');
}

export function getCharacterClasses(input: string): KanaClass[] {
  return Array.from(input, (character) => CHAR_CLASS_HASH.get(character) ?? character);
}

export function processIterationCharacters(classes: readonly KanaClass[]): KanaClass[] {
  const output: KanaClass[] = [];
  let previous: KanaClass | undefined;
  for (const charClass of classes) {
    if (charClass === 'iter') {
      if (previous !== undefined) output.push(previous);
    } else if (charClass === 'iterV') {
      if (previous !== undefined) output.push(voiceChar(previous));
    } else {
      output.push(charClass);
      previous = charClass;
    }
  }
  return output;
}

export function processModifiers(classes: readonly KanaClass[]): KanaTree {
  const output: KanaTree = [];
  for (let index = 0; index < classes.length; index++) {
    const charClass = classes[index]!;
    if (charClass === 'sokuon') {
      output.push([charClass, ...processModifiers(classes.slice(index + 1))]);
      break;
    }
    if (Object.hasOwn(MODIFIER_CHARACTERS, charClass)) {
      output.push([charClass, output.pop()]);
    } else {
      output.push(charClass);
    }
  }
  return output;
}

export function leftmostAtom(tree: KanaTree): KanaClass {
  const first = tree[0];
  if (!Array.isArray(first)) return first ?? '';
  return leftmostAtom(first.slice(1));
}

export function romanizeCore(method: RomanizationMethod, tree: KanaTree): string {
  let output = '';
  for (const item of tree) {
    if (item === null || item === undefined) continue;
    if (typeof item === 'string' && !KANA_CLASS_NAMES.has(item)) {
      output += item;
    } else if (!Array.isArray(item)) {
      output += method.base(item);
    } else if (item.length > 0) {
      output += method.apply(item[0] as KanaClass, item.slice(1));
    }
  }
  return output;
}

const HEPBURN = new Map<KanaClass, string>([
  ['a', 'a'], ['i', 'i'], ['u', 'u'], ['e', 'e'], ['o', 'o'],
  ['ka', 'ka'], ['ki', 'ki'], ['ku', 'ku'], ['ke', 'ke'], ['ko', 'ko'],
  ['sa', 'sa'], ['shi', 'shi'], ['su', 'su'], ['se', 'se'], ['so', 'so'],
  ['ta', 'ta'], ['chi', 'chi'], ['tsu', 'tsu'], ['te', 'te'], ['to', 'to'],
  ['na', 'na'], ['ni', 'ni'], ['nu', 'nu'], ['ne', 'ne'], ['no', 'no'],
  ['ha', 'ha'], ['hi', 'hi'], ['fu', 'fu'], ['he', 'he'], ['ho', 'ho'],
  ['ma', 'ma'], ['mi', 'mi'], ['mu', 'mu'], ['me', 'me'], ['mo', 'mo'],
  ['ya', 'ya'], ['yu', 'yu'], ['yo', 'yo'],
  ['ra', 'ra'], ['ri', 'ri'], ['ru', 'ru'], ['re', 're'], ['ro', 'ro'],
  ['wa', 'wa'], ['wi', 'wi'], ['we', 'we'], ['wo', 'wo'], ['n', "n'"],
  ['ga', 'ga'], ['gi', 'gi'], ['gu', 'gu'], ['ge', 'ge'], ['go', 'go'],
  ['za', 'za'], ['ji', 'ji'], ['zu', 'zu'], ['ze', 'ze'], ['zo', 'zo'],
  ['da', 'da'], ['dji', 'ji'], ['dzu', 'zu'], ['de', 'de'], ['do', 'do'],
  ['ba', 'ba'], ['bi', 'bi'], ['bu', 'bu'], ['be', 'be'], ['bo', 'bo'],
  ['pa', 'pa'], ['pi', 'pi'], ['pu', 'pu'], ['pe', 'pe'], ['po', 'po'],
  ['+a', 'a'], ['+i', 'i'], ['+u', 'u'], ['+e', 'e'], ['+o', 'o'],
  ['+ya', 'ya'], ['+yu', 'yu'], ['+yo', 'yo'], ['+wa', 'wa'], ['vu', 'vu']
]);

const KUNREI = new Map<KanaClass, string>([
  ...HEPBURN,
  ['shi', 'si'], ['chi', 'ti'], ['tsu', 'tu'], ['fu', 'hu'],
  ['wi', 'i'], ['we', 'e'], ['wo', 'o'], ['ji', 'zi'], ['dji', 'zi']
]);

function applyDefault(
  modifier: KanaClass,
  method: RomanizationMethod,
  tree: KanaTree
): string {
  if (modifier === 'sokuon') {
    const inner = romanizeCore(method, tree);
    if (inner.length === 0 || inner.charCodeAt(0) > 127) return inner;
    return inner[0]! + inner;
  }
  if (modifier === 'longVowel') return romanizeCore(method, tree);
  return romanizeCore(method, tree) + modifier.toLowerCase();
}

class GenericRomanization implements RomanizationMethod {
  constructor(protected readonly table: ReadonlyMap<KanaClass, string>) {}

  base(item: KanaClass): string {
    return this.table.get(item) ?? item.toLowerCase();
  }

  apply(modifier: KanaClass, tree: KanaTree): string {
    const yoon = this.table.get(modifier);
    if (!yoon) return applyDefault(modifier, this, tree);
    const first = tree[0];
    if (first === 'u') return 'w' + yoon;
    if (first === 'a' || first === 'i' || first === 'e' || first === 'o') {
      return (this.table.get(first) ?? '') + yoon;
    }
    const inner = romanizeCore(this, tree);
    return inner.slice(0, Math.max(0, inner.length - 1)) + yoon;
  }

  simplify(input: string): string {
    return input;
  }

  special(input: string): string | null {
    if (input === 'っ') return '!';
    if (input === 'ー') return '~';
    return null;
  }
}

class Hepburn extends GenericRomanization {
  override apply(modifier: KanaClass, tree: KanaTree): string {
    if (modifier === 'sokuon') {
      if (leftmostAtom(tree) === 'chi') return 't' + romanizeCore(this, tree);
      return applyDefault(modifier, this, tree);
    }
    const first = tree[0];
    if (modifier === '+ya') {
      if (first === 'shi') return 'sha';
      if (first === 'chi') return 'cha';
      if (first === 'ji' || first === 'dji') return 'ja';
    } else if (modifier === '+yu') {
      if (first === 'shi') return 'shu';
      if (first === 'chi') return 'chu';
      if (first === 'ji' || first === 'dji') return 'ju';
    } else if (modifier === '+yo') {
      if (first === 'shi') return 'sho';
      if (first === 'chi') return 'cho';
      if (first === 'ji' || first === 'dji') return 'jo';
    }
    return super.apply(modifier, tree);
  }

  override simplify(input: string): string {
    return input.replace(/n'([^aiueoy]|$)/g, 'n$1');
  }
}

class SimplifiedHepburn extends Hepburn {
  constructor(
    private readonly replacements: readonly (readonly [string, string])[],
    table: ReadonlyMap<KanaClass, string> = HEPBURN
  ) {
    super(new Map(table));
  }

  override simplify(input: string): string {
    return simplifyNgrams(super.simplify(input), this.replacements);
  }
}

class TraditionalHepburn extends SimplifiedHepburn {
  constructor() {
    super([['oo', 'ō'], ['ou', 'ō'], ['uu', 'ū']]);
  }

  override simplify(input: string): string {
    return super.simplify(input)
      .replace(/n'([aiueoy])/g, 'n-$1')
      .replace(/n([mbp])/g, 'm$1');
  }
}

class KunreiSiki extends GenericRomanization {
  override simplify(input: string): string {
    return simplifyNgrams(input.replace(/n'([^aiueoy]|$)/g, 'n$1'), [
      ['oo', 'ô'], ['ou', 'ô'], ['uu', 'û']
    ]);
  }
}

const modifiedHepburnTable = new Map(HEPBURN);
modifiedHepburnTable.set('wo', 'o');

export const romanizationMethods: Readonly<Record<RomanizationName, RomanizationMethod>> = {
  'hepburn-basic': new Hepburn(new Map(HEPBURN)),
  'hepburn-simple': new SimplifiedHepburn([['oo', 'o'], ['ou', 'o'], ['uu', 'u']]),
  'hepburn-passport': new SimplifiedHepburn([['oo', 'oh'], ['ou', 'oh'], ['uu', 'u']]),
  'hepburn-traditional': new TraditionalHepburn(),
  'hepburn-modified': new SimplifiedHepburn(
    [['oo', 'ō'], ['ou', 'ō'], ['uu', 'ū'], ['aa', 'ā'], ['ee', 'ē']],
    modifiedHepburnTable
  ),
  'kunrei-siki': new KunreiSiki(new Map(KUNREI))
};

export function romanizeList(
  classes: readonly KanaClass[],
  method: RomanizationMethod = romanizationMethods['hepburn-traditional']
): string {
  const tree = processModifiers(processIterationCharacters(classes));
  return method.simplify(romanizeCore(method, tree));
}

export function romanizeWord(
  input: string,
  options: {
    readonly method?: RomanizationName | RomanizationMethod;
    readonly originalSpelling?: string;
    readonly normalize?: boolean;
  } = {}
): string {
  const method = typeof options.method === 'string'
    ? romanizationMethods[options.method]
    : options.method ?? romanizationMethods['hepburn-traditional'];
  const word = options.normalize === false ? input : normalize(input);
  const special = method.special(options.originalSpelling ?? word);
  if (special !== null) return special;
  return romanizeList(getCharacterClasses(processHints(word)), method);
}
