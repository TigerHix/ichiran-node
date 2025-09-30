// Transliteration module (for hiragana / katakana only)
// Port of romanize.lisp

import { CHAR_CLASS_HASH, MODIFIER_CHARACTERS, voiceChar, basicSplit, normalize, simplifyNgrams } from './characters.js';
import { processHints, stripHints } from './dict-split.js';
import { dictSegment, simpleSegment, wordInfoStr, WordInfo, getKana, resetCalcScoreCache } from './dict.js';

// Character class type - in TypeScript these are strings, not symbols
type CharClass = string;

// Build a set of all character class names (values in CHAR_CLASS_HASH)
// This distinguishes character class names from plain characters
const CHARACTER_CLASS_NAMES = new Set(CHAR_CLASS_HASH.values());

// Line 5-7: defun get-character-classes
/**
 * Transforms a word (or a string) into a list of character classes
 *
 * Lisp source:
 * (defun get-character-classes (word)
 *   "Transforms a word (or a string) into a list of character classes"
 *   (map 'list (lambda (char) (gethash char *char-class-hash* char)) word))
 */
export function getCharacterClasses(word: string): CharClass[] {
  return Array.from(word).map(char => CHAR_CLASS_HASH.get(char) ?? char);
}

// Line 9-15: defun process-iteration-characters
/**
 * Replaces iteration characters in a character class list
 *
 * Lisp source:
 * (defun process-iteration-characters (cc-list)
 *   "Replaces iteration characters in a character class list"
 *   (loop with prev
 *      for cc in cc-list
 *      if (eql cc :iter) if prev collect prev end
 *      else if (eql cc :iter-v) if prev collect (voice-char prev) end
 *      else collect cc and do (setf prev cc)))
 */
export function processIterationCharacters(ccList: CharClass[]): CharClass[] {
  const result: CharClass[] = [];
  let prev: CharClass | undefined;

  for (const cc of ccList) {
    if (cc === 'iter') {
      // Line 12: Repeat previous character
      if (prev !== undefined) result.push(prev);
    } else if (cc === 'iterV') {
      // Line 13: Repeat previous character with voicing
      if (prev !== undefined) {
        const voiced = voiceChar(prev);
        result.push(voiced);
      }
    } else {
      // Line 14: Collect and remember
      result.push(cc);
      prev = cc;
    }
  }

  return result;
}

// Type for character class tree (atoms and nested structures)
type CCTree = (CharClass | CCTree)[];

// Line 17-25: defun process-modifiers
/**
 * Processes modifier characters (sokuon, etc.) into nested structures
 *
 * Lisp source:
 * (defun process-modifiers (cc-list)
 *   (loop with result
 *        for (cc . rest) on cc-list
 *        if (eql cc :sokuon)
 *          do (push (cons cc (process-modifiers rest)) result) (loop-finish)
 *        else if (member cc *modifier-characters*)
 *          do (push (list cc (pop result)) result)
 *        else do (push cc result)
 *        finally (return (nreverse result))))
 */
export function processModifiers(ccList: CharClass[]): CCTree {
  const result: any[] = [];
  const modifierKeys = Object.keys(MODIFIER_CHARACTERS);

  for (let i = 0; i < ccList.length; i++) {
    const cc = ccList[i];

    if (cc === 'sokuon') {
      // Line 20: Recursively process rest of list and push as nested structure
      const rest = ccList.slice(i + 1);
      result.push([cc, ...processModifiers(rest)]);
      break; // loop-finish
    } else if (modifierKeys.includes(cc)) {
      // Line 22-23: Pop last result and wrap with modifier
      const popped = result.pop();
      result.push([cc, popped]);
    } else {
      // Line 24: Just add to result
      result.push(cc);
    }
  }

  return result;
}

// Line 27-29: defun leftmost-atom
/**
 * Gets the leftmost atom in a character class tree
 *
 * Lisp source:
 * (defun leftmost-atom (cc-list &aux (first (car cc-list)))
 *   (cond ((atom first) first)
 *         (t (leftmost-atom (cdr first)))))
 */
export function leftmostAtom(ccList: CCTree): CharClass {
  const first = ccList[0];

  if (!Array.isArray(first)) {
    return first;
  } else {
    // cdr first = rest of array starting at index 1
    return leftmostAtom(first.slice(1));
  }
}

// Romanization method interface
export interface RomanizationMethod {
  rBase(item: CharClass): string;
  rApply(modifier: CharClass, ccTree: CCTree): string;
  rSimplify(str: string): string;
  rSpecial?(word: string): string | null;
}

// Line 31-37: defun romanize-core
/**
 * Core romanization function that processes a character class tree
 *
 * Lisp source:
 * (defun romanize-core (method cc-tree)
 *   (with-output-to-string (out)
 *     (dolist (item cc-tree)
 *       (cond ((null item))
 *             ((characterp item) (princ item out))
 *             ((atom item) (princ (r-base method item) out))
 *             ((listp item) (princ (r-apply (car item) method (cdr item)) out))))))
 */
export function romanizeCore(method: RomanizationMethod, ccTree: CCTree): string {
  let out = '';

  for (const item of ccTree) {
    if (item === null || item === undefined) {
      // Line 34: (null item) - skip
    } else if (typeof item === 'string' && !CHARACTER_CLASS_NAMES.has(item)) {
      // Line 35: (characterp item) - plain character that's not a character class name
      // In Lisp, character classes are symbols. In TypeScript, we distinguish them
      // by checking if they're in CHARACTER_CLASS_NAMES (values from CHAR_CLASS_HASH)
      out += item;
    } else if (!Array.isArray(item)) {
      // Line 36: (atom item) - character class
      out += method.rBase(item as CharClass);
    } else if (Array.isArray(item) && item.length > 0) {
      // Line 37: (listp item) - nested structure [modifier, ...rest]
      const modifier = item[0] as CharClass;
      const rest = item.slice(1) as CCTree;
      out += method.rApply(modifier, rest);
    }
  }

  return out;
}

// Base implementation for r-base
// Line 39-42: defgeneric r-base with default method
function rBaseDefault(item: CharClass): string {
  return String(item).toLowerCase();
}

// Line 210-215: defmethod r-special (base implementation)
/**
 * Romanize words that are exceptions
 *
 * Lisp source:
 * (defmethod r-special or (method word)
 *            (cond ((equal word "っ") "!")
 *                  ((equal word "ー") "~")))
 */
function rSpecialDefault(word: string): string | null {
  if (word === 'っ') return '!';
  if (word === 'ー') return '~';
  return null;
}

// Line 44-56: defgeneric r-apply with default methods
function rApplyDefault(modifier: CharClass, method: RomanizationMethod, ccTree: CCTree): string {
  if (modifier === 'sokuon') {
    // Line 46-51: sokuon handling - double first consonant
    const inner = romanizeCore(method, ccTree);
    if (inner.length === 0) return inner;

    const firstChar = inner[0];
    // Check if Basic Latin (ASCII) - cl-unicode:has-property check
    if (firstChar.charCodeAt(0) > 127) return inner;

    return firstChar + inner;
  } else if (modifier === 'longVowel') {
    // Line 52-53: long vowel - just romanize as-is
    return romanizeCore(method, ccTree);
  } else {
    // Line 54-55: default - append modifier name
    return romanizeCore(method, ccTree) + String(modifier).toLowerCase();
  }
}

// Line 57-59: defgeneric r-simplify with default method
function rSimplifyDefault(str: string): string {
  return str;
}

export { rBaseDefault, rApplyDefault, rSimplifyDefault, rSpecialDefault };

// ============================================================================
// Romanization Method Classes
// ============================================================================

// Line 81-101: Hash table for Hepburn kana mappings
const hepburnKanaTable = new Map<CharClass, string>([
  ['a', 'a'],      ['i', 'i'],      ['u', 'u'],      ['e', 'e'],      ['o', 'o'],
  ['ka', 'ka'],    ['ki', 'ki'],    ['ku', 'ku'],    ['ke', 'ke'],    ['ko', 'ko'],
  ['sa', 'sa'],    ['shi', 'shi'],  ['su', 'su'],    ['se', 'se'],    ['so', 'so'],
  ['ta', 'ta'],    ['chi', 'chi'],  ['tsu', 'tsu'],  ['te', 'te'],    ['to', 'to'],
  ['na', 'na'],    ['ni', 'ni'],    ['nu', 'nu'],    ['ne', 'ne'],    ['no', 'no'],
  ['ha', 'ha'],    ['hi', 'hi'],    ['fu', 'fu'],    ['he', 'he'],    ['ho', 'ho'],
  ['ma', 'ma'],    ['mi', 'mi'],    ['mu', 'mu'],    ['me', 'me'],    ['mo', 'mo'],
  ['ya', 'ya'],                     ['yu', 'yu'],                     ['yo', 'yo'],
  ['ra', 'ra'],    ['ri', 'ri'],    ['ru', 'ru'],    ['re', 're'],    ['ro', 'ro'],
  ['wa', 'wa'],    ['wi', 'wi'],                     ['we', 'we'],    ['wo', 'wo'],
  ['n', "n'"],
  ['ga', 'ga'],    ['gi', 'gi'],    ['gu', 'gu'],    ['ge', 'ge'],    ['go', 'go'],
  ['za', 'za'],    ['ji', 'ji'],    ['zu', 'zu'],    ['ze', 'ze'],    ['zo', 'zo'],
  ['da', 'da'],    ['dji', 'ji'],   ['dzu', 'zu'],   ['de', 'de'],    ['do', 'do'],
  ['ba', 'ba'],    ['bi', 'bi'],    ['bu', 'bu'],    ['be', 'be'],    ['bo', 'bo'],
  ['pa', 'pa'],    ['pi', 'pi'],    ['pu', 'pu'],    ['pe', 'pe'],    ['po', 'po'],
  ['+a', 'a'],     ['+i', 'i'],     ['+u', 'u'],     ['+e', 'e'],     ['+o', 'o'],
  ['+ya', 'ya'],                    ['+yu', 'yu'],                    ['+yo', 'yo'],
  ['vu', 'vu'],    ['+wa', 'wa']
]);

// Line 172-192: Hash table for Kunrei-siki kana mappings
const kunreiSikiKanaTable = new Map<CharClass, string>([
  ['a', 'a'],      ['i', 'i'],      ['u', 'u'],      ['e', 'e'],      ['o', 'o'],
  ['ka', 'ka'],    ['ki', 'ki'],    ['ku', 'ku'],    ['ke', 'ke'],    ['ko', 'ko'],
  ['sa', 'sa'],    ['shi', 'si'],   ['su', 'su'],    ['se', 'se'],    ['so', 'so'],
  ['ta', 'ta'],    ['chi', 'ti'],   ['tsu', 'tu'],   ['te', 'te'],    ['to', 'to'],
  ['na', 'na'],    ['ni', 'ni'],    ['nu', 'nu'],    ['ne', 'ne'],    ['no', 'no'],
  ['ha', 'ha'],    ['hi', 'hi'],    ['fu', 'hu'],    ['he', 'he'],    ['ho', 'ho'],
  ['ma', 'ma'],    ['mi', 'mi'],    ['mu', 'mu'],    ['me', 'me'],    ['mo', 'mo'],
  ['ya', 'ya'],                     ['yu', 'yu'],                     ['yo', 'yo'],
  ['ra', 'ra'],    ['ri', 'ri'],    ['ru', 'ru'],    ['re', 're'],    ['ro', 'ro'],
  ['wa', 'wa'],    ['wi', 'i'],                      ['we', 'e'],     ['wo', 'o'],
  ['n', "n'"],
  ['ga', 'ga'],    ['gi', 'gi'],    ['gu', 'gu'],    ['ge', 'ge'],    ['go', 'go'],
  ['za', 'za'],    ['ji', 'zi'],    ['zu', 'zu'],    ['ze', 'ze'],    ['zo', 'zo'],
  ['da', 'da'],    ['dji', 'zi'],   ['dzu', 'zu'],   ['de', 'de'],    ['do', 'do'],
  ['ba', 'ba'],    ['bi', 'bi'],    ['bu', 'bu'],    ['be', 'be'],    ['bo', 'bo'],
  ['pa', 'pa'],    ['pi', 'pi'],    ['pu', 'pu'],    ['pe', 'pe'],    ['po', 'po'],
  ['+a', 'a'],     ['+i', 'i'],     ['+u', 'u'],     ['+e', 'e'],     ['+o', 'o'],
  ['+ya', 'ya'],                    ['+yu', 'yu'],                    ['+yo', 'yo'],
  ['vu', 'vu'],    ['+wa', 'wa']
]);

// Line 62-67: defclass generic-romanization
class GenericRomanization implements RomanizationMethod {
  protected kanaTable: Map<CharClass, string>;

  constructor() {
    this.kanaTable = new Map();
  }

  // Line 66-67: defmethod r-base for generic-romanization
  rBase(item: CharClass): string {
    return this.kanaTable.get(item) ?? rBaseDefault(item);
  }

  // Line 69-77: defmethod r-apply for generic-romanization with yoon handling
  rApply(modifier: CharClass, ccTree: CCTree): string {
    const yoon = this.kanaTable.get(modifier);

    if (yoon) {
      const first = ccTree[0];

      // Line 72-74: Special cases for vowel combinations
      if (first === 'u') {
        return 'w' + yoon;
      } else if (['a', 'i', 'e', 'o'].includes(first as string)) {
        const base = this.kanaTable.get(first as CharClass) ?? '';
        return base + yoon;
      } else {
        // Line 75-76: General case - replace last char
        const inner = romanizeCore(this, ccTree);
        const len = inner.length;
        return inner.substring(0, Math.max(0, len - 1)) + yoon;
      }
    }

    return rApplyDefault(modifier, this, ccTree);
  }

  rSimplify(str: string): string {
    return rSimplifyDefault(str);
  }

  rSpecial(word: string): string | null {
    return rSpecialDefault(word);
  }
}

// Line 103-109: defclass generic-hepburn
class GenericHepburn extends GenericRomanization {
  constructor() {
    super();
    // Line 104: Copy Hepburn kana table
    this.kanaTable = new Map(hepburnKanaTable);
  }

  // Line 106-109: defmethod r-apply for :sokuon with chi special case
  rApply(modifier: CharClass, ccTree: CCTree): string {
    if (modifier === 'sokuon') {
      // Line 107-108: Special handling for chi
      if (leftmostAtom(ccTree) === 'chi') {
        return 't' + romanizeCore(this, ccTree);
      }
      // Fall through to default sokuon handling
      return rApplyDefault(modifier, this, ccTree);
    }

    // Line 111-130: Special yoon combinations for Hepburn
    if (modifier === '+ya') {
      const first = ccTree[0];
      if (first === 'shi') return 'sha';
      if (first === 'chi') return 'cha';
      if (first === 'ji' || first === 'dji') return 'ja';
    } else if (modifier === '+yu') {
      const first = ccTree[0];
      if (first === 'shi') return 'shu';
      if (first === 'chi') return 'chu';
      if (first === 'ji' || first === 'dji') return 'ju';
    } else if (modifier === '+yo') {
      const first = ccTree[0];
      if (first === 'shi') return 'sho';
      if (first === 'chi') return 'cho';
      if (first === 'ji' || first === 'dji') return 'jo';
    }

    // Fall through to parent class method
    return super.rApply(modifier, ccTree);
  }

  // Line 132-134: defmethod r-simplify for generic-hepburn
  rSimplify(str: string): string {
    // Remove apostrophe after n if the next character isn't a vowel
    return str.replace(/n'([^aiueoy]|$)/g, 'n$1');
  }
}

// Line 136-142: defclass simplified-hepburn with simplifications
class SimplifiedHepburn extends GenericHepburn {
  protected simplifications: string[];

  constructor(simplifications: string[] = []) {
    super();
    this.simplifications = simplifications;
  }

  // Line 141-142: defmethod r-simplify for simplified-hepburn
  rSimplify(str: string): string {
    const simplified = super.rSimplify(str);
    // Convert array to tuple array format expected by simplifyNgrams
    const tuples: [string, string][] = [];
    for (let i = 0; i < this.simplifications.length; i += 2) {
      tuples.push([this.simplifications[i], this.simplifications[i + 1]]);
    }
    return simplifyNgrams(simplified, tuples);
  }
}

// Line 152-158: defclass traditional-hepburn
class TraditionalHepburn extends SimplifiedHepburn {
  constructor() {
    // Line 153: Simplifications with macrons
    super(['oo', 'ō', 'ou', 'ō', 'uu', 'ū']);
  }

  // Line 155-158: defmethod r-simplify for traditional-hepburn
  rSimplify(str: string): string {
    let result = super.rSimplify(str);
    // Line 157: Replace n' before aiueoy with n-
    result = result.replace(/n'([aiueoy])/g, 'n-$1');
    // Line 158: Replace n before mbp with m
    result = result.replace(/n([mbp])/g, 'm$1');
    return result;
  }
}

// Line 162-168: defclass modified-hepburn
class ModifiedHepburn extends SimplifiedHepburn {
  constructor() {
    // Line 163: Simplifications with aa and ee macrons
    super(['oo', 'ō', 'ou', 'ō', 'uu', 'ū', 'aa', 'ā', 'ee', 'ē']);
    // Line 165-166: Override :wo mapping
    this.kanaTable.set('wo', 'o');
  }
}

// Line 194-200: defclass kunrei-siki
class KunreiSiki extends GenericRomanization {
  constructor() {
    super();
    // Line 195: Copy Kunrei-siki kana table
    this.kanaTable = new Map(kunreiSikiKanaTable);
  }

  // Line 197-199: defmethod r-simplify for kunrei-siki
  rSimplify(str: string): string {
    // Remove apostrophe after n if the next character isn't a vowel
    const simplified = str.replace(/n'([^aiueoy]|$)/g, 'n$1');
    const tuples: [string, string][] = [['oo', 'ô'], ['ou', 'ô'], ['uu', 'û']];
    return simplifyNgrams(simplified, tuples);
  }
}

// Line 144-201: Exported method instances (defparameter)
export const hepburnBasic = new GenericHepburn();
export const hepburnSimple = new SimplifiedHepburn(['oo', 'o', 'ou', 'o', 'uu', 'u']);
export const hepburnPassport = new SimplifiedHepburn(['oo', 'oh', 'ou', 'oh', 'uu', 'u']);
export const hepburnTraditional = new TraditionalHepburn();
export const hepburnModified = new ModifiedHepburn();
export const kunreiSiki = new KunreiSiki();

// Line 203: Default romanization method
export let defaultRomanizationMethod: RomanizationMethod = hepburnTraditional;

// ============================================================================
// Main Romanization Functions
// ============================================================================

// Line 205-208: defun romanize-list
/**
 * Romanize a character class list according to method
 *
 * Lisp source:
 * (defun romanize-list (cc-list &key (method *default-romanization-method*))
 *   "Romanize a character class list according to method"
 *   (let ((cc-tree (process-modifiers (process-iteration-characters cc-list))))
 *     (values (r-simplify method (romanize-core method cc-tree)))))
 */
export function romanizeList(
  ccList: CharClass[],
  options: { method?: RomanizationMethod } = {}
): string {
  const method = options.method ?? defaultRomanizationMethod;
  // Line 207: Process iteration characters, then modifiers
  const ccTree = processModifiers(processIterationCharacters(ccList));
  // Line 208: Romanize core and simplify
  return method.rSimplify(romanizeCore(method, ccTree));
}

// Line 224-230: defun romanize-word
/**
 * Romanize a word according to method
 *
 * Lisp source:
 * (defun romanize-word (word &key (method *default-romanization-method*) original-spelling (normalize t))
 *   "Romanize a word according to method"
 *   (when normalize
 *     (setf word (normalize word)))
 *   (or (r-special method (or original-spelling word))
 *       (let ((word (process-hints word)))
 *         (romanize-list (get-character-classes word) :method method))))
 */
export function romanizeWord(
  word: string,
  options: {
    method?: RomanizationMethod;
    originalSpelling?: string;
    normalize?: boolean;
  } = {}
): string {
  const method = options.method ?? defaultRomanizationMethod;
  const shouldNormalize = options.normalize ?? true;

  // Line 226-227: Normalize word if requested
  if (shouldNormalize) {
    word = normalize(word);
  }

  // Line 228: Try special romanization first
  const special = method.rSpecial?.(options.originalSpelling ?? word);
  if (special) return special;

  // Line 229-230: Process hints and romanize
  const processed = processHints(word);
  return romanizeList(getCharacterClasses(processed), { method });
}

// Line 232-233: defun romanize-word-geo
/**
 * Romanize word for geographic names (capitalized)
 *
 * Lisp source:
 * (defun romanize-word-geo (input &key (method *hepburn-simple*))
 *   (string-capitalize (romanize-word input :method method :normalize t)))
 */
export function romanizeWordGeo(
  input: string,
  options: { method?: RomanizationMethod } = {}
): string {
  const method = options.method ?? hepburnSimple;
  const romanized = romanizeWord(input, { method, normalize: true });
  // Capitalize first letter
  return romanized.charAt(0).toUpperCase() + romanized.slice(1).toLowerCase();
}

// Line 235-246: defun join-parts
/**
 * Join romanized parts with proper spacing
 *
 * Lisp source:
 * (defun join-parts (parts)
 *   (with-output-to-string (s)
 *     (loop with last-space = t
 *          for part in parts
 *          for len = (length part) do
 *          (when (and (not (zerop len))
 *                     (not last-space)
 *                     (alphanumericp (char part 0)))
 *            (princ #\Space s))
 *          (princ part s)
 *          (unless (zerop len)
 *            (setf last-space (cl-unicode:has-property (char part (1- len)) "WhiteSpace"))))))
 */
export function joinParts(parts: string[]): string {
  let result = '';
  let lastSpace = true;

  for (const part of parts) {
    const len = part.length;

    if (len > 0) {
      // Line 240-242: Add space before alphanumeric if needed
      if (!lastSpace && /[a-zA-Z0-9]/.test(part[0])) {
        result += ' ';
      }

      result += part;

      // Line 245-246: Check if part ends with whitespace
      lastSpace = /\s/.test(part[len - 1]);
    }
  }

  return result;
}

// Helper function for map-word-info-kana
// Line 1725-1729 from dict.lisp: defun map-word-info-kana
// NOTE: In Lisp, (word-info-kana word-info) calls the :reader get-kana, which has an
// :around method that automatically calls get-hint. In TypeScript, we must explicitly
// call getKana() to get the hinted kana.
async function mapWordInfoKana(
  fn: (kana: string) => string,
  wordInfo: WordInfo,
  options: { separator?: string } = {}
): Promise<string> {
  const separator = options.separator ?? '/';
  // WordInfo.kana already has hints applied, so use it directly instead of calling getKana()
  // which would apply hints again. This matches Lisp behavior where word-info kana field
  // is already processed.
  const wkana = wordInfo.kana;

  if (Array.isArray(wkana)) {
    // Line 1728: Join and simplify reading list
    // Import simplifyReadingList from dict.ts
    const { simplifyReadingList } = await import('./dict.js');
    return simplifyReadingList(wkana.map(fn)).join(separator);
  } else {
    // Line 1729: Single kana string
    return fn(wkana);
  }
}

// Line 248-255: defun romanize-word-info
/**
 * Romanize word-info instance
 *
 * Lisp source:
 * (defun romanize-word-info (word-info &key (method *default-romanization-method*))
 *   "Romanize word-info instance"
 *   (let* ((orig-text (word-info-text word-info)))
 *     (map-word-info-kana
 *      (if (eql method :kana)
 *          (lambda (wk) (strip-hints wk))
 *          (lambda (wk) (romanize-word wk :method method :original-spelling orig-text :normalize nil)))
 *      word-info)))
 */
export async function romanizeWordInfo(
  wordInfo: WordInfo,
  options: { method?: RomanizationMethod | 'kana' } = {}
): Promise<string> {
  const method = options.method ?? defaultRomanizationMethod;
  const origText = wordInfo.text;

  // Line 252-254: Choose function based on method
  if (method === 'kana') {
    // Line 253: Just strip hints
    return await mapWordInfoKana(wk => stripHints(wk), wordInfo);
  } else {
    // Line 254: Full romanization
    return await mapWordInfoKana(
      wk => romanizeWord(wk, {
        method: method as RomanizationMethod,
        originalSpelling: origText,
        normalize: false
      }),
      wordInfo
    );
  }
}

// Line 257-271: defun romanize
/**
 * Romanize a sentence according to method
 *
 * Lisp source:
 * (defun romanize (input &key (method *default-romanization-method*) (with-info nil))
 *   "Romanize a sentence according to method"
 *   (setf input (normalize input :context method))
 *   (loop with definitions = nil
 *      for (split-type . split-text) in (basic-split input)
 *      nconc
 *        (if (eql split-type :word)
 *            (mapcar (lambda (word)
 *                      (let ((rom (romanize-word-info word :method method)))
 *                        (when with-info
 *                          (push (cons rom (word-info-str word)) definitions))
 *                        rom))
 *                    (simple-segment split-text))
 *            (list split-text)) into parts
 *      finally (return (values (join-parts parts) (nreverse definitions)))))
 */
export async function romanize(
  input: string,
  options: { method?: RomanizationMethod; withInfo?: boolean } = {}
): Promise<{ romanized: string; info?: Array<[string, string]> }> {
  const method = options.method ?? defaultRomanizationMethod;
  const withInfo = options.withInfo ?? false;

  // Line 259: Normalize input (context parameter for normalize is only 'kana' or undefined)
  const normalized = normalize(input);

  const definitions: Array<[string, string]> = [];
  const parts: string[] = [];

  // Line 260-270: Process each segment from basic-split
  for (const segment of basicSplit(normalized)) {
    if (segment.type === 'word') {
      // Line 263-269: Segment and romanize words
      const words = await simpleSegment(segment.text);

      for (const word of words) {
        const rom = await romanizeWordInfo(word, { method });

        if (withInfo) {
          // Line 267: Store definition
          const info = await wordInfoStr(word);
          definitions.push([rom, info]);
        }

        parts.push(rom);
      }
    } else {
      // Line 270: Keep non-word segments as-is
      parts.push(segment.text);
    }
  }

  // Line 271: Join parts and return (multiple values)
  const romanized = joinParts(parts);

  if (withInfo) {
    return { romanized, info: definitions.reverse() };
  } else {
    return { romanized };
  }
}

// Line 273-290: defun romanize*
/**
 * Romanizes text with very detailed metadata
 *
 * Lisp source:
 * (defun romanize* (input &key (method *default-romanization-method*) (limit 5) (wordprop-fn (constantly nil)))
 *   "Romanizes text with very detailed metadata"
 *   (setf input (normalize input :context method))
 *   (loop for (split-type . split-text) in (basic-split input)
 *      collect
 *        (if (eql split-type :word)
 *            (mapcar (lambda (pair)
 *                      (let ((word-list (car pair))
 *                            (score (cdr pair)))
 *                        (list
 *                         (mapcar (lambda (word)
 *                                   (let* ((romanized (romanize-word-info word :method method))
 *                                          (prop (funcall wordprop-fn romanized word)))
 *                                     (list romanized word prop)))
 *                                 word-list)
 *                         score)))
 *                    (dict-segment split-text :limit limit))
 *            split-text)))
 */
export async function romanizeStar(
  input: string,
  options: {
    method?: RomanizationMethod;
    limit?: number;
    wordpropFn?: (romanized: string, word: WordInfo) => any;
  } = {}
): Promise<Array<string | Array<[Array<[string, WordInfo, any]>, number]>>> {
  const method = options.method ?? defaultRomanizationMethod;
  const limit = options.limit ?? 5;
  // Note: Lisp default is (constantly nil), which jsown serializes as []
  const wordpropFn = options.wordpropFn ?? (() => []);

  // Line 275: Normalize input
  const normalized = normalize(input);

  const result: Array<string | Array<[Array<[string, WordInfo, any]>, number]>> = [];

  // Line 276-289: Process each segment from basic-split
  for (const segment of basicSplit(normalized)) {
    if (segment.type === 'word') {
      // Line 278-289: Get dict-segment with limit and process
      const segmentResult = await dictSegment(segment.text, { limit });

      const wordListResults: Array<[Array<[string, WordInfo, any]>, number]> = await Promise.all(
        segmentResult.map(async ([wordList, score]) => {
          // Line 283-287: Process each word in the word-list
          const processedWords: Array<[string, WordInfo, any]> = await Promise.all(
            wordList.map(async word => {
              const romanized = await romanizeWordInfo(word, { method });
              const prop = wordpropFn(romanized, word);
              return [romanized, word, prop];
            })
          );

          // Line 288: Return [processed-words, score]
          return [processedWords, score];
        })
      );

      result.push(wordListResults);
    } else {
      // Line 290: Keep non-word segments as-is
      result.push(segment.text);
    }
  }

  return result;
}