// ichiran/kanji - Kanji reading matching utilities
// Port of kanji.lisp (Lines 199-314: Core reading matching functions)

import { getConnection } from './conn.js';
import { KANJI_REGEX, rendaku, unrendaku, geminate } from './characters.js';
import type { ReadingMatchItem } from './types.js';

// =============================================================================
// Line 199-211: Reading cache and getReadingsCache
// =============================================================================

/**
 * Cache for kanji readings from database
 * Key format: [kanji-string, typeset-array] -> reading results
 */
const readingCache = new Map<string, Array<[string, string]>>();

/**
 * Get readings from cache or database
 *
 * Original Lisp (Lines 201-211):
 * (defun get-readings-cache (str typeset)
 *   (with-connection *connection*
 *     (let* ((key (cons str typeset))
 *            (val (gethash key *reading-cache* :none)))
 *       (if (eql val :none)
 *           (let ((result (query (:select 'r.text 'r.type :from (:as 'kanji 'k)
 *                                         :inner-join (:as 'reading 'r) :on (:= 'r.kanji-id 'k.id)
 *                                         :where (:and (:= 'k.text str)
 *                                                      (:not (:in 'r.type (:set typeset))))))))
 *             (setf (gethash key *reading-cache*) result))
 *           val))))
 */
async function getReadingsCache(str: string, typeset: string[]): Promise<Array<[string, string]>> {
  const key = JSON.stringify([str, typeset]);
  const cached = readingCache.get(key);
  if (cached !== undefined) {
    return cached;
  }

  // Query: SELECT r.text, r.type FROM kanji k
  //        INNER JOIN reading r ON r.kanji_id = k.id
  //        WHERE k.text = str AND r.type NOT IN typeset
  const sql = getConnection();
  const result = await sql<Array<{ text: string; type: string }>>`
    SELECT r.text, r.type
    FROM kanji k
    INNER JOIN reading r ON r.kanji_id = k.id
    WHERE k.text = ${str}
      AND r.type NOT IN ${sql(typeset)}
  `;

  const readings: Array<[string, string]> = result.map((row: { text: string; type: string }) => [row.text, row.type]);
  readingCache.set(key, readings);
  return readings;
}

/**
 * Get readings for a kanji character
 *
 * Original Lisp (Lines 213-216):
 * (defun get-readings (char &key names)
 *   (let ((str (if (typep char 'character) (make-string 1 :initial-element char) char))
 *         (typeset (if names nil '("ja_na"))))
 *     (get-readings-cache str typeset)))
 */
async function getReadings(char: string, names = false): Promise<Array<[string, string]>> {
  const typeset = names ? [] : ['ja_na'];
  return getReadingsCache(char, typeset);
}

// =============================================================================
// Line 218-229: Reading alternatives (rendaku, gemination)
// =============================================================================

/**
 * Generate reading alternatives including rendaku and gemination variants
 *
 * Original Lisp (Lines 218-229):
 * (defun get-reading-alternatives (reading type &key rendaku)
 *   (let* ((end (1- (length reading)))
 *          (lst `(,(list reading type nil)
 *                 ,@(when (and (> end 0)
 *                              (string= type "ja_on")
 *                              (find (char reading end) "つくきち"))
 *                         (list (list (geminate reading :fresh t) type nil (format nil "~c" (char reading end))))))))
 *     (if rendaku
 *         (append lst (loop for (rd nil nil gem) in lst
 *                        collect (list (rendaku rd :fresh t) type :rendaku gem)
 *                        collect (list (rendaku rd :fresh t :handakuten t) type :rendaku gem)))
 *         lst)))
 *
 * Returns: Array of [reading, type, rendaku-flag, geminated-char]
 * - reading: The kana reading text
 * - type: "ja_on" or "ja_kun"
 * - rendaku-flag: undefined or :rendaku symbol
 * - geminated-char: undefined or original final character (for gemination)
 */
function getReadingAlternatives(
  reading: string,
  type: string,
  rendakuOpt = false
): Array<[string, string, string | null, string | null]> {
  const end = reading.length - 1;
  // Line 220-224: Base alternatives (original + geminated if applicable)
  const lst: Array<[string, string, string | null, string | null]> = [
    [reading, type, null, null]
  ];

  // Line 221-224: Add geminated form for certain final characters
  if (end > 0 && type === 'ja_on' && 'つくきち'.includes(reading[end])) {
    lst.push([geminate(reading, true), type, null, reading[end]]);
  }

  // Line 225-228: Add rendaku alternatives if requested
  if (rendakuOpt) {
    const rendakuForms: Array<[string, string, string | null, string | null]> = [];
    for (const [rd, typ, _rend, gem] of lst) {
      // Line 226-227: Dakuten and handakuten rendaku variants
      rendakuForms.push([rendaku(rd, true, false), typ, 'rendaku', gem]);
      rendakuForms.push([rendaku(rd, true, true), typ, 'rendaku', gem]);
    }
    return [...lst, ...rendakuForms];
  }

  return lst;
}

// =============================================================================
// Line 231-239: Normal readings with all alternatives
// =============================================================================

/**
 * Get all normal readings (non-name readings) with alternatives
 *
 * Original Lisp (Lines 231-239):
 * (defun get-normal-readings (char &key rendaku)
 *   (let* ((str (if (typep char 'character) (make-string 1 :initial-element char) char))
 *          (readings (get-readings-cache str '("ja_na")))
 *          (readings* (loop for (reading type) in readings
 *                          for (main . rest) = (get-reading-alternatives reading type :rendaku rendaku)
 *                          collect main into main-readings
 *                          nconc rest into alt-readings
 *                          finally (return (append main-readings alt-readings)))))
 *     (remove-duplicates readings* :test 'equal :key 'car :from-end t)))
 *
 * Returns: Array of reading tuples with main readings first, then alternatives
 */
async function getNormalReadings(
  char: string,
  rendakuOpt = false
): Promise<Array<[string, string, string | null, string | null]>> {
  const readings = await getReadings(char, false);  // Line 233: exclude ja_na

  const mainReadings: Array<[string, string, string | null, string | null]> = [];
  const altReadings: Array<[string, string, string | null, string | null]> = [];

  // Line 234-237: Separate main and alternative readings
  for (const [reading, type] of readings) {
    const alternatives = getReadingAlternatives(reading, type, rendakuOpt);
    const [main, ...rest] = alternatives;
    mainReadings.push(main);
    altReadings.push(...rest);
  }

  const allReadings = [...mainReadings, ...altReadings];

  // Line 238: Remove duplicates (keep FIRST occurrence, matching Lisp :from-end t behavior)
  // Lisp: (remove-duplicates readings* :test 'equal :key 'car :from-end t)
  // :from-end t means scan from end but keep the first occurrence
  const seen = new Map<string, number>();
  for (let i = 0; i < allReadings.length; i++) {
    const key = allReadings[i][0]; // reading text
    if (!seen.has(key)) {
      seen.set(key, i);
    }
  }
  return allReadings.filter((_r, idx) => seen.get(allReadings[idx][0]) === idx);
}

// =============================================================================
// Line 273-290: makeRmap - Create reading map from kanji string
// =============================================================================

/**
 * Create reading map (rmap) from kanji text
 * Each kanji is replaced with array of possible readings, kana kept as-is
 *
 * Original Lisp (Lines 273-290):
 * (defun make-rmap (str)
 *   (loop with prev-kanji
 *      for start from 0 below (length str)
 *      for end = (1+ start)
 *      for char = (char str start)
 *      if (ppcre:scan *kanji-regex* str :start start :end end)
 *      collect (cond ((eql char #\々)
 *                     (prog1 (when prev-kanji (get-normal-readings prev-kanji :rendaku t))
 *                       (setf prev-kanji nil)))
 *                    ((eql char #\ヶ)
 *                     (setf prev-kanji nil)
 *                     '(("か" "ja_on") ("が" "abbr")))
 *                    ((eql char #\〆)
 *                     (setf prev-kanji #\締)
 *                     '(("しめ" "ja_kun") ("じめ" "ja_kun" :rendaku)))
 *                    (t (setf prev-kanji char)
 *                       (get-normal-readings char :rendaku (> start 0))))
 *      else collect char))
 *
 * Returns: Array where each element is either:
 *   - A character (for kana)
 *   - An array of possible readings (for kanji)
 */
async function makeRmap(str: string): Promise<Array<string | Array<[string, string, string | null, string | null]>>> {
  const rmap: Array<string | Array<[string, string, string | null, string | null]>> = [];
  let prevKanji: string | null = null;

  for (let start = 0; start < str.length; start++) {
    const char = str[start];

    // Line 278: Check if character is kanji using KANJI_REGEX
    if (KANJI_REGEX.test(char)) {
      // Line 279-289: Handle special kanji characters
      if (char === '々') {  // Line 279-281: Iteration mark (repeat previous kanji with rendaku)
        const readings = prevKanji ? await getNormalReadings(prevKanji, true) : [];
        rmap.push(readings);
        prevKanji = null;
      } else if (char === 'ヶ') {  // Line 282-284: Counter mark (ka/ga)
        prevKanji = null;
        rmap.push([
          ['か', 'ja_on', null, null] as [string, string, string | null, string | null],
          ['が', 'abbr', null, null] as [string, string, string | null, string | null]
        ]);
      } else if (char === '〆') {  // Line 285-287:締 abbreviation (shime/jime)
        prevKanji = '締';
        rmap.push([
          ['しめ', 'ja_kun', null, null] as [string, string, string | null, string | null],
          ['じめ', 'ja_kun', ':rendaku', null] as [string, string, string | null, string | null]
        ]);
      } else {  // Line 288-289: Regular kanji
        prevKanji = char;
        rmap.push(await getNormalReadings(char, start > 0));
      }
    } else {  // Line 290: Not kanji, keep as literal character
      rmap.push(char);
    }
  }

  return rmap;
}

// =============================================================================
// Line 241-271: matchReadings* - Core recursive matching algorithm
// =============================================================================

/**
 * Recursive algorithm to match reading map against kana reading
 *
 * Original Lisp (Lines 241-271):
 * (defun match-readings* (rmap reading &key (start 0))
 *   (unless rmap
 *     (return-from match-readings*
 *       (if (>= start (length reading))
 *           (values nil 0)
 *           :none)))
 *   (when (>= start (length reading))
 *     (return-from match-readings* :none))
 *
 *   (let ((item (car rmap))
 *         matches)
 *     (cond ((listp item)
 *            (loop for end from (1+ start) to (length reading)
 *                 for (match score) = (multiple-value-list (match-readings* (cdr rmap) reading :start end))
 *                 unless (eql match :none)
 *                 do (unless (loop for r in item
 *                               unless (mismatch reading (car r) :start1 start :end1 end)
 *                               do (push (cons (cons r match) score) matches) (return t))
 *                      (push (cons (cons (list (subseq reading start end) "irr") match) (- score (- end start))) matches)))
 *            (if matches
 *                (loop with max-score and best-match
 *                     for (match . score) in matches
 *                     if (or (not max-score) (> score max-score))
 *                     do (setf max-score score best-match match)
 *                     finally (return (values best-match max-score)))
 *                :none))
 *           (t (if (eql item (char reading start))
 *                  (multiple-value-bind (match score) (match-readings* (cdr rmap) reading :start (1+ start))
 *                      (if (eql match :none) :none
 *                          (values (cons item match) score)))
 *                  :none)))))
 *
 * Returns: [match, score] or ':none' if no match
 *   - match: Array of matched items (characters or reading tuples)
 *   - score: Quality score of the match
 */
async function matchReadingsStar(
  rmap: Array<string | Array<[string, string, string | null, string | null]>>,
  reading: string,
  start = 0
): Promise<[ReadingMatchItem[], number] | ':none'> {
  // Line 242-245: Empty rmap - success if we consumed all reading
  if (rmap.length === 0) {
    return start >= reading.length ? [[], 0] : ':none';
  }

  // Line 247-248: Reading exhausted but rmap not empty - failure
  if (start >= reading.length) {
    return ':none';
  }

  const item = rmap[0];  // Line 250: car rmap
  const restRmap = rmap.slice(1);  // cdr rmap

  // Line 252-266: Item is reading list (kanji position)
  if (Array.isArray(item)) {
    const matches: Array<[ReadingMatchItem[], number]> = [];

    // Line 253: Try all possible substring lengths
    for (let end = start + 1; end <= reading.length; end++) {
      // Line 254: Recursively match rest of rmap
      const restResult = await matchReadingsStar(restRmap, reading, end);

      if (restResult !== ':none') {
        const [match, score] = restResult;
        let foundMatch = false;

        // Line 256-258: Try to match substring against readings in item
        for (const r of item) {
          const readingText = r[0];
          // Line 257: Check if substring matches this reading
          if (reading.substring(start, end) === readingText) {
            // Use unshift to match Lisp's push behavior (adds to front)
            matches.unshift([[r as any, ...match], score]);
            foundMatch = true;
            break;
          }
        }

        // Line 259: No match found - mark as irregular reading with penalty
        if (!foundMatch) {
          const substring = reading.substring(start, end);
          const penalty = end - start;
          // Use unshift to match Lisp's push behavior (adds to front)
          matches.unshift([[[substring, 'irr'] as any, ...match], score - penalty]);
        }
      }
    }

    // Line 260-265: Return best match
    // Matches Lisp: only replace when score > max-score (not when equal)
    // This keeps the first match with the highest score
    if (matches.length > 0) {
      let bestMatch = matches[0][0];
      let maxScore = matches[0][1];
      for (const [match, score] of matches) {
        if (score > maxScore) {
          maxScore = score;
          bestMatch = match;
        }
      }
      return [bestMatch, maxScore];
    }

    return ':none';
  }

  // Line 267-270: Item is character (kana position)
  if (item === reading[start]) {
    const restResult = await matchReadingsStar(restRmap, reading, start + 1);
    if (restResult === ':none') {
      return ':none';
    }
    const [match, score] = restResult;
    return [[item, ...match], score];
  }

  return ':none';
}

// =============================================================================
// Line 292-306: matchReadings - Main public function
// =============================================================================

/**
 * Match kanji text with kana reading to determine character boundaries
 *
 * Original Lisp (Lines 292-306):
 * (defun match-readings (str reading)
 *   (let* ((rmap (make-rmap str))
 *          (match (match-readings* rmap reading)))
 *     (unless (eql match :none)
 *       (loop with charbag and result
 *          for m in match
 *          for c across str
 *          if (listp m)
 *            when charbag do (push (coerce (nreverse charbag) 'string) result) (setf charbag nil) end
 *            and do (push (cons (make-string 1 :initial-element c) m) result)
 *          else
 *            do (push c charbag)
 *          finally
 *            (when charbag (push (coerce (nreverse charbag) 'string) result))
 *            (return (nreverse result))))))
 *
 * Returns: Array of match segments or null if no match
 *   - String segments: Literal kana that matched directly
 *   - [kanji-char, reading, type, ...]: Kanji matched to reading
 *
 * Example: matchReadings("漢字", "かんじ") might return:
 *   [["漢", "かん", "ja_on"], ["字", "じ", "ja_on"]]
 */
export async function matchReadings(str: string, reading: string): Promise<ReadingMatchItem[] | null> {
  // Line 293: Create reading map
  const rmap = await makeRmap(str);

  // Line 294: Run matching algorithm
  const matchResult = await matchReadingsStar(rmap, reading);

  if (matchResult === ':none') {
    return null;
  }

  const [match] = matchResult;

  // Line 296-305: Process match to create result array
  // Combine consecutive characters into strings, keep reading tuples as arrays
  const result: ReadingMatchItem[] = [];
  let charBag: string[] = [];

  for (let i = 0; i < match.length && i < str.length; i++) {
    const m = match[i];
    const c = str[i];

    // Line 299-301: If match item is list (reading tuple)
    if (Array.isArray(m)) {
      // Line 299: Flush accumulated characters
      if (charBag.length > 0) {
        result.push(charBag.join(''));
        charBag = [];
      }
      // Line 300: Add kanji + reading tuple
      // Lisp format matches (cons kanji-char reading-tuple):
      // - irr: (kanji reading "irr") = 3 elements
      // - regular: (kanji reading type rendaku-flag geminated?) = 4-5 elements
      //   rendaku-flag is always present (NIL or :RENDAKU)
      //   if rendaku is :RENDAKU, 5th element is always present (geminated or NIL)
      //   if rendaku is NIL and geminated exists, 5th element is geminated
      const [readingText, type, rendakuFlag, geminated] = m;

      if (type === 'irr') {
        // Irregular: only 3 elements
        result.push([c, readingText, type]);
      } else {
        // Regular: always 4 elements minimum
        const output: any[] = [c, readingText, type, rendakuFlag ?? null];
        // If rendaku is present OR geminated is present (and not null), add 5th element
        if (rendakuFlag || (geminated !== undefined && geminated !== null)) {
          output.push(geminated ?? null);
        }
        result.push(output as ReadingMatchItem);
      }
    } else {
      // Line 303: Accumulate literal character
      charBag.push(m as string);
    }
  }

  // Line 304-305: Flush remaining characters
  if (charBag.length > 0) {
    result.push(charBag.join(''));
  }

  return result;
}

// =============================================================================
// Line 308-314: getOriginalReading - Restore original reading from variants
// =============================================================================

/**
 * Get original reading text before rendaku/gemination transformations
 *
 * Original Lisp (Lines 308-314):
 * (defun get-original-reading (rtext &optional rendaku geminated)
 *   (when rendaku
 *     (setf rtext (unrendaku rtext :fresh t)))
 *   (when geminated
 *     (setf rtext (copy-seq rtext))
 *     (setf (char rtext (1- (length rtext))) (char geminated 0)))
 *   rtext)
 */
export function getOriginalReading(
  rtext: string,
  rendakuFlag?: string,
  geminated?: string
): string {
  let result = rtext;

  // Line 309-310: Undo rendaku transformation
  if (rendakuFlag) {
    result = unrendaku(result, true);
  }

  // Line 311-313: Undo gemination (restore original final character)
  if (geminated) {
    result = result.slice(0, -1) + geminated[0];
  }

  return result;
}

// =============================================================================
// Additional database helper: getKanjiWords
// =============================================================================

/**
 * Get all common words containing a specific kanji character
 * Used for kanji statistics and info lookups
 *
 * Original Lisp (dict.lisp Lines 1832-1842):
 * (defun get-kanji-words (char)
 *   (with-connection *connection*
 *     (let* ((str (if (typep char 'character) (make-string 1 :initial-element char) char)))
 *       (query (:select 'e.seq 'k.text 'r.text 'k.common
 *                       :from (:as 'entry 'e) (:as 'kanji-text 'k) (:as 'kana-text 'r)
 *                       :where (:and (:= 'e.seq 'k.seq)
 *                                    (:= 'e.seq 'r.seq)
 *                                    (:= 'r.text 'k.best-kana)
 *                                    (:not-null 'k.common)
 *                                    'e.root-p
 *                                    (:like 'k.text (:|| "%" str "%"))))))))
 *
 * Returns: Array of [seq, kanji-text, kana-text, common-score]
 */
export async function getKanjiWords(char: string): Promise<Array<[number, string, string, number]>> {
  const sql = getConnection();
  const result = await sql<Array<{ seq: number; ktext: string; rtext: string; common: number }>>`
    SELECT e.seq, k.text AS ktext, r.text AS rtext, k.common
    FROM entry e
    INNER JOIN kanji_text k ON e.seq = k.seq
    INNER JOIN kana_text r ON e.seq = r.seq
    WHERE k.text LIKE ${'%' + char + '%'}
      AND r.text = k.best_kana
      AND k.common IS NOT NULL
      AND e.root_p = true
  `;

  return result.map((row: { seq: number; ktext: string; rtext: string; common: number }) => [row.seq, row.ktext, row.rtext, row.common]);
}