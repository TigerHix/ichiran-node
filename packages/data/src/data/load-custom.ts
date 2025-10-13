/**
 * Custom data loading system
 * Ported from ~/ichiran/dict-custom.lisp
 *
 * Includes:
 * - XML loader (extra.xml entries)
 * - Municipality CSV loader (jichitai.csv - ~2000 places)
 * - Ward CSV loader (gyoseiku.csv - ~500 wards)
 * - match-glosses() deduplication function
 */

import fs from 'fs';
import path from 'path';
import { XMLValidator } from 'fast-xml-parser';
import { loadEntry } from './load-entry.js';
import { getConnection } from '@ichiran/core';
import { romanizeWordGeo } from '@ichiran/core';
import { addNewSense } from './errata.js';

/**
 * Base class for custom data sources
 * Ported from dict-custom.lisp:26-28 custom-source
 */
export abstract class CustomSource {
  protected entries: any[] = [];
  abstract description: string;

  /**
   * Reads custom data from the source file
   * Returns the number of entries read
   */
  abstract slurp(): Promise<number>;

  /**
   * Inserts slurped data into database
   */
  abstract insert(): Promise<void>;

  /**
   * Gets the number of entries loaded
   */
  getEntryCount(): number {
    return this.entries.length;
  }
}

/**
 * XML entry structure
 */
interface XmlEntry {
  seq: number | string;
  content: string;  // XML string
}

/**
 * Loads extra entries from XML file
 * Ported from dict-custom.lisp:35-53 xml-loader
 */
export class XmlLoader extends CustomSource {
  description = 'extra XML data';
  private sourceFile: string;

  constructor(sourceFile: string) {
    super();
    this.sourceFile = sourceFile;
  }

  async slurp(): Promise<number> {
    const content = fs.readFileSync(this.sourceFile, 'utf-8');

    // Validate XML structure (more efficient than full parse)
    const validationResult = XMLValidator.validate(content);
    if (validationResult !== true) {
      throw new Error(`Invalid XML in ${this.sourceFile}: ${validationResult.err.msg}`);
    }

    // Extract <entry> elements using regex (validated XML is safe to parse this way)
    this.entries = this.extractEntries(content);

    return this.entries.length;
  }

  /**
   * Extracts individual entry XML strings from the full XML content
   */
  private extractEntries(content: string): XmlEntry[] {
    const entries: XmlEntry[] = [];
    const entryRegex = /<entry>([\s\S]*?)<\/entry>/g;
    let match: RegExpExecArray | null;

    while ((match = entryRegex.exec(content)) !== null) {
      const entryXml = match[0];

      // Extract sequence number from ent_seq
      const seqMatch = entryXml.match(/<ent_seq>(.*?)<\/ent_seq>/);
      const seqStr = seqMatch ? seqMatch[1].trim() : '';

      // Try to parse as integer, fallback to string
      let seq: number | string;
      try {
        seq = parseInt(seqStr);
        if (isNaN(seq)) {
          seq = seqStr;
        }
      } catch {
        seq = seqStr;
      }

      entries.push({
        seq,
        content: entryXml
      });
    }

    return entries;
  }

  async insert(): Promise<void> {
    for (const entry of this.entries) {
      await loadEntry(entry.content, {
        ifExists: 'skip',
        seq: typeof entry.seq === 'number' && Number.isFinite(entry.seq) ? entry.seq : undefined,
        conjugateP: true
      });
    }
  }
}

/**
 * Loads custom data from source files
 * Ported from dict-custom.lisp:320-335 load-custom-data
 *
 * @param options - Loading options
 */
export async function loadCustomData(options: {
  types?: Array<'extra' | 'municipality' | 'ward'>;
  dataPath?: string;
  silent?: boolean;
} = {}): Promise<void> {
  const { types = ['extra'], dataPath = './data/sources', silent = false } = options;

  const loaders: CustomSource[] = [];

  // Create loaders based on requested types
  if (types.includes('extra')) {
    const extraPath = path.join(dataPath, 'extra.xml');
    if (fs.existsSync(extraPath)) {
      loaders.push(new XmlLoader(extraPath));
    } else if (!silent) {
      console.warn(`Warning: extra.xml not found at ${extraPath}`);
    }
  }

  if (types.includes('municipality')) {
    const municipalityPath = path.join(dataPath, 'jichitai.csv');
    if (fs.existsSync(municipalityPath)) {
      loaders.push(new MunicipalityCsvLoader(municipalityPath));
    } else if (!silent) {
      console.warn(`Warning: jichitai.csv not found at ${municipalityPath}`);
    }
  }

  if (types.includes('ward')) {
    const wardPath = path.join(dataPath, 'gyoseiku.csv');
    if (fs.existsSync(wardPath)) {
      loaders.push(new WardCsvLoader(wardPath));
    } else if (!silent) {
      console.warn(`Warning: gyoseiku.csv not found at ${wardPath}`);
    }
  }

  // Process each loader
  for (const loader of loaders) {
    if (!silent) {
      console.log(`Loading ${loader.description}...`);
    }

    const nEntries = await loader.slurp();

    if (!silent) {
      console.log(`Inserting ${nEntries} entries...`);
    }

    await loader.insert();

    if (!silent) {
      console.log(`✓ ${nEntries} entries loaded`);
    }
  }
}

/**
 * Options for match-glosses
 */
interface MatchGlossesOptions {
  normalize?: (text: string) => string;
  updateGloss?: RegExp;
}

/**
 * Match result types
 */
type MatchGlossesResult =
  | { seq: number; match: 'full' }  // All words found in glosses
  | { seq: number; match: 'update'; gloss: string }  // updateGloss pattern matched
  | { seq: number; match: 'partial' }  // Some words found
  | null;  // No match found

/**
 * Finds existing dictionary entries by text and reading, then matches glosses
 * to detect duplicates. Used for deduplication in municipality/ward loaders.
 *
 * Ported from dict.lisp get-candidates + match-glosses
 *
 * @param text - Kanji/kana text to search for
 * @param reading - Reading to search for
 * @param words - Words that should appear in glosses
 * @param options - Optional normalization function and updateGloss pattern
 * @returns Match result or null if no match
 */
export async function matchGlosses(
  text: string,
  reading: string,
  words: string[],
  options?: MatchGlossesOptions
): Promise<MatchGlossesResult> {
  // Get connection is called in getCandidates and getGlosses

  // Get candidate entries with matching text and reading
  const candidates = await getCandidates(text, reading);

  if (candidates.length === 0) {
    return null;
  }

  // Get glosses for all candidates
  const glossMap = await getGlosses(candidates);

  // Check each candidate
  for (const seq of candidates) {
    const glosses = glossMap.get(seq) || [];

    // Apply normalization if provided
    const normalizedGlosses = options?.normalize
      ? glosses.map(options.normalize)
      : glosses;

    const normalizedWords = options?.normalize
      ? words.map(options.normalize)
      : words;

    // Check if all words appear in any gloss
    const allWordsFound = normalizedWords.every(word =>
      normalizedGlosses.some(gloss =>
        gloss.toLowerCase().includes(word.toLowerCase())
      )
    );

    if (allWordsFound) {
      return { seq, match: 'full' };
    }

    // Check if updateGloss pattern matches
    if (options?.updateGloss) {
      for (const gloss of glosses) {
        if (options.updateGloss.test(gloss)) {
          return { seq, match: 'update', gloss };
        }
      }
    }

    // Check for partial match (at least one word found)
    const someWordsFound = normalizedWords.some(word =>
      normalizedGlosses.some(gloss =>
        gloss.toLowerCase().includes(word.toLowerCase())
      )
    );

    if (someWordsFound) {
      return { seq, match: 'partial' };
    }
  }

  return null;
}

/**
 * Gets candidate entry sequences with matching text and reading
 */
async function getCandidates(text: string, reading: string): Promise<number[]> {
  const sql = getConnection();

  // Find entries where:
  // 1. There's a kanji_text matching the text
  // 2. There's a kana_text matching the reading
  // 3. Both are for the same seq
  const results = await sql<{ seq: number }[]>`
    SELECT DISTINCT kt.seq
    FROM kanji_text kt
    JOIN kana_text r ON r.seq = kt.seq
    WHERE kt.text = ${text}
      AND r.text = ${reading}
    ORDER BY kt.seq
  `;

  return results.map(r => r.seq);
}

/**
 * Gets all glosses for the given entry sequences
 */
async function getGlosses(seqs: number[]): Promise<Map<number, string[]>> {
  const sql = getConnection();

  if (seqs.length === 0) {
    return new Map();
  }

  const results = await sql`
    SELECT s.seq, g.text
    FROM sense s
    JOIN gloss g ON g.sense_id = s.id
    WHERE s.seq = ANY(${seqs})
    ORDER BY s.seq, s.ord, g.ord
  `;

  const map = new Map<number, string[]>();

  for (const row of results) {
    const existing = map.get(row.seq) || [];
    existing.push(row.text);
    map.set(row.seq, existing);
  }

  return map;
}

/**
 * Removes diacritics from romanized text for matching
 * Used by municipality and ward loaders to normalize place names
 */
function removeDiacritics(text: string): string {
  const map: Record<string, string> = {
    'ā': 'a', 'á': 'a', 'ǎ': 'a', 'à': 'a',
    'ī': 'i', 'í': 'i', 'ǐ': 'i', 'ì': 'i',
    'ū': 'u', 'ú': 'u', 'ǔ': 'u', 'ù': 'u',
    'ē': 'e', 'é': 'e', 'ě': 'e', 'è': 'e',
    'ō': 'o', 'ó': 'o', 'ǒ': 'o', 'ò': 'o'
  };
  return text.toLowerCase().replace(/[āáǎàīíǐìūúǔùēéěèōóǒò]/g, ch => map[ch] || ch);
}

/**
 * Converts half-width katakana to full-width hiragana
 * Ported from ichiran characters.lisp (as-hiragana + to-normal-char)
 * 
 * Uses parallel string mapping from characters.lisp:
 * - *half-width-kana* and *full-width-kana* for character mapping
 * - Combines dakuten/handakuten marks using simplify-ngrams logic
 */
function convertKatakanaToHiragana(halfWidth: string): string {
  // Parallel string mapping from Lisp characters.lisp
  const HALFWIDTH_KANA = "･ｦｧｨｩｪｫｬｭｮｯｰｱｲｳｴｵｶｷｸｹｺｻｼｽｾｿﾀﾁﾂﾃﾄﾅﾆﾇﾈﾉﾊﾋﾌﾍﾎﾏﾐﾑﾒﾓﾔﾕﾖﾗﾘﾙﾚﾛﾜﾝﾞﾟ";
  const FULLWIDTH_KANA = "・ヲァィゥェォャュョッーアイウエオカキクケコサシスセソタチツテトナニヌネノハヒフヘホマミムメモヤユヨラリルレロワン゛゜";

  // Step 1: Convert half-width to full-width katakana using parallel strings
  let fullWidth = '';
  for (const char of halfWidth) {
    const pos = HALFWIDTH_KANA.indexOf(char);
    fullWidth += pos >= 0 ? FULLWIDTH_KANA[pos] : char;
  }
  
  // Step 2: Combine dakuten (゛) and handakuten (゜) marks
  let combined = '';
  for (let i = 0; i < fullWidth.length; i++) {
    const ch = fullWidth[i];
    const next = fullWidth[i + 1];

    if (next === '゛') {
      // Dakuten mark - convert to voiced consonant
      const dakutenMap: Record<string, string> = {
        'カ': 'ガ', 'キ': 'ギ', 'ク': 'グ', 'ケ': 'ゲ', 'コ': 'ゴ',
        'サ': 'ザ', 'シ': 'ジ', 'ス': 'ズ', 'セ': 'ゼ', 'ソ': 'ゾ',
        'タ': 'ダ', 'チ': 'ヂ', 'ツ': 'ヅ', 'テ': 'デ', 'ト': 'ド',
        'ハ': 'バ', 'ヒ': 'ビ', 'フ': 'ブ', 'ヘ': 'ベ', 'ホ': 'ボ'
      };
      combined += dakutenMap[ch] || ch;
      i++; // Skip the dakuten mark
    } else if (next === '゜') {
      // Handakuten mark - convert to p-sound
      const handakutenMap: Record<string, string> = {
        'ハ': 'パ', 'ヒ': 'ピ', 'フ': 'プ', 'ヘ': 'ペ', 'ホ': 'ポ'
      };
      combined += handakutenMap[ch] || ch;
      i++; // Skip the handakuten mark
    } else {
      combined += ch;
    }
  }
  
  // Step 3: Convert katakana to hiragana (subtract 0x60 from character code)
  return combined.replace(/[\u30a1-\u30f6]/g, (ch) => {
    return String.fromCharCode(ch.charCodeAt(0) - 0x60);
  });
}

/**
 * Municipality entry structure
 */
interface Municipality {
  id: string;
  text: string;           // e.g., "東京都", "札幌市"
  reading: string;        // e.g., "とうきょうと", "さっぽろし"
  romanized: string;      // e.g., "Tokyo", "Sapporo"
  type: string;           // e.g., "metropolis", "city", "prefecture"
  fullName: string;       // e.g., "Tokyo Metropolis", "Sapporo City"
  isPrefecture: boolean;  // True for prefecture-level entries
}

/**
 * Municipality CSV Loader
 * Ported from dict-custom.lisp:46-115 municipality-csv class
 *
 * Loads Japanese administrative divisions from jichitai.csv:
 * - Prefectures (都道府県)
 * - Cities (市)
 * - Towns (町)
 * - Villages (村)
 */
export class MunicipalityCsvLoader extends CustomSource {
  description = 'municipality data from jichitai.csv';
  private sourceFile: string;
  private municipalities: Municipality[] = [];

  constructor(sourceFile: string) {
    super();
    this.sourceFile = sourceFile;
  }

  async slurp(): Promise<number> {
    const content = fs.readFileSync(this.sourceFile, 'utf-8');
    const lines = content.trim().split('\n');

    for (const line of lines) {
      const municipalities = this.processEntry(line);
      this.municipalities.push(...municipalities);
    }

    return this.municipalities.length;
  }

  /**
   * Processes a CSV line and generates municipality entries
   */
  private processEntry(line: string): Municipality[] {
    const parts = line.split(',');
    if (parts.length < 5) {
      return [];
    }

    const [id, prefectureKanji, municipalityKanji, prefectureKana, municipalityKana] = parts;

    const entries: Municipality[] = [];

    // Prefecture entry (when municipality is empty)
    if (!municipalityKanji || !municipalityKana) {
      const reading = convertKatakanaToHiragana(prefectureKana);
      const romanized = romanizeWordGeo(reading);

      // Determine type based on suffix
      let type = 'prefecture';
      if (prefectureKanji.endsWith('都')) type = 'metropolis';
      else if (prefectureKanji.endsWith('道')) type = 'prefecture'; // Hokkaido
      else if (prefectureKanji.endsWith('府')) type = 'prefecture'; // Osaka/Kyoto

      const typeStr = this.getTypeString(prefectureKanji[prefectureKanji.length - 1]);

      entries.push({
        id,
        text: prefectureKanji,
        reading,
        romanized,
        type,
        fullName: `${romanized} ${typeStr}`,
        isPrefecture: true
      });
    } else {
      // Municipality entry (city/town/village)
      const muniReading = convertKatakanaToHiragana(municipalityKana);
      const romanized = romanizeWordGeo(muniReading);

      // Determine type based on suffix
      let type = 'city';
      const lastChar = municipalityKanji[municipalityKanji.length - 1];
      if (lastChar === '町') type = 'town';
      else if (lastChar === '村') type = 'village';

      const typeStr = this.getTypeString(lastChar);
      const prefectureReading = convertKatakanaToHiragana(prefectureKana);
      const prefectureRomanized = romanizeWordGeo(prefectureReading);
      const prefectureTypeStr = this.getTypeString(prefectureKanji[prefectureKanji.length - 1]);
      const prefecture = `${prefectureRomanized} ${prefectureTypeStr}`;

      // Full municipality entry (with suffix: 市/町/村)
      entries.push({
        id,
        text: municipalityKanji,
        reading: muniReading,
        romanized,
        type,
        fullName: `${romanized} ${typeStr}, ${prefecture}`,
        isPrefecture: false
      });

      // Short municipality entry (without suffix) - dict-custom.lisp:171-176
      // Don't create short version for 道 (Hokkaido)
      if (lastChar !== '道') {
        const shortName = this.municipalityShort(municipalityKanji, muniReading);
        if (shortName) {
          entries.push({
            id,
            text: shortName.text,
            reading: shortName.reading,
            romanized: romanizeWordGeo(shortName.reading),
            type,
            fullName: `${romanized} ${typeStr}, ${prefecture}`,
            isPrefecture: false
          });
        }
      }
    }

    return entries;
  }

  /**
   * Gets short form of municipality by removing suffix
   * Ported from dict-custom.lisp:117-127 municipality-short
   *
   * @param text - Full municipality name (e.g., "札幌市")
   * @param reading - Full reading (e.g., "さっぽろし")
   * @returns Short form { text, reading } or null
   */
  private municipalityShort(text: string, reading: string): { text: string; reading: string } | null {
    // Special case: 道 (Hokkaido) - return as-is
    if (text.endsWith('道')) {
      return { text, reading };
    }

    const typeChar = text[text.length - 1];
    const shortText = text.substring(0, text.length - 1);

    // Type readings for each suffix character (dict-custom.lisp:98-105)
    const typeReadings: Record<string, string[]> = {
      '都': ['と'],
      '道': ['どう'],
      '府': ['ふ'],
      '県': ['けん'],
      '市': ['し'],
      '町': ['ちょう', 'まち'],
      '村': ['そん', 'むら'],
      '区': ['く']
    };

    const possibleReadings = typeReadings[typeChar];
    if (!possibleReadings) {
      return null;
    }

    // Find which suffix the reading ends with
    for (const typeSuffix of possibleReadings) {
      if (reading.endsWith(typeSuffix)) {
        const shortReading = reading.substring(0, reading.length - typeSuffix.length);
        return { text: shortText, reading: shortReading };
      }
    }

    return null;
  }

  /**
   * Gets English type string for Japanese suffix
   */
  private getTypeString(suffix: string): string {
    const typeMap: Record<string, string> = {
      '都': 'Metropolis',
      '道': 'Prefecture',  // For Hokkaido
      '府': 'Prefecture',  // For Osaka/Kyoto
      '県': 'Prefecture',
      '市': 'City',
      '区': 'Ward',
      '町': 'Town',
      '村': 'Village'
    };
    return typeMap[suffix] || '';
  }

  async insert(): Promise<void> {
    let inserted = 0;
    let updated = 0;
    let skipped = 0;

    for (const municipality of this.municipalities) {
      const result = await this.testEntry(municipality);

      if (result === null) {
        // No existing entry - insert new
        await this.insertEntry(municipality);
        inserted++;
      } else if (result.match === 'update') {
        // Entry exists but needs update
        await this.updateEntry(municipality, result.seq);
        updated++;
      } else {
        // Entry already exists with correct data
        skipped++;
      }
    }

    console.log(`  Inserted: ${inserted}, Updated: ${updated}, Skipped: ${skipped}`);
  }

  /**
   * Tests if entry exists and needs updating
   */
  private async testEntry(entry: Municipality): Promise<MatchGlossesResult> {
    // Search for entries with this text and reading
    const words = entry.fullName.split(' ');

    // Try to find existing entry
    const result = await matchGlosses(entry.text, entry.reading, words, {
      normalize: removeDiacritics
    });

    return result;
  }

  /**
   * Inserts a new municipality entry
   */
  private async insertEntry(entry: Municipality): Promise<void> {
    // Create XML entry in JMDict format
    const xml = this.generateEntryXml(entry);

    await loadEntry(xml, {
      seq: parseInt(entry.id),  // Use seq from CSV (first column)
      ifExists: 'skip',
      conjugateP: false  // Place names don't conjugate
    });
  }

  /**
   * Updates an existing entry with new gloss
   */
  private async updateEntry(entry: Municipality, seq: number): Promise<void> {
    // Add new sense with the municipality gloss
    await addNewSense(seq, ['n'], [entry.fullName]);
  }

  /**
   * Generates JMDict XML for a municipality entry
   */
  private generateEntryXml(entry: Municipality): string {
    return `<entry>
  <k_ele>
    <keb>${entry.text}</keb>
  </k_ele>
  <r_ele>
    <reb>${entry.reading}</reb>
  </r_ele>
  <sense>
    <pos>&n;</pos>
    <gloss>${entry.fullName}</gloss>
  </sense>
</entry>`;
  }
}

/**
 * Ward entry structure
 */
interface Ward {
  id: string;
  text: string;           // e.g., "新宿区"
  reading: string;        // e.g., "しんじゅくく"
  cityText: string;       // e.g., "東京"
  cityRomanized: string;  // e.g., "Tokyo"
  wardRomanized: string;  // e.g., "Shinjuku"
  fullName: string;       // e.g., "Shinjuku Ward, Tokyo"
}

/**
 * Ward CSV Loader
 * Ported from dict-custom.lisp:243-283 ward-csv class
 *
 * Loads Japanese administrative ward data from gyoseiku.csv
 * Wards are 区 subdivisions of major cities
 */
export class WardCsvLoader extends CustomSource {
  description = 'ward data from gyoseiku.csv';
  private sourceFile: string;
  private wards: Ward[] = [];

  constructor(sourceFile: string) {
    super();
    this.sourceFile = sourceFile;
  }

  async slurp(): Promise<number> {
    const content = fs.readFileSync(this.sourceFile, 'utf-8');
    const lines = content.trim().split('\n');

    let currentCity: { text: string; reading: string; romanized: string } | null = null;

    for (const line of lines) {
      const parts = line.split(',');
      if (parts.length < 3) {
        continue;
      }

      const [id, text, reading] = parts;

      // Check if this is a city line (ends with 市)
      if (text.endsWith('市')) {
        // Extract city name (remove 市 suffix)
        const cityText = text.substring(0, text.length - 1);
        const cityReading = reading.substring(0, reading.length - 1); // Remove し
        const cityRomanized = romanizeWordGeo(cityReading);

        currentCity = {
          text: cityText,
          reading: cityReading,
          romanized: cityRomanized
        };
      } else if (text.endsWith('区') && currentCity) {
        // This is a ward line
        const ward = this.processWardEntry(id, text, reading, currentCity);
        if (ward) {
          this.wards.push(ward);
        }
      }
    }

    return this.wards.length;
  }

  /**
   * Processes a ward entry
   */
  private processWardEntry(
    id: string,
    fullText: string,
    fullReading: string,
    city: { text: string; reading: string; romanized: string }
  ): Ward | null {
    // Full text is like "札幌市中央区", we want just "中央区"
    // Full reading is like "さっぽろしちゅうおうく", we want just "ちゅうおうく"

    // Remove city prefix from text
    const cityWithSuffix = city.text + '市';
    if (!fullText.startsWith(cityWithSuffix)) {
      // Fallback: just use the full text
      const wardText = fullText;
      const wardReading = fullReading;
      const wardRomanized = romanizeWordGeo(wardReading.substring(0, wardReading.length - 1)); // Remove く

      return {
        id,
        text: wardText,
        reading: wardReading,
        cityText: city.text,
        cityRomanized: city.romanized,
        wardRomanized,
        fullName: `${wardRomanized} Ward, ${city.romanized}`
      };
    }

    const wardText = fullText.substring(cityWithSuffix.length);

    // Remove city prefix from reading
    const cityReadingWithSuffix = city.reading + 'し';
    const wardReading = fullReading.substring(cityReadingWithSuffix.length);

    // Romanize ward name (remove 区/く suffix)
    const wardNameOnly = wardReading.substring(0, wardReading.length - 1); // Remove く
    const wardRomanized = romanizeWordGeo(wardNameOnly);

    return {
      id,
      text: wardText,
      reading: wardReading,
      cityText: city.text,
      cityRomanized: city.romanized,
      wardRomanized,
      fullName: `${wardRomanized} Ward, ${city.romanized}`
    };
  }

  async insert(): Promise<void> {
    let inserted = 0;
    let updated = 0;
    let skipped = 0;

    for (const ward of this.wards) {
      const result = await this.testEntry(ward);

      if (result === null) {
        // No existing entry - insert new
        await this.insertEntry(ward);
        inserted++;
      } else if (result.match === 'update') {
        // Entry exists but needs update
        await this.updateEntry(ward, result.seq);
        updated++;
      } else {
        // Entry already exists with correct data
        skipped++;
      }
    }

    console.log(`  Inserted: ${inserted}, Updated: ${updated}, Skipped: ${skipped}`);
  }

  /**
   * Tests if entry exists and needs updating
   */
  private async testEntry(entry: Ward): Promise<MatchGlossesResult> {
    const words = entry.fullName.split(/[, ]+/);

    const result = await matchGlosses(entry.text, entry.reading, words, {
      normalize: removeDiacritics
    });

    return result;
  }

  /**
   * Inserts a new ward entry
   */
  private async insertEntry(entry: Ward): Promise<void> {
    const xml = this.generateEntryXml(entry);

    await loadEntry(xml, {
      seq: parseInt(entry.id),  // Use seq from CSV (first column)
      ifExists: 'skip',
      conjugateP: false  // Place names don't conjugate
    });
  }

  /**
   * Updates an existing entry with new gloss
   */
  private async updateEntry(entry: Ward, seq: number): Promise<void> {
    await addNewSense(seq, ['n'], [entry.fullName]);
  }

  /**
   * Generates JMDict XML for a ward entry
   */
  private generateEntryXml(entry: Ward): string {
    return `<entry>
  <k_ele>
    <keb>${entry.text}</keb>
  </k_ele>
  <r_ele>
    <reb>${entry.reading}</reb>
  </r_ele>
  <sense>
    <pos>&n;</pos>
    <gloss>${entry.fullName}</gloss>
  </sense>
</entry>`;
  }
}
