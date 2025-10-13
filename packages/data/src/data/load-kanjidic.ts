/**
 * Kanjidic2 XML Loader
 * Loads kanji character data from kanjidic2.xml
 * Ported from ~/ichiran/kanji.lisp lines 127-195
 *
 * Populates 4 tables:
 * - kanji: Character metadata (radical, grade, stroke count, frequency)
 * - reading: On/kun readings with prefix/suffix flags
 * - okurigana: Okurigana for kun readings
 * - meaning: English meanings
 */

import fs from 'fs';
import { gunzipSync } from 'zlib';
import { XMLParser } from 'fast-xml-parser';
import { getConnection, asHiragana } from '@ichiran/core';

interface KanjidicCharacter {
  literal: string;
  radical?: {
    rad_value: Array<{
      '#text': string;
      rad_type: string;
    }>;
  };
  misc?: {
    grade?: number;
    stroke_count: number | number[];
    freq?: number;
  };
  reading_meaning?: {
    rmgroup?: {
      reading?: Array<{
        '#text': string;
        r_type: string;
        r_status?: string;
        on_type?: string;
      }>;
      meaning?: Array<{
        '#text'?: string;
        m_lang?: string;
      } | string>;
    };
  };
}

interface KanjiRecord {
  text: string;
  radicalC: number;
  radicalN: number;
  grade: number | null;
  strokes: number;
  freq: number | null;
}

interface ReadingRecord {
  kanjiId: number;
  type: 'ja_on' | 'ja_kun' | 'ja_onkun';  // Full type names from XML, matching Lisp
  text: string;
  suffixp: boolean;
  prefixp: boolean;
}

interface OkuriganaRecord {
  readingId: number;
  text: string;
}

interface MeaningRecord {
  kanjiId: number;
  text: string;
}

/**
 * Loads kanjidic2.xml file and populates kanji tables
 */
export async function loadKanjidic(xmlPath: string): Promise<void> {
  console.log('Loading kanjidic2.xml...');

  // Read file, auto-decompress if .gz
  let content: string;
  if (xmlPath.endsWith('.gz')) {
    const compressed = fs.readFileSync(xmlPath);
    content = gunzipSync(compressed).toString('utf-8');
  } else {
    content = fs.readFileSync(xmlPath, 'utf-8');
  }

  // Parse XML
  const parser = new XMLParser({
    ignoreAttributes: false,
    attributeNamePrefix: '',
    textNodeName: '#text',
    trimValues: true,
    parseTagValue: true,
    parseAttributeValue: true,
    isArray: (name) => {
      // These elements can appear multiple times
      return ['character', 'rad_value', 'reading', 'meaning', 'stroke_count'].includes(name);
    }
  });

  const parsed = parser.parse(content);
  const characters = parsed.kanjidic2?.character || [];

  console.log(`Found ${characters.length} characters to load`);

  let loaded = 0;
  const batchSize = 500;  // Increased batch size for better performance

  // Process in batches
  for (let i = 0; i < characters.length; i += batchSize) {
    const batch = characters.slice(i, Math.min(i + batchSize, characters.length));
    await loadKanjiBatch(batch);

    loaded += batch.length;
    console.log(`Loaded ${loaded} / ${characters.length} characters...`);
  }

  console.log(`✓ Loaded ${loaded} kanji characters`);
}

/**
 * Loads a batch of kanji characters with all their readings and meanings
 * Uses batch inserts for improved performance
 */
async function loadKanjiBatch(characters: KanjidicCharacter[]): Promise<void> {
  if (characters.length === 0) return;

  const sql = getConnection();

  // Prepare kanji records for batch insert
  const kanjiRecords: KanjiRecord[] = [];

  for (const char of characters) {
    const literal = char.literal;
    if (!literal) continue;

    // Extract radicals (classical and nelson)
    let radicalC = 1;
    let radicalN = 1;

    if (char.radical?.rad_value) {
      for (const rad of char.radical.rad_value) {
        const value = parseInt(rad['#text']);
        if (rad.rad_type === 'classical') {
          radicalC = value;
        } else if (rad.rad_type === 'nelson_c') {
          radicalN = value;
        }
      }
    }

    // Extract misc properties
    const misc = char.misc;
    const grade = misc?.grade || null;

    // Stroke count can be array, take first value
    let strokes = 1;
    if (misc?.stroke_count) {
      strokes = Array.isArray(misc.stroke_count)
        ? misc.stroke_count[0]
        : misc.stroke_count;
    }

    const freq = misc?.freq || null;

    kanjiRecords.push({
      text: literal,
      radicalC,
      radicalN,
      grade,
      strokes,
      freq
    });
  }

  // Batch insert all kanji - use sql() helper for proper array handling
  const insertedKanji = await sql<Array<{ id: number; text: string }>>`
    INSERT INTO kanji ${sql(kanjiRecords.map(k => ({
      text: k.text,
      radical_c: k.radicalC,
      radical_n: k.radicalN,
      grade: k.grade,
      strokes: k.strokes,
      freq: k.freq,
      stat_common: 0,
      stat_irregular: 0
    })))}
    RETURNING id, text
  `;

  // Create mapping from text to id for later use
  const textToId = new Map<string, number>();
  for (const k of insertedKanji) {
    textToId.set(k.text, k.id);
  }

  // Collect all readings, okurigana, and meanings for batch insert
  const allReadings: ReadingRecord[] = [];
  const allMeanings: MeaningRecord[] = [];

  for (const char of characters) {
    const literal = char.literal;
    if (!literal) continue;

    const kanjiId = textToId.get(literal);
    if (!kanjiId) continue;

    const rmgroup = char.reading_meaning?.rmgroup;
    if (!rmgroup) continue;

    // Collect readings
    if (rmgroup.reading) {
      for (const reading of rmgroup.reading) {
        const rtype = reading.r_type;
        if (rtype !== 'ja_on' && rtype !== 'ja_kun') continue;

        const readingText = reading['#text'];
        if (!readingText) continue;

        // Parse reading text for okurigana and prefix/suffix markers
        // Convert to hiragana first (matches Lisp kanji.lisp:167 - katakana ON readings → hiragana)
        let text = asHiragana(readingText);
        let suffixp = false;
        let prefixp = false;

        // For kun readings, check for okurigana (marked with '.')
        // Note: We use the full type name from XML ("ja_kun") not simplified ("kun")
        if (rtype === 'ja_kun') {
          if (text.startsWith('-')) {
            prefixp = true;
            text = text.substring(1);
          }
          if (text.endsWith('-')) {
            suffixp = true;
            text = text.substring(0, text.length - 1);
          }
          // Remove okurigana for now (will be stored separately)
          const parts = text.split('.');
          if (parts.length > 1) {
            text = parts[0];
          }
        } else {
          if (text.startsWith('-')) {
            prefixp = true;
            text = text.substring(1);
          }
          if (text.endsWith('-')) {
            suffixp = true;
            text = text.substring(0, text.length - 1);
          }
        }

        // Store full type name from XML ("ja_on"/"ja_kun"), matching Lisp
        allReadings.push({ kanjiId, type: rtype, text, suffixp, prefixp });
      }
    }

    // Collect meanings
    if (rmgroup.meaning) {
      for (const meaning of rmgroup.meaning) {
        let text: string;
        let lang: string | undefined;

        if (typeof meaning === 'string') {
          text = meaning;
          lang = 'en';
        } else {
          text = meaning['#text'] || '';
          lang = meaning.m_lang || 'en';
        }

        if (!text || lang !== 'en') continue;

        allMeanings.push({ kanjiId, text });
      }
    }
  }

  // Batch insert all readings
  if (allReadings.length > 0) {
    const insertedReadings = await sql<Array<{ id: number; kanjiId: number; text: string }>>`
      INSERT INTO reading ${sql(allReadings.map(r => ({
        kanji_id: r.kanjiId,
        type: r.type,
        text: r.text,
        suffixp: r.suffixp,
        prefixp: r.prefixp,
        stat_common: 0
      })))}
      RETURNING id, kanji_id, text
    `;

    // Now process okurigana for kun readings
    const allOkurigana: OkuriganaRecord[] = [];

    for (const char of characters) {
      const literal = char.literal;
      if (!literal) continue;

      const kanjiId = textToId.get(literal);
      if (!kanjiId) continue;

      const rmgroup = char.reading_meaning?.rmgroup;
      if (!rmgroup?.reading) continue;

      for (const reading of rmgroup.reading) {
        if (reading.r_type !== 'ja_kun') continue;

        let text = reading['#text'];
        if (!text) continue;

        // Remove prefix/suffix markers
        if (text.startsWith('-')) text = text.substring(1);
        if (text.endsWith('-')) text = text.substring(0, text.length - 1);

        // Extract okurigana
        const parts = text.split('.');
        if (parts.length <= 1) continue;

        const baseText = parts[0];
        const okurigana = parts.slice(1);

        // Find the reading ID
        const readingRecord = insertedReadings.find(
          (r: { id: number; kanjiId: number; text: string }) => r.kanjiId === kanjiId && r.text === baseText
        );
        if (!readingRecord) continue;

        // Add okurigana entries
        for (const okuri of okurigana) {
          if (okuri) {
            allOkurigana.push({ readingId: readingRecord.id, text: okuri });
          }
        }
      }
    }

    // Batch insert all okurigana
    if (allOkurigana.length > 0) {
      await sql`
        INSERT INTO okurigana ${sql(allOkurigana, 'readingId', 'text')}
      `;
    }
  }

  // Batch insert all meanings
  if (allMeanings.length > 0) {
    await sql`
      INSERT INTO meaning ${sql(allMeanings, 'kanjiId', 'text')}
    `;
  }
}

/**
 * Loads a single kanji character with all its readings and meanings
 */
export async function loadKanji(char: KanjidicCharacter): Promise<number> {
  const sql = getConnection();

  // Extract character literal
  const literal = char.literal;
  if (!literal) {
    throw new Error('Character missing literal element');
  }

  // Extract radicals (classical and nelson)
  let radicalC = 1;  // Default
  let radicalN = 1;  // Default

  if (char.radical?.rad_value) {
    for (const rad of char.radical.rad_value) {
      const value = parseInt(rad['#text']);
      if (rad.rad_type === 'classical') {
        radicalC = value;
      } else if (rad.rad_type === 'nelson_c') {
        radicalN = value;
      }
    }
  }

  // Extract misc properties
  const misc = char.misc;
  const grade = misc?.grade || null;

  // Stroke count can be array, take first value
  let strokes = 1;
  if (misc?.stroke_count) {
    strokes = Array.isArray(misc.stroke_count)
      ? misc.stroke_count[0]
      : misc.stroke_count;
  }

  const freq = misc?.freq || null;

  // Insert kanji record
  const [kanji] = await sql<[{ id: number }]>`
    INSERT INTO kanji (text, radical_c, radical_n, grade, strokes, freq, stat_common, stat_irregular)
    VALUES (${literal}, ${radicalC}, ${radicalN}, ${grade}, ${strokes}, ${freq}, 0, 0)
    RETURNING id
  `;

  const kanjiId = kanji.id;

  // Load readings and meanings
  const rmgroup = char.reading_meaning?.rmgroup;
  if (rmgroup) {
    // Load readings
    if (rmgroup.reading) {
      await loadReadings(rmgroup.reading, kanjiId);
    }

    // Load meanings
    if (rmgroup.meaning) {
      await loadMeanings(rmgroup.meaning, kanjiId);
    }
  }

  return kanjiId;
}

/**
 * Loads readings for a kanji character
 */
export async function loadReadings(
  readings: Array<{
    '#text': string;
    r_type: string;
    r_status?: string;
    on_type?: string;
  }>,
  kanjiId: number
): Promise<void> {
  const sql = getConnection();

  for (const reading of readings) {
    const rtype = reading.r_type;

    // Only process ja_on and ja_kun readings
    if (rtype !== 'ja_on' && rtype !== 'ja_kun') {
      continue;
    }

    const readingText = reading['#text'];
    if (!readingText) {
      continue;
    }

    // Parse reading text for okurigana and prefix/suffix markers
    // Convert to hiragana first (matches Lisp kanji.lisp:167 - katakana ON readings → hiragana)
    let text = asHiragana(readingText);
    let okurigana: string[] = [];
    let suffixp = false;
    let prefixp = false;

    // For kun readings, check for okurigana (marked with '.')
    // Note: We use the full type name from XML ("ja_kun") not simplified ("kun")
    if (rtype === 'ja_kun') {
      // Check for prefix marker (starts with '-')
      if (text.startsWith('-')) {
        prefixp = true;
        text = text.substring(1);
      }

      // Check for suffix marker (ends with '-')
      if (text.endsWith('-')) {
        suffixp = true;
        text = text.substring(0, text.length - 1);
      }

      // Split on '.' to separate base reading from okurigana
      const parts = text.split('.');
      if (parts.length > 1) {
        text = parts[0];  // Base reading
        okurigana = parts.slice(1);  // Okurigana parts
      }
    } else {
      // For on readings, check for prefix/suffix markers
      if (text.startsWith('-')) {
        prefixp = true;
        text = text.substring(1);
      }
      if (text.endsWith('-')) {
        suffixp = true;
        text = text.substring(0, text.length - 1);
      }
    }

    // Insert reading
    // Store full type name from XML ("ja_on"/"ja_kun"), matching Lisp kanji.lisp:143
    const [readingRecord] = await sql<[{ id: number }]>`
      INSERT INTO reading (kanji_id, type, text, suffixp, prefixp, stat_common)
      VALUES (${kanjiId}, ${rtype}, ${text}, ${suffixp}, ${prefixp}, 0)
      RETURNING id
    `;

    const readingId = readingRecord.id;

    // Insert okurigana if present
    for (const okuri of okurigana) {
      if (okuri) {
        await sql`
          INSERT INTO okurigana (reading_id, text)
          VALUES (${readingId}, ${okuri})
        `;
      }
    }
  }
}

/**
 * Loads meanings for a kanji character
 */
export async function loadMeanings(
  meanings: Array<{
    '#text'?: string;
    m_lang?: string;
  } | string>,
  kanjiId: number
): Promise<void> {
  const sql = getConnection();

  for (const meaning of meanings) {
    // Handle both object and string formats
    let text: string;
    let lang: string | undefined;

    if (typeof meaning === 'string') {
      text = meaning;
      lang = 'en';  // Default to English
    } else {
      text = meaning['#text'] || '';
      lang = meaning.m_lang || 'en';
    }

    if (!text) {
      continue;
    }

    // Only load English meanings
    if (lang !== 'en') {
      continue;
    }

    await sql`
      INSERT INTO meaning (kanji_id, text)
      VALUES (${kanjiId}, ${text})
    `;
  }
}

/**
 * Gets kanji statistics (for debugging/verification)
 */
export async function getKanjiStats(): Promise<{
  totalKanji: number;
  totalReadings: number;
  totalOkurigana: number;
  totalMeanings: number;
}> {
  const sql = getConnection();

  const [stats] = await sql`
    SELECT
      (SELECT COUNT(*) FROM kanji) as total_kanji,
      (SELECT COUNT(*) FROM reading) as total_readings,
      (SELECT COUNT(*) FROM okurigana) as total_okurigana,
      (SELECT COUNT(*) FROM meaning) as total_meanings
  `;

  return {
    totalKanji: stats.totalKanji,
    totalReadings: stats.totalReadings,
    totalOkurigana: stats.totalOkurigana,
    totalMeanings: stats.totalMeanings
  };
}
