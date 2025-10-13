/**
 * Database maintenance and calculation functions
 * Includes: best readings calculation, kanji statistics, etc.
 */

import { getConnection } from '@ichiran/core';

/**
 * Calculates and populates best_kana and best_kanji fields
 * Ported from dict-load.lisp:532-548 load-best-readings
 *
 * For each kanji_text:
 *   - Finds the first kana_text (by ord) that is not restricted
 *   - Sets best_kana to that kana text
 *
 * For each kana_text:
 *   - Finds the first kanji_text (by ord) that is not restricted
 *   - Sets best_kanji to that kanji text
 */
export async function calculateBestReadings(options: { reset?: boolean } = {}): Promise<void> {
  const sql = getConnection();

  // Reset existing values if requested
  if (options.reset) {
    console.log('Resetting existing best_kana and best_kanji values...');
    await sql`UPDATE kanji_text SET best_kana = NULL`;
    await sql`UPDATE kana_text SET best_kanji = NULL`;
  }

  console.log('Calculating best_kana for kanji_text...');

  // Update best_kana for each kanji_text entry
  // Logic from dict-load.lisp:494-509 (defmethod set-reading ((obj kanji-text)))
  // For each kanji, find the first kana reading (by ord) where:
  // - kana is not marked as nokanji
  // - either no restrictions exist, OR this kanji is in the allowed list for that kana
  await sql`
    UPDATE kanji_text kt
    SET best_kana = (
      SELECT r.text
      FROM kana_text r
      WHERE r.seq = kt.seq
        AND r.nokanji = false
        AND (
          -- No restrictions for this kana reading
          NOT EXISTS (
            SELECT 1 FROM restricted_readings rr
            WHERE rr.seq = kt.seq AND rr.reading = r.text
          )
          OR
          -- This kanji is in the allowed list for this kana reading
          EXISTS (
            SELECT 1 FROM restricted_readings rr
            WHERE rr.seq = kt.seq
              AND rr.reading = r.text
              AND rr.text = kt.text
          )
        )
      ORDER BY r.ord
      LIMIT 1
    )
    FROM entry e
    WHERE kt.seq = e.seq AND e.root_p = true AND kt.best_kana IS NULL
  `;

  console.log('Calculating best_kanji for kana_text...');

  // Update best_kanji for each kana_text entry
  // Logic from dict-load.lisp:511-531 (defmethod set-reading ((obj kana-text)))
  // For each kana, find the first kanji reading (by ord) where:
  // - kana is not marked as nokanji
  // - either no restrictions exist, OR kanji is in the allowed list
  await sql`
    UPDATE kana_text r
    SET best_kanji = (
      SELECT kt.text
      FROM kanji_text kt
      WHERE kt.seq = r.seq
        AND (
          -- No restrictions for this kana reading
          NOT EXISTS (
            SELECT 1 FROM restricted_readings rr
            WHERE rr.seq = r.seq AND rr.reading = r.text
          )
          OR
          -- This kanji is in the allowed list for this kana reading
          EXISTS (
            SELECT 1 FROM restricted_readings rr
            WHERE rr.seq = r.seq
              AND rr.reading = r.text
              AND rr.text = kt.text
          )
        )
      ORDER BY kt.ord
      LIMIT 1
    )
    FROM entry e
    WHERE r.seq = e.seq AND e.root_p = true AND r.nokanji = false AND r.best_kanji IS NULL
  `;

  // Get counts for verification
  const [kanjiStats] = await sql`
    SELECT
      COUNT(*) as total_kanji,
      COUNT(best_kana) as with_best_kana
    FROM kanji_text
  `;

  const [kanaStats] = await sql`
    SELECT
      COUNT(*) as total_kana,
      COUNT(best_kanji) as with_best_kanji
    FROM kana_text
  `;

  console.log(`✓ Best readings calculated:`);
  console.log(`  Kanji texts: ${kanjiStats.withBestKana}/${kanjiStats.totalKanji} have best_kana`);
  console.log(`  Kana texts:  ${kanaStats.withBestKanji}/${kanaStats.totalKana} have best_kanji`);
}

/**
 * Calculates kanji usage statistics
 * Updates stat_common and stat_irregular fields in kanji table
 *
 * stat_common: Count of common entries that use this kanji
 * stat_irregular: Count of entries with irregular readings for this kanji
 */
export async function calculateKanjiStatistics(): Promise<void> {
  const sql = getConnection();

  console.log('Calculating kanji statistics...');
  console.log('  This may take several minutes for 13,000+ kanji...');

  // Use a more efficient approach: create a temporary table with kanji usage counts
  // This avoids the slow LIKE queries for each kanji
  console.log('  Calculating stat_common...');

  // First, create a temporary table to hold the kanji usage counts
  await sql`
    CREATE TEMP TABLE IF NOT EXISTS temp_kanji_counts (
      kanji_text TEXT PRIMARY KEY,
      common_count INTEGER
    )
  `;

  // Clear any existing data
  await sql`TRUNCATE TABLE temp_kanji_counts`;

  // For each common kanji_text entry, extract individual kanji characters
  // and count their occurrences
  await sql`
    INSERT INTO temp_kanji_counts (kanji_text, common_count)
    SELECT
      unnest(string_to_array(kt.text, '')) as kanji_text,
      COUNT(DISTINCT e.seq) as common_count
    FROM entry e
    JOIN kanji_text kt ON kt.seq = e.seq
    WHERE kt.common IS NOT NULL
    GROUP BY kanji_text
  `;

  // Now update the kanji table using the temp table
  await sql`
    UPDATE kanji k
    SET stat_common = COALESCE(tc.common_count, 0)
    FROM temp_kanji_counts tc
    WHERE k.text = tc.kanji_text
  `;

  // Clean up
  await sql`DROP TABLE temp_kanji_counts`;

  // Note: stat_irregular calculation is complex and requires reading analysis
  // For now, we'll leave it at 0 (as initialized)
  // TODO: Implement irregular reading detection
  // This requires comparing kanji readings with actual dictionary readings
  // and determining which ones are non-standard

  const [stats] = await sql`
    SELECT
      COUNT(*) as total_kanji,
      COUNT(CASE WHEN stat_common > 0 THEN 1 END) as kanji_with_common,
      SUM(stat_common) as total_common_usage
    FROM kanji
  `;

  console.log(`✓ Kanji statistics calculated:`);
  console.log(`  Total kanji: ${stats.totalKanji}`);
  console.log(`  Kanji used in common words: ${stats.kanjiWithCommon}`);
  console.log(`  Total common word usage: ${stats.totalCommonUsage}`);
}

/**
 * Calculates reading statistics for kanji readings
 * Updates stat_common field in reading table
 *
 * Counts how many common entries use each reading
 */
export async function calculateReadingStatistics(): Promise<void> {
  const sql = getConnection();

  console.log('Calculating reading statistics...');

  // Use a more efficient approach similar to kanji statistics
  // Create a temporary mapping table for faster lookups

  console.log('  Building temporary kanji-entry mapping...');
  await sql`
    CREATE TEMP TABLE IF NOT EXISTS temp_kanji_entries (
      kanji_char TEXT,
      entry_seq INTEGER,
      is_common BOOLEAN
    )
  `;

  await sql`TRUNCATE TABLE temp_kanji_entries`;

  // Extract all kanji characters from common entries
  await sql`
    INSERT INTO temp_kanji_entries (kanji_char, entry_seq, is_common)
    SELECT
      unnest(string_to_array(kt.text, '')) as kanji_char,
      kt.seq,
      (kt.common IS NOT NULL) as is_common
    FROM kanji_text kt
  `;

  // Create index for faster lookups
  await sql`CREATE INDEX IF NOT EXISTS idx_temp_kanji_char ON temp_kanji_entries(kanji_char)`;

  // Update reading statistics using the temp table
  console.log('  Calculating stats for all readings...');
  await sql`
    UPDATE reading r
    SET stat_common = (
      SELECT COUNT(DISTINCT tke.entry_seq)
      FROM temp_kanji_entries tke
      JOIN kanji k ON k.id = r.kanji_id AND k.text = tke.kanji_char
      WHERE tke.is_common = true
    )
  `;

  // Clean up
  await sql`DROP TABLE temp_kanji_entries`;

  const [stats] = await sql`
    SELECT
      COUNT(*) as total_readings,
      COUNT(CASE WHEN stat_common > 0 THEN 1 END) as readings_with_common
    FROM reading
  `;

  console.log(`✓ Reading statistics calculated:`);
  console.log(`  Total readings: ${stats.totalReadings}`);
  console.log(`  Readings used in common words: ${stats.readingsWithCommon}`);
}
