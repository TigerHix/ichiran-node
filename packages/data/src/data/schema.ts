/**
 * Database schema definitions for Ichiran dictionary tables
 * Ported from ~/ichiran/dict.lisp lines 26-326
 */

import type postgres from 'postgres';

/**
 * Creates the entry table - Main dictionary entries (root_p flag distinguishes base vs conjugated)
 */
export const createEntryTable = (sql: postgres.Sql) => sql`
  CREATE TABLE IF NOT EXISTS entry (
    seq INTEGER PRIMARY KEY,
    content TEXT NOT NULL,
    root_p BOOLEAN NOT NULL DEFAULT false,
    n_kanji INTEGER NOT NULL DEFAULT 0,
    n_kana INTEGER NOT NULL DEFAULT 0,
    primary_nokanji BOOLEAN NOT NULL DEFAULT false
  )
`;

/**
 * Creates the kanji_text table - Kanji readings with best_kana links
 */
export const createKanjiTextTable = (sql: postgres.Sql) => sql`
  CREATE TABLE IF NOT EXISTS kanji_text (
    id SERIAL PRIMARY KEY,
    seq INTEGER NOT NULL,
    text TEXT NOT NULL,
    ord INTEGER NOT NULL,
    common INTEGER,
    common_tags TEXT NOT NULL DEFAULT '',
    conjugate_p BOOLEAN NOT NULL DEFAULT true,
    nokanji BOOLEAN NOT NULL DEFAULT false,
    best_kana TEXT
  )
`;

/**
 * Creates the kana_text table - Kana readings with best_kanji links
 */
export const createKanaTextTable = (sql: postgres.Sql) => sql`
  CREATE TABLE IF NOT EXISTS kana_text (
    id SERIAL PRIMARY KEY,
    seq INTEGER NOT NULL,
    text TEXT NOT NULL,
    ord INTEGER NOT NULL,
    common INTEGER,
    common_tags TEXT NOT NULL DEFAULT '',
    conjugate_p BOOLEAN NOT NULL DEFAULT true,
    nokanji BOOLEAN NOT NULL DEFAULT false,
    best_kanji TEXT
  )
`;

/**
 * Creates the sense table - Word senses/meanings
 */
export const createSenseTable = (sql: postgres.Sql) => sql`
  CREATE TABLE IF NOT EXISTS sense (
    id SERIAL PRIMARY KEY,
    seq INTEGER NOT NULL,
    ord INTEGER NOT NULL
  )
`;

/**
 * Creates the gloss table - English glosses for each sense
 */
export const createGlossTable = (sql: postgres.Sql) => sql`
  CREATE TABLE IF NOT EXISTS gloss (
    id SERIAL PRIMARY KEY,
    sense_id INTEGER NOT NULL,
    text TEXT NOT NULL,
    ord INTEGER NOT NULL
  )
`;

/**
 * Creates entry_reading_sig table - Signature-based dedupe for conjugated entries
 * Ensures idempotent conjugation generation across parallel runs
 */
export const createEntryReadingSigTable = (sql: postgres.Sql) => sql`
  CREATE TABLE IF NOT EXISTS entry_reading_sig (
    sig TEXT PRIMARY KEY,
    seq INTEGER NOT NULL UNIQUE
  )
`;

/**
 * Creates the sense_prop table - Sense properties (POS, misc, field, dial, etc.)
 */
export const createSensePropTable = (sql: postgres.Sql) => sql`
  CREATE TABLE IF NOT EXISTS sense_prop (
    id SERIAL PRIMARY KEY,
    tag TEXT NOT NULL,
    sense_id INTEGER NOT NULL,
    text TEXT NOT NULL,
    ord INTEGER NOT NULL,
    seq INTEGER NOT NULL
  )
`;

/**
 * Creates the restricted_readings table - Kanji-kana reading restrictions
 */
export const createRestrictedReadingsTable = (sql: postgres.Sql) => sql`
  CREATE TABLE IF NOT EXISTS restricted_readings (
    id SERIAL PRIMARY KEY,
    seq INTEGER NOT NULL,
    reading TEXT NOT NULL,
    text TEXT NOT NULL
  )
`;

/**
 * Creates the conjugation table - Conjugation links (from → via → seq)
 */
export const createConjugationTable = (sql: postgres.Sql) => sql`
  CREATE TABLE IF NOT EXISTS conjugation (
    id SERIAL PRIMARY KEY,
    seq INTEGER NOT NULL,
    "from" INTEGER NOT NULL,
    via INTEGER,
    via_n INTEGER GENERATED ALWAYS AS (COALESCE(via, -1)) STORED
  )
`;

/**
 * Creates the conj_prop table - Conjugation properties (type, pos, neg, fml)
 */
export const createConjPropTable = (sql: postgres.Sql) => sql`
  CREATE TABLE IF NOT EXISTS conj_prop (
    id SERIAL PRIMARY KEY,
    conj_id INTEGER NOT NULL,
    conj_type INTEGER NOT NULL,
    pos TEXT NOT NULL,
    neg BOOLEAN,
    fml BOOLEAN,
    neg_i SMALLINT GENERATED ALWAYS AS (CASE WHEN neg THEN 1 WHEN neg IS FALSE THEN 0 ELSE -1 END) STORED,
    fml_i SMALLINT GENERATED ALWAYS AS (CASE WHEN fml THEN 1 WHEN fml IS FALSE THEN 0 ELSE -1 END) STORED
  )
`;

/**
 * Creates the conj_source_reading table - Maps conjugated forms to source readings
 */
export const createConjSourceReadingTable = (sql: postgres.Sql) => sql`
  CREATE TABLE IF NOT EXISTS conj_source_reading (
    id SERIAL PRIMARY KEY,
    conj_id INTEGER NOT NULL,
    text TEXT NOT NULL,
    source_text TEXT NOT NULL
  )
`;

/**
 * Creates indexes for kanji_text table
 */
export const createKanjiTextIndexes = async (sql: postgres.Sql) => {
  await sql`CREATE INDEX IF NOT EXISTS kanji_text_seq_idx ON kanji_text (seq)`;
  await sql`CREATE INDEX IF NOT EXISTS kanji_text_ord_idx ON kanji_text (ord)`;
  await sql`CREATE INDEX IF NOT EXISTS kanji_text_text_idx ON kanji_text (text)`;
  await sql`CREATE INDEX IF NOT EXISTS kanji_text_common_idx ON kanji_text (common)`;
  // Unique constraint to prevent duplicate texts per entry (guards against race conditions)
  await sql`CREATE UNIQUE INDEX IF NOT EXISTS kanji_text_seq_text_unique_idx ON kanji_text (seq, text)`;
};

/**
 * Creates indexes for kana_text table
 */
export const createKanaTextIndexes = async (sql: postgres.Sql) => {
  await sql`CREATE INDEX IF NOT EXISTS kana_text_seq_idx ON kana_text (seq)`;
  await sql`CREATE INDEX IF NOT EXISTS kana_text_ord_idx ON kana_text (ord)`;
  await sql`CREATE INDEX IF NOT EXISTS kana_text_text_idx ON kana_text (text)`;
  await sql`CREATE INDEX IF NOT EXISTS kana_text_common_idx ON kana_text (common)`;
  // Unique constraint to prevent duplicate texts per entry (guards against race conditions)
  await sql`CREATE UNIQUE INDEX IF NOT EXISTS kana_text_seq_text_unique_idx ON kana_text (seq, text)`;
};

/**
 * Creates indexes for sense table
 */
export const createSenseIndexes = (sql: postgres.Sql) => sql`
  CREATE INDEX IF NOT EXISTS sense_seq_idx ON sense (seq)
`;

/**
 * Creates indexes for gloss table
 */
export const createGlossIndexes = (sql: postgres.Sql) => sql`
  CREATE INDEX IF NOT EXISTS gloss_sense_id_idx ON gloss (sense_id)
`;

/**
 * Creates indexes for sense_prop table
 */
export const createSensePropIndexes = (sql: postgres.Sql) => sql.begin(async (sql) => {
  await sql`CREATE INDEX IF NOT EXISTS sense_prop_sense_id_tag_idx ON sense_prop (sense_id, tag)`;
  await sql`CREATE INDEX IF NOT EXISTS sense_prop_tag_text_idx ON sense_prop (tag, text)`;
  await sql`CREATE INDEX IF NOT EXISTS sense_prop_seq_tag_text_idx ON sense_prop (seq, tag, text)`;
});

/**
 * Creates indexes for restricted_readings table
 */
export const createRestrictedReadingsIndexes = (sql: postgres.Sql) => sql`
  CREATE INDEX IF NOT EXISTS restricted_readings_seq_reading_idx ON restricted_readings (seq, reading)
`;

/**
 * Creates indexes for conjugation table
 */
export const createConjugationIndexes = async (sql: postgres.Sql) => {
  await sql`CREATE INDEX IF NOT EXISTS conjugation_seq_idx ON conjugation (seq)`;
  await sql`CREATE INDEX IF NOT EXISTS conjugation_from_idx ON conjugation ("from")`;
  // Unique constraint on generated column for clean ON CONFLICT handling
  await sql`
    DO $$ 
    BEGIN
      IF NOT EXISTS (
        SELECT 1 FROM pg_constraint WHERE conname = 'conjugation_from_seq_via_n_unique'
      ) THEN
        ALTER TABLE conjugation ADD CONSTRAINT conjugation_from_seq_via_n_unique UNIQUE ("from", seq, via_n);
      END IF;
    END $$`;
};

/**
 * Creates indexes for conj_prop table
 */
export const createConjPropIndexes = async (sql: postgres.Sql) => {
  await sql`CREATE INDEX IF NOT EXISTS conj_prop_conj_id_idx ON conj_prop (conj_id)`;
  // Unique constraint on generated columns for clean ON CONFLICT handling
  await sql`
    DO $$ 
    BEGIN
      IF NOT EXISTS (
        SELECT 1 FROM pg_constraint WHERE conname = 'conj_prop_unique'
      ) THEN
        ALTER TABLE conj_prop ADD CONSTRAINT conj_prop_unique UNIQUE (conj_id, conj_type, pos, neg_i, fml_i);
      END IF;
    END $$`;
};

/**
 * Creates indexes for conj_source_reading table
 */
export const createConjSourceReadingIndexes = async (sql: postgres.Sql) => {
  await sql`CREATE INDEX IF NOT EXISTS conj_source_reading_conj_id_text_idx ON conj_source_reading (conj_id, text)`;
  // Unique constraint for clean ON CONFLICT handling
  await sql`
    DO $$ 
    BEGIN
      IF NOT EXISTS (
        SELECT 1 FROM pg_constraint WHERE conname = 'conj_source_reading_unique'
      ) THEN
        ALTER TABLE conj_source_reading ADD CONSTRAINT conj_source_reading_unique UNIQUE (conj_id, text, source_text);
      END IF;
    END $$`;
};

/**
 * Helper to handle FK creation errors - only ignore "already exists"
 */
const handleFkError = (constraintName: string) => (error: any) => {
  const isAlreadyExists = error.message && (
    error.message.includes('already exists') ||
    error.code === '42710' // PostgreSQL: duplicate object
  );
  
  if (!isAlreadyExists) {
    console.error(`Failed to create constraint ${constraintName}:`, error.message);
    throw error;
  }
  // Silently ignore "already exists" errors
};

/**
 * Creates foreign key constraints for all tables
 */
export const createForeignKeys = (sql: postgres.Sql) => sql.begin(async (sql) => {
  // kanji_text foreign keys
  await sql`
    ALTER TABLE kanji_text
    ADD CONSTRAINT kanji_text_seq_fkey
    FOREIGN KEY (seq) REFERENCES entry(seq) ON DELETE CASCADE
  `.catch(handleFkError('kanji_text_seq_fkey'));

  // kana_text foreign keys
  await sql`
    ALTER TABLE kana_text
    ADD CONSTRAINT kana_text_seq_fkey
    FOREIGN KEY (seq) REFERENCES entry(seq) ON DELETE CASCADE
  `.catch(handleFkError('kana_text_seq_fkey'));

  // sense foreign keys
  await sql`
    ALTER TABLE sense
    ADD CONSTRAINT sense_seq_fkey
    FOREIGN KEY (seq) REFERENCES entry(seq) ON DELETE CASCADE
  `.catch(handleFkError('sense_seq_fkey'));

  // gloss foreign keys
  await sql`
    ALTER TABLE gloss
    ADD CONSTRAINT gloss_sense_id_fkey
    FOREIGN KEY (sense_id) REFERENCES sense(id) ON DELETE CASCADE
  `.catch(handleFkError('gloss_sense_id_fkey'));

  // sense_prop foreign keys
  await sql`
    ALTER TABLE sense_prop
    ADD CONSTRAINT sense_prop_seq_fkey
    FOREIGN KEY (seq) REFERENCES entry(seq) ON DELETE CASCADE
  `.catch(handleFkError('sense_prop_seq_fkey'));

  await sql`
    ALTER TABLE sense_prop
    ADD CONSTRAINT sense_prop_sense_id_fkey
    FOREIGN KEY (sense_id) REFERENCES sense(id) ON DELETE CASCADE
  `.catch(handleFkError('sense_prop_sense_id_fkey'));

  // restricted_readings foreign keys
  await sql`
    ALTER TABLE restricted_readings
    ADD CONSTRAINT restricted_readings_seq_fkey
    FOREIGN KEY (seq) REFERENCES entry(seq) ON DELETE CASCADE
  `.catch(handleFkError('restricted_readings_seq_fkey'));

  // conjugation foreign keys
  await sql`
    ALTER TABLE conjugation
    ADD CONSTRAINT conjugation_seq_fkey
    FOREIGN KEY (seq) REFERENCES entry(seq) ON DELETE CASCADE
  `.catch(handleFkError('conjugation_seq_fkey'));

  await sql`
    ALTER TABLE conjugation
    ADD CONSTRAINT conjugation_from_fkey
    FOREIGN KEY ("from") REFERENCES entry(seq) ON DELETE CASCADE
  `.catch(handleFkError('conjugation_from_fkey'));

  // conj_prop foreign keys
  await sql`
    ALTER TABLE conj_prop
    ADD CONSTRAINT conj_prop_conj_id_fkey
    FOREIGN KEY (conj_id) REFERENCES conjugation(id) ON DELETE CASCADE
  `.catch(handleFkError('conj_prop_conj_id_fkey'));

  // conj_source_reading foreign keys
  await sql`
    ALTER TABLE conj_source_reading
    ADD CONSTRAINT conj_source_reading_conj_id_fkey
    FOREIGN KEY (conj_id) REFERENCES conjugation(id) ON DELETE CASCADE
  `.catch(handleFkError('conj_source_reading_conj_id_fkey'));
});

/**
 * Drops all dictionary tables in reverse dependency order
 */
/**
 * Creates the kanji table - Kanjidic2 character data
 * Ported from kanji.lisp:11-29 kanji dao
 */
async function createKanjiTable(sql: postgres.Sql): Promise<void> {
  await sql`
    CREATE TABLE IF NOT EXISTS kanji (
      id SERIAL PRIMARY KEY,
      text VARCHAR NOT NULL,
      radical_c INTEGER NOT NULL,
      radical_n INTEGER NOT NULL,
      grade INTEGER,
      strokes INTEGER NOT NULL,
      freq INTEGER,
      stat_common INTEGER NOT NULL DEFAULT 0,
      stat_irregular INTEGER NOT NULL DEFAULT 0
    )
  `;
}

/**
 * Creates the reading table - Kanji readings (on/kun)
 * Ported from kanji.lisp:43-57 reading dao
 */
async function createReadingTable(sql: postgres.Sql): Promise<void> {
  await sql`
    CREATE TABLE IF NOT EXISTS reading (
      id SERIAL PRIMARY KEY,
      kanji_id INTEGER NOT NULL,
      type VARCHAR NOT NULL,
      text VARCHAR NOT NULL,
      suffixp BOOLEAN NOT NULL DEFAULT false,
      prefixp BOOLEAN NOT NULL DEFAULT false,
      stat_common INTEGER NOT NULL DEFAULT 0
    )
  `;
}

/**
 * Creates the okurigana table - Okurigana for kanji readings
 * Ported from kanji.lisp:71-81 okurigana dao
 */
async function createOkuriganaTable(sql: postgres.Sql): Promise<void> {
  await sql`
    CREATE TABLE IF NOT EXISTS okurigana (
      id SERIAL PRIMARY KEY,
      reading_id INTEGER NOT NULL,
      text VARCHAR NOT NULL
    )
  `;
}

/**
 * Creates the meaning table - Kanji meanings (English)
 * Ported from kanji.lisp:83-92 meaning dao
 */
async function createMeaningTable(sql: postgres.Sql): Promise<void> {
  await sql`
    CREATE TABLE IF NOT EXISTS meaning (
      id SERIAL PRIMARY KEY,
      kanji_id INTEGER NOT NULL,
      text VARCHAR NOT NULL
    )
  `;
}

/**
 * Creates indexes for kanji tables
 */
async function createKanjiTableIndexes(sql: postgres.Sql): Promise<void> {
  await sql`CREATE INDEX IF NOT EXISTS kanji_text_idx ON kanji(text)`;
  await sql`CREATE INDEX IF NOT EXISTS kanji_radical_c_idx ON kanji(radical_c)`;
  await sql`CREATE INDEX IF NOT EXISTS kanji_radical_n_idx ON kanji(radical_n)`;
  await sql`CREATE INDEX IF NOT EXISTS kanji_grade_idx ON kanji(grade)`;
  await sql`CREATE INDEX IF NOT EXISTS kanji_strokes_idx ON kanji(strokes)`;
  await sql`CREATE INDEX IF NOT EXISTS kanji_freq_idx ON kanji(freq)`;
  await sql`CREATE INDEX IF NOT EXISTS kanji_stat_common_idx ON kanji(stat_common)`;
  await sql`CREATE INDEX IF NOT EXISTS kanji_stat_irregular_idx ON kanji(stat_irregular)`;

  await sql`CREATE INDEX IF NOT EXISTS reading_kanji_id_idx ON reading(kanji_id)`;
  await sql`CREATE INDEX IF NOT EXISTS reading_type_idx ON reading(type)`;
  await sql`CREATE INDEX IF NOT EXISTS reading_text_idx ON reading(text)`;
  await sql`CREATE INDEX IF NOT EXISTS reading_stat_common_idx ON reading(stat_common)`;

  await sql`CREATE INDEX IF NOT EXISTS okurigana_reading_id_idx ON okurigana(reading_id)`;

  await sql`CREATE INDEX IF NOT EXISTS meaning_kanji_id_idx ON meaning(kanji_id)`;
  await sql`CREATE INDEX IF NOT EXISTS meaning_text_idx ON meaning(text)`;
}

/**
 * Creates foreign keys for kanji tables
 */
async function createKanjiTableForeignKeys(sql: postgres.Sql): Promise<void> {
  await sql`
    ALTER TABLE reading
    ADD CONSTRAINT reading_kanji_id_fkey
    FOREIGN KEY (kanji_id) REFERENCES kanji(id)
    ON DELETE CASCADE
  `;

  await sql`
    ALTER TABLE okurigana
    ADD CONSTRAINT okurigana_reading_id_fkey
    FOREIGN KEY (reading_id) REFERENCES reading(id)
    ON DELETE CASCADE
  `;

  await sql`
    ALTER TABLE meaning
    ADD CONSTRAINT meaning_kanji_id_fkey
    FOREIGN KEY (kanji_id) REFERENCES kanji(id)
    ON DELETE CASCADE
  `;
}

export const dropAllTables = (sql: postgres.Sql) => sql.begin(async (sql) => {
  // Drop Kanjidic2 tables first
  await sql`DROP TABLE IF EXISTS okurigana CASCADE`;
  await sql`DROP TABLE IF EXISTS meaning CASCADE`;
  await sql`DROP TABLE IF EXISTS reading CASCADE`;
  await sql`DROP TABLE IF EXISTS kanji CASCADE`;

  // Drop JMDict tables
  await sql`DROP TABLE IF EXISTS conj_source_reading CASCADE`;
  await sql`DROP TABLE IF EXISTS conj_prop CASCADE`;
  await sql`DROP TABLE IF EXISTS conjugation CASCADE`;
  await sql`DROP TABLE IF EXISTS restricted_readings CASCADE`;
  await sql`DROP TABLE IF EXISTS sense_prop CASCADE`;
  await sql`DROP TABLE IF EXISTS gloss CASCADE`;
  await sql`DROP TABLE IF EXISTS sense CASCADE`;
  await sql`DROP TABLE IF EXISTS kana_text CASCADE`;
  await sql`DROP TABLE IF EXISTS kanji_text CASCADE`;
  await sql`DROP TABLE IF EXISTS entry CASCADE`;

  // Drop sequences
  await sql`DROP SEQUENCE IF EXISTS entry_seq_generator CASCADE`;
});

// Start conjugated entry IDs at 10M to avoid conflicts with JMDict entries (max ~2.8M)
const CONJUGATED_ENTRY_SEQ_START = 10_000_000;

/**
 * Creates a sequence for generating conjugated entry seq numbers
 * Starts from 10,000,000 to avoid conflicts with JMDict entries (max ~2.8M)
 */
export const createEntrySeqGenerator = async (sql: postgres.Sql) => {
  // Note: DDL commands don't support parameterized queries, must use literal value
  await sql`CREATE SEQUENCE IF NOT EXISTS entry_seq_generator START WITH 10000000`;
};

/**
 * Resets the sequence to continue from current MAX(seq)
 * Used when loading into existing database
 */
export const resetEntrySeqGenerator = async (sql: postgres.Sql) => {
  const result = await sql<Array<{ max: number | null }>>`
    SELECT MAX(seq) as max FROM entry
  `;
  const maxSeq = result[0].max || CONJUGATED_ENTRY_SEQ_START;
  await sql`SELECT setval('entry_seq_generator', ${maxSeq})`;
};

/**
 * Initializes all dictionary tables (drops and recreates)
 * This is the TypeScript equivalent of init-tables() from dict-load.lisp:7-14
 */
export async function initTables(sql: postgres.Sql): Promise<void> {
  console.log('Dropping existing tables...');
  await dropAllTables(sql);

  console.log('Creating core tables...');
  await createEntryTable(sql);
  await createKanjiTextTable(sql);
  await createKanaTextTable(sql);
  await createSenseTable(sql);
  await createGlossTable(sql);
  await createSensePropTable(sql);
  await createRestrictedReadingsTable(sql);
  await createEntryReadingSigTable(sql);

  console.log('Creating conjugation tables...');
  await createConjugationTable(sql);
  await createConjPropTable(sql);
  await createConjSourceReadingTable(sql);

  console.log('Creating Kanjidic2 tables...');
  await createKanjiTable(sql);
  await createReadingTable(sql);
  await createOkuriganaTable(sql);
  await createMeaningTable(sql);

  console.log('Creating indexes...');
  await createKanjiTextIndexes(sql);
  await createKanaTextIndexes(sql);
  await createSenseIndexes(sql);
  await createGlossIndexes(sql);
  await createSensePropIndexes(sql);
  await createRestrictedReadingsIndexes(sql);
  await createConjugationIndexes(sql);
  await createConjPropIndexes(sql);
  await createConjSourceReadingIndexes(sql);
  await createKanjiTableIndexes(sql);

  console.log('Creating foreign key constraints...');
  await createForeignKeys(sql);
  await createKanjiTableForeignKeys(sql);

  console.log('Creating sequences...');
  await createEntrySeqGenerator(sql);

  console.log('Database schema initialized successfully!');
}
