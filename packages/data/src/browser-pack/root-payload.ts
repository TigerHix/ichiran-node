import { createHash } from 'node:crypto';
import type postgres from 'postgres';

// Keep these physical constants synchronized with
// packages/portable/src/root-payload.ts. Cross-package round-trip tests lock the
// writer and the zero-dependency reader together without making the compiler a
// browser dependency.
const FORMAT_VERSION = 2;
const HEADER_BYTES = 128;
const MAGIC = 'IROOT002';
const ALIGNMENT = 8;

const SPAN_BYTES = 4;
const FORM_BYTES = 11;
const ENTRY_BYTES = 9;
const RESTRICTION_BYTES = 12;
const POS_SET_BYTES = 6;

const SURFACE_REF_STRING_BIT = 0x8000_0000;
const SURFACE_REF_NONE = 0xffff_ffff;

const HEADER_FLAGS_OFFSET = 12;
const HEADER_TOTAL_BYTES_OFFSET = 16;
const HEADER_CHECKSUM_OFFSET = 20;
const PAYLOAD_CHECKSUM_OFFSET = 24;
const HEADER_RESERVED_OFFSET = 28;

const SURFACE_COUNT_OFFSET = 32;
const FORM_COUNT_OFFSET = 36;
const ENTRY_COUNT_OFFSET = 40;
const RESTRICTION_COUNT_OFFSET = 44;
const STRING_COUNT_OFFSET = 48;
const POS_SET_COUNT_OFFSET = 52;
const POS_MEMBER_COUNT_OFFSET = 56;

const SPAN_STRIDE_OFFSET = 60;
const FORM_STRIDE_OFFSET = 61;
const ENTRY_STRIDE_OFFSET = 62;
const RESTRICTION_STRIDE_OFFSET = 63;

const SPANS_OFFSET = 64;
const FORMS_OFFSET = 68;
const ENTRIES_OFFSET = 72;
const RESTRICTIONS_OFFSET = 76;
const POS_SETS_OFFSET = 80;
const POS_MEMBERS_OFFSET = 84;
const STRING_OFFSETS_OFFSET = 88;
const STRING_DATA_OFFSET = 92;
const STRING_DATA_BYTES_OFFSET = 96;

const FORM_COMMON_NULL = 63;
const FORM_ROUTE_KANA_BIT = 0x40;
const FORM_CONJUGATABLE_BIT = 0x80;
const FORM_NOKANJI_BIT = 0x80;

const ENTRY_PRIMARY_NOKANJI_BIT = 1 << 0;
const ENTRY_ARCHIVED_BIT = 1 << 1;
const ENTRY_PREFER_KANA_BIT = 1 << 2;
const ENTRY_PREFER_KANA_ORDINAL_ZERO_BIT = 1 << 3;

const CRC32_POLYNOMIAL = 0xedb8_8320;
const CRC32_TABLE = new Uint32Array(256);
for (let value = 0; value < CRC32_TABLE.length; value++) {
  let checksum = value;
  for (let bit = 0; bit < 8; bit++) {
    checksum = (checksum & 1) === 1
      ? CRC32_POLYNOMIAL ^ (checksum >>> 1)
      : checksum >>> 1;
  }
  CRC32_TABLE[value] = checksum >>> 0;
}

const UTF8_ENCODER = new TextEncoder();

export type RootPayloadRoute = 'kanji' | 'kana';

export interface RootPayloadEntrySource {
  readonly seq: number;
  readonly nKanji: number;
  readonly nKana: number;
  readonly primaryNokanji: boolean;
  readonly archived: boolean;
  readonly preferKana: boolean;
  /** At least one `uk` property belongs to sense ordinal zero. */
  readonly preferKanaOnOrdinalZero: boolean;
  /** Scoring's exact non-archived POS set, not every descriptive POS value. */
  readonly pos: readonly string[];
}

export interface RootPayloadFormSource {
  readonly surface: string;
  readonly route: RootPayloadRoute;
  readonly seq: number;
  /**
   * Dense semantic order reproducing legacy bulk lookup's unordered physical
   * scan followed by `unshift`. The compiler derives it from the pinned
   * database, but neither physical tuple IDs nor surrogate row IDs leave SQL.
   */
  readonly lookupOrder?: number;
  readonly ord: number;
  readonly common: number | null;
  readonly commonTags: string;
  readonly conjugatable: boolean;
  readonly nokanji: boolean;
  readonly best: string | null;
}

export interface RootPayloadRestrictionSource {
  readonly seq: number;
  readonly reading: string;
  readonly written: string;
}

export interface RootPayloadSource {
  readonly entries: readonly RootPayloadEntrySource[];
  readonly forms: readonly RootPayloadFormSource[];
  readonly restrictions: readonly RootPayloadRestrictionSource[];
}

export interface RootPayloadBuildStats {
  readonly directOrderProjection: {
    /** Every directly reachable root form, including singleton surfaces. */
    readonly rows: number;
    readonly surfaces: number;
    /** SHA-256 of ordered semantic `(route,surface,rank,rootSeq)` tuples. */
    readonly sha256: string;
  };
  readonly counts: {
    readonly surfaces: number;
    readonly forms: number;
    readonly entries: number;
    readonly restrictions: number;
    readonly strings: number;
    readonly posSets: number;
    readonly posMembers: number;
    readonly pooledSurfaceExceptions: number;
  };
  readonly sectionBytes: {
    readonly header: number;
    readonly spans: number;
    readonly forms: number;
    readonly entries: number;
    readonly restrictions: number;
    readonly posSets: number;
    readonly posMembers: number;
    readonly stringOffsets: number;
    readonly stringData: number;
    readonly alignmentPadding: number;
    readonly total: number;
  };
}

export interface RootPayloadBuild {
  readonly bytes: Uint8Array;
  readonly stats: RootPayloadBuildStats;
}

export class RootPayloadEncodingError extends Error {
  constructor(message: string) {
    super(message);
    this.name = 'RootPayloadEncodingError';
  }
}

interface EntryQueryRow {
  seq: number;
  nKanji: number;
  nKana: number;
  primaryNokanji: boolean;
  archived: boolean;
  preferKana: boolean;
  preferKanaOnOrdinalZero: boolean;
}

interface PosQueryRow {
  seq: number;
  pos: string;
}

interface FormQueryRow extends RootPayloadFormSource {}

interface LegacyFormOrderRow {
  surface: string;
  route: RootPayloadRoute;
  seq: number;
}

interface RestrictionQueryRow {
  seq: number;
  reading: string;
  written: string;
}

function crc32(bytes: Uint8Array): number {
  let checksum = 0xffff_ffff;
  for (let index = 0; index < bytes.byteLength; index++) {
    checksum = CRC32_TABLE[(checksum ^ bytes[index]!) & 0xff]! ^ (checksum >>> 8);
  }
  return (checksum ^ 0xffff_ffff) >>> 0;
}

function align(value: number): number {
  return Math.ceil(value / ALIGNMENT) * ALIGNMENT;
}

function checkedUint32(value: number, label: string): number {
  if (!Number.isSafeInteger(value) || value < 0 || value > 0xffff_ffff) {
    throw new RootPayloadEncodingError(`${label} must fit uint32`);
  }
  return value;
}

function checkedProduct(count: number, stride: number, label: string): number {
  return checkedUint32(count * stride, `${label} byte length`);
}

function writeUint24(view: DataView, offset: number, value: number, label: string): void {
  if (!Number.isSafeInteger(value) || value < 0 || value > 0xff_ffff) {
    throw new RootPayloadEncodingError(`${label} must fit uint24`);
  }
  view.setUint16(offset, value & 0xffff, true);
  view.setUint8(offset + 2, value >>> 16);
}

function compareBytes(left: Uint8Array, right: Uint8Array): number {
  const shared = Math.min(left.byteLength, right.byteLength);
  for (let index = 0; index < shared; index++) {
    const difference = left[index]! - right[index]!;
    if (difference !== 0) return difference;
  }
  return left.byteLength - right.byteLength;
}

/** Explicit UTF-8 byte ordering; never locale or UTF-16 code-unit ordering. */
export function compareRootPayloadText(left: string, right: string): number {
  if (left === right) return 0;
  return compareBytes(UTF8_ENCODER.encode(left), UTF8_ENCODER.encode(right));
}

function makeTextComparator(values: Iterable<string>): (left: string, right: string) => number {
  const encoded = new Map<string, Uint8Array>();
  for (const value of values) {
    if (!encoded.has(value)) encoded.set(value, UTF8_ENCODER.encode(value));
  }
  return (left, right) => {
    if (left === right) return 0;
    const leftBytes = encoded.get(left);
    const rightBytes = encoded.get(right);
    if (!leftBytes || !rightBytes) {
      throw new RootPayloadEncodingError('UTF-8 comparator received an unregistered string');
    }
    return compareBytes(leftBytes, rightBytes);
  };
}

/** Runtime route parity with core `testWord(surface, 'kana')`. */
export function isRootPayloadKanaSurface(surface: string): boolean {
  return /^[ァ-ヺヽヾーぁ-ゔゝゞ]+$/u.test(surface);
}

function canonicalStrings(values: readonly string[]): string[] {
  const unique = [...new Set(values)];
  const compare = makeTextComparator(unique);
  unique.sort(compare);
  return unique;
}

function compareNumberArrays(left: readonly number[], right: readonly number[]): number {
  const shared = Math.min(left.length, right.length);
  for (let index = 0; index < shared; index++) {
    const difference = left[index]! - right[index]!;
    if (difference !== 0) return difference;
  }
  return left.length - right.length;
}

function numberArrayKey(values: readonly number[]): string {
  return values.join(',');
}

function writeMagic(target: Uint8Array): void {
  for (let index = 0; index < MAGIC.length; index++) {
    target[index] = MAGIC.charCodeAt(index);
  }
}

function surfaceReference(
  surface: string | null,
  ranks: ReadonlyMap<string, number>,
  stringIds: ReadonlyMap<string, number>
): number {
  if (surface === null) return SURFACE_REF_NONE;
  const rank = ranks.get(surface);
  if (rank !== undefined) return rank;
  const stringId = stringIds.get(surface);
  if (stringId === undefined || stringId >= 0x7fff_ffff) {
    throw new RootPayloadEncodingError(`Missing pooled surface exception ${JSON.stringify(surface)}`);
  }
  return (SURFACE_REF_STRING_BIT | stringId) >>> 0;
}

/**
 * Read the exact scoring/root projection from PostgreSQL.
 *
 * The form query intentionally follows the analyzer's one-table route. Rows in
 * `kana_text` with non-kana surfaces and rows in `kanji_text` with all-kana
 * surfaces are unreachable through current exact lookup and are not hot forms.
 */
export async function loadRootPayloadSource(sql: postgres.Sql): Promise<RootPayloadSource> {
  const [entryRows, posRows, formRows, restrictionRows] = await Promise.all([
    sql.unsafe<EntryQueryRow[]>(`
      WITH archived AS (
        SELECT s.seq
        FROM sense s
        LEFT JOIN sense_prop sp
          ON sp.sense_id = s.id
          AND sp.tag = 'misc'
          AND sp.text IN ('arch', 'obsc', 'rare')
        GROUP BY s.seq
        HAVING EVERY(sp.id IS NOT NULL)
      ), prefer_kana AS (
        SELECT sp.seq,
               TRUE AS "preferKana",
               BOOL_OR(s.ord = 0) AS "preferKanaOnOrdinalZero"
        FROM sense_prop sp
        LEFT JOIN sense s ON s.id = sp.sense_id
        WHERE sp.tag = 'misc' AND sp.text = 'uk'
        GROUP BY sp.seq
      )
      SELECT e.seq,
             e.n_kanji AS "nKanji",
             e.n_kana AS "nKana",
             e.primary_nokanji AS "primaryNokanji",
             (a.seq IS NOT NULL) AS archived,
             COALESCE(pk."preferKana", FALSE) AS "preferKana",
             COALESCE(pk."preferKanaOnOrdinalZero", FALSE) AS "preferKanaOnOrdinalZero"
      FROM entry e
      LEFT JOIN archived a ON a.seq = e.seq
      LEFT JOIN prefer_kana pk ON pk.seq = e.seq
      WHERE e.root_p = TRUE
      ORDER BY e.seq
    `),
    sql.unsafe<PosQueryRow[]>(`
      SELECT sp1.seq, sp1.text AS pos
      FROM sense_prop sp1
      JOIN entry e ON e.seq = sp1.seq AND e.root_p = TRUE
      LEFT JOIN sense_prop sp2
        ON sp1.sense_id = sp2.sense_id
        AND sp2.tag = 'misc'
        AND sp2.text IN ('arch', 'obsc', 'rare')
      WHERE sp1.tag = 'pos' AND sp2.id IS NULL
      GROUP BY sp1.seq, sp1.text
      ORDER BY sp1.seq, sp1.text COLLATE "C"
    `),
    sql.unsafe<FormQueryRow[]>(`
      SELECT * FROM (
        SELECT kt.text AS surface,
               'kanji'::text AS route,
               kt.seq,
               (ROW_NUMBER() OVER (
                 PARTITION BY kt.text
                 ORDER BY kt.ctid DESC
               ) - 1)::integer AS "lookupOrder",
               kt.ord,
               kt.common,
               COALESCE(kt.common_tags, '') AS "commonTags",
               kt.conjugate_p AS conjugatable,
               kt.nokanji,
               kt.best_kana AS best
        FROM kanji_text kt
        JOIN entry e USING (seq)
        WHERE e.root_p = TRUE
          AND NOT (kt.text ~ '^[ァ-ヺヽヾーぁ-ゔゝゞ]+$')

        UNION ALL

        SELECT rt.text AS surface,
               'kana'::text AS route,
               rt.seq,
               (ROW_NUMBER() OVER (
                 PARTITION BY rt.text
                 ORDER BY rt.ctid DESC
               ) - 1)::integer AS "lookupOrder",
               rt.ord,
               rt.common,
               COALESCE(rt.common_tags, '') AS "commonTags",
               rt.conjugate_p AS conjugatable,
               rt.nokanji,
               rt.best_kanji AS best
        FROM kana_text rt
        JOIN entry e USING (seq)
        WHERE e.root_p = TRUE
          AND rt.text ~ '^[ァ-ヺヽヾーぁ-ゔゝゞ]+$'
      ) forms
      ORDER BY surface COLLATE "C", route, "lookupOrder"
    `),
    sql.unsafe<RestrictionQueryRow[]>(`
      SELECT rr.seq, rr.reading, rr.text AS written
      FROM restricted_readings rr
      JOIN entry e USING (seq)
      WHERE e.root_p = TRUE
      ORDER BY rr.seq, rr.reading COLLATE "C", rr.text COLLATE "C"
    `)
  ]);

  const requestedKanjiSurfaces: string[] = [];
  const requestedKanaSurfaces: string[] = [];
  let requestedSurface: string | undefined;
  for (const form of formRows) {
    if (form.surface === requestedSurface) continue;
    (form.route === 'kana' ? requestedKanaSurfaces : requestedKanjiSurfaces).push(form.surface);
    requestedSurface = form.surface;
  }

  // Production proof for the otherwise-observable unordered-query behavior in
  // core findSubstringWords(). OFFSET 0 keeps one parameterized text-index
  // scan per requested surface, matching core's no-ORDER-BY lookup. PostgreSQL
  // visits equal index keys by ascending heap tuple, and core's `unshift`
  // reverses that stream. Only route/surface/root seq leave SQL; CTID is used
  // solely by the canonical projection above and is never emitted or hashed.
  const legacyFormRows = await sql.unsafe<LegacyFormOrderRow[]>(`
    SELECT requested.surface, 'kanji'::text AS route, found.seq
    FROM unnest($1::text[]) requested(surface)
    CROSS JOIN LATERAL (
      SELECT kt.seq
      FROM kanji_text kt
      JOIN entry e USING (seq)
      WHERE kt.text = requested.surface AND e.root_p = TRUE
      OFFSET 0
    ) found

    UNION ALL

    SELECT requested.surface, 'kana'::text AS route, found.seq
    FROM unnest($2::text[]) requested(surface)
    CROSS JOIN LATERAL (
      SELECT rt.seq
      FROM kana_text rt
      JOIN entry e USING (seq)
      WHERE rt.text = requested.surface AND e.root_p = TRUE
      OFFSET 0
    ) found
  `, [requestedKanjiSurfaces, requestedKanaSurfaces]);
  const legacyOrder = new Map<string, number[]>();
  for (const row of legacyFormRows) {
    const key = `${row.route}\u0000${row.surface}`;
    const values = legacyOrder.get(key) ?? [];
    values.unshift(row.seq);
    legacyOrder.set(key, values);
  }
  if (legacyFormRows.length !== formRows.length) {
    throw new RootPayloadEncodingError(
      `Legacy direct-order proof covered ${legacyFormRows.length} of ${formRows.length} forms`
    );
  }
  let proofIndex = 0;
  while (proofIndex < formRows.length) {
    const first = formRows[proofIndex]!;
    let proofEnd = proofIndex + 1;
    while (proofEnd < formRows.length && formRows[proofEnd]!.surface === first.surface) proofEnd++;
    const projected = formRows.slice(proofIndex, proofEnd).map(form => form.seq);
    const observed = legacyOrder.get(`${first.route}\u0000${first.surface}`);
    if (
      observed === undefined
      || observed.length !== projected.length
      || observed.some((seq, index) => seq !== projected[index])
    ) {
      throw new RootPayloadEncodingError(
        `Legacy direct-order proof differs for ${JSON.stringify(first.surface)}`
      );
    }
    legacyOrder.delete(`${first.route}\u0000${first.surface}`);
    proofIndex = proofEnd;
  }
  if (legacyOrder.size !== 0) {
    throw new RootPayloadEncodingError('Legacy direct-order proof contains unprojected forms');
  }

  const entries: RootPayloadEntrySource[] = [];
  let posIndex = 0;
  for (const row of entryRows) {
    const pos: string[] = [];
    while (posIndex < posRows.length && posRows[posIndex]!.seq === row.seq) {
      pos.push(posRows[posIndex]!.pos);
      posIndex++;
    }
    if (posIndex < posRows.length && posRows[posIndex]!.seq < row.seq) {
      throw new RootPayloadEncodingError(`POS row has no root entry: ${posRows[posIndex]!.seq}`);
    }
    entries.push({ ...row, pos });
  }
  if (posIndex !== posRows.length) {
    throw new RootPayloadEncodingError(`POS row has no root entry: ${posRows[posIndex]!.seq}`);
  }

  return {
    entries,
    forms: formRows,
    restrictions: restrictionRows
  };
}

/** Build the directly readable root payload from normalized logical rows. */
export function buildRootPayload(source: RootPayloadSource): RootPayloadBuild {
  const entries = source.entries.map((entry) => ({
    ...entry,
    pos: canonicalStrings(entry.pos)
  }));
  entries.sort((left, right) => left.seq - right.seq);

  const entryIndex = new Map<number, number>();
  for (let index = 0; index < entries.length; index++) {
    const entry = entries[index]!;
    checkedUint32(entry.seq, 'Entry sequence');
    if (
      !Number.isSafeInteger(entry.nKanji) || entry.nKanji < 0 || entry.nKanji > 0xff
      || !Number.isSafeInteger(entry.nKana) || entry.nKana < 0 || entry.nKana > 0xff
    ) {
      throw new RootPayloadEncodingError(`Entry ${entry.seq} form counts must fit uint8`);
    }
    if (entry.preferKanaOnOrdinalZero && !entry.preferKana) {
      throw new RootPayloadEncodingError(`Entry ${entry.seq} has ordinal-zero uk without uk`);
    }
    if (entryIndex.has(entry.seq)) {
      throw new RootPayloadEncodingError(`Duplicate root entry ${entry.seq}`);
    }
    entryIndex.set(entry.seq, index);
  }

  const formSurfaceValues = source.forms.map((form) => form.surface);
  const compareSurface = makeTextComparator(formSurfaceValues);
  const hasLookupOrder = source.forms.some(form => form.lookupOrder !== undefined);
  if (hasLookupOrder && source.forms.some(form => form.lookupOrder === undefined)) {
    throw new RootPayloadEncodingError('Root forms mix physical and synthetic lookup order');
  }
  const forms = [...source.forms].sort((left, right) =>
    compareSurface(left.surface, right.surface)
    || (left.route < right.route ? -1 : left.route > right.route ? 1 : 0)
    || (hasLookupOrder
      ? left.lookupOrder! - right.lookupOrder!
      : left.seq - right.seq || left.ord - right.ord)
  );

  const directOrderProjection = createHash('sha256');
  let directOrderRows = 0;
  let directOrderSurfaces = 0;
  for (let start = 0; start < forms.length;) {
    let end = start + 1;
    while (end < forms.length && forms[end]!.surface === forms[start]!.surface) end++;
    directOrderSurfaces++;
    for (let index = start; index < end; index++) {
      const form = forms[index]!;
      const order = index - start;
      if (hasLookupOrder && form.lookupOrder !== order) {
        throw new RootPayloadEncodingError(
          `Direct form order for ${JSON.stringify(form.surface)} is not dense at ${order}`
        );
      }
      directOrderProjection.update(`${JSON.stringify([
        form.route,
        form.surface,
        order,
        form.seq
      ])}\n`);
      directOrderRows++;
    }
    start = end;
  }

  const surfaceRanks = new Map<string, number>();
  let previousSurface: string | undefined;
  let currentSpanCount = 0;
  let maxSpanCount = 0;
  for (const form of forms) {
    if (form.surface.length === 0) {
      throw new RootPayloadEncodingError('Root form surface cannot be empty');
    }
    const route: RootPayloadRoute = isRootPayloadKanaSurface(form.surface) ? 'kana' : 'kanji';
    if (form.route !== route) {
      throw new RootPayloadEncodingError(
        `Inactive-route form ${JSON.stringify(form.surface)} is marked ${form.route}; expected ${route}`
      );
    }
    if (!entryIndex.has(form.seq)) {
      throw new RootPayloadEncodingError(`Form ${JSON.stringify(form.surface)} has no root entry ${form.seq}`);
    }
    if (form.lookupOrder !== undefined) checkedUint32(form.lookupOrder, 'Form lookup order');
    if (!Number.isSafeInteger(form.ord) || form.ord < 0 || form.ord > 0x7f) {
      throw new RootPayloadEncodingError(`Form ordinal ${form.ord} must fit seven bits`);
    }
    if (
      form.common !== null
      && (!Number.isSafeInteger(form.common) || form.common < 0 || form.common >= FORM_COMMON_NULL)
    ) {
      throw new RootPayloadEncodingError(`Form common rank ${form.common} cannot be packed`);
    }

    if (previousSurface !== form.surface) {
      if (currentSpanCount > maxSpanCount) maxSpanCount = currentSpanCount;
      const rank = surfaceRanks.size;
      if (rank >= SURFACE_REF_STRING_BIT) {
        throw new RootPayloadEncodingError('Direct surface ranks exceed 31 bits');
      }
      surfaceRanks.set(form.surface, rank);
      previousSurface = form.surface;
      currentSpanCount = 1;
    } else {
      currentSpanCount++;
    }
  }
  if (currentSpanCount > maxSpanCount) maxSpanCount = currentSpanCount;
  if (maxSpanCount > 0xff) {
    throw new RootPayloadEncodingError('A direct surface has more than 255 root forms');
  }
  if (forms.length > 0xff_ffff || entries.length > 0xff_ffff) {
    throw new RootPayloadEncodingError('Root form or entry count exceeds the packed uint24 limit');
  }

  const restrictionValues = source.restrictions.flatMap((restriction) => [
    restriction.reading,
    restriction.written
  ]);
  const compareRestrictionText = makeTextComparator(restrictionValues);
  const restrictions = [...source.restrictions].sort((left, right) =>
    left.seq - right.seq
    || compareRestrictionText(left.reading, right.reading)
    || compareRestrictionText(left.written, right.written)
  );
  for (const restriction of restrictions) {
    if (!entryIndex.has(restriction.seq)) {
      throw new RootPayloadEncodingError(`Restriction has no root entry ${restriction.seq}`);
    }
  }

  const pooledStrings = new Set<string>(['']);
  const exceptionStrings = new Set<string>();
  for (const entry of entries) {
    for (const pos of entry.pos) pooledStrings.add(pos);
  }
  for (const form of forms) {
    pooledStrings.add(form.commonTags);
    if (form.best !== null && !surfaceRanks.has(form.best)) {
      pooledStrings.add(form.best);
      exceptionStrings.add(form.best);
    }
  }
  for (const restriction of restrictions) {
    if (!surfaceRanks.has(restriction.reading)) {
      pooledStrings.add(restriction.reading);
      exceptionStrings.add(restriction.reading);
    }
    if (!surfaceRanks.has(restriction.written)) {
      pooledStrings.add(restriction.written);
      exceptionStrings.add(restriction.written);
    }
  }

  const strings = canonicalStrings([...pooledStrings]);
  if (strings.length > 0xffff) {
    throw new RootPayloadEncodingError('String pool exceeds uint16 IDs used by forms and POS sets');
  }
  const stringIds = new Map(strings.map((value, index) => [value, index]));

  const entryPosIds = entries.map((entry) =>
    entry.pos.map((pos) => {
      const id = stringIds.get(pos);
      if (id === undefined) throw new RootPayloadEncodingError(`Missing POS string ${pos}`);
      return id;
    })
  );
  const uniquePosSets = new Map<string, number[]>([['', []]]);
  for (const posIds of entryPosIds) uniquePosSets.set(numberArrayKey(posIds), posIds);
  const posSets = [...uniquePosSets.values()].sort(compareNumberArrays);
  if (posSets.length > 0xffff) {
    throw new RootPayloadEncodingError('POS-set dictionary exceeds uint16 IDs');
  }
  const posSetIds = new Map(posSets.map((set, index) => [numberArrayKey(set), index]));
  const posMemberCount = posSets.reduce((sum, set) => sum + set.length, 0);

  const encodedStrings = strings.map((value) => UTF8_ENCODER.encode(value));
  const stringDataBytes = encodedStrings.reduce((sum, value) => sum + value.byteLength, 0);
  checkedUint32(stringDataBytes, 'String data size');

  const sectionBytes = {
    spans: checkedProduct(surfaceRanks.size, SPAN_BYTES, 'Span'),
    forms: checkedProduct(forms.length, FORM_BYTES, 'Form'),
    entries: checkedProduct(entries.length, ENTRY_BYTES, 'Entry'),
    restrictions: checkedProduct(restrictions.length, RESTRICTION_BYTES, 'Restriction'),
    posSets: checkedProduct(posSets.length, POS_SET_BYTES, 'POS set'),
    posMembers: checkedProduct(posMemberCount, 2, 'POS member'),
    stringOffsets: checkedProduct(strings.length + 1, 4, 'String offset'),
    stringData: stringDataBytes
  };

  let nextOffset = align(HEADER_BYTES);
  const spansOffset = nextOffset;
  nextOffset = align(nextOffset + sectionBytes.spans);
  const formsOffset = nextOffset;
  nextOffset = align(nextOffset + sectionBytes.forms);
  const entriesOffset = nextOffset;
  nextOffset = align(nextOffset + sectionBytes.entries);
  const restrictionsOffset = nextOffset;
  nextOffset = align(nextOffset + sectionBytes.restrictions);
  const posSetsOffset = nextOffset;
  nextOffset = align(nextOffset + sectionBytes.posSets);
  const posMembersOffset = nextOffset;
  nextOffset = align(nextOffset + sectionBytes.posMembers);
  const stringOffsetsOffset = nextOffset;
  nextOffset = align(nextOffset + sectionBytes.stringOffsets);
  const stringDataOffset = nextOffset;
  nextOffset = align(nextOffset + sectionBytes.stringData);
  const totalBytes = checkedUint32(nextOffset, 'Root payload size');

  const output = new Uint8Array(totalBytes);
  const view = new DataView(output.buffer);
  writeMagic(output);
  view.setUint16(8, FORMAT_VERSION, true);
  view.setUint16(10, HEADER_BYTES, true);
  view.setUint32(HEADER_FLAGS_OFFSET, 0, true);
  view.setUint32(HEADER_TOTAL_BYTES_OFFSET, totalBytes, true);
  view.setUint32(HEADER_CHECKSUM_OFFSET, 0, true);
  view.setUint32(PAYLOAD_CHECKSUM_OFFSET, 0, true);
  view.setUint32(HEADER_RESERVED_OFFSET, 0, true);

  view.setUint32(SURFACE_COUNT_OFFSET, surfaceRanks.size, true);
  view.setUint32(FORM_COUNT_OFFSET, forms.length, true);
  view.setUint32(ENTRY_COUNT_OFFSET, entries.length, true);
  view.setUint32(RESTRICTION_COUNT_OFFSET, restrictions.length, true);
  view.setUint32(STRING_COUNT_OFFSET, strings.length, true);
  view.setUint32(POS_SET_COUNT_OFFSET, posSets.length, true);
  view.setUint32(POS_MEMBER_COUNT_OFFSET, posMemberCount, true);

  view.setUint8(SPAN_STRIDE_OFFSET, SPAN_BYTES);
  view.setUint8(FORM_STRIDE_OFFSET, FORM_BYTES);
  view.setUint8(ENTRY_STRIDE_OFFSET, ENTRY_BYTES);
  view.setUint8(RESTRICTION_STRIDE_OFFSET, RESTRICTION_BYTES);

  view.setUint32(SPANS_OFFSET, spansOffset, true);
  view.setUint32(FORMS_OFFSET, formsOffset, true);
  view.setUint32(ENTRIES_OFFSET, entriesOffset, true);
  view.setUint32(RESTRICTIONS_OFFSET, restrictionsOffset, true);
  view.setUint32(POS_SETS_OFFSET, posSetsOffset, true);
  view.setUint32(POS_MEMBERS_OFFSET, posMembersOffset, true);
  view.setUint32(STRING_OFFSETS_OFFSET, stringOffsetsOffset, true);
  view.setUint32(STRING_DATA_OFFSET, stringDataOffset, true);
  view.setUint32(STRING_DATA_BYTES_OFFSET, stringDataBytes, true);

  previousSurface = undefined;
  let rank = -1;
  let spanFirst = 0;
  for (let formIndex = 0; formIndex < forms.length; formIndex++) {
    const form = forms[formIndex]!;
    if (form.surface !== previousSurface) {
      if (rank >= 0) {
        view.setUint8(spansOffset + rank * SPAN_BYTES + 3, formIndex - spanFirst);
      }
      rank++;
      spanFirst = formIndex;
      writeUint24(view, spansOffset + rank * SPAN_BYTES, formIndex, 'Surface first form');
      previousSurface = form.surface;
    }

    const formOffset = formsOffset + formIndex * FORM_BYTES;
    writeUint24(view, formOffset, entryIndex.get(form.seq)!, 'Form entry index');
    view.setUint32(formOffset + 3, surfaceReference(form.best, surfaceRanks, stringIds), true);
    view.setUint16(formOffset + 7, stringIds.get(form.commonTags)!, true);
    const common = form.common === null ? FORM_COMMON_NULL : form.common;
    view.setUint8(
      formOffset + 9,
      common
      | (form.route === 'kana' ? FORM_ROUTE_KANA_BIT : 0)
      | (form.conjugatable ? FORM_CONJUGATABLE_BIT : 0)
    );
    view.setUint8(formOffset + 10, form.ord | (form.nokanji ? FORM_NOKANJI_BIT : 0));
  }
  if (rank >= 0) {
    view.setUint8(spansOffset + rank * SPAN_BYTES + 3, forms.length - spanFirst);
  }

  for (let index = 0; index < entries.length; index++) {
    const entry = entries[index]!;
    const offset = entriesOffset + index * ENTRY_BYTES;
    const posSetId = posSetIds.get(numberArrayKey(entryPosIds[index]!));
    if (posSetId === undefined) throw new RootPayloadEncodingError('Missing canonical POS set');
    view.setUint32(offset, entry.seq, true);
    view.setUint16(offset + 4, posSetId, true);
    view.setUint8(offset + 6, entry.nKanji);
    view.setUint8(offset + 7, entry.nKana);
    view.setUint8(
      offset + 8,
      (entry.primaryNokanji ? ENTRY_PRIMARY_NOKANJI_BIT : 0)
      | (entry.archived ? ENTRY_ARCHIVED_BIT : 0)
      | (entry.preferKana ? ENTRY_PREFER_KANA_BIT : 0)
      | (entry.preferKanaOnOrdinalZero ? ENTRY_PREFER_KANA_ORDINAL_ZERO_BIT : 0)
    );
  }

  for (let index = 0; index < restrictions.length; index++) {
    const restriction = restrictions[index]!;
    const offset = restrictionsOffset + index * RESTRICTION_BYTES;
    view.setUint32(offset, entryIndex.get(restriction.seq)!, true);
    view.setUint32(
      offset + 4,
      surfaceReference(restriction.reading, surfaceRanks, stringIds),
      true
    );
    view.setUint32(
      offset + 8,
      surfaceReference(restriction.written, surfaceRanks, stringIds),
      true
    );
  }

  let memberIndex = 0;
  for (let setIndex = 0; setIndex < posSets.length; setIndex++) {
    const set = posSets[setIndex]!;
    const offset = posSetsOffset + setIndex * POS_SET_BYTES;
    view.setUint32(offset, memberIndex, true);
    view.setUint16(offset + 4, set.length, true);
    for (const stringId of set) {
      view.setUint16(posMembersOffset + memberIndex * 2, stringId, true);
      memberIndex++;
    }
  }

  let stringOffset = 0;
  for (let stringId = 0; stringId < encodedStrings.length; stringId++) {
    view.setUint32(stringOffsetsOffset + stringId * 4, stringOffset, true);
    const encoded = encodedStrings[stringId]!;
    output.set(encoded, stringDataOffset + stringOffset);
    stringOffset += encoded.byteLength;
  }
  view.setUint32(stringOffsetsOffset + strings.length * 4, stringOffset, true);

  view.setUint32(
    PAYLOAD_CHECKSUM_OFFSET,
    crc32(output.subarray(HEADER_BYTES)),
    true
  );
  view.setUint32(HEADER_CHECKSUM_OFFSET, crc32(output.subarray(0, HEADER_BYTES)), true);

  const usedBytes = HEADER_BYTES
    + sectionBytes.spans
    + sectionBytes.forms
    + sectionBytes.entries
    + sectionBytes.restrictions
    + sectionBytes.posSets
    + sectionBytes.posMembers
    + sectionBytes.stringOffsets
    + sectionBytes.stringData;

  return {
    bytes: output,
    stats: {
      directOrderProjection: {
        rows: directOrderRows,
        surfaces: directOrderSurfaces,
        sha256: directOrderProjection.digest('hex')
      },
      counts: {
        surfaces: surfaceRanks.size,
        forms: forms.length,
        entries: entries.length,
        restrictions: restrictions.length,
        strings: strings.length,
        posSets: posSets.length,
        posMembers: posMemberCount,
        pooledSurfaceExceptions: exceptionStrings.size
      },
      sectionBytes: {
        header: HEADER_BYTES,
        ...sectionBytes,
        alignmentPadding: totalBytes - usedBytes,
        total: totalBytes
      }
    }
  };
}

export function encodeRootPayload(source: RootPayloadSource): Uint8Array {
  return buildRootPayload(source).bytes;
}

export async function compileRootPayload(sql: postgres.Sql): Promise<RootPayloadBuild> {
  return buildRootPayload(await loadRootPayloadSource(sql));
}
