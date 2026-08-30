import { createHash } from 'node:crypto';
import { getConnection } from '@ichiran/reference-postgres';

type Sql = ReturnType<typeof getConnection>;
type Route = 'kana' | 'kanji';

interface LookupProperty {
  pos: string;
  type: number;
  negative: boolean | null;
  formal: boolean | null;
  ordinal: number;
}

interface LookupCandidate {
  rootSeq: number;
  sourceText: string;
  sourceForm: string;
  sourceReading: string;
  intermediate: string | null;
  path: readonly LookupProperty[];
  ord: number;
  common: number | null;
}

export interface MorphologyLookup {
  lookup(surface: string, route: Route): LookupCandidate[];
}

interface RelationRow {
  route: Route;
  surface: string;
  rootSeq: number;
  sourceText: string;
  sourceForm: string;
  sourceReading: string;
  intermediate: string | null;
  firstPos: string;
  firstType: number;
  firstNegative: boolean | null;
  firstFormal: boolean | null;
  secondPos: string | null;
  secondType: number | null;
  secondNegative: boolean | null;
  secondFormal: boolean | null;
  ord: number;
  common: number | null;
}

export interface MorphologyRelationDiff {
  route: Route;
  surface: string;
  side: 'legacy-only' | 'alpha-only';
  key: string;
}

export interface MorphologyVerificationResult {
  elapsedMs: number;
  diffRows: number;
  diffSha256: string;
  relationRows: number;
  surfaceGroups: number;
  exactSurfaceGroups: number;
  legacyRelationKeys: number;
  alphaRelationKeys: number;
  legacyOnly: number;
  alphaOnly: number;
  duplicateLegacyRows: number;
  duplicateAlphaCandidates: number;
  databaseArtifacts: {
    csrRows: number;
    installedRouteCsrRows: number;
    activeRouteCsrRows: number;
    inactiveRouteCsrRows: number;
    uninstalledCsrRows: number;
    dualRouteCsrRows: number;
    ghostSourceRows: number;
    ghostRootSurfacePairs: number;
    multiPropertyLinks: number;
    staleRawKanaSurfaces: number;
  };
  examples: MorphologyRelationDiff[];
}

export interface MorphologyRelationAttestation {
  rows: number;
  sha256: string;
  relationRows: number;
  surfaceGroups: number;
  exactSurfaceGroups: number;
  legacyRelationKeys: number;
  alphaRelationKeys: number;
  legacyOnly: number;
  alphaOnly: number;
  duplicateLegacyRows: number;
  duplicateAlphaCandidates: number;
  databaseArtifacts: MorphologyVerificationResult['databaseArtifacts'];
}

export function morphologyRelationAttestation(
  result: MorphologyVerificationResult
): MorphologyRelationAttestation {
  return {
    rows: result.diffRows,
    sha256: result.diffSha256,
    relationRows: result.relationRows,
    surfaceGroups: result.surfaceGroups,
    exactSurfaceGroups: result.exactSurfaceGroups,
    legacyRelationKeys: result.legacyRelationKeys,
    alphaRelationKeys: result.alphaRelationKeys,
    legacyOnly: result.legacyOnly,
    alphaOnly: result.alphaOnly,
    duplicateLegacyRows: result.duplicateLegacyRows,
    duplicateAlphaCandidates: result.duplicateAlphaCandidates,
    databaseArtifacts: result.databaseArtifacts
  };
}

export function canonicalMorphologyDiffLine(diff: MorphologyRelationDiff): string {
  return `${JSON.stringify({
    route: diff.route,
    surface: diff.surface,
    side: diff.side,
    key: diff.key
  })}\n`;
}

const RELATION_QUERY = String.raw`
  WITH relation AS (
    SELECT 'kana'::text AS route, target.text AS surface, c."from" AS "rootSeq",
           source.text AS "sourceText", COALESCE(source.best_kanji, source.text) AS "sourceForm",
           source.text AS "sourceReading", NULL::text AS intermediate,
           cp.pos AS "firstPos", cp.conj_type AS "firstType",
           cp.neg AS "firstNegative", cp.fml AS "firstFormal",
           NULL::text AS "secondPos", NULL::integer AS "secondType",
           NULL::boolean AS "secondNegative", NULL::boolean AS "secondFormal",
           source.ord, source.common
    FROM conjugation c
    JOIN conj_prop cp ON cp.conj_id = c.id
    JOIN conj_source_reading csr ON csr.conj_id = c.id
    JOIN kana_text target ON target.seq = c.seq AND target.text = csr.text
    JOIN kana_text source ON source.seq = c."from" AND source.text = csr.source_text
    WHERE c.via IS NULL
      AND target.text ~ '^[ァ-ヺヽヾーぁ-ゔゝゞー]+$'

    UNION ALL
    SELECT 'kanji'::text, target.text, c."from", source.text, source.text,
           COALESCE(source.best_kana, source.text), NULL::text,
           cp.pos, cp.conj_type, cp.neg, cp.fml,
           NULL::text, NULL::integer, NULL::boolean, NULL::boolean,
           source.ord, source.common
    FROM conjugation c
    JOIN conj_prop cp ON cp.conj_id = c.id
    JOIN conj_source_reading csr ON csr.conj_id = c.id
    JOIN kanji_text target ON target.seq = c.seq AND target.text = csr.text
    JOIN kanji_text source ON source.seq = c."from" AND source.text = csr.source_text
    WHERE c.via IS NULL
      AND target.text !~ '^[ァ-ヺヽヾーぁ-ゔゝゞー]+$'

    UNION ALL
    SELECT 'kana'::text, target.text, c2."from", source.text,
           COALESCE(source.best_kanji, source.text), source.text, csr2.source_text,
           cp1.pos, cp1.conj_type, cp1.neg, cp1.fml,
           cp2.pos, cp2.conj_type, cp2.neg, cp2.fml,
           source.ord, source.common
    FROM conjugation c2
    JOIN conj_prop cp2 ON cp2.conj_id = c2.id
    JOIN conj_source_reading csr2 ON csr2.conj_id = c2.id
    JOIN kana_text target ON target.seq = c2.seq AND target.text = csr2.text
    JOIN conjugation c1 ON c1.seq = c2.via AND c1."from" = c2."from" AND c1.via IS NULL
    JOIN conj_prop cp1 ON cp1.conj_id = c1.id
    JOIN conj_source_reading csr1 ON csr1.conj_id = c1.id AND csr1.text = csr2.source_text
    JOIN kana_text source ON source.seq = c1."from" AND source.text = csr1.source_text
    WHERE c2.via IS NOT NULL
      AND target.text ~ '^[ァ-ヺヽヾーぁ-ゔゝゞー]+$'

    UNION ALL
    SELECT 'kanji'::text, target.text, c2."from", source.text, source.text,
           COALESCE(source.best_kana, source.text), csr2.source_text,
           cp1.pos, cp1.conj_type, cp1.neg, cp1.fml,
           cp2.pos, cp2.conj_type, cp2.neg, cp2.fml,
           source.ord, source.common
    FROM conjugation c2
    JOIN conj_prop cp2 ON cp2.conj_id = c2.id
    JOIN conj_source_reading csr2 ON csr2.conj_id = c2.id
    JOIN kanji_text target ON target.seq = c2.seq AND target.text = csr2.text
    JOIN conjugation c1 ON c1.seq = c2.via AND c1."from" = c2."from" AND c1.via IS NULL
    JOIN conj_prop cp1 ON cp1.conj_id = c1.id
    JOIN conj_source_reading csr1 ON csr1.conj_id = c1.id AND csr1.text = csr2.source_text
    JOIN kanji_text source ON source.seq = c1."from" AND source.text = csr1.source_text
    WHERE c2.via IS NOT NULL
      AND target.text !~ '^[ァ-ヺヽヾーぁ-ゔゝゞー]+$'
  )
  SELECT * FROM relation
  ORDER BY route COLLATE "C", surface COLLATE "C", "rootSeq", "sourceText" COLLATE "C",
           "firstPos" COLLATE "C", "firstType", "firstNegative" NULLS FIRST,
           "firstFormal" NULLS FIRST, "secondPos" COLLATE "C" NULLS FIRST,
           "secondType" NULLS FIRST, "secondNegative" NULLS FIRST,
           "secondFormal" NULLS FIRST
`;

function propertyTuple(property: {
  pos: string;
  type: number;
  negative: boolean | null;
  formal: boolean | null;
}): readonly unknown[] {
  return [property.pos, property.type, property.negative, property.formal];
}

function rowKey(row: RelationRow): string {
  const path: unknown[] = [propertyTuple({
    pos: row.firstPos,
    type: row.firstType,
    negative: row.firstNegative,
    formal: row.firstFormal
  })];
  if (row.secondPos !== null && row.secondType !== null) {
    path.push(propertyTuple({
      pos: row.secondPos,
      type: row.secondType,
      negative: row.secondNegative,
      formal: row.secondFormal
    }));
  }
  return JSON.stringify([
    row.rootSeq,
    row.sourceText,
    row.sourceForm,
    row.sourceReading,
    row.intermediate,
    path,
    row.ord,
    row.common
  ]);
}

function candidateKey(candidate: LookupCandidate): string {
  return JSON.stringify([
    candidate.rootSeq,
    candidate.sourceText,
    candidate.sourceForm,
    candidate.sourceReading,
    candidate.intermediate,
    candidate.path.map(propertyTuple),
    candidate.ord,
    candidate.common
  ]);
}

async function artifactCounts(sql: Sql): Promise<MorphologyVerificationResult['databaseArtifacts']> {
  const [row] = await sql<Array<{
    ghostSourceRows: number;
    ghostRootSurfacePairs: number;
    multiPropertyLinks: number;
    staleRawKanaSurfaces: number;
    csrRows: number;
    installedRouteCsrRows: number;
    activeRouteCsrRows: number;
    inactiveRouteCsrRows: number;
    uninstalledCsrRows: number;
    dualRouteCsrRows: number;
  }>>`
    WITH installed_route AS MATERIALIZED (
      SELECT csr.id, (csr.text ~ '^[ァ-ヺヽヾーぁ-ゔゝゞー]+$') AS active
      FROM conjugation c
      JOIN conj_source_reading csr ON csr.conj_id = c.id
      JOIN kana_text target ON target.seq = c.seq AND target.text = csr.text
      UNION ALL
      SELECT csr.id, (csr.text !~ '^[ァ-ヺヽヾーぁ-ゔゝゞー]+$') AS active
      FROM conjugation c
      JOIN conj_source_reading csr ON csr.conj_id = c.id
      JOIN kanji_text target ON target.seq = c.seq AND target.text = csr.text
    ), route_counts AS (
      SELECT count(*)::int AS installed,
             count(*) FILTER (WHERE active)::int AS active,
             count(*) FILTER (WHERE NOT active)::int AS inactive,
             count(DISTINCT id)::int AS installed_csr
      FROM installed_route
    )
    SELECT
      (SELECT count(*)::int FROM conj_source_reading) AS csr_rows,
      route_counts.installed AS installed_route_csr_rows,
      route_counts.active AS active_route_csr_rows,
      route_counts.inactive AS inactive_route_csr_rows,
      ((SELECT count(*) FROM conj_source_reading) - route_counts.installed_csr)::int AS uninstalled_csr_rows,
      (route_counts.installed - route_counts.installed_csr)::int AS dual_route_csr_rows,
      (
        SELECT count(*)::int
        FROM conjugation c2
        JOIN conj_source_reading csr2 ON csr2.conj_id = c2.id
        JOIN conjugation c1
          ON c1."from" = c2."from" AND c1.seq = c2.via AND c1.via IS NULL
        WHERE c2.via IS NOT NULL
          AND NOT EXISTS (
            SELECT 1 FROM conj_source_reading csr1
            WHERE csr1.conj_id = c1.id AND csr1.text = csr2.source_text
          )
      ) AS ghost_source_rows,
      (
        SELECT count(*)::int FROM (
          SELECT DISTINCT c2."from", csr2.text
          FROM conjugation c2
          JOIN conj_source_reading csr2 ON csr2.conj_id = c2.id
          JOIN conjugation c1
            ON c1."from" = c2."from" AND c1.seq = c2.via AND c1.via IS NULL
          WHERE c2.via IS NOT NULL
            AND NOT EXISTS (
              SELECT 1 FROM conj_source_reading csr1
              WHERE csr1.conj_id = c1.id AND csr1.text = csr2.source_text
            )
        ) ghosts
      ) AS ghost_root_surface_pairs,
      (
        SELECT count(*)::int FROM (
          SELECT conj_id FROM conj_prop GROUP BY conj_id HAVING count(*) > 1
        ) links
      ) AS multi_property_links,
      (
        SELECT count(*)::int
        FROM kana_text target
        JOIN entry e ON e.seq = target.seq
        WHERE NOT e.root_p
          AND EXISTS (SELECT 1 FROM conjugation c WHERE c.seq = target.seq)
          AND NOT EXISTS (
            SELECT 1
            FROM conjugation c
            JOIN conj_source_reading csr ON csr.conj_id = c.id
            WHERE c.seq = target.seq AND csr.text = target.text
          )
      ) AS stale_raw_kana_surfaces
    FROM route_counts
  `;
  if (!row) throw new Error('Artifact-count query returned no row');
  return row;
}

/**
 * Stream the complete lineage-valid installed relation and compare it by
 * route/surface with the portable reverse matcher. Rule ordinal is not stored
 * in PostgreSQL and is therefore deliberately excluded from this table-level
 * key; it remains present in the portable candidate and output golden.
 */
export async function verifyMorphologyRelation(options: {
  lookup: MorphologyLookup;
  sql?: Sql;
  exampleLimit?: number;
  onDiff?: (diff: MorphologyRelationDiff, canonicalLine: string) => void;
  onProgress?: (surfaceGroups: number, relationRows: number) => void;
}): Promise<MorphologyVerificationResult> {
  const sql = options.sql ?? getConnection();
  const exampleLimit = options.exampleLimit ?? 100;
  const examples: MorphologyRelationDiff[] = [];
  const started = performance.now();

  let relationRows = 0;
  let surfaceGroups = 0;
  let exactSurfaceGroups = 0;
  let legacyRelationKeys = 0;
  let alphaRelationKeys = 0;
  let legacyOnly = 0;
  let alphaOnly = 0;
  let duplicateLegacyRows = 0;
  let duplicateAlphaCandidates = 0;
  let diffRows = 0;
  const diffDigest = createHash('sha256');
  let currentRoute: Route | null = null;
  let currentSurface = '';
  let legacyKeys = new Set<string>();

  const emit = (diff: MorphologyRelationDiff): void => {
    const line = canonicalMorphologyDiffLine(diff);
    diffRows++;
    diffDigest.update(line);
    options.onDiff?.(diff, line);
    if (examples.length < exampleLimit) examples.push(diff);
  };

  const finishGroup = (): void => {
    if (currentRoute === null) return;
    surfaceGroups++;
    const candidates = options.lookup.lookup(currentSurface, currentRoute);
    const alphaKeys = new Set(candidates.map(candidateKey));
    duplicateAlphaCandidates += candidates.length - alphaKeys.size;
    legacyRelationKeys += legacyKeys.size;
    alphaRelationKeys += alphaKeys.size;
    let exact = true;
    for (const key of [...legacyKeys].sort()) {
      if (!alphaKeys.has(key)) {
        exact = false;
        legacyOnly++;
        emit({ route: currentRoute, surface: currentSurface, side: 'legacy-only', key });
      }
    }
    for (const key of [...alphaKeys].sort()) {
      if (!legacyKeys.has(key)) {
        exact = false;
        alphaOnly++;
        emit({ route: currentRoute, surface: currentSurface, side: 'alpha-only', key });
      }
    }
    if (exact) exactSurfaceGroups++;
    if (surfaceGroups % 100_000 === 0) options.onProgress?.(surfaceGroups, relationRows);
  };

  const query = sql.unsafe<RelationRow[]>(RELATION_QUERY);
  await query.cursor(10_000, rows => {
    for (const row of rows) {
      relationRows++;
      if (currentRoute !== row.route || currentSurface !== row.surface) {
        finishGroup();
        currentRoute = row.route;
        currentSurface = row.surface;
        legacyKeys = new Set();
      }
      const key = rowKey(row);
      if (legacyKeys.has(key)) duplicateLegacyRows++;
      legacyKeys.add(key);
    }
  });
  finishGroup();

  return {
    elapsedMs: performance.now() - started,
    diffRows,
    diffSha256: diffDigest.digest('hex'),
    relationRows,
    surfaceGroups,
    exactSurfaceGroups,
    legacyRelationKeys,
    alphaRelationKeys,
    legacyOnly,
    alphaOnly,
    duplicateLegacyRows,
    duplicateAlphaCandidates,
    databaseArtifacts: await artifactCounts(sql),
    examples
  };
}
