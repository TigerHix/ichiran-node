import { createHash } from 'node:crypto';
import type postgres from 'postgres';
import type {
  AnalyzerSupportGeneratedMemberSource,
  AnalyzerSupportGeneratedRecordSource,
  AnalyzerSupportGeneratedSource
} from './analyzer-support.js';
import type { CompiledMorphologyArtifact } from './morphology-format.js';

interface GeneratedRow {
  rootSeq: number;
  targetSeq: number;
  firstAlias: number;
  secondAlias: number | null;
  nKanji: number;
  nKana: number;
  rootNKanji: number;
  rootNKana: number;
  finalConjugationId: number;
  viaTargetSeq: number | null;
  viaConjugationId: number | null;
  finalPropertyId: number;
  pos: string;
  type: number;
  negative: boolean | null;
  formal: boolean | null;
  defaultPos: string;
  defaultType: number;
  defaultNegative: boolean | null;
  defaultFormal: boolean | null;
  viaMembers: number;
  targetPaths: number;
  keyTargets: number;
  semanticPaths: number;
  matchedPaths: number;
  loadedPatches: number;
}

interface RuleProjection {
  alias: number;
  pos: string;
  type: number;
  negative: boolean | null;
  formal: boolean | null;
  stem: number;
  okuri: string;
  euphr: string;
  euphk: string;
}

interface PatchProjection {
  route: 'kana' | 'kanji';
  root_seq: number;
  surface: string;
  first_alias: number;
  second_alias: number | null;
}

/** @internal Exported only for deterministic compiler projection tests. */
export interface LookupOrderRow {
  rootSeq: number;
  firstAlias: number | null;
  secondAlias: number | null;
  route: 'kana' | 'kanji';
  surface: string;
  rank: number;
  physicalClasses: number;
  locatedClasses: number;
  ambiguousSurfaces: number;
  loadedPatches: number;
}

interface LookupOrderLocator {
  readonly rootSeq: number;
  readonly firstAlias: number | null;
  readonly secondAlias: number | null;
}

class LookupOrderEquivalence {
  readonly #parents = new Map<string, string>();
  readonly #sizes = new Map<string, number>();

  add(value: string): void {
    if (this.#parents.has(value)) return;
    this.#parents.set(value, value);
    this.#sizes.set(value, 1);
  }

  find(value: string): string {
    const parent = this.#parents.get(value);
    if (parent === undefined) throw new Error(`Unknown lookup-order locator ${value}`);
    if (parent === value) return value;
    const root = this.find(parent);
    this.#parents.set(value, root);
    return root;
  }

  union(left: string, right: string): void {
    let leftRoot = this.find(left);
    let rightRoot = this.find(right);
    if (leftRoot === rightRoot) return;
    const leftSize = this.#sizes.get(leftRoot)!;
    const rightSize = this.#sizes.get(rightRoot)!;
    // Size keeps paths shallow; the semantic key makes an equal-size union
    // independent of SQL/input iteration order.
    if (leftSize < rightSize || (leftSize === rightSize && leftRoot > rightRoot)) {
      [leftRoot, rightRoot] = [rightRoot, leftRoot];
    }
    this.#parents.set(rightRoot, leftRoot);
    this.#sizes.set(leftRoot, this.#sizes.get(leftRoot)! + this.#sizes.get(rightRoot)!);
  }

  values(): IterableIterator<string> {
    return this.#parents.keys();
  }
}

/* Retain the matching conj_prop row until `matched_paths` so the raw-versus-
 * semantic projection delta is explicit. Then attach every property of each
 * physical conjugation, matching getConjData rather than only the rule match. */
const GENERATED_QUERY = String.raw`
  WITH rules AS MATERIALIZED (
    SELECT * FROM jsonb_to_recordset($1::jsonb) AS r(
      alias integer, pos text, type integer, negative boolean, formal boolean,
      stem integer, okuri text, euphr text, euphk text
    )
  ), aliases AS MATERIALIZED (
    SELECT DISTINCT alias, pos, type, negative, formal FROM rules
  ), patches AS MATERIALIZED (
    SELECT * FROM jsonb_to_recordset($2::jsonb) AS p(
      route text, root_seq integer, surface text,
      first_alias integer, second_alias integer
    )
  ), direct_paths AS (
    SELECT c."from" AS root_seq, c.seq AS target_seq,
      r.alias AS first_alias, NULL::integer AS second_alias,
      target.n_kanji, target.n_kana,
      root.n_kanji AS root_n_kanji, root.n_kana AS root_n_kana,
      c.id AS final_conjugation_id,
      NULL::integer AS via_target_seq, NULL::integer AS via_conjugation_id,
      cp.id AS matched_property_id,
      r.pos AS default_pos, r.type AS default_type,
      r.negative AS default_negative, r.formal AS default_formal
    FROM conjugation c
    JOIN entry target ON target.seq = c.seq AND NOT target.root_p
    JOIN entry root ON root.seq = c."from" AND root.root_p
    JOIN conj_prop cp ON cp.conj_id = c.id
    JOIN conj_source_reading csr ON csr.conj_id = c.id
    JOIN rules r ON r.pos = cp.pos AND r.type = cp.conj_type
      AND (r.negative IS NULL OR r.negative = cp.neg)
      AND (r.formal IS NULL OR r.formal = cp.fml)
    CROSS JOIN LATERAL (
      SELECT CASE WHEN right(csr.source_text, 2) ~ '^[ァ-ヺヽヾーぁ-ゔゝゞー]+$'
        THEN r.euphr ELSE r.euphk END AS euphony
    ) selected
    WHERE c.via IS NULL
      AND left(csr.source_text, char_length(csr.source_text) - r.stem
        - CASE WHEN selected.euphony <> '' THEN 1 ELSE 0 END)
        || selected.euphony || r.okuri = csr.text
  ), secondary_paths AS (
    SELECT c."from" AS root_seq, c.seq AS target_seq,
      r1.alias AS first_alias, r2.alias AS second_alias,
      target.n_kanji, target.n_kana,
      root.n_kanji AS root_n_kanji, root.n_kana AS root_n_kana,
      c.id AS final_conjugation_id,
      c.via AS via_target_seq, c1.id AS via_conjugation_id,
      cp2.id AS matched_property_id,
      r2.pos AS default_pos, r2.type AS default_type,
      r2.negative AS default_negative, r2.formal AS default_formal
    FROM conjugation c
    JOIN entry target ON target.seq = c.seq AND NOT target.root_p
    JOIN entry root ON root.seq = c."from" AND root.root_p
    JOIN conj_prop cp2 ON cp2.conj_id = c.id
    JOIN conj_source_reading csr2 ON csr2.conj_id = c.id
    JOIN conjugation c1 ON c1.seq = c.via AND c1."from" = c."from"
    JOIN conj_prop cp1 ON cp1.conj_id = c1.id
    JOIN conj_source_reading csr1 ON csr1.conj_id = c1.id AND csr1.text = csr2.source_text
    JOIN rules r1 ON r1.pos = cp1.pos AND r1.type = cp1.conj_type
      AND (r1.negative IS NULL OR r1.negative = cp1.neg)
      AND (r1.formal IS NULL OR r1.formal = cp1.fml)
    JOIN rules r2 ON r2.pos = cp2.pos AND r2.type = cp2.conj_type
      AND (r2.negative IS NULL OR r2.negative = cp2.neg)
      AND (r2.formal IS NULL OR r2.formal = cp2.fml)
    CROSS JOIN LATERAL (
      SELECT CASE WHEN right(csr1.source_text, 2) ~ '^[ァ-ヺヽヾーぁ-ゔゝゞー]+$'
        THEN r1.euphr ELSE r1.euphk END AS euphony
    ) first_selected
    CROSS JOIN LATERAL (
      SELECT CASE WHEN right(csr2.source_text, 2) ~ '^[ァ-ヺヽヾーぁ-ゔゝゞー]+$'
        THEN r2.euphr ELSE r2.euphk END AS euphony
    ) second_selected
    WHERE c.via IS NOT NULL
      AND left(csr1.source_text, char_length(csr1.source_text) - r1.stem
        - CASE WHEN first_selected.euphony <> '' THEN 1 ELSE 0 END)
        || first_selected.euphony || r1.okuri = csr1.text
      AND left(csr2.source_text, char_length(csr2.source_text) - r2.stem
        - CASE WHEN second_selected.euphony <> '' THEN 1 ELSE 0 END)
        || second_selected.euphony || r2.okuri = csr2.text
  ), direct_patch_paths AS (
    SELECT c."from" AS root_seq, c.seq AS target_seq,
      p.first_alias, NULL::integer AS second_alias,
      target.n_kanji, target.n_kana,
      root.n_kanji AS root_n_kanji, root.n_kana AS root_n_kana,
      c.id AS final_conjugation_id,
      NULL::integer AS via_target_seq, NULL::integer AS via_conjugation_id,
      0::integer AS matched_property_id,
      a.pos AS default_pos, a.type AS default_type,
      a.negative AS default_negative, a.formal AS default_formal
    FROM patches p
    JOIN aliases a ON a.alias = p.first_alias
    JOIN conjugation c ON c."from" = p.root_seq AND c.via IS NULL
    JOIN conj_source_reading csr ON csr.conj_id = c.id AND csr.text = p.surface
    JOIN entry target ON target.seq = c.seq AND NOT target.root_p
    JOIN entry root ON root.seq = c."from" AND root.root_p
    WHERE p.second_alias IS NULL
  ), secondary_patch_paths AS (
    SELECT c."from" AS root_seq, c.seq AS target_seq,
      p.first_alias, p.second_alias,
      target.n_kanji, target.n_kana,
      root.n_kanji AS root_n_kanji, root.n_kana AS root_n_kana,
      c.id AS final_conjugation_id,
      c.via AS via_target_seq, c1.id AS via_conjugation_id,
      0::integer AS matched_property_id,
      a.pos AS default_pos, a.type AS default_type,
      a.negative AS default_negative, a.formal AS default_formal
    FROM patches p
    JOIN aliases a ON a.alias = p.second_alias
    JOIN conjugation c ON c."from" = p.root_seq AND c.via IS NOT NULL
    JOIN conj_source_reading csr2 ON csr2.conj_id = c.id AND csr2.text = p.surface
    JOIN conjugation c1 ON c1.seq = c.via AND c1."from" = c."from"
    JOIN conj_source_reading csr1 ON csr1.conj_id = c1.id AND csr1.text = csr2.source_text
    JOIN entry target ON target.seq = c.seq AND NOT target.root_p
    JOIN entry root ON root.seq = c."from" AND root.root_p
    WHERE p.second_alias IS NOT NULL
  ), matched_paths AS MATERIALIZED (
    SELECT DISTINCT * FROM (
      SELECT * FROM direct_paths
      UNION ALL SELECT * FROM secondary_paths
      UNION ALL SELECT * FROM direct_patch_paths
      UNION ALL SELECT * FROM secondary_patch_paths
    ) paths
  ), matched_members AS MATERIALIZED (
    SELECT DISTINCT root_seq, target_seq, first_alias, second_alias,
      n_kanji, n_kana, root_n_kanji, root_n_kana,
      final_conjugation_id, via_target_seq, via_conjugation_id,
      default_pos, default_type, default_negative, default_formal
    FROM matched_paths
  ), semantic_paths AS MATERIALIZED (
    SELECT DISTINCT root_seq, target_seq, first_alias, second_alias,
      n_kanji, n_kana, root_n_kanji, root_n_kana,
      default_pos, default_type, default_negative, default_formal
    FROM matched_members
  ), measured AS MATERIALIZED (
    SELECT *,
      count(*) OVER (PARTITION BY target_seq)::integer AS target_paths,
      count(*) OVER (PARTITION BY root_seq, first_alias, second_alias)::integer AS key_targets
    FROM semantic_paths
  ), target_members AS MATERIALIZED (
    SELECT target_seq, count(DISTINCT final_conjugation_id)::integer AS members
    FROM matched_members GROUP BY target_seq
  ), member_summary AS MATERIALIZED (
    SELECT m.root_seq, m.target_seq, m.first_alias, m.second_alias,
      count(DISTINCT (m.final_conjugation_id, m.via_conjugation_id))::integer AS member_paths,
      count(DISTINCT (m.final_conjugation_id, actual.id))::integer AS property_rows,
      bool_or(actual.pos IS DISTINCT FROM m.default_pos
        OR actual.conj_type IS DISTINCT FROM m.default_type
        OR actual.neg IS DISTINCT FROM m.default_negative
        OR actual.fml IS DISTINCT FROM m.default_formal) AS property_override,
      max(COALESCE(via.members, 0))::integer AS via_members
    FROM matched_members m
    JOIN conj_prop actual ON actual.conj_id = m.final_conjugation_id
    LEFT JOIN target_members via ON via.target_seq = m.via_target_seq
    GROUP BY m.root_seq, m.target_seq, m.first_alias, m.second_alias
  ), selected AS MATERIALIZED (
    SELECT measured.*, summary.via_members FROM measured
    JOIN member_summary summary
      ON summary.root_seq = measured.root_seq
      AND summary.target_seq = measured.target_seq
      AND summary.first_alias = measured.first_alias
      AND COALESCE(summary.second_alias, -1) = COALESCE(measured.second_alias, -1)
  ), totals AS (
    SELECT (SELECT count(*)::integer FROM semantic_paths) AS semantic_paths,
      (SELECT count(*)::integer FROM matched_paths) AS matched_paths,
      (SELECT count(*)::integer FROM patches
       WHERE route IS NOT NULL AND root_seq IS NOT NULL
         AND surface IS NOT NULL AND first_alias IS NOT NULL) AS loaded_patches
  )
  SELECT DISTINCT selected.*,
    member.final_conjugation_id, member.via_target_seq, member.via_conjugation_id,
    actual.id AS final_property_id,
    actual.pos, actual.conj_type AS type, actual.neg AS negative, actual.fml AS formal,
    totals.semantic_paths, totals.matched_paths, totals.loaded_patches
  FROM selected
  JOIN matched_members member
    ON member.root_seq = selected.root_seq
    AND member.target_seq = selected.target_seq
    AND member.first_alias = selected.first_alias
    AND COALESCE(member.second_alias, -1) = COALESCE(selected.second_alias, -1)
  JOIN conj_prop actual ON actual.conj_id = member.final_conjugation_id
  CROSS JOIN totals
  ORDER BY selected.root_seq, selected.first_alias, selected.second_alias NULLS FIRST,
    member.final_conjugation_id, actual.id, member.via_conjugation_id NULLS FIRST,
    selected.target_seq
`;

/*
 * Heap tuple positions are used only inside this query to observe the pinned
 * legacy bulk lookup followed by `unshift`. The result is normalized to
 * semantic locators plus a dense global rank; no CTID, target seq, or text-row
 * surrogate crosses the compiler boundary or enters a projection digest.
 */
const LOOKUP_ORDER_QUERY = String.raw`
  WITH rules AS MATERIALIZED (
    SELECT * FROM jsonb_to_recordset($1::jsonb) AS r(
      alias integer, pos text, type integer, negative boolean, formal boolean,
      stem integer, okuri text, euphr text, euphk text
    )
  ), aliases AS MATERIALIZED (
    SELECT DISTINCT alias, pos, type, negative, formal FROM rules
  ), patches AS MATERIALIZED (
    SELECT * FROM jsonb_to_recordset($2::jsonb) AS p(
      route text, root_seq integer, surface text,
      first_alias integer, second_alias integer
    )
  ), direct_paths AS (
    SELECT DISTINCT c."from" AS root_seq, c.seq AS target_seq,
      r.alias AS first_alias, NULL::integer AS second_alias,
      CASE WHEN csr.source_text ~ '^[ァ-ヺヽヾーぁ-ゔゝゞー]+$'
        THEN 'kana'::text ELSE 'kanji'::text END AS route,
      csr.text AS surface
    FROM conjugation c
    JOIN entry target ON target.seq = c.seq
    JOIN conj_prop cp ON cp.conj_id = c.id
    JOIN conj_source_reading csr ON csr.conj_id = c.id
    JOIN rules r ON r.pos = cp.pos AND r.type = cp.conj_type
      AND (r.negative IS NULL OR r.negative = cp.neg)
      AND (r.formal IS NULL OR r.formal = cp.fml)
    CROSS JOIN LATERAL (
      SELECT CASE WHEN right(csr.source_text, 2) ~ '^[ァ-ヺヽヾーぁ-ゔゝゞー]+$'
        THEN r.euphr ELSE r.euphk END AS euphony
    ) selected
    WHERE c.via IS NULL
      AND left(csr.source_text, char_length(csr.source_text) - r.stem
        - CASE WHEN selected.euphony <> '' THEN 1 ELSE 0 END)
        || selected.euphony || r.okuri = csr.text
  ), secondary_paths AS (
    SELECT DISTINCT c."from" AS root_seq, c.seq AS target_seq,
      r1.alias AS first_alias, r2.alias AS second_alias,
      CASE WHEN csr1.source_text ~ '^[ァ-ヺヽヾーぁ-ゔゝゞー]+$'
        THEN 'kana'::text ELSE 'kanji'::text END AS route,
      csr2.text AS surface
    FROM conjugation c
    JOIN entry target ON target.seq = c.seq
    JOIN conj_prop cp2 ON cp2.conj_id = c.id
    JOIN conj_source_reading csr2 ON csr2.conj_id = c.id
    JOIN conjugation c1 ON c1.seq = c.via AND c1."from" = c."from"
    JOIN conj_prop cp1 ON cp1.conj_id = c1.id
    JOIN conj_source_reading csr1
      ON csr1.conj_id = c1.id AND csr1.text = csr2.source_text
    JOIN rules r1 ON r1.pos = cp1.pos AND r1.type = cp1.conj_type
      AND (r1.negative IS NULL OR r1.negative = cp1.neg)
      AND (r1.formal IS NULL OR r1.formal = cp1.fml)
    JOIN rules r2 ON r2.pos = cp2.pos AND r2.type = cp2.conj_type
      AND (r2.negative IS NULL OR r2.negative = cp2.neg)
      AND (r2.formal IS NULL OR r2.formal = cp2.fml)
    CROSS JOIN LATERAL (
      SELECT CASE WHEN right(csr1.source_text, 2) ~ '^[ァ-ヺヽヾーぁ-ゔゝゞー]+$'
        THEN r1.euphr ELSE r1.euphk END AS euphony
    ) first_selected
    CROSS JOIN LATERAL (
      SELECT CASE WHEN right(csr2.source_text, 2) ~ '^[ァ-ヺヽヾーぁ-ゔゝゞー]+$'
        THEN r2.euphr ELSE r2.euphk END AS euphony
    ) second_selected
    WHERE c.via IS NOT NULL
      AND left(csr1.source_text, char_length(csr1.source_text) - r1.stem
        - CASE WHEN first_selected.euphony <> '' THEN 1 ELSE 0 END)
        || first_selected.euphony || r1.okuri = csr1.text
      AND left(csr2.source_text, char_length(csr2.source_text) - r2.stem
        - CASE WHEN second_selected.euphony <> '' THEN 1 ELSE 0 END)
        || second_selected.euphony || r2.okuri = csr2.text
  ), direct_patch_paths AS (
    SELECT DISTINCT c."from" AS root_seq, c.seq AS target_seq,
      p.first_alias, NULL::integer AS second_alias, p.route, p.surface
    FROM patches p
    JOIN conjugation c ON c."from" = p.root_seq AND c.via IS NULL
    JOIN conj_source_reading csr ON csr.conj_id = c.id AND csr.text = p.surface
    JOIN entry target ON target.seq = c.seq
    WHERE p.second_alias IS NULL
  ), secondary_patch_paths AS (
    SELECT DISTINCT c."from" AS root_seq, c.seq AS target_seq,
      p.first_alias, p.second_alias, p.route, p.surface
    FROM patches p
    JOIN conjugation c ON c."from" = p.root_seq AND c.via IS NOT NULL
    JOIN conj_source_reading csr ON csr.conj_id = c.id AND csr.text = p.surface
    JOIN entry target ON target.seq = c.seq
    WHERE p.second_alias IS NOT NULL
  ), all_morphology_paths AS MATERIALIZED (
    SELECT * FROM direct_paths
    UNION SELECT * FROM secondary_paths
    UNION SELECT * FROM direct_patch_paths
    UNION SELECT * FROM secondary_patch_paths
  ), generated_paths AS MATERIALIZED (
    SELECT p.*
    FROM all_morphology_paths p
    JOIN entry target ON target.seq = p.target_seq
    WHERE NOT target.root_p
  ), generated_locators AS MATERIALIZED (
    SELECT p.*, max(t.ctid) AS lookup_tuple
    FROM generated_paths p
    JOIN kana_text t ON p.route = 'kana' AND t.seq = p.target_seq
      AND t.text = p.surface
      AND t.text ~ '^[ァ-ヺヽヾーぁ-ゔゝゞー]+$'
    GROUP BY p.root_seq, p.target_seq, p.first_alias, p.second_alias,
      p.route, p.surface
    UNION ALL
    SELECT p.*, max(t.ctid) AS lookup_tuple
    FROM generated_paths p
    JOIN kanji_text t ON p.route = 'kanji' AND t.seq = p.target_seq
      AND t.text = p.surface
      AND t.text !~ '^[ァ-ヺヽヾーぁ-ゔゝゞー]+$'
    GROUP BY p.root_seq, p.target_seq, p.first_alias, p.second_alias,
      p.route, p.surface
  ), direct_locators AS MATERIALIZED (
    SELECT t.seq AS root_seq, t.seq AS target_seq,
      NULL::integer AS first_alias, NULL::integer AS second_alias,
      'kana'::text AS route, t.text AS surface, max(t.ctid) AS lookup_tuple
    FROM kana_text t JOIN entry e USING (seq)
    WHERE e.root_p AND t.text ~ '^[ァ-ヺヽヾーぁ-ゔゝゞー]+$'
    GROUP BY t.seq, t.text
    UNION ALL
    SELECT t.seq AS root_seq, t.seq AS target_seq,
      NULL::integer AS first_alias, NULL::integer AS second_alias,
      'kanji'::text AS route, t.text AS surface, max(t.ctid) AS lookup_tuple
    FROM kanji_text t JOIN entry e USING (seq)
    WHERE e.root_p AND t.text !~ '^[ァ-ヺヽヾーぁ-ゔゝゞー]+$'
    GROUP BY t.seq, t.text
  ), locators AS MATERIALIZED (
    SELECT * FROM generated_locators
    UNION ALL SELECT * FROM direct_locators
  ), active_routes AS MATERIALIZED (
    SELECT DISTINCT route, surface FROM all_morphology_paths
    UNION SELECT DISTINCT route, surface FROM direct_locators
  ), physical_targets AS MATERIALIZED (
    SELECT 'kana'::text AS route, t.text AS surface, t.seq AS target_seq,
      max(t.ctid) AS lookup_tuple
    FROM kana_text t
    JOIN active_routes a ON a.route = 'kana' AND a.surface = t.text
    WHERE t.text ~ '^[ァ-ヺヽヾーぁ-ゔゝゞー]+$'
    GROUP BY t.text, t.seq
    UNION ALL
    SELECT 'kanji'::text AS route, t.text AS surface, t.seq AS target_seq,
      max(t.ctid) AS lookup_tuple
    FROM kanji_text t
    JOIN active_routes a ON a.route = 'kanji' AND a.surface = t.text
    WHERE t.text !~ '^[ァ-ヺヽヾーぁ-ゔゝゞー]+$'
    GROUP BY t.text, t.seq
  ), ambiguous AS MATERIALIZED (
    SELECT p.route, p.surface
    FROM physical_targets p
    GROUP BY p.route, p.surface
    HAVING count(*) > 1
      AND EXISTS (
        SELECT 1 FROM all_morphology_paths g
        WHERE g.route = p.route AND g.surface = p.surface
      )
  ), ranked AS MATERIALIZED (
    SELECT p.*, (row_number() OVER (
      /* Within one route each target class owns disjoint physical text rows,
       * so its max CTID is unique. CTID is discarded here; only the resulting
       * dense semantic order enters the source projection and release lock. */
      PARTITION BY route, surface ORDER BY lookup_tuple DESC
    ) - 1)::integer AS rank
    FROM physical_targets p JOIN ambiguous USING (route, surface)
  ), coverage AS (
    SELECT
      (SELECT count(*)::integer FROM ranked) AS physical_classes,
      (SELECT count(DISTINCT (r.route, r.surface, r.target_seq))::integer
       FROM ranked r JOIN locators l USING (route, surface, target_seq)) AS located_classes,
      (SELECT count(*)::integer FROM ambiguous) AS ambiguous_surfaces,
      (SELECT count(*)::integer FROM patches
       WHERE route IS NOT NULL AND root_seq IS NOT NULL
         AND surface IS NOT NULL AND first_alias IS NOT NULL) AS loaded_patches
  )
  SELECT * FROM (
    SELECT DISTINCT l.root_seq, l.first_alias, l.second_alias,
      l.route, l.surface, r.rank,
      coverage.physical_classes, coverage.located_classes,
      coverage.ambiguous_surfaces, coverage.loaded_patches
    FROM locators l
    JOIN ranked r USING (route, surface, target_seq)
    CROSS JOIN coverage
  ) projected
  ORDER BY root_seq, first_alias NULLS FIRST, second_alias NULLS FIRST,
    route COLLATE "C", surface COLLATE "C", rank
`;

function compareText(left: string, right: string): number {
  return left < right ? -1 : left > right ? 1 : 0;
}

function semanticRuleKey(rule: CompiledMorphologyArtifact['rules'][number]): string {
  return JSON.stringify([rule.pos, rule.type, rule.negative, rule.formal]);
}

function recordKey(value: Pick<GeneratedRow, 'rootSeq' | 'firstAlias' | 'secondAlias'>): string {
  return `${value.rootSeq.toString().padStart(10, '0')}\u0000${value.firstAlias
    .toString().padStart(5, '0')}\u0000${(value.secondAlias ?? -1).toString().padStart(5, '0')}`;
}

function sameProperty(
  left: { posId: number; type: number; negative: boolean | null; formal: boolean | null },
  right: { posId: number; type: number; negative: boolean | null; formal: boolean | null }
): boolean {
  return left.posId === right.posId && left.type === right.type
    && left.negative === right.negative && left.formal === right.formal;
}

function compareLookupOrderLocator(
  left: LookupOrderLocator,
  right: LookupOrderLocator
): number {
  return left.rootSeq - right.rootSeq
    || (left.firstAlias ?? -1) - (right.firstAlias ?? -1)
    || (left.secondAlias ?? -1) - (right.secondAlias ?? -1);
}

function lookupOrderLocatorKey(value: LookupOrderLocator): string {
  return `${value.rootSeq}\u0000${value.firstAlias ?? -1}\u0000${value.secondAlias ?? -1}`;
}

/** @internal Exported only for deterministic compiler projection tests. */
export function compileLookupOrders(
  rows: readonly LookupOrderRow[],
  aliasCount: number,
  expectedPatchCount: number
): {
  readonly values: AnalyzerSupportGeneratedSource['lookupOrders'];
  readonly sourceRows: number;
  readonly sourceSha256: string;
  readonly surfaces: number;
  readonly physicalClasses: number;
  readonly equivalenceClasses: number;
  readonly components: number;
  readonly cyclicComponents: number;
  readonly edges: number;
  readonly maxRank: number;
  readonly sha256: string;
  readonly exceptions: AnalyzerSupportGeneratedSource['lookupOrderExceptions'];
  readonly exceptionClasses: number;
  readonly exceptionLocators: number;
} {
  if (rows.length === 0 || rows.some(row => row.loadedPatches !== expectedPatchCount)) {
    throw new Error(
      `Generated SQL loaded an incomplete manual-patch projection; expected ${expectedPatchCount}`
    );
  }

  const physicalClasses = rows[0]!.physicalClasses;
  const locatedClasses = rows[0]!.locatedClasses;
  const ambiguousSurfaces = rows[0]!.ambiguousSurfaces;
  if (physicalClasses !== locatedClasses || rows.some(value =>
    value.physicalClasses !== physicalClasses
    || value.locatedClasses !== locatedClasses
    || value.ambiguousSurfaces !== ambiguousSurfaces)) {
    throw new Error(
      `Lookup-order physical coverage failed: ${locatedClasses}/${physicalClasses} classes`
    );
  }

  const locatorByKey = new Map<string, LookupOrderLocator>();
  const surfaces = new Map<string, Map<number, Set<string>>>();
  const surfaceLocators = new Map<string, Map<string, number>>();
  for (const value of rows) {
    if (!Number.isSafeInteger(value.rootSeq) || value.rootSeq <= 0
      || !Number.isSafeInteger(value.rank) || value.rank < 0
      || (value.firstAlias === null && value.secondAlias !== null)
      || (value.route !== 'kana' && value.route !== 'kanji')
      || value.surface.length === 0
      || (value.firstAlias !== null && (value.firstAlias < 0 || value.firstAlias >= aliasCount))
      || (value.secondAlias !== null && (value.secondAlias < 0 || value.secondAlias >= aliasCount))) {
      throw new Error(`Invalid semantic lookup-order locator ${JSON.stringify(value)}`);
    }
    const locator: LookupOrderLocator = {
      rootSeq: value.rootSeq,
      firstAlias: value.firstAlias,
      secondAlias: value.secondAlias
    };
    const locatorKey = lookupOrderLocatorKey(locator);
    locatorByKey.set(locatorKey, locator);
    const surfaceKey = `${value.route}\u0000${value.surface}`;
    const priorRanks = surfaceLocators.get(surfaceKey) ?? new Map<string, number>();
    const priorRank = priorRanks.get(locatorKey);
    if (priorRank !== undefined && priorRank !== value.rank) {
      throw new Error(
        `Lookup-order locator maps to physical classes ${priorRank} and ${value.rank} on ${JSON.stringify(surfaceKey)}`
      );
    }
    priorRanks.set(locatorKey, value.rank);
    surfaceLocators.set(surfaceKey, priorRanks);
    const classes = surfaces.get(surfaceKey) ?? new Map<number, Set<string>>();
    const locators = classes.get(value.rank) ?? new Set<string>();
    locators.add(locatorKey);
    classes.set(value.rank, locators);
    surfaces.set(surfaceKey, classes);
  }

  let coveredClasses = 0;
  const equivalence = new LookupOrderEquivalence();
  for (const [surface, classes] of surfaces) {
    const ranks = [...classes.keys()].sort((left, right) => left - right);
    if (ranks.length < 2 || ranks.some((rank, index) => rank !== index)) {
      throw new Error(`Lookup-order ranks are not dense for ${JSON.stringify(surface)}`);
    }
    coveredClasses += ranks.length;
    for (const rank of ranks) {
      const locators = [...classes.get(rank)!];
      if (locators.length === 0) {
        throw new Error(`Lookup-order physical class ${rank} is empty for ${JSON.stringify(surface)}`);
      }
      for (const locator of locators) equivalence.add(locator);
      for (let index = 1; index < locators.length; index++) {
        equivalence.union(locators[0]!, locators[index]!);
      }
    }
  }
  if (surfaces.size !== ambiguousSurfaces || coveredClasses !== physicalClasses) {
    throw new Error(
      `Lookup-order coverage disagrees: ${surfaces.size}/${ambiguousSurfaces} surfaces, `
        + `${coveredClasses}/${physicalClasses} classes`
    );
  }
  const sourceProjection = createHash('sha256');
  let sourceProjectionRows = 0;
  for (const [surfaceKey, classes] of [...surfaces].sort((left, right) =>
    compareText(left[0], right[0]))) {
    const delimiter = surfaceKey.indexOf('\u0000');
    const route = surfaceKey.slice(0, delimiter);
    const surface = surfaceKey.slice(delimiter + 1);
    for (const [rank, locators] of [...classes].sort((left, right) => left[0] - right[0])) {
      for (const locatorKey of [...locators].sort((left, right) =>
        compareLookupOrderLocator(locatorByKey.get(left)!, locatorByKey.get(right)!))) {
        const locator = locatorByKey.get(locatorKey)!;
        sourceProjection.update(JSON.stringify([
          route, surface, rank, locator.rootSeq,
          locator.firstAlias ?? -1, locator.secondAlias ?? -1
        ]) + '\n');
        sourceProjectionRows++;
      }
    }
  }
  if (sourceProjectionRows !== rows.length) {
    throw new Error(
      `Lookup-order semantic source contains ${rows.length - sourceProjectionRows} duplicate row(s)`
    );
  }

  const membersByRoot = new Map<string, string[]>();
  for (const locator of equivalence.values()) {
    const root = equivalence.find(locator);
    const members = membersByRoot.get(root) ?? [];
    members.push(locator);
    membersByRoot.set(root, members);
  }
  const quotient = [...membersByRoot.entries()].map(([root, members]) => {
    members.sort((left, right) => compareLookupOrderLocator(
      locatorByKey.get(left)!, locatorByKey.get(right)!
    ));
    return { root, members, canonical: locatorByKey.get(members[0]!)! };
  }).sort((left, right) => compareLookupOrderLocator(left.canonical, right.canonical));
  const nodeByRoot = new Map(quotient.map((value, index) => [value.root, index]));
  const nodeForLocator = (locator: string): number => nodeByRoot.get(equivalence.find(locator))!;
  const adjacencySets = Array.from({ length: quotient.length }, () => new Set<number>());
  let duplicateSurfaceClasses = 0;
  let selfEdges = 0;
  const orderedSurfaceNodes = new Map<string, number[]>();
  for (const [surface, classes] of [...surfaces].sort((left, right) =>
    compareText(left[0], right[0]))) {
    const nodes = [...classes.entries()]
      .sort((left, right) => left[0] - right[0])
      .map(([, locators]) => {
        const values = [...locators].map(nodeForLocator);
        if (new Set(values).size !== 1) {
          throw new Error(`Lookup-order equivalence split one physical class on ${JSON.stringify(surface)}`);
        }
        return values[0]!;
      });
    if (new Set(nodes).size !== nodes.length) duplicateSurfaceClasses++;
    for (let index = 1; index < nodes.length; index++) {
      const from = nodes[index - 1]!;
      const to = nodes[index]!;
      if (from === to) selfEdges++;
      else adjacencySets[from]!.add(to);
    }
    orderedSurfaceNodes.set(surface, nodes);
  }
  if (duplicateSurfaceClasses !== 0 || selfEdges !== 0) {
    throw new Error(
      `Lookup-order quotient has ${duplicateSurfaceClasses} duplicate surface classes and ${selfEdges} self edges`
    );
  }

  const reverse = Array.from({ length: quotient.length }, () => [] as number[]);
  let edgeCount = 0;
  const adjacency = adjacencySets.map((values, from) => {
    const result = [...values].sort((left, right) => left - right);
    edgeCount += result.length;
    for (const to of result) reverse[to]!.push(from);
    return result;
  });

  // Explicit iterative Kosaraju audit keeps a corrupt/cyclic source from being
  // hidden behind the later topological-rank failure.
  const seen = new Uint8Array(quotient.length);
  const finish: number[] = [];
  for (let start = 0; start < quotient.length; start++) {
    if (seen[start]) continue;
    const stack = [start];
    while (stack.length > 0) {
      const encoded = stack.pop()!;
      if (encoded < 0) {
        finish.push(~encoded);
        continue;
      }
      if (seen[encoded]) continue;
      seen[encoded] = 1;
      stack.push(~encoded);
      const next = adjacency[encoded]!;
      for (let index = next.length - 1; index >= 0; index--) {
        if (!seen[next[index]!]) stack.push(next[index]!);
      }
    }
  }
  seen.fill(0);
  const componentByNode = new Int32Array(quotient.length);
  componentByNode.fill(-1);
  const componentMembers: number[][] = [];
  let cyclicComponents = 0;
  for (let index = finish.length - 1; index >= 0; index--) {
    const start = finish[index]!;
    if (seen[start]) continue;
    const component = componentMembers.length;
    const members: number[] = [];
    const stack = [start];
    seen[start] = 1;
    while (stack.length > 0) {
      const node = stack.pop()!;
      componentByNode[node] = component;
      members.push(node);
      for (const prior of reverse[node]!) {
        if (seen[prior]) continue;
        seen[prior] = 1;
        stack.push(prior);
      }
    }
    if (members.length > 1) cyclicComponents++;
    componentMembers.push(members);
  }
  if (componentMembers.reduce((sum, values) => sum + values.length, 0) !== quotient.length) {
    throw new Error(
      `Lookup-order SCC traversal covered an incomplete quotient`
    );
  }

  const componentAdjacencySets = Array.from(
    { length: componentMembers.length },
    () => new Set<number>()
  );
  for (let from = 0; from < adjacency.length; from++) {
    const fromComponent = componentByNode[from]!;
    for (const to of adjacency[from]!) {
      const toComponent = componentByNode[to]!;
      if (fromComponent !== toComponent) componentAdjacencySets[fromComponent]!.add(toComponent);
    }
  }
  const componentAdjacency = componentAdjacencySets.map(values => [...values].sort((a, b) => a - b));
  const indegree = new Uint32Array(componentMembers.length);
  for (const next of componentAdjacency) for (const node of next) indegree[node]++;
  const ready: number[] = [];
  for (let index = 0; index < indegree.length; index++) {
    if (indegree[index] === 0) ready.push(index);
  }
  const componentLevels = new Uint8Array(componentMembers.length);
  let visited = 0;
  let maxRank = 0;
  while (ready.length > 0) {
    const node = ready.pop()!;
    visited++;
    for (const next of componentAdjacency[node]!) {
      const rank = componentLevels[node]! + 1;
      if (rank > 0x3f) throw new Error('Lookup-order global rank exceeds six bits');
      if (rank > componentLevels[next]!) componentLevels[next] = rank;
      maxRank = Math.max(maxRank, componentLevels[next]!);
      if (--indegree[next] === 0) ready.push(next);
    }
  }
  if (visited !== componentMembers.length) {
    throw new Error(`Lookup-order Kahn traversal covered ${visited}/${componentMembers.length} SCCs`);
  }
  const levels = new Uint8Array(quotient.length);
  for (let node = 0; node < quotient.length; node++) {
    levels[node] = componentLevels[componentByNode[node]!]!;
  }

  const exceptionSurfaces: string[] = [];
  for (const [surface, nodes] of orderedSurfaceNodes) {
    let exact = true;
    for (let index = 1; index < nodes.length; index++) {
      if (levels[nodes[index - 1]!]! >= levels[nodes[index]!]!) {
        exact = false;
        break;
      }
    }
    if (!exact) exceptionSurfaces.push(surface);
  }

  let exceptionClasses = 0;
  let exceptionLocators = 0;
  const exceptions = exceptionSurfaces.sort(compareText).map(surfaceKey => {
    const delimiter = surfaceKey.indexOf('\u0000');
    const route = surfaceKey.slice(0, delimiter);
    const surface = surfaceKey.slice(delimiter + 1);
    if ((route !== 'kana' && route !== 'kanji') || surface.length === 0) {
      throw new Error(`Invalid lookup-order exception key ${JSON.stringify(surfaceKey)}`);
    }
    const exceptionRoute: 'kana' | 'kanji' = route;
    const classes = surfaces.get(surfaceKey)!;
    exceptionClasses += classes.size;
    const orders = [] as Array<LookupOrderLocator & { rank: number }>;
    for (const [rank, locators] of [...classes].sort((a, b) => a[0] - b[0])) {
      for (const locatorKey of [...locators].sort((left, right) =>
        compareLookupOrderLocator(locatorByKey.get(left)!, locatorByKey.get(right)!))) {
        const locator = locatorByKey.get(locatorKey)!;
        orders.push({ ...locator, rank });
      }
    }
    exceptionLocators += orders.length;
    return { route: exceptionRoute, surface, orders };
  });

  const values = [...locatorByKey.entries()].map(([key, locator]) => ({
    ...locator,
    rank: levels[nodeForLocator(key)]!
  })).sort((left, right) => compareLookupOrderLocator(left, right));
  const projection = createHash('sha256');
  // This JSON-lines spelling is part of the release-lock ABI. Changing only
  // its punctuation changes the provenance digest even when every rank and
  // emitted byte is identical, so migrations must be explicit lock updates.
  for (const value of values) {
    projection.update(JSON.stringify([
      'global', value.rootSeq, value.firstAlias ?? -1,
      value.secondAlias ?? -1, value.rank
    ]) + '\n');
  }
  for (const exception of exceptions) {
    for (const value of exception.orders) {
      projection.update(JSON.stringify([
        'exception', exception.route, exception.surface, value.rootSeq,
        value.firstAlias ?? -1, value.secondAlias ?? -1, value.rank
      ]) + '\n');
    }
  }

  // Final compiler gate emulates the runtime's atomic choice: exact local
  // ranks for an exception surface, SCC-global levels everywhere else.
  const exceptionsBySurface = new Map(exceptions.map(value => [
    `${value.route}\u0000${value.surface}`,
    new Map(value.orders.map(order => [lookupOrderLocatorKey(order), order.rank]))
  ]));
  let replayedSurfaces = 0;
  for (const [surfaceKey, classes] of surfaces) {
    const local = exceptionsBySurface.get(surfaceKey);
    for (const [expectedRank, locators] of [...classes].sort((a, b) => a[0] - b[0])) {
      const ranks = new Set([...locators].map(locator => local
        ? local.get(locator)
        : levels[nodeForLocator(locator)]));
      if (ranks.size !== 1 || ranks.has(undefined)) {
        throw new Error(`Lookup-order runtime replay has incomplete class on ${JSON.stringify(surfaceKey)}`);
      }
      const actualRank = ranks.values().next().value as number;
      if (local && actualRank !== expectedRank) {
        throw new Error(`Lookup-order runtime replay has invalid rank on ${JSON.stringify(surfaceKey)}`);
      }
    }
    const physicalRanks = [...classes.entries()]
      .sort((left, right) => left[0] - right[0])
      .map(([, locators]) => {
        const locator = locators.values().next().value as string;
        return local ? local.get(locator)! : levels[nodeForLocator(locator)]!;
      });
    if (physicalRanks.some((rank, index) => index > 0 && rank <= physicalRanks[index - 1]!)) {
      throw new Error(`Lookup-order runtime ranks do not replay ${JSON.stringify(surfaceKey)}`);
    }
    if (local && local.size !== [...classes.values()].reduce((sum, locators) => sum + locators.size, 0)) {
      throw new Error(`Lookup-order exception is not complete for ${JSON.stringify(surfaceKey)}`);
    }
    replayedSurfaces++;
  }
  if (replayedSurfaces !== ambiguousSurfaces) {
    throw new Error(`Lookup-order runtime replay covered ${replayedSurfaces}/${ambiguousSurfaces} surfaces`);
  }
  return {
    values,
    sourceRows: rows.length,
    sourceSha256: sourceProjection.digest('hex'),
    surfaces: ambiguousSurfaces,
    physicalClasses,
    equivalenceClasses: quotient.length,
    components: componentMembers.length,
    cyclicComponents,
    edges: edgeCount,
    maxRank,
    sha256: projection.digest('hex'),
    exceptions,
    exceptionClasses,
    exceptionLocators
  };
}

export async function loadAnalyzerGeneratedSource(
  sql: postgres.Sql,
  morphology: CompiledMorphologyArtifact
): Promise<AnalyzerSupportGeneratedSource> {
  const aliasKeys = [...new Set(morphology.rules.map(semanticRuleKey))].sort(compareText);
  const aliasIds = new Map(aliasKeys.map((key, alias) => [key, alias]));
  const aliasProperties = aliasKeys.map(key => {
    const [pos, type, negative, formal] = JSON.parse(key) as [string, number, boolean | null, boolean | null];
    return { pos, type, negative, formal };
  });
  const ruleAliases = morphology.rules.map(rule => aliasIds.get(semanticRuleKey(rule))!);
  const rules: RuleProjection[] = morphology.rules.map((rule, ruleId) => ({
    alias: ruleAliases[ruleId]!, pos: rule.pos, type: rule.type,
    negative: rule.negative, formal: rule.formal,
    stem: rule.stem, okuri: rule.okuri, euphr: rule.euphr, euphk: rule.euphk
  }));
  const patchKeys = new Set<string>();
  const patches: PatchProjection[] = [];
  for (const patch of morphology.patches) {
    const value: PatchProjection = {
      route: patch.route, root_seq: patch.rootSeq, surface: patch.surface,
      first_alias: ruleAliases[patch.firstRule]!,
      second_alias: patch.secondRule === null ? null : ruleAliases[patch.secondRule]!
    };
    const key = JSON.stringify(value);
    if (!patchKeys.has(key)) { patchKeys.add(key); patches.push(value); }
  }

  const queryInputs = [
    sql.json(rules as unknown as postgres.JSONValue),
    sql.json(patches as unknown as postgres.JSONValue)
  ];
  const [rows, lookupOrderRows] = await Promise.all([
    sql.unsafe<GeneratedRow[]>(GENERATED_QUERY, queryInputs),
    sql.unsafe<LookupOrderRow[]>(LOOKUP_ORDER_QUERY, queryInputs)
  ]);
  if (rows.length === 0 || lookupOrderRows.length === 0
    || rows.some(row => row.loadedPatches !== patches.length)
    || lookupOrderRows.some(row => row.loadedPatches !== patches.length)) {
    throw new Error(`Generated SQL loaded an incomplete manual-patch projection; expected ${patches.length}`);
  }
  const positions = new Map(morphology.positions.map((pos, index) => [pos, index]));
  const targetConjugations = new Map<number, Set<number>>();
  const addConjugation = (target: number, conjugation: number): void => {
    const values = targetConjugations.get(target) ?? new Set<number>();
    values.add(conjugation); targetConjugations.set(target, values);
  };
  for (const row of rows) {
    addConjugation(row.targetSeq, row.finalConjugationId);
    if (row.viaTargetSeq !== null && row.viaConjugationId !== null) {
      addConjugation(row.viaTargetSeq, row.viaConjugationId);
    }
  }
  const memberOrdinals = new Map<number, Map<number, number>>();
  for (const [target, values] of targetConjugations) {
    memberOrdinals.set(target, new Map([...values].sort((a, b) => a - b).map((value, index) => [value, index])));
  }

  const byKey = new Map<string, GeneratedRow[]>();
  for (const row of rows) {
    const key = recordKey(row);
    const values = byKey.get(key) ?? [];
    if (values.length && values[0]!.targetSeq !== row.targetSeq) {
      throw new Error(`Generated key ${JSON.stringify(key)} maps to targets ${values[0]!.targetSeq} and ${row.targetSeq}`);
    }
    values.push(row); byKey.set(key, values);
  }
  const groupedTargets = new Map<number, string>();
  for (const [key, values] of byKey) {
    const row = values[0]!;
    if (row.targetPaths <= 1) continue;
    const prior = groupedTargets.get(row.targetSeq);
    if (prior === undefined || key < prior) groupedTargets.set(row.targetSeq, key);
  }
  const groupIds = new Map([...groupedTargets].sort((a, b) => compareText(a[1], b[1]))
    .map(([target], index) => [target, index + 1]));

  let propertyOverrides = 0;
  let physicalMembers = 0;
  let maxMemberOrd = 0;
  let maxViaMemberOrd = 0;
  let maxPropOrd = 0;
  const records: AnalyzerSupportGeneratedRecordSource[] = [...byKey]
    .sort((a, b) => compareText(a[0], b[0])).map(([, values]) => {
      const row = values[0]!;
      const semantic = aliasProperties[row.secondAlias ?? row.firstAlias]!;
      if (semantic.pos !== row.defaultPos || semantic.type !== row.defaultType
        || semantic.negative !== row.defaultNegative || semantic.formal !== row.defaultFormal) {
        throw new Error(`Generated default property disagrees for ${recordKey(row)}`);
      }
      const defaultPosId = positions.get(semantic.pos);
      if (defaultPosId === undefined) throw new Error(`Unknown generated position ${semantic.pos}`);
      const defaultProperty = { posId: defaultPosId, type: semantic.type,
        negative: semantic.negative, formal: semantic.formal };
      const propertyOrdinals = new Map<number, Map<number, number>>();
      for (const value of values) {
        const props = propertyOrdinals.get(value.finalConjugationId) ?? new Map<number, number>();
        if (!props.has(value.finalPropertyId)) props.set(value.finalPropertyId, props.size);
        propertyOrdinals.set(value.finalConjugationId, props);
      }
      const seen = new Set<string>();
      const members: AnalyzerSupportGeneratedMemberSource[] = [];
      for (const value of values) {
        const unique = `${value.finalConjugationId}\u0000${value.finalPropertyId}\u0000${value.viaConjugationId ?? -1}`;
        if (seen.has(unique)) continue;
        seen.add(unique);
        const posId = positions.get(value.pos);
        const memberOrd = memberOrdinals.get(value.targetSeq)?.get(value.finalConjugationId);
        const propOrd = propertyOrdinals.get(value.finalConjugationId)?.get(value.finalPropertyId);
        const viaMemberOrd = value.viaConjugationId === null || value.viaTargetSeq === null
          ? null : memberOrdinals.get(value.viaTargetSeq)?.get(value.viaConjugationId);
        if (posId === undefined || memberOrd === undefined || propOrd === undefined
          || (value.viaConjugationId !== null && viaMemberOrd === undefined)) {
          throw new Error(`Incomplete generated member projection for ${recordKey(row)}`);
        }
        const property = {
          posId,
          type: value.type,
          negative: value.negative,
          formal: value.formal
        };
        if (!sameProperty(property, defaultProperty)) propertyOverrides++;
        maxMemberOrd = Math.max(maxMemberOrd, memberOrd);
        maxViaMemberOrd = Math.max(maxViaMemberOrd, viaMemberOrd ?? 0);
        maxPropOrd = Math.max(maxPropOrd, propOrd);
        members.push({ property, memberOrd, propOrd, viaMemberOrd: viaMemberOrd ?? null });
      }
      members.sort((a, b) => a.memberOrd - b.memberOrd || a.propOrd - b.propOrd
        || (a.viaMemberOrd ?? -1) - (b.viaMemberOrd ?? -1));
      const physicalGroup = groupIds.get(row.targetSeq) ?? null;
      const needsMembers = physicalGroup !== null || members.length !== 1
        || members[0]!.memberOrd !== 0
        // Even ordinal zero is data when the prefix target has multiple
        // physical members: the final row must bind to that exact prefix.
        || (members[0]!.viaMemberOrd !== null && row.viaMembers > 1)
        || !sameProperty(members[0]!.property, defaultProperty);
      if (needsMembers) physicalMembers += members.length;
      const countException = row.nKanji !== row.rootNKanji || row.nKana !== row.rootNKana;
      return { rootSeq: row.rootSeq, firstAlias: row.firstAlias,
        secondAlias: row.secondAlias,
        counts: countException ? [row.nKanji, row.nKana] as const : null,
        physicalGroup, members: needsMembers ? members : null };
    });

  // Ordinary one-member/root-count paths need no scoring or presentation
  // overlay. Target seq remains a transient compiler join key and is never
  // persisted or hashed.
  const overlayRecords = records.filter(record =>
    record.counts !== null || record.members !== null
  );

  const projection = createHash('sha256');
  for (const record of overlayRecords) {
    projection.update([record.rootSeq, record.firstAlias, record.secondAlias ?? -1,
      record.counts?.[0] ?? -1, record.counts?.[1] ?? -1, record.physicalGroup ?? 0].join('\t') + '\n');
    for (const member of record.members ?? []) {
      projection.update([member.property.posId, member.property.type,
        member.property.negative === null ? -1 : Number(member.property.negative),
        member.property.formal === null ? -1 : Number(member.property.formal),
        member.memberOrd, member.propOrd, member.viaMemberOrd ?? -1].join('\t') + '\n');
    }
  }
  const lookupOrder = compileLookupOrders(lookupOrderRows, aliasKeys.length, patches.length);
  return {
    ruleAliases, aliasCount: aliasKeys.length, records: overlayRecords,
    semanticPaths: rows[0]?.semanticPaths ?? 0,
    matchedPaths: rows[0]?.matchedPaths ?? 0,
    countExceptions: overlayRecords.filter(record => record.counts !== null).length,
    lookupOrders: lookupOrder.values,
    lookupOrderSourceRows: lookupOrder.sourceRows,
    lookupOrderSourceSha256: lookupOrder.sourceSha256,
    lookupOrderSurfaces: lookupOrder.surfaces,
    lookupOrderClasses: lookupOrder.physicalClasses,
    lookupOrderEquivalenceClasses: lookupOrder.equivalenceClasses,
    lookupOrderComponents: lookupOrder.components,
    lookupOrderCyclicComponents: lookupOrder.cyclicComponents,
    lookupOrderEdges: lookupOrder.edges,
    lookupOrderMaxRank: lookupOrder.maxRank,
    lookupOrderProjectionSha256: lookupOrder.sha256,
    lookupOrderExceptions: lookupOrder.exceptions,
    lookupOrderExceptionClasses: lookupOrder.exceptionClasses,
    lookupOrderExceptionLocators: lookupOrder.exceptionLocators,
    physicalGroups: groupIds.size, physicalMembers, propertyOverrides,
    maxMemberOrd, maxViaMemberOrd, maxPropOrd,
    projectionSha256: projection.digest('hex')
  };
}
