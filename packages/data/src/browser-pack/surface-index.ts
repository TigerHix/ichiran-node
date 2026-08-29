/**
 * Read-only PostgreSQL export consumed by `tools/surface-index`.
 *
 * `text COLLATE "C"` is part of the binary format contract: PostgreSQL emits
 * the same unsigned UTF-8 byte order required by the streaming automaton
 * compiler. The five output fields are surface, then kana/kanji direct and
 * morphology flags. Morphology comes from `conj_source_reading`, not from
 * `entry.root_p = false`, because some installed conjugation links reuse
 * otherwise lexical entries. This also deliberately follows the normalized
 * morphology relation: it replaces 55 stale physical-table surfaces with the
 * 55 current source-reading surfaces. A source-reading surface missing from
 * both text tables is assigned by the same kana classifier used at runtime.
 */
export const SURFACE_INDEX_COPY_QUERY = String.raw`
COPY (
  WITH kana_surface AS MATERIALIZED (
    SELECT
      form.text,
      bool_or(entry.root_p) AS direct
    FROM kana_text AS form
    INNER JOIN entry USING (seq)
    GROUP BY form.text
  ),
  kanji_surface AS MATERIALIZED (
    SELECT
      form.text,
      bool_or(entry.root_p) AS direct
    FROM kanji_text AS form
    INNER JOIN entry USING (seq)
    GROUP BY form.text
  ),
  morphology AS MATERIALIZED (
    SELECT DISTINCT text
    FROM conj_source_reading
  ),
  routed_flags AS (
    SELECT text, true AS kd, false AS km, false AS jd, false AS jm
    FROM kana_surface
    WHERE direct
    UNION ALL
    SELECT text, false AS kd, false AS km, true AS jd, false AS jm
    FROM kanji_surface
    WHERE direct
    UNION ALL
    SELECT
      morphology.text,
      false AS kd,
      (
        kana_surface.text IS NOT NULL
        OR (
          kana_surface.text IS NULL
          AND kanji_surface.text IS NULL
          AND morphology.text ~ '^[ァ-ヺヽヾーぁ-ゔゝゞ]+$'
        )
      ) AS km,
      false AS jd,
      (
        kanji_surface.text IS NOT NULL
        OR (
          kana_surface.text IS NULL
          AND kanji_surface.text IS NULL
          AND morphology.text !~ '^[ァ-ヺヽヾーぁ-ゔゝゞ]+$'
        )
      ) AS jm
    FROM morphology
    LEFT JOIN kana_surface USING (text)
    LEFT JOIN kanji_surface USING (text)
  )
  SELECT
    text,
    bool_or(kd)::integer,
    bool_or(km)::integer,
    bool_or(jd)::integer,
    bool_or(jm)::integer
  FROM routed_flags
  GROUP BY text
  ORDER BY text COLLATE "C"
) TO STDOUT WITH (FORMAT text, DELIMITER E'\t', NULL '')
`.trim();
