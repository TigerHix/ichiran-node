#!/usr/bin/env bun

import { createHash } from 'node:crypto';
import {
  upstream260118HintMap,
  upstream260118SplitMap
} from '../packages/data/src/browser-pack/analyzer-upstream-260118.js';
import {
  LEGACY_EASY_HINT_DECLARATIONS,
  UPSTREAM_260118_EASY_HINT_DECLARATIONS
} from '../packages/data/src/source-compiler/analyzer-support-easy-hint-declarations.js';
import {
  LEGACY_SIMPLE_HINT_DECLARATIONS,
  UPSTREAM_260118_SIMPLE_HINT_DECLARATIONS
} from '../packages/data/src/source-compiler/analyzer-support-simple-hint-declarations.js';
import {
  LEGACY_SPLIT_DECLARATIONS,
  SEGMENT_SPLIT_DECLARATIONS,
  SPLIT_DECLARATIONS
} from '../packages/data/src/source-compiler/analyzer-support-split-declarations.js';
import {
  hintMap,
  segsplitMap,
  splitMap
} from '../packages/reference-postgres/src/dict/splitMaps.js';
import '../packages/reference-postgres/src/dict/splitDefinitions.js';

function digest(values: readonly number[]): string {
  return createHash('sha256')
    .update([...values].sort((left, right) => left - right).join('\n') + '\n')
    .digest('hex');
}

function compare(
  source: readonly number[],
  oracle: readonly number[]
): { readonly missing: readonly number[]; readonly extra: readonly number[] } {
  const sourceSet = new Set(source);
  const oracleSet = new Set(oracle);
  return {
    missing: [...oracleSet].filter(value => !sourceSet.has(value)).sort((a, b) => a - b),
    extra: [...sourceSet].filter(value => !oracleSet.has(value)).sort((a, b) => a - b)
  };
}

const sourceLegacyHints = [
  ...LEGACY_SIMPLE_HINT_DECLARATIONS.map(value => value[0]),
  ...LEGACY_EASY_HINT_DECLARATIONS.map(value => value[0])
];
const sourceHints = [
  ...sourceLegacyHints,
  ...UPSTREAM_260118_SIMPLE_HINT_DECLARATIONS.map(value => value[0]),
  ...UPSTREAM_260118_EASY_HINT_DECLARATIONS.map(value => value[0])
];
const oracleSplits = [...new Set([...splitMap.keys(), ...upstream260118SplitMap.keys()])];
const oracleHints = [...new Set([...hintMap.keys(), ...upstream260118HintMap.keys()])];

const report = {
  legacySplits: {
    source: LEGACY_SPLIT_DECLARATIONS.length,
    oracle: splitMap.size,
    sha256: digest(LEGACY_SPLIT_DECLARATIONS.map(value => value.seq)),
    ...compare(LEGACY_SPLIT_DECLARATIONS.map(value => value.seq), [...splitMap.keys()])
  },
  qualifiedSplits: {
    source: SPLIT_DECLARATIONS.length,
    oracle: oracleSplits.length,
    sha256: digest(SPLIT_DECLARATIONS.map(value => value.seq)),
    ...compare(SPLIT_DECLARATIONS.map(value => value.seq), oracleSplits)
  },
  segmentSplits: {
    source: SEGMENT_SPLIT_DECLARATIONS.length,
    oracle: segsplitMap.size,
    sha256: digest(SEGMENT_SPLIT_DECLARATIONS.map(value => value.seq)),
    ...compare(SEGMENT_SPLIT_DECLARATIONS.map(value => value.seq), [...segsplitMap.keys()])
  },
  legacyHints: {
    source: sourceLegacyHints.length,
    oracle: hintMap.size,
    sha256: digest(sourceLegacyHints),
    ...compare(sourceLegacyHints, [...hintMap.keys()])
  },
  qualifiedHints: {
    source: sourceHints.length,
    oracle: oracleHints.length,
    sha256: digest(sourceHints),
    ...compare(sourceHints, oracleHints)
  }
};

console.log(JSON.stringify(report, null, 2));
if (Object.values(report).some(value => value.missing.length > 0 || value.extra.length > 0)) {
  process.exitCode = 1;
}
