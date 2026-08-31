import { createHash } from 'node:crypto';
import { readFile } from 'node:fs/promises';
import { join } from 'node:path';
import ts from 'typescript';

export interface AnalyzerFixtureRequest {
  readonly text: string;
  readonly limit: number;
  readonly normalizePunctuation?: boolean;
}

export interface AnalyzerProbeFixture {
  readonly category:
    | 'top-n'
    | 'counter-number'
    | 'normalization'
    | 'punctuation-chunks'
    | 'generated-exception';
  readonly name: string;
  readonly request: AnalyzerFixtureRequest;
}

export interface SegmentationFixture {
  readonly input: string;
  readonly expected: readonly string[];
}

export interface AnalyzerEntityFixture {
  readonly title: string;
  readonly text: string;
  readonly entities: readonly {
    readonly start: number;
    readonly end: number;
    readonly boost?: number;
  }[];
}

export interface AnalyzerParityCorpus {
  readonly segmentation: readonly SegmentationFixture[];
  readonly romanization: readonly string[];
  readonly cli: readonly AnalyzerFixtureRequest[];
  readonly hard: readonly AnalyzerFixtureRequest[];
  readonly counters: readonly AnalyzerFixtureRequest[];
  readonly entities: readonly AnalyzerEntityFixture[];
  readonly probes: readonly AnalyzerProbeFixture[];
  readonly currentLispCli: Readonly<Record<string, string>>;
  readonly currentLispHard: Readonly<Record<string, string>>;
  readonly currentLispRomanization: Readonly<Record<string, string>>;
}

const FIXTURES = Object.freeze({
  'packages/reference-postgres/tests/data/segmentation.json':
    'a3df8f66132c50d3f78d68632ed8d3477717f8e95b0730e89f05e588252e4944',
  'packages/cli/tests/data/cli.json':
    'bc611dcf11e4b271ca2775a58f8c6615130fa2d42782cc1a679fb34eb8d73f5a',
  'packages/cli/tests/data/cli-lisp-outputs.json':
    'a092f07a2b7337c3a790b0d93808213adf2e89eef1750aeaed54160b90856bb8',
  'packages/cli/tests/data/cli-canonical-outputs.json':
    '2558e17996d2f08f100a4b3448e2af5e1b63706606c13066367ea437f3b1697b',
  'packages/cli/tests/data/hard-cli.json':
    '5e8a910314843a25c4bf2dd4663db0211fecc31a031b38c88c9880780115be69',
  'packages/cli/tests/data/hard-cli-lisp-outputs.json':
    'd82f5d5e9ef3b858209ea63a1ea5b448c6460e4ea0fddfc6b811ebb7c3756a85',
  'packages/cli/tests/data/hard-cli-canonical-outputs.json':
    '71b1417d13a2c546fa56d502c23bf9614e73d0a01cec731a378512a8b785c736'
});

const COUNTERS = Object.freeze([
  '倍', '晩', '秒', '着', '挺', '丁', '台', '段', '度', '円', '服', '幅', '分', '杯',
  '発', '遍', '篇', '匹', '本', '時', '畳', '帖', '条', '課', '日', '回', 'ヵ月', '階',
  '軒', '機', '個', '脚', '間', '枚', '巻', '名', '年', '人', '列', '輪', '輌', '才',
  '歳', '棹', '冊', '隻', '章', '首', '足', '艘', '反', '滴', '点', '頭', 'つ', '通',
  '対', '羽', '把', '割', '膳', '時間', '週間', '人中', '番目', '期目', '巻目'
]);

const COUNTER_NUMBERS = Object.freeze(['1', '三', '十一']);

function probeFixtures(): AnalyzerProbeFixture[] {
  const result: AnalyzerProbeFixture[] = [];
  for (const limit of [1, 2, 3, 5, 10]) {
    result.push({
      category: 'top-n',
      name: `ambiguous-hashi-top-${limit}`,
      request: { text: 'はし', limit }
    });
  }
  for (const text of [
    '0個', '4時', '6本', '8匹', '10冊', '100本', '1000本', '10000本',
    '1日', '14日', '20日', '24日', '30日', '1人', '2人', '20歳',
    '１２人', '一億二万三人', '3番目', '三巻目'
  ]) {
    result.push({
      category: 'counter-number',
      name: `counter-${text}`,
      request: { text, limit: 5 }
    });
  }
  for (const [name, text] of [
    ['fullwidth-ascii-digits', 'ＡＢＣ１２３'],
    ['halfwidth-katakana', 'ﾊｼ'],
    ['combining-dakuten', 'ばし']
  ] as const) {
    result.push({ category: 'normalization', name, request: { text, limit: 5 } });
  }
  for (const normalizePunctuation of [false, true]) {
    result.push({
      category: 'punctuation-chunks',
      name: `mixed-punctuation-${normalizePunctuation ? 'normalized' : 'preserved'}`,
      request: {
        text: '猫，犬。雨？',
        limit: 5,
        normalizePunctuation
      }
    });
  }
  // Short witnesses are taken from the locked CLI/hard corpus and the pinned
  // generated-projection investigations; keep the shape explicit here rather
  // than maintaining a broad second morphology word list.
  for (const [name, text, limit] of [
    ['direct-generated', '食べた', 1],
    ['formal-generated-top-n', '食べました', 3],
    ['contextual-list-reading-top-1', '何他', 1],
    ['contextual-list-reading-top-3', '何他', 3],
    ['passive-compound-top-n', 'あてられている', 5],
    ['causative-compound-top-n', 'きかせられている', 5],
    ['generated-count-exception-root', '忘れる', 5],
    ['generated-count-exception-stem', '忘れ', 5],
    ['generated-multi-property', '忘れた', 5],
    ['generated-conjunctive', '忘れて', 5],
    ['generated-two-stage', '盛れてて', 5],
    ['generated-shared-target', 'もらえた', 5]
  ] as const) {
    result.push({
      category: 'generated-exception',
      name,
      request: { text, limit }
    });
  }
  for (let depth = 7; depth <= 11; depth++) {
    result.push({
      category: 'generated-exception',
      name: `nested-teiru-depth-${depth}`,
      request: {
        text: `食べて${'いて'.repeat(depth)}いる`,
        limit: depth === 11 ? 3 : 1
      }
    });
  }
  return result;
}

function digest(bytes: Uint8Array): string {
  return createHash('sha256').update(bytes).digest('hex');
}

async function lockedJson<T>(root: string, path: keyof typeof FIXTURES): Promise<T> {
  const bytes = new Uint8Array(await readFile(join(root, path)));
  const actual = digest(bytes);
  if (actual !== FIXTURES[path]) {
    throw new Error(`${path} digest ${actual}; parity contract requires ${FIXTURES[path]}`);
  }
  return JSON.parse(new TextDecoder().decode(bytes)) as T;
}

function literal(node: ts.Expression): unknown {
  if (ts.isParenthesizedExpression(node)) return literal(node.expression);
  if (ts.isAsExpression(node) || ts.isTypeAssertionExpression(node)) return literal(node.expression);
  if (ts.isStringLiteral(node) || ts.isNoSubstitutionTemplateLiteral(node)) return node.text;
  if (ts.isNumericLiteral(node)) return Number(node.text);
  if (node.kind === ts.SyntaxKind.TrueKeyword) return true;
  if (node.kind === ts.SyntaxKind.FalseKeyword) return false;
  if (node.kind === ts.SyntaxKind.NullKeyword) return null;
  if (ts.isPrefixUnaryExpression(node) && node.operator === ts.SyntaxKind.MinusToken) {
    const value = literal(node.operand);
    if (typeof value === 'number') return -value;
  }
  if (ts.isArrayLiteralExpression(node)) return node.elements.map(value => literal(value as ts.Expression));
  if (ts.isObjectLiteralExpression(node)) {
    const value: Record<string, unknown> = {};
    for (const property of node.properties) {
      if (!ts.isPropertyAssignment(property)) {
        throw new Error(`Unsupported entity fixture property: ${property.getText()}`);
      }
      const name = property.name;
      const key = ts.isIdentifier(name) || ts.isStringLiteral(name) || ts.isNumericLiteral(name)
        ? name.text
        : null;
      if (key === null) throw new Error(`Unsupported entity fixture key: ${name.getText()}`);
      value[key] = literal(property.initializer);
    }
    return value;
  }
  throw new Error(`Unsupported entity fixture literal: ${node.getText()}`);
}

function variable(block: ts.Block, name: string): unknown {
  for (const statement of block.statements) {
    if (!ts.isVariableStatement(statement)) continue;
    for (const declaration of statement.declarationList.declarations) {
      if (
        ts.isIdentifier(declaration.name)
        && declaration.name.text === name
        && declaration.initializer
      ) return literal(declaration.initializer);
    }
  }
  return undefined;
}

/**
 * Keep the 54 entity requests owned by their existing tests instead of
 * maintaining a second hand-copied list. Only literal fixture declarations
 * are accepted, so executable test code is never evaluated by the harness.
 */
async function entityFixtures(root: string): Promise<AnalyzerEntityFixture[]> {
  const path = join(root, 'packages/reference-postgres/tests/entity-hints.test.ts');
  const source = await readFile(path, 'utf8');
  const file = ts.createSourceFile(path, source, ts.ScriptTarget.Latest, true, ts.ScriptKind.TS);
  const result: AnalyzerEntityFixture[] = [];
  const visit = (node: ts.Node): void => {
    if (
      ts.isCallExpression(node)
      && ts.isIdentifier(node.expression)
      && node.expression.text === 'test'
      && node.arguments.length >= 2
      && ts.isStringLiteral(node.arguments[0]!)
      && (ts.isArrowFunction(node.arguments[1]!) || ts.isFunctionExpression(node.arguments[1]!))
      && ts.isBlock(node.arguments[1]!.body)
    ) {
      const text = variable(node.arguments[1]!.body, 'text');
      const entities = variable(node.arguments[1]!.body, 'entities');
      if (typeof text !== 'string') {
        throw new Error(`Entity fixture ${node.arguments[0]!.text} has no literal text`);
      }
      if (entities !== undefined && !Array.isArray(entities)) {
        throw new Error(`Entity fixture ${node.arguments[0]!.text} has non-array entities`);
      }
      result.push({
        title: node.arguments[0]!.text,
        text,
        entities: (entities ?? []) as AnalyzerEntityFixture['entities']
      });
    }
    ts.forEachChild(node, visit);
  };
  visit(file);
  if (result.length !== 54) throw new Error(`Expected 54 entity fixtures, found ${result.length}`);
  return result;
}

function counterFixtures(): AnalyzerFixtureRequest[] {
  const result: AnalyzerFixtureRequest[] = [];
  for (const counter of COUNTERS) {
    for (const number of COUNTER_NUMBERS) {
      if (counter === 'つ' && number === '十一') continue;
      result.push({ text: `${number}${counter}`, limit: 1 });
    }
  }
  if (result.length !== 200) throw new Error(`Expected 200 counter fixtures, found ${result.length}`);
  return result;
}

interface CliFixtureFile {
  readonly romanization: readonly string[];
  readonly fullJson: readonly AnalyzerFixtureRequest[];
}

interface HistoricalOutputFile {
  readonly romanization: Readonly<Record<string, string>>;
  readonly fullJson: Readonly<Record<string, string>>;
}

interface CanonicalOutputFile {
  readonly formatVersion: number;
  readonly identityPolicy: string;
  readonly source: {
    readonly path: string;
    readonly sha256: string;
  };
  readonly oracle: {
    readonly sourcesLockSha256: string;
    readonly upstreamIchiranCommit: string;
    readonly dataReleaseTag: string;
    readonly postgresReferenceCommit: string;
    readonly databaseDumpSha256: string;
    readonly databaseSchemaSha256: string;
  };
  readonly stats: {
    readonly requests: number;
    readonly rewrittenSeqFields: number;
    readonly multipleRootIdentityKeys: number;
    readonly outputsSha256: string;
  };
  readonly fullJson: Readonly<Record<string, string>>;
}

async function canonicalOutputs(
  root: string,
  canonicalPath: keyof typeof FIXTURES,
  rawPath: keyof typeof FIXTURES,
  expectedRequests: number
): Promise<CanonicalOutputFile> {
  const [canonical, lockBytes] = await Promise.all([
    lockedJson<CanonicalOutputFile>(root, canonicalPath),
    readFile(join(root, 'browser-alpha/sources.lock.json'))
  ]);
  const lock = JSON.parse(lockBytes.toString()) as {
    readonly upstreamIchiran: { readonly commit: string; readonly dataReleaseTag: string };
    readonly postgresReference: { readonly repositoryCommit: string };
    readonly databaseDump: { readonly sha256: string };
    readonly database: { readonly schemaSha256: string };
  };
  const outputBytes = new TextEncoder().encode(JSON.stringify(canonical.fullJson));
  const checks: readonly [label: string, actual: unknown, expected: unknown][] = [
    ['format version', canonical.formatVersion, 1],
    ['identity policy', canonical.identityPolicy, 'terminal-root-v1'],
    ['raw source path', canonical.source.path, rawPath],
    ['raw source SHA-256', canonical.source.sha256, FIXTURES[rawPath]],
    ['sources lock SHA-256', canonical.oracle.sourcesLockSha256, digest(lockBytes)],
    ['upstream Ichiran commit', canonical.oracle.upstreamIchiranCommit, lock.upstreamIchiran.commit],
    ['data release tag', canonical.oracle.dataReleaseTag, lock.upstreamIchiran.dataReleaseTag],
    [
      'PostgreSQL reference commit',
      canonical.oracle.postgresReferenceCommit,
      lock.postgresReference.repositoryCommit
    ],
    ['database dump SHA-256', canonical.oracle.databaseDumpSha256, lock.databaseDump.sha256],
    ['database schema SHA-256', canonical.oracle.databaseSchemaSha256, lock.database.schemaSha256],
    ['request count', canonical.stats.requests, expectedRequests],
    ['output count', Object.keys(canonical.fullJson).length, expectedRequests],
    ['output SHA-256', canonical.stats.outputsSha256, digest(outputBytes)]
  ];
  for (const [label, actual, expected] of checks) {
    if (actual !== expected) {
      throw new Error(
        `${canonicalPath} ${label} ${JSON.stringify(actual)}; expected ${JSON.stringify(expected)}`
      );
    }
  }
  if (canonical.stats.rewrittenSeqFields <= 0) {
    throw new Error(`${canonicalPath} has no generated sequence identities to normalize`);
  }
  return canonical;
}

export function fixtureKey(request: AnalyzerFixtureRequest): string {
  const normalization = request.normalizePunctuation === undefined
    ? ''
    : `|normalizePunctuation=${request.normalizePunctuation}`;
  return `${request.text}|${request.limit}${normalization}`;
}

export async function loadAnalyzerParityCorpus(root: string): Promise<AnalyzerParityCorpus> {
  const [
    segmentation,
    cliFile,
    hardFile,
    cliExpected,
    hardExpected,
    cliCanonical,
    hardCanonical,
    entities
  ] = await Promise.all([
    lockedJson<SegmentationFixture[]>(root, 'packages/reference-postgres/tests/data/segmentation.json'),
    lockedJson<CliFixtureFile>(root, 'packages/cli/tests/data/cli.json'),
    lockedJson<CliFixtureFile>(root, 'packages/cli/tests/data/hard-cli.json'),
    lockedJson<HistoricalOutputFile>(root, 'packages/cli/tests/data/cli-lisp-outputs.json'),
    lockedJson<HistoricalOutputFile>(root, 'packages/cli/tests/data/hard-cli-lisp-outputs.json'),
    canonicalOutputs(
      root,
      'packages/cli/tests/data/cli-canonical-outputs.json',
      'packages/cli/tests/data/cli-lisp-outputs.json',
      252
    ),
    canonicalOutputs(
      root,
      'packages/cli/tests/data/hard-cli-canonical-outputs.json',
      'packages/cli/tests/data/hard-cli-lisp-outputs.json',
      149
    ),
    entityFixtures(root)
  ]);
  const counters = counterFixtures();
  const probes = probeFixtures();
  for (const [label, actual, expected] of [
    ['segmentation', segmentation.length, 534],
    ['romanization', cliFile.romanization.length, 5],
    ['CLI', cliFile.fullJson.length, 252],
    ['hard CLI', hardFile.fullJson.length, 149]
  ] as const) {
    if (actual !== expected) throw new Error(`Expected ${expected} ${label} fixtures, found ${actual}`);
  }
  for (const request of cliFile.fullJson) {
    const key = fixtureKey(request);
    if (!(key in cliExpected.fullJson) || !(key in cliCanonical.fullJson)) {
      throw new Error(`Current-Lisp CLI output is missing ${key}`);
    }
  }
  for (const input of cliFile.romanization) {
    if (!(input in cliExpected.romanization)) {
      throw new Error(`Historical romanization output is missing ${JSON.stringify(input)}`);
    }
  }
  for (const request of hardFile.fullJson) {
    const key = fixtureKey(request);
    if (!(key in hardExpected.fullJson) || !(key in hardCanonical.fullJson)) {
      throw new Error(`Current-Lisp hard output is missing ${key}`);
    }
  }
  return {
    segmentation,
    romanization: cliFile.romanization,
    cli: cliFile.fullJson,
    hard: hardFile.fullJson,
    counters,
    entities,
    probes,
    currentLispCli: cliCanonical.fullJson,
    currentLispHard: hardCanonical.fullJson,
    currentLispRomanization: cliExpected.romanization
  };
}
