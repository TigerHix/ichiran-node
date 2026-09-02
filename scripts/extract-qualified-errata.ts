import { readFile, writeFile } from 'node:fs/promises';
import ts from 'typescript';

const INPUT = 'packages/data/src/data/errata.ts';
const OUTPUT = 'data/source-compiler-errata.json';

const PHASES = [
  'addErrata',
  'addErrataFeb17',
  'addErrataJan18',
  'addErrataMar18',
  'addErrataAug18',
  'addErrataJan19',
  'addErrataApr19',
  'addErrataJan20',
  'addErrataApr20',
  'addErrataJul20',
  'addErrataJan21',
  'addErrataMay21',
  'addErrataJan22',
  'addErrataDec23',
  'addErrataJan25',
  'addErrataJan26',
  'addErrataCounters'
] as const;

const OPERATIONS = new Set([
  'conjugateDa',
  'addDehaJaReadings',
  'removeHiraganaNokanji',
  'addGozaimasuConjs',
  'setCommon',
  'setPrimaryNokanji',
  'addPrimaryNokanji',
  'deleteReading',
  'addReading',
  'replaceReading',
  'replaceReadingConj',
  'deleteSenseProp',
  'addSenseProp',
  'addNewSense',
  'addGloss',
  'rearrangeReadingsConj',
  'deleteSenses',
  'deleteConjugation',
  'addConjReading',
  'addConj'
]);

interface ErrataRow {
  event: number;
  phase: string;
  operation: string;
  arguments: unknown[];
  sourceLine: number;
  preservedBehavior: string;
}

function literal(node: ts.Expression, variables: ReadonlyMap<string, unknown>): unknown {
  if (ts.isParenthesizedExpression(node)) return literal(node.expression, variables);
  if (ts.isStringLiteral(node) || ts.isNoSubstitutionTemplateLiteral(node)) return node.text;
  if (ts.isNumericLiteral(node)) return Number(node.text);
  if (node.kind === ts.SyntaxKind.TrueKeyword) return true;
  if (node.kind === ts.SyntaxKind.FalseKeyword) return false;
  if (node.kind === ts.SyntaxKind.NullKeyword) return null;
  if (ts.isIdentifier(node)) {
    if (node.text === 'undefined') return null;
    if (variables.has(node.text)) return variables.get(node.text);
  }
  if (ts.isPrefixUnaryExpression(node) && node.operator === ts.SyntaxKind.MinusToken) {
    const value = literal(node.operand, variables);
    if (typeof value === 'number') return -value;
  }
  if (ts.isArrayLiteralExpression(node)) {
    return node.elements.map(element => literal(element as ts.Expression, variables));
  }
  if (ts.isObjectLiteralExpression(node)) {
    const value: Record<string, unknown> = {};
    for (const property of node.properties) {
      if (!ts.isPropertyAssignment(property)) {
        throw new Error(`Unsupported object property: ${property.getText()}`);
      }
      const key = property.name.getText().replace(/^['"]|['"]$/g, '');
      value[key] = literal(property.initializer, variables);
    }
    return value;
  }
  if (ts.isArrowFunction(node) || ts.isFunctionExpression(node)) {
    return { predicate: node.getText().replace(/\s+/g, ' ') };
  }
  throw new Error(`Unsupported errata argument: ${node.getText()}`);
}

function behavior(operation: string, args: readonly unknown[]): string {
  const [first, second, third] = args;
  switch (operation) {
    case 'setCommon':
      return `Set ${String(first)} form ${String(third)} on entry ${String(second)} to the declared common rank.`;
    case 'setPrimaryNokanji':
      return `Set entry ${String(first)} primary-no-kanji to ${String(second)}.`;
    case 'addPrimaryNokanji':
      return `Add ${String(second)} to entry ${String(first)} and make it the primary no-kanji reading.`;
    case 'deleteReading':
      return `Remove the declared ${String(second)} form from entry ${String(first)}.`;
    case 'addReading':
      return `Add the declared ${String(second)} form to entry ${String(first)}.`;
    case 'replaceReading':
    case 'replaceReadingConj':
      return `Apply the declared reading correction to entry ${String(first)}.`;
    case 'deleteSenseProp':
      return `Remove ${String(second)}:${String(third)} from entry ${String(first)}.`;
    case 'addSenseProp':
      return `Add the declared sense property to entry ${String(first)} sense ${String(second)}.`;
    case 'addNewSense':
    case 'addGloss':
      return `Add the declared sense content to entry ${String(first)}.`;
    case 'rearrangeReadingsConj':
      return `Give entry ${String(first)} forms with the declared prefix stable precedence.`;
    case 'deleteSenses':
      return `Remove the declared sense subset from entry ${String(first)}.`;
    case 'deleteConjugation':
      return `Remove the declared generated lineage ${String(first)} from root ${String(second)}.`;
    case 'addConjReading':
    case 'addConj':
      return `Add the declared manual generated lineage for root ${String(first)}.`;
    case 'conjugateDa':
      return 'Give だ the synthetic cop-da conjugation position.';
    case 'addDehaJaReadings':
      return 'Add the chronological では-to-じゃ generated readings.';
    case 'removeHiraganaNokanji':
      return 'Clear entry-level primary-no-kanji when a no-kanji reading is hiragana; preserve per-form flags.';
    case 'addGozaimasuConjs':
      return 'Add the declared ございます manual conjugations.';
    default:
      return `Apply the declared ${operation} correction.`;
  }
}

function callFromStatement(statement: ts.Statement): ts.CallExpression | null {
  if (!ts.isExpressionStatement(statement)) return null;
  const expression = ts.isAwaitExpression(statement.expression)
    ? statement.expression.expression
    : statement.expression;
  return ts.isCallExpression(expression) ? expression : null;
}

const sourceText = await readFile(INPUT, 'utf8');
const sourceFile = ts.createSourceFile(INPUT, sourceText, ts.ScriptTarget.Latest, true);
const functions = new Map<string, ts.Block>();
for (const statement of sourceFile.statements) {
  if (ts.isFunctionDeclaration(statement) && statement.name && statement.body) {
    functions.set(statement.name.text, statement.body);
  }
}

const rows: ErrataRow[] = [];
let event = 0;

function record(call: ts.CallExpression, phase: string, variables: ReadonlyMap<string, unknown>): void {
  if (!ts.isIdentifier(call.expression) || !OPERATIONS.has(call.expression.text)) return;
  const operation = call.expression.text;
  const args = call.arguments.map(argument => literal(argument, variables));
  const sourceLine = sourceFile.getLineAndCharacterOfPosition(call.getStart()).line + 1;
  rows.push({
    event: event++,
    phase,
    operation,
    arguments: args,
    sourceLine,
    preservedBehavior: behavior(operation, args)
  });
}

function walk(statements: ts.NodeArray<ts.Statement>, phase: string, inherited = new Map<string, unknown>()): void {
  const variables = new Map(inherited);
  for (const statement of statements) {
    if (ts.isVariableStatement(statement)) {
      for (const declaration of statement.declarationList.declarations) {
        if (ts.isIdentifier(declaration.name) && declaration.initializer) {
          try {
            variables.set(declaration.name.text, literal(declaration.initializer, variables));
          } catch {
            // Database handles and other implementation locals are not semantic inputs.
          }
        }
      }
      continue;
    }
    const call = callFromStatement(statement);
    if (call) {
      record(call, phase, variables);
      continue;
    }
    if (ts.isForOfStatement(statement) && ts.isVariableDeclarationList(statement.initializer)) {
      const declaration = statement.initializer.declarations[0];
      if (!declaration || !ts.isIdentifier(declaration.name)) continue;
      const values = literal(statement.expression, variables);
      if (!Array.isArray(values)) throw new Error(`For-of value is not a literal array: ${statement.expression.getText()}`);
      for (const value of values) {
        const iteration = new Map(variables).set(declaration.name.text, value);
        if (ts.isBlock(statement.statement)) walk(statement.statement.statements, phase, iteration);
        else walk(ts.factory.createNodeArray([statement.statement]), phase, iteration);
      }
      continue;
    }
    if (ts.isBlock(statement)) walk(statement.statements, phase, variables);
  }
}

for (const phase of PHASES) {
  const body = functions.get(phase);
  if (!body) throw new Error(`Missing errata phase ${phase}`);
  walk(body.statements, phase);
}

await writeFile(OUTPUT, `${JSON.stringify({
  formatVersion: 1,
  authority: {
    upstreamRepository: 'https://github.com/tshatrov/ichiran.git',
    upstreamCommit: 'ea9583368e67cad22d94abae8dbcc8df96d99bcd',
    upstreamPath: 'dict-errata.lisp',
    upstreamSha256: '44b37171b95f7b0e40181ee5ea0edd77439871363c7dfdb0d71bf9187538cdb7',
    migrationPortPath: INPUT
  },
  rows
}, null, 2)}\n`);

console.log(JSON.stringify({ output: OUTPUT, rows: rows.length }));
