import type { GrammarDefinition, PatternNode } from './types.js';
import { readFileSync, readdirSync } from 'node:fs';
import { join, dirname } from 'node:path';
import { fileURLToPath } from 'node:url';

function loadJsonFile<T>(filePath: string): T {
  const raw = readFileSync(filePath, 'utf8');
  return JSON.parse(raw) as T;
}

function loadGrammars(level: string): GrammarDefinition[] {
  const baseDir = typeof __dirname === 'string' ? __dirname : dirname(fileURLToPath(import.meta.url));
  const dir = join(baseDir, 'grammars', level);
  const files = readdirSync(dir).filter(f => f.endsWith('.json'));
  const grammars: GrammarDefinition[] = files
    .map(f => loadJsonFile<GrammarDefinition>(join(dir, f)))
    .filter(def => isNonEmptyPattern(def.pattern));
  return grammars;
}

export const grammarCatalog: GrammarDefinition[] = [
  ...loadGrammars('n1'),
  ...loadGrammars('n2'),
  ...loadGrammars('n3'),
  ...loadGrammars('n4'),
  ...loadGrammars('n5'),
];

function isNonEmptyPattern(pattern: PatternNode | undefined): boolean {
  if (!pattern) return false;
  // Consider a pattern empty if it has an empty alt/sequence or is otherwise an empty object
  if ('alt' in pattern) return Array.isArray(pattern.alt) && pattern.alt.length > 0;
  if ('sequence' in pattern) return Array.isArray(pattern.sequence) && pattern.sequence.length > 0;
  if ('token' in pattern) return Array.isArray(pattern.token) && pattern.token.length > 0;
  if ('repeat' in pattern) return !!pattern.repeat && isNonEmptyPattern(pattern.repeat.pattern);
  if ('optional' in pattern) return !!pattern.optional && isNonEmptyPattern(pattern.optional);
  if ('capture' in pattern) return !!pattern.capture && isNonEmptyPattern(pattern.pattern);
  if ('peek' in pattern) return isNonEmptyPattern(pattern.peek);
  if ('not' in pattern) return isNonEmptyPattern(pattern.not);
  if ('anchor' in pattern) return true; // start/end anchors are valid
  if ('macro' in pattern) return !!pattern.macro; // assume macro expands to non-empty
  return false;
}
