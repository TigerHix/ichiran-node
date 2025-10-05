import type { GrammarDefinition } from './types.js';
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
  const grammars: GrammarDefinition[] = files.map(f => loadJsonFile<GrammarDefinition>(join(dir, f)));
  return grammars;
}

export const grammarCatalog: GrammarDefinition[] = [
  ...loadGrammars('n1'),
  ...loadGrammars('n2'),
  ...loadGrammars('n3'),
  ...loadGrammars('n4'),
  ...loadGrammars('n5'),
];
