#!/usr/bin/env bun

import { readFileSync, readdirSync, statSync } from 'fs';
import { join } from 'path';
import type { GrammarDefinition } from '../src/grammarMatcher/types.js';

interface GrammarWithPath extends GrammarDefinition {
  filePath: string;
}

/**
 * Load all grammar files from grammars directory and subdirectories
 */
function loadAllGrammars(baseDir: string): GrammarWithPath[] {
  const grammars: GrammarWithPath[] = [];
  
  function scanDirectory(dirPath: string) {
    try {
      const entries = readdirSync(dirPath);
      
      for (const entry of entries) {
        const fullPath = join(dirPath, entry);
        const stat = statSync(fullPath);
        
        if (stat.isDirectory()) {
          // Recursively scan subdirectories
          scanDirectory(fullPath);
        } else if (entry.endsWith('.json')) {
          // Load grammar file
          try {
            const content = readFileSync(fullPath, 'utf-8');
            const grammar = JSON.parse(content) as GrammarDefinition;
            grammars.push({ ...grammar, filePath: fullPath });
          } catch (error) {
            console.error(`Error loading grammar ${fullPath}:`, error);
          }
        }
      }
    } catch (error) {
      console.error(`Error scanning directory ${dirPath}:`, error);
    }
  }
  
  scanDirectory(baseDir);
  return grammars;
}

/**
 * Check if a grammar has a valid pattern definition
 * Based on the validation logic from examples.test.ts
 */
function isValidGrammar(grammar: GrammarDefinition): boolean {
  const hasPattern = grammar.pattern && Object.keys(grammar.pattern).length > 0
    && ((grammar.pattern['sequence'] && grammar.pattern['sequence'].length > 0)
      || (grammar.pattern['alt'] && grammar.pattern['alt'].length > 0));
  
  return hasPattern;
}

function main() {
  const grammarsPath = join(process.cwd(), 'src/grammarMatcher/grammars');
  const allGrammars = loadAllGrammars(grammarsPath);
  
  const validGrammars = allGrammars.filter(isValidGrammar);
  
  // Sort by ID for consistent output
  validGrammars.sort((a, b) => a.id.localeCompare(b.id));
  
  for (const grammar of validGrammars) {
    const grammarWithPath = grammar as GrammarWithPath;
    console.log(`${grammar.id} - ${grammarWithPath.filePath}`);
  }
}

if (import.meta.main) {
  main();
}
