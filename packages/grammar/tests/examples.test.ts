import { describe, test, expect } from 'bun:test';
import { matchSentence } from '@ichiran/grammar';
import type { GrammarDefinition } from '@ichiran/grammar';
import { setupTests } from './test-setup.js';
import { readFileSync, readdirSync, statSync } from 'fs';
import { join, dirname } from 'path';
import { fileURLToPath } from 'url';

const __filename = fileURLToPath(import.meta.url);
const __dirname = dirname(__filename);

setupTests();

interface ExampleSentence {
  jp: string;
  en: string;
}

interface GrammarWithExamples extends GrammarDefinition {
  examples?: ExampleSentence[];
  negativeExamples?: ExampleSentence[];
}

/**
 * Load all grammar files from grammars directory and subdirectories
 */
function loadAllGrammars(baseDir: string): GrammarWithExamples[] {
  const grammars: GrammarWithExamples[] = [];
  
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
            const grammar = JSON.parse(content) as GrammarWithExamples;
            grammars.push(grammar);
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

const grammarsPath = join(__dirname, '../src/grammarMatcher/grammars');
const allGrammars = loadAllGrammars(grammarsPath);

describe('Grammar examples validation', () => {
  if (allGrammars.length === 0) {
    test('No grammars found', () => {
      expect(allGrammars.length).toBeGreaterThan(0);
    });
  }
  
  for (const grammar of allGrammars) {
    const hasPattern = grammar.pattern && Object.keys(grammar.pattern).length > 0
      && ((grammar.pattern['sequence'] && grammar.pattern['sequence'].length > 0)
        || (grammar.pattern['alt'] && grammar.pattern['alt'].length > 0));

    if (!hasPattern) {
      test.skip(`${grammar.id} - no pattern defined`, () => {});
      continue;
    }
    
    const hasPositive = grammar.examples && grammar.examples.length > 0;
    const hasNegative = grammar.negativeExamples && grammar.negativeExamples.length > 0;
    
    if (hasPositive) {
      test(`${grammar.id} - positive examples (${grammar.examples!.length})`, async () => {
        const failures: string[] = [];
        
        for (const example of grammar.examples!) {
          try {
            const hits = await matchSentence(example.jp, [grammar]);
            const matched = hits.some(hit => hit.grammarId === grammar.id);
            
            if (!matched) {
              failures.push(`  ❌ ${example.jp}\n     (${example.en})`);
            }
          } catch (error: any) {
            failures.push(`  ❌ ${example.jp}\n     Error: ${error.message}`);
          }
        }
        
        if (failures.length > 0) {
          console.log(`\n❌ ${grammar.id} - Failed positive examples:`);
          console.log(failures.join('\n'));
        } else {
          console.log(`\n✅ ${grammar.id} - All ${grammar.examples!.length} positive examples matched`);
        }
        
        expect(failures.length).toBe(0);
      }, { timeout: 60000 });
    }
    
    if (hasNegative) {
      test(`${grammar.id} - negative examples (${grammar.negativeExamples!.length})`, async () => {
        const failures: string[] = [];
        
        for (const example of grammar.negativeExamples!) {
          try {
            const hits = await matchSentence(example.jp, [grammar]);
            const matched = hits.some(hit => hit.grammarId === grammar.id);
            
            if (matched) {
              // This should NOT have matched
              failures.push(`  ❌ ${example.jp}\n     (${example.en})\n     Incorrectly matched this negative example`);
            }
          } catch (error: any) {
            failures.push(`  ❌ ${example.jp}\n     Error: ${error.message}`);
          }
        }
        
        if (failures.length > 0) {
          console.log(`\n❌ ${grammar.id} - Failed negative examples:`);
          console.log(failures.join('\n'));
        } else {
          console.log(`\n✅ ${grammar.id} - All ${grammar.negativeExamples!.length} negative examples correctly rejected`);
        }
        
        expect(failures.length).toBe(0);
      }, { timeout: 60000 });
    }
    
    if (!hasPositive && !hasNegative) {
      test.skip(`${grammar.id} - no examples defined`, () => {});
    }
  }
});
