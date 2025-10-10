#!/usr/bin/env bun

import { readFileSync, readdirSync, statSync } from 'fs';
import { join } from 'path';
import type { GrammarDefinition, MatchHit } from '../packages/grammar/src/types.js';
import { matchText } from '../packages/grammar/src/runtime.js';

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
          scanDirectory(fullPath);
        } else if (entry.endsWith('.json')) {
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
 */
function isValidGrammar(grammar: GrammarDefinition): boolean {
  const hasPattern = grammar.pattern && Object.keys(grammar.pattern).length > 0
    && ((grammar.pattern['sequence'] && grammar.pattern['sequence'].length > 0)
      || (grammar.pattern['alt'] && grammar.pattern['alt'].length > 0));
  
  return hasPattern;
}

/**
 * Get a signature for a set of capture labels (sorted for consistency)
 */
function getCaptureSignature(captures: { label: string }[]): string {
  return captures.map(c => c.label).sort().join('|');
}

/**
 * Format a match hit as markdown with labeled captures
 */
function formatMatchMarkdown(hit: MatchHit): string {
  if (!hit.segments || hit.segments.length === 0) {
    return '(no segments)';
  }
  
  const parts: string[] = [];
  for (const segment of hit.segments) {
    if (segment.type === 'capture' && segment.label) {
      parts.push(`[${segment.text}](${segment.label})`);
    } else {
      parts.push(segment.text);
    }
  }
  
  return parts.join('');
}

/**
 * Analyze a grammar to find all unique capture label combinations
 */
async function analyzeGrammar(grammar: GrammarDefinition): Promise<Map<string, MatchHit>> {
  const uniqueCaptureSets = new Map<string, MatchHit>();
  
  // Test existing examples first
  if (grammar.examples && grammar.examples.length > 0) {
    for (const example of grammar.examples) {
      try {
        const matches = await matchText(example.jp, [grammar], { limit: 5 });
        
        for (const match of matches) {
          if (match.grammarId === grammar.id) {
            const signature = getCaptureSignature(match.captures);
            if (!uniqueCaptureSets.has(signature)) {
              uniqueCaptureSets.set(signature, match);
            }
          }
        }
      } catch (error) {
        // Silently continue on match errors
      }
    }
  }
  
  // If no captures found and there are no examples, note it
  if (uniqueCaptureSets.size === 0 && (!grammar.examples || grammar.examples.length === 0)) {
    // Try a minimal test sentence
    const testSentences = [
      'これは本です。',
      '食べます。',
      '行きます。',
      '高いです。',
      '静かです。',
      '学生です。',
    ];
    
    for (const sentence of testSentences) {
      try {
        const matches = await matchText(sentence, [grammar], { limit: 5 });
        for (const match of matches) {
          if (match.grammarId === grammar.id) {
            const signature = getCaptureSignature(match.captures);
            if (!uniqueCaptureSets.has(signature)) {
              uniqueCaptureSets.set(signature, match);
            }
          }
        }
      } catch (error) {
        // Continue
      }
    }
  }
  
  return uniqueCaptureSets;
}

/**
 * Generate a markdown report of all capture label combinations
 */
async function generateReport() {
  const grammarsPath = join(process.cwd(), 'packages/grammar/src/grammars');
  const allGrammars = loadAllGrammars(grammarsPath);
  const validGrammars = allGrammars.filter(isValidGrammar);
  
  // Sort by ID for consistent output
  validGrammars.sort((a, b) => a.id.localeCompare(b.id));
  
  console.log('# Grammar Capture Label Verification\n');
  console.log(`Total grammars analyzed: ${validGrammars.length}\n`);
  
  let grammarsWithCaptures = 0;
  let totalCaptureVariations = 0;
  
  for (const grammar of validGrammars) {
    const captureSets = await analyzeGrammar(grammar);
    
    if (captureSets.size > 0) {
      grammarsWithCaptures++;
      totalCaptureVariations += captureSets.size;
      
      console.log(`## ${grammar.id}`);
      if (grammar.description) {
        console.log(`*${grammar.description}*\n`);
      }
      
      const signatures = Array.from(captureSets.entries());
      
      for (const [signature, hit] of signatures) {
        const captureLabels = hit.captures.map(c => c.label).sort();
        console.log(`**Captures:** ${captureLabels.join(', ') || '(none)'}`);
        console.log(`- ${formatMatchMarkdown(hit)}`);
        console.log();
      }
    }
  }
  
  console.log('---\n');
  console.log(`## Summary`);
  console.log(`- Total grammars: ${validGrammars.length}`);
  console.log(`- Grammars with captures: ${grammarsWithCaptures}`);
  console.log(`- Total capture variations: ${totalCaptureVariations}`);
}

if (import.meta.main) {
  generateReport().catch(console.error);
}

