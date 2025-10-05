#!/usr/bin/env node

import { readFileSync, writeFileSync, readdirSync, statSync } from 'fs';
import { join, dirname } from 'path';
import { fileURLToPath } from 'url';

const __filename = fileURLToPath(import.meta.url);
const __dirname = dirname(__filename);
const rootDir = join(__dirname, '..');

// Recursively find all .json files
function findJsonFiles(dir, files = []) {
  const entries = readdirSync(dir);
  for (const entry of entries) {
    const fullPath = join(dir, entry);
    const stat = statSync(fullPath);
    if (stat.isDirectory()) {
      findJsonFiles(fullPath, files);
    } else if (entry.endsWith('.json')) {
      files.push(fullPath);
    }
  }
  return files;
}

// Compact JSON stringification for pattern nodes
function compactStringify(obj, depth = 0) {
  if (obj === null || obj === undefined) return JSON.stringify(obj);
  if (typeof obj !== 'object') return JSON.stringify(obj);
  
  if (Array.isArray(obj)) {
    if (obj.length === 0) return '[]';
    // For arrays of strings/primitives, keep on one line
    if (obj.every(item => typeof item !== 'object')) {
      return JSON.stringify(obj);
    }
    // For arrays of objects, format compactly
    const items = obj.map(item => compactStringify(item, depth + 1));
    return `[${items.join(', ')}]`;
  }
  
  const keys = Object.keys(obj);
  if (keys.length === 0) return '{}';
  
  // Special compact formatting for common pattern nodes
  if (keys.length === 1) {
    const key = keys[0];
    const value = obj[key];
    
    // Single-key objects on one line
    if (key === 'token' && Array.isArray(value)) {
      return `{ "token": ${JSON.stringify(value)} }`;
    }
    if (key === 'macro') {
      return `{ "macro": ${JSON.stringify(value)} }`;
    }
    if (key === 'anchor') {
      return `{ "anchor": ${JSON.stringify(value)} }`;
    }
    if (key === 'peek') {
      return `{ "peek": ${compactStringify(value, depth + 1)} }`;
    }
    if (key === 'not') {
      return `{ "not": ${compactStringify(value, depth + 1)} }`;
    }
    if (key === 'optional') {
      return `{ "optional": ${compactStringify(value, depth + 1)} }`;
    }
  }
  
  // For sequence with 2 items, try one line
  if (keys.includes('sequence') && keys.length === 1) {
    const seq = obj.sequence;
    if (Array.isArray(seq) && seq.length <= 2) {
      const items = seq.map(item => compactStringify(item, depth + 1));
      return `{ "sequence": [${items.join(', ')}] }`;
    }
  }
  
  // For alt with 2 items, try one line
  if (keys.includes('alt') && keys.length === 1) {
    const alt = obj.alt;
    if (Array.isArray(alt) && alt.length === 2) {
      const items = alt.map(item => compactStringify(item, depth + 1));
      return `{ "alt": [${items.join(', ')}] }`;
    }
  }
  
  // For repeat, format compactly
  if (keys.includes('repeat') && keys.length === 1) {
    const rep = obj.repeat;
    const parts = [];
    parts.push(`"pattern": ${compactStringify(rep.pattern, depth + 1)}`);
    if (rep.min !== undefined) parts.push(`"min": ${rep.min}`);
    if (rep.max !== undefined) parts.push(`"max": ${rep.max}`);
    if (rep.greedy !== undefined) parts.push(`"greedy": ${rep.greedy}`);
    if (rep.until !== undefined) parts.push(`"until": ${compactStringify(rep.until, depth + 1)}`);
    return `{ "repeat": { ${parts.join(', ')} } }`;
  }
  
  // For capture, format on one line if possible
  if (keys.includes('capture') && keys.includes('pattern') && keys.length === 2) {
    const label = JSON.stringify(obj.capture);
    const pattern = compactStringify(obj.pattern, depth + 1);
    return `{ "capture": ${label}, "pattern": ${pattern} }`;
  }
  
  // Default: use standard JSON
  const pairs = keys.map(key => {
    const value = obj[key];
    return `${JSON.stringify(key)}: ${compactStringify(value, depth + 1)}`;
  });
  return `{ ${pairs.join(', ')} }`;
}

// Remove unnecessary optional wrapping
function unwrapUnnecessaryOptional(pattern) {
  if (!pattern || typeof pattern !== 'object') return pattern;
  
  // Recursively process children first
  if (pattern.sequence) {
    pattern.sequence = pattern.sequence.map(unwrapUnnecessaryOptional);
  }
  if (pattern.alt) {
    pattern.alt = pattern.alt.map(unwrapUnnecessaryOptional);
  }
  if (pattern.optional) {
    pattern.optional = unwrapUnnecessaryOptional(pattern.optional);
  }
  if (pattern.repeat) {
    pattern.repeat.pattern = unwrapUnnecessaryOptional(pattern.repeat.pattern);
    if (pattern.repeat.until) {
      pattern.repeat.until = unwrapUnnecessaryOptional(pattern.repeat.until);
    }
  }
  if (pattern.capture) {
    pattern.pattern = unwrapUnnecessaryOptional(pattern.pattern);
  }
  if (pattern.peek) {
    pattern.peek = unwrapUnnecessaryOptional(pattern.peek);
  }
  if (pattern.not) {
    pattern.not = unwrapUnnecessaryOptional(pattern.not);
  }
  
  return pattern;
}

// Remove useless optional at start of sequence
function removeUselessOptionalAtStart(pattern) {
  if (!pattern || typeof pattern !== 'object') return pattern;
  
  // Top-level sequence with optional at start
  if (pattern.sequence && Array.isArray(pattern.sequence)) {
    // If the entire sequence is just one optional, that's useless at top level
    if (pattern.sequence.length === 1 && pattern.sequence[0].optional) {
      // This whole pattern is optional, which is useless at grammar top level
      // Keep it but flag for review
    }
    
    // Process each element
    pattern.sequence = pattern.sequence.map(removeUselessOptionalAtStart);
  }
  
  if (pattern.alt) {
    pattern.alt = pattern.alt.map(removeUselessOptionalAtStart);
  }
  
  if (pattern.optional) {
    pattern.optional = removeUselessOptionalAtStart(pattern.optional);
  }
  
  if (pattern.repeat) {
    if (pattern.repeat.pattern) {
      pattern.repeat.pattern = removeUselessOptionalAtStart(pattern.repeat.pattern);
    }
    if (pattern.repeat.until) {
      pattern.repeat.until = removeUselessOptionalAtStart(pattern.repeat.until);
    }
  }
  
  if (pattern.capture) {
    pattern.pattern = removeUselessOptionalAtStart(pattern.pattern);
  }
  
  if (pattern.peek) {
    pattern.peek = removeUselessOptionalAtStart(pattern.peek);
  }
  
  if (pattern.not) {
    pattern.not = removeUselessOptionalAtStart(pattern.not);
  }
  
  return pattern;
}

// Remove useless optional/repeat at end with min:0
function removeUselessOptionalAtEnd(pattern) {
  if (!pattern || typeof pattern !== 'object') return pattern;
  
  if (pattern.sequence && Array.isArray(pattern.sequence) && pattern.sequence.length > 0) {
    // Process children first
    pattern.sequence = pattern.sequence.map(removeUselessOptionalAtEnd);
    
    // Remove trailing optional or repeat with min:0 that just match auxiliaries or similar
    const last = pattern.sequence[pattern.sequence.length - 1];
    if (last && (last.optional || (last.repeat && (last.repeat.min === 0 || last.repeat.min === undefined)))) {
      // Check if it's matching common trailing elements that are better left implicit
      const isTrailingAux = JSON.stringify(last).includes('isAuxiliary');
      if (isTrailingAux) {
        // Keep it but note it's questionable - auxiliaries are often meaningful
      }
    }
  }
  
  if (pattern.alt) {
    pattern.alt = pattern.alt.map(removeUselessOptionalAtEnd);
  }
  
  if (pattern.optional) {
    pattern.optional = removeUselessOptionalAtEnd(pattern.optional);
  }
  
  if (pattern.repeat) {
    if (pattern.repeat.pattern) {
      pattern.repeat.pattern = removeUselessOptionalAtEnd(pattern.repeat.pattern);
    }
    if (pattern.repeat.until) {
      pattern.repeat.until = removeUselessOptionalAtEnd(pattern.repeat.until);
    }
  }
  
  if (pattern.capture) {
    pattern.pattern = removeUselessOptionalAtEnd(pattern.pattern);
  }
  
  if (pattern.peek) {
    pattern.peek = removeUselessOptionalAtEnd(pattern.peek);
  }
  
  if (pattern.not) {
    pattern.not = removeUselessOptionalAtEnd(pattern.not);
  }
  
  return pattern;
}

async function processGrammarFile(filePath) {
  try {
    const content = readFileSync(filePath, 'utf-8');
    const grammar = JSON.parse(content);
    
    if (!grammar.pattern) return;
    
    let modified = false;
    let pattern = grammar.pattern;
    
    // Remove useless optionals
    const cleaned = removeUselessOptionalAtEnd(removeUselessOptionalAtStart(pattern));
    
    // Check for leading optional in top-level sequence
    if (cleaned.sequence && Array.isArray(cleaned.sequence) && cleaned.sequence.length > 0) {
      const first = cleaned.sequence[0];
      // Remove optional at start if it's just topic/NP that doesn't add value
      if (first.optional) {
        const optStr = JSON.stringify(first.optional);
        if (optStr.includes('isTopicMarker') || optStr.includes('"NP"')) {
          // This is likely matching optional context that's not part of the core pattern
          // Remove it
          cleaned.sequence = cleaned.sequence.slice(1);
          modified = true;
        }
      }
    }
    
    // Check for trailing optional auxiliaries
    if (cleaned.sequence && Array.isArray(cleaned.sequence) && cleaned.sequence.length > 0) {
      const last = cleaned.sequence[cleaned.sequence.length - 1];
      if (last.optional || last.repeat) {
        const lastStr = JSON.stringify(last);
        if (lastStr.includes('isAuxiliary')) {
          // Remove trailing optional auxiliary matching - it's often useless
          cleaned.sequence = cleaned.sequence.slice(0, -1);
          modified = true;
        }
      }
    }
    
    grammar.pattern = cleaned;
    
    // Rewrite with compact formatting
    const output = {
      ...grammar,
      pattern: JSON.parse(compactStringify(grammar.pattern))
    };
    
    const newContent = JSON.stringify(output, null, 2);
    
    // Only write if actually changed
    if (newContent !== content) {
      writeFileSync(filePath, newContent + '\n');
      console.log(`✓ ${filePath.replace(rootDir + '/', '')}`);
      return true;
    }
    
    return false;
  } catch (error) {
    console.error(`✗ Error processing ${filePath}:`, error.message);
    return false;
  }
}

async function main() {
  const grammarsDir = join(rootDir, 'src', 'grammarMatcher', 'grammars');
  const grammarFiles = findJsonFiles(grammarsDir);
  
  console.log(`Found ${grammarFiles.length} grammar files\n`);
  
  let modified = 0;
  for (const file of grammarFiles) {
    if (await processGrammarFile(file)) {
      modified++;
    }
  }
  
  console.log(`\nModified ${modified} of ${grammarFiles.length} files`);
}

main().catch(console.error);

