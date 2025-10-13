#!/usr/bin/env tsx
/**
 * Script to analyze database queries made during romanizeStar() segmentation
 * This will help determine what data is needed for pure segmentation vs gloss lookup
 */

import { romanizeStar } from './packages/core/src/romanize.js';
import { 
  enableQueryLogging, 
  disableQueryLogging, 
  getQueryLog, 
  resetQueryCount,
  getConnection 
} from './packages/core/src/conn.js';

const tableRegex = /(?:FROM|JOIN|INTO|UPDATE)\s+([a-z_]+)/gi;

function extractTables(query: string): Set<string> {
  const tables = new Set<string>();
  let match;
  const regex = new RegExp(tableRegex.source, tableRegex.flags);
  while ((match = regex.exec(query)) !== null) {
    tables.add(match[1]);
  }
  return tables;
}

// Test strings of varying complexity
const testStrings = [
  '食べる',           // Simple verb
  '食べました',       // Conjugated verb
  '美味しい食べ物',   // Adjective + noun
  'これは本です',     // Full sentence
  '行ってきます',     // Compound verb with て-form
];

async function analyzeQueries() {
  console.log('='.repeat(80));
  console.log('DATABASE QUERY ANALYSIS FOR romanizeStar()');
  console.log('='.repeat(80));
  console.log();

  // Initialize connection first
  const conn = getConnection();
  
  const allQueryLogs: Array<{
    testStr: string;
    queryLog: Array<{query: string; params: any[]; timestamp: number}>;
    timeMs: number;
  }> = [];

  for (const testStr of testStrings) {
    console.log(`\nAnalyzing: "${testStr}"`);
    console.log('-'.repeat(80));
    
    resetQueryCount();
    enableQueryLogging();
    
    const startTime = Date.now();
    
    try {
      await romanizeStar(testStr, { limit: 5 });
      const endTime = Date.now();
      const timeMs = endTime - startTime;
      
      const queryLog = getQueryLog();
      allQueryLogs.push({ testStr, queryLog, timeMs });
      
      console.log(`Time: ${timeMs}ms`);
      console.log(`Total queries: ${queryLog.length}`);
      
      // Analyze tables accessed
      const allTables = new Set<string>();
      const tableAccessCount = new Map<string, number>();
      
      for (const log of queryLog) {
        const tables = extractTables(log.query);
        for (const table of tables) {
          allTables.add(table);
          tableAccessCount.set(table, (tableAccessCount.get(table) || 0) + 1);
        }
      }
      
      console.log('\nTables accessed:');
      const sortedTables = Array.from(tableAccessCount.entries())
        .sort((a, b) => b[1] - a[1]);
      
      for (const [table, count] of sortedTables) {
        console.log(`  ${table.padEnd(25)} ${count} queries`);
      }
      
      // Categorize queries
      const segmentationTables = new Set(['kana_text', 'kanji_text', 'entry', 'conjugation', 'conj_prop', 'conj_source_reading']);
      const glossTables = new Set(['sense', 'gloss', 'sense_prop', 'meaning']);
      const otherTables = new Set(['reading', 'okurigana', 'kanji', 'restricted_readings']);
      
      let segmentationQueries = 0;
      let glossQueries = 0;
      let otherQueries = 0;
      
      for (const log of queryLog) {
        const tables = extractTables(log.query);
        let isSegmentation = false;
        let isGloss = false;
        
        for (const table of tables) {
          if (segmentationTables.has(table)) isSegmentation = true;
          if (glossTables.has(table)) isGloss = true;
        }
        
        if (isSegmentation) segmentationQueries++;
        if (isGloss) glossQueries++;
        if (!isSegmentation && !isGloss) otherQueries++;
      }
      
      console.log('\nQuery categorization:');
      console.log(`  Segmentation queries:     ${segmentationQueries}`);
      console.log(`  Gloss/meaning queries:    ${glossQueries}`);
      console.log(`  Other queries:            ${otherQueries}`);
      
      disableQueryLogging();
      
    } catch (error) {
      console.error('Error:', error);
      disableQueryLogging();
    }
  }
  
  console.log('\n' + '='.repeat(80));
  console.log('SUMMARY');
  console.log('='.repeat(80));
  
  // Overall table usage across all tests
  const overallTables = new Map<string, number>();
  
  for (const {queryLog} of allQueryLogs) {
    for (const log of queryLog) {
      const tables = extractTables(log.query);
      for (const table of tables) {
        overallTables.set(table, (overallTables.get(table) || 0) + 1);
      }
    }
  }
  
  console.log('\nOverall table access across all tests:');
  const sorted = Array.from(overallTables.entries())
    .sort((a, b) => b[1] - a[1]);
  
  for (const [table, count] of sorted) {
    console.log(`  ${table.padEnd(25)} ${count} accesses`);
  }
  
  await conn.end();
}

analyzeQueries().catch(console.error);

