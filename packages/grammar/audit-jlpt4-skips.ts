/**
 * JLPT4 SkipPositives Audit Script
 *
 * Audits all skipPositives in JLPT4 test files to identify illegitimate skips
 */

import { readFileSync, readdirSync } from 'fs';
import { join } from 'path';

// New rules to exclude from audit
const NEW_RULES = [
  'ようにいう', 'ようにいのる', 'ようにする', 'ようになる',
  'よていだ', 'より', 'ら', 'らしい1', 'らしい2', 'るところだ',
  'んだけど-んですが', '代', '以上1', '化する', '各',
  '命令形', '真(っ)', '聞こえる', '見える', '風'
];

interface SkipEntry {
  rule: string;
  sentence: string;
  reason: string;
  testFile: string;
}

function extractSkipPositives(testFile: string): SkipEntry[] {
  const content = readFileSync(testFile, 'utf-8');
  const skips: SkipEntry[] = [];

  const ruleName = testFile.split('/').pop()?.replace('.test.ts', '') || '';

  // Find skipPositives array and extract comment before it
  const skipMatch = content.match(/const skipPositives\s*=\s*\[([\s\S]*?)\];/);
  if (!skipMatch) return skips;

  const arrayContent = skipMatch[1];

  // Extract sentences from the array
  const sentenceRegex = /['"`]([^'"`]+)['"`]/g;
  const sentences: string[] = [];
  let match;
  while ((match = sentenceRegex.exec(arrayContent)) !== null) {
    sentences.push(match[1]);
  }

  // Extract the comment block before skipPositives (contains reason)
  const beforeSkips = content.substring(0, skipMatch.index);
  const commentMatch = beforeSkips.match(/\/\*[\s\S]*?\*\/|\/\/.*$/gm);
  let reason = 'No reason provided';

  // Get lines immediately before skipPositives for reason
  const linesBefore = beforeSkips.split('\n').slice(-10).join('\n');
  const commentLines = linesBefore.split('\n').filter(line =>
    line.trim().startsWith('//') || line.trim().startsWith('*')
  );

  if (commentLines.length > 0) {
    reason = commentLines
      .map(line => line.replace(/^\s*\/\/\s?|^\s*\*\s?/, '').trim())
      .filter(line => line.length > 0)
      .join(' ');
  }

  for (const sentence of sentences) {
    skips.push({
      rule: ruleName,
      sentence,
      reason,
      testFile
    });
  }

  return skips;
}

function main() {
  const testDir = '/home/tiger/ichiran-node/packages/grammar/src/rules/bunpro/jlpt4';
  const allFiles = readdirSync(testDir).filter(f => f.endsWith('.test.ts'));

  // Filter out new rules
  const oldRuleFiles = allFiles.filter(file => {
    const ruleName = file.replace('.test.ts', '');
    return !NEW_RULES.includes(ruleName);
  }).map(f => join(testDir, f));

  console.log(`Auditing ${oldRuleFiles.length} old JLPT4 rules...\n`);

  const allSkips: SkipEntry[] = [];

  for (const file of oldRuleFiles) {
    const skips = extractSkipPositives(file);
    if (skips.length > 0) {
      allSkips.push(...skips);
    }
  }

  console.log('=== ALL SKIPPOSITIVES FOUND ===\n');
  console.log(`Total skips found: ${allSkips.length}\n`);

  // Group by rule
  const byRule = new Map<string, SkipEntry[]>();
  for (const skip of allSkips) {
    if (!byRule.has(skip.rule)) {
      byRule.set(skip.rule, []);
    }
    byRule.get(skip.rule)!.push(skip);
  }

  // Print summary by rule
  console.log('=== SKIPS BY RULE ===\n');
  for (const [rule, skips] of byRule.entries()) {
    console.log(`${rule}: ${skips.length} skip(s)`);
    for (const skip of skips) {
      console.log(`  - "${skip.sentence}"`);
      console.log(`    Reason: ${skip.reason}`);
    }
    console.log('');
  }

  // Save to file for detailed analysis
  const outputData = {
    totalRules: byRule.size,
    totalSkips: allSkips.length,
    skipsByRule: Object.fromEntries(byRule)
  };

  require('fs').writeFileSync(
    '/home/tiger/ichiran-node/packages/grammar/skip-audit-raw.json',
    JSON.stringify(outputData, null, 2)
  );

  console.log('\nRaw data saved to skip-audit-raw.json');
}

main();
