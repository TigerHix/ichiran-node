/**
 * JLPT4 Skip Legitimacy Analysis
 *
 * For each skip, applies the 5-item checklist to determine legitimacy
 */

import { readFileSync, readdirSync } from 'fs';
import { join } from 'path';

interface SkipEntry {
  rule: string;
  sentence: string;
  reason: string;
  testFile: string;
}

interface AnalysisResult {
  rule: string;
  sentence: string;
  currentReason: string;
  classification: 'LEGITIMATE' | 'ILLEGITIMATE' | 'SUSPICIOUS';
  checklistResults: {
    item1: boolean; // Different grammar point exists
    item2: boolean; // Grammar scope includes both forms
    item3: boolean; // Verified GiNZA limitation
    item4: boolean; // Data bug
    item5: boolean; // Different structure
  };
  justification: string;
  recommendedAction?: string;
}

// Check if a separate rule exists
function checkSeparateRule(pattern: string, allRules: string[]): boolean {
  // Common patterns to check
  const patterns = [
    `んだけど-んですが`,
    `かどうか`,
    `だけ-のみ`,
    `しか-ない`,
    `こと`
  ];

  // Check for exact matches or close variants
  return patterns.some(p => allRules.includes(p));
}

// Analyze skip legitimacy
function analyzeSkip(skip: SkipEntry, allRules: string[]): AnalysisResult {
  const { sentence, reason, rule } = skip;
  const lowerReason = reason.toLowerCase();
  const lowerSentence = sentence.toLowerCase();

  const results = {
    item1: false,
    item2: false,
    item3: false,
    item4: false,
    item5: false
  };

  // Item 1: Different grammar point exists
  const separateRulePatterns = [
    { pattern: 'んです', rule: 'んだけど-んですが' },
    { pattern: 'こと が できる', rule: 'ことができる' },
    { pattern: 'かどうか', rule: 'かどうか' },
    { pattern: 'しか', rule: 'しか-ない' },
  ];

  for (const { pattern, rule: ruleName } of separateRulePatterns) {
    if (reason.includes(pattern) || reason.includes(ruleName)) {
      if (allRules.includes(ruleName)) {
        results.item1 = true;
      }
    }
  }

  // Item 2: Grammar scope (check for "polite" or "casual" mentions)
  if (lowerReason.includes('polite') || lowerReason.includes('casual') ||
      lowerReason.includes('register') || lowerReason.includes('variant')) {
    // This is POTENTIALLY illegitimate - need to verify if JSON has both
    results.item2 = true;
  }

  // Item 3: Verified GiNZA limitation
  if (lowerReason.includes('ginza') &&
      (lowerReason.includes('parse') || lowerReason.includes('token') ||
       lowerReason.includes('limitation') || lowerReason.includes('lemma') ||
       lowerReason.includes('pos='))) {
    results.item3 = true;
  }

  // Item 4: Data bug
  if (lowerReason.includes('ungrammatical') || lowerReason.includes('truncated') ||
      lowerReason.includes('malformed') || lowerReason.includes('omitted')) {
    results.item4 = true;
  }

  // Item 5: Different structure
  if (lowerReason.includes('different structure') ||
      lowerReason.includes('different grammar') ||
      lowerReason.includes('different pattern') ||
      lowerReason.includes('abbreviated') && lowerReason.includes('context-dependent')) {
    results.item5 = true;
  }

  // Classify based on checklist
  const legitimateCount = [results.item1, results.item3, results.item4, results.item5]
    .filter(Boolean).length;

  let classification: 'LEGITIMATE' | 'ILLEGITIMATE' | 'SUSPICIOUS';
  let justification = '';
  let recommendedAction: string | undefined;

  if (results.item2 && !results.item1 && !results.item3 && !results.item4 && !results.item5) {
    // Only mentions polite/casual - LIKELY ILLEGITIMATE
    classification = 'ILLEGITIMATE';
    justification = 'Skip reason mentions "polite/casual" but no separate rule exists and no verified GiNZA issue. Grammar rules typically include both registers.';
    recommendedAction = 'Check if rule should handle both polite and casual forms using r.either() or similar';
  } else if (legitimateCount === 1) {
    classification = 'LEGITIMATE';
    if (results.item1) justification = 'Separate grammar rule exists for this pattern.';
    else if (results.item3) justification = 'Verified GiNZA parsing/tokenization limitation.';
    else if (results.item4) justification = 'Data bug in test sentences.';
    else if (results.item5) justification = 'Fundamentally different grammatical structure.';
  } else if (legitimateCount === 0) {
    classification = 'ILLEGITIMATE';
    justification = 'No valid justification found - fails all checklist items.';
    recommendedAction = 'Remove skip or fix rule to handle this sentence';
  } else {
    classification = 'LEGITIMATE';
    justification = `Multiple valid reasons: ${legitimateCount} checklist items passed.`;
  }

  return {
    rule,
    sentence,
    currentReason: reason,
    classification,
    checklistResults: results,
    justification,
    recommendedAction
  };
}

function main() {
  const rawData = JSON.parse(
    readFileSync('/home/tiger/ichiran-node/packages/grammar/skip-audit-raw.json', 'utf-8')
  );

  const allRules = readdirSync('/home/tiger/ichiran-node/packages/grammar/data/bunpro/JLPT4')
    .filter(f => f.endsWith('.json'))
    .map(f => f.replace('.json', ''));

  console.log(`Analyzing ${rawData.totalSkips} skips across ${rawData.totalRules} rules...\n`);

  const illegitimate: AnalysisResult[] = [];
  const legitimate: AnalysisResult[] = [];
  const suspicious: AnalysisResult[] = [];

  for (const [ruleName, skips] of Object.entries(rawData.skipsByRule)) {
    for (const skip of skips as SkipEntry[]) {
      const analysis = analyzeSkip(skip, allRules);

      if (analysis.classification === 'ILLEGITIMATE') {
        illegitimate.push(analysis);
      } else if (analysis.classification === 'LEGITIMATE') {
        legitimate.push(analysis);
      } else {
        suspicious.push(analysis);
      }
    }
  }

  console.log('=== ILLEGITIMATE SKIPS (NEED FIXING) ===\n');
  for (const skip of illegitimate) {
    console.log(`#### Rule: ${skip.rule}`);
    console.log(`**Sentence**: "${skip.sentence}"`);
    console.log(`**Current reason**: ${skip.currentReason}`);
    console.log(`**Why ILLEGITIMATE**: ${skip.justification}`);
    if (skip.recommendedAction) {
      console.log(`**Recommended action**: ${skip.recommendedAction}`);
    }
    console.log('');
  }

  console.log('\n=== SUMMARY ===\n');
  console.log(`Total skips analyzed: ${rawData.totalSkips}`);
  console.log(`Legitimate: ${legitimate.length}`);
  console.log(`Illegitimate: ${illegitimate.length}`);
  console.log(`Suspicious: ${suspicious.length}`);

  // Save detailed results
  const output = {
    summary: {
      total: rawData.totalSkips,
      legitimate: legitimate.length,
      illegitimate: illegitimate.length,
      suspicious: suspicious.length
    },
    illegitimate,
    legitimate,
    suspicious
  };

  require('fs').writeFileSync(
    '/home/tiger/ichiran-node/packages/grammar/skip-audit-analysis.json',
    JSON.stringify(output, null, 2)
  );

  console.log('\nDetailed analysis saved to skip-audit-analysis.json');
}

main();
