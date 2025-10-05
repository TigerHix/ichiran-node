import * as fs from 'fs';

const sourceFile = '/home/tiger/ichiran-node/tmp/hanabira.org-japanese-content/grammar_json/grammar_ja_N1_full_alphabetical_0001.json';
const sourceData = JSON.parse(fs.readFileSync(sourceFile, 'utf-8'));

// Find entries with empty or problematic titles
for (let i = 0; i < sourceData.length; i++) {
  const item = sourceData[i];
  const title = item.title || '';

  // Check if title would result in empty kebab-case
  const romajiMatch = title.match(/\(([^)]+)\)/);
  const name = romajiMatch ? romajiMatch[1] : title;

  if (!name || name.trim() === '') {
    console.log(`Index ${i}:`, JSON.stringify(item, null, 2));
  }
}
