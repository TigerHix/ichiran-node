// Test a specific sentence
import { readFileSync } from 'fs';
const data = JSON.parse(readFileSync('./data/bunpro/JLPT3/という-called.json', 'utf-8'));

// Find the failing sentence
const failing = data.included.find(item => 
  item.type === 'study_question' && 
  item.attributes.content.includes('強いという')
);

console.log('Failing sentence:', failing.attributes.content);
console.log('Translation:', failing.attributes.translation);
