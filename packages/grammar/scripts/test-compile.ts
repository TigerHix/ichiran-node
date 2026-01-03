import rule from '../src/rules/bunpro/jlpt5/か-or.js';

console.log('Rule ID:', rule.id);
console.log('\nClauses:');
rule.where.forEach((c, i) => {
  console.log(`  ${i}:`, c);
});
console.log('\nCaptures:');
rule.captures.forEach((c, i) => {
  console.log(`  ${i}:`, c);
});
