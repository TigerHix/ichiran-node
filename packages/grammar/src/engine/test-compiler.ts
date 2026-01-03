import { describeClause } from './compiler.ts';
import rule from '../src/rules/bunpro/jlpt5/か-or.js';

console.log('Rule:', rule.id);
console.log('\nClauses:');
rule.where.forEach((c, i) => {
  console.log(i + ': ' + describeClause(c));
});
