import rule from '../src/rules/bunpro/jlpt5/か-or.js';

console.log('Rule ID:', rule.id);
const either = rule.where.find(c => c.kind === 'either');
if (either && either.kind === 'either') {
  console.log('\nBranches:', either.branches.length);
  either.branches.forEach((branch, bi) => {
    console.log(`\nBranch ${bi}:`);
    branch.clauses.forEach((c, ci) => {
      console.log(`  Clause ${ci}: kind=${c.kind}`);
      if (c.kind === 'node') {
        console.log(`    preds:`, c.preds.map(p => p.kind));
      } else if (c.kind === 'not') {
        console.log(`    inner kind:`, c.clause.kind);
      }
    });
  });
}
