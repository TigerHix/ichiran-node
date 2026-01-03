import rule from '../src/rules/bunpro/jlpt5/か-or.js';

const either = rule.where.find(c => c.kind === 'either');
if (either && either.kind === 'either') {
  console.log('Branches: ' + either.branches.length + '\n');
  
  either.branches.forEach((branch, bi) => {
    console.log('--- Branch ' + bi + ' ---');
    branch.clauses.forEach((c, ci) => {
      if (c.kind === 'node') {
        console.log('  Clause ' + ci + ': node (text=' + c.preds.find((p: any) => p.kind === 'text')?.value + ')');
      } else if (c.kind === 'before') {
        console.log('  Clause ' + ci + ': before (maxDist=' + c.maxDistance + ')');
      } else if (c.kind === 'not') {
        const inner = c.clause;
        if (inner.kind === 'node') {
          const text = inner.preds.find((p: any) => p.kind === 'text')?.value;
          const pos = inner.preds.find((p: any) => p.kind === 'pos')?.value;
          console.log('  Clause ' + ci + ': not node (text=' + text + ', pos=' + pos + ')');
        } else if (inner.kind === 'before') {
          console.log('  Clause ' + ci + ': not before (maxDist=' + inner.maxDistance + ')');
        } else {
          console.log('  Clause ' + ci + ': not ' + inner.kind);
        }
      } else {
        console.log('  Clause ' + ci + ': ' + c.kind);
      }
    });
    console.log('  Captures: ' + branch.captures.length);
  });
}
