import { execSync } from 'child_process';
import fs from 'fs';

console.log('Running CLI tests and analyzing failures...\n');

const output = execSync(
  'ICHIRAN_TEST_DB_URL="postgresql://postgres:password@localhost:6777/jmdict_test" bun test packages/cli/tests/cli-parity.test.ts 2>&1',
  { encoding: 'utf-8', maxBuffer: 50 * 1024 * 1024 }
);

fs.writeFileSync('cli-test-full-output.txt', output);

// Parse failures
const lines = output.split('\n');
let failCount = 0;
const diffTypes = {
  glossAdded: 0,
  glossRemoved: 0,
  infoChanged: 0,
  seqChanged: 0,
  otherChanges: 0
};

let inDiff = false;
for (const line of lines) {
  if (line.includes('Expected  -')) {
    failCount++;
    inDiff = true;
  }
  
  if (inDiff) {
    if (line.match(/^\+.*"gloss":/)) diffTypes.glossAdded++;
    if (line.match(/^\-.*"gloss":/)) diffTypes.glossRemoved++;
    if (line.match(/^\+.*"info":/)) diffTypes.infoChanged++;
    if (line.match(/^\-.*"seq":/)) diffTypes.seqChanged++;
    
    if (line.includes('(fail)')) {
      inDiff = false;
    }
  }
}

console.log('=== RESULTS ===');
console.log(`Total test cases: ${lines.filter(l => l.includes('expect()')).length}`);
console.log(`Failures: ${lines.filter(l => l.includes('fail')).length}`);
console.log(`Passes: ${lines.filter(l => l.includes('pass')).length}`);

console.log('\n=== DIFF ANALYSIS ===');
console.log(`Gloss additions: ${diffTypes.glossAdded}`);
console.log(`Gloss removals: ${diffTypes.glossRemoved}`);
console.log(`Info field changes: ${diffTypes.infoChanged}`);
console.log(`Seq number changes: ${diffTypes.seqChanged}`);

console.log('\nFull output saved to: cli-test-full-output.txt');
