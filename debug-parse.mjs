// Quick parse test using the existing test infrastructure
const { spawn } = require('child_process');

const testCode = `
import { describe } from 'bun:test';
import { useSharedEngine } from './packages/grammar/src/rules/bunpro/_test/engine.js';
import { BUNPRO_JLPT3 } from './packages/grammar/src/rules/bunpro/jlpt3/index.js';

describe('debug-parse', () => {
  const engine = useSharedEngine([BUNPRO_JLPT3]);
  
  it('should parse sentence', async () => {
    const e = await engine.get();
    const doc = await e.analyze('面倒くさくても朝ご飯を食べることだ。');
    console.log('\\nTokens:');
    doc.tokens.forEach((t, i) => {
      console.log(\`\${i}: \${t.text} (lemma=\${t.lemma}, pos=\${t.pos}, inflection=\${t.inflectionForm})\`);
    });
  });
});
`;

require('fs').writeFileSync('/tmp/debug-parse.test.ts', testCode);
`;

const proc = spawn('bun', ['test', '/tmp/debug-parse.test.ts'], {
  cwd: '/home/tiger/ichiran-node',
  stdio: 'pipe'
});

let output = '';
proc.stdout.on('data', (d) => { output += d.toString(); });
proc.stderr.on('data', (d) => { output += d.toString(); });
proc.on('close', () => { 
  console.log(output);
});
