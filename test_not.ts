import { linguisticRule } from './src/engine/lang.js';

// Try to use not()
const rule = linguisticRule('test-not', (r) => {
  const a = r.tok({ text: 'A' });
  const b = r.tok({ text: 'B' });
  
  r.inOrder(a, b);
  
  // Try to use not - this should fail to compile
  r.optional((opt) => {
    const c = opt.tok({ text: 'C' });
    opt.inOrder(b, c);
    
    // Not available in high-level API
    // opt.not(...)
  });
});

console.log(JSON.stringify(rule, null, 2));
