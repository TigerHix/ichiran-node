import { loadCustomData } from './packages/data/dist/data/load-custom.js';
import { setConnection } from './packages/core/dist/conn.js';

await setConnection({ 
  database: 'jmdict_test',
  host: 'localhost',
  port: 6777,
  user: 'postgres',
  password: 'password'
});

console.log('Attempting to reload :extra (should skip existing entries)...');
await loadCustomData({ types: ['extra'], silent: true });
console.log('✓ Reload completed');

process.exit(0);
