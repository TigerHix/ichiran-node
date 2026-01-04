import * as compiler from './dist/engine/compiler.js';
const keys = Object.keys(compiler).filter(x => !x.startsWith('_'));
console.log('Exports:', keys);
