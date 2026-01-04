import { ginzaTokenize } from './packages/grammar/src/ginza/client.ts';

const text = '乾杯と言わないで、のみだした。';
const tokens = await ginzaTokenize(text);
console.log(JSON.stringify(tokens, null, 2));
