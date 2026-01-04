import { useSharedEngine } from './src/rules/bunpro/_test/engine.ts';

const engine = useSharedEngine([])();

const sentences = [
  '食べられないことはないが、あまり美味しくない。',
  '行けないことはないよ。',
  'できないことはない。',
  'あぶなくないことはない。',
  'きれないことはない。',
];

for (const s of sentences) {
  console.log('\n=== ' + s + ' ===');
  const result = await engine.analyze(s);
  console.log(JSON.stringify(result, null, 2));
}
