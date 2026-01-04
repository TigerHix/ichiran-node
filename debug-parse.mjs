// Simple script to check how sentences are tokenized
const sentences = [
  'この猫はまん丸でかわいい。',
  'まん中にある本をとってください。',
  '体がまっ赤になるくらい お風呂が熱かった。',
  '道が雪でまっ白になった。',
  '真っ直ぐに行ってください。',
];

sentences.forEach(s => {
  console.log('\n===', s, '===');
  // Just check what words we see
  const words = s.replace(/[がをにでとは・、。]/g, ' ').split(/\s+/).filter(w => w);
  console.log('Words:', words);
});
