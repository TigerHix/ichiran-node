// Quick test to understand the structure
const testCases = [
  'していては',
  '食べていては',
  '遊んでいては',
  '太っていては',
  '見られていては',
];

testCases.forEach(test => {
  console.log(test);
  // Break down the structure
  // verb-te form + いて + は
  const teMatch = test.match(/^(.+て)(いては)$/);
  if (teMatch) {
    console.log('  verb-te:', teMatch[1]);
    console.log('  suffix:', teMatch[2]);
  }
});
