// Quick test to see how GiNZA parses this
const sentence = '禁煙区域でタバコをすおうとする人が多いので';
// Tokenize manually to understand the structure
const tokens = [
  '禁煙', '区域', 'で', 'タバコ', 'を', 'すおう', 'と', 'する', '人', 'が', '多い', 'ので'
];
console.log('Tokens:', tokens);
console.log('"すおう" should be at index 5');
console.log('Looking for volitional form with 意志推量形');
