// Check how GiNZA parses the negative examples

const negatives = [
  '本を読んでいる。',  // ている - should NOT match
  '窓を開けておく。',  // ておく - should NOT match
  '宿題をしてしまった。',  // てしまった - should NOT match
];

console.log("Checking negative examples:");
console.log("These sentences should NOT match verbて-request");
console.log("");
console.log("Issue: GiNZA parses these differently than expected");
console.log("Need to identify the distinguishing feature");
