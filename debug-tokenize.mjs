// Simple tokenizer test
const sentence = "赤ちゃんが泣き止んだかとおもったら、また大声で泣き始めた。";

// Try to find how "かとおもったら" is tokenized
const target = "かとおもったら";
const idx = sentence.indexOf(target);

if (idx !== -1) {
  console.log("Found 'かとおもったら' at index:", idx);
  console.log("Substring:", sentence.substring(idx, idx + target.length));
  console.log("Sentence length:", sentence.length);
  console.log("Target length:", target.length);
} else {
  console.log("NOT FOUND as substring");

  // Try character by character
  for (let i = 0; i < sentence.length; i++) {
    if (sentence[i] === 'か') {
      console.log(`Found 'か' at position ${i}: "${sentence.substring(i, Math.min(i + 10, sentence.length))}"`);
    }
  }
}

console.log("\nFull sentence:");
console.log(sentence);
