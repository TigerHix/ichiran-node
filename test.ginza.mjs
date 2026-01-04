// Simple test to see how GiNZA parses sentences
import { readFileSync } from 'fs';

// Mock the ginza module to print parse info
const sentence = "来週出かけるとしたらどこに行きたい？";
console.log("Testing:", sentence);

// We'll look at existing test output to understand the pattern
console.log("\nLooking at test errors:");
console.log("- 'to' particle is found at token index 2");
console.log("- 'shitara' (したら) is NOT found as a single token");
console.log("\nThis suggests したら is parsed as multiple tokens:");
console.log("Possible split: し (shi) + た (ta) + ら (ra)");
console.log("Or: した (shita) + ら (ra)");
