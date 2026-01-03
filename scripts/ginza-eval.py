#!/usr/bin/env python3
"""
Evaluate GiNZA tokenization against Ichiran's segmentation test cases.
"""
import json
import sys
from pathlib import Path

# Must import ginza before spacy to register its components
import ginza
import spacy

nlp = spacy.load("ja_ginza")

# Load test data
test_data_path = Path(__file__).parent.parent / "packages/core/tests/data/segmentation.json"
with open(test_data_path, "r", encoding="utf-8") as f:
    test_cases = json.load(f)

# Run evaluation
results = {
    "total": len(test_cases),
    "exact_match": 0,
    "token_count_match": 0,
    "mismatches": [],
}

for i, tc in enumerate(test_cases):
    input_text = tc["input"]
    expected = tc["expected"]
    
    # GiNZA tokenization
    doc = nlp(input_text)
    ginza_tokens = [token.text for token in doc]
    
    # Filter out :gap from expected for fair comparison (GiNZA doesn't produce gaps)
    expected_filtered = [t for t in expected if t != ":gap"]
    
    # Compare
    exact_match = ginza_tokens == expected_filtered
    token_count_match = len(ginza_tokens) == len(expected_filtered)
    
    if exact_match:
        results["exact_match"] += 1
    if token_count_match:
        results["token_count_match"] += 1
    
    if not exact_match:
        results["mismatches"].append({
            "input": input_text,
            "expected": expected_filtered,
            "ginza": ginza_tokens,
            "expected_len": len(expected_filtered),
            "ginza_len": len(ginza_tokens),
        })

# Print summary
print(f"\n{'='*60}")
print(f"GiNZA Tokenization Evaluation vs Ichiran")
print(f"{'='*60}")
print(f"Total test cases: {results['total']}")
print(f"Exact matches:    {results['exact_match']} ({100*results['exact_match']/results['total']:.1f}%)")
print(f"Token count matches: {results['token_count_match']} ({100*results['token_count_match']/results['total']:.1f}%)")
print(f"Mismatches:       {len(results['mismatches'])}")

# Show first N mismatches as examples
print(f"\n{'='*60}")
print(f"Sample mismatches (first 30):")
print(f"{'='*60}")
for mm in results["mismatches"][:30]:
    print(f"\nInput: {mm['input']}")
    print(f"  Expected ({mm['expected_len']}): {mm['expected']}")
    print(f"  GiNZA    ({mm['ginza_len']}): {mm['ginza']}")

# Save full results to JSON
output_path = Path(__file__).parent / "ginza-eval-results.json"
with open(output_path, "w", encoding="utf-8") as f:
    json.dump(results, f, ensure_ascii=False, indent=2)
print(f"\nFull results saved to: {output_path}")

