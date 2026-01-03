#!/usr/bin/env python3
"""
Deeper analysis of GiNZA vs Ichiran tokenization differences.
"""
import json
from pathlib import Path
from collections import Counter

# Load results
results_path = Path(__file__).parent / "ginza-eval-results.json"
with open(results_path, "r", encoding="utf-8") as f:
    results = json.load(f)

mismatches = results["mismatches"]

# Categorize mismatches
categories = {
    "over_segmented": [],      # GiNZA produces more tokens
    "under_segmented": [],     # GiNZA produces fewer tokens (rare)
    "same_count_diff": [],     # Same count but different boundaries
}

over_seg_patterns = Counter()
under_seg_patterns = Counter()

for mm in mismatches:
    diff = mm["ginza_len"] - mm["expected_len"]
    if diff > 0:
        categories["over_segmented"].append(mm)
        # Analyze what got split
        expected_set = set(mm["expected"])
        ginza_set = set(mm["ginza"])
        # Find tokens in expected that were split
        for exp_tok in mm["expected"]:
            if exp_tok not in ginza_set and len(exp_tok) > 1:
                # This token was likely split
                over_seg_patterns[exp_tok] += 1
    elif diff < 0:
        categories["under_segmented"].append(mm)
    else:
        categories["same_count_diff"].append(mm)

# Analyze token length differences
len_diffs = [mm["ginza_len"] - mm["expected_len"] for mm in mismatches]
avg_over = sum(d for d in len_diffs if d > 0) / max(1, len([d for d in len_diffs if d > 0]))

# Find common split patterns
# Auxiliaries and suffixes that GiNZA splits
aux_splits = Counter()
for mm in categories["over_segmented"]:
    ginza = mm["ginza"]
    for i, tok in enumerate(ginza):
        if tok in ['て', 'た', 'ない', 'ます', 'ません', 'でしょう', 'だ', 'です', 'な', 'の', 'に', 'ば', 'たい', 'てる', 'ちゃう', 'じゃう', 'くれ', 'もらう', 'あげる', 'し', 'さ', 'ぬ', 'ん']:
            aux_splits[tok] += 1

# Check for honorific prefix splits
honorific_splits = 0
for mm in categories["over_segmented"]:
    if mm["ginza"][0] in ['ご', 'お'] and len(mm["ginza"]) > len(mm["expected"]):
        honorific_splits += 1

# Compound expression analysis
compound_splits = 0
compound_examples = []
for mm in mismatches:
    for exp in mm["expected"]:
        if len(exp) >= 4 and exp not in mm["ginza"]:
            compound_splits += 1
            if len(compound_examples) < 20:
                compound_examples.append((exp, mm["input"]))
            break

print("=" * 70)
print("DETAILED ANALYSIS: GiNZA vs Ichiran Tokenization")
print("=" * 70)

print(f"\n## SUMMARY STATISTICS")
print(f"Total test cases: {results['total']}")
print(f"Exact matches: {results['exact_match']} ({100*results['exact_match']/results['total']:.1f}%)")
print(f"Mismatches: {len(mismatches)} ({100*len(mismatches)/results['total']:.1f}%)")

print(f"\n## MISMATCH BREAKDOWN")
print(f"Over-segmented (GiNZA > Ichiran): {len(categories['over_segmented'])} ({100*len(categories['over_segmented'])/len(mismatches):.1f}%)")
print(f"Under-segmented (GiNZA < Ichiran): {len(categories['under_segmented'])} ({100*len(categories['under_segmented'])/len(mismatches):.1f}%)")
print(f"Same count, different splits: {len(categories['same_count_diff'])} ({100*len(categories['same_count_diff'])/len(mismatches):.1f}%)")
print(f"Average extra tokens when over-segmented: {avg_over:.1f}")

print(f"\n## AUXILIARY/SUFFIX SPLITS (GiNZA separates these)")
for tok, count in aux_splits.most_common(15):
    print(f"  '{tok}': {count} times")

print(f"\n## HONORIFIC PREFIX SPLITS")
print(f"Cases where ご/お was split from word: {honorific_splits}")

print(f"\n## COMPOUND EXPRESSIONS SPLIT")
print(f"Total compound expressions broken up: {compound_splits}")
print(f"Examples of expressions Ichiran keeps together:")
for exp, inp in compound_examples[:15]:
    print(f"  '{exp}' in: {inp}")

# Specific pattern analysis
print(f"\n## SPECIFIC PATTERN ANALYSIS")

# Te-form compounds
te_compounds = sum(1 for mm in mismatches if any('て' in g for g in mm["ginza"]) and 
                   any(len(e) > 2 and 'て' in e for e in mm["expected"]))
print(f"Te-form compounds split: {te_compounds}")

# Negative forms
neg_splits = sum(1 for mm in mismatches if 'ない' in mm["ginza"] and 
                 any('ない' in e and len(e) > 2 for e in mm["expected"]))
print(f"Negative form splits: {neg_splits}")

# Conditional forms
cond_splits = sum(1 for mm in mismatches if any(g in ['ば', 'たら', 'なら'] for g in mm["ginza"]))
print(f"Conditional form splits: {cond_splits}")

# Desire/volition
desire_splits = sum(1 for mm in mismatches if 'たい' in mm["ginza"])
print(f"Desire (-tai) form splits: {desire_splits}")

print(f"\n## UNDER-SEGMENTED EXAMPLES (GiNZA merged what Ichiran split)")
for mm in categories["under_segmented"][:10]:
    print(f"  Input: {mm['input']}")
    print(f"    Expected: {mm['expected']}")
    print(f"    GiNZA:    {mm['ginza']}")

