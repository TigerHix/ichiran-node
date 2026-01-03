#!/usr/bin/env python3
"""
Grammar Detection Evaluation using GiNZA
Tests if we can reliably detect grammar points from Bunpro sentences using
GiNZA's dependency parsing + morphological analysis.
"""
import json
import sys
import os
import re
import time
import random
from pathlib import Path
from collections import defaultdict
import requests

# Import ginza+spacy
import ginza
import spacy
nlp = spacy.load("ja_ginza")

# Configuration
BUNPRO_DIR = Path(__file__).parent.parent / "packages/grammar/data/bunpro"
ICHIRAN_API = "http://0.0.0.0:3000/api"
GRAMMARS_PER_LEVEL = 20
RANDOM_SEED = 42

def clean_html(text):
    """Remove HTML tags and extract plain Japanese text."""
    # Remove all HTML tags
    text = re.sub(r'<[^>]+>', '', text)
    # Remove furigana in parentheses (kanji reading aids)
    text = re.sub(r'（[^）]*）', '', text)
    text = re.sub(r'\([^)]*\)', '', text)
    return text.strip()

def extract_sentence(content, answer):
    """Reconstruct full sentence from cloze content + answer."""
    # Replace the blank placeholder with the answer
    sentence = content.replace('____', answer)
    return clean_html(sentence)

def load_bunpro_grammars(level, count=20):
    """Load grammar points and their study questions from Bunpro data."""
    level_dir = BUNPRO_DIR / level
    if not level_dir.exists():
        print(f"Warning: {level_dir} does not exist")
        return []
    
    files = list(level_dir.glob("*.json"))
    random.seed(RANDOM_SEED)
    random.shuffle(files)
    
    grammars = []
    for f in files[:count]:
        try:
            with open(f, "r", encoding="utf-8") as fp:
                data = json.load(fp)
            
            grammar_info = data["data"]["attributes"]
            grammar_id = grammar_info["slug"]
            grammar_title = grammar_info["title"]
            grammar_meaning = grammar_info["meaning"]
            
            # Extract study questions from included
            sentences = []
            for item in data.get("included", []):
                if item.get("type") == "study_question":
                    attrs = item.get("attributes", {})
                    content = attrs.get("content", "")
                    answer = attrs.get("answer", "")
                    if content and answer:
                        sentence = extract_sentence(content, answer)
                        if sentence and len(sentence) > 3:
                            sentences.append({
                                "sentence": sentence,
                                "answer": answer,
                                "full_content": content,
                            })
            
            if sentences:
                grammars.append({
                    "id": grammar_id,
                    "title": grammar_title,
                    "meaning": grammar_meaning,
                    "level": level,
                    "sentences": sentences[:5],  # Limit to 5 sentences per grammar
                })
        except Exception as e:
            print(f"Error loading {f}: {e}")
    
    return grammars

def analyze_with_ginza(text):
    """Full GiNZA analysis with timing."""
    start = time.perf_counter()
    doc = nlp(text)
    elapsed = time.perf_counter() - start
    
    tokens = []
    for token in doc:
        tokens.append({
            "text": token.text,
            "lemma": token.lemma_,
            "pos": token.pos_,
            "tag": token.tag_,  # Fine-grained POS
            "dep": token.dep_,  # Dependency relation
            "head": token.head.text,
            "head_i": token.head.i,
            "children": [c.text for c in token.children],
        })
    
    return {
        "tokens": tokens,
        "elapsed_ms": elapsed * 1000,
        "token_texts": [t["text"] for t in tokens],
        "dependencies": [(t.text, t.dep_, t.head.text) for t in doc],
    }

def analyze_with_ichiran(text):
    """Ichiran analysis with timing."""
    start = time.perf_counter()
    try:
        resp = requests.post(
            f"{ICHIRAN_API}/segment",
            json={"text": text, "limit": 1},
            timeout=30
        )
        data = resp.json()
        elapsed = time.perf_counter() - start
        
        # Extract tokens from nested structure
        tokens = []
        segments = data.get("segments", [])
        if segments and isinstance(segments[0], list):
            # Navigate: segments[0][0][words]
            for alt in segments:
                if isinstance(alt, list) and len(alt) > 0:
                    words_list = alt[0] if isinstance(alt[0], list) else alt
                    for word in words_list:
                        if isinstance(word, dict):
                            tokens.append({
                                "text": word.get("word", ""),
                                "reading": word.get("reading", ""),
                                "gloss": word.get("gloss", []),
                                "conj": word.get("conj", []),
                            })
                        elif isinstance(word, str):
                            tokens.append({"text": word, "type": "punct"})
                    break  # Only take first alternative
        
        return {
            "tokens": tokens,
            "elapsed_ms": elapsed * 1000,
            "token_texts": [t.get("text", str(t)) for t in tokens],
        }
    except Exception as e:
        return {"tokens": [], "elapsed_ms": 0, "error": str(e), "token_texts": []}

def detect_grammar_pattern_ginza(doc_analysis, grammar_answer):
    """
    Try to detect if a grammar pattern is present using GiNZA tokens.
    Returns confidence score and matched tokens.
    """
    tokens = doc_analysis["tokens"]
    token_texts = doc_analysis["token_texts"]
    
    # Clean the grammar answer for matching
    answer_clean = clean_html(grammar_answer)
    
    # Strategy 1: Direct substring match in token sequence
    joined = "".join(token_texts)
    if answer_clean in joined:
        return {"found": True, "method": "direct_substring", "confidence": 1.0}
    
    # Strategy 2: Check if key morphemes are present in sequence
    # Split answer into likely morphemes
    answer_morphemes = list(answer_clean)
    
    matched = 0
    for m in answer_morphemes:
        if m in joined:
            matched += 1
    
    if matched > 0:
        confidence = matched / len(answer_morphemes)
        return {"found": confidence > 0.5, "method": "morpheme_coverage", "confidence": confidence}
    
    return {"found": False, "method": "none", "confidence": 0}

def main():
    print("=" * 80)
    print("Grammar Detection Evaluation: GiNZA vs Ichiran")
    print("=" * 80)
    
    levels = ["JLPT5", "JLPT4", "JLPT3", "JLPT2", "JLPT1"]
    all_results = []
    
    # Performance stats
    ginza_times = []
    ichiran_times = []
    
    for level in levels:
        print(f"\n{'='*40}")
        print(f"Loading {level}...")
        grammars = load_bunpro_grammars(level, GRAMMARS_PER_LEVEL)
        print(f"Loaded {len(grammars)} grammars with study questions")
        
        level_results = {
            "level": level,
            "grammars": [],
            "detection_success": 0,
            "detection_total": 0,
        }
        
        for gi, grammar in enumerate(grammars):
            grammar_result = {
                "id": grammar["id"],
                "title": grammar["title"],
                "sentences": [],
            }
            
            # Sample up to 3 sentences per grammar
            for si, sent in enumerate(grammar["sentences"][:3]):
                sentence = sent["sentence"]
                answer = sent["answer"]
                
                # Analyze with both
                ginza_result = analyze_with_ginza(sentence)
                ichiran_result = analyze_with_ichiran(sentence)
                
                ginza_times.append(ginza_result["elapsed_ms"])
                ichiran_times.append(ichiran_result["elapsed_ms"])
                
                # Try to detect grammar
                detection = detect_grammar_pattern_ginza(ginza_result, answer)
                
                level_results["detection_total"] += 1
                if detection["found"]:
                    level_results["detection_success"] += 1
                
                sent_result = {
                    "sentence": sentence,
                    "answer": answer,
                    "ginza_tokens": ginza_result["token_texts"],
                    "ginza_ms": ginza_result["elapsed_ms"],
                    "ichiran_tokens": ichiran_result["token_texts"],
                    "ichiran_ms": ichiran_result.get("elapsed_ms", 0),
                    "detection": detection,
                }
                grammar_result["sentences"].append(sent_result)
                
                # Print sample
                if gi < 3 and si == 0:
                    print(f"\n--- {grammar['id']} ---")
                    print(f"Sentence: {sentence}")
                    print(f"Answer: {answer}")
                    print(f"GiNZA ({ginza_result['elapsed_ms']:.1f}ms): {' | '.join(ginza_result['token_texts'])}")
                    print(f"Ichiran ({ichiran_result.get('elapsed_ms', 0):.1f}ms): {' | '.join(ichiran_result['token_texts'])}")
                    print(f"Detection: {detection}")
            
            level_results["grammars"].append(grammar_result)
        
        # Level summary
        rate = level_results["detection_success"] / max(level_results["detection_total"], 1) * 100
        print(f"\n{level} Detection Rate: {rate:.1f}% ({level_results['detection_success']}/{level_results['detection_total']})")
        all_results.append(level_results)
    
    # Overall stats
    print("\n" + "=" * 80)
    print("PERFORMANCE SUMMARY")
    print("=" * 80)
    
    print(f"\nGiNZA Latency:")
    print(f"  Mean: {sum(ginza_times)/len(ginza_times):.2f}ms")
    print(f"  Min:  {min(ginza_times):.2f}ms")
    print(f"  Max:  {max(ginza_times):.2f}ms")
    
    print(f"\nIchiran Latency:")
    ichiran_valid = [t for t in ichiran_times if t > 0]
    if ichiran_valid:
        print(f"  Mean: {sum(ichiran_valid)/len(ichiran_valid):.2f}ms")
        print(f"  Min:  {min(ichiran_valid):.2f}ms")
        print(f"  Max:  {max(ichiran_valid):.2f}ms")
    else:
        print("  No valid Ichiran timings")
    
    print("\n" + "=" * 80)
    print("DETECTION SUMMARY")
    print("=" * 80)
    
    total_success = sum(r["detection_success"] for r in all_results)
    total_tests = sum(r["detection_total"] for r in all_results)
    
    for r in all_results:
        rate = r["detection_success"] / max(r["detection_total"], 1) * 100
        print(f"{r['level']}: {rate:.1f}% ({r['detection_success']}/{r['detection_total']})")
    
    print(f"\nOverall: {total_success/total_tests*100:.1f}% ({total_success}/{total_tests})")
    
    # Save detailed results
    output_path = Path(__file__).parent / "grammar-detection-results.json"
    with open(output_path, "w", encoding="utf-8") as f:
        json.dump(all_results, f, ensure_ascii=False, indent=2)
    print(f"\nDetailed results saved to: {output_path}")

if __name__ == "__main__":
    main()

