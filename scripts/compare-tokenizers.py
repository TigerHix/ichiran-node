#!/usr/bin/env python3
"""
Compare GiNZA and Ichiran tokenization on grammar example sentences.
Calls local Ichiran API at http://0.0.0.0:3000/api
"""
import json
import sys
import os
import random
from pathlib import Path
import requests

# Import ginza+spacy
import ginza
import spacy
nlp = spacy.load("ja_ginza")

# Paths
GRAMMARS_DIR = Path(__file__).parent.parent / "packages/grammar/src/grammars"
ICHIRAN_API = "http://0.0.0.0:3000/api"

def extract_ichiran_tokens(segments):
    """Extract tokens from Ichiran's nested segment structure.
    
    Structure: segments is an array where each item is either:
    - A string (punctuation)
    - An array of alternatives, where:
      - alternatives[0] is [words_array, score]
      - words_array contains [romaji, info_dict, extra] triples
    """
    tokens = []
    
    for seg in segments:
        if isinstance(seg, str):
            # Punctuation
            tokens.append({"text": seg, "type": "punct"})
        elif isinstance(seg, list) and len(seg) > 0:
            # Array of alternatives - take first alternative
            first_alt = seg[0]
            if isinstance(first_alt, list) and len(first_alt) >= 2:
                words_array = first_alt[0]
                if isinstance(words_array, list):
                    for word in words_array:
                        if isinstance(word, list) and len(word) >= 2:
                            romaji = word[0]
                            info = word[1]
                            if isinstance(info, dict):
                                tokens.append({
                                    "text": info.get("text", ""),
                                    "reading": info.get("reading", ""),
                                    "kana": info.get("kana", ""),
                                    "romaji": romaji,
                                    "gloss": info.get("gloss", []),
                                    "compound": info.get("compound"),
                                    "components": info.get("components"),
                                    "conj": info.get("conj", []),
                                })
    
    return tokens

def get_ichiran_tokens(text):
    """Call local Ichiran API and extract tokens."""
    try:
        resp = requests.post(f"{ICHIRAN_API}/segment", 
                            json={"text": text, "limit": 1}, 
                            timeout=30)
        data = resp.json()
        # Extract tokens from segmentation response
        tokens = []
        if isinstance(data, dict) and "segments" in data:
            tokens = extract_ichiran_tokens(data["segments"])
        return tokens, data
    except Exception as e:
        return [f"ERROR: {e}"], {"error": str(e)}

def get_ichiran_raw(text):
    """Get raw Ichiran API response for detailed analysis."""
    try:
        resp = requests.post(f"{ICHIRAN_API}/segment", 
                            json={"text": text, "limit": 1}, 
                            timeout=30)
        return resp.json()
    except Exception as e:
        return {"error": str(e)}

def get_ginza_tokens(text):
    """Get GiNZA tokens."""
    doc = nlp(text)
    return [token.text for token in doc]

def get_ginza_detailed(text):
    """Get GiNZA detailed analysis."""
    doc = nlp(text)
    return [{
        "text": token.text,
        "lemma": token.lemma_,
        "pos": token.pos_,
        "tag": token.tag_,
        "dep": token.dep_,
        "head": token.head.text,
        "head_idx": token.head.i,
    } for token in doc]

def load_grammar_examples(level, count=10):
    """Load example sentences from grammar files."""
    level_dir = GRAMMARS_DIR / level
    if not level_dir.exists():
        return []
    
    examples = []
    files = list(level_dir.glob("*.json"))
    random.seed(42)  # Reproducible
    random.shuffle(files)
    
    for f in files:
        if len(examples) >= count:
            break
        try:
            with open(f, "r", encoding="utf-8") as fp:
                data = json.load(fp)
            if data.get("examples"):
                ex = data["examples"][0]  # First example
                examples.append({
                    "grammar": data["id"],
                    "jp": ex["jp"],
                    "en": ex.get("en", ""),
                })
        except Exception:
            pass
    
    return examples

# Hardcoded N3 examples (N3 grammar files have empty examples)
N3_EXAMPLES = [
    {"grammar": "n3.beki-da", "jp": "学生は勉強するべきだ。", "en": "Students should study."},
    {"grammar": "n3.hazu-da", "jp": "彼は来るはずだ。", "en": "He should come."},
    {"grammar": "n3.wake-da", "jp": "だから遅れたわけだ。", "en": "That's why I was late."},
    {"grammar": "n3.you-ni-natta", "jp": "日本語が話せるようになった。", "en": "I've become able to speak Japanese."},
    {"grammar": "n3.koto-ni-natte-iru", "jp": "来週出発することになっている。", "en": "It's been decided that we'll depart next week."},
    {"grammar": "n3.te-hoshii", "jp": "手伝ってほしい。", "en": "I want you to help me."},
    {"grammar": "n3.ba-yokatta", "jp": "もっと早く起きればよかった。", "en": "I should have woken up earlier."},
    {"grammar": "n3.rashii", "jp": "明日は雨らしい。", "en": "It seems like it'll rain tomorrow."},
    {"grammar": "n3.nai-to", "jp": "早く行かないと。", "en": "I have to go soon."},
    {"grammar": "n3.koto-wa-nai", "jp": "心配することはない。", "en": "There's no need to worry."},
]

def main():
    print("=" * 80)
    print("GiNZA vs Ichiran Tokenization Comparison")
    print("=" * 80)
    
    # Load samples
    n5_examples = load_grammar_examples("n5", 10)
    n3_examples = N3_EXAMPLES[:10]  # Use hardcoded N3 examples
    
    all_examples = []
    for ex in n5_examples:
        ex["level"] = "N5"
        all_examples.append(ex)
    for ex in n3_examples:
        ex["level"] = "N3"
        all_examples.append(ex)
    
    print(f"\nLoaded {len(n5_examples)} N5 and {len(n3_examples)} N3 examples\n")
    
    results = []
    
    for i, ex in enumerate(all_examples):
        print(f"\n{'='*80}")
        print(f"[{i+1}] {ex['level']} - {ex['grammar']}")
        print(f"{'='*80}")
        print(f"JP: {ex['jp']}")
        print(f"EN: {ex['en']}")
        
        # Get tokenizations
        ginza_tokens = get_ginza_tokens(ex["jp"])
        ginza_detailed = get_ginza_detailed(ex["jp"])
        ichiran_tokens, ichiran_raw = get_ichiran_tokens(ex["jp"])
        
        print(f"\n--- GiNZA ({len(ginza_tokens)} tokens) ---")
        print(" | ".join(ginza_tokens))
        
        print(f"\n--- GiNZA Detailed ---")
        for t in ginza_detailed:
            print(f"  {t['text']:10} lemma={t['lemma']:10} pos={t['pos']:5} tag={t['tag']:15} dep={t['dep']:10} head={t['head']}")
        
        print(f"\n--- Ichiran ({len(ichiran_tokens)} tokens) ---")
        token_texts = [t["text"] if isinstance(t, dict) else str(t) for t in ichiran_tokens]
        print(" | ".join(token_texts) if token_texts else "No tokens extracted")
        
        print(f"\n--- Ichiran Detailed ---")
        for t in ichiran_tokens:
            if isinstance(t, dict):
                gloss_str = ""
                if t.get("gloss"):
                    g = t["gloss"][0] if t["gloss"] else {}
                    gloss_str = g.get("gloss", "")[:50] if isinstance(g, dict) else ""
                compound = t.get("compound", [])
                comp_str = f" compound={compound}" if compound else ""
                print(f"  {t.get('text', '?'):15} kana={t.get('kana', ''):15} romaji={t.get('romaji', ''):20}{comp_str} gloss={gloss_str}")
        
        results.append({
            "level": ex["level"],
            "grammar": ex["grammar"],
            "sentence": ex["jp"],
            "ginza_tokens": ginza_tokens,
            "ginza_count": len(ginza_tokens),
            "ginza_detailed": ginza_detailed,
            "ichiran_tokens": ichiran_tokens,
            "ichiran_count": len(ichiran_tokens),
            "ichiran_raw": ichiran_raw,
        })
    
    # Save results
    output_path = Path(__file__).parent / "tokenizer-comparison-results.json"
    with open(output_path, "w", encoding="utf-8") as f:
        json.dump(results, f, ensure_ascii=False, indent=2)
    print(f"\n\nResults saved to {output_path}")

if __name__ == "__main__":
    main()

