#!/usr/bin/env python3
"""
Demo: Dependency-based grammar pattern matching.
Shows how GiNZA's structural info can disambiguate overlapping patterns.
"""
import ginza
import spacy
nlp = spacy.load("ja_ginza")

def analyze(text):
    """Show full GiNZA analysis with dependencies."""
    doc = nlp(text)
    print(f"\n{'='*60}")
    print(f"Sentence: {text}")
    print(f"{'='*60}")
    print(f"\n{'Token':<10} {'POS':<8} {'Tag':<15} {'Dep':<10} {'Head':<10}")
    print("-" * 60)
    for t in doc:
        print(f"{t.text:<10} {t.pos_:<8} {t.tag_:<15} {t.dep_:<10} {t.head.text:<10}")
    
    # Show dependency tree
    print(f"\nDependency edges:")
    for t in doc:
        if t.dep_ != "ROOT":
            print(f"  {t.text} --[{t.dep_}]--> {t.head.text}")
    
    return doc

def check_kara_miru_to(doc):
    """
    Check for からみると grammar pattern using dependencies.
    
    Structure required:
    - から: case marker attached to noun
    - みる: verb with conditional marker
    - と: mark attached to みる
    """
    for i, t in enumerate(doc):
        if t.text == "から" and t.dep_ == "case":
            # Found から as case marker
            head = t.head
            # Look for みる after
            for j in range(i+1, len(doc)):
                if doc[j].lemma_ in ["見る", "みる"]:
                    # Found みる, check for と
                    for child in doc[j].children:
                        if child.text == "と" and child.dep_ == "mark":
                            return {
                                "found": True,
                                "from": head.text,
                                "verb": doc[j].text,
                                "confidence": 1.0,
                            }
    return {"found": False, "confidence": 0}

def check_dake_de_naku(doc):
    """
    Check for だけでなく grammar pattern.
    
    Structure: だけ + で + なく in sequence, だけ attached to previous element
    """
    for i, t in enumerate(doc):
        if t.text == "だけ":
            # Check next tokens
            if i + 2 < len(doc):
                if doc[i+1].text == "で" and doc[i+2].lemma_ == "ない":
                    # Found the pattern
                    return {
                        "found": True,
                        "scope": t.head.text,
                        "confidence": 1.0,
                    }
    return {"found": False, "confidence": 0}

def check_wa_particle(doc):
    """
    Check for は as topic marker (not part of another grammar).
    Should be attached to noun as case marker.
    """
    for t in doc:
        if t.text == "は" and t.dep_ == "case":
            if t.head.pos_ in ["NOUN", "PROPN", "PRON"]:
                return {
                    "found": True,
                    "topic": t.head.text,
                    "confidence": 0.5,  # Low confidence - very common
                }
    return {"found": False, "confidence": 0}

def classify_with_deps(text, grammar_checkers):
    """
    Classify a sentence using dependency-aware grammar checkers.
    More specific patterns win over generic ones.
    """
    doc = nlp(text)
    
    results = {}
    for name, checker in grammar_checkers.items():
        result = checker(doc)
        if result["found"]:
            results[name] = result
    
    # Rank by specificity (more tokens in pattern = more specific)
    specificity = {
        "からみると": 3,
        "だけでなく": 3,
        "は": 1,
    }
    
    if results:
        best = max(results.items(), key=lambda x: specificity.get(x[0], 1) * x[1]["confidence"])
        return best[0], best[1], results
    return None, {}, {}

def main():
    print("=" * 60)
    print("Dependency-Based Grammar Pattern Demo")
    print("=" * 60)
    
    # Test sentences that caused confusion in simple matching
    test_cases = [
        ("名古屋の夏は暑いだけでなく、湿気もひどい。", "だけでなく"),
        ("素人からみるとかなりうまい人でも、プロの世界では全然通用しないらしい。", "からみると"),
        ("私は学生です。", "は"),
        ("田舎は静かなだけでなく、空気もきれいだ。", "だけでなく"),
    ]
    
    grammar_checkers = {
        "からみると": check_kara_miru_to,
        "だけでなく": check_dake_de_naku,
        "は": check_wa_particle,
    }
    
    print("\n" + "=" * 60)
    print("Classification Results")
    print("=" * 60)
    
    correct = 0
    for sentence, expected in test_cases:
        doc = analyze(sentence)
        predicted, match_info, all_matches = classify_with_deps(sentence, grammar_checkers)
        
        is_correct = predicted == expected
        if is_correct:
            correct += 1
        
        status = "✓" if is_correct else "✗"
        print(f"\n{status} Expected: {expected}, Got: {predicted}")
        print(f"   Match info: {match_info}")
        if len(all_matches) > 1:
            print(f"   All matches: {list(all_matches.keys())}")
    
    print(f"\n{'='*60}")
    print(f"Accuracy: {correct}/{len(test_cases)} ({correct/len(test_cases)*100:.0f}%)")
    print("=" * 60)
    
    print("\n--- Key Insight ---")
    print("Dependency-based matching can disambiguate overlapping patterns:")
    print("- だけでなく (specific structure) wins over は (generic particle)")
    print("- からみると (verb+conditional) beats simple からの")
    print("\nThis is the foundation for a robust grammar detection system.")

if __name__ == "__main__":
    main()

