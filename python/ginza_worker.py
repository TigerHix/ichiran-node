#!/usr/bin/env python3
"""
GiNZA worker process (spaCy) with a simple NDJSON protocol over stdin/stdout.

Protocol (one JSON object per line):
  Request:  {"id":"...","op":"analyze","texts":["...","..."]}
  Response: {"id":"...","ok":true,"docs":[{"text":"...","sentences":[...]}]}

We intentionally use GiNZA's own sentence boundaries (doc.sents).
"""

from __future__ import annotations

import json
import sys
import importlib.metadata
from typing import Any, Dict, List, Optional


def _eprint(*args: Any) -> None:
    print(*args, file=sys.stderr)


def _require_spacy() -> Any:
    try:
        import ginza  # noqa: F401  # must import before spacy to register components
        import spacy
        from spacy.lang.ja import Japanese
    except Exception as e:
        _eprint("Failed to import ginza/spacy:", repr(e))
        raise

    try:
        nlp = spacy.load("ja_ginza")
        return nlp, Japanese
    except Exception as e:
        _eprint("Failed to load model 'ja_ginza'. Install with:")
        _eprint("  pip install ginza ja-ginza")
        _eprint("Error:", repr(e))
        raise


def _escape_misc_value(value: str) -> str:
    # Follow ginza's escaping for Reading (escape backslash then pipe).
    return value.replace("\\", "\\\\").replace("|", "\\|")


def _token_to_json(token, sent_start: int, sent_end: int, use_orth_if_reading_is_none: bool, use_bunsetu: bool) -> Dict[str, Any]:
    """
    Serialize a spaCy/GiNZA Token into a JSON-friendly object.
    This is intentionally "raw-ish": we keep the spaCy surface and parse signals
    (pos/tag/dep/head/morph/char offsets) plus GiNZA helper outputs.
    """
    import ginza

    # Sentence-local indices
    i = int(token.i - sent_start)

    if token.head.i == token.i:
        head = -1
    else:
        hi = int(token.head.i)
        head = hi - sent_start if sent_start <= hi < sent_end else -1

    dep = (token.dep_ or "").lower()

    start = int(token.idx)
    end = int(token.idx + len(token.text))

    feats = token.morph.to_dict() if token.morph is not None else {}

    inf = ginza.inflection(token) or ""
    reading = ginza.reading_form(token, use_orth_if_reading_is_none) or ""
    ne = ginza.ent_label_ontonotes(token) or ""
    ene = ginza.ent_label_ene(token) or ""

    bunsetu_bi: Optional[str] = None
    bunsetu_position_type: Optional[str] = None
    clause_head: int = -1
    if use_bunsetu:
        try:
            bunsetu_bi = ginza.bunsetu_bi_label(token)
            bunsetu_position_type = ginza.bunsetu_position_type(token)
            clause_head = int(ginza.clause_head_i(token))
        except Exception:
            bunsetu_bi = None
            bunsetu_position_type = None
            clause_head = -1

    misc: Dict[str, Any] = {}

    # Provide a few GiNZA-ish extras in misc too (keeps rule DSL surface stable and easy to debug).
    if inf:
        misc["Inf"] = inf
    if reading:
        misc["Reading"] = _escape_misc_value(reading)
    if ne and ne != "O":
        misc["NE"] = ne
    if ene and ene != "O":
        misc["ENE"] = ene
    if bunsetu_bi:
        misc["BunsetuBILabel"] = bunsetu_bi
    if bunsetu_position_type:
        misc["BunsetuPositionType"] = bunsetu_position_type
    if clause_head >= 0:
        misc["ClauseHead"] = str(clause_head)

    return {
        "i": i,
        "text": token.text,
        "lemma": token.lemma_ or "",
        "norm": token.norm_ or "",
        "pos": token.pos_ or "",
        "tag": (token.tag_ or "").replace(",*", "").replace(",", "-"),
        "dep": dep,
        "head": head,
        "start": start,
        "end": end,
        "whitespace": token.whitespace_ or "",
        "feats": feats if feats else {},
        "inflection": inf,
        "reading": reading,
        "ne": ne,
        "ene": ene,
        "bunsetu": {
            "bi": bunsetu_bi,
            "positionType": bunsetu_position_type,
        },
        "clauseHead": clause_head,
        "misc": misc if misc else {},
    }


def _doc_to_json(doc, use_orth_if_reading_is_none: bool) -> Dict[str, Any]:
    # We rely on GiNZA's sentence boundaries (doc.sents). Most dependency edges are sentence-internal.
    from ginza.bunsetu_recognizer import bunsetu_available

    out_sents: List[Dict[str, Any]] = []
    for sent in doc.sents:
        sent_start = int(sent.start)
        sent_end = int(sent.end)
        use_bunsetu = False
        try:
            use_bunsetu = bool(bunsetu_available(sent))
        except Exception:
            use_bunsetu = False

        tokens = [
            _token_to_json(t, sent_start, sent_end, use_orth_if_reading_is_none, use_bunsetu)
            for t in sent
        ]

        out_sents.append(
            {
                "text": sent.text,
                "start": int(sent.start_char),
                "end": int(sent.end_char),
                "tokens": tokens,
            }
        )

    return {"text": doc.text, "sentences": out_sents}


def _get_version(dist_name: str) -> str | None:
    try:
        return importlib.metadata.version(dist_name)
    except Exception:
        return None


def _meta_to_json(nlp) -> Dict[str, Any]:
    # Introspect the *installed* model/pipeline, not a corpus sample.
    import spacy

    pipes = list(nlp.pipe_names)
    labels: Dict[str, List[str]] = {}
    for name in pipes:
        try:
            pipe = nlp.get_pipe(name)
        except Exception:
            continue
        if hasattr(pipe, "labels"):
            try:
                labels[name] = list(getattr(pipe, "labels"))
            except Exception:
                pass

    return {
        "model": getattr(getattr(nlp, "meta", None), "get", lambda k, d=None: d)("name", None),
        "lang": getattr(nlp, "lang", None),
        "spacyVersion": getattr(spacy, "__version__", None),
        "ginzaVersion": _get_version("ginza"),
        "jaGinzaModelVersion": _get_version("ja-ginza"),
        "pipes": pipes,
        "labels": labels,
    }


def main() -> None:
    nlp, Japanese = _require_spacy()
    use_orth_if_reading_is_none = isinstance(nlp, Japanese)

    # Print a one-time ready banner on stderr (stdout must be NDJSON only)
    _eprint("[ginza_worker] ready")

    for line in sys.stdin:
        line = line.strip()
        if not line:
            continue

        try:
            req = json.loads(line)
            req_id = req.get("id")
            op = req.get("op")
            if op == "meta":
                out = {"id": req_id, "ok": True, "meta": _meta_to_json(nlp)}
                sys.stdout.write(json.dumps(out, ensure_ascii=False) + "\n")
                sys.stdout.flush()
                continue
            if op != "analyze":
                raise ValueError(f"unknown op: {op}")

            texts = req.get("texts")
            if not isinstance(texts, list) or not all(isinstance(t, str) for t in texts):
                raise ValueError("texts must be an array of strings")

            docs = []
            # nlp.pipe is much faster for batches
            for doc in nlp.pipe(texts):
                docs.append(_doc_to_json(doc, use_orth_if_reading_is_none))

            out = {"id": req_id, "ok": True, "docs": docs}
            sys.stdout.write(json.dumps(out, ensure_ascii=False) + "\n")
            sys.stdout.flush()
        except Exception as e:
            err = {"id": req.get("id") if "req" in locals() else None, "ok": False, "error": str(e)}
            sys.stdout.write(json.dumps(err, ensure_ascii=False) + "\n")
            sys.stdout.flush()


if __name__ == "__main__":
    main()


