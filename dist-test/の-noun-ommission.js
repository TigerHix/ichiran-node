// @bun
// src/engine/dsl.ts
function V(v) {
  return { v };
}
function text(value) {
  return { kind: "text", value };
}
function textOneOf(value) {
  return { kind: "textOneOf", value };
}
function lemma(value) {
  return { kind: "lemma", value };
}
function lemmaOneOf(value) {
  return { kind: "lemmaOneOf", value };
}
function pos(value) {
  return { kind: "pos", value };
}
function dep(value) {
  return { kind: "dep", value };
}
function depOneOf(value) {
  return { kind: "depOneOf", value };
}
function posOneOf(value) {
  return { kind: "posOneOf", value };
}
function inflectionForm(value) {
  return { kind: "inflectionForm", value };
}
function inflectionFormOneOf(value) {
  return { kind: "inflectionFormOneOf", value };
}
function conjugationClass(value) {
  return { kind: "conjugationClass", value };
}
function conjugationClassOneOf(value) {
  return { kind: "conjugationClassOneOf", value };
}
function tag(value) {
  return { kind: "tag", value };
}
function node(node2, preds) {
  return { kind: "node", node: node2, preds };
}
function edge(child, head, depLabel) {
  return { kind: "edge", child, head, dep: depLabel };
}
function before(a, b, maxDistance) {
  return { kind: "before", a, b, maxDistance };
}
function not(clause) {
  return { kind: "not", clause };
}

// src/engine/lang.ts
function condToPreds(cond) {
  const out = [];
  if (cond.text !== undefined)
    out.push(text(cond.text));
  if (cond.textOneOf !== undefined)
    out.push(textOneOf(cond.textOneOf));
  if (cond.lemma !== undefined)
    out.push(lemma(cond.lemma));
  if (cond.lemmaOneOf !== undefined)
    out.push(lemmaOneOf(cond.lemmaOneOf));
  if (cond.pos !== undefined)
    out.push(pos(cond.pos));
  if (cond.posOneOf !== undefined)
    out.push(posOneOf(cond.posOneOf));
  if (cond.dep !== undefined)
    out.push(dep(cond.dep));
  if (cond.depOneOf !== undefined)
    out.push(depOneOf(cond.depOneOf));
  if (cond.inflectionForm !== undefined)
    out.push(inflectionForm(cond.inflectionForm));
  if (cond.inflectionFormOneOf !== undefined)
    out.push(inflectionFormOneOf(cond.inflectionFormOneOf));
  if (cond.conjugationClass !== undefined)
    out.push(conjugationClass(cond.conjugationClass));
  if (cond.conjugationClassOneOf !== undefined)
    out.push(conjugationClassOneOf(cond.conjugationClassOneOf));
  if (cond.tag !== undefined)
    out.push(tag(cond.tag));
  return out;
}

class LinguisticRuleBuilder {
  id;
  vars = [];
  clauses = [];
  captureSpecs = [];
  varSeq = 0;
  constructor(id) {
    this.id = id;
  }
  tok(cond, name) {
    const n = name ?? `v${this.varSeq++}`;
    const ref = V(n);
    const v = { name: n, ref, cond };
    this.vars.push(v);
    this.clauses.push(node(ref, condToPreds(cond)));
    return v;
  }
  verb(cond = {}, name) {
    return this.tok({ ...cond, pos: "VERB" }, name);
  }
  noun(cond = {}, name) {
    return this.tok({ ...cond, pos: "NOUN" }, name);
  }
  aux(cond = {}, name) {
    return this.tok({ ...cond, pos: "AUX" }, name);
  }
  adj(cond = {}, name) {
    return this.tok({ ...cond, pos: "ADJ" }, name);
  }
  adv(cond = {}, name) {
    return this.tok({ ...cond, pos: "ADV" }, name);
  }
  particle(particleText, name, cond) {
    return this.tok({ text: particleText, ...cond }, name);
  }
  headChild(head, child, depLabel) {
    this.clauses.push(edge(child.ref, head.ref, depLabel));
    return this;
  }
  caseMarker(nominal, particle) {
    return this.headChild(nominal, particle, "case");
  }
  auxOf(head, auxTok) {
    return this.headChild(head, auxTok, "aux");
  }
  copulaOf(head, copTok) {
    return this.headChild(head, copTok, "cop");
  }
  objectOf(verb, obj) {
    return this.headChild(verb, obj, "obj");
  }
  inOrder(a, b, maxDistance) {
    this.clauses.push(before(a.ref, b.ref, maxDistance));
    return this;
  }
  not(build) {
    const prevLen = this.clauses.length;
    build(this);
    const clausesToNegate = this.clauses.splice(prevLen);
    if (clausesToNegate.length > 0) {
      if (clausesToNegate.length === 1) {
        this.clauses.push(not(clausesToNegate[0]));
      } else {
        for (const c of clausesToNegate) {
          this.clauses.push(not(c));
        }
      }
    }
    return this;
  }
  optional(build) {
    const prevLen = this.clauses.length;
    build(this);
    const optionalClauses = this.clauses.splice(prevLen);
    if (optionalClauses.length > 0) {
      this.clauses.push({ kind: "optional", clauses: optionalClauses });
    }
    return this;
  }
  either(...branches) {
    const eitherBranches = [];
    for (const buildBranch of branches) {
      const branchBuilder = new LinguisticRuleBuilder(this.id);
      branchBuilder.varSeq = this.varSeq;
      buildBranch(branchBuilder);
      this.varSeq = branchBuilder.varSeq;
      eitherBranches.push({
        clauses: branchBuilder.clauses,
        captures: branchBuilder.captureSpecs
      });
    }
    this.clauses.push({ kind: "either", branches: eitherBranches });
    return this;
  }
  capture(v) {
    this.captureSpecs.push({ kind: "token", name: "match", var: v.ref });
    return this;
  }
  captureAs(name, v) {
    this.captureSpecs.push({ kind: "token", name, var: v.ref });
    return this;
  }
  captureSpan(name, from, to) {
    this.captureSpecs.push({ kind: "span", name, from: from.ref, to: to.ref });
    return this;
  }
  build() {
    return {
      id: this.id,
      where: this.clauses,
      captures: this.captureSpecs
    };
  }
}
function linguisticRule(id, build) {
  const r = new LinguisticRuleBuilder(id);
  build(r);
  return r.build();
}

/* src/rules/bunpro/jlpt5/の-noun-ommission.ts */
var \u{306e}_noun_ommission_default = linguisticRule("\u306E-noun-ommission", (r) => {
  const no = r.particle("\u306E", "no", { dep: "case" });
  const owner = r.tok({ posOneOf: ["NOUN", "PROPN", "PRON", "DET"] }, "owner");
  r.caseMarker(owner, no);
  r.either((b1) => {
    const copula = b1.tok({ posOneOf: ["AUX", "VERB"] }, "copula");
    b1.inOrder(no, copula, 3);
    b1.capture(no);
  }, (b2) => {
    b2.capture(no);
  }, (b3) => {
    const particle = b3.tok({ pos: "ADP", dep: "case" }, "particle");
    b3.inOrder(no, particle, 2);
    b3.capture(no);
  });
});
export {
  \u{306e}_noun_ommission_default as default
};
