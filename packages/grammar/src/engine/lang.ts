import type { RuleSpec, NodeRef, TokPred, CaptureSpec, EitherBranch } from './dsl.js';
import type { GinzaPOS, GinzaDep, GinzaInflectionForm, GinzaConjugationClass } from '../ginza/generated.js';
import { V, node, edge, before, text, textOneOf, lemma, lemmaOneOf, pos, posOneOf, dep, depOneOf, inflectionForm, inflectionFormOneOf, conjugationClass, conjugationClassOneOf, tag, not } from './dsl.js';
import type { Clause } from './dsl.js';

export type TokenCond = {
  text?: string;
  textOneOf?: string[];
  lemma?: string;
  lemmaOneOf?: string[];
  pos?: GinzaPOS;
  posOneOf?: GinzaPOS[];
  dep?: GinzaDep;
  depOneOf?: GinzaDep[];
  inflectionForm?: GinzaInflectionForm;
  inflectionFormOneOf?: GinzaInflectionForm[];
  conjugationClass?: GinzaConjugationClass;
  conjugationClassOneOf?: GinzaConjugationClass[];
  tag?: string;
};

export type LangVar = {
  name: string;
  ref: NodeRef;
  cond: TokenCond;
};

function condToPreds(cond: TokenCond): TokPred[] {
  const out: TokPred[] = [];
  if (cond.text !== undefined) out.push(text(cond.text));
  if (cond.textOneOf !== undefined) out.push(textOneOf(cond.textOneOf));
  if (cond.lemma !== undefined) out.push(lemma(cond.lemma));
  if (cond.lemmaOneOf !== undefined) out.push(lemmaOneOf(cond.lemmaOneOf));
  if (cond.pos !== undefined) out.push(pos(cond.pos));
  if (cond.posOneOf !== undefined) out.push(posOneOf(cond.posOneOf));
  if (cond.dep !== undefined) out.push(dep(cond.dep));
  if (cond.depOneOf !== undefined) out.push(depOneOf(cond.depOneOf));
  if (cond.inflectionForm !== undefined) out.push(inflectionForm(cond.inflectionForm));
  if (cond.inflectionFormOneOf !== undefined) out.push(inflectionFormOneOf(cond.inflectionFormOneOf));
  if (cond.conjugationClass !== undefined) out.push(conjugationClass(cond.conjugationClass));
  if (cond.conjugationClassOneOf !== undefined) out.push(conjugationClassOneOf(cond.conjugationClassOneOf));
  if (cond.tag !== undefined) out.push(tag(cond.tag));
  return out;
}

export class LinguisticRuleBuilder {
  private id: string;
  private vars: LangVar[] = [];
  private clauses: RuleSpec['where'] = [];
  private captureSpecs: CaptureSpec[] = [];
  private varSeq = 0;

  constructor(id: string) {
    this.id = id;
  }

  /** Create a new bound token variable. */
  tok(cond: TokenCond, name?: string): LangVar {
    const n = name ?? `v${this.varSeq++}`;
    const ref = V(n);
    const v: LangVar = { name: n, ref, cond };
    this.vars.push(v);
    this.clauses.push(node(ref, condToPreds(cond)));
    return v;
  }

  verb(cond: Omit<TokenCond, 'pos'> = {}, name?: string): LangVar {
    return this.tok({ ...cond, pos: 'VERB' }, name);
  }

  noun(cond: Omit<TokenCond, 'pos'> = {}, name?: string): LangVar {
    return this.tok({ ...cond, pos: 'NOUN' }, name);
  }

  aux(cond: Omit<TokenCond, 'pos'> = {}, name?: string): LangVar {
    return this.tok({ ...cond, pos: 'AUX' }, name);
  }

  adj(cond: Omit<TokenCond, 'pos'> = {}, name?: string): LangVar {
    return this.tok({ ...cond, pos: 'ADJ' }, name);
  }

  adv(cond: Omit<TokenCond, 'pos'> = {}, name?: string): LangVar {
    return this.tok({ ...cond, pos: 'ADV' }, name);
  }

  /** Create a particle token. POS is NOT forced by default (GiNZA varies). */
  particle(particleText: string, name?: string, cond?: Omit<TokenCond, 'text'>): LangVar {
    return this.tok({ text: particleText, ...cond }, name);
  }

  /** Dependency edge: child -> head. */
  headChild(head: LangVar, child: LangVar, depLabel?: GinzaDep): this {
    this.clauses.push(edge(child.ref, head.ref, depLabel));
    return this;
  }

  /** "case" particle attached to a nominal head (UD: case). */
  caseMarker(nominal: LangVar, particle: LangVar): this {
    return this.headChild(nominal, particle, 'case');
  }

  /** "aux" attached to a predicate head (UD: aux). */
  auxOf(head: LangVar, auxTok: LangVar): this {
    return this.headChild(head, auxTok, 'aux');
  }

  /** Copula attached to a nominal/adjectival head (UD: cop). */
  copulaOf(head: LangVar, copTok: LangVar): this {
    return this.headChild(head, copTok, 'cop');
  }

  /** Object nominal attached to verb (UD: obj). */
  objectOf(verb: LangVar, obj: LangVar): this {
    return this.headChild(verb, obj, 'obj');
  }

  /** Enforce surface order with optional maxDistance (in tokens). */
  inOrder(a: LangVar, b: LangVar, maxDistance?: number): this {
    this.clauses.push(before(a.ref, b.ref, maxDistance));
    return this;
  }

  /** Negate a clause - the clause must NOT match. */
  not(build: (r: this) => void): this {
    const prevLen = this.clauses.length;
    build(this);
    const clausesToNegate = this.clauses.splice(prevLen);
    if (clausesToNegate.length > 0) {
      // If multiple clauses, negate as a group
      if (clausesToNegate.length === 1) {
        this.clauses.push(not(clausesToNegate[0]!));
      } else {
        // For multiple clauses, use not() around each
        for (const c of clausesToNegate) {
          this.clauses.push(not(c));
        }
      }
    }
    return this;
  }

  /** Optional clause block: clauses inside don't cause match failure if unmet. */
  optional(build: (r: this) => void): this {
    const prevLen = this.clauses.length;
    build(this);
    const optionalClauses = this.clauses.splice(prevLen);
    if (optionalClauses.length > 0) {
      this.clauses.push({ kind: 'optional', clauses: optionalClauses } as Clause);
    }
    return this;
  }

  /**
   * Define alternative patterns. Each branch is expanded at compile time into
   * separate rules with the same id. Each branch defines its own clauses and captures.
   * 
   * @example
   * r.either(
   *   (b) => {
   *     const shimau = b.tok({ lemma: 'しまう', dep: 'fixed' }, 'shimau');
   *     b.captureAs('match', shimau);
   *   },
   *   (b) => {
   *     const chau = b.tok({ lemmaOneOf: ['ちゃう', 'じゃう'] }, 'shimau');
   *     b.captureAs('match', chau);
   *   }
   * );
   */
  either(...branches: Array<(b: LinguisticRuleBuilder) => void>): this {
    const eitherBranches: EitherBranch[] = [];

    for (const buildBranch of branches) {
      // Create a child builder for this branch
      const branchBuilder = new LinguisticRuleBuilder(this.id);
      branchBuilder.varSeq = this.varSeq; // Share var counter
      buildBranch(branchBuilder);
      this.varSeq = branchBuilder.varSeq; // Update shared counter

      eitherBranches.push({
        clauses: branchBuilder.clauses,
        captures: branchBuilder.captureSpecs,
      });
    }

    this.clauses.push({ kind: 'either', branches: eitherBranches } as Clause);
    return this;
  }

  /** Capture a single token under the default name 'match'. */
  capture(v: LangVar): this {
    this.captureSpecs.push({ kind: 'token', name: 'match', var: v.ref });
    return this;
  }

  /** Capture a single token under a custom name. */
  captureAs(name: string, v: LangVar): this {
    this.captureSpecs.push({ kind: 'token', name, var: v.ref });
    return this;
  }

  /** Capture a span from token `from` to token `to` (inclusive, by char offsets). */
  captureSpan(name: string, from: LangVar, to: LangVar): this {
    this.captureSpecs.push({ kind: 'span', name, from: from.ref, to: to.ref });
    return this;
  }

  build(): RuleSpec {
    return {
      id: this.id,
      where: this.clauses,
      captures: this.captureSpecs,
    };
  }
}

export function linguisticRule(
  id: string,
  build: (r: LinguisticRuleBuilder) => void
): RuleSpec {
  const r = new LinguisticRuleBuilder(id);
  build(r);
  return r.build();
}

