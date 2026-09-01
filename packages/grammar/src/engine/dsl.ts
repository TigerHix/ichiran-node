import type { GinzaSentence, GinzaToken } from '../ginza/types.js';
import type { GinzaPOS, GinzaDep, GinzaInflectionForm, GinzaConjugationClass } from '../ginza/generated.js';
import { SentenceIndex } from './compiler.js';

export type TokPred =
  | { kind: 'text'; value: string }
  | { kind: 'textRe'; value: RegExp }
  | { kind: 'textOneOf'; value: string[] }
  | { kind: 'lemma'; value: string }
  | { kind: 'lemmaRe'; value: RegExp }
  | { kind: 'lemmaOneOf'; value: string[] }
  | { kind: 'pos'; value: GinzaPOS }
  | { kind: 'posOneOf'; value: GinzaPOS[] }
  | { kind: 'dep'; value: GinzaDep }
  | { kind: 'depOneOf'; value: GinzaDep[] }
  | { kind: 'inflectionForm'; value: GinzaInflectionForm }
  | { kind: 'inflectionFormOneOf'; value: GinzaInflectionForm[] }
  | { kind: 'conjugationClass'; value: GinzaConjugationClass }
  | { kind: 'conjugationClassOneOf'; value: GinzaConjugationClass[] }
  | { kind: 'tag'; value: string }
  | { kind: 'tagOneOf'; value: string[] }
  | { kind: 'textEqualsLemma'; value: boolean };

export type NodeRef = { v: string }; // variable name

export type Clause =
  | { kind: 'node'; node: NodeRef; preds: TokPred[] }
  | { kind: 'edge'; child: NodeRef; head: NodeRef; dep?: GinzaDep }
  | { kind: 'next'; a: NodeRef; b: NodeRef }
  | { kind: 'before'; a: NodeRef; b: NodeRef; maxDistance?: number }
  | { kind: 'not'; clause: Clause }
  | { kind: 'optional'; clauses: Clause[] }
  | { kind: 'either'; branches: EitherBranch[] }
  | { kind: 'notBefore'; token: NodeRef; preds: TokPred[]; maxDistance?: number };

/** A branch in an either clause - contains its own clauses and captures */
export type EitherBranch = {
  clauses: Clause[];
  captures: CaptureSpec[];
};

export type CaptureSpec =
  | { kind: 'token'; name: string; var: NodeRef }
  | { kind: 'span'; name: string; from: NodeRef; to: NodeRef };

export type RuleSpec = {
  id: string;
  where: Clause[];
  captures: CaptureSpec[];
  details?: any;
};

export type Trigger = { kind: 'lemma' | 'text'; value: string };

export type CaptureValue = { start: number; end: number; text: string };

export type CompiledRule = {
  id: string;
  triggers: Trigger[];
  details?: any;
  /** Pass pre-built SentenceIndex to avoid rebuilding per-rule */
  match: (sent: GinzaSentence, sourceText: string, idx?: SentenceIndex) => Array<Record<string, CaptureValue>>;
};

export function V(v: string): NodeRef {
  return { v };
}

export function text(value: string): TokPred {
  return { kind: 'text', value };
}
export function textRe(value: RegExp): TokPred {
  return { kind: 'textRe', value };
}
export function textOneOf(value: string[]): TokPred {
  return { kind: 'textOneOf', value };
}
export function lemma(value: string): TokPred {
  return { kind: 'lemma', value };
}
export function lemmaRe(value: RegExp): TokPred {
  return { kind: 'lemmaRe', value };
}
export function lemmaOneOf(value: string[]): TokPred {
  return { kind: 'lemmaOneOf', value };
}
export function pos(value: GinzaPOS): TokPred {
  return { kind: 'pos', value };
}
export function dep(value: GinzaDep): TokPred {
  return { kind: 'dep', value };
}
export function depOneOf(value: GinzaDep[]): TokPred {
  return { kind: 'depOneOf', value };
}
export function posOneOf(value: GinzaPOS[]): TokPred {
  return { kind: 'posOneOf', value };
}
export function inflectionForm(value: GinzaInflectionForm): TokPred {
  return { kind: 'inflectionForm', value };
}
export function inflectionFormOneOf(value: GinzaInflectionForm[]): TokPred {
  return { kind: 'inflectionFormOneOf', value };
}
export function conjugationClass(value: GinzaConjugationClass): TokPred {
  return { kind: 'conjugationClass', value };
}
export function conjugationClassOneOf(value: GinzaConjugationClass[]): TokPred {
  return { kind: 'conjugationClassOneOf', value };
}

export function tag(value: string): TokPred {
  return { kind: 'tag', value };
}
export function tagOneOf(value: string[]): TokPred {
  return { kind: 'tagOneOf', value };
}
export function textEqualsLemma(value: boolean): TokPred {
  return { kind: 'textEqualsLemma', value };
}

export function node(node: NodeRef, preds: TokPred[]): Clause {
  return { kind: 'node', node, preds };
}
export function edge(child: NodeRef, head: NodeRef, depLabel?: GinzaDep): Clause {
  return { kind: 'edge', child, head, dep: depLabel };
}
export function next(a: NodeRef, b: NodeRef): Clause {
  return { kind: 'next', a, b };
}
export function before(a: NodeRef, b: NodeRef, maxDistance?: number): Clause {
  return { kind: 'before', a, b, maxDistance };
}
export function not(clause: Clause): Clause {
  return { kind: 'not', clause };
}

/** Token matching: simple equality checks (inflection already parsed on ingest) */
export function tokenMatchesPreds(tok: GinzaToken, preds: TokPred[]): boolean {
  for (const p of preds) {
    if (p.kind === 'text' && tok.text !== p.value) return false;
    if (p.kind === 'textRe' && !p.value.test(tok.text)) return false;
    if (p.kind === 'textOneOf' && !p.value.includes(tok.text)) return false;
    if (p.kind === 'lemma' && tok.lemma !== p.value) return false;
    if (p.kind === 'lemmaRe' && !p.value.test(tok.lemma)) return false;
    if (p.kind === 'lemmaOneOf' && !p.value.includes(tok.lemma)) return false;
    if (p.kind === 'pos' && tok.pos !== p.value) return false;
    if (p.kind === 'posOneOf' && !p.value.includes(tok.pos)) return false;
    if (p.kind === 'dep' && tok.dep !== p.value) return false;
    if (p.kind === 'depOneOf' && !p.value.includes(tok.dep)) return false;
    if (p.kind === 'inflectionForm' && tok.inflectionForm !== p.value) return false;
    if (p.kind === 'inflectionFormOneOf' && tok.inflectionForm && !p.value.includes(tok.inflectionForm)) return false;
    if (p.kind === 'conjugationClass' && tok.conjugationClass !== p.value) return false;
    if (p.kind === 'conjugationClassOneOf' && tok.conjugationClass && !p.value.includes(tok.conjugationClass)) return false;
    if (p.kind === 'tag' && tok.tag !== p.value) return false;
    if (p.kind === 'tagOneOf' && tok.tag && !p.value.includes(tok.tag)) return false;
    if (p.kind === 'textEqualsLemma' && (tok.text === tok.lemma) !== p.value) return false;
  }
  return true;
}

