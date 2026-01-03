export type { RuleSpec, CompiledRule, Trigger, CaptureValue, Clause, TokPred, NodeRef, CaptureSpec } from './dsl.js';
export { tokenMatchesPreds, V, text, textRe, textOneOf, lemma, lemmaRe, lemmaOneOf, pos, dep, inflectionForm, conjugationClass, conjugationClassOneOf, node, edge, next, before, not } from './dsl.js';
export { LinguisticRuleBuilder, linguisticRule } from './lang.js';
export type { TokenCond, LangVar } from './lang.js';
export { compileRule, deriveTriggers, explainMatch, buildSentenceIndex } from './compiler.js';
export type { ExplainResult, ExplainSuccess, ExplainFailure, SentenceIndex } from './compiler.js';

