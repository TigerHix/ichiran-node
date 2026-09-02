import type { CompiledMorphologyRule } from '../browser-pack/morphology-format.js';
import type { ConjugationProperty } from './model.js';

interface SourceRuleDeclaration extends ConjugationProperty {
  readonly order: number;
  readonly stem: number;
  readonly okuri: string;
  readonly euphr: string;
  readonly euphk: string;
}

/** Stable identity of one semantic conjugation property. */
export function conjugationPropertyKey(value: ConjugationProperty): string {
  return JSON.stringify([value.pos, value.type, value.negative, value.formal]);
}

export function sameConjugationProperty(
  left: ConjugationProperty,
  right: ConjugationProperty
): boolean {
  return left.pos === right.pos
    && left.type === right.type
    && left.negative === right.negative
    && left.formal === right.formal;
}

/** Stable identity of one concrete source-native rule declaration. */
export function emissionRuleKey(value: SourceRuleDeclaration): string {
  return JSON.stringify([
    value.pos, value.type, value.negative, value.formal, value.order,
    value.stem, value.okuri, value.euphr, value.euphk
  ]);
}

/** The same concrete identity at the compiled morphology boundary. */
export function compiledMorphologyRuleKey(value: CompiledMorphologyRule): string {
  return JSON.stringify([
    value.pos, value.type, value.negative, value.formal, value.ordinal,
    value.stem, value.okuri, value.euphr, value.euphk
  ]);
}

export function sameEmissionRule(
  left: SourceRuleDeclaration,
  right: SourceRuleDeclaration
): boolean {
  return sameConjugationProperty(left, right)
    && left.order === right.order
    && left.stem === right.stem
    && left.okuri === right.okuri
    && left.euphr === right.euphr
    && left.euphk === right.euphk;
}
