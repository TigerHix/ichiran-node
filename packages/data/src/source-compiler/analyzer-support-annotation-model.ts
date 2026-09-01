import type {
  AnalyzerSupportRoute,
  AnalyzerSupportSplitPartSource
} from '../browser-pack/analyzer-support.js';

export interface AnnotationCandidate {
  readonly rootSeq: number;
  readonly route: AnalyzerSupportRoute;
  readonly surface: string;
  readonly form: string;
  readonly reading: string;
  readonly ord: number;
  readonly common: number | null;
  readonly ruleIds: readonly [number] | readonly [number, number] | null;
}

export type SplitAttributes = number | {
  readonly score: number;
  readonly primary?: number;
  readonly connector?: string;
  readonly root?: readonly number[];
};

export type SplitPartDefinition =
  | {
      readonly type: 'guard';
      readonly condition: (
        length: number,
        text: string,
        candidate: AnnotationCandidate
      ) => boolean;
    }
  | {
      readonly type: 'test';
      readonly condition: (
        length: number,
        text: string,
        candidate: AnnotationCandidate
      ) => boolean;
      readonly newScore?: number;
      readonly pushOnFail?: ':score' | ':pscore';
    }
  | { readonly type: 'marker'; readonly marker: ':score' | ':pscore' }
  | {
      readonly type: 'part';
      readonly seqs: number | readonly number[] | readonly [string, ...number[]];
      readonly lengthFn: (
        length: number,
        text: string,
        candidate: AnnotationCandidate
      ) => number | null;
      readonly conjP?: boolean;
      readonly modify?: boolean | ((text: string) => string);
    };

export interface SplitDeclaration {
  readonly seq: number;
  readonly score: SplitAttributes;
  readonly parts: readonly SplitPartDefinition[];
}

export type SplitRegistration = (declaration: SplitDeclaration) => void;

export interface SplitPartResolver {
  find(
    text: string,
    seqs: readonly number[],
    conjugated: boolean
  ): AnalyzerSupportSplitPartSource | null;
}
