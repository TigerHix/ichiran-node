import type {
  PortableAnalysisPath,
  PortableAnalysisResult,
  PortableAnalysisToken
} from '@ichiran/portable';

export interface PackAssetManifest {
  readonly file: string;
  readonly encoding: 'identity' | 'gzip';
  readonly downloadBytes: number;
  readonly downloadSha256: string;
  readonly installedBytes: number;
  readonly installedSha256: string;
}

export interface AnalyzerPackManifest {
  readonly formatVersion: 1;
  readonly packVersion: string;
  readonly sourceCommit: string;
  readonly sourcesLockSha256: string;
  readonly manifestSha256: string;
  readonly hot: PackAssetManifest;
  readonly details: PackAssetManifest;
}

export type InstallPhase =
  | 'downloading'
  | 'verifying'
  | 'installing'
  | 'opening';

export type PackStatus =
  | { readonly state: 'not-installed' }
  | {
      readonly state: 'incomplete' | 'corrupt';
      readonly message: string;
    }
  | {
      readonly state: 'ready';
      readonly packVersion: string;
      readonly manifestSha256: string;
      readonly downloadBytes: number;
      readonly installedBytes: number;
      readonly persistent: boolean;
      readonly workerOpen: boolean;
    };

export interface AnalyzeOptions {
  readonly limit?: number;
  readonly entities?: readonly {
    readonly start: number;
    readonly end: number;
    readonly boost?: number;
  }[];
  readonly normalizePunctuation?: boolean;
}

export type AnalysisToken = PortableAnalysisToken;
export type AnalysisPath = PortableAnalysisPath;
export type AnalysisResult = PortableAnalysisResult;

export interface BenchmarkGroupResult {
  readonly corpus: string;
  readonly samples: number;
  readonly p50Ms: number;
  readonly p95Ms: number;
  readonly maxMs: number;
  readonly rawMs: readonly number[];
}

export interface BenchmarkResult {
  /** Exact identity of the installed release measured by this report. */
  readonly release: AnalyzerPackManifest;
  readonly corpusVersion: 2;
  readonly warmupPasses: 2;
  readonly measuredPasses: 10;
  /** The only groups with alpha pass/fail thresholds. */
  readonly groups: readonly BenchmarkGroupResult[];
  /** Additional measurements retained for optimization, never release gates. */
  readonly diagnostics: {
    readonly analyzeGroups: readonly BenchmarkGroupResult[];
    readonly describe: BenchmarkGroupResult;
    readonly workerReadyMs: number | null;
    readonly firstAnalyzeMs: number | null;
  };
}

export type WorkerRequest =
  | { readonly id: number; readonly op: 'status' }
  | { readonly id: number; readonly op: 'install'; readonly manifestUrl: string }
  | { readonly id: number; readonly op: 'clear' }
  | { readonly id: number; readonly op: 'analyze'; readonly text: string; readonly options: AnalyzeOptions }
  | { readonly id: number; readonly op: 'legacy'; readonly text: string; readonly options: AnalyzeOptions }
  | { readonly id: number; readonly op: 'describe'; readonly entryIndex: number }
  | { readonly id: number; readonly op: 'romanize'; readonly text: string };

export type WorkerResultByOperation = {
  readonly status: PackStatus;
  readonly install: PackStatus;
  readonly clear: PackStatus;
  readonly analyze: AnalysisResult;
  readonly legacy: unknown;
  readonly describe: unknown;
  readonly romanize: string;
};

export type WorkerEvent = {
  readonly id: number;
  readonly type: 'progress';
  readonly phase: InstallPhase;
  readonly receivedBytes: number;
  readonly totalBytes: number;
};

export type WorkerResponse =
  | WorkerEvent
  | { readonly id: number; readonly type: 'result'; readonly result: unknown }
  | {
      readonly id: number;
      readonly type: 'error';
      readonly code: string;
      readonly message: string;
    };
