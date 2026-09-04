import type {
  AnalyzeOptions,
  AnalysisPath,
  AnalysisResult,
  AnalysisToken,
  AnalyzerErrorCode,
  DictionaryEntry,
  DictionaryEntryOptions,
  RomanizeOptions,
  TokenDetails,
  TokenDetailsOptions
} from '@ichiran/core';
import type {
  AnalyzerReleaseAsset,
  AnalyzerReleaseManifest
} from '@ichiran/core/release';

export type PackAssetManifest = AnalyzerReleaseAsset;
export type AnalyzerPackManifest = AnalyzerReleaseManifest;
export type {
  AnalyzeOptions,
  AnalysisPath,
  AnalysisResult,
  AnalysisToken,
  DictionaryEntry,
  DictionaryEntryOptions,
  RomanizeOptions,
  TokenDetails,
  TokenDetailsOptions
};

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
      readonly state: 'stale';
      readonly message: string;
      readonly installedPackVersion: string;
      readonly installedManifestSha256: string;
      readonly expectedPackVersion: string;
      readonly expectedManifestSha256: string;
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

export type AnalyzerClientErrorCode = AnalyzerErrorCode
  | 'corrupt-install'
  | 'insufficient-storage'
  | 'not-installed'
  | 'release-changed'
  | 'release-not-set'
  | 'request-superseded'
  | 'stale-install'
  | 'worker-crashed'
  | 'worker-error'
  | 'worker-terminated'
  | 'worker-unavailable';

export type WorkerRequest =
  | {
      readonly id: number;
      readonly op: 'expect-release';
      readonly release: AnalyzerPackManifest;
    }
  | { readonly id: number; readonly op: 'status' }
  | { readonly id: number; readonly op: 'install'; readonly manifestUrl: string }
  | { readonly id: number; readonly op: 'clear' }
  | {
      readonly id: number;
      readonly op: 'analyze';
      readonly text: string;
      readonly options?: AnalyzeOptions;
    }
  | {
      readonly id: number;
      readonly op: 'romanize';
      readonly text: string;
      readonly options?: RomanizeOptions;
    }
  | {
      readonly id: number;
      readonly op: 'details';
      readonly text: string;
      readonly options: TokenDetailsOptions;
    }
  | {
      readonly id: number;
      readonly op: 'entry';
      readonly entryIndex: number;
      readonly options?: DictionaryEntryOptions;
    };

export type WorkerResultByOperation = {
  readonly 'expect-release': PackStatus;
  readonly status: PackStatus;
  readonly install: PackStatus;
  readonly clear: PackStatus;
  readonly analyze: AnalysisResult;
  readonly romanize: string;
  readonly details: TokenDetails;
  readonly entry: DictionaryEntry;
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
      readonly code: AnalyzerClientErrorCode;
      readonly message: string;
    };
