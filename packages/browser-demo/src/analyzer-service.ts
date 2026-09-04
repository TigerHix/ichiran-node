import type {
  DictionaryEntry,
  DictionaryEntryOptions,
  RomanizeOptions,
  TokenConjugation,
  TokenDetails,
  TokenDetailsOptions,
  TokenMeaning
} from '@ichiran/core';
import {
  AnalyzerClient,
  AnalyzerClientError,
  parseDeployedRelease,
  type InstallProgressValue
} from './client.js';
import { fetchBoundedJson } from './bounded-json-fetch.js';
import type {
  AnalysisPath,
  AnalysisResult,
  AnalysisToken,
  AnalyzeOptions,
  AnalyzerPackManifest,
  PackStatus
} from './protocol.js';

export const ANALYZER_MANIFEST_URL = '/analyzer/manifest.json';
export const MAX_ANALYZER_TEXT_LENGTH = 4096;
export type AnalyzerProgress = InstallProgressValue;
export type AnalyzerStatus = PackStatus;
export type AnalyzerRelease = AnalyzerPackManifest;
export type { AnalysisPath, AnalysisResult, AnalysisToken, DictionaryEntry };
export type { TokenConjugation, TokenDetails, TokenMeaning };

export interface InitializedAnalyzer {
  readonly release: AnalyzerRelease | null;
  readonly status: AnalyzerStatus;
}

export function isInvalidInstallError(reason: unknown): reason is AnalyzerClientError {
  return reason instanceof AnalyzerClientError
    && (
      reason.code === 'corrupt-install'
      || reason.code === 'not-installed'
      || reason.code === 'stale-install'
    );
}

export function isTerminalAnalyzerError(reason: unknown): reason is AnalyzerClientError {
  return reason instanceof AnalyzerClientError
    && (
      reason.code === 'worker-crashed'
      || reason.code === 'worker-unavailable'
      || reason.code === 'worker-terminated'
    );
}

/** The product UI's only boundary to the analyzer Worker and on-device pack. */
export class BrowserAnalyzer {
  readonly #client: AnalyzerClient;

  constructor(client: AnalyzerClient) {
    this.#client = client;
  }

  async initialize(): Promise<InitializedAnalyzer> {
    try {
      const release = parseDeployedRelease(await fetchBoundedJson(
        ANALYZER_MANIFEST_URL,
        { cache: 'no-store' },
        'Analyzer release manifest'
      ));
      return {
        release,
        status: await this.#client.expectRelease(release)
      };
    } catch (releaseError) {
      const status = await this.#client.status();
      if (status.state === 'ready') return { release: null, status };
      throw releaseError;
    }
  }

  status(): Promise<AnalyzerStatus> {
    return this.#client.status();
  }

  install(progress: (value: AnalyzerProgress) => void): Promise<AnalyzerStatus> {
    return this.#client.install(ANALYZER_MANIFEST_URL, progress);
  }

  clear(): Promise<AnalyzerStatus> {
    return this.#client.clear();
  }

  analyze(text: string, options: AnalyzeOptions = { limit: 3 }): Promise<AnalysisResult> {
    return this.#client.analyze(text, options);
  }

  entry(entryIndex: number, options?: DictionaryEntryOptions): Promise<DictionaryEntry> {
    return this.#client.entry(entryIndex, options);
  }

  details(text: string, options: TokenDetailsOptions): Promise<TokenDetails> {
    return this.#client.details(text, options);
  }

  romanize(text: string, options?: RomanizeOptions): Promise<string> {
    return this.#client.romanize(text, options);
  }

  supersede(): void {
    this.#client.restart();
  }

  dispose(): void {
    this.#client.dispose();
  }
}
