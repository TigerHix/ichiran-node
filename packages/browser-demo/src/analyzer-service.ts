import {
  MAX_ANALYZER_TEXT_LENGTH,
  validatePortableAnalyzeRequest,
  type DetailEntry
} from '@ichiran/core';
import {
  AnalyzerClient,
  AnalyzerClientError,
  parseDeployedRelease,
  type InstallProgressValue
} from './client.js';
import { fetchBoundedJson } from './bounded-json-fetch.js';
import type {
  AnalysisResult,
  AnalyzeOptions,
  AnalyzerPackManifest,
  PackStatus
} from './protocol.js';

export const ANALYZER_MANIFEST_URL = '/analyzer/manifest.json';
export { MAX_ANALYZER_TEXT_LENGTH };
export type DictionaryEntry = DetailEntry;
export type AnalyzerProgress = InstallProgressValue;
export type AnalyzerStatus = PackStatus;
export type AnalyzerRelease = AnalyzerPackManifest;
export type AnalyzerOutput = AnalysisResult;
export type AnalyzerPath = AnalyzerOutput['paths'][number];
export type AnalyzerToken = AnalyzerPath['tokens'][number];

export interface InitializedAnalyzer {
  readonly release: AnalyzerRelease;
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

  constructor(client = new AnalyzerClient()) {
    this.#client = client;
  }

  async initialize(): Promise<InitializedAnalyzer> {
    const release = parseDeployedRelease(await fetchBoundedJson(
      ANALYZER_MANIFEST_URL,
      { cache: 'no-store' },
      'Analyzer release manifest'
    ));
    return {
      release,
      status: await this.#client.expectRelease(release)
    };
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

  analyze(text: string, options: AnalyzeOptions = { limit: 3 }): Promise<AnalyzerOutput> {
    const validated = validatePortableAnalyzeRequest(text, options);
    return this.#client.analyze(validated.input, validated.options);
  }

  /** Named for the public API; maps to the transition client's describe operation. */
  entry(entryIndex: number): Promise<DictionaryEntry> {
    return this.#client.describe(entryIndex) as Promise<DictionaryEntry>;
  }

  romanize(text: string): Promise<string> {
    return this.#client.romanize(text);
  }

  supersede(): void {
    this.#client.restart();
  }

  dispose(): void {
    this.#client.dispose();
  }
}
