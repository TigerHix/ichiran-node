import type {
  AnalyzeOptions,
  AnalysisResult,
  AnalyzerPackManifest,
  AnalyzerClientErrorCode,
  DictionaryEntry,
  DictionaryEntryOptions,
  InstallPhase,
  PackStatus,
  RomanizeOptions,
  TokenDetails,
  TokenDetailsOptions,
  WorkerRequest,
  WorkerResponse
} from './protocol.js';
import { parseAnalyzerReleaseManifest } from '@ichiran/core/release';
import { Sha256 } from './worker/sha256.js';

export function parseDeployedRelease(value: unknown): AnalyzerPackManifest {
  try {
    return parseAnalyzerReleaseManifest(
      value,
      text => new Sha256().update(new TextEncoder().encode(text)).digestHex()
    );
  } catch (error) {
    throw new AnalyzerClientError(
      'invalid-pack',
      error instanceof Error ? error.message : String(error)
    );
  }
}

export interface InstallProgressValue {
  readonly phase: InstallPhase;
  readonly receivedBytes: number;
  readonly totalBytes: number;
}

export class AnalyzerClientError extends Error {
  readonly code: AnalyzerClientErrorCode;

  constructor(code: AnalyzerClientErrorCode, message: string) {
    super(message);
    this.name = 'AnalyzerClientError';
    this.code = code;
  }
}

interface PendingRequest {
  readonly resolve: (value: unknown) => void;
  readonly reject: (error: Error) => void;
  readonly progress?: (value: InstallProgressValue) => void;
}

type WorkerRequestBody = WorkerRequest extends infer Request
  ? Request extends { readonly id: number }
    ? Omit<Request, 'id'>
    : never
  : never;

type AnalyzerWorkerFactory = () => Worker;

type InternalWorkerRequest = Readonly<Record<string, unknown>> & { readonly op: string };
const internalRequest = Symbol();

function createAnalyzerWorker(): Worker {
  return new Worker(new URL('./analyzer.worker.ts', import.meta.url), {
    type: 'module',
    name: 'ichiran-analyzer'
  });
}

/** One thin request map around the dedicated analyzer Worker. */
export class AnalyzerClient {
  #worker: Worker | null = null;
  #expectedRelease: AnalyzerPackManifest | null = null;
  #initializedWorker: Worker | null = null;
  #initialization: { readonly worker: Worker; readonly promise: Promise<PackStatus> } | null = null;
  #disposed = false;
  #nextId = 1;
  readonly #workerFactory: AnalyzerWorkerFactory;
  readonly #pending = new Map<number, PendingRequest>();

  constructor(workerFactory: AnalyzerWorkerFactory = createAnalyzerWorker) {
    this.#workerFactory = workerFactory;
  }

  /** Stops obsolete work so a newer user request does not wait behind it. */
  restart(): void {
    if (this.#disposed) return;
    this.#replaceWorker(new AnalyzerClientError(
      'request-superseded',
      'Analyzer request was replaced by newer input'
    ));
  }

  /** Pins every Worker incarnation to the authenticated currently published release. */
  async expectRelease(release: AnalyzerPackManifest): Promise<PackStatus> {
    const verifiedRelease = parseDeployedRelease(release);
    if (
      this.#worker
      && this.#expectedRelease?.manifestSha256 !== verifiedRelease.manifestSha256
    ) {
      this.#replaceWorker(new AnalyzerClientError(
        'release-changed',
        'The deployed analyzer release changed. Restarting the analyzer.'
      ));
    }
    this.#expectedRelease = verifiedRelease;
    const worker = this.#openWorker();
    const initialized = await this.#initializeWorker(worker);
    if (worker !== this.#worker) {
      throw new AnalyzerClientError(
        'worker-crashed',
        'Analyzer Worker stopped unexpectedly. Try again to restart it.'
      );
    }
    return initialized ?? await this.#requestOnWorker<PackStatus>(worker, { op: 'status' });
  }

  status(): Promise<PackStatus> {
    return this.#request({ op: 'status' });
  }

  install(
    manifestUrl: string,
    progress?: (value: InstallProgressValue) => void
  ): Promise<PackStatus> {
    return this.#request({ op: 'install', manifestUrl }, progress);
  }

  clear(): Promise<PackStatus> {
    return this.#request({ op: 'clear' });
  }

  analyze(text: string, options?: AnalyzeOptions): Promise<AnalysisResult> {
    return this.#request({ op: 'analyze', text, options });
  }

  details(text: string, options: TokenDetailsOptions): Promise<TokenDetails> {
    return this.#request({ op: 'details', text, options });
  }

  entry(entryIndex: number, options?: DictionaryEntryOptions): Promise<DictionaryEntry> {
    return options
      ? this.#request({ op: 'entry', entryIndex, options })
      : this.#request({ op: 'entry', entryIndex });
  }

  romanize(text: string, options?: RomanizeOptions): Promise<string> {
    return this.#request({ op: 'romanize', text, options });
  }

  /** @internal Keeps non-product probes on this client's exact Worker. */
  [internalRequest]<T>(body: InternalWorkerRequest): Promise<T> {
    return this.#request(body);
  }

  dispose(): void {
    if (this.#disposed) return;
    this.#disposed = true;
    this.#worker?.terminate();
    this.#worker = null;
    const error = new AnalyzerClientError('worker-terminated', 'Analyzer Worker was stopped');
    for (const pending of this.#pending.values()) pending.reject(error);
    this.#pending.clear();
  }

  #createWorker(): Worker {
    const worker = this.#workerFactory();
    worker.addEventListener('message', (event: MessageEvent<WorkerResponse>) => {
      if (worker !== this.#worker) return;
      const response = event.data;
      const pending = this.#pending.get(response.id);
      if (!pending) return;
      if (response.type === 'progress') {
        pending.progress?.({
          phase: response.phase,
          receivedBytes: response.receivedBytes,
          totalBytes: response.totalBytes
        });
        return;
      }
      this.#pending.delete(response.id);
      if (response.type === 'error') {
        pending.reject(new AnalyzerClientError(response.code, response.message));
      } else {
        pending.resolve(response.result);
      }
    });
    worker.addEventListener('error', event => {
      event.preventDefault();
      if (worker !== this.#worker) return;
      this.#replaceWorker(new AnalyzerClientError(
        'worker-crashed',
        'Analyzer Worker stopped unexpectedly. Try again to restart it.'
      ));
    });
    worker.addEventListener('messageerror', () => {
      if (worker !== this.#worker) return;
      this.#replaceWorker(new AnalyzerClientError(
        'worker-crashed',
        'Analyzer Worker returned unreadable data. Try again to restart it.'
      ));
    });
    return worker;
  }

  #replaceWorker(reason: AnalyzerClientError): void {
    const previous = this.#worker;
    this.#worker = null;
    previous?.terminate();
    for (const pending of this.#pending.values()) pending.reject(reason);
    this.#pending.clear();
    this.#initializedWorker = null;
    this.#initialization = null;
  }

  #openWorker(): Worker {
    if (this.#disposed) {
      throw new AnalyzerClientError('worker-terminated', 'Analyzer Worker was stopped');
    }
    if (this.#worker) return this.#worker;
    try {
      this.#worker = this.#createWorker();
      return this.#worker;
    } catch {
      throw new AnalyzerClientError(
        'worker-unavailable',
        'The analyzer could not restart. Reload this page and try again.'
      );
    }
  }

  async #initializeWorker(worker: Worker): Promise<PackStatus | null> {
    if (this.#initializedWorker === worker) return null;
    if (this.#initialization?.worker === worker) return this.#initialization.promise;
    const release = this.#expectedRelease;
    const request: WorkerRequestBody = release
      ? { op: 'expect-release', release }
      : { op: 'status' };
    const promise = this.#requestOnWorker<PackStatus>(worker, request).then(status => {
      if (worker === this.#worker && release === this.#expectedRelease) {
        this.#initializedWorker = worker;
      }
      return status;
    }).finally(() => {
      if (this.#initialization?.promise === promise) this.#initialization = null;
    });
    this.#initialization = { worker, promise };
    return promise;
  }

  async #request<T>(
    body: WorkerRequestBody | InternalWorkerRequest,
    progress?: (value: InstallProgressValue) => void
  ): Promise<T> {
    const worker = this.#openWorker();
    if (this.#initializedWorker === worker) {
      return this.#requestOnWorker(worker, body, progress);
    }
    const initialized = await this.#initializeWorker(worker);
    if (body.op === 'status' && initialized) return initialized as T;
    if (worker !== this.#worker) {
      throw new AnalyzerClientError(
        'worker-crashed',
        'Analyzer Worker stopped unexpectedly. Try again to restart it.'
      );
    }
    return this.#requestOnWorker(worker, body, progress);
  }

  #requestOnWorker<T>(
    worker: Worker,
    body: WorkerRequestBody | InternalWorkerRequest,
    progress?: (value: InstallProgressValue) => void
  ): Promise<T> {
    const id = this.#nextId++;
    return new Promise<T>((resolve, reject) => {
      this.#pending.set(id, {
        resolve: (value) => resolve(value as T),
        reject,
        progress
      });
      try {
        worker.postMessage({ ...body, id });
      } catch (reason) {
        this.#pending.delete(id);
        reject(new AnalyzerClientError(
          'worker-error',
          reason instanceof Error ? reason.message : String(reason)
        ));
      }
    });
  }
}

/** @internal Used only from modules excluded from product builds. */
export function requestClientInternal<T>(
  client: AnalyzerClient,
  body: InternalWorkerRequest
): Promise<T> {
  return client[internalRequest]<T>(body);
}
