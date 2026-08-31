import {
  useEffect,
  useCallback,
  useMemo,
  useRef,
  useState,
  type KeyboardEvent,
  type ReactElement
} from 'react';
import {
  MAX_ANALYZER_TEXT_LENGTH,
  MAX_ANALYZER_WORD_LENGTH,
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
import { MAX_ENTITY_SPEC_LENGTH, parseEntityHints } from './entity-hints.js';
import type {
  AnalysisPath,
  AnalysisResult,
  AnalysisToken,
  AnalyzeOptions,
  AnalyzerPackManifest,
  BenchmarkResult,
  PackStatus
} from './protocol.js';

const MANIFEST_URL = '/analyzer/manifest.json';
const SAMPLE = '今日は公園で友達と話しました。';
const APP_VERSION = 'alpha.1';

export interface OfflineShellResult {
  readonly ready: boolean;
  readonly message?: string;
  readonly registration?: ServiceWorkerRegistration;
}

type OfflineShellState =
  | { readonly state: 'opening' }
  | { readonly state: 'ready' }
  | { readonly state: 'update-ready' }
  | { readonly state: 'error'; readonly message: string };

const POS_LABELS: Readonly<Record<string, string>> = {
  n: '名', 'n-adv': '名', 'n-pr': '名', 'n-t': '名',
  v1: '動', v5: '動', 'v5r': '動', 'vs': '動', 'vs-i': '動', 'vk': '動',
  adj: '形', 'adj-i': '形', 'adj-na': '形', adv: '副',
  prt: '助', pn: '代', conj: '接', cop: '繋', int: '感', aux: '助動',
  ctr: '助数', exp: '表現', num: '数', suf: '接辞', pref: '接辞'
};

function formatBytes(bytes: number): string {
  if (bytes < 1024) return `${bytes} B`;
  const units = ['KiB', 'MiB', 'GiB'];
  let value = bytes / 1024;
  let unit = 0;
  while (value >= 1024 && unit < units.length - 1) {
    value /= 1024;
    unit++;
  }
  return `${value >= 10 ? value.toFixed(1) : value.toFixed(2)} ${units[unit]}`;
}

function isPackInvalidError(reason: unknown): reason is AnalyzerClientError {
  return reason instanceof AnalyzerClientError
    && (
      reason.code === 'corrupt-install'
      || reason.code === 'not-installed'
      || reason.code === 'stale-install'
    );
}

function isTerminalWorkerError(reason: unknown): reason is AnalyzerClientError {
  return reason instanceof AnalyzerClientError
    && (
      reason.code === 'worker-crashed'
      || reason.code === 'worker-unavailable'
      || reason.code === 'worker-terminated'
    );
}

function posLabel(pos: readonly string[]): string {
  for (const value of pos) {
    const direct = POS_LABELS[value];
    if (direct) return direct;
    const prefix = Object.entries(POS_LABELS).find(([key]) => value.startsWith(key));
    if (prefix) return prefix[1];
  }
  return '語';
}

function posTone(pos: readonly string[]): string {
  const joined = pos.join(' ');
  if (/\bv|verb/.test(joined)) return 'verb';
  if (/adj/.test(joined)) return 'adjective';
  if (/prt|particle/.test(joined)) return 'particle';
  return 'noun';
}

function DownloadIcon(): ReactElement {
  return (
    <svg viewBox="0 0 24 24" aria-hidden="true">
      <path d="M12 3v11m0 0 4-4m-4 4-4-4M5 18.5h14" />
    </svg>
  );
}

function StorageDownloadIcon(): ReactElement {
  return (
    <svg viewBox="0 0 32 32" aria-hidden="true">
      <path d="M5 7.5c0-2.2 4.5-4 10-4s10 1.8 10 4-4.5 4-10 4-10-1.8-10-4Zm0 0v7c0 2.2 4.5 4 10 4 2.1 0 4-.3 5.6-.8M5 14.5v7c0 2.2 4.5 4 10 4 1.4 0 2.7-.1 3.9-.4" />
      <circle cx="24.5" cy="23.5" r="6" />
      <path d="M24.5 20v7m0 0 2.5-2.5M24.5 27 22 24.5" />
    </svg>
  );
}

function DataIcon({ kind }: { kind: 'book' | 'grid' | 'document' }): ReactElement {
  if (kind === 'grid') {
    return <svg viewBox="0 0 24 24" aria-hidden="true"><path d="M4 4h6v6H4zm10 0h6v6h-6zM4 14h6v6H4zm10 0h6v6h-6z" /></svg>;
  }
  if (kind === 'document') {
    return <svg viewBox="0 0 24 24" aria-hidden="true"><path d="M7 3h7l4 4v14H7zM14 3v5h4M10 12h5m-5 4h5" /></svg>;
  }
  return <svg viewBox="0 0 24 24" aria-hidden="true"><path d="M4 5.5A3.5 3.5 0 0 1 7.5 2H11v17H7.5A3.5 3.5 0 0 0 4 22zm16 0A3.5 3.5 0 0 0 16.5 2H13v17h3.5A3.5 3.5 0 0 1 20 22z" /></svg>;
}

function AppHeader({
  status,
  offlineShell
}: {
  status: PackStatus | null;
  offlineShell: OfflineShellState;
}): ReactElement {
  const packReady = status?.state === 'ready';
  const offlineReady = offlineShell.state === 'ready'
    || offlineShell.state === 'update-ready';
  const ready = packReady && offlineReady;
  const statusText = status === null
    ? 'Opening analyzer…'
    : offlineShell.state === 'update-ready'
      ? 'Update available'
    : ready
      ? 'Ready offline'
      : packReady && offlineShell.state === 'opening'
        ? 'Preparing offline…'
        : packReady && offlineShell.state === 'error'
          ? 'Offline shell unavailable'
      : status.state === 'incomplete' || status.state === 'corrupt' || status.state === 'stale'
        ? 'Needs reinstall'
        : 'Not installed';
  return (
    <header className="app-header">
      <a className="wordmark" href="/" aria-label="Browser Analyzer Alpha home">Browser Analyzer <span>Alpha</span></a>
      <div className={`status ${ready ? 'status-ready' : ''}`}>
        <span aria-hidden="true" />
        {statusText}
      </div>
    </header>
  );
}

function UpdateNotice({ state }: { state: OfflineShellState }): ReactElement | null {
  if (state.state !== 'update-ready') return null;
  return (
    <section className="update-notice" aria-live="polite">
      <span><strong>App update downloaded</strong> You can keep using this version. Close every analyzer tab, then reopen one to switch safely.</span>
    </section>
  );
}

interface InstallPanelProps {
  manifest: AnalyzerPackManifest | null;
  manifestError: string | null;
  status: PackStatus | null;
  progress: InstallProgressValue | null;
  error: { readonly code: string; readonly message: string } | null;
  offlineShell: OfflineShellState;
  onInstall(): void;
  onClear(): void;
}

function InstallPanel(props: InstallPanelProps): ReactElement {
  const {
    manifest, manifestError, status, progress, error, offlineShell, onInstall, onClear
  } = props;
  const total = manifest ? manifest.hot.downloadBytes + manifest.details.downloadBytes : 0;
  const installed = manifest ? manifest.hot.installedBytes + manifest.details.installedBytes : 0;
  const busy = progress !== null;
  const broken = status?.state === 'incomplete'
    || status?.state === 'corrupt'
    || status?.state === 'stale';
  const offlineReady = offlineShell.state === 'ready'
    || offlineShell.state === 'update-ready';
  const phaseCopy = progress?.phase === 'downloading'
    ? 'Downloading analyzer data'
    : progress?.phase === 'verifying'
      ? 'Verifying download'
      : progress?.phase === 'installing'
        ? 'Installing for offline use'
        : progress?.phase === 'opening'
          ? 'Opening analyzer'
          : null;
  const percent = progress && progress.totalBytes > 0
    ? Math.min(100, Math.round(progress.receivedBytes / progress.totalBytes * 100))
    : 0;

  return (
    <main className="install-layout">
      <section className="install-panel" aria-labelledby="install-title">
        <div className="install-symbol"><StorageDownloadIcon /></div>
        <h1 id="install-title">Japanese analyzer data</h1>
        <p className="install-subtitle">Download once, then analyze Japanese entirely on this device.</p>
        <div className="included-data">
          <div><DataIcon kind="book" /><span><strong>Dictionary &amp; readings</strong><small>Forms, readings, and lexical identity</small></span></div>
          <div><DataIcon kind="grid" /><span><strong>Conjugation &amp; scoring</strong><small>Complete analyzer behavior, no server</small></span></div>
          <div><DataIcon kind="document" /><span><strong>Complete senses &amp; glosses</strong><small>Available offline when you inspect a word</small></span></div>
        </div>

        {status?.state === 'stale'
          ? <p className="inline-error">Analyzer data is from an earlier app release. <small>{status.message}</small></p>
          : broken && <p className="inline-error">Analyzer data is incomplete or corrupted.</p>}
        {error?.code === 'insufficient-storage'
          ? <p className="inline-error">Not enough device storage to install analyzer data. <small>{error.message}</small></p>
          : error?.code === 'clear-error'
            ? <p className="inline-error">Analyzer data could not be cleared. <small>{error.message}</small></p>
          : error && <p className="inline-error">Analyzer data could not be installed. <small>{error.message}</small></p>}
        {manifestError && <p className="inline-error">{manifestError}</p>}
        {offlineShell.state === 'opening' && (
          <p className="runtime-message" aria-live="polite">Preparing the offline app shell…</p>
        )}
        {offlineShell.state === 'error' && (
          <p className="inline-error">Offline app shell could not be prepared. <small>{offlineShell.message}</small></p>
        )}

        {busy && progress && (
          <div className="install-progress" aria-live="polite">
            <div><strong>{phaseCopy}…</strong><span>{percent}%</span></div>
            <progress max={progress.totalBytes} value={progress.receivedBytes} />
            <small>{formatBytes(progress.receivedBytes)} of {formatBytes(progress.totalBytes)}</small>
          </div>
        )}

        {!busy && (
          <button
            className="primary install-action"
            type="button"
            onClick={onInstall}
            disabled={!manifest || !offlineReady}
          >
            <DownloadIcon />{broken ? 'Reinstall analyzer data' : error ? 'Retry' : 'Install analyzer data'}
          </button>
        )}

        <div className="install-meta">
          <div><span>Download size</span><strong>{manifest ? formatBytes(total) : '—'}</strong></div>
          <div><span>On-device size</span><strong>{manifest ? formatBytes(installed) : '—'}</strong></div>
        </div>
        <p className="privacy">No account. No text leaves this device.</p>
        {manifest && (
          <details className="data-details">
            <summary>Data details</summary>
            <p>Pack {manifest.packVersion} · manifest {manifest.manifestSha256.slice(0, 12)}</p>
          </details>
        )}
        {broken && !busy && <button className="text-button danger" type="button" onClick={onClear}>Clear installed data</button>}
      </section>
    </main>
  );
}

function TokenButton({
  token,
  index,
  selected,
  onSelect
}: {
  token: AnalysisToken;
  index: number;
  selected: boolean;
  onSelect(): void;
}): ReactElement {
  if (token.entryIndex === null && token.pos.length === 0) {
    return <span className="punctuation">{token.text}</span>;
  }
  const label = posLabel(token.pos);
  const accessible = token.reading && token.reading !== token.text
    ? `${token.text}, reading ${token.reading}, ${token.pos.join(', ') || 'word'}`
    : `${token.text}, ${token.pos.join(', ') || 'word'}`;
  return (
    <button
      className={`token token-${posTone(token.pos)}`}
      type="button"
      aria-label={accessible}
      aria-pressed={selected}
      data-token-index={index}
      onClick={onSelect}
    >
      <span className="furigana">{token.reading === token.text ? '\u00a0' : token.reading}</span>
      <span className="token-surface">{token.text}</span>
      <span className="token-pos">{label}</span>
    </button>
  );
}

function Sentence({
  path,
  selected,
  onSelect
}: {
  path: AnalysisPath;
  selected: number | null;
  onSelect(index: number): void;
}): ReactElement {
  return (
    <div className="sentence" lang="ja">
      {path.tokens.map((token, index) => (
        <TokenButton
          key={`${token.start}:${token.end}:${index}`}
          token={token}
          index={index}
          selected={selected === index}
          onSelect={() => onSelect(index)}
        />
      ))}
    </div>
  );
}

function Inspector({ token, details, onClose }: {
  token: AnalysisToken | null;
  details: DetailEntry | null;
  onClose(): void;
}): ReactElement {
  useEffect(() => {
    if (!token) return;
    const keyDown = (event: globalThis.KeyboardEvent) => {
      if (event.key === 'Escape') onClose();
    };
    window.addEventListener('keydown', keyDown);
    return () => window.removeEventListener('keydown', keyDown);
  }, [onClose, token]);

  if (!token) {
    return <aside className="inspector inspector-empty"><p>Tap a word for details</p></aside>;
  }
  return (
    <aside className="inspector" aria-label="Word details" aria-live="polite">
      <button className="sheet-close" type="button" aria-label="Close word details" onClick={onClose}>×</button>
      <p className="inspector-reading" lang="ja">{token.reading}</p>
      <h2 lang="ja">{token.text}</h2>
      <div className="tags">
        {token.pos.map(value => <span key={value}>{value}</span>)}
        {token.inflection.map((value, index) => <span key={`${value.pos}:${value.type}:${index}`}>type {value.type}</span>)}
      </div>
      {details && details.forms.length > 0 && (
        <section className="dictionary-forms">
          <p>Dictionary forms</p>
          <div>
            {details.forms.map(form => (
              <span key={`${form.route}:${form.ord}:${form.text}`}>
                <strong lang="ja">{form.text}</strong>
                <small>{form.route === 'kanji' ? 'written' : 'reading'}{form.common !== null ? ' · common' : ''}</small>
              </span>
            ))}
          </div>
        </section>
      )}
      {details?.senses.map((sense, index) => (
        <section className="sense" key={`${sense.ord}:${index}`}>
          <h3>{index + 1}</h3>
          <div>
            {sense.glosses.map(gloss => <p key={`${gloss.ord}:${gloss.text}`}>{gloss.text}</p>)}
            {sense.properties.length > 0 && (
              <small>{sense.properties.map(property => `${property.tag}: ${property.text}`).join(' · ')}</small>
            )}
          </div>
        </section>
      ))}
      {token.root && (
        <section className="base-form">
          <p>Base form</p>
          <strong lang="ja">{token.root.form}</strong>
          <span lang="ja">{token.root.reading}</span>
        </section>
      )}
      {token.components.length > 0 && (
        <section className="token-structure">
          <p>Structure</p>
          {token.components.map((component, index) => (
            <div key={`${component.text}:${component.entryIndex}:${index}`}>
              <strong lang="ja">{component.text}</strong>
              <span lang="ja">{component.reading}</span>
              {component.root && <small>base {component.root.form}</small>}
            </div>
          ))}
        </section>
      )}
      {token.inflection.length > 0 && (
        <section className="conjugation-path">
          <p>Conjugation path</p>
          {token.inflection.map((step, index) => (
            <div key={`${step.pos}:${step.type}:${index}`}><span>{index + 1}</span><strong>{step.pos}</strong><small>type {step.type}</small></div>
          ))}
        </section>
      )}
      {token.alternatives.length > 0 && (
        <section className="token-alternatives">
          <p>Alternative readings</p>
          {token.alternatives.map(alternative => (
            <div key={alternative.candidateId}>
              <span><strong lang="ja">{alternative.text}</strong><small lang="ja">{alternative.reading}</small></span>
              <em>{alternative.score}</em>
            </div>
          ))}
        </section>
      )}
    </aside>
  );
}

function Workspace({
  status,
  manifest,
  client,
  onClear,
  onPackInvalid,
  operationError
}: {
  status: Extract<PackStatus, { state: 'ready' }>;
  manifest: AnalyzerPackManifest | null;
  client: AnalyzerClient;
  onClear(): void;
  onPackInvalid(): void;
  operationError: { readonly code: string; readonly message: string } | null;
}): ReactElement {
  const [text, setText] = useState(SAMPLE);
  const [limit, setLimit] = useState(1);
  const [entitySpec, setEntitySpec] = useState('');
  const [normalizePunctuation, setNormalizePunctuation] = useState(false);
  const [result, setResult] = useState<AnalysisResult | null>(null);
  const [pathIndex, setPathIndex] = useState(0);
  const [selected, setSelected] = useState<number | null>(null);
  const [details, setDetails] = useState<DetailEntry | null>(null);
  const [running, setRunning] = useState(false);
  const [runningIntent, setRunningIntent] = useState<string | null>(null);
  const [showBusy, setShowBusy] = useState(false);
  const [error, setError] = useState<string | null>(null);
  const [wallMs, setWallMs] = useState<number | null>(null);
  const [benchmark, setBenchmark] = useState<BenchmarkResult | null>(null);
  const [benchmarkRunning, setBenchmarkRunning] = useState(false);
  const [romanization, setRomanization] = useState<string | null>(null);
  const [runtimeMessage, setRuntimeMessage] = useState<string | null>(null);
  const [lastOptions, setLastOptions] = useState<AnalyzeOptions>({ limit: 1 });
  const request = useRef(0);
  const benchmarkRequest = useRef(0);
  const benchmarkActive = useRef(false);
  const activeIntent = useRef<string | null>(null);
  const selectedToken = selected === null ? null : result?.paths[pathIndex]?.tokens[selected] ?? null;
  const path = result?.paths[pathIndex] ?? null;
  const intentKey = JSON.stringify([text, limit, entitySpec, normalizePunctuation]);
  const latestIntent = useRef(intentKey);
  latestIntent.current = intentKey;

  useEffect(() => {
    if (!running) {
      setShowBusy(false);
      return;
    }
    const timer = window.setTimeout(() => setShowBusy(true), 120);
    return () => window.clearTimeout(timer);
  }, [running]);

  useEffect(() => {
    setDetails(null);
    if (!selectedToken || selectedToken.entryIndex === null) return;
    let current = true;
    void client.describe(selectedToken.entryIndex).then(value => {
      if (current) setDetails(value as DetailEntry);
    }, reason => {
      if (current) setDetails(null);
      if (isPackInvalidError(reason)) onPackInvalid();
    });
    return () => { current = false; };
  }, [client, onPackInvalid, selectedToken]);

  async function analyze(): Promise<void> {
    const intent = intentKey;
    if (!text.trim() || activeIntent.current === intent) return;
    const stoppedBenchmark = benchmarkActive.current;
    if (activeIntent.current !== null || benchmarkActive.current) {
      ++request.current;
      ++benchmarkRequest.current;
      activeIntent.current = null;
      benchmarkActive.current = false;
      client.restart();
      setRunning(false);
      setRunningIntent(null);
      setBenchmarkRunning(false);
      if (stoppedBenchmark) setRuntimeMessage('Benchmark stopped for the new analysis.');
    }
    let options: AnalyzeOptions;
    try {
      const entities = parseEntityHints(entitySpec, text.length);
      options = validatePortableAnalyzeRequest(text, {
        limit,
        ...(entities.length > 0 ? { entities } : {}),
        ...(normalizePunctuation ? { normalizePunctuation: true } : {})
      }).options;
    } catch (reason) {
      setError(reason instanceof Error ? reason.message : String(reason));
      return;
    }
    const id = ++request.current;
    activeIntent.current = intent;
    const started = performance.now();
    setRunning(true);
    setRunningIntent(intent);
    setError(null);
    try {
      const next = await client.analyze(text, options);
      if (id !== request.current || intent !== latestIntent.current) return;
      setResult(next);
      setPathIndex(0);
      setSelected(next.paths[0]?.tokens.length === 1 ? 0 : null);
      setWallMs(performance.now() - started);
      setLastOptions(options);
    } catch (reason) {
      if (id !== request.current || intent !== latestIntent.current) return;
      if (isPackInvalidError(reason)) onPackInvalid();
      setError(reason instanceof Error ? reason.message : String(reason));
    } finally {
      if (id === request.current) {
        activeIntent.current = null;
        setRunning(false);
        setRunningIntent(null);
      }
    }
  }

  function keyDown(event: KeyboardEvent<HTMLTextAreaElement>): void {
    if (event.key === 'Enter' && (event.metaKey || event.ctrlKey)) {
      event.preventDefault();
      void analyze();
    }
  }

  function choosePath(index: number): void {
    setPathIndex(index);
    setSelected(null);
    setDetails(null);
  }

  function closeInspector(): void {
    const tokenIndex = selected;
    setSelected(null);
    if (tokenIndex !== null) {
      window.requestAnimationFrame(() => {
        document.querySelector<HTMLButtonElement>(`[data-token-index="${tokenIndex}"]`)?.focus();
      });
    }
  }

  async function copyLegacy(): Promise<void> {
    if (!result) return;
    setRuntimeMessage('Preparing legacy JSON…');
    try {
      const legacy = await client.legacy(result.input, {
        ...lastOptions,
        limit: Math.max(1, result.paths.length)
      });
      await navigator.clipboard.writeText(JSON.stringify(legacy, null, 2));
      setRuntimeMessage('Legacy JSON copied.');
    } catch (reason) {
      if (isPackInvalidError(reason)) onPackInvalid();
      setRuntimeMessage(`Legacy serialization failed: ${reason instanceof Error ? reason.message : String(reason)}`);
    }
  }

  async function copyClean(): Promise<void> {
    if (!result) return;
    try {
      await navigator.clipboard.writeText(JSON.stringify(result, null, 2));
      setRuntimeMessage('Clean JSON copied.');
    } catch (reason) {
      setRuntimeMessage(`Copy failed: ${reason instanceof Error ? reason.message : String(reason)}`);
    }
  }

  async function runBenchmark(): Promise<void> {
    if (benchmarkActive.current) return;
    if (!manifest || manifest.manifestSha256 !== status.manifestSha256) {
      setRuntimeMessage('Benchmark unavailable: the installed release manifest could not be verified.');
      return;
    }
    const id = ++benchmarkRequest.current;
    benchmarkActive.current = true;
    setBenchmarkRunning(true);
    setRuntimeMessage('Running the fixed corpus in the analyzer Worker…');
    try {
      const nextBenchmark = await client.benchmark(manifest);
      if (id !== benchmarkRequest.current) return;
      setBenchmark(nextBenchmark);
      setRuntimeMessage('Benchmark complete.');
    } catch (reason) {
      if (id !== benchmarkRequest.current) return;
      if (isPackInvalidError(reason)) onPackInvalid();
      setRuntimeMessage(`Benchmark failed: ${reason instanceof Error ? reason.message : String(reason)}`);
    } finally {
      if (id === benchmarkRequest.current) {
        benchmarkActive.current = false;
        setBenchmarkRunning(false);
      }
    }
  }

  async function romanizeResult(): Promise<void> {
    if (!result) return;
    setRuntimeMessage('Romanizing the analyzed input…');
    try {
      setRomanization(await client.romanize(result.input));
      setRuntimeMessage('Romanization complete.');
    } catch (reason) {
      if (isPackInvalidError(reason)) onPackInvalid();
      setRuntimeMessage(`Romanization failed: ${reason instanceof Error ? reason.message : String(reason)}`);
    }
  }

  function downloadBenchmark(): void {
    if (!benchmark) return;
    const url = URL.createObjectURL(new Blob(
      [`${JSON.stringify(benchmark, null, 2)}\n`],
      { type: 'application/json' }
    ));
    const link = document.createElement('a');
    link.href = url;
    link.download = 'ichiran-browser-alpha-benchmark.json';
    link.click();
    URL.revokeObjectURL(url);
  }

  return (
    <main className="workspace">
      {operationError && (
        <p className="inline-error operation-error" role="alert">
          {operationError.code === 'clear-error'
            ? 'Device data could not be cleared. '
            : 'Analyzer data could not be replaced; the previous pack is still available. '}
          <small>{operationError.message}</small>
        </p>
      )}
      <section className="composer" aria-labelledby="composer-title">
        <div className="section-heading">
          <h1 id="composer-title"><label htmlFor="japanese-input">Japanese text</label></h1>
          <button type="button" className="text-button" onClick={() => setText(SAMPLE)}>Use sample</button>
        </div>
        <div className="textarea-wrap">
          <textarea
            id="japanese-input"
            value={text}
            onChange={event => setText(event.target.value)}
            onKeyDown={keyDown}
            lang="ja"
            rows={4}
            maxLength={MAX_ANALYZER_TEXT_LENGTH}
            aria-describedby="input-guidance"
            placeholder="Paste or type Japanese text"
          />
          {text && <button className="clear-input" type="button" onClick={() => setText('')} aria-label="Clear Japanese text">×</button>}
        </div>
        <p id="input-guidance" className="input-guidance">
          {text.length.toLocaleString()} / {MAX_ANALYZER_TEXT_LENGTH.toLocaleString()} text units
          {' · '}uninterrupted word runs up to {MAX_ANALYZER_WORD_LENGTH.toLocaleString()}
        </p>
        <div className="composer-actions">
          <details className="advanced">
            <summary>Advanced</summary>
            <div>
              <label>Top results <select value={limit} onChange={event => setLimit(Number(event.target.value))}>{[1, 2, 3, 4, 5].map(value => <option key={value}>{value}</option>)}</select></label>
              <label className="entity-field">Entity spans <input value={entitySpec} onChange={event => setEntitySpec(event.target.value)} placeholder="0:2:120" inputMode="text" maxLength={MAX_ENTITY_SPEC_LENGTH} /></label>
              <label className="check-field"><input type="checkbox" checked={normalizePunctuation} onChange={event => setNormalizePunctuation(event.target.checked)} /> Normalize punctuation</label>
            </div>
          </details>
          <button className="primary" type="button" onClick={() => void analyze()} disabled={!text.trim() || runningIntent === intentKey}>Analyze</button>
        </div>
        <div className="analysis-status" aria-live="polite">
          {showBusy && 'Analyzing…'}
          {error && (
            <span className="inline-error">
              Analysis failed. Your installed data was not changed. <small>{error}</small>
              <button type="button" className="text-button" onClick={() => void analyze()}>Try again</button>
            </span>
          )}
        </div>
      </section>

      <section className="result-workspace" aria-label="Analysis result">
        <div className="result-column">
          <div className="result-heading">
            <h2>Analysis</h2>
            {path && <span>{wallMs?.toFixed(1)} ms · score {path.score}</span>}
          </div>
          {path
            ? <Sentence path={path} selected={selected} onSelect={setSelected} />
            : <p className="empty-result">{result ? 'No Japanese analysis was found.' : 'Enter Japanese text to begin.'}</p>}
          {result && result.paths.length > 1 && (
            <details className="alternatives">
              <summary>Alternatives <span>{result.paths.length - 1}</span></summary>
              {result.paths.map((alternative, index) => (
                <button type="button" key={index} className={index === pathIndex ? 'active' : ''} onClick={() => choosePath(index)}>
                  <span lang="ja">{alternative.tokens.map(token => token.text).join(' · ')}</span>
                  <strong>{alternative.score}</strong>
                </button>
              ))}
            </details>
          )}
        </div>
        <Inspector token={selectedToken} details={details} onClose={closeInspector} />
      </section>

      <details className="runtime-panel">
        <summary>Runtime &amp; data</summary>
        <dl>
          <div><dt>Pack</dt><dd>{status.packVersion}</dd></div>
          <div><dt>App</dt><dd>{APP_VERSION}</dd></div>
          <div><dt>Manifest</dt><dd>{status.manifestSha256.slice(0, 12)}</dd></div>
          <div><dt>One-time download</dt><dd>{formatBytes(status.downloadBytes)}</dd></div>
          <div><dt>Installed</dt><dd>{formatBytes(status.installedBytes)}</dd></div>
          <div><dt>Worker</dt><dd>{status.workerOpen ? 'Open' : 'Closed'}</dd></div>
          <div><dt>Persistent storage</dt><dd>{status.persistent ? 'Granted' : 'Best effort'}</dd></div>
          {result && <div><dt>Worker compute</dt><dd>{result.computeMs.toFixed(1)} ms</dd></div>}
          {result && <div><dt>Request</dt><dd>{result.input.length} units · top {lastOptions.limit ?? 1} · {lastOptions.entities?.length ?? 0} boosts</dd></div>}
          {romanization !== null && <div><dt>Romanization</dt><dd>{romanization}</dd></div>}
          <div><dt>Performance gate</dt><dd>Measured externally</dd></div>
          {benchmark?.groups.map(group => (
            <div key={group.corpus}><dt>{group.corpus} p95</dt><dd>{group.p95Ms.toFixed(1)} ms</dd></div>
          ))}
        </dl>
        {runtimeMessage && <p className="runtime-message" aria-live="polite">{runtimeMessage}</p>}
        <div className="runtime-actions">
          {result && <button type="button" className="secondary" onClick={() => void copyClean()}>Copy clean JSON</button>}
          {result && <button type="button" className="secondary" onClick={() => void copyLegacy()}>Copy legacy JSON</button>}
          {result && <button type="button" className="secondary" onClick={() => void romanizeResult()}>Romanize input</button>}
          <button type="button" className="secondary" disabled={benchmarkRunning} onClick={() => void runBenchmark()}>
            {benchmarkRunning ? 'Running benchmark…' : 'Run benchmark'}
          </button>
          {benchmark && <button type="button" className="secondary" onClick={downloadBenchmark}>Download benchmark JSON</button>}
          <button type="button" className="text-button danger" onClick={onClear}>Clear installed data</button>
        </div>
      </details>
    </main>
  );
}

function supportsRequiredFeatures(): boolean {
  return typeof Worker === 'function'
    && 'serviceWorker' in navigator
    && 'storage' in navigator
    && typeof navigator.storage.getDirectory === 'function'
    && 'locks' in navigator
    && 'indexedDB' in window
    && 'DecompressionStream' in window
    && 'FileSystemFileHandle' in window
    && typeof FileSystemFileHandle.prototype.createWritable === 'function';
}

export function App({
  offlineShellReady
}: {
  offlineShellReady: Promise<OfflineShellResult>;
}): ReactElement {
  const supported = supportsRequiredFeatures();
  const client = useMemo(() => supported ? new AnalyzerClient() : null, [supported]);
  const [status, setStatus] = useState<PackStatus | null>(null);
  const [manifest, setManifest] = useState<AnalyzerPackManifest | null>(null);
  const [manifestError, setManifestError] = useState<string | null>(null);
  const [progress, setProgress] = useState<InstallProgressValue | null>(null);
  const [installError, setInstallError] = useState<{ code: string; message: string } | null>(null);
  const [offlineShell, setOfflineShell] = useState<OfflineShellState>({ state: 'opening' });

  useEffect(() => {
    let current = true;
    if (!('serviceWorker' in navigator)) {
      void offlineShellReady.then(result => {
        if (!current) return;
        setOfflineShell(result.ready
          ? { state: 'ready' }
          : { state: 'error', message: result.message ?? 'Service Workers are unavailable.' });
      });
      return () => { current = false; };
    }

    let registration: ServiceWorkerRegistration | null = null;
    let watchedWorker: ServiceWorker | null = null;
    let previousController = navigator.serviceWorker.controller;

    const showWaitingUpdate = (): void => {
      if (!current || !registration?.waiting || !navigator.serviceWorker.controller) return;
      setOfflineShell({ state: 'update-ready' });
    };
    const workerStateChanged = (): void => {
      if (watchedWorker?.state === 'installed') showWaitingUpdate();
    };
    const watchInstallingWorker = (): void => {
      watchedWorker?.removeEventListener('statechange', workerStateChanged);
      watchedWorker = registration?.installing ?? null;
      watchedWorker?.addEventListener('statechange', workerStateChanged);
    };
    const controllerChanged = (): void => {
      const nextController = navigator.serviceWorker.controller;
      if (previousController && nextController && previousController !== nextController) {
        window.location.reload();
        return;
      }
      previousController = nextController;
    };

    navigator.serviceWorker.addEventListener('controllerchange', controllerChanged);
    void offlineShellReady.then(result => {
      if (!current) return;
      registration = result.registration ?? null;
      setOfflineShell(result.ready
        ? { state: 'ready' }
        : { state: 'error', message: result.message ?? 'Service Worker registration failed.' });
      registration?.addEventListener('updatefound', watchInstallingWorker);
      watchInstallingWorker();
      showWaitingUpdate();
    });
    return () => {
      current = false;
      watchedWorker?.removeEventListener('statechange', workerStateChanged);
      registration?.removeEventListener('updatefound', watchInstallingWorker);
      navigator.serviceWorker.removeEventListener('controllerchange', controllerChanged);
    };
  }, [offlineShellReady]);

  useEffect(() => {
    if (!supported || !client) return;
    let current = true;
    void (async () => {
      const nextManifest = parseDeployedRelease(await fetchBoundedJson(
        MANIFEST_URL,
        { cache: 'no-store' },
        'Analyzer release manifest'
      ));
      const nextStatus = await client.expectRelease(nextManifest);
      if (!current) return;
      setManifest(nextManifest);
      setStatus(nextStatus);
      setManifestError(null);
    })().catch(reason => {
      if (current) setManifestError(reason instanceof Error ? reason.message : String(reason));
    });
    return () => {
      current = false;
      client.dispose();
    };
  }, [client, supported]);

  async function install(): Promise<void> {
    if (!client) return;
    setInstallError(null);
    setProgress({ phase: 'downloading', receivedBytes: 0, totalBytes: manifest ? manifest.hot.downloadBytes + manifest.details.downloadBytes : 1 });
    try {
      if (typeof navigator.storage.persist === 'function') {
        try { await navigator.storage.persist(); } catch { /* Best-effort storage remains supported. */ }
      }
      const next = await client.install(MANIFEST_URL, setProgress);
      setStatus(next);
    } catch (reason) {
      setInstallError(reason instanceof AnalyzerClientError
        ? { code: reason.code, message: reason.message }
        : { code: 'install-error', message: String(reason) });
      if (isTerminalWorkerError(reason)) return;
      try {
        setStatus(await client.status());
      } catch {
        setStatus({ state: 'incomplete', message: 'Reload this page to restart the analyzer.' });
      }
    } finally {
      setProgress(null);
    }
  }

  async function clear(): Promise<void> {
    if (!client) return;
    if (!window.confirm('Clear the installed analyzer data from this device?')) return;
    setInstallError(null);
    try {
      setStatus(await client.clear());
    } catch (reason) {
      setInstallError({
        code: 'clear-error',
        message: reason instanceof Error ? reason.message : String(reason)
      });
      if (isTerminalWorkerError(reason)) {
        setStatus({
          state: 'incomplete',
          message: 'Reload this page to verify what remains before reinstalling.'
        });
        return;
      }
      try {
        setStatus(await client.status());
      } catch {
        setStatus({ state: 'incomplete', message: 'Reload this page to restart the analyzer.' });
      }
    }
  }

  const refreshStatus = useCallback((): void => {
    if (!client) return;
    void client.status().then(setStatus, () => setStatus({ state: 'corrupt', message: 'Analyzer data is corrupted.' }));
  }, [client]);

  if (!supported || !client) {
    return (
      <div className="app-shell">
        <AppHeader status={null} offlineShell={offlineShell} />
        <main className="unsupported"><h1>This browser does not support the storage features required by this alpha.</h1></main>
      </div>
    );
  }

  return (
    <div className="app-shell">
      <AppHeader status={status} offlineShell={offlineShell} />
      <UpdateNotice state={offlineShell} />
      {status?.state === 'ready'
        ? <Workspace status={status} manifest={manifest} client={client} onClear={() => void clear()} onPackInvalid={refreshStatus} operationError={installError} />
        : <InstallPanel manifest={manifest} manifestError={manifestError} status={status} progress={progress} error={installError} offlineShell={offlineShell} onInstall={() => void install()} onClear={() => void clear()} />}
      <footer><span>Runs entirely on this device</span><a href="/licenses.html">About &amp; licenses</a></footer>
    </div>
  );
}
