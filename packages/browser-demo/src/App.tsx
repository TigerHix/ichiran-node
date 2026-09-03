import {
  useCallback,
  useEffect,
  useMemo,
  useRef,
  useState,
  type KeyboardEvent,
  type ReactElement
} from 'react';
import {
  CaretDown as CaretDownIcon,
  Check as CheckIcon,
  Copy as CopyIcon,
  Database as DatabaseIcon,
  DownloadSimple as DownloadSimpleIcon,
  GearSix as GearSixIcon,
  TextAa as TextAaIcon,
  Trash as TrashIcon,
  X as XIcon
} from '@phosphor-icons/react';
import { Badge } from '@/components/ui/badge';
import { Button } from '@/components/ui/button';
import {
  DropdownMenu,
  DropdownMenuContent,
  DropdownMenuGroup,
  DropdownMenuItem,
  DropdownMenuLabel,
  DropdownMenuSeparator,
  DropdownMenuTrigger
} from '@/components/ui/dropdown-menu';
import { Separator } from '@/components/ui/separator';
import {
  Sheet,
  SheetContent,
  SheetDescription,
  SheetHeader,
  SheetTitle
} from '@/components/ui/sheet';
import { Skeleton } from '@/components/ui/skeleton';
import { Textarea } from '@/components/ui/textarea';
import {
  BrowserAnalyzer,
  MAX_ANALYZER_TEXT_LENGTH,
  isInvalidInstallError,
  isTerminalAnalyzerError,
  type AnalysisPath,
  type AnalysisResult,
  type AnalysisToken,
  type AnalyzerProgress,
  type AnalyzerRelease,
  type AnalyzerStatus,
  type DictionaryEntry
} from './analyzer-service.js';
import { AnalyzerClient } from './client.js';
import { ANALYZER_SAMPLES } from './samples.js';
import { useTokenSelection, type TokenSelection } from './use-token-selection.js';

declare const __ICHIRAN_BROWSER_QUALIFICATION__: boolean;

const DEFAULT_SAMPLE = ANALYZER_SAMPLES[0]!.text;

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

interface AppError {
  readonly code: string;
  readonly message: string;
}

const POS_LABELS: Readonly<Record<string, string>> = {
  n: 'noun', 'n-adv': 'adverbial noun', 'n-pr': 'proper noun', 'n-t': 'temporal noun',
  v1: 'ichidan verb', v5: 'godan verb', v5r: 'godan verb', vs: 'suru verb',
  'vs-i': 'suru verb', vk: 'kuru verb', adj: 'adjective', 'adj-i': 'i-adjective',
  'adj-na': 'na-adjective', adv: 'adverb', prt: 'particle', pn: 'pronoun',
  conj: 'conjunction', cop: 'copula', int: 'interjection', aux: 'auxiliary',
  ctr: 'counter', exp: 'expression', num: 'number', suf: 'suffix', pref: 'prefix'
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

function posLabel(value: string): string {
  const direct = POS_LABELS[value];
  if (direct) return direct;
  const match = Object.entries(POS_LABELS).find(([key]) => value.startsWith(key));
  return match?.[1] ?? value;
}

function posTone(pos: readonly string[]): string {
  const joined = pos.join(' ');
  if (/\bv|verb/.test(joined)) return 'verb';
  if (/adj/.test(joined)) return 'adjective';
  if (/prt|particle/.test(joined)) return 'particle';
  if (/adv/.test(joined)) return 'adverb';
  return 'noun';
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

function useMobileLayout(): boolean {
  const [mobile, setMobile] = useState(() => window.matchMedia('(max-width: 767px)').matches);
  useEffect(() => {
    const query = window.matchMedia('(max-width: 767px)');
    const update = (): void => setMobile(query.matches);
    query.addEventListener('change', update);
    return () => query.removeEventListener('change', update);
  }, []);
  return mobile;
}

function Header({ status, onClear }: {
  status: AnalyzerStatus | null;
  onClear(): void;
}): ReactElement {
  const ready = status?.state === 'ready';
  return (
    <header className="app-header">
      <a className="wordmark" href="/" aria-label="Ichiran home">
        <span lang="ja">一覧</span>
        <strong>Ichiran</strong>
      </a>
      <DropdownMenu>
        <DropdownMenuTrigger asChild>
          <Button variant="ghost" size="icon" aria-label="Analyzer settings">
            <GearSixIcon weight="regular" />
          </Button>
        </DropdownMenuTrigger>
        <DropdownMenuContent className="settings-menu" align="end" sideOffset={8}>
          <DropdownMenuLabel>Analyzer data</DropdownMenuLabel>
          {ready && (
            <DropdownMenuItem disabled>
              <DatabaseIcon />
              {formatBytes(status.installedBytes)} on this device
            </DropdownMenuItem>
          )}
          <DropdownMenuSeparator />
          {ready && (
            <DropdownMenuItem variant="destructive" onSelect={onClear}>
              <TrashIcon />
              Remove data
            </DropdownMenuItem>
          )}
          <DropdownMenuItem asChild><a href="/licenses.html">Licenses</a></DropdownMenuItem>
        </DropdownMenuContent>
      </DropdownMenu>
    </header>
  );
}

function ShellNotice({ state }: { state: OfflineShellState }): ReactElement | null {
  if (state.state === 'update-ready') {
    return (
      <div className="shell-notice" role="status">
        <span><strong>Update available.</strong> Close other Ichiran tabs, then reopen this one.</span>
      </div>
    );
  }
  if (state.state === 'error') {
    return (
      <div className="shell-notice shell-notice-error" role="alert">
        <span><strong>Offline use is unavailable.</strong> {state.message}</span>
      </div>
    );
  }
  return null;
}

function InstallView({
  release, releaseError, status, progress, error, offlineShell, onInstall, onClear
}: {
  release: AnalyzerRelease | null;
  releaseError: string | null;
  status: AnalyzerStatus | null;
  progress: AnalyzerProgress | null;
  error: AppError | null;
  offlineShell: OfflineShellState;
  onInstall(): void;
  onClear(): void;
}): ReactElement {
  const downloadBytes = release ? release.hot.downloadBytes + release.details.downloadBytes : 0;
  const installedBytes = release ? release.hot.installedBytes + release.details.installedBytes : 0;
  const broken = status?.state === 'incomplete' || status?.state === 'corrupt' || status?.state === 'stale';
  const shellReady = offlineShell.state === 'ready' || offlineShell.state === 'update-ready';
  const busy = progress !== null;
  const percent = progress && progress.totalBytes > 0
    ? Math.min(100, Math.round(progress.receivedBytes / progress.totalBytes * 100))
    : 0;
  const phase = progress?.phase === 'downloading'
    ? 'Downloading'
    : progress?.phase === 'verifying'
      ? 'Checking files'
      : progress?.phase === 'installing'
        ? 'Saving to this device'
        : 'Opening analyzer';

  return (
    <main className="install-layout">
      <section className="install-panel" aria-labelledby="install-title">
        <div className="install-icon" aria-hidden="true"><DatabaseIcon weight="duotone" /></div>
        <h1 id="install-title">Install Japanese data</h1>
        <p className="install-intro">Download once to analyze Japanese privately, even without a connection.</p>

        {status === null && !releaseError && (
          <div className="install-loading" aria-label="Preparing analyzer">
            <Skeleton className="h-10 w-full" />
            <Skeleton className="h-4 w-48" />
          </div>
        )}
        {status?.state === 'stale' && <p className="message error" role="alert">Your local data needs an update.</p>}
        {(status?.state === 'incomplete' || status?.state === 'corrupt') && (
          <p className="message error" role="alert">The saved data is incomplete. Install it again.</p>
        )}
        {error && (
          <p className="message error" role="alert">
            {error.code === 'insufficient-storage'
              ? 'There is not enough free storage for the analyzer.'
              : error.code === 'clear-error'
                ? 'The saved data could not be removed.'
                : 'The download did not finish.'}
            <small>{error.message}</small>
          </p>
        )}
        {releaseError && <p className="message error" role="alert">{releaseError}</p>}
        {busy && progress ? (
          <div className="install-progress" aria-live="polite">
            <div><span>{phase}</span><strong>{percent}%</strong></div>
            <progress max={progress.totalBytes} value={progress.receivedBytes} />
            <small>{formatBytes(progress.receivedBytes)} of {formatBytes(progress.totalBytes)}</small>
          </div>
        ) : release && (
          <Button className="install-action" size="lg" type="button" onClick={onInstall} disabled={!shellReady}>
            <DownloadSimpleIcon data-icon="inline-start" />
            {broken ? 'Reinstall analyzer data' : error ? 'Retry' : 'Install analyzer data'}
          </Button>
        )}

        {release && (
          <dl className="install-size">
            <div><dt>Download</dt><dd>{formatBytes(downloadBytes)}</dd></div>
            <div><dt>Stored</dt><dd>{formatBytes(installedBytes)}</dd></div>
          </dl>
        )}
        <p className="privacy">Your text stays on this device.</p>
        {broken && !busy && <Button variant="ghost" className="remove-data" onClick={onClear}>Remove saved data</Button>}
      </section>
    </main>
  );
}

function ExamplesMenu({ onChoose }: { onChoose(text: string): void }): ReactElement {
  return (
    <DropdownMenu>
      <DropdownMenuTrigger asChild>
        <Button variant="ghost" size="sm">Examples <CaretDownIcon data-icon="inline-end" /></Button>
      </DropdownMenuTrigger>
      <DropdownMenuContent className="examples-menu" align="end" sideOffset={6}>
        <DropdownMenuLabel>Try a sentence</DropdownMenuLabel>
        <DropdownMenuGroup>
          {ANALYZER_SAMPLES.map(sample => (
            <DropdownMenuItem className="example-item" key={sample.text} onSelect={() => onChoose(sample.text)}>
              <span>{sample.label}</span>
              <strong lang="ja">{sample.text}</strong>
            </DropdownMenuItem>
          ))}
        </DropdownMenuGroup>
      </DropdownMenuContent>
    </DropdownMenu>
  );
}

function TokenButton({ token, index, selected, onPointerDown, onPointerEnter, onKeyboardSelect }: {
  token: AnalysisToken;
  index: number;
  selected: boolean;
  onPointerDown(): void;
  onPointerEnter(): void;
  onKeyboardSelect(): void;
}): ReactElement {
  if (token.entryIndex === null && token.pos.length === 0) return <span className="punctuation">{token.text}</span>;
  const reading = token.reading && token.reading !== token.text ? token.reading : null;
  const accessible = reading
    ? `${token.text}, reading ${reading}, ${token.pos.map(posLabel).join(', ') || 'word'}`
    : `${token.text}, ${token.pos.map(posLabel).join(', ') || 'word'}`;
  return (
    <button
      className={`token token-${posTone(token.pos)}`}
      type="button"
      aria-label={accessible}
      aria-pressed={selected}
      data-token-index={index}
      onPointerDown={event => { if (event.button === 0) onPointerDown(); }}
      onPointerEnter={onPointerEnter}
      onClick={event => { if (event.detail === 0) onKeyboardSelect(); }}
    >
      <span className="token-reading" lang="ja">{reading ?? '\u00a0'}</span>
      <span className="token-surface" lang="ja">{token.text}</span>
    </button>
  );
}

function Sentence({ path, selection, onPointerDown, onPointerEnter, onPointerUp, onKeyboardSelect }: {
  path: AnalysisPath;
  selection: TokenSelection | null;
  onPointerDown(index: number): void;
  onPointerEnter(index: number): void;
  onPointerUp(index: number): void;
  onKeyboardSelect(index: number): void;
}): ReactElement {
  const tokenAt = (clientX: number, clientY: number): number | null => {
    const value = document.elementFromPoint(clientX, clientY)
      ?.closest<HTMLElement>('[data-token-index]')?.dataset.tokenIndex;
    if (value === undefined) return null;
    const index = Number(value);
    return Number.isSafeInteger(index) ? index : null;
  };
  return (
    <div
      className="sentence"
      lang="ja"
      onPointerMove={event => {
        if (event.buttons !== 1) return;
        const index = tokenAt(event.clientX, event.clientY);
        if (index !== null) onPointerEnter(index);
      }}
      onPointerUp={event => {
        const index = tokenAt(event.clientX, event.clientY);
        if (index !== null) onPointerUp(index);
      }}
    >
      {path.tokens.map((token, index) => (
        <TokenButton
          key={`${token.start}:${token.end}:${index}`}
          token={token}
          index={index}
          selected={selection !== null && index >= selection.start && index <= selection.end}
          onPointerDown={() => onPointerDown(index)}
          onPointerEnter={() => onPointerEnter(index)}
          onKeyboardSelect={() => onKeyboardSelect(index)}
        />
      ))}
    </div>
  );
}

function DetailSection({ title, children }: { title: string; children: React.ReactNode }): ReactElement {
  return <section className="detail-section"><h3>{title}</h3>{children}</section>;
}

function WordDetails({
  token, selectionText, entry, loading, error, copied, onCopy, onClose, compact = false
}: {
  token: AnalysisToken | null;
  selectionText: string;
  entry: DictionaryEntry | null;
  loading: boolean;
  error: string | null;
  copied: boolean;
  onCopy(): void;
  onClose(): void;
  compact?: boolean;
}): ReactElement {
  if (!selectionText) {
    return <div className="detail-empty"><TextAaIcon weight="light" /><p>Select a word</p></div>;
  }
  if (!token) {
    return (
      <div className="selection-details">
        <p>Selected text</p><h2 lang="ja">{selectionText}</h2>
        <Button variant="outline" onClick={onCopy}>{copied ? <CheckIcon /> : <CopyIcon />}{copied ? 'Copied' : 'Copy'}</Button>
      </div>
    );
  }
  return (
    <div className={`word-details ${compact ? 'word-details-compact' : ''}`}>
      <div className="detail-heading">
        <div>
          {token.reading && token.reading !== token.text && <p lang="ja">{token.reading}</p>}
          <h2 lang="ja">{token.text}</h2>
        </div>
        <div className="detail-actions">
          <Button variant="ghost" size="icon-sm" onClick={onCopy} aria-label="Copy selected word">
            {copied ? <CheckIcon /> : <CopyIcon />}
          </Button>
          {!compact && (
            <Button variant="ghost" size="icon-sm" onClick={onClose} aria-label="Close word details"><XIcon /></Button>
          )}
        </div>
      </div>
      <div className="pos-tags">
        {token.pos.map((value, index) => <Badge key={`${value}:${index}`} variant="secondary">{posLabel(value)}</Badge>)}
        {token.inflection.map((value, index) => (
          <Badge key={`${value.pos}:${value.type}:${index}`} variant="outline">
            {value.negative ? 'negative ' : ''}{value.formal ? 'formal ' : ''}{posLabel(value.pos)}
          </Badge>
        ))}
      </div>

      {loading && (
        <div className="detail-loading" aria-label="Loading word details">
          <Skeleton className="h-4 w-4/5" /><Skeleton className="h-4 w-3/5" /><Skeleton className="h-4 w-2/3" />
        </div>
      )}
      {error && <p className="message error" role="alert">{error}</p>}

      {entry && entry.senses.length > 0 && (
        <div className="meanings">
          {entry.senses.map((sense, index) => (
            <div className="meaning" key={`${sense.ord}:${index}`}>
              <span>{index + 1}</span>
              <div>
                {sense.glosses.map(gloss => <p key={`${gloss.ord}:${gloss.text}`}>{gloss.text}</p>)}
                {sense.properties.length > 0 && <small>{sense.properties.map(property => property.text).join(', ')}</small>}
              </div>
            </div>
          ))}
        </div>
      )}

      {entry && entry.forms.length > 0 && (
        <DetailSection title="Dictionary forms">
          <div className="forms dictionary-forms">
            {entry.forms.map(form => (
              <span key={`${form.route}:${form.ord}:${form.text}`}>
                <strong lang="ja">{form.text}</strong>
                <small>{form.route === 'kanji' ? 'written form' : 'reading'}{form.common !== null ? ', common' : ''}</small>
              </span>
            ))}
          </div>
        </DetailSection>
      )}
      {token.root && (
        <DetailSection title="Base form"><div className="base-form"><strong lang="ja">{token.root.form}</strong><span lang="ja">{token.root.reading}</span></div></DetailSection>
      )}
      {token.components.length > 0 && (
        <DetailSection title="Structure">
          <div className="structure-equation">
            {token.components.map((component, index) => (
              <span key={`${component.text}:${component.entryIndex}:${index}`}>
                {index > 0 && <i aria-hidden="true">+</i>}
                <strong lang="ja">{component.text}</strong>
                {component.reading !== component.text && <small lang="ja">{component.reading}</small>}
              </span>
            ))}
          </div>
        </DetailSection>
      )}
      {token.inflection.length > 0 && (
        <DetailSection title="Conjugation">
          <ol className="inflections">
            {token.inflection.map((step, index) => (
              <li key={`${step.pos}:${step.type}:${index}`}>
                <span>{index + 1}</span><strong>{posLabel(step.pos)}</strong>
                <small>{step.negative ? 'negative' : step.formal ? 'formal' : `form ${step.type}`}</small>
              </li>
            ))}
          </ol>
        </DetailSection>
      )}
      {token.alternatives.length > 0 && (
        <DetailSection title="Other readings">
          <div className="reading-alternatives">
            {token.alternatives.map(alternative => (
              <div key={alternative.candidateId}><strong lang="ja">{alternative.text}</strong><span lang="ja">{alternative.reading}</span></div>
            ))}
          </div>
        </DetailSection>
      )}
    </div>
  );
}

function AnalysisWorkspace({ analyzer, operationError, onPackInvalid }: {
  analyzer: BrowserAnalyzer;
  operationError: AppError | null;
  onPackInvalid(): void;
}): ReactElement {
  const [text, setText] = useState(DEFAULT_SAMPLE);
  const [result, setResult] = useState<AnalysisResult | null>(null);
  const [pathIndex, setPathIndex] = useState(0);
  const [running, setRunning] = useState(false);
  const [showBusy, setShowBusy] = useState(false);
  const [error, setError] = useState<string | null>(null);
  const [entry, setEntry] = useState<DictionaryEntry | null>(null);
  const [entryLoading, setEntryLoading] = useState(false);
  const [entryError, setEntryError] = useState<string | null>(null);
  const [romanization, setRomanization] = useState<string | null>(null);
  const [copyState, setCopyState] = useState<'idle' | 'copied' | 'error'>('idle');
  const mobileLayout = useMobileLayout();
  const activeIntent = useRef<string | null>(null);
  const request = useRef(0);
  const latestText = useRef(text);
  const selectionState = useTokenSelection();
  const path = result?.paths[pathIndex] ?? null;
  const selection = selectionState.selection;
  const selectedTokens = selection && path ? path.tokens.slice(selection.start, selection.end + 1) : [];
  const selectionText = selectedTokens.map(token => token.text).join('');
  const selectedToken = selectedTokens.length === 1 ? selectedTokens[0] ?? null : null;

  useEffect(() => {
    if (!running) { setShowBusy(false); return; }
    const timer = window.setTimeout(() => setShowBusy(true), 120);
    return () => window.clearTimeout(timer);
  }, [running]);

  useEffect(() => {
    setEntry(null);
    setEntryError(null);
    if (!selectedToken || selectedToken.entryIndex === null) { setEntryLoading(false); return; }
    let current = true;
    setEntryLoading(true);
    void analyzer.entry(selectedToken.entryIndex).then(value => {
      if (!current) return;
      setEntry(value);
      setEntryLoading(false);
    }, reason => {
      if (!current) return;
      setEntryLoading(false);
      setEntryError('Word details could not be opened.');
      if (isInvalidInstallError(reason)) onPackInvalid();
    });
    return () => { current = false; };
  }, [analyzer, onPackInvalid, selectedToken]);

  const changeText = useCallback((value: string): void => {
    latestText.current = value;
    setText(value);
    setResult(current => current?.input === value ? current : null);
    setPathIndex(0);
    setRomanization(null);
    selectionState.clear();
  }, [selectionState]);

  async function analyze(value = text): Promise<void> {
    const intent = value;
    if (!value.trim() || activeIntent.current === intent) return;
    if (activeIntent.current !== null) {
      ++request.current;
      activeIntent.current = null;
      analyzer.supersede();
    }
    const id = ++request.current;
    activeIntent.current = intent;
    setRunning(true);
    setError(null);
    try {
      const next = await analyzer.analyze(value, { limit: 3 });
      if (id !== request.current || value !== latestText.current) return;
      setResult(next);
      setPathIndex(0);
      setRomanization(null);
      selectionState.clear();
      if (next.paths[0]?.tokens.length === 1) selectionState.select(0);
    } catch (reason) {
      if (id !== request.current || value !== latestText.current) return;
      if (isInvalidInstallError(reason)) onPackInvalid();
      setError(reason instanceof Error ? reason.message : String(reason));
    } finally {
      if (id === request.current) { activeIntent.current = null; setRunning(false); }
    }
  }

  function chooseSample(value: string): void { changeText(value); void analyze(value); }
  function inputKeyDown(event: KeyboardEvent<HTMLTextAreaElement>): void {
    if (event.key === 'Enter' && (event.metaKey || event.ctrlKey)) { event.preventDefault(); void analyze(); }
  }
  function choosePath(index: number): void { setPathIndex(index); selectionState.clear(); setRomanization(null); }
  function closeDetails(): void {
    const tokenIndex = selection?.start ?? null;
    selectionState.clear();
    if (tokenIndex !== null) window.requestAnimationFrame(() => {
      document.querySelector<HTMLButtonElement>(`[data-token-index="${tokenIndex}"]`)?.focus();
    });
  }
  async function copySelection(): Promise<void> {
    if (!selectionText) return;
    try {
      await navigator.clipboard.writeText(selectionText);
      setCopyState('copied');
      window.setTimeout(() => setCopyState('idle'), 1_400);
    } catch { setCopyState('error'); }
  }
  async function toggleRomanization(): Promise<void> {
    if (!result) return;
    if (romanization !== null) { setRomanization(null); return; }
    try { setRomanization(await analyzer.romanize(result.input)); }
    catch (reason) {
      if (isInvalidInstallError(reason)) onPackInvalid();
      setError('Romanization could not be generated.');
    }
  }

  const detailProps = {
    token: selectedToken, selectionText, entry, loading: entryLoading, error: entryError,
    copied: copyState === 'copied', onCopy: () => void copySelection(), onClose: closeDetails
  };

  return (
    <main className="workspace">
      {operationError && (
        <p className="message error operation-error" role="alert">
          {operationError.code === 'clear-error' ? 'The saved data could not be removed.' : 'The new data could not replace the current copy.'}
          <small>{operationError.message}</small>
        </p>
      )}
      <section className="composer" aria-labelledby="composer-title">
        <div className="composer-heading">
          <div><h1 id="composer-title"><label htmlFor="japanese-input">Japanese text</label></h1><p>Paste a sentence or choose an example.</p></div>
          <ExamplesMenu onChoose={chooseSample} />
        </div>
        <div className="textarea-wrap">
          <Textarea
            id="japanese-input" className="japanese-input" value={text}
            onChange={event => changeText(event.target.value)} onKeyDown={inputKeyDown}
            lang="ja" rows={3} maxLength={MAX_ANALYZER_TEXT_LENGTH}
            aria-label="Japanese text" placeholder="日本語を入力してください"
          />
          {text && (
            <Button className="clear-input" variant="ghost" size="icon-sm" type="button" onClick={() => changeText('')} aria-label="Clear Japanese text"><XIcon /></Button>
          )}
        </div>
        <div className="composer-footer">
          <span>{text.length.toLocaleString()} / {MAX_ANALYZER_TEXT_LENGTH.toLocaleString()}</span>
          <Button type="button" size="lg" onClick={() => void analyze()} disabled={!text.trim() || running}>{running ? 'Analyzing' : 'Analyze'}</Button>
        </div>
        {error && (
          <p className="message error analysis-error" role="alert">
            Analysis failed.<small>{error}</small><Button variant="link" size="sm" onClick={() => void analyze()}>Try again</Button>
          </p>
        )}
      </section>
      <Separator />
      <section className="analysis" aria-label="Analysis result" aria-busy={running}>
        <div className="analysis-main">
          <div className="analysis-toolbar">
            <h2>Analysis</h2>
            {result && (
              <Button variant="ghost" size="sm" onClick={() => void toggleRomanization()}>
                <TextAaIcon />{romanization === null ? 'Romanize' : 'Hide romaji'}
              </Button>
            )}
          </div>
          {showBusy && !path ? (
            <div className="analysis-skeleton" aria-label="Analyzing Japanese text">
              <Skeleton className="h-8 w-28" /><Skeleton className="h-8 w-20" /><Skeleton className="h-8 w-32" />
            </div>
          ) : path ? (
            <>
              <Sentence path={path} selection={selection} onPointerDown={selectionState.pointerDown} onPointerEnter={selectionState.pointerEnter} onPointerUp={selectionState.pointerUp} onKeyboardSelect={selectionState.toggle} />
              {romanization && <p className="romanization">{romanization}</p>}
              {result && result.paths.length > 1 && (
                <details className="parse-alternatives">
                  <summary>Other parses <span>{result.paths.length - 1}</span></summary>
                  <div>
                    {result.paths.map((alternative, index) => ({ alternative, index }))
                      .filter(({ index }) => index !== pathIndex)
                      .map(({ alternative, index }) => (
                        <button type="button" key={index} onClick={() => choosePath(index)}>
                          <span lang="ja">{alternative.tokens.map(token => token.text).join(' / ')}</span>
                        </button>
                      ))}
                  </div>
                </details>
              )}
            </>
          ) : (
            <div className="analysis-empty"><p>{text.trim() ? 'Analyze the text to see each word.' : 'Enter Japanese text to begin.'}</p></div>
          )}
        </div>
        <aside className="detail-desktop" aria-label="Word details" aria-live="polite"><WordDetails {...detailProps} /></aside>
      </section>
      <Sheet open={mobileLayout && !selectionState.selecting && selectionText.length > 0} onOpenChange={open => { if (!open) closeDetails(); }}>
        <SheetContent className="detail-mobile" side="bottom">
          <SheetHeader className="sr-only"><SheetTitle>Word details</SheetTitle><SheetDescription>Dictionary and morphology details for the selected text.</SheetDescription></SheetHeader>
          <WordDetails {...detailProps} compact />
        </SheetContent>
      </Sheet>
      {copyState === 'error' && <p className="message error" role="alert">The selection could not be copied.</p>}
    </main>
  );
}

export function App({ offlineShellReady }: { offlineShellReady: Promise<OfflineShellResult> }): ReactElement {
  const supported = supportsRequiredFeatures();
  const client = useMemo(() => supported ? new AnalyzerClient() : null, [supported]);
  const analyzer = useMemo(() => client ? new BrowserAnalyzer(client) : null, [client]);
  const [status, setStatus] = useState<AnalyzerStatus | null>(null);
  const [release, setRelease] = useState<AnalyzerRelease | null>(null);
  const [releaseError, setReleaseError] = useState<string | null>(null);
  const [progress, setProgress] = useState<AnalyzerProgress | null>(null);
  const [operationError, setOperationError] = useState<AppError | null>(null);
  const [offlineShell, setOfflineShell] = useState<OfflineShellState>({ state: 'opening' });

  useEffect(() => {
    let current = true;
    if (!('serviceWorker' in navigator)) {
      void offlineShellReady.then(result => {
        if (!current) return;
        setOfflineShell(result.ready ? { state: 'ready' } : { state: 'error', message: result.message ?? 'Service Workers are unavailable.' });
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
    const workerStateChanged = (): void => { if (watchedWorker?.state === 'installed') showWaitingUpdate(); };
    const watchInstallingWorker = (): void => {
      watchedWorker?.removeEventListener('statechange', workerStateChanged);
      watchedWorker = registration?.installing ?? null;
      watchedWorker?.addEventListener('statechange', workerStateChanged);
    };
    const controllerChanged = (): void => {
      const nextController = navigator.serviceWorker.controller;
      if (previousController && nextController && previousController !== nextController) { window.location.reload(); return; }
      previousController = nextController;
    };
    navigator.serviceWorker.addEventListener('controllerchange', controllerChanged);
    void offlineShellReady.then(result => {
      if (!current) return;
      registration = result.registration ?? null;
      setOfflineShell(result.ready ? { state: 'ready' } : { state: 'error', message: result.message ?? 'Service Worker registration failed.' });
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
    if (!analyzer) return;
    let current = true;
    void analyzer.initialize().then(initialized => {
      if (!current) return;
      setRelease(initialized.release);
      setStatus(initialized.status);
      setReleaseError(null);
    }, reason => { if (current) setReleaseError(reason instanceof Error ? reason.message : String(reason)); });
    return () => { current = false; analyzer.dispose(); };
  }, [analyzer]);

  useEffect(() => {
    if (
      !__ICHIRAN_BROWSER_QUALIFICATION__
      || !new URLSearchParams(window.location.search).has('qualification')
      || !client
      || !release
      || status?.state !== 'ready'
    ) return;
    let current = true;
    const target = window as typeof window & {
      __ichiranQualification?: import('./qualification-client.js').AnalyzerQualification;
    };
    void import('./qualification-client.js').then(({ createAnalyzerQualification }) => {
      if (current) target.__ichiranQualification = createAnalyzerQualification(client, release);
    });
    return () => {
      current = false;
      delete target.__ichiranQualification;
    };
  }, [client, release, status]);

  async function install(): Promise<void> {
    if (!analyzer) return;
    setOperationError(null);
    setProgress({ phase: 'downloading', receivedBytes: 0, totalBytes: release ? release.hot.downloadBytes + release.details.downloadBytes : 1 });
    try {
      if (typeof navigator.storage.persist === 'function') try { await navigator.storage.persist(); } catch { /* Best effort. */ }
      setStatus(await analyzer.install(setProgress));
    } catch (reason) {
      setOperationError(reason instanceof Error && 'code' in reason
        ? { code: String(reason.code), message: reason.message }
        : { code: 'install-error', message: String(reason) });
      if (!isTerminalAnalyzerError(reason)) {
        try { setStatus(await analyzer.status()); }
        catch { setStatus({ state: 'incomplete', message: 'Reload to check the saved data.' }); }
      }
    } finally { setProgress(null); }
  }

  async function clear(): Promise<void> {
    if (!analyzer || !window.confirm('Remove the downloaded Japanese data from this device?')) return;
    setOperationError(null);
    try { setStatus(await analyzer.clear()); }
    catch (reason) {
      setOperationError({ code: 'clear-error', message: reason instanceof Error ? reason.message : String(reason) });
      if (isTerminalAnalyzerError(reason)) setStatus({ state: 'incomplete', message: 'Reload to check the saved data.' });
      else {
        try { setStatus(await analyzer.status()); }
        catch { setStatus({ state: 'incomplete', message: 'Reload to check the saved data.' }); }
      }
    }
  }

  const refreshStatus = useCallback((): void => {
    if (!analyzer) return;
    void analyzer.status().then(setStatus, () => setStatus({ state: 'corrupt', message: 'The saved data is corrupted.' }));
  }, [analyzer]);

  if (!supported || !analyzer) {
    return (
      <div className="app-shell">
        <Header status={null} onClear={() => undefined} />
        <main className="unsupported"><DatabaseIcon weight="light" /><h1>This browser cannot store the analyzer locally.</h1><p>Open Ichiran in a current browser with private browsing turned off.</p></main>
      </div>
    );
  }

  return (
    <div className="app-shell">
      <Header status={status} onClear={() => void clear()} />
      <ShellNotice state={offlineShell} />
      {status?.state === 'ready'
        ? <AnalysisWorkspace analyzer={analyzer} operationError={operationError} onPackInvalid={refreshStatus} />
        : <InstallView release={release} releaseError={releaseError} status={status} progress={progress} error={operationError} offlineShell={offlineShell} onInstall={() => void install()} onClear={() => void clear()} />}
      <footer><span>Runs on this device</span><a href="/licenses.html">Licenses</a></footer>
    </div>
  );
}
