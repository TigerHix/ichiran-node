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
  Database as DatabaseIcon,
  DownloadSimple as DownloadSimpleIcon,
  GearSix as GearSixIcon,
  TextAa as TextAaIcon,
  Trash as TrashIcon,
  X as XIcon
} from '@phosphor-icons/react';
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
import { Skeleton } from '@/components/ui/skeleton';
import { Textarea } from '@/components/ui/textarea';
import {
  createPresentation,
  partOfSpeechCategory,
  type Presentation,
  type PresentationLocale
} from '@ichiran/presentation';
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
  type TokenDetails
} from './analyzer-service.js';
import { AnalyzerClient } from './client.js';
import { analysisPathChoices } from './analysis-path-choices.js';
import { ANALYZER_SAMPLES } from './samples.js';
import { useTokenSelection, type TokenSelection } from './use-token-selection.js';
import { WordDetails } from './WordDetails.js';

declare const __ICHIRAN_BROWSER_QUALIFICATION__: boolean;

const DEFAULT_SAMPLE = ANALYZER_SAMPLES[0]!.text;

interface AppError {
  readonly code: string;
  readonly message: string;
}

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

function releaseBytes(release: AnalyzerRelease, kind: 'downloadBytes' | 'installedBytes'): number {
  return release.hot[kind]
    + release.lexicon[kind]
    + Object.values(release.locales).reduce((total, asset) => total + asset[kind], 0);
}

function posTone(pos: readonly string[]): string {
  const categories = pos.map(partOfSpeechCategory);
  if (categories.includes('verb')) return 'verb';
  if (categories.includes('adjective')) return 'adjective';
  if (categories.includes('particle')) return 'particle';
  if (categories.includes('adverb')) return 'adverb';
  return 'noun';
}

function supportsRequiredFeatures(): boolean {
  return typeof Worker === 'function'
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

function Header({ status, presentation, uiLocale, onUiLocale, onClear }: {
  status: AnalyzerStatus | null;
  presentation: Presentation;
  uiLocale: PresentationLocale;
  onUiLocale(locale: PresentationLocale): void;
  onClear(): void;
}): ReactElement {
  const ready = status?.state === 'ready';
  return (
    <header className="app-header">
      <a className="wordmark" href="/" aria-label={presentation.message('home')}>
        <span lang="ja">一覧</span>
        <strong>Ichiran</strong>
      </a>
      <DropdownMenu>
        <DropdownMenuTrigger asChild>
          <Button variant="ghost" size="icon" aria-label={presentation.message('settings')}>
            <GearSixIcon weight="regular" />
          </Button>
        </DropdownMenuTrigger>
        <DropdownMenuContent className="settings-menu" align="end" sideOffset={8}>
          <DropdownMenuLabel>{presentation.message('analyzerData')}</DropdownMenuLabel>
          <label className="interface-locale">
            <span>{presentation.message('interfaceLanguage')}</span>
            <select value={uiLocale} onChange={event => onUiLocale(event.target.value as PresentationLocale)}>
              <option value="en">English</option>
              <option value="zh-Hans">简体中文</option>
            </select>
          </label>
          <DropdownMenuSeparator />
          {ready && (
            <DropdownMenuItem disabled>
              <DatabaseIcon />
              {presentation.message('onThisDevice', { size: formatBytes(status.installedBytes) })}
            </DropdownMenuItem>
          )}
          <DropdownMenuSeparator />
          {ready && (
            <DropdownMenuItem variant="destructive" onSelect={onClear}>
              <TrashIcon />
              {presentation.message('removeData')}
            </DropdownMenuItem>
          )}
          <DropdownMenuItem asChild><a href="/licenses.html">{presentation.message('licenses')}</a></DropdownMenuItem>
        </DropdownMenuContent>
      </DropdownMenu>
    </header>
  );
}

function InstallView({
  release, releaseError, status, progress, error, presentation, onInstall, onClear
}: {
  release: AnalyzerRelease | null;
  releaseError: string | null;
  status: AnalyzerStatus | null;
  progress: AnalyzerProgress | null;
  error: AppError | null;
  presentation: Presentation;
  onInstall(): void;
  onClear(): void;
}): ReactElement {
  const downloadBytes = release ? releaseBytes(release, 'downloadBytes') : 0;
  const installedBytes = release ? releaseBytes(release, 'installedBytes') : 0;
  const broken = status?.state === 'incomplete' || status?.state === 'corrupt' || status?.state === 'stale';
  const busy = progress !== null;
  const percent = progress && progress.totalBytes > 0
    ? Math.min(100, Math.round(progress.receivedBytes / progress.totalBytes * 100))
    : 0;
  const phase = progress?.phase === 'downloading'
    ? presentation.message('downloading')
    : progress?.phase === 'verifying'
      ? presentation.message('checkingFiles')
      : progress?.phase === 'installing'
        ? presentation.message('savingDevice')
        : presentation.message('openingAnalyzer');

  return (
    <main className="install-layout">
      <section className="install-panel" aria-labelledby="install-title">
        <div className="install-icon" aria-hidden="true"><DatabaseIcon weight="duotone" /></div>
        <h1 id="install-title">{presentation.message('installTitle')}</h1>
        <p className="install-intro">{presentation.message('installIntro')}</p>

        {status === null && !releaseError && (
          <div className="install-loading" aria-label={presentation.message('preparing')}>
            <Skeleton className="h-10 w-full" />
            <Skeleton className="h-4 w-48" />
          </div>
        )}
        {status?.state === 'stale' && <p className="message error" role="alert">{presentation.message('staleData')}</p>}
        {(status?.state === 'incomplete' || status?.state === 'corrupt') && (
          <p className="message error" role="alert">{presentation.message('incompleteData')}</p>
        )}
        {error && (
          <p className="message error" role="alert">
            {error.code === 'insufficient-storage'
              ? presentation.message('insufficientStorage')
              : error.code === 'clear-error'
                ? presentation.message('clearFailed')
                : presentation.message('downloadFailed')}
            <small>{error.message}</small>
          </p>
        )}
        {releaseError && <p className="message error" role="alert">{releaseError}</p>}
        {busy && progress ? (
          <div className="install-progress" aria-live="polite">
            <div><span>{phase}</span><strong>{percent}%</strong></div>
            <progress max={progress.totalBytes} value={progress.receivedBytes} />
            <small>{presentation.message('byteProgress', {
              received: formatBytes(progress.receivedBytes),
              total: formatBytes(progress.totalBytes)
            })}</small>
          </div>
        ) : release && (
          <Button className="install-action" size="lg" type="button" onClick={onInstall}>
            <DownloadSimpleIcon data-icon="inline-start" />
            {presentation.message(broken ? 'reinstall' : error ? 'retry' : 'install')}
          </Button>
        )}

        {release && (
          <dl className="install-size">
            <div><dt>{presentation.message('download')}</dt><dd>{formatBytes(downloadBytes)}</dd></div>
            <div><dt>{presentation.message('stored')}</dt><dd>{formatBytes(installedBytes)}</dd></div>
          </dl>
        )}
        <p className="privacy">{presentation.message('privacy')}</p>
        {broken && !busy && <Button variant="ghost" className="remove-data" onClick={onClear}>{presentation.message('removeData')}</Button>}
      </section>
    </main>
  );
}

function ExamplesMenu({ presentation, onChoose }: {
  presentation: Presentation;
  onChoose(text: string): void;
}): ReactElement {
  return (
    <DropdownMenu>
      <DropdownMenuTrigger asChild>
        <Button variant="ghost" size="sm">{presentation.message('examples')} <CaretDownIcon data-icon="inline-end" /></Button>
      </DropdownMenuTrigger>
      <DropdownMenuContent className="examples-menu" align="end" sideOffset={6}>
        <DropdownMenuLabel>{presentation.message('trySentence')}</DropdownMenuLabel>
        <DropdownMenuGroup>
          {ANALYZER_SAMPLES.map(sample => (
            <DropdownMenuItem className="example-item" key={sample.text} onSelect={() => onChoose(sample.text)}>
              <span>{presentation.sampleLabel(sample.id)}</span>
              <strong lang="ja">{sample.text}</strong>
            </DropdownMenuItem>
          ))}
        </DropdownMenuGroup>
      </DropdownMenuContent>
    </DropdownMenu>
  );
}

function TokenButton({ token, index, selected, presentation, onPointerDown, onPointerEnter, onKeyboardSelect }: {
  token: AnalysisToken;
  index: number;
  selected: boolean;
  presentation: Presentation;
  onPointerDown(): void;
  onPointerEnter(): void;
  onKeyboardSelect(): void;
}): ReactElement {
  if (token.entryIndex === null && token.pos.length === 0) return <span className="punctuation">{token.text}</span>;
  const reading = token.reading && token.reading !== token.text ? token.reading : null;
  const accessible = reading
    ? `${token.text}, ${presentation.message('reading', { reading })}, ${token.pos.map(presentation.partOfSpeechLabel).join(', ') || presentation.message('word')}`
    : `${token.text}, ${token.pos.map(presentation.partOfSpeechLabel).join(', ') || presentation.message('word')}`;
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

function Sentence({ path, selection, presentation, onPointerDown, onPointerEnter, onPointerUp, onKeyboardSelect }: {
  path: AnalysisPath;
  selection: TokenSelection | null;
  presentation: Presentation;
  onPointerDown(index: number): void;
  onPointerEnter(index: number): void;
  onPointerUp(): void;
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
      onPointerUp={onPointerUp}
    >
      {path.tokens.map((token, index) => (
        <TokenButton
          key={`${token.start}:${token.end}:${index}`}
          token={token}
          index={index}
          selected={selection !== null && index >= selection.start && index <= selection.end}
          presentation={presentation}
          onPointerDown={() => onPointerDown(index)}
          onPointerEnter={() => onPointerEnter(index)}
          onKeyboardSelect={() => onKeyboardSelect(index)}
        />
      ))}
    </div>
  );
}

function AnalysisWorkspace({ analyzer, operationError, presentation, onPackInvalid }: {
  analyzer: BrowserAnalyzer;
  operationError: AppError | null;
  presentation: Presentation;
  onPackInvalid(): void;
}): ReactElement {
  const [text, setText] = useState(DEFAULT_SAMPLE);
  const [result, setResult] = useState<AnalysisResult | null>(null);
  const [pathIndex, setPathIndex] = useState(0);
  const [running, setRunning] = useState(false);
  const [showBusy, setShowBusy] = useState(false);
  const [error, setError] = useState<string | null>(null);
  const [details, setDetails] = useState<TokenDetails | null>(null);
  const [detailsLoading, setDetailsLoading] = useState(false);
  const [detailsError, setDetailsError] = useState<string | null>(null);
  const [romanization, setRomanization] = useState<string | null>(null);
  const [copyState, setCopyState] = useState<'idle' | 'copied' | 'error'>('idle');
  const [definitionLocale, setDefinitionLocale] = useState<'en' | 'zh-Hans'>('en');
  const mobileLayout = useMobileLayout();
  const activeIntent = useRef<string | null>(null);
  const request = useRef(0);
  const latestText = useRef(text);
  const selectionState = useTokenSelection();
  const path = result?.paths[pathIndex] ?? null;
  const parseChoices = useMemo(
    () => result ? analysisPathChoices(result.paths, pathIndex, presentation) : [],
    [pathIndex, presentation, result]
  );
  const otherParseChoices = parseChoices.filter(choice => choice.index !== pathIndex);
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
    setDetails(null);
    setDetailsError(null);
    if (!selectedToken || !result || !selection) { setDetailsLoading(false); return; }
    let current = true;
    setDetailsLoading(true);
    void analyzer.details(result.input, {
      limit: 3,
      pathIndex,
      tokenIndex: selection.start,
      locale: definitionLocale
    }).then(value => {
      if (!current) return;
      setDetails(value);
      setDetailsLoading(false);
    }, reason => {
      if (!current) return;
      setDetailsLoading(false);
      setDetailsError(presentation.message('detailsFailed'));
      if (isInvalidInstallError(reason)) onPackInvalid();
    });
    return () => { current = false; };
  }, [analyzer, definitionLocale, onPackInvalid, pathIndex, presentation, result, selectedToken, selection]);

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
      setError(presentation.message('romanizationFailed'));
    }
  }

  const detailProps = {
    token: selectedToken, selectionText, details, loading: detailsLoading, error: detailsError,
    copied: copyState === 'copied', presentation,
    onCopy: () => void copySelection(), onClose: closeDetails
  };

  return (
    <main className="workspace">
      {operationError && (
        <p className="message error operation-error" role="alert">
          {presentation.message(operationError.code === 'clear-error' ? 'clearFailed' : 'replaceFailed')}
          <small>{operationError.message}</small>
        </p>
      )}
      <section className="composer" aria-labelledby="composer-title">
        <div className="composer-heading">
          <div><h1 id="composer-title"><label htmlFor="japanese-input">{presentation.message('japaneseText')}</label></h1><p>{presentation.message('composerIntro')}</p></div>
          <ExamplesMenu presentation={presentation} onChoose={chooseSample} />
        </div>
        <div className="textarea-wrap">
          <Textarea
            id="japanese-input" className="japanese-input" value={text}
            onChange={event => changeText(event.target.value)} onKeyDown={inputKeyDown}
            lang="ja" rows={3} maxLength={MAX_ANALYZER_TEXT_LENGTH}
            aria-label={presentation.message('japaneseText')} placeholder="日本語を入力してください"
          />
          {text && (
            <Button className="clear-input" variant="ghost" size="icon-sm" type="button" onClick={() => changeText('')} aria-label={presentation.message('clearJapanese')}><XIcon /></Button>
          )}
        </div>
        <div className="composer-footer">
          <span>{text.length.toLocaleString()} / {MAX_ANALYZER_TEXT_LENGTH.toLocaleString()}</span>
          <Button type="button" size="lg" onClick={() => void analyze()} disabled={!text.trim() || running}>{presentation.message(running ? 'analyzing' : 'analyze')}</Button>
        </div>
        {error && (
          <p className="message error analysis-error" role="alert">
            {presentation.message('analysisFailed')}<small>{error}</small><Button variant="link" size="sm" onClick={() => void analyze()}>{presentation.message('tryAgain')}</Button>
          </p>
        )}
      </section>
      <Separator />
      <section className="analysis" aria-label={presentation.message('analysisResult')} aria-busy={running}>
        <div className="analysis-main">
          <div className="analysis-toolbar">
            <h2>{presentation.message('analysis')}</h2>
            <div className="analysis-actions">
              <label className="definition-locale">
                <span>{presentation.message('definitions')}</span>
                <select
                  value={definitionLocale}
                  onChange={event => setDefinitionLocale(event.target.value as 'en' | 'zh-Hans')}
                >
                  <option value="en">{presentation.message('english')}</option>
                  <option value="zh-Hans">{presentation.message('simplifiedChinese')}</option>
                </select>
              </label>
              {result && (
                <Button variant="ghost" size="sm" onClick={() => void toggleRomanization()}>
                  <TextAaIcon />{presentation.message(romanization === null ? 'romanize' : 'hideRomaji')}
                </Button>
              )}
            </div>
          </div>
          {showBusy && !path ? (
            <div className="analysis-skeleton" aria-label={presentation.message('analyzing')}>
              <Skeleton className="h-8 w-28" /><Skeleton className="h-8 w-20" /><Skeleton className="h-8 w-32" />
            </div>
          ) : path ? (
            <>
              <Sentence path={path} selection={selection} presentation={presentation} onPointerDown={selectionState.pointerDown} onPointerEnter={selectionState.pointerEnter} onPointerUp={selectionState.pointerUp} onKeyboardSelect={selectionState.toggle} />
              {romanization && <p className="romanization">{romanization}</p>}
              {otherParseChoices.length > 0 && (
                <details className="parse-alternatives">
                  <summary>{presentation.message('otherParses')} <span>{otherParseChoices.length}</span></summary>
                  <div>
                    {otherParseChoices.map(choice => (
                        <button type="button" key={choice.index} onClick={() => choosePath(choice.index)}>
                          <span lang="ja">{choice.label}</span>
                        </button>
                      ))}
                  </div>
                </details>
              )}
            </>
          ) : (
            <div className="analysis-empty"><p>{presentation.message(text.trim() ? 'analyzeHint' : 'enterHint')}</p></div>
          )}
        </div>
        {!mobileLayout && (
          <aside className="detail-desktop" aria-label={presentation.message('wordDetails')} aria-live="polite">
            <WordDetails {...detailProps} />
          </aside>
        )}
      </section>
      {mobileLayout && !selectionState.selecting && selectionText.length > 0 && (
        <aside className="detail-mobile" aria-label={presentation.message('wordDetails')} aria-live="polite">
          <Button className="detail-mobile-close" variant="ghost" size="icon-sm" type="button" onClick={closeDetails} aria-label={presentation.message('close')}>
            <XIcon />
          </Button>
          <WordDetails {...detailProps} compact />
        </aside>
      )}
      {copyState === 'error' && <p className="message error" role="alert">{presentation.message('copyFailed')}</p>}
    </main>
  );
}

export function App(): ReactElement {
  const [uiLocale, setUiLocale] = useState<PresentationLocale>('en');
  const presentation = useMemo(() => createPresentation(uiLocale), [uiLocale]);
  const supported = supportsRequiredFeatures();
  const client = useMemo(() => supported ? new AnalyzerClient() : null, [supported]);
  const analyzer = useMemo(() => client ? new BrowserAnalyzer(client) : null, [client]);
  const [status, setStatus] = useState<AnalyzerStatus | null>(null);
  const [release, setRelease] = useState<AnalyzerRelease | null>(null);
  const [releaseError, setReleaseError] = useState<string | null>(null);
  const [progress, setProgress] = useState<AnalyzerProgress | null>(null);
  const [operationError, setOperationError] = useState<AppError | null>(null);
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
    setProgress({
      phase: 'downloading',
      receivedBytes: 0,
      totalBytes: release ? releaseBytes(release, 'downloadBytes') : 1
    });
    try {
      if (typeof navigator.storage.persist === 'function') try { await navigator.storage.persist(); } catch { /* Best effort. */ }
      setStatus(await analyzer.install(setProgress));
    } catch (reason) {
      setOperationError(reason instanceof Error && 'code' in reason
        ? { code: String(reason.code), message: reason.message }
        : { code: 'install-error', message: String(reason) });
      if (!isTerminalAnalyzerError(reason)) {
        try { setStatus(await analyzer.status()); }
        catch { setStatus({ state: 'incomplete', message: presentation.message('reloadStatus') }); }
      }
    } finally { setProgress(null); }
  }

  async function clear(): Promise<void> {
    if (!analyzer || !window.confirm(presentation.message('confirmRemove'))) return;
    setOperationError(null);
    try { setStatus(await analyzer.clear()); }
    catch (reason) {
      setOperationError({ code: 'clear-error', message: reason instanceof Error ? reason.message : String(reason) });
      if (isTerminalAnalyzerError(reason)) setStatus({ state: 'incomplete', message: presentation.message('reloadStatus') });
      else {
        try { setStatus(await analyzer.status()); }
        catch { setStatus({ state: 'incomplete', message: presentation.message('reloadStatus') }); }
      }
    }
  }

  const refreshStatus = useCallback((): void => {
    if (!analyzer) return;
    void analyzer.status().then(setStatus, () => setStatus({ state: 'corrupt', message: presentation.message('corruptData') }));
  }, [analyzer, presentation]);

  if (!supported || !analyzer) {
    return (
      <div className="app-shell">
        <Header status={null} presentation={presentation} uiLocale={uiLocale} onUiLocale={setUiLocale} onClear={() => undefined} />
        <main className="unsupported"><DatabaseIcon weight="light" /><h1>{presentation.message('unsupportedTitle')}</h1><p>{presentation.message('unsupportedIntro')}</p></main>
      </div>
    );
  }

  return (
    <div className="app-shell">
      <Header status={status} presentation={presentation} uiLocale={uiLocale} onUiLocale={setUiLocale} onClear={() => void clear()} />
      {status?.state === 'ready'
        ? <AnalysisWorkspace analyzer={analyzer} operationError={operationError} presentation={presentation} onPackInvalid={refreshStatus} />
        : <InstallView release={release} releaseError={releaseError} status={status} progress={progress} error={operationError} presentation={presentation} onInstall={() => void install()} onClear={() => void clear()} />}
      <footer><span>{presentation.message('runsOnDevice')}</span><a href="/licenses.html">{presentation.message('licenses')}</a></footer>
    </div>
  );
}
