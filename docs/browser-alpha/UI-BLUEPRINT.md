# Browser Analyzer Alpha: UI blueprint

Status: implementation contract, 2026-08-28
Scope: the installable, analyzer-only browser alpha. OCR, chat, TTS, the
experimental grammar package, and a general Kanjidic UI are excluded.

Browser capability floor: Safari 26+ or current Chromium. Installation requires a
Worker, OPFS, Web Locks, writable file streams, and `DecompressionStream`; unsupported browsers
show the required-feature message and no install action.

## 1. Product rule

Borrow Nemu's analyzer content and interaction language, not its manga-reader
container. Nemu uses a bottom drawer because sentence analysis is secondary to the
reader. In this standalone product, installation, input, results, details, and
runtime proof belong on one persistent page.

The interface must make four facts obvious:

1. the analyzer data is installed once and has passed integrity verification;
2. the current analysis ran in a dedicated Worker on this device;
3. the same result remains available after an offline restart; and
4. top-N analyses and full dictionary/morphology details are local, not fetched on
   selection.

Keep ownership direct. Render from pack status, Worker status, the current request,
and the current result. Do not introduce a generalized workflow engine, navigation
framework, policy object, or UI state machine for the alpha.

## 2. Nemu evidence

| Behavior | Source evidence | Alpha decision |
|---|---|---|
| A transcript opens the sheet with raw text, clears stale tokens, and starts cancellable analysis. | `/home/tiger/nemu/src/lib/plugins/builtin/japanese-learning/store.ts:298-316`, `:340-354`, `:512-584` | Preserve immediate raw-text continuity, cancellation, and stale-result rejection. |
| The reader result is a 70-vh, `max-w-2xl` bottom drawer with an explicit one-tap mobile overlay close fix. | `/home/tiger/nemu/src/lib/plugins/builtin/japanese-learning/ui/ocr-result-sheet.tsx:71-93` | Do not preserve the modal container. Use a persistent document surface. |
| Raw text stays visible while tokenization runs, then a token layer replaces it. | `/home/tiger/nemu/src/lib/plugins/builtin/japanese-learning/ui/sentence-display.tsx:65-95`, `:176-250` | Preserve the continuity; simplify the reveal animation. |
| Sentence and details panes have independent scrolling and 48-px edge fades. | `/home/tiger/nemu/src/lib/plugins/builtin/japanese-learning/ui/sentence-display.tsx:179-256`; `/home/tiger/nemu/src/lib/plugins/builtin/japanese-learning/ui/scroll-fading-overlay.tsx:14-72` | Use document scrolling by default. Cap only unusually long token areas. |
| Each token stacks furigana, a large Japanese word, and a short Japanese POS label in fixed-height rows. | `/home/tiger/nemu/src/lib/plugins/builtin/japanese-learning/ui/token-display.tsx:31-50`, `:76-188` | Preserve this token anatomy and label mapping. |
| Tap toggles one token; drag captures a range and triggers haptics. | `/home/tiger/nemu/src/lib/plugins/builtin/japanese-learning/useWordSelection.ts:19-60`; `/home/tiger/nemu/src/lib/plugins/builtin/japanese-learning/ui/token-display.tsx:119-167` | Preserve tap selection. Omit range dragging until a real multi-token action exists. |
| Selection clears when the token set changes, a one-token result auto-selects, and a new selection resets detail scroll. | `/home/tiger/nemu/src/lib/plugins/builtin/japanese-learning/ui/sentence-display.tsx:129-174` | Preserve all three rules. |
| Detail anatomy includes word/reading, actions, POS/conjugation tags, numbered senses, component structure, base/conjugation path, and token alternatives. | `/home/tiger/nemu/src/lib/plugins/builtin/japanese-learning/ui/token-details.tsx:35-159`, `:172-255` | Preserve analyzer-relevant sections; remove chat actions. |
| UI text uses Inter; Japanese textbook text uses Noto Serif JP with Japanese system fallbacks, `palt`, and optical sizing. | `/home/tiger/nemu/src/index.css:1-5`, `:99-100`, `:1844-1849` | Preserve the sans/serif distinction, subject to the transfer budget. |
| The theme is near-white or charcoal with a blue-violet primary and semantic POS washes. | `/home/tiger/nemu/src/index.css:9-96`, `:1851-2071` | Preserve the restrained theme and POS color semantics. |
| Nemu requests five segmentations but parses only the first alternative. | `/home/tiger/nemu/src/lib/plugins/builtin/japanese-learning/ichiran-service.ts:54-73`, `:99-135` | Do not copy this limitation. Sentence-level top-N is a first-class alpha result. |
| Nemu includes iOS standalone viewport and safe-area handling. | `/home/tiger/nemu/src/index.css:190-216` | Preserve safe-area padding and test standalone mode. |

Nemu calls this display state `grammarAnalysis`, but the reusable surface is token,
dictionary, and morphology presentation. The alpha must call it **analysis**, never
imply that the excluded grammar package is present.

## 3. Information architecture

Use one centered column, at most 48 rem wide, with normal document scrolling. The
same hierarchy works at phone and desktop widths:

1. **Header** — product name, pack version, and local readiness.
2. **Install card** — shown until the pinned pack is complete and verified.
3. **Composer** — multiline Japanese input and one primary Analyze action.
4. **Sentence result** — selected whole-sentence analysis rendered as tokens.
5. **Token detail** — detail for the selected token.
6. **Sentence alternatives** — collapsed top-N paths distinct from token readings.
7. **Runtime** — collapsed installation, timing, benchmark, and compatibility proof.

Do not use a modal, bottom sheet, persistent action footer, nested route, or side
panel. On a successful analysis, scroll the sentence result into view only when it
is wholly below the viewport; never steal focus from the input or selected token.

### Header

Show:

- `Browser Analyzer Alpha`
- `Ready offline` only when the pinned pack is complete, verified, and opened by
  the Worker;
- the short pack version; and
- a compact Runtime disclosure.

Do not equate `navigator.onLine === false` with readiness. Readiness comes from the
installed manifest, integrity record, and successful Worker open.

### Composer

- A real `<textarea>` with the label `Japanese text` and placeholder
  `Paste or type Japanese text`.
- Preserve newlines and permit ordinary text selection.
- `Analyze` is the only primary action.
- Cmd/Ctrl+Enter analyzes. Enter alone inserts a newline.
- Disable Analyze only for empty normalized input or while the same complete
  intent (text, top-N, entities, and punctuation mode) is already running. A
  newly submitted intent supersedes obsolete Worker computation.
- Cap input at 4,096 UTF-16 code units and show the live count. Reject an
  uninterrupted analyzable word run over 256 units with inline guidance.
- Put `Top results` (1-5) and entity boosts in a collapsed `Advanced` disclosure.
- Include `Use sample` as a quiet action. A useful morphology sample is
  `今日は公園で友達と話しました。`.

Do not add automatic analysis on every keystroke in the alpha. It obscures latency
measurement and creates unnecessary cancellation churn on phones.

## 4. Required visible states and copy

These are direct render conditions, not a request for a state-machine abstraction.

| Condition | Required presentation and exact primary copy |
|---|---|
| Required browser features unavailable | `This browser does not support the storage features required by this alpha.` No install action. |
| Pack absent | Title `Japanese analyzer data`; body `Download once, then analyze Japanese entirely on this device.`; action `Install analyzer data`. Show compressed and installed sizes from the manifest. |
| Downloading | `Downloading analyzer data…`; determinate progress, percent, and `{received} of {total}` bytes. |
| Verifying | `Verifying download…`; retain completed byte progress. |
| Installing | `Installing for offline use…`; do not pretend this phase has byte progress. |
| Pack ready, Worker opening | `Opening analyzer…` |
| Pack and Worker ready | `Ready offline` |
| New app shell downloaded | `App update downloaded`; tell the user to close every analyzer tab and reopen. The waiting Service Worker must not activate or remove the old shell while an old tab is live. |
| Incomplete or corrupt pack | `Analyzer data is incomplete or corrupted.`; primary action `Reinstall`; secondary action `Clear installed data`. |
| Insufficient storage | `Not enough device storage to install analyzer data.`; show required and available estimates when the browser provides them. |
| Recoverable install failure | `Analyzer data could not be installed.`; primary action `Retry`. Preserve any verified complete artifact; remove only the incomplete staging install. |
| Empty input | `Enter Japanese text to begin.` |
| Analysis running under 120 ms | Keep the prior/raw sentence without a spinner or layout change. |
| Analysis running at least 120 ms | Keep the prior/raw sentence and add `Analyzing…` with a subtle busy indicator. |
| Success | Render tokens, total latency, and alternatives count. Do not announce success with a toast. |
| No Japanese candidate | `No Japanese analysis was found.` Keep the original input editable. |
| Analysis failure | `Analysis failed. Your installed data was not changed.`; action `Try again`. |
| Worker crash | Reject the interrupted request, restart only after an explicit retry, and never leave later requests pending. |
| Benchmark running | `Running benchmark…`; disable only the benchmark action, not ordinary analysis. |
| Offline reopen proved | In Runtime, show `Opened from device storage with networking disabled.` only when the test harness has actually observed that condition. |

Failures remain inline beside the operation that failed. Toasts are reserved for a
short confirmation such as copied JSON; they never carry the only error text.

## 5. Sentence and token interaction

### Token anatomy

Match Nemu's information hierarchy:

- furigana: approximately 0.6 rem, fixed 0.9-rem row;
- surface: 1.4 rem on phones and 1.6 rem from the small breakpoint;
- POS abbreviation: approximately 0.5 rem, fixed 1-rem row;
- Japanese text: a serif Japanese stack with proportional alternates;
- surrounding UI and Latin metadata: a neutral sans-serif stack.

Use Nemu's labels where applicable: `名`, `動`, `形`, `副`, `助`, `代`, `接`,
`繋`, `感`, `助動`, `助数`, `表現`, `数`, and `接辞`.

Preserve semantic POS colors, but selection cannot rely on color alone. A selected
token receives a stronger wash, a crisp inset underline, and `aria-pressed="true"`.
Unknown and punctuation remain neutral.

The alpha should prefer system UI and Japanese fonts initially. A full Noto Serif JP
variable webfont is optional only if its compressed transfer and decoded memory fit
the agreed gates. The visual distinction matters; that particular font file does
not.

### Accessible mobile rules

- Each token is a real `<button type="button">`, not a clickable `<span>`.
- The accessible name includes surface, reading when different, and POS, for example
  `話しました, reading はなしました, Godan verb`.
- Enter, Space, or a tap selects. A second activation may keep the token selected;
  provide an explicit close only if an empty-detail state is useful.
- Keep at least 44 CSS px of block-axis target size and at least 32 CSS px of
  inline-axis target size. Narrow particles may use transparent internal padding;
  hit areas must not overlap.
- Preserve browser vertical panning. Do not apply `touch-action: none`, pointer
  capture, or drag-range selection in this milestone.
- Use a visible focus ring with at least 3:1 contrast.
- Keep token order in the DOM and ordinary tab order. Left/Right arrow navigation is
  optional, not a prerequisite for keyboard use.
- Keep focus on the activated token while details update. Announce the new detail
  heading through a polite live region; do not move focus after touch input.
- Result text, readings, glosses, scores, and JSON remain selectable and copyable.
- Respect `prefers-reduced-motion`. Reduced mode performs no token entrance motion.
- Use at least `max(1rem, env(safe-area-inset-bottom))` at the bottom and the
  corresponding safe-area inset on each exposed edge. Verify the home-indicator
  area in standalone display mode.

When a new result replaces the token set, clear selection. If there is exactly one
token, select it automatically. Otherwise begin with no selection and show
`Tap a word for details`.

## 6. Token detail

Render one solid, rounded card with a thin border. Avoid backdrop blur and Nemu's
multi-layer glass shadow until profiling proves they are harmless on the target
phones.

Order sections as follows, omitting empty sections rather than showing placeholders:

1. surface, reading, canonical root identity, and Copy;
2. conjugation/suffix tags;
3. numbered senses with POS, gloss, field, misc, dialect, and info metadata;
4. `Structure` for compound components;
5. `Conjugation path` or `Base form`;
6. `Alternative readings` for alternatives belonging to this token.

Nested components and conjugation roots use quieter bordered rows, not independently
elevated cards. Preserve array order from the analyzer. Do not truncate glosses or
hide metadata needed for parity inspection.

## 7. Sentence-level alternatives

Whole-sentence top-N paths and token-level alternative readings are different
concepts and must never share a control.

- The best path is displayed initially.
- When more paths exist, show a disclosure such as `Alternatives (4)`.
- Each row contains a compact tokenized preview and its exact integer score.
- Activating a row replaces the sentence token view and clears token selection.
- Equal-score paths remain in canonical semantic order supplied by the analyzer.
- Never recompute, round, or cosmetically normalize scores in the UI.

The clean API result is the source of truth. The legacy serializer belongs in
Runtime as copyable JSON and does not drive rendering.

## 8. Runtime and performance proof

Runtime is collapsed by default and contains:

- app version, pack version, manifest hash, compressed size, installed size, and
  Worker-open status;
- last analysis wall time, Worker compute time, requested/returned top-N, input
  length, and whether entity boosts were supplied;
- `Run benchmark`, its corpus/version, p50, p95, maximum, and calibrated 6x-proxy status;
- `Copy clean JSON` and `Copy legacy JSON`;
- `Clear installed data`, visually separated as a destructive maintenance action.

Analysis work and artifact decoding must stay in the Worker. The UI sends one
request, renders one response, and does not reproduce scoring or presentation
logic. Avoid animation during benchmark runs. Measurement starts after the Worker
and hot image are ready unless a benchmark is explicitly labeled cold start.

## 9. Preserve versus simplify

| Preserve from Nemu | Simplify or remove for alpha |
|---|---|
| Raw sentence remains visible during analysis. | OCR extraction and normalization prepass. |
| Furigana/surface/POS token stack. | Reader bottom drawer and overlay dismissal behavior. |
| Textbook POS palette and serif Japanese typography. | Chat, TTS, Ask actions, and the fixed three-button footer. |
| Tap selection, selection reset, one-token auto-selection. | Drag multi-selection, haptics, pointer capture, and `touch-action: none`. |
| Numbered senses, components, conjugation/base form, token alternatives. | Experimental grammar wording and UI. |
| Reduced-motion and iOS safe-area awareness. | Per-token spring/stagger animation and iOS text-animation workarounds made unnecessary by no animation. |
| Exact detail ordering and metadata. | Glass blur, large shadows, and nested independently scrolling panes. |
| Quiet light palette and legible textbook colors. | Theme switching and a mandatory bundled Noto variable font. |
| Cancellable stale-request protection. | Analyze-on-every-keystroke behavior. |

## 10. Responsive behavior

Required QA viewports are 390 x 844 and 1280 x 900. Also inspect 320 CSS px width.

- At phone widths, all sections are one column and the document owns scrolling.
- Tokens wrap at linguistic boundaries. Furigana, surface, and POS for one token
  never split across lines.
- The token region may cap at about 14 rem only when a long input would otherwise
  bury details; if capped, show visible overflow affordance and preserve vertical
  panning.
- Buttons never form a persistent footer over the home-indicator area.
- No horizontal page scroll, clipped furigana, footer overlap, or keyboard-induced
  viewport trap is acceptable.
- At desktop widths, retain the same reading order and centered column rather than
  introducing a second interaction model.

## 11. UI acceptance checks

The alpha UI is complete when automated Chromium coverage and later device QA prove:

- first-run install shows determinate bytes and all install phases;
- an interrupted install reopens as incomplete and can reinstall cleanly;
- a verified pack reopens with all network requests blocked;
- the default sample analyzes and exposes furigana, POS, senses, morphology, and
  sentence alternatives;
- selecting a conjugated token shows its base form and ordered path;
- clean and legacy JSON are copyable and correspond to the visible selected path;
- keyboard-only input, Analyze, token selection, disclosures, Retry, and Clear work;
- focus indication, live announcements, reduced motion, the implemented light theme,
  and text zoom to 200% remain usable;
- the 390 x 844 layout has no clipping, overlap, scroll trap, or unsafe-area issue;
- analysis creates no main-thread long task and no network request;
- install, analyze, corruption, no-result, and benchmark errors are visible inline.

Actual iPhone 13-class validation remains a production gate. Chromium with a measured
5.0-7.5x slowdown on the exact analyzer Worker is only the agreed alpha proxy.

## 12. Reference harness limitation

The integrated Nemu analyzer route could not be captured from the current checkout:
`/home/tiger/nemu/src/components/ui/drawer.tsx:4` imports `vaul`, but `vaul` is absent
from the current `package.json`, `bun.lock`, and `node_modules`, so Vite fails when it
loads the live drawer.

An external, read-only reference harness rendered Nemu's actual `SentenceDisplay`,
`TokenDisplay`, `TokenDetails`, Button components, theme, fonts, and POS CSS inside a
non-Vaul shell. Playwright exercised load, token rendering, and selection of
`話しました` at 1280 x 900 and 390 x 844. The sample data was synthetic; the shell
was an approximation; Chromium was not Safari; and no actual iPhone was tested.

Reference screenshots:

- `/mnt/c/Users/tiger/Documents/Codex/2026-08-28/i-wan/work/nemu-ui-screenshots/desktop-initial.png`
- `/mnt/c/Users/tiger/Documents/Codex/2026-08-28/i-wan/work/nemu-ui-screenshots/desktop-token-detail.png`
- `/mnt/c/Users/tiger/Documents/Codex/2026-08-28/i-wan/work/nemu-ui-screenshots/iphone13-token-detail.png`

This limitation does not weaken the source evidence above. It means the screenshots
are component references, not proof that the current integrated Nemu route builds or
that its drawer behavior works on iOS.
