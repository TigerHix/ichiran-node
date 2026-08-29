# Browser Analyzer Alpha design specification

Status: accepted implementation reference
Companion interaction contract: `UI-BLUEPRINT.md`

## Direction

The UI is a refined Japanese editorial utility: Japanese text is the visual subject,
while installation, controls, and diagnostics stay quiet. It borrows Nemu's useful
three-level token hierarchy—furigana, surface, POS—and tap-to-inspect behavior, but not
its OCR drawer, chat actions, multi-selection, blur, or spring-heavy presentation.

The installed application is one persistent, centered page. Selected-token details
remain in document flow below the sentence at every width. This keeps one reading order,
one scroll owner, and one interaction model; the standalone demo does not inherit Nemu's
reader-specific bottom drawer or introduce a desktop side panel.

## Locked visible copy

First install:

- `Browser Analyzer Alpha`
- `Not installed`
- `Japanese analyzer data`
- `Download once, then analyze Japanese entirely on this device.`
- `Dictionary & readings`
- `Conjugation & scoring`
- `Complete senses & glosses`
- `No account. No text leaves this device.`
- `Download size`
- `Install analyzer data`
- `Data details`
- `Downloading analyzer data`, `Verifying download`, `Installing for offline use`
- `Retry`
- `About & licenses`

Installed workspace:

- `Browser Analyzer Alpha`
- `Ready offline`
- `Use sample`
- `Advanced` with `Top results`, entity spans, and punctuation normalization
- `Analyze`
- `Alternatives`
- `Runtime & data`
- `Base form`
- `Runs entirely on this device`
- transient status: `Analyzing…` and `Analysis failed`

Manifest-derived versions, exact byte sizes, phases, percentages, latencies, and result
content are dynamic. The implementation must not freeze the illustrative concept's
download-size or progress values.

No other above-the-fold labels may be added without a functional requirement. In
particular, there is no hero copy, eyebrow, feature badge, grammar label, AI/chat action,
OCR action, or TTS action.

## Design tokens

Use OKLCH in CSS where supported, with these sRGB values as review anchors:

| Token | Value | Use |
|---|---|---|
| `--background` | `#FCFCFD` | page and sheet background |
| `--surface` | `#FFFFFF` | input/inspector surfaces |
| `--ink` | `#111827` | primary text |
| `--muted` | `#667085` | metadata and status |
| `--border` | `#D0D5DD` | hairline rules and controls |
| `--accent` | `#1D4ED8` | primary action and selection |
| `--accent-strong` | `#173FAF` | pressed action |
| `--accent-soft` | `#EEF3FF` | selected token fill |
| `--danger` | `#B42318` | install/analyze errors only |
| `--noun` | `#2F6FAE` | noun underline/POS accent |
| `--verb` | `#27845B` | verb underline/POS accent |
| `--adjective` | `#7656B3` | adjective underline/POS accent |
| `--particle` | `#B86B16` | particle underline/POS accent |

Background temperature is cool white, never cream. Shadows are absent by default.

Radii are `6px` for controls and `8px` for input/selection. Do not turn rows or metadata
into pills. POS and inflection tags may use a small `4px` framed label because they
encode real structure.

Spacing uses a 4px base with the main steps `4, 8, 12, 16, 24, 32, 48`. Minimum touch
target is 44×44 CSS pixels. Mobile side padding is 18px; the document column is at most
768px wide.

## Typography

- UI and control chrome: `Inter Variable`, followed by the system sans stack.
- Japanese content: `Hiragino Mincho ProN`, `Yu Mincho`, `Noto Serif CJK JP`, and
  `serif`.
- Header wordmark: Japanese-content serif at 32px mobile / 30px desktop, 600.
- Editable Japanese input: 22px mobile / 20px desktop, 1.65 line height.
- Token surface: 30–34px mobile and 28–32px desktop, line height 1.25.
- Furigana: 12–13px, line height 1.2, at least 0.02em tracking.
- Token POS: 11–12px sans, muted, one line with accessible full text.
- Inspector surface: 30px mobile / 28px desktop.
- Body/gloss: 16px mobile / 15px desktop, 1.55 line height.
- Controls: deliberately set 15–16px/600; never inherit browser defaults.

Bundle the small Latin UI font subsets. Prefer installed Japanese system fonts for this
performance milestone; a multi-megabyte Japanese webfont is not required for offline
correctness and may be added only after measuring transfer and decoded memory.

## Component inventory

1. `AppHeader` — wordmark and truthful installation status; no navigation bar.
2. `InstallPanel` — inclusion rows, manifest size, privacy statement, action, phases,
   failure/retry, data details, and licenses.
3. `InputComposer` — multiline textarea, clear control, sample action, top-N selector,
   analyze action, and delayed progress state.
4. `SentenceResult` — semantic line of `TokenButton` elements plus punctuation.
5. `TokenButton` — furigana/surface/POS stack, POS underline, selected/focus/pressed states.
6. `TokenInspector` — selected surface/reading, structural tags, numbered senses, base form,
   ordered conjugation path, components, suffix information, and token alternatives.
7. `PathAlternatives` — whole-sentence top-N paths, collapsed initially.
8. `RuntimePanel` — pack/version/bytes, Worker state, recent latency, benchmark action,
   legacy JSON, clear installed data, and reinstall. Collapsed initially.
9. `AppFooter` — local-execution statement and licenses link.

Cards are not a default primitive. Input, result workspace, inspector, and install section
use open regions separated by borders and whitespace.

## Icon inventory

Use a single 1.75px rounded-outline SVG family with `currentColor`:

- close/clear: circle-close in input;
- chevron-down/right: disclosures and rows;
- storage/database: install/data entry point;
- download: install symbol and download phase;
- book: dictionary/readings inclusion;
- morphology grid: conjugation/scoring inclusion;
- document: senses/glosses inclusion;
- lock: local privacy statement;
- info: licenses/about.

Icons remain subordinate at 18–22px. The large install symbol may be 72px. No decorative
icons appear in the installed workspace.

## Interaction and state rules

- Preserve raw input while analysis is pending. Show `Analyzing…` only after 120ms to
  avoid flicker.
- A completed result selects no token unless only one semantic token exists. New input or
  a new result clears selection and resets inspector scroll.
- Tokens are real buttons with keyboard focus and `aria-pressed`; punctuation is not a
  fake button unless it has details.
- Token selection updates the persistent inspector without stealing focus or introducing
  a nested scroll owner. Escape or the explicit close action clears it and restores
  focus to the token.
- Whole-sentence path alternatives and token-level alternative readings are visibly
  distinct concepts.
- Installation phase changes are announced through one polite live region. Progress uses
  native progress semantics. Persistence denial is information, not a fatal state.
- Motion is limited to 140–180ms opacity/transform transitions. Reduced-motion removes
  transforms and scrolling animation.
- Apply `env(safe-area-inset-top)` and `env(safe-area-inset-bottom)` in standalone mode.

## Responsive composition

At 320px, 390×844, and desktop widths, the header, composer, sentence, details, and
runtime remain one centered column in the same DOM order. Tokens wrap only between
tokens. The document owns vertical scrolling and must never gain horizontal overflow.

## Reference-concept policy

Earlier generated mobile-sheet/two-column concepts are visual references for typography,
token anatomy, and restrained color only. `UI-BLUEPRINT.md` and the persistent layout
above own interaction and responsive behavior. Device status bars and illustrative byte
values are never product UI. Dark mode remains optional after the light implementation
passes visual and performance QA.
