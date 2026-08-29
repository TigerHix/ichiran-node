# Browser Analyzer Alpha design specification

Status: accepted implementation reference
Concepts: `design/mobile-install.png`, `design/mobile-analyzer.png`, and
`design/desktop-analyzer.png`

## Direction

The UI is a refined Japanese editorial utility: Japanese text is the visual subject,
while installation, controls, and diagnostics stay quiet. It borrows Nemu's useful
three-level token hierarchy—furigana, surface, POS—and tap-to-inspect behavior, but not
its OCR drawer, chat actions, multi-selection, blur, or spring-heavy presentation.

The installed application is one persistent page. On narrow screens, selected-token
details use an anchored bottom sheet. At `min-width: 900px`, results and details become a
stable two-column workspace. This is a responsive change of placement, not two separate
products.

## Locked visible copy

First install:

- `Ichiran`
- `Not installed`
- `Japanese analyzer data`
- `One download, then works entirely on this device.`
- `Dictionary & readings`
- `Conjugation & scoring`
- `Complete senses & glosses`
- `No account. No text leaves this device.`
- `Download size`
- `Install analyzer`
- `Data details`
- `Downloading`, `Verifying`, `Installing`
- `Retry`
- `About & licenses`

Installed workspace:

- `Ichiran`
- `Ready offline`
- `Use sample`
- `Top 1`
- `Analyze`
- `Alternatives`
- `Runtime & data` on wide screens; `Data & benchmark` on narrow screens
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

Background temperature is cool white, never cream. Shadows are absent by default. The
mobile sheet may use one `0 -1px 0` rule and a very restrained ambient shadow solely to
separate it from results.

Radii are `6px` for controls, `8px` for input/selection, and `20px 20px 0 0` only for the
mobile sheet. Do not turn rows or metadata into pills. POS and inflection tags may use a
small `4px` framed label because they encode real structure.

Spacing uses a 4px base with the main steps `4, 8, 12, 16, 24, 32, 48`. Minimum touch
target is 44×44 CSS pixels. Mobile side padding is 18px; desktop max width is 1360px with
32px gutters.

## Typography

- UI and control chrome: `Inter Variable`, followed by the system sans stack.
- Japanese content: `Noto Serif JP Variable`, then `Hiragino Mincho ProN`, `Yu Mincho`,
  and `serif`.
- Header wordmark: Japanese-content serif at 32px mobile / 30px desktop, 600.
- Editable Japanese input: 22px mobile / 20px desktop, 1.65 line height.
- Token surface: 30–34px mobile and 28–32px desktop, line height 1.25.
- Furigana: 12–13px, line height 1.2, at least 0.02em tracking.
- Token POS: 11–12px sans, muted, one line with accessible full text.
- Inspector surface: 30px mobile / 28px desktop.
- Body/gloss: 16px mobile / 15px desktop, 1.55 line height.
- Controls: deliberately set 15–16px/600; never inherit browser defaults.

Fonts must be bundled into the app shell so the UI remains typographically stable
offline. Use only the required subsets/weights after measuring their transfer cost.

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
7. `MobileDetailSheet` — drag handle, focus management, safe-area padding, and scroll body;
   it does not depend on a generic drawer library.
8. `PathAlternatives` — whole-sentence top-N paths, collapsed initially.
9. `RuntimePanel` — pack/version/bytes, Worker state, recent latency, benchmark action,
   legacy JSON, clear installed data, and reinstall. Collapsed initially.
10. `AppFooter` — local-execution statement and licenses link.

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
- Mobile token selection opens the sheet without preventing normal sentence scrolling.
  Escape/back closes it and restores focus to the token.
- Desktop selection updates the persistent inspector without page jump.
- Whole-sentence path alternatives and token-level alternative readings are visibly
  distinct concepts.
- Installation phase changes are announced through one polite live region. Progress uses
  native progress semantics. Persistence denial is information, not a fatal state.
- Motion is limited to 140–180ms opacity/transform transitions. Reduced-motion removes
  transforms and scrolling animation.
- Apply `env(safe-area-inset-top)` and `env(safe-area-inset-bottom)` in standalone mode.

## Responsive composition

At 390×844, the header, composer, and sentence are a single column. The selected-token
sheet occupies at most 56dvh and leaves the selected sentence visible. Detail content
scrolls inside the sheet, not behind it.

At 900px and wider, the result workspace is a 2:1 split with one shared outer border:
sentence/path sections on the left and a 360–440px inspector on the right. Empty inspector
state gives one short instruction and does not become an illustration. The input composer
stays full width above this split.

## Concept deviations already authorized

- Device status bars/home indicators in the generated concepts are framing only and are
  not implemented.
- Illustrative download size/progress values are dynamic manifest/runtime values.
- The mobile concept visually frames structural labels; implementation may use an
  underline plus text instead if that improves wrapping while preserving hierarchy.
- Dark mode is optional after the light implementation passes visual and performance QA.

Everything else in the three concepts is the visual implementation target.
