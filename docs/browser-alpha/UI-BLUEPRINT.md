# Browser analyzer UI blueprint

Status: implemented product contract, 2026-09-02

Scope: the analyzer-only browser integration demo. OCR, chat, TTS, experimental
grammar, a general Kanjidic UI, and consumer application-shell policy are excluded.

The browser floor is Safari 26+ or current Chromium. Analyzer-data installation
requires a Worker, OPFS, Web Locks, writable file streams, `DecompressionStream`,
and IndexedDB. It does not require or register a Service Worker. Unsupported
browsers show one actionable compatibility message and no install action.

## Product rule

This is a Japanese-analysis workbench, not a launch page or a qualification
dashboard. Its permanent surface contains only input, analysis, token interaction,
dictionary and morphology details, examples, and on-device data maintenance.

The application must communicate these facts without celebratory status copy:

1. Japanese data is downloaded once and stored on this device.
2. Text and analysis stay on this device.
3. Once the consumer has loaded its page, the installed analyzer works with the
   network unavailable.
4. A damaged analyzer-data install or available pack update is never hidden.

Whether the containing application can itself launch offline is a consumer concern.
The consumer may use a Service Worker, a native bundle, ordinary HTTP caching, or no
offline shell at all. This package must not register a Service Worker, ship a PWA
manifest, cache application assets, or own shell-update UX.

The analyzer adapter does not rely on a consumer cache for its own manifest. If the
published manifest is unreachable, it opens a previously verified pack from the
signed OPFS install marker. When online, the published manifest remains the authority
for detecting that an installed pack needs replacement.

Pack version, hashes, timings, benchmarks, raw JSON, and compatibility evidence are
qualification concerns. They do not belong in the product UI or public browser
client. The explicit qualification build may attach test-only operations to the
application's existing analyzer instance; it must not create a second Worker or
enter the production module graph.

## Komi interaction evidence

The visual styling is deliberately quieter than Komi, but the analyzer interaction
and information hierarchy come directly from these files:

| Behavior | Komi source | Browser decision |
|---|---|---|
| Furigana sits over an inline, selectable Japanese surface. | `/home/tiger/komi/src/components/SentenceDisplay.tsx` | Keep the two-line token anatomy and linguistic wrapping. |
| Tap selects one token; dragging extends a contiguous selection. | `/home/tiger/komi/src/hooks/useWordSelection.ts` | Keep both. A range exposes a copy action; one token opens lexical details. |
| Details use a word-and-reading header, then meanings with POS attached to each sense, followed by structure, conjugations, and alternative meanings. | `/home/tiger/komi/src/components/TokenDetails.tsx` | Keep this hierarchy and omit empty sections. |
| Sentence editing and analysis are one focused task. | `/home/tiger/komi/src/components/InlineEditor.tsx` | Use one labeled textarea and one primary Analyze action. |
| Semantic token colors support scanning without becoming the interface theme. | `/home/tiger/komi/src/index.css` | Use restrained POS-colored underlines; selection also has a fill and inset outline. |

Komi's grammar, external search, contenteditable behavior, unsafe HTML rendering,
and app-specific navigation are not copied.

## Information architecture

The header contains the Ichiran wordmark and a settings menu. The menu owns local
data size, removal, and licenses; it does not repeat readiness or release metadata.

Before installation, the page is a narrow, centered install surface. After
installation, the workbench is:

- a compact composer with textarea, examples, live input count, and Analyze;
- the chosen sentence parse rendered as furigana tokens;
- a desktop details pane adjacent to the sentence;
- a mobile bottom sheet for the same details;
- a collapsed list of other whole-sentence parses; and
- a quiet footer stating that analysis runs on this device.

Desktop and mobile share the same document order and analyzer instance. The mobile
sheet is a presentation change, not a second workflow.

## Installation and lifecycle states

State is rendered directly from the analyzer release, installed-pack status, and
current operation. Do not add a general workflow state machine.

| Condition | Product presentation |
|---|---|
| Browser features unavailable | Explain that the browser cannot store the analyzer locally and recommend a current non-private browser. |
| Release or pack status opening | Show a compact skeleton in the install surface. |
| Pack absent | `Install Japanese data`; show download and stored sizes and `Install analyzer data`. |
| Downloading, checking, or saving | Show the current terse phase, determinate percentage, and byte progress. |
| Pack stale | Explain that local data needs an update and offer reinstall. |
| Pack incomplete or corrupt | Explain that saved data is incomplete and offer reinstall and removal. |
| Insufficient storage | State that there is not enough free storage. |
| Recoverable install failure | State that the download did not finish and offer Retry. |
| Worker or pack invalidation | Reject the current request, surface the error, and refresh installed status. |

Do not show `Alpha`, `Ready offline`, version strings, success banners, or a toast
for successful install or analysis. Errors stay beside the affected operation.

## Composer and examples

- Use a real labeled `<textarea>` with a Japanese placeholder.
- Preserve ordinary text selection and newlines.
- Cmd/Ctrl+Enter analyzes; Enter alone inserts a newline.
- Do not analyze on every keystroke.
- A newer submission supersedes obsolete Worker computation, and stale results are
  rejected if the text changes.
- Cap input at the analyzer's public maximum and show a live count.
- Analyze is the only primary action.
- Choosing an example replaces the input and analyzes immediately.

Examples must exercise useful analyzer behavior, not merely vary prose. The shipped
set covers everyday segmentation, inflection, counters, numbers, ambiguous
readings, mixed kana and kanji, punctuation, colloquial forms, compounds, and
kana-only text.

## Sentence and selection

Each analyzable token is a real button with:

- a fixed furigana row;
- a large Japanese surface;
- a semantic POS-colored underline;
- an accessible name containing surface, reading, and POS; and
- `aria-pressed` plus a non-color-only selected treatment.

Punctuation remains neutral and outside token selection. Tokens preserve source
order in the DOM and wrap only between linguistic units.

Tap or keyboard activation toggles one token. Pointer drag selects a contiguous
range. Vertical touch panning remains available, and the implementation does not
capture the pointer or apply `touch-action: none`. A range exposes its exact source
text and Copy. A one-token result auto-selects. A new analysis or parse clears the
old selection.

The desktop detail pane stays in the workbench. On narrow screens, completing a
selection opens a Radix bottom sheet; it must not open mid-drag. Closing details
returns focus to the selected token where appropriate.

## Token details

Render the following order, omitting empty sections:

1. surface and reading in a fixed header with Copy;
2. numbered meanings, with readable POS labels attached to each individual sense;
3. component structure as an equation followed by nested component cards;
4. conjugations as nested base-form cards with readable form and politeness labels; and
5. alternative meanings as nested token cards.

Use Komi's exact learner-facing POS vocabulary, including `Transitive Verb`,
`Intransitive Verb`, and specific classes such as `Godan Verb (-ku Special)`.
Never expose JMdict abbreviations such as `vt`, `vi`, or `v5k-s` in visible copy.
Sense restrictions and POS carry-forward follow the dictionary data; miscellaneous
internal tags do not become an undifferentiated metadata dump. Dictionary form lists
and a separate base-form row are deliberately omitted because the nested structure
and conjugation cards carry that information in context.

Meanings are the primary content. Scores, physical pack identity, raw analyzer JSON,
and migration shapes are not product information.

Whole-sentence parses and token reading alternatives are different concepts. The
best sentence parse is active initially. Other parses appear in a disclosure; each
row is a tokenized preview. Selecting one replaces the sentence and clears token
selection. The active parse is omitted from the disclosure, so the previous best
parse remains available after switching.

## Visual system

Use current owned shadcn/Radix components and Tailwind utilities only where they
improve semantics or consistency. Generated components are application code and are
trimmed to the variants actually used.

- Geist is the compact UI face; Japanese uses native Japanese system fonts.
- Surfaces are cool neutral rather than pure white or black.
- One cool blue accent handles focus and selection.
- POS colors are semantic annotations, not decorative theme colors.
- Corners use one compact radius scale. Shadows are limited to separation that a
  border cannot provide.
- There are no gradients, glass effects, entrance choreography, or marketing art.
- Motion is limited to short hover/press feedback and is disabled under reduced
  motion.
- System dark mode changes semantic color tokens without changing layout.

The absence of imagery is intentional: this is a dense product tool whose subject is
the user's Japanese text, not a promotional page.

## Responsive and accessibility contract

Required automated viewports are 390 x 844 and 1280 x 900; also inspect 320 CSS px.

- No horizontal page scrolling, clipped furigana, footer overlap, or unsafe-area
  obstruction.
- Interactive controls retain visible focus treatment and usable touch height.
- The document owns page scrolling. Only long lexical details may scroll within the
  bounded desktop pane or mobile sheet.
- Sheet title and description remain available to assistive technology even when
  visually hidden.
- Loading and update states use polite announcements; failures use alerts.
- Text zoom and long meanings may grow vertically rather than truncate.
- Safe-area insets protect the demo layout and mobile detail sheet.

## Acceptance checks

The UI is complete when automated Chromium qualification proves:

- first install, progress, interrupted install, reinstall, and removal;
- verified pack persistence across browser restart and no analyzer-data requests
  after the already-loaded page is put offline;
- corruption quarantine and update behavior;
- default and representative example analysis;
- furigana, token selection, meanings, morphology, romanization, and other parses;
- keyboard input and selection, pointer range selection, copy, Retry, and Clear;
- one analyzer Worker even in explicit qualification mode;
- production output contains no qualification bridge or global test API;
- phone and desktop layouts have no clipping or scroll traps; and
- dark mode and reduced motion remain usable.

Calibrated performance and raw parity stay in the test harness. Physical Safari and
iPhone qualification remain release gates; desktop Chromium emulation is not a
substitute for them.

Consumer shell caching, installability, navigation fallback, and shell-update tests
belong in the consuming application and are not Ichiran acceptance gates.
