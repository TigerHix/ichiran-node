# Ichiran Node.js Port

Node.js/TypeScript port of [Ichiran](https://github.com/tshatrov/ichiran) - Japanese text segmentation and analysis library.

## Installation

```bash
cd /home/tiger/ichiran/ichiran-node
bun install
bun run build
```

## Configuration

Create a `.env` file in the project root:

```env
ICHIRAN_DB_URL=postgresql://postgres:password@localhost:6777/jmdict
ICHIRAN_LISP_CONTAINER=komu-ichiran-main-1
```

Alternatively, set environment variables directly:

```bash
export ICHIRAN_DB_URL="postgresql://postgres:password@localhost:6777/jmdict"
```

## Usage

### Basic Romanization

```bash
node dist/cli.js "こんにちは"
```

Output:
```
konnichiwa
```

### With Dictionary Info

Use the `-i` or `--with-info` flag to display word definitions alongside romanization:

```bash
node dist/cli.js -i "今日はいい天気です"
```

Output shows each word with its reading and definition.

### Full JSON Output

Use the `-f` or `--full` flag to get complete segmentation data as JSON:

```bash
node dist/cli.js -f "ご注文はうさぎですか"
```

Returns structured JSON with all segmentation details, including:
- Text and readings for each word
- Dictionary entries and glosses
- Conjugation information
- Segmentation scores

### Multiple Segmentations

Combine `-f` with `-l` to get alternative segmentations:

```bash
node dist/cli.js -f -l 3 "みんな土足でおいで"
```

The `-l` flag limits the number of segmentations returned (default: 1).

## CLI Options

```
Options:
  -h, --help              Print help text
  -i, --with-info         Print dictionary info with romanization
  -f, --full              Full split info (as JSON)
  -l, --limit <number>    Limit segmentations (default: 1, use with -f)
```

## Examples

**Simple text:**
```bash
node dist/cli.js "食べました"
# Output: tabemashita
```

**Detailed analysis:**
```bash
node dist/cli.js -i "食べました"
# Shows: 食べる (taberu) - "to eat" + past tense
```

**JSON export:**
```bash
node dist/cli.js -f "桜が咲いた" > output.json
# Saves full segmentation data to output.json
```

**Compare alternatives:**
```bash
node dist/cli.js -f -l 5 "時々" | jq
# Pretty-prints up to 5 different segmentations
```

## Environment Variables

- `ICHIRAN_DB_URL`: PostgreSQL connection string (required)
  - Format: `postgresql://user:password@host:port/database`
  - Default database: `jmdict`
- `ICHIRAN_LISP_CONTAINER`: Docker container name for Lisp CLI comparison tests (optional)
  - Default: `komu-ichiran-main-1`

## Development

### Run Tests

Tests automatically load configuration from `.env` file:

```bash
bun test
```

Run specific test suites:

```bash
bun test tests/counters.test.ts
bun test tests/segmentation.test.ts
bun test tests/cli-parity.test.ts  # Compares with Lisp CLI output
```

### Type Checking

```bash
bun run typecheck
```

### Build

```bash
bun run build
```

## Project Status

**Core Features:** Complete
- Japanese text segmentation
- Dictionary lookup (JMDict)
- Conjugation analysis
- Romanization (multiple systems)
- Counter word recognition
- Number parsing

**Test Suite:** All 750+ tests from original Lisp implementation ported

See [CLAUDE.md](./CLAUDE.md) for detailed development documentation.

## License

MIT (matching original Ichiran license)