# ichiran-node

Node.js/TypeScript port of [Ichiran](https://github.com/tshatrov/ichiran) - Japanese text segmentation and analysis library.

## Installation

```bash
bun install
bun run build
```

## Configuration

Set `ICHIRAN_DB_URL`:

```bash
export ICHIRAN_DB_URL="postgresql://postgres:password@localhost:5432/jmdict"
```

Or create `.env` file.

## Usage

### CLI

```bash
ichiran-cli "こんにちは"
# konnichiwa

ichiran-cli -i "今日はいい天気です"
# with definitions

ichiran-cli -f "text"
# full JSON

ichiran-cli -l 5 -f "text"
# 5 alternatives
```

### API

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

## Packages

Monorepo with 5 packages. See [PACKAGES.md](./PACKAGES.md):

- `@ichiran/core` - segmentation, dict, romanize, connection
- `@ichiran/grammar` - grammar runtime, predicates, defs
- `@ichiran/api` - HTTP server
- `@ichiran/cli` - CLI
- `@ichiran/data` - DB init/ETL

## Data Loading

See [DATA.md](./DATA.md):

```bash
ichiran-data init-db
ichiran-data download
ichiran-data load-jmdict
ichiran-data load-kanjidic
ichiran-data load-conjugations
ichiran-data load-secondary-conjugations
ichiran-data load-custom --extra
ichiran-data apply-errata
ichiran-data stats
```

## Development

### Run Tests

```bash
bun test
```

### Build

```bash
bun run build
```

## License

[MIT](./LICENSE)