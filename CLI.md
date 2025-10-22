# Ichiran CLI

## Usage

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
