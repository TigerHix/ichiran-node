# Ichiran CLI

The `ichiran` executable is an explicit command interface to one verified packed
release.

```bash
bun run build
export ICHIRAN_PACK_DIR=/absolute/path/to/analyzer-release

ichiran analyze --limit 3 "みんな土足でおいで"
ichiran romanize "今日はいい天気です"
ichiran romanize --method kunrei-siki --normalize-punctuation "こんにちは。"
ichiran entry 54321
ichiran details --locale zh-Hans "猫"
ichiran entry --locale zh-Hans 54321
```

`analyze`, `details`, and `entry` print their product JSON directly. Definition
commands default to English and accept `--locale en` or `--locale zh-Hans`. `romanize` prints only the
romanized string. Text may be supplied as multiple arguments and is joined with
spaces. Analyzer failures are written as `ERROR [code]: message` and exit with status
2.

There is no implicit command, `--full`, `--with-info`, or `--eval` compatibility
mode. See [MIGRATION.md](./MIGRATION.md) for the old-to-new mapping. Grammar is not a
CLI mode.
