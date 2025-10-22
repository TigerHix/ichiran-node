# ichiran-node

Node.js/TypeScript port of [Ichiran](https://github.com/tshatrov/ichiran) - Japanese text segmentation and analysis library.

## Installation

```bash
bun install
bun run build
```

## Setup

* You must have a PostgreSQL database running.
* Create a `.env` file with your PostgreSQL connection string. For example:
  ```bash
  ICHIRAN_DB_URL="postgresql://postgres:password@localhost:5432/jmdict"
  ```
* Run `./scripts/build-db.sh jmdict` to build the database (`jmdict` is the database name). This will take ~30 minutes.
* Run tests to ensure all tests pass:
  ```bash
  bun test
  ```
* Start the API server:
  ```bash
  bun run dev
  ```
  * See [API.md](./API.md) for more details.
* You can also use the CLI directly:
  ```bash
  bun run cli "こんにちは"
  ```
  * See [CLI.md](./CLI.md) for more details.

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

[FSL-1.1-MIT](./LICENSE)
