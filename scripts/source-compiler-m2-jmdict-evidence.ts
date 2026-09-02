import postgres from 'postgres';
import {
  canonicalEntriesDigest,
  canonicalEntryDigest
} from '../packages/data/src/source-compiler/digest.js';
import {
  loadJmdictEntries,
  parseJmdictEntry
} from '../packages/data/src/source-compiler/jmdict.js';

const [sourcePath] = process.argv.slice(2);
if (!sourcePath) {
  throw new Error('Usage: bun scripts/source-compiler-m2-jmdict-evidence.ts <verified-jmdict-file>');
}
const sourceId = 'edrdg-jmdict-e-2026-01-01';
const sourceDigests = new Map<number, string>();

const sourceDigest = await canonicalEntriesDigest((async function* () {
  for await (const entry of loadJmdictEntries(sourcePath, sourceId)) {
    sourceDigests.set(entry.seq, canonicalEntryDigest(entry));
    yield entry;
  }
})());

const databaseUrl = process.env.SOURCE_COMPILER_ORACLE_URL;
const sql = databaseUrl
  ? postgres(databaseUrl, { max: 1 })
  : postgres({ host: '/var/run/postgresql', database: 'ichiran_oracle_ea958336', max: 1 });

const rows = await sql<Array<{ seq: number; content: string }>>`
  SELECT seq, content
  FROM entry
  WHERE root_p
    AND seq BETWEEN 1000000 AND 9999999
  ORDER BY seq
`;

const producerDigests = new Map<number, string>();
const mismatched: number[] = [];
let producerOrdinal = 0;
const producerDigest = await canonicalEntriesDigest((function* () {
  for (const row of rows) {
    const entry = parseJmdictEntry(row.content, sourceId, producerOrdinal++);
    const digest = canonicalEntryDigest(entry);
    producerDigests.set(entry.seq, digest);
    if (sourceDigests.get(entry.seq) !== digest) mismatched.push(entry.seq);
    yield entry;
  }
})());

const sourceOnly = [...sourceDigests.keys()].filter(seq => !producerDigests.has(seq));
const producerOnly = [...producerDigests.keys()].filter(seq => !sourceDigests.has(seq));
await sql.end();

process.stdout.write(`${JSON.stringify({
  formatVersion: 1,
  source: {
    id: sourceId,
    path: sourcePath,
    ...sourceDigest
  },
  qualifiedProducerContent: producerDigest,
  comparison: {
    equalEntries: producerDigests.size - mismatched.length - producerOnly.length,
    mismatched,
    sourceOnly,
    producerOnly
  }
}, null, 2)}\n`);
