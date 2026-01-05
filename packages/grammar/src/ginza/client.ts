import { spawn, type ChildProcessWithoutNullStreams } from 'node:child_process';
import { createHash } from 'node:crypto';
import { once } from 'node:events';
import { existsSync, mkdirSync, readFileSync, writeFileSync } from 'node:fs';
import { createInterface } from 'node:readline';
import { join } from 'node:path';
import { z } from 'zod';
import { findPackageRoot } from '../paths.js';
import {
  GINZA_CONJUGATION_CLASSES,
  GINZA_DEP_LABELS,
  GINZA_INFLECTION_FORMS,
  GINZA_POS_LABELS,
  parseInflection,
} from './generated.js';
import type { GinzaDoc, GinzaMeta, GinzaToken, GinzaSentence } from './types.js';

const WorkerResponseSchema = z.object({
  id: z.string().optional().nullable(),
  ok: z.boolean(),
  docs: z.array(z.any()).optional(),
  meta: z.any().optional(),
  error: z.string().optional(),
});

const WorkerMetaSchema = z.object({
  model: z.string().nullable().optional(),
  lang: z.string().nullable().optional(),
  spacyVersion: z.string().nullable().optional(),
  ginzaVersion: z.string().nullable().optional(),
  jaGinzaModelVersion: z.string().nullable().optional(),
  pipes: z.array(z.string()),
  labels: z.record(z.array(z.string())),
});

const WorkerTokenSchema = z.object({
  i: z.number().int().nonnegative(),
  text: z.string(),
  lemma: z.string(),
  pos: z.string(),
  tag: z.string(),
  dep: z.string(),
  head: z.number().int(),
  start: z.number().int(),
  end: z.number().int(),

  norm: z.string().optional(),
  whitespace: z.string().optional(),
  feats: z.record(z.string()).optional(),
  inflection: z.string().optional(),
  reading: z.string().optional(),
  ne: z.string().optional(),
  ene: z.string().optional(),
  bunsetu: z
    .object({
      bi: z.string().nullable().optional(),
      positionType: z.string().nullable().optional(),
    })
    .optional(),
  clauseHead: z.number().int().optional(),

  misc: z.record(z.union([z.string(), z.literal(true)])).optional(),
});

const WorkerSentenceSchema = z.object({
  text: z.string(),
  start: z.number().int(),
  end: z.number().int(),
  tokens: z.array(WorkerTokenSchema),
});

const WorkerDocSchema = z.object({
  text: z.string(),
  sentences: z.array(WorkerSentenceSchema),
});

type WorkerToken = z.infer<typeof WorkerTokenSchema>;
type WorkerSentence = z.infer<typeof WorkerSentenceSchema>;
type WorkerDoc = z.infer<typeof WorkerDocSchema>;

const POS_SET = new Set<string>(GINZA_POS_LABELS);
const DEP_SET = new Set<string>(GINZA_DEP_LABELS);
const CONJ_SET = new Set<string>(GINZA_CONJUGATION_CLASSES);
const INFL_FORM_SET = new Set<string>(GINZA_INFLECTION_FORMS);

const WARNED = {
  pos: new Set<string>(),
  dep: new Set<string>(),
  conjugationClass: new Set<string>(),
  inflectionForm: new Set<string>(),
  inflectionExtra: new Set<string>(),
};

function warnOnce(kind: keyof typeof WARNED, value: string, msg: string): void {
  const set = WARNED[kind];
  if (set.has(value)) return;
  set.add(value);
  // eslint-disable-next-line no-console
  console.warn(msg);
}

/** Parse inflection once and enrich token with typed fields */
function enrichToken(t: WorkerToken): GinzaToken {
  if (t.pos && !POS_SET.has(t.pos)) {
    warnOnce('pos', t.pos, `[grammar][ginza] unseen POS label: '${t.pos}'`);
  }
  if (t.dep && !DEP_SET.has(t.dep)) {
    warnOnce('dep', t.dep, `[grammar][ginza] unseen dependency label: '${t.dep}'`);
  }

  if (t.inflection) {
    const parts = t.inflection
      .split(/[;,]/)
      .map((p) => p.trim())
      .filter(Boolean);
    const cc = parts[0];
    const form = parts[1];
    if (cc && !CONJ_SET.has(cc)) {
      warnOnce(
        'conjugationClass',
        cc,
        `[grammar][ginza] unseen conjugation class: '${cc}' (raw inflection='${t.inflection}')`
      );
    }
    if (form && !INFL_FORM_SET.has(form)) {
      warnOnce(
        'inflectionForm',
        form,
        `[grammar][ginza] unseen inflection form: '${form}' (raw inflection='${t.inflection}')`
      );
    }
    if (parts.length > 2) {
      warnOnce(
        'inflectionExtra',
        t.inflection,
        `[grammar][ginza] inflection has extra parts (not modeled): '${t.inflection}'`
      );
    }
  }

  const parsed = parseInflection(t.inflection);
  return {
    ...t,
    pos: t.pos as GinzaToken['pos'],
    dep: t.dep as GinzaToken['dep'],
    conjugationClass: parsed?.conjugationClass ?? undefined,
    inflectionForm: parsed?.inflectionForm ?? undefined,
  };
}

function enrichSentence(s: WorkerSentence): GinzaSentence {
  return {
    ...s,
    tokens: s.tokens.map(enrichToken),
  };
}

function enrichDoc(d: WorkerDoc): GinzaDoc {
  return {
    ...d,
    sentences: d.sentences.map(enrichSentence),
  };
}

export type GinzaClientOptions = {
  python?: string;
  workerPath?: string;
  warmup?: boolean;
  /** Directory to cache GiNZA parse results (for faster test runs) */
  cacheDir?: string;
};

export class GinzaClient {
  private proc: ChildProcessWithoutNullStreams | null = null;
  private rl: ReturnType<typeof createInterface> | null = null;
  private pending = new Map<
    string,
    | { kind: 'analyze'; resolve: (v: GinzaDoc[]) => void; reject: (e: unknown) => void }
    | { kind: 'meta'; resolve: (v: GinzaMeta) => void; reject: (e: unknown) => void }
  >();
  private nextId = 1;
  private python: string;
  private workerPath: string;
  private cacheDir: string | null;

  constructor(opts: GinzaClientOptions = {}) {
    this.python = opts.python ?? 'python3';
    const pkgRoot = findPackageRoot(import.meta.url);
    this.workerPath = opts.workerPath ?? join(pkgRoot, 'python', 'ginza_worker.py');
    this.cacheDir = opts.cacheDir ?? null;
    if (this.cacheDir && !existsSync(this.cacheDir)) {
      mkdirSync(this.cacheDir, { recursive: true });
    }
  }

  private getCacheKey(texts: string[]): string {
    return createHash('sha256').update(JSON.stringify(texts)).digest('hex');
  }

  private getCachePath(key: string): string | null {
    return this.cacheDir ? join(this.cacheDir, `${key}.json`) : null;
  }

  async start(): Promise<void> {
    if (this.proc) return;
    this.proc = spawn(this.python, ['-u', this.workerPath], {
      stdio: ['pipe', 'pipe', 'pipe'],
      env: process.env,
    });

    this.proc.on('exit', (code, signal) => {
      const err = new Error(`ginza worker exited (code=${code}, signal=${signal})`);
      for (const { reject } of this.pending.values()) reject(err);
      this.pending.clear();
      this.proc = null;
      this.rl?.close();
      this.rl = null;
    });

    this.rl = createInterface({ input: this.proc.stdout });
    this.rl.on('line', (line: string) => {
      let parsed: unknown;
      try {
        parsed = JSON.parse(line);
      } catch {
        return;
      }
      const msg = WorkerResponseSchema.safeParse(parsed);
      if (!msg.success) return;
      const { id, ok, docs, meta, error } = msg.data;
      if (!id) return;
      const entry = this.pending.get(id);
      if (!entry) return;
      this.pending.delete(id);
      if (!ok) {
        entry.reject(new Error(error ?? 'ginza worker error'));
        return;
      }

      if (entry.kind === 'meta') {
        entry.resolve(WorkerMetaSchema.parse(meta) as GinzaMeta);
        return;
      }

      // analyze: Zod-parse, then enrich with pre-parsed inflection
      entry.resolve(
        (docs ?? []).map((d: unknown) => enrichDoc(WorkerDocSchema.parse(d)))
      );
    });

    this.proc.once('error', (e) => {
      throw e;
    });

    await Promise.race([once(this.proc.stderr, 'data'), new Promise((r) => setTimeout(r, 50))]);
  }

  async stop(): Promise<void> {
    if (!this.proc) return;
    this.proc.kill('SIGTERM');
    this.proc = null;
    this.rl?.close();
    this.rl = null;
    this.pending.clear();
  }

  async analyze(texts: string[]): Promise<GinzaDoc[]> {
    // Check cache first
    const cacheKey = this.getCacheKey(texts);
    const cachePath = this.getCachePath(cacheKey);
    if (cachePath && existsSync(cachePath)) {
      const cached = JSON.parse(readFileSync(cachePath, 'utf-8')) as GinzaDoc[];
      return cached;
    }

    if (!this.proc || !this.rl) await this.start();
    if (!this.proc) throw new Error('ginza worker not running');

    const id = `req-${this.nextId++}`;
    const payload = JSON.stringify({ id, op: 'analyze', texts });
    const p = new Promise<GinzaDoc[]>((resolve, reject) => {
      this.pending.set(id, { kind: 'analyze', resolve, reject });
    });
    this.proc.stdin.write(payload + '\n');
    const result = await p;

    // Write to cache
    if (cachePath) {
      writeFileSync(cachePath, JSON.stringify(result), 'utf-8');
    }

    return result;
  }

  async meta(): Promise<GinzaMeta> {
    if (!this.proc || !this.rl) await this.start();
    if (!this.proc) throw new Error('ginza worker not running');

    const id = `req-${this.nextId++}`;
    const payload = JSON.stringify({ id, op: 'meta' });
    const p = new Promise<GinzaMeta>((resolve, reject) => {
      this.pending.set(id, { kind: 'meta', resolve, reject });
    });
    this.proc.stdin.write(payload + '\n');
    return await p;
  }
}
