import type { ChildProcess } from 'node:child_process';
import { readFileSync } from 'node:fs';
import { resolve } from 'node:path';
import { expect, test, watchConsoleHealth } from './console-health.js';
import {
  analyzerReady,
  attachAnalyzerWorker,
  median,
  singleCpuAffinity,
  startCpuHogs,
  stopCpuHogs
} from './offline-analyzer-helpers.js';
import type { AnalyzerPackManifest, RustKernelMetrics } from '../src/protocol.js';

interface M1OracleWitness {
  readonly name: string;
  readonly codeUnits: readonly number[];
  readonly serialized: string;
}

const retainedM1Witnesses = JSON.parse(readFileSync(
  resolve(import.meta.dirname, '../../rust-kernel/tests/fixtures/m1-oracle.json'),
  'utf8'
)) as readonly M1OracleWitness[];
const m1Witnesses = process.env.ICHIRAN_E2E_M1_WITNESSES
  ? JSON.parse(process.env.ICHIRAN_E2E_M1_WITNESSES) as readonly M1OracleWitness[]
  : retainedM1Witnesses;

interface RustKernelMeasurement {
  readonly workerReadyMs: number;
  readonly openMs: number;
  readonly firstAnalyzeMs: number;
  readonly lexicalP50Ms: number;
  readonly lexicalP95Ms: number;
  readonly morphologyP50Ms: number;
  readonly morphologyP95Ms: number;
  readonly detailMs: number;
  readonly transientBytes: number;
  readonly wasmLinearMemoryBytes: number;
  readonly kernelPayloadBytes: number;
  readonly detailResidentBytesBefore: number;
  readonly detailResidentBytesAfter: number;
  readonly workerHeapBytes: number | null;
  readonly workerJsHeapUsedBytes: number;
  readonly workerEmbedderHeapUsedBytes: number;
  readonly workerBackingStorageBytes: number;
  readonly generatedScore: number;
  readonly invalidInputCode: string;
  readonly calibrationRatio: number;
}

test.skip(process.env.ICHIRAN_TYPESCRIPT_ORACLE === '1', 'requires the Rust kernel build');

test('Rust Worker owns the complete analyzer boundary', async ({ browser }) => {
  const context = await browser.newContext({
    baseURL: 'http://127.0.0.1:4173',
    serviceWorkers: 'allow'
  });
  watchConsoleHealth(context);
  try {
    const page = await context.newPage();
    await page.goto('/');
    await page.getByRole('button', { name: 'Install analyzer data' }).click();
    await expect(analyzerReady(page)).toBeVisible({ timeout: 180_000 });

    const input = page.getByRole('textbox', { name: 'Japanese text', exact: true });
    await input.fill('猫');
    await page.getByRole('button', { name: 'Analyze', exact: true }).click();
    const cat = page.getByRole('button', { name: /猫/ }).first();
    await expect(cat).toBeVisible();
    // A one-token result is selected automatically.
    await expect(page.locator('.word-details:visible').getByText('Dictionary forms')).toBeVisible();
    await page.getByRole('button', { name: 'Close', exact: true }).click();

    await input.fill('食べた');
    await page.getByRole('button', { name: 'Analyze', exact: true }).click();
    await expect(page.getByRole('button', { name: /食べた/ }).first()).toBeVisible();

    const manifest = await page.request
      .get('/analyzer/manifest.json')
      .then(response => response.json() as Promise<AnalyzerPackManifest>);
    const serviceWorker = await page.request.get('/sw.js').then(response => response.text());
    const workerPath = serviceWorker.match(/\/assets\/analyzer\.worker-[^"']+\.js/)?.[0];
    if (!workerPath) throw new Error('Rust analyzer Worker shell asset is missing');

    const affinityCpu = await singleCpuAffinity();
    const calibration = await attachAnalyzerWorker(browser);
    let hogs: readonly ChildProcess[] = [];
    let measurement: RustKernelMeasurement;
    try {
      await calibration.samples(2);
      const baseline = await calibration.samples(7);
      await calibration.collectGarbage();
      const heapUsage = await calibration.heapUsage();
      hogs = await startCpuHogs(affinityCpu, 5);
      await new Promise(resolve => setTimeout(resolve, 1_000));
      const contended = await calibration.samples(7);
      const calibrationRatio = median(contended.map(value => value.ms))
        / median(baseline.map(value => value.ms));
      expect(calibrationRatio).toBeGreaterThanOrEqual(5);
      expect(calibrationRatio).toBeLessThanOrEqual(7.5);

      const rpc = await page.evaluate(async ({
        workerPath,
        manifest,
        calibrationRatio,
        witnesses
      }) => {
        const worker = new Worker(workerPath, {
          type: 'module',
          name: 'ichiran-rust-kernel-measurement'
        });
        let nextId = 0;
        const request = <T>(value: Record<string, unknown>): Promise<T> => new Promise((resolve, reject) => {
          const id = ++nextId;
          const receive = (event: MessageEvent): void => {
            const response = event.data as {
              readonly id: number;
              readonly type: 'progress' | 'result' | 'error';
              readonly result?: unknown;
              readonly code?: string;
              readonly message?: string;
            };
            if (response.id !== id || response.type === 'progress') return;
            worker.removeEventListener('message', receive);
            if (response.type === 'error') {
              const error = new Error(response.message);
              Object.assign(error, { code: response.code });
              reject(error);
            }
            else resolve(response.result as T);
          };
          worker.addEventListener('message', receive);
          worker.postMessage({ id, ...value });
        });
      const percentile = (values: number[], ratio: number): number => {
        const sorted = [...values].sort((left, right) => left - right);
        return sorted[Math.max(0, Math.ceil(sorted.length * ratio) - 1)]!;
      };
      const timed = async (text: string): Promise<number> => {
        const started = performance.now();
        await request({ op: 'analyze', text, options: { limit: 1 } });
        return performance.now() - started;
      };
        try {
          const readyStarted = performance.now();
          await request({ op: 'expect-release', release: manifest });
          const workerReadyMs = performance.now() - readyStarted;
          const firstAnalyzeMs = await timed('猫');
          const generated = await request<{ readonly paths: readonly { readonly score: number }[] }>({
            op: 'analyze', text: '忘れた', options: { limit: 1 }
          });
          let invalidInputCode = '';
          try {
            await request({ op: 'analyze', text: '猫'.repeat(257), options: { limit: 1 } });
          } catch (error) {
            invalidInputCode = String((error as Error & { readonly code?: unknown }).code ?? '');
          }
          for (let pass = 0; pass < 10; pass++) {
            await request({ op: 'analyze', text: '猫', options: { limit: 1 } });
            await request({ op: 'analyze', text: '食べた', options: { limit: 1 } });
          }
          const lexical: number[] = [];
          const morphology: number[] = [];
          for (let pass = 0; pass < 100; pass++) {
            lexical.push(await timed('猫'));
            morphology.push(await timed('食べた'));
          }
          const before = await request<RustKernelMetrics>({ op: 'rust-kernel-metrics' });
          const detailStarted = performance.now();
          await request({ op: 'describe', entryIndex: 43_720 });
          const detailMs = performance.now() - detailStarted;
          const after = await request<RustKernelMetrics>({ op: 'rust-kernel-metrics' });
          const differential = [];
          for (const witness of witnesses) {
            // Construct from numeric UTF-16 units in the browser realm so lone
            // surrogates cross JS -> Worker -> WASM without source or transport
            // normalization before the analyzer receives them.
            const text = String.fromCharCode(...witness.codeUnits);
            const result = await request<{ readonly input: string }>({
              op: 'analyze', text, options: { limit: 1 }
            });
            const resultInputCodeUnits = Array.from(
              { length: result.input.length },
              (_, index) => result.input.charCodeAt(index)
            );
            differential.push({
              name: witness.name,
              requestCodeUnits: Array.from(
                { length: text.length },
                (_, index) => text.charCodeAt(index)
              ),
              resultInputCodeUnits,
              serialized: JSON.stringify(result)
            });
          }
          return {
            workerReadyMs,
            openMs: before.openMs,
            firstAnalyzeMs,
            lexicalP50Ms: percentile(lexical, 0.5),
            lexicalP95Ms: percentile(lexical, 0.95),
            morphologyP50Ms: percentile(morphology, 0.5),
            morphologyP95Ms: percentile(morphology, 0.95),
            detailMs,
            transientBytes: before.transientBytes,
            wasmLinearMemoryBytes: after.wasmLinearMemoryBytes,
            kernelPayloadBytes: after.kernelPayloadBytes,
            detailResidentBytesBefore: before.detailResidentBytes,
            detailResidentBytesAfter: after.detailResidentBytes,
            workerHeapBytes: after.workerHeapBytes,
            generatedScore: generated.paths[0]!.score,
            invalidInputCode,
            calibrationRatio,
            differential
          };
        } finally {
          worker.terminate();
        }
      }, { workerPath, manifest, calibrationRatio, witnesses: m1Witnesses });
      for (const witness of m1Witnesses) {
        const observed = rpc.differential.find(result => result.name === witness.name);
        expect(observed, `missing ${witness.name} Worker differential result`).toBeDefined();
        expect(observed!.requestCodeUnits).toEqual(Array.from(witness.codeUnits));
        expect(observed!.resultInputCodeUnits).toEqual(Array.from(witness.codeUnits));
        expect(observed!.serialized, `${witness.name} full DTO mismatch`)
          .toBe(witness.serialized);
      }
      expect(rpc.invalidInputCode).toBe('invalid-input');
      const { differential: _, ...runtimeMeasurement } = rpc;
      measurement = {
        ...runtimeMeasurement,
        workerJsHeapUsedBytes: heapUsage.usedSize,
        workerEmbedderHeapUsedBytes: heapUsage.embedderHeapUsedSize,
        workerBackingStorageBytes: heapUsage.backingStorageSize
      };
    } finally {
      await stopCpuHogs(hogs);
      await calibration.close();
    }
    console.log(`RUST_KERNEL_MEASUREMENT=${JSON.stringify(measurement)}`);
    expect(measurement.workerReadyMs).toBeLessThan(1_050);
    expect(measurement.openMs).toBeLessThan(1_050);
    expect(measurement.firstAnalyzeMs).toBeLessThan(53.1);
    expect(measurement.lexicalP95Ms).toBeLessThan(33.8);
    expect(measurement.morphologyP95Ms).toBeLessThan(33.8);
    expect(measurement.detailMs).toBeLessThan(65.7);
    expect(measurement.transientBytes).toBeLessThanOrEqual(128 * 1024 * 1024);
    expect(measurement.wasmLinearMemoryBytes).toBeLessThanOrEqual(96 * 1024 * 1024);
    expect(measurement.detailResidentBytesBefore).toBe(1_755_112);
    expect(measurement.detailResidentBytesAfter).toBeGreaterThan(1_755_112);
    expect(measurement.generatedScore).toBe(216);
  } finally {
    await context.close();
  }
});
