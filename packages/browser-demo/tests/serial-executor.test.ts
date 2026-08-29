import { describe, expect, test } from 'bun:test';
import { createSerialExecutor } from '../src/worker/serial-executor.js';

describe('Worker request serialization', () => {
  test('runs requests in arrival order without overlap', async () => {
    const runSerially = createSerialExecutor();
    const events: string[] = [];
    let active = 0;
    let maximumActive = 0;
    let releaseFirst = (): void => {};
    const firstGate = new Promise<void>(resolve => {
      releaseFirst = resolve;
    });

    const first = runSerially(async () => {
      active++;
      maximumActive = Math.max(maximumActive, active);
      events.push('first:start');
      await firstGate;
      events.push('first:end');
      active--;
      return 1;
    });
    const second = runSerially(async () => {
      active++;
      maximumActive = Math.max(maximumActive, active);
      events.push('second:start');
      active--;
      return 2;
    });

    await Promise.resolve();
    expect(events).toEqual(['first:start']);
    releaseFirst();
    expect(await Promise.all([first, second])).toEqual([1, 2]);
    expect(events).toEqual(['first:start', 'first:end', 'second:start']);
    expect(maximumActive).toBe(1);
  });

  test('continues after a rejected request', async () => {
    const runSerially = createSerialExecutor();
    const failed = runSerially(() => {
      throw new Error('expected failure');
    });
    const recovered = runSerially(() => 'recovered');

    await expect(failed).rejects.toThrow('expected failure');
    await expect(recovered).resolves.toBe('recovered');
  });
});
