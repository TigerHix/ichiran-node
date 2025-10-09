import { performance } from 'node:perf_hooks';

export const GRAMMAR_PROFILE = process.env.GRAMMAR_PROFILE === '1' || process.env.GRAMMAR_PROFILE === 'true';

export function time<T>(label: string, fn: () => T): T {
  if (!GRAMMAR_PROFILE) return fn();
  const start = performance.now();
  try {
    return fn();
  } finally {
    const ms = performance.now() - start;
    console.log(`[GRAMMAR_PROFILE] ${label}: ${ms.toFixed(2)}ms`);
  }
}

export async function timeAsync<T>(label: string, fn: () => Promise<T>): Promise<T> {
  if (!GRAMMAR_PROFILE) return fn();
  const start = performance.now();
  try {
    return await fn();
  } finally {
    const ms = performance.now() - start;
    console.log(`[GRAMMAR_PROFILE] ${label}: ${ms.toFixed(2)}ms`);
  }
}


