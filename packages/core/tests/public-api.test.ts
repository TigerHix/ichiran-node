import { describe, expect, test } from 'bun:test';

import * as core from '../src/index.js';
import { TypeScriptOracleRuntime } from '../src/qualification.js';

describe('core public entry points', () => {
  test('keeps the TypeScript oracle on the qualification-only entry point', () => {
    expect('TypeScriptOracleRuntime' in core).toBe(false);
    expect(typeof TypeScriptOracleRuntime.open).toBe('function');
  });
});
