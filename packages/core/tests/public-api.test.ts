import { describe, expect, test } from 'bun:test';

import * as core from '../src/index.js';
import { romanizeWord as compilerRomanizeWord } from '../src/compiler.js';
import { TypeScriptOracleRuntime } from '../src/qualification.js';

describe('core public entry points', () => {
  test('keeps TypeScript analyzer execution on the qualification-only entry point', () => {
    for (const name of [
      'TypeScriptOracleRuntime',
      'PortableAnalyzer',
      'findAnalyzerPaths',
      'scoreAnalyzerCandidate',
      'filterAndCullAnalyzerSegments',
      'materializeAnalyzerCounter',
      'serializePortableLegacyCompact',
      'serializePortableLegacyDetailed',
      'numberToKanji',
      'parseNumber',
      'romanizeWord'
    ]) {
      expect(name in core).toBe(false);
    }
    expect(typeof compilerRomanizeWord).toBe('function');
    expect(typeof TypeScriptOracleRuntime.open).toBe('function');
  });
});
