// Number Split tests - ported from tests.lisp
import { describe, test, expect } from 'bun:test';
import { basicSplit } from '../src/characters.js';

describe('Number Split Tests', () => {
  test('二〇二〇 should be treated as single word', () => {
    // Line 639-640 in tests.lisp:
    // (assert-equal (basic-split "二〇二〇") '((:word . "二〇二〇")))
    const result = basicSplit("二〇二〇");
    expect(result).toEqual([{ type: 'word', text: "二〇二〇" }]);
  });
});