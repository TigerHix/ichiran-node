import { describe, expect, test } from 'bun:test';
import { createPresentation, isPresentationLocale } from '../src/index.js';

describe('presentation catalogs', () => {
  test('formats the same semantic result independently in each UI locale', () => {
    const en = createPresentation('en');
    const zh = createPresentation('zh-Hans');
    expect(en.conjugationLabel(2)).toBe('Past (~ta)');
    expect(zh.conjugationLabel(2)).toBe('过去式（～た）');
    expect(en.suffixLabel('iru')).toContain('continuing action');
    expect(zh.suffixLabel('iru')).toContain('动作持续');
    expect(zh.partOfSpeechLabel('v1')).toBe('一段动词（-ru）');
    expect(zh.fieldLabel('comp')).toBe('计算机');
  });

  test('preserves unknown source codes without inventing English labels', () => {
    const zh = createPresentation('zh-Hans');
    expect(zh.partOfSpeechLabel('future-pos')).toBe('future-pos');
    expect(zh.fieldLabel('future-field')).toBe('future-field');
    expect(zh.conjugationLabel(999)).toBe('活用 999');
  });

  test('recognizes only shipped presentation locales', () => {
    expect(isPresentationLocale('en')).toBe(true);
    expect(isPresentationLocale('zh-Hans')).toBe(true);
    expect(isPresentationLocale('zh')).toBe(false);
  });
});
