import { describe, expect, test } from 'bun:test';
import { createPresentation } from '@ichiran/presentation';
import type { AnalysisPath, AnalysisToken } from '../src/analyzer-service.js';
import { analysisPathChoices } from '../src/analysis-path-choices.js';

const presentation = createPresentation('en');

function token(
  text: string,
  reading: string,
  pos: readonly string[],
  entity = false
): AnalysisToken {
  return { text, reading, pos, entity } as AnalysisToken;
}

function path(...tokens: readonly AnalysisToken[]): AnalysisPath {
  return { score: 0, tokens };
}

describe('alternative analysis path choices', () => {
  test('coalesces paths with the same consumer-visible token sequence', () => {
    const paths = [
      path(token('日本語', 'にほんご', ['n'])),
      path(token('日本語', 'にほんご', ['n'])),
      path(token('日本', 'にほん', ['n']), token('語', 'ご', ['n']))
    ];

    expect(analysisPathChoices(paths, 0, presentation)).toEqual([
      { index: 0, label: '日本語' },
      { index: 2, label: '日本 / 語' }
    ]);
  });

  test('keeps the selected original index when its duplicate is coalesced', () => {
    const paths = [
      path(token('日本語', 'にほんご', ['n'])),
      path(token('日本語', 'にほんご', ['n'])),
      path(token('日本語', 'にっぽんご', ['n']))
    ];

    expect(analysisPathChoices(paths, 1, presentation)).toEqual([
      { index: 1, label: '日本語（にほんご）' },
      { index: 2, label: '日本語（にっぽんご）' }
    ]);
  });

  test('adds only the reading, POS, or entity distinction needed by colliding labels', () => {
    const choices = analysisPathChoices([
      path(token('はし', 'はし', ['n'])),
      path(token('はし', 'はし', ['n-pr'], true)),
      path(token('はし', 'ばし', ['n']))
    ], 0, presentation);

    expect(choices).toEqual([
      { index: 0, label: 'はし（はし · Noun）' },
      { index: 1, label: 'はし（はし · Proper noun）' },
      { index: 2, label: 'はし（ばし）' }
    ]);
  });
});
