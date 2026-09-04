import { describe, expect, test } from 'bun:test';
import { renderToStaticMarkup } from 'react-dom/server';
import { createPresentation } from '@ichiran/presentation';
import type { AnalysisToken, TokenDetails } from '../src/analyzer-service.js';
import { WordDetails } from '../src/WordDetails.js';

const token = {
  text: '食べました',
  reading: 'たべました'
} as AnalysisToken;

const details: TokenDetails = {
  text: '食べました',
  reading: 'たべました',
  meanings: [],
  components: [],
  conjugations: [{
    root: { text: '食べる', reading: 'たべる' },
    properties: [{ pos: 'vt', type: 2, negative: false, formal: true }],
    meanings: [{
      gloss: 'to eat',
      pos: ['v1', 'vt'],
      fields: [],
      info: null
    }],
    via: []
  }],
  alternatives: [{
    text: '食べました',
    reading: 'たべました',
    meanings: [{ gloss: 'alternative reading', pos: ['exp'], fields: [], info: null }],
    components: [],
    conjugations: [],
    alternatives: [],
    suffixId: null,
    counter: null,
    entityKind: null
  }],
  suffixId: null,
  counter: null,
  entityKind: null
};

describe('canonical word-detail rendering', () => {
  test('renders kernel-selected meanings once and maps opaque tags to learner labels', () => {
    const markup = renderToStaticMarkup(
      <WordDetails
        token={token}
        selectionText="食べました"
        details={details}
        loading={false}
        error={null}
        copied={false}
        presentation={createPresentation('en')}
        onCopy={() => undefined}
        onClose={() => undefined}
      />
    );
    expect(markup.match(/to eat/g)).toHaveLength(1);
    expect(markup.match(/alternative reading/g)).toHaveLength(1);
    expect(markup).toContain('Transitive Verb');
    expect(markup).toContain('Past (~ta)');
    expect(markup).toContain('Formal');
    expect(markup).not.toContain('vt</span>');
  });

  test('renders analyzer terminology from zh-Hans without changing dictionary meanings', () => {
    const markup = renderToStaticMarkup(
      <WordDetails
        token={token}
        selectionText="食べました"
        details={{ ...details, suffixId: 'iru', entityKind: 'proper-noun' }}
        loading={false}
        error={null}
        copied={false}
        presentation={createPresentation('zh-Hans')}
        onCopy={() => undefined}
        onClose={() => undefined}
      />
    );
    expect(markup).toContain('及物动词');
    expect(markup).toContain('过去式（～た）');
    expect(markup).toContain('动作持续');
    expect(markup).toContain('专有名词');
    expect(markup).toContain('to eat');
  });

  test('shares a POS heading across each consecutive run while preserving sense order', () => {
    const groupedDetails: TokenDetails = {
      ...details,
      conjugations: [],
      alternatives: [],
      meanings: [
        { gloss: 'to find', pos: ['v1', 'vt'], fields: [], info: null },
        { gloss: 'to be familiar with', pos: ['vt', 'v1'], fields: [], info: null },
        { gloss: 'domestic', pos: ['adj-i'], fields: [], info: null },
        { gloss: 'to identify', pos: ['v1', 'vt'], fields: [], info: null }
      ]
    };
    const markup = renderToStaticMarkup(
      <WordDetails
        token={token}
        selectionText="家"
        details={groupedDetails}
        loading={false}
        error={null}
        copied={false}
        presentation={createPresentation('en')}
        onCopy={() => undefined}
        onClose={() => undefined}
      />
    );

    expect(markup.match(/>Ichidan Verb \(-ru\)</g)).toHaveLength(2);
    expect(markup.match(/>Transitive Verb</g)).toHaveLength(2);
    expect(markup.match(/>I-Adjective</g)).toHaveLength(1);
    expect(markup.indexOf('to find')).toBeLessThan(markup.indexOf('to be familiar with'));
    expect(markup.indexOf('to be familiar with')).toBeLessThan(markup.indexOf('domestic'));
    expect(markup.indexOf('domestic')).toBeLessThan(markup.indexOf('to identify'));
  });

  test('combines sibling conjugation interpretations and factors shared modifiers', () => {
    const ambiguousDetails: TokenDetails = {
      ...details,
      conjugations: [{
        root: { text: '見つける', reading: 'みつける' },
        properties: [
          { pos: 'v1', type: 5, negative: true, formal: false },
          { pos: 'v1', type: 6, negative: true, formal: false }
        ],
        meanings: [
          { gloss: 'to find', pos: ['v1', 'vt'], fields: [], info: null },
          { gloss: 'to be familiar with', pos: ['vt', 'v1'], fields: [], info: null }
        ],
        via: []
      }]
    };
    const markup = renderToStaticMarkup(
      <WordDetails
        token={token}
        selectionText="見つけられない"
        details={ambiguousDetails}
        loading={false}
        error={null}
        copied={false}
        presentation={createPresentation('en')}
        onCopy={() => undefined}
        onClose={() => undefined}
      />
    );

    expect(markup).toContain('Potential / Passive');
    expect(markup.match(/>Negative</g)).toHaveLength(1);
    expect(markup.match(/>Ichidan Verb \(-ru\)</g)).toHaveLength(1);
    expect(markup.match(/>Transitive Verb</g)).toHaveLength(1);
  });
});
