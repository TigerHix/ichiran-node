import { describe, expect, test } from 'bun:test';
import { renderToStaticMarkup } from 'react-dom/server';
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
    suffix: null,
    counter: null,
    entity: false
  }],
  suffix: null,
  counter: null,
  entity: false
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
});
