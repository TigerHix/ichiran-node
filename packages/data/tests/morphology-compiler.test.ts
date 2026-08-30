import { fileURLToPath } from 'node:url';
import { describe, expect, test } from 'bun:test';
import { openMorphology } from '../../core/src/morphology.js';
import { compileMorphology } from '../src/browser-pack/morphology-compiler.js';

const roots = [
  {
    seq: 2257550, pos: 'adj-i', route: 'kana', text: 'ない', ord: 0,
    common: null, counterpart: null
  },
  {
    seq: 2684620, pos: 'adj-i', route: 'kana', text: 'しい', ord: 0,
    common: null, counterpart: null
  },
  {
    seq: 1593170, pos: 'v1', route: 'kana', text: 'コケる', ord: 1,
    common: null, counterpart: '転ける'
  }
] as const;

const rootForms = [
  { seq: 1593170, text: 'コケる' },
  { seq: 1593170, text: '転ける' },
  { seq: 2257550, text: 'ない' },
  { seq: 2684620, text: 'しい' }
] as const;

function fakeSql(strings: TemplateStringsArray): Promise<readonly unknown[]> {
  const query = strings.join('');
  if (query.includes('WITH root_pos AS')) return Promise.resolve(roots);
  if (query.includes('WITH roots AS')) return Promise.resolve(rootForms);
  if (query.includes('WITH selected AS')) return Promise.resolve([]);
  throw new Error(`Unexpected morphology compiler query: ${query.slice(0, 80)}`);
}

describe('morphology compiler tombstones', () => {
  test('suppresses both non-materialized コケさせ rule paths', async () => {
    const build = await compileMorphology({
      sql: fakeSql as never,
      dataPath: fileURLToPath(new URL('../../../data/', import.meta.url))
    });
    const paths = build.artifact.tombstones
      .filter(tombstone => tombstone.rootSeq === 1593170 && tombstone.surface === 'コケさせ')
      .map(tombstone => {
        const first = build.artifact.rules[tombstone.firstRule]!;
        const second = build.artifact.rules[tombstone.secondRule!]!;
        return [first.pos, first.type, second.pos, second.type];
      });

    expect(paths).toEqual([
      ['v1', 53, 'v5s', 10],
      ['v1', 7, 'v1', 13]
    ]);
    expect(openMorphology(build.bytes).lookup('コケさせ', 'kana')).toEqual([]);
  });
});
