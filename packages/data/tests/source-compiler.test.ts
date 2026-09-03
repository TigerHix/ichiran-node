import { describe, expect, test } from 'bun:test';
import { mkdtemp, rm, writeFile } from 'node:fs/promises';
import { tmpdir } from 'node:os';
import { join } from 'node:path';
import { fileURLToPath } from 'node:url';
import { encodePack, openPack } from '@ichiran/core/compiler';
import { buildDetailStore } from '../src/browser-pack/details.js';
import { buildRootPayload } from '../src/browser-pack/root-payload.js';
import { deriveBestReadings } from '../src/source-compiler/best-readings.js';
import {
  parseJmdictEntry,
  streamJmdictXml
} from '../src/source-compiler/jmdict.js';
import {
  compileEasyHint,
  loadKanjidicHintReadings
} from '../src/source-compiler/kanjidic-hints.js';
import { CanonicalLexicon } from '../src/source-compiler/lexicon.js';
import type { CanonicalEntry } from '../src/source-compiler/model.js';
import {
  canonicalDetailEntries,
  canonicalRootPayloadSource
} from '../src/source-compiler/pack-input.js';
import {
  canonicalSurfaceIndexRows,
  encodeSurfaceIndexTsv
} from '../src/source-compiler/surface-index-input.js';

const XML = `<entry>
<ent_seq>1234560</ent_seq>
<k_ele><keb>書く</keb><ke_pri>news1</ke_pri></k_ele>
<k_ele><keb>描く</keb></k_ele>
<r_ele><reb>かく</reb><re_pri>ichi1</re_pri><re_pri>nf07</re_pri><re_restr>書く</re_restr></r_ele>
<r_ele><reb>えがく</reb><re_restr>描く</re_restr></r_ele>
<r_ele><reb>カク</reb><re_nokanji/></r_ele>
<r_ele><reb>かき</reb><re_inf>&ok;</re_inf></r_ele>
<sense><pos>&v5k;</pos><misc>&uk;</misc><gloss>to write</gloss></sense>
<sense><stagk>描く</stagk><pos>&v5k;</pos><misc>&arch;</misc><gloss>to draw</gloss></sense>
</entry>`;

function parsed(): CanonicalEntry {
  return parseJmdictEntry(XML, 'test-jmdict', 7);
}

describe('source-native JMdict projection', () => {
  test('projects the compiler-owned semantic entry without database shapes', () => {
    const entry = parsed();
    expect(entry.seq).toBe(1234560);
    expect(entry.source).toEqual({ sourceId: 'test-jmdict', ordinal: 7 });
    expect(entry.kanji.map(value => value.text)).toEqual(['書く', '描く']);
    expect(entry.kana.map(value => value.text)).toEqual(['かく', 'えがく', 'カク']);
    expect(entry.kana[0]?.common).toBe(7);
    expect(entry.kana[2]?.noKanji).toBe(true);
    expect(entry.primaryNoKanji).toBe(true);
    expect(entry.restrictions).toEqual([
      { reading: 'かく', written: '書く', ordinal: 0 },
      { reading: 'えがく', written: '描く', ordinal: 1 }
    ]);
    expect(entry.senses[0]?.properties.map(value => [value.tag, value.text])).toEqual([
      ['pos', 'v5k'],
      ['misc', 'uk']
    ]);
  });

  test('preserves UTF-8 code points split across input chunks', async () => {
    const directory = await mkdtemp(join(tmpdir(), 'ichiran-jmdict-stream-'));
    const path = join(directory, 'JMdict_e');
    const beforeText = '<entry><ent_seq>1</ent_seq><r_ele><reb>';
    const padding = ' '.repeat(65_535 - Buffer.byteLength(beforeText));
    await writeFile(path, `${padding}${beforeText}た</reb></r_ele><sense><gloss>x</gloss></sense></entry>`);
    try {
      const entries: string[] = [];
      for await (const xml of streamJmdictXml(path)) entries.push(xml);
      expect(parseJmdictEntry(entries[0]!, 'split', 0).kana[0]?.text).toBe('た');
    } finally {
      await rm(directory, { recursive: true, force: true });
    }
  });

  test('derives restriction-aware best readings', () => {
    const entry = deriveBestReadings(parsed());
    expect(entry.kanji.map(value => value.best)).toEqual(['かく', 'えがく']);
    expect(entry.kana.map(value => value.best)).toEqual(['書く', '描く', null]);
  });

  test('applies custom roots before chronological root demotion', async () => {
    const path = fileURLToPath(new URL('../../../data/sources/extra.xml', import.meta.url));
    let custom: CanonicalEntry | null = null;
    let sourceOrdinal = 0;
    for await (const xml of streamJmdictXml(path)) {
      if (xml.includes('<ent_seq>900000</ent_seq>')) {
        custom = parseJmdictEntry(xml, 'ichiran-custom-extra-260118', sourceOrdinal, 214_699 + sourceOrdinal);
        break;
      }
      sourceOrdinal++;
    }
    expect(custom?.kana[0]?.text).toBe('たそう');
    if (!custom) throw new Error('Pinned custom root 900000 is missing');

    const removed = parseJmdictEntry(
      '<entry><ent_seq>2611370</ent_seq><k_ele><keb>為り</keb></k_ele><r_ele><reb>なり</reb></r_ele><sense><pos>n</pos><gloss>being</gloss></sense></entry>',
      'edrdg-jmdict-e-2026-01-01',
      214_000
    );
    const lexicon = new CanonicalLexicon([removed]);
    lexicon.add(custom);
    expect(lexicon.demoteRoot(2611370).kanji[0]?.text).toBe('為り');
    expect(lexicon.entries().map(entry => entry.seq)).toEqual([900000]);
  });
});

test('qualified pack writers accept compiler-owned semantic input', () => {
  const entry = deriveBestReadings(parsed());
  const root = buildRootPayload(canonicalRootPayloadSource([entry]));
  const details = buildDetailStore(canonicalDetailEntries([entry]));
  const pack = encodePack([{ id: 2, bytes: root.bytes }]);
  const reader = openPack(pack);

  expect(reader.getSection(2)).toEqual(root.bytes);
  expect(root.stats.counts.entries).toBe(1);
  expect(details.stats.entryCount).toBe(1);
  expect(buildRootPayload(canonicalRootPayloadSource([entry])).bytes).toEqual(root.bytes);
  expect(buildDetailStore(canonicalDetailEntries([entry])).bytes).toEqual(details.bytes);
});

test('surface-index input is source-owned and UTF-8 ordered', () => {
  const rows = canonicalSurfaceIndexRows([parsed()], [
    { route: 'kanji', surface: '書いた' },
    { route: 'kana', surface: 'かいた' }
  ]);
  expect(rows.find(value => value.surface === '書く')).toMatchObject({
    kanjiDirect: true,
    kanjiMorphology: false
  });
  expect(rows.find(value => value.surface === 'かいた')).toMatchObject({
    kanaDirect: false,
    kanaMorphology: true
  });
  expect(new TextDecoder().decode(encodeSurfaceIndexTsv(rows))).toEndWith('\n');
});

test('Kanjidic compiler input reproduces an analyzer easy hint', async () => {
  const path = fileURLToPath(new URL('../kanjidic2.xml.gz', import.meta.url));
  const readings = await loadKanjidicHintReadings(path, [{
    literal: '楊',
    reading: 'かわ',
    type: 'ja_kun',
    prefix: false,
    suffix: false
  }]);

  expect(compileEasyHint(
    readings,
    '時 は 金 なり',
    '時は金なり',
    'ときはかねなり'
  )).toBe('とき\u200b\u200cは\u200bかね\u200bなり');
});
