import { describe, expect, test } from 'bun:test';
import {
  buildRootPayload,
  compareRootPayloadText,
  type RootPayloadSource
} from '../../data/src/browser-pack/root-payload.js';
import {
  openRootPayload,
  RootPayloadFormatError,
  ROOT_PAYLOAD_FORMAT_VERSION,
  ROOT_PAYLOAD_HEADER_BYTES
} from '../src/root-payload.js';

const fixture: RootPayloadSource = {
  entries: [
    {
      seq: 300,
      nKanji: 1,
      nKana: 1,
      primaryNokanji: false,
      archived: true,
      preferKana: false,
      preferKanaOnOrdinalZero: false,
      pos: ['n']
    },
    {
      seq: 100,
      nKanji: 1,
      nKana: 1,
      primaryNokanji: false,
      archived: false,
      preferKana: true,
      preferKanaOnOrdinalZero: true,
      pos: ['vt', 'v1', 'v1']
    },
    {
      seq: 200,
      nKanji: 1,
      nKana: 1,
      primaryNokanji: true,
      archived: false,
      preferKana: true,
      preferKanaOnOrdinalZero: false,
      pos: []
    }
  ],
  forms: [
    {
      surface: 'かな', route: 'kana', seq: 200, ord: 0, common: 5,
      commonTags: 'ichi1', conjugatable: false, nokanji: true, best: '句'
    },
    {
      surface: '食べる', route: 'kanji', seq: 100, ord: 0, common: 0,
      commonTags: 'news1,ichi1', conjugatable: true, nokanji: false, best: 'たべる'
    },
    {
      surface: 'ロイス・ディーツ症候群', route: 'kanji', seq: 300, ord: 0, common: null,
      commonTags: '', conjugatable: false, nokanji: false,
      best: 'ロイス・ディーツしょうこうぐん'
    },
    {
      surface: 'たべる', route: 'kana', seq: 100, ord: 0, common: 2,
      commonTags: 'ichi1', conjugatable: true, nokanji: false, best: '食べる'
    },
    {
      surface: '句', route: 'kanji', seq: 200, ord: 1, common: null,
      commonTags: '', conjugatable: false, nokanji: false, best: 'かな、かな'
    },
    {
      surface: 'かな', route: 'kana', seq: 300, ord: 1, common: null,
      commonTags: '', conjugatable: false, nokanji: false, best: null
    }
  ],
  restrictions: [
    { seq: 300, reading: 'ロイス・ディーツしょうこうぐん', written: 'ロイス・ディーツ症候群' },
    { seq: 200, reading: 'かな', written: '句' }
  ]
};

function expectRootError(action: () => unknown, code: RootPayloadFormatError['code']): void {
  try {
    action();
    throw new Error(`Expected RootPayloadFormatError ${code}`);
  } catch (error) {
    expect(error).toBeInstanceOf(RootPayloadFormatError);
    expect((error as RootPayloadFormatError).code).toBe(code);
  }
}

describe('packed root payload', () => {
  test('round-trips every scoring-critical fixture field without row expansion', () => {
    const first = buildRootPayload(fixture);
    const second = buildRootPayload({
      entries: [...fixture.entries].reverse(),
      forms: [...fixture.forms].reverse(),
      restrictions: [...fixture.restrictions].reverse()
    });
    expect(second.bytes).toEqual(first.bytes);

    const reader = openRootPayload(first.bytes);
    const surfaces = [...new Set(fixture.forms.map((form) => form.surface))]
      .sort(compareRootPayloadText);

    expect(reader.surfaceCount).toBe(surfaces.length);
    expect(reader.formCount).toBe(fixture.forms.length);
    expect(reader.entryCount).toBe(fixture.entries.length);
    expect(reader.restrictionCount).toBe(fixture.restrictions.length);
    expect(first.stats.counts.pooledSurfaceExceptions).toBe(2);

    const expectedEntries = [...fixture.entries].sort((left, right) => left.seq - right.seq);
    for (let entry = 0; entry < expectedEntries.length; entry++) {
      const expected = expectedEntries[entry]!;
      expect(reader.entrySeq(entry)).toBe(expected.seq);
      expect(reader.findEntryIndex(expected.seq)).toBe(entry);
      expect(reader.entryNKanji(entry)).toBe(expected.nKanji);
      expect(reader.entryNKana(entry)).toBe(expected.nKana);
      expect(reader.entryPrimaryNokanji(entry)).toBe(expected.primaryNokanji);
      expect(reader.entryArchived(entry)).toBe(expected.archived);
      expect(reader.entryPreferKana(entry)).toBe(expected.preferKana);
      expect(reader.entryPreferKanaOnOrdinalZero(entry)).toBe(expected.preferKanaOnOrdinalZero);

      const actualPos: string[] = [];
      for (let position = 0; position < reader.entryPosCount(entry); position++) {
        actualPos.push(reader.string(reader.entryPosStringIdAt(entry, position)));
      }
      expect(actualPos).toEqual([...new Set(expected.pos)].sort(compareRootPayloadText));
    }
    expect(reader.findEntryIndex(999)).toBe(-1);

    for (let rank = 0; rank < surfaces.length; rank++) {
      const expectedForms = fixture.forms
        .filter((form) => form.surface === surfaces[rank])
        .sort((left, right) => left.seq - right.seq || left.ord - right.ord);
      const start = reader.surfaceFormStart(rank);
      expect(reader.surfaceFormCount(rank)).toBe(expectedForms.length);

      for (let offset = 0; offset < expectedForms.length; offset++) {
        const form = start + offset;
        const expected = expectedForms[offset]!;
        expect(reader.entrySeq(reader.formEntryIndex(form))).toBe(expected.seq);
        expect(reader.formOrdinal(form)).toBe(expected.ord);
        expect(reader.formCommon(form)).toBe(expected.common);
        expect(reader.string(reader.formCommonTagStringId(form))).toBe(expected.commonTags);
        expect(reader.formRoute(form)).toBe(expected.route);
        expect(reader.formConjugatable(form)).toBe(expected.conjugatable);
        expect(reader.formNokanji(form)).toBe(expected.nokanji);
        expect(
          reader.resolveSurfaceReference(
            reader.formBestReference(form),
            (surfaceRank) => surfaces[surfaceRank]!
          )
        ).toBe(expected.best);
      }
    }

    const expectedRestrictions = [...fixture.restrictions].sort((left, right) =>
      left.seq - right.seq
      || compareRootPayloadText(left.reading, right.reading)
      || compareRootPayloadText(left.written, right.written)
    );
    for (let restriction = 0; restriction < expectedRestrictions.length; restriction++) {
      const expected = expectedRestrictions[restriction]!;
      expect(reader.entrySeq(reader.restrictionEntryIndex(restriction))).toBe(expected.seq);
      expect(
        reader.resolveSurfaceReference(
          reader.restrictionReadingReference(restriction),
          (surfaceRank) => surfaces[surfaceRank]!
        )
      ).toBe(expected.reading);
      expect(
        reader.resolveSurfaceReference(
          reader.restrictionWrittenReference(restriction),
          (surfaceRank) => surfaces[surfaceRank]!
        )
      ).toBe(expected.written);
    }

    const seq200 = reader.findEntryIndex(200);
    expect([
      reader.restrictionStart(seq200),
      reader.restrictionEnd(seq200)
    ]).toEqual([0, 1]);
  });

  test('reads a non-zero-offset view and rejects header and payload corruption', () => {
    const encoded = buildRootPayload(fixture).bytes;
    const wrapped = new Uint8Array(encoded.byteLength + 6);
    wrapped.set(encoded, 3);
    expect(openRootPayload(wrapped.subarray(3, 3 + encoded.byteLength)).entryCount).toBe(3);

    const badMagic = encoded.slice();
    badMagic[0] ^= 0xff;
    expectRootError(() => openRootPayload(badMagic), 'invalid-header');

    const badVersion = encoded.slice();
    new DataView(badVersion.buffer).setUint16(8, ROOT_PAYLOAD_FORMAT_VERSION + 1, true);
    expectRootError(() => openRootPayload(badVersion), 'unsupported-version');

    const badHeader = encoded.slice();
    new DataView(badHeader.buffer).setUint32(32, 999, true);
    expectRootError(() => openRootPayload(badHeader), 'invalid-header');

    const intact = openRootPayload(encoded);
    const badPayload = encoded.slice();
    badPayload[intact.layout.formsOffset] ^= 1;
    expectRootError(() => openRootPayload(badPayload), 'corrupt-payload');

    expectRootError(
      () => openRootPayload(encoded.subarray(0, ROOT_PAYLOAD_HEADER_BYTES - 1)),
      'invalid-header'
    );
    expectRootError(
      () => openRootPayload(encoded.subarray(0, encoded.byteLength - 1)),
      'invalid-header'
    );
  });

  test('preserves the dense semantic direct order supplied by the projection', () => {
    const forms = fixture.forms.map((form) => ({
      ...form,
      lookupOrder: form.surface === 'かな' ? (form.seq === 300 ? 0 : 1) : 0
    }));
    const first = buildRootPayload({ ...fixture, forms });
    const second = buildRootPayload({ ...fixture, forms: [...forms].reverse() });
    expect(second.bytes).toEqual(first.bytes);

    const reader = openRootPayload(first.bytes);
    const surfaces = [...new Set(forms.map(form => form.surface))].sort(compareRootPayloadText);
    const rank = surfaces.indexOf('かな');
    const start = reader.surfaceFormStart(rank);
    expect(Array.from({ length: reader.surfaceFormCount(rank) }, (_, offset) =>
      reader.entrySeq(reader.formEntryIndex(start + offset))
    )).toEqual([300, 200]);

    expect(() => buildRootPayload({
      ...fixture,
      forms: forms.map((form, index) => index === 0
        ? { ...form, lookupOrder: undefined }
        : form)
    })).toThrow('mix physical and synthetic lookup order');

    expect(() => buildRootPayload({
      ...fixture,
      forms: forms.map((form) => form.surface === 'かな' && form.seq === 300
        ? { ...form, lookupOrder: 2 }
        : form)
    })).toThrow('is not dense');
  });

  test('locks the direct reading order witnesses for 心, 的, 音, and 風', () => {
    const witnesses = [
      ['心', [[1360480, 'こころ'], [1595125, 'しん']]],
      ['的', [[1437260, 'てき'], [1437270, 'まと']]],
      ['音', [[2859161, 'おん'], [2859162, 'ね'], [1576900, 'おと']]],
      ['風', [[1499730, 'ふう'], [1499720, 'かぜ'], [1361130, 'ふり']]]
    ] as const;
    const entries = witnesses.flatMap(([, values]) => values.map(([seq]) => ({
      seq,
      nKanji: 1,
      nKana: 1,
      primaryNokanji: false,
      archived: false,
      preferKana: false,
      preferKanaOnOrdinalZero: false,
      pos: ['n']
    })));
    const forms = witnesses.flatMap(([surface, values]) => values.map(([seq, reading], lookupOrder) => ({
      surface,
      route: 'kanji' as const,
      seq,
      lookupOrder,
      ord: 0,
      common: null,
      commonTags: '',
      conjugatable: false,
      nokanji: false,
      best: reading
    })));
    const build = buildRootPayload({ entries, forms, restrictions: [] });
    const reader = openRootPayload(build.bytes);
    const surfaces = witnesses.map(([surface]) => surface).sort(compareRootPayloadText);

    for (const [surface, expected] of witnesses) {
      const rank = surfaces.indexOf(surface);
      const start = reader.surfaceFormStart(rank);
      expect(Array.from({ length: reader.surfaceFormCount(rank) }, (_, offset) => {
        const form = start + offset;
        return [
          reader.entrySeq(reader.formEntryIndex(form)),
          reader.resolveSurfaceReference(
            reader.formBestReference(form),
            (surfaceRank) => surfaces[surfaceRank]!
          )
        ];
      })).toEqual(expected);
    }

    expect(build.stats.directOrderProjection).toMatchObject({ rows: 10, surfaces: 4 });
  });

  test('rejects inactive-route forms and unrepresentable packed values', () => {
    const wrongRoute: RootPayloadSource = {
      ...fixture,
      forms: [{ ...fixture.forms[0]!, route: 'kanji' }]
    };
    expect(() => buildRootPayload(wrongRoute)).toThrow('Inactive-route form');

    const badCommon: RootPayloadSource = {
      ...fixture,
      forms: [{ ...fixture.forms[0]!, common: 63 }]
    };
    expect(() => buildRootPayload(badCommon)).toThrow('cannot be packed');
  });
});
