import { expect, test } from 'bun:test';
import { createHash } from 'node:crypto';
import { gunzipSync, gzipSync } from 'node:zlib';
import { getConnection } from '../../core/src/conn.js';
import {
  buildAnalyzerSupportCore,
  loadAnalyzerSupportSource,
  type AnalyzerSupportSuffixSource
} from '../src/browser-pack/analyzer-support.js';
import { buildAnalyzerAnnotations } from '../src/browser-pack/analyzer-annotations.js';
import { openAnalyzerSupport } from '../../portable/src/analyzer-support.js';
import {
  AnalyzerAnnotationsReader,
  analyzerAnnotationsMemorySource
} from '../../portable/src/analyzer-annotations.js';

const RUN_POSTGRES_TEST = process.env.RUN_ANALYZER_SUPPORT_POSTGRES === 'true';

function same(actual: unknown, expected: unknown, label: string): void {
  const actualJson = JSON.stringify(actual);
  const expectedJson = JSON.stringify(expected);
  if (actualJson !== expectedJson) throw new Error(`${label}: ${actualJson} !== ${expectedJson}`);
}

function hash(bytes: Uint8Array): string {
  return createHash('sha256').update(bytes).digest('hex');
}

function portableSuffixValues(values: AnalyzerSupportSuffixSource['values']): unknown {
  return values.map(value => ({
    keyword: value.keyword,
    form: value.form === null ? null : {
      ...value.form,
      conjugations: !Array.isArray(value.form.conjugations)
        ? value.form.conjugations
        : value.form.conjugations.map(conjugation => {
            const { pos, type, negative, formal, ...identity } = conjugation;
            return { ...identity, property: { pos, type, negative, formal } };
          })
    }
  }));
}

test.skipIf(!RUN_POSTGRES_TEST)(
  'analyzer support exhaustively freezes PostgreSQL/cache behavior',
  async () => {
    const started = performance.now();
    const sql = getConnection();
    const source = await sql.begin('isolation level repeatable read read only', async transaction => {
      const tx = transaction as unknown as typeof sql;
      await tx.unsafe("SET LOCAL work_mem = '256MB'");
      await tx.unsafe('SET LOCAL enable_nestloop = off');
      return loadAnalyzerSupportSource(tx);
    });
    expect(source.issues ?? []).toEqual([]);
    if (!source.generated) throw new Error('Generated source was not loaded');
    console.log(JSON.stringify({
      generatedSource: {
        semanticPaths: source.generated.semanticPaths,
        matchedPaths: source.generated.matchedPaths,
        records: source.generated.records.length,
        lookupOrderRecords: source.generated.lookupOrders.length,
        lookupOrderSourceRows: source.generated.lookupOrderSourceRows,
        lookupOrderSourceSha256: source.generated.lookupOrderSourceSha256,
        lookupOrderSurfaces: source.generated.lookupOrderSurfaces,
        lookupOrderClasses: source.generated.lookupOrderClasses,
        lookupOrderEquivalenceClasses: source.generated.lookupOrderEquivalenceClasses,
        lookupOrderComponents: source.generated.lookupOrderComponents,
        lookupOrderCyclicComponents: source.generated.lookupOrderCyclicComponents,
        lookupOrderEdges: source.generated.lookupOrderEdges,
        lookupOrderMaxRank: source.generated.lookupOrderMaxRank,
        lookupOrderSha256: source.generated.lookupOrderProjectionSha256,
        lookupOrderExceptionSurfaces: source.generated.lookupOrderExceptions.length,
        lookupOrderExceptionClasses: source.generated.lookupOrderExceptionClasses,
        lookupOrderExceptionLocators: source.generated.lookupOrderExceptionLocators,
        countExceptions: source.generated.countExceptions,
        physicalGroups: source.generated.physicalGroups,
        physicalMembers: source.generated.physicalMembers,
        propertyOverrides: source.generated.propertyOverrides,
        maxMemberOrd: source.generated.maxMemberOrd,
        maxViaMemberOrd: source.generated.maxViaMemberOrd,
        maxPropOrd: source.generated.maxPropOrd,
        sha256: source.generated.projectionSha256
      }
    }, null, 2));
    const annotations = buildAnalyzerAnnotations(source.splits, source.hints, source.generated);
    console.log(JSON.stringify({
      annotations: { ...annotations.stats, sha256: hash(annotations.bytes) }
    }, null, 2));
    // 1519210 (忘れる) reaches 忘れた through a one-stage row whose
    // second_alias is SQL NULL. This production witness prevents a nullable
    // composite-key join from silently deleting every direct generated path.
    const wasureta = source.generated.records.find(value =>
      value.rootSeq === 1519210
      && value.firstAlias === 85
      && value.secondAlias === null
    );
    expect(wasureta?.members).toEqual([expect.objectContaining({
      property: expect.objectContaining({ type: 2, negative: false, formal: false }),
      memberOrd: 0,
      propOrd: 0,
      viaMemberOrd: null
    })]);
    // This production two-stage row has a physical conj_prop wildcard. The
    // u16 member property must preserve NULL rather than coerce it to false.
    const nullableProperty = source.generated.records.find(value =>
      value.rootSeq === 1000280
      && value.firstAlias === 796
      && value.secondAlias === 84
    );
    expect(nullableProperty?.members?.some(member =>
      member.property.negative === null || member.property.formal === null
    )).toBe(true);
    // The prefix target for this production path has multiple members. Its
    // final row explicitly selects prefix ordinal zero, which is not "absent".
    const ordinalZeroVia = source.generated.records.find(value =>
      value.rootSeq === 1337800
      && value.firstAlias === 693
      && value.secondAlias === 664
    );
    expect(ordinalZeroVia?.members?.some(member => member.viaMemberOrd === 0))
      .toBe(true);
    // JSON field names are not transformed by postgres.js. This manual patch
    // freezes the snake_case jsonb_to_recordset bridge in both generated CTEs.
    expect(source.generated.records.find(value =>
      value.rootSeq === 2089020
      && value.firstAlias === 45
      && value.secondAlias === null
    )?.counts).toEqual([0, 4]);
    expect(source.generated.lookupOrders.find(value =>
      value.rootSeq === 2089020
      && value.firstAlias === 45
      && value.secondAlias === null
    )).toBeDefined();
    // Collision-only morphology still activates physical lookup ordering, but
    // its root_p target is represented by the direct semantic locator. These
    // witnesses prevent the order projection from considering only non-root
    // generated targets when deciding which surfaces are observable.
    expect(source.generated.lookupOrders.find(value =>
      value.rootSeq === 2701430
      && value.firstAlias === null
      && value.secondAlias === null
    )).toBeDefined();
    expect(source.generated.lookupOrders.find(value =>
      value.rootSeq === 2844354
      && value.firstAlias === null
      && value.secondAlias === null
    )).toBeDefined();
    for (const collisionTarget of [1628500, 2065150, 2577750]) {
      expect(source.generated.lookupOrders.find(value =>
        value.rootSeq === collisionTarget
        && value.firstAlias === null
        && value.secondAlias === null
      )).toBeDefined();
    }
    const lookupRank = (
      route: 'kana' | 'kanji',
      surface: string,
      rootSeq: number,
      firstAlias: number | null
    ): number => {
      const exception = source.generated!.lookupOrderExceptions.find(value =>
        value.route === route && value.surface === surface
      );
      const value = (exception?.orders ?? source.generated!.lookupOrders).find(order =>
        order.rootSeq === rootSeq
        && order.firstAlias === firstAlias
        && order.secondAlias === null
      );
      if (!value) {
        throw new Error(
          `Missing lookup-order witness ${route}/${surface}/${rootSeq}/${String(firstAlias)}`
        );
      }
      return value.rank;
    };
    // These ties are observable before score sorting. They deliberately mix
    // direct and generated physical rows whose surrogate-id order differs from
    // the legacy unordered text lookup followed by `unshift`.
    expect(lookupRank('kana', 'やわらげる', 1561960, null))
      .toBeLessThan(lookupRank('kana', 'やわらげる', 1561950, 311));
    expect(lookupRank('kanji', '否めない', 1482930, null))
      .toBeLessThan(lookupRank('kanji', '否めない', 1482910, 475));
    expect(lookupRank('kanji', '否めない', 1482910, 475))
      .toBeLessThan(lookupRank('kanji', '否めない', 1482920, 475));
    expect(lookupRank('kana', 'でもない', 2098230, 608))
      .toBeLessThan(lookupRank('kana', 'でもない', 2097940, null));
    // Preserve the collision-only routes that originally widened graph
    // coverage. Runtime collision candidates normalize to the direct target
    // locator, so only direct semantic roots appear here.
    expect(lookupRank('kana', 'です', 2701430, null))
      .toBeLessThan(lookupRank('kana', 'です', 1628500, null));
    expect(lookupRank('kana', 'こ', 2844354, null))
      .toBeLessThan(lookupRank('kana', 'こ', 2577750, null));
    expect(lookupRank('kana', 'こ', 2577750, null))
      .toBeLessThan(lookupRank('kana', 'こ', 2065150, null));

    const core = buildAnalyzerSupportCore(source);
    const rebuiltCore = buildAnalyzerSupportCore({
      ...source,
      suffixes: [...source.suffixes].reverse(),
      suffixClasses: [...source.suffixClasses].reverse(),
      counters: [...source.counters].reverse(),
      collisions: [...source.collisions].reverse()
    });
    expect(rebuiltCore.bytes).toEqual(core.bytes);
    const reader = openAnalyzerSupport(core.bytes);

    for (const suffix of source.suffixes) {
      same(reader.suffix(suffix.text), portableSuffixValues(suffix.values), `suffix ${suffix.text}`);
    }
    for (const value of source.suffixClasses) {
      expect(reader.suffixClass(value.seq)).toBe(value.keyword);
    }
    const counterKeys = new Set(source.counters.map(value => value.key));
    for (const key of counterKeys) {
      const expected = source.counters
        .filter(value => value.key === key)
        .sort((left, right) => left.order - right.order)
        .map(({ key: _key, order: _order, ...value }) => value);
      same(reader.counters(key), expected, `counter ${key}`);
    }
    for (const value of source.collisions) {
      expect(reader.collision(value.rootSeq, value.route, value.surface, value.ruleIds))
        .toEqual(value);
    }

    const rebuiltAnnotations = buildAnalyzerAnnotations(
      [...source.splits].reverse(),
      [...source.hints].reverse(),
      {
        ...source.generated,
        records: [...source.generated.records].reverse(),
        lookupOrders: [...source.generated.lookupOrders].reverse(),
        lookupOrderExceptions: [...source.generated.lookupOrderExceptions].reverse().map(value => ({
          ...value,
          orders: [...value.orders].reverse()
        }))
      }
    );
    expect(rebuiltAnnotations.bytes).toEqual(annotations.bytes);
    const annotationReader = await AnalyzerAnnotationsReader.open(
      analyzerAnnotationsMemorySource(annotations.bytes),
      async bytes => new Uint8Array(gunzipSync(bytes))
    );
    const splitsBySeq = new Map<number, typeof source.splits[number][]>();
    for (const value of source.splits) {
      const values = splitsBySeq.get(value.definitionSeq) ?? [];
      values.push(value);
      splitsBySeq.set(value.definitionSeq, values);
    }
    const hintsBySeq = new Map<number, typeof source.hints[number][]>();
    for (const value of source.hints) {
      const values = hintsBySeq.get(value.definitionSeq) ?? [];
      values.push(value);
      hintsBySeq.set(value.definitionSeq, values);
    }
    for (const seq of new Set([...splitsBySeq.keys(), ...hintsBySeq.keys()])) {
      const view = await annotationReader.preload([seq]);
      for (const value of splitsBySeq.get(seq) ?? []) {
        same(view.split(seq, value.route, value.surface, value.kind), value, `split ${seq}/${value.surface}`);
      }
      for (const value of hintsBySeq.get(seq) ?? []) {
        expect(view.hint(seq, value.route, value.surface, value.reading)).toBe(value.hint);
      }
      view.clear();
    }
    for (const value of source.generated.records) {
      const aliases = value.secondAlias === null
        ? [value.firstAlias] as const
        : [value.firstAlias, value.secondAlias] as const;
      same(await annotationReader.generated(value.rootSeq, aliases), {
        nKanji: value.counts?.[0] ?? null,
        nKana: value.counts?.[1] ?? null,
        physicalGroup: value.physicalGroup,
        members: value.members
      }, `generated ${value.rootSeq}/${aliases.join(',')}`);
    }
    for (const value of source.generated.lookupOrders) {
      const aliases = value.firstAlias === null
        ? null
        : value.secondAlias === null
          ? [value.firstAlias] as const
          : [value.firstAlias, value.secondAlias] as const;
      expect(await annotationReader.lookupOrder('kana', '__global__', value.rootSeq, aliases))
        .toBe(value.rank);
    }
    for (const exception of source.generated.lookupOrderExceptions) {
      for (const value of exception.orders) {
        const aliases = value.firstAlias === null
          ? null
          : value.secondAlias === null
            ? [value.firstAlias] as const
            : [value.firstAlias, value.secondAlias] as const;
        expect(await annotationReader.lookupOrder(
          exception.route, exception.surface, value.rootSeq, aliases
        )).toBe(value.rank);
      }
    }

    console.log(JSON.stringify({
      elapsedMs: Math.round(performance.now() - started),
      core: {
        ...core.stats,
        sha256: hash(core.bytes),
        gzip9Bytes: gzipSync(core.bytes, { level: 9 }).byteLength
      },
      annotations: {
        ...annotations.stats,
        sha256: hash(annotations.bytes)
      },
      generatedProjection: {
        semanticPaths: source.generated.semanticPaths,
        matchedPaths: source.generated.matchedPaths,
        records: source.generated.records.length,
        lookupOrderRecords: source.generated.lookupOrders.length,
        lookupOrderSourceRows: source.generated.lookupOrderSourceRows,
        lookupOrderSourceSha256: source.generated.lookupOrderSourceSha256,
        lookupOrderSurfaces: source.generated.lookupOrderSurfaces,
        lookupOrderClasses: source.generated.lookupOrderClasses,
        lookupOrderEquivalenceClasses: source.generated.lookupOrderEquivalenceClasses,
        lookupOrderComponents: source.generated.lookupOrderComponents,
        lookupOrderCyclicComponents: source.generated.lookupOrderCyclicComponents,
        lookupOrderEdges: source.generated.lookupOrderEdges,
        lookupOrderMaxRank: source.generated.lookupOrderMaxRank,
        lookupOrderSha256: source.generated.lookupOrderProjectionSha256,
        lookupOrderExceptionSurfaces: source.generated.lookupOrderExceptions.length,
        lookupOrderExceptionClasses: source.generated.lookupOrderExceptionClasses,
        lookupOrderExceptionLocators: source.generated.lookupOrderExceptionLocators,
        countExceptions: source.generated.countExceptions,
        physicalGroups: source.generated.physicalGroups,
        physicalMembers: source.generated.physicalMembers,
        propertyOverrides: source.generated.propertyOverrides,
        maxMemberOrd: source.generated.maxMemberOrd,
        maxViaMemberOrd: source.generated.maxViaMemberOrd,
        maxPropOrd: source.generated.maxPropOrd,
        sha256: source.generated.projectionSha256
      }
    }, null, 2));
  },
  600_000
);
