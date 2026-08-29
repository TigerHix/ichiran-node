import { gunzipSync, gzipSync } from 'node:zlib';
import { expect, test } from 'bun:test';
import postgres from 'postgres';
import { openDetailStore, memoryDetailSource } from '../../portable/src/details.js';
import { buildDetailStore, loadDetailEntries } from '../src/browser-pack/details.js';

const RUN_POSTGRES_TEST = process.env.RUN_DETAILS_POSTGRES === 'true';

function assertSame(actual: unknown, expected: unknown, label: string): void {
  if (actual !== expected) {
    throw new Error(`${label}: expected ${JSON.stringify(expected)}, got ${JSON.stringify(actual)}`);
  }
}

test.skipIf(!RUN_POSTGRES_TEST)(
  'detail store exhaustively equals every root form and sense in ichiran_test',
  async () => {
    const sql = postgres({
      host: process.env.DETAILS_DATABASE_HOST ?? '/var/run/postgresql',
      database: process.env.DETAILS_DATABASE_NAME ?? 'ichiran_test',
      user: process.env.DETAILS_DATABASE_USER ?? 'tiger',
      password: process.env.DETAILS_DATABASE_PASSWORD ?? '',
      max: 5
    });

    try {
      const started = performance.now();
      const source = await loadDetailEntries(sql);
      console.log(`detail source loaded in ${Math.round(performance.now() - started)}ms`);
      const build = buildDetailStore(source);
      console.log(`detail store built in ${Math.round(performance.now() - started)}ms total`);
      const reader = await openDetailStore(
        memoryDetailSource(build.bytes),
        async (bytes) => new Uint8Array(gunzipSync(bytes))
      );

      expect(source).toHaveLength(213_732);
      expect(build.stats.entryCount).toBe(213_732);
      expect(build.stats.formCount).toBe(480_480);
      expect(reader.manifest.entryCount).toBe(source.length);

      for (let entryIndex = 0; entryIndex < source.length; entryIndex++) {
        const expected = source[entryIndex]!;
        const actual = await reader.entry(entryIndex);
        assertSame(actual.seq, expected.seq, `entry ${entryIndex} seq`);
        assertSame(actual.forms.length, expected.forms.length, `entry ${entryIndex} form count`);
        for (let formIndex = 0; formIndex < expected.forms.length; formIndex++) {
          const expectedForm = expected.forms[formIndex]!;
          const actualForm = actual.forms[formIndex]!;
          assertSame(actualForm.route, expectedForm.route, `entry ${entryIndex} form ${formIndex} route`);
          assertSame(actualForm.text, expectedForm.text, `entry ${entryIndex} form ${formIndex} text`);
          assertSame(actualForm.ord, expectedForm.ord, `entry ${entryIndex} form ${formIndex} ord`);
          assertSame(actualForm.common, expectedForm.common, `entry ${entryIndex} form ${formIndex} common`);
          assertSame(actualForm.commonTags, expectedForm.commonTags, `entry ${entryIndex} form ${formIndex} common tags`);
          assertSame(actualForm.conjugatable, expectedForm.conjugatable, `entry ${entryIndex} form ${formIndex} conjugatable`);
          assertSame(actualForm.nokanji, expectedForm.nokanji, `entry ${entryIndex} form ${formIndex} nokanji`);
          assertSame(actualForm.best, expectedForm.best, `entry ${entryIndex} form ${formIndex} best`);
        }

        assertSame(actual.senses.length, expected.senses.length, `entry ${entryIndex} sense count`);
        for (let senseIndex = 0; senseIndex < expected.senses.length; senseIndex++) {
          const expectedSense = expected.senses[senseIndex]!;
          const actualSense = actual.senses[senseIndex]!;
          assertSame(actualSense.ord, expectedSense.ord, `entry ${entryIndex} sense ${senseIndex} ord`);
          assertSame(actualSense.glosses.length, expectedSense.glosses.length, `entry ${entryIndex} sense ${senseIndex} gloss count`);
          for (let glossIndex = 0; glossIndex < expectedSense.glosses.length; glossIndex++) {
            const expectedGloss = expectedSense.glosses[glossIndex]!;
            const actualGloss = actualSense.glosses[glossIndex]!;
            assertSame(actualGloss.ord, expectedGloss.ord, `entry ${entryIndex} sense ${senseIndex} gloss ${glossIndex} ord`);
            assertSame(actualGloss.text, expectedGloss.text, `entry ${entryIndex} sense ${senseIndex} gloss ${glossIndex} text`);
          }
          assertSame(actualSense.properties.length, expectedSense.properties.length, `entry ${entryIndex} sense ${senseIndex} property count`);
          for (let propertyIndex = 0; propertyIndex < expectedSense.properties.length; propertyIndex++) {
            const expectedProperty = expectedSense.properties[propertyIndex]!;
            const actualProperty = actualSense.properties[propertyIndex]!;
            assertSame(actualProperty.tag, expectedProperty.tag, `entry ${entryIndex} sense ${senseIndex} property ${propertyIndex} tag`);
            assertSame(actualProperty.ord, expectedProperty.ord, `entry ${entryIndex} sense ${senseIndex} property ${propertyIndex} ord`);
            assertSame(actualProperty.text, expectedProperty.text, `entry ${entryIndex} sense ${senseIndex} property ${propertyIndex} text`);
          }
        }
      }

      console.log(JSON.stringify({
        details: build.stats,
        transport: {
          identityBytes: build.bytes.byteLength,
          gzip9Bytes: gzipSync(build.bytes, { level: 9 }).byteLength
        },
        verifiedMs: Math.round(performance.now() - started)
      }, null, 2));
    } finally {
      await sql.end();
    }
  },
  300_000
);
