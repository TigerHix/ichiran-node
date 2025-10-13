// Load entry tests - verify core loading behavior
import { describe, test, expect } from 'bun:test';
import { setupTests } from '@ichiran/testing';
import { getConnection } from '@ichiran/core';
import { loadEntry } from '../src/data/load-entry.js';

setupTests();

describe('loadEntry primary_nokanji behavior', () => {
  test('sets primary_nokanji=true when any kana reading has re_nokanji', async () => {
    const sql = getConnection();
    
    // Cleanup first (in case previous test failed)
    await sql`DELETE FROM entry WHERE seq = 9999001`;
    
    // Entry with re_nokanji in one of its r_ele
    const xml = `<entry>
      <ent_seq>9999001</ent_seq>
      <k_ele>
        <keb>漢字</keb>
      </k_ele>
      <r_ele>
        <reb>かんじ</reb>
      </r_ele>
      <r_ele>
        <reb>カンジ</reb>
        <re_nokanji/>
      </r_ele>
      <sense>
        <pos>&n;</pos>
        <gloss xml:lang="eng">test</gloss>
      </sense>
    </entry>`;

    const result_seq = await loadEntry(xml, { seq: 9999001 });
    expect(result_seq).toBe(9999001);

    const result = await sql`SELECT primary_nokanji FROM entry WHERE seq = 9999001`;
    expect(result.length).toBe(1);
    expect(result[0].primaryNokanji).toBe(true);  // postgres library returns camelCase

    // Cleanup
    await sql`DELETE FROM entry WHERE seq = 9999001`;
  });

  test('sets primary_nokanji=false when no kana readings have re_nokanji', async () => {
    const sql = getConnection();
    
    // Cleanup first
    await sql`DELETE FROM entry WHERE seq = 9999002`;
    
    // Entry without re_nokanji
    const xml = `<entry>
      <ent_seq>9999002</ent_seq>
      <k_ele>
        <keb>漢字</keb>
      </k_ele>
      <r_ele>
        <reb>かんじ</reb>
      </r_ele>
      <sense>
        <pos>&n;</pos>
        <gloss xml:lang="eng">test</gloss>
      </sense>
    </entry>`;

    const result_seq = await loadEntry(xml, { seq: 9999002 });
    expect(result_seq).toBe(9999002);

    const result = await sql`SELECT primary_nokanji FROM entry WHERE seq = 9999002`;
    expect(result.length).toBe(1);
    expect(result[0].primaryNokanji).toBe(false);

    // Cleanup
    await sql`DELETE FROM entry WHERE seq = 9999002`;
  });

  test('does not process re_nokanji for kanji readings', async () => {
    const sql = getConnection();
    
    // Cleanup first
    await sql`DELETE FROM entry WHERE seq = 9999003`;
    
    // Entry with only k_ele (no r_ele)
    const xml = `<entry>
      <ent_seq>9999003</ent_seq>
      <k_ele>
        <keb>漢字</keb>
      </k_ele>
      <sense>
        <pos>&n;</pos>
        <gloss xml:lang="eng">test</gloss>
      </sense>
    </entry>`;

    const result_seq = await loadEntry(xml, { seq: 9999003 });
    expect(result_seq).toBe(9999003);

    const result = await sql`SELECT primary_nokanji, n_kanji, n_kana FROM entry WHERE seq = 9999003`;
    expect(result.length).toBe(1);
    expect(result[0].primaryNokanji).toBe(false);
    expect(result[0].nKanji).toBe(1);
    expect(result[0].nKana).toBe(0);

    // Cleanup
    await sql`DELETE FROM entry WHERE seq = 9999003`;
  });
});

describe('loadEntry transaction atomicity', () => {
  test('returns undefined for ON CONFLICT (skip existing)', async () => {
    const sql = getConnection();
    
    // Cleanup first
    await sql`DELETE FROM entry WHERE seq = 9999004`;
    
    // Insert entry once
    const xml = `<entry>
      <ent_seq>9999004</ent_seq>
      <r_ele>
        <reb>test</reb>
      </r_ele>
      <sense>
        <pos>&n;</pos>
        <gloss xml:lang="eng">test entry</gloss>
      </sense>
    </entry>`;

    const first = await loadEntry(xml, { seq: 9999004 });
    expect(first).toBe(9999004);

    // Try to insert again - should return undefined (ON CONFLICT)
    const second = await loadEntry(xml, { seq: 9999004, ifExists: 'skip' });
    expect(second).toBe(undefined);

    // Cleanup
    await sql`DELETE FROM entry WHERE seq = 9999004`;
  });
});

