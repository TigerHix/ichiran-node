/**
 * Tests for custom data loading system
 */

import { describe, test, expect } from 'bun:test';
import { XmlLoader, loadCustomData } from '../src/data/load-custom.js';
import fs from 'fs';
import { fileURLToPath } from 'node:url';

const extraPath = fileURLToPath(new URL('../../../data/sources/extra.xml', import.meta.url));

describe('Custom Data Loading', () => {
  test('XmlLoader can parse extra.xml', async () => {
    expect(fs.existsSync(extraPath)).toBe(true);

    const loader = new XmlLoader(extraPath);
    const count = await loader.slurp();

    expect(count).toBeGreaterThan(0);
    expect(loader.getEntryCount()).toBe(count);
    expect(loader.description).toBe('extra XML data');
  });

  test('XmlLoader extracts entries with sequence numbers', async () => {
    expect(fs.existsSync(extraPath)).toBe(true);

    const loader = new XmlLoader(extraPath);
    await loader.slurp();

    // Check that entries were extracted (accessing private field for testing)
    const entries = (loader as any).entries;
    expect(entries.length).toBeGreaterThan(0);

    // Check that each entry has seq and content
    for (const entry of entries) {
      expect(entry).toHaveProperty('seq');
      expect(entry).toHaveProperty('content');
      expect(entry.content).toContain('<entry>');
      expect(entry.content).toContain('</entry>');
    }
  });

  test('loadCustomData handles missing files gracefully', async () => {
    // Should not throw when files are missing
    await expect(
      loadCustomData({
        types: ['extra'],
        dataPath: '/nonexistent/path',
        silent: true
      })
    ).resolves.toBeUndefined();
  });
});
