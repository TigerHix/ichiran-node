/**
 * Downloads all Bunpro grammar points and saves them to data/bunpro/{LEVEL}/
 *
 * Usage: bun packages/grammar/scripts/download-bunpro.ts
 */

import { mkdir, writeFile, readFile, rm } from "fs/promises";
import { existsSync } from "fs";
import path from "path";

const GRAMMAR_PAGE_URL = "https://bunpro.jp/grammar_points";
const API_BASE = "https://api.bunpro.jp/api/frontend/reviewables/grammar_point";
const OUTPUT_DIR = path.join(import.meta.dir, "..", "data", "bunpro");

// Rate limiting
const DELAY_MS = 200;
const sleep = (ms: number) => new Promise((r) => setTimeout(r, ms));

interface GrammarData {
  data: {
    attributes: {
      level: string; // e.g. "JLPT5", "JLPT4", "Non-JLPT"
      slug: string;
      title: string;
    };
  };
}

async function getAllGrammarSlugs(): Promise<string[]> {
  console.log("Fetching grammar points list from", GRAMMAR_PAGE_URL);
  const res = await fetch(GRAMMAR_PAGE_URL);
  const html = await res.text();

  // Extract slugs from href="/grammar_points/SLUG"
  const regex = /grammar_points\/([^"\/]+)/g;
  const slugs = new Set<string>();
  let match;
  while ((match = regex.exec(html)) !== null) {
    const rawSlug = match[1];
    // Decode URL-encoded slugs (page has %E3%81%A0 but API wants だ)
    const slug = decodeURIComponent(rawSlug);
    // Filter out non-grammar slugs (like "new", "search", etc.)
    if (slug && !slug.includes(".") && slug !== "new" && slug !== "search") {
      slugs.add(slug);
    }
  }

  return Array.from(slugs);
}

async function downloadGrammarPoint(slug: string): Promise<GrammarData | null> {
  // Slug is already decoded, encode for URL
  const url = `${API_BASE}/${encodeURIComponent(slug)}`;
  try {
    const res = await fetch(url);
    if (!res.ok) {
      console.error(`  Failed to fetch ${slug}: ${res.status}`);
      return null;
    }
    return await res.json();
  } catch (err) {
    console.error(`  Error fetching ${slug}:`, err);
    return null;
  }
}

function sanitizeFilename(slug: string): string {
  // Replace unsafe chars for filesystem
  return slug.replace(/[<>:"/\\|?*]/g, "_");
}

function normalizeLevel(level: string): string {
  // "JLPT5" -> "JLPT5", "Non-JLPT" -> "Non-JLPT", etc.
  return level.replace(/[^a-zA-Z0-9-]/g, "");
}

async function main() {
  // Check for existing index to resume
  const indexPath = path.join(OUTPUT_DIR, "_index.json");
  let existingIndex: Record<string, { level: string; filename: string }> = {};
  if (existsSync(indexPath)) {
    try {
      const raw = JSON.parse(await readFile(indexPath, "utf-8"));
      // Handle old format (slug -> filename) vs new format (slug -> {level, filename})
      for (const [slug, value] of Object.entries(raw)) {
        if (typeof value === "string") {
          // Old format - skip, will re-download
        } else {
          existingIndex[slug] = value as { level: string; filename: string };
        }
      }
    } catch {
      // Ignore parse errors
    }
    console.log(`Found existing index with ${Object.keys(existingIndex).length} entries (new format)`);
  }

  const slugs = await getAllGrammarSlugs();
  console.log(`Found ${slugs.length} grammar points`);

  // Filter out already downloaded
  const toDownload = slugs.filter((s) => !existingIndex[s]);
  console.log(`Need to download ${toDownload.length} new grammar points`);

  const index: Record<string, { level: string; filename: string }> = { ...existingIndex };
  let downloaded = 0;
  let failed = 0;

  for (const slug of toDownload) {
    const data = await downloadGrammarPoint(slug);
    if (data) {
      const level = normalizeLevel(data.data.attributes.level || "Unknown");
      const levelDir = path.join(OUTPUT_DIR, level);
      await mkdir(levelDir, { recursive: true });

      const filename = sanitizeFilename(slug) + ".json";
      const filepath = path.join(levelDir, filename);
      await writeFile(filepath, JSON.stringify(data, null, 2));
      
      index[slug] = { level, filename };
      downloaded++;
      console.log(`[${downloaded}/${toDownload.length}] ${level}/${slug}`);
    } else {
      failed++;
      console.log(`[FAILED] ${slug}`);
    }

    // Save index periodically
    if (downloaded % 50 === 0) {
      await writeFile(indexPath, JSON.stringify(index, null, 2));
    }

    await sleep(DELAY_MS);
  }

  // Save final index
  await mkdir(OUTPUT_DIR, { recursive: true });
  await writeFile(indexPath, JSON.stringify(index, null, 2));

  console.log(`\nDone! Downloaded: ${downloaded}, Failed: ${failed}`);
  console.log(`Total in index: ${Object.keys(index).length}`);
  console.log(`Output directory: ${OUTPUT_DIR}`);
}

main().catch(console.error);
