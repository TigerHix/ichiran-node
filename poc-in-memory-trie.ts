/**
 * Proof of Concept: In-Memory Trie for Japanese Word Segmentation
 * 
 * This demonstrates a minimal trie structure that could replace the PostgreSQL
 * word lookup queries for segmentation.
 */

interface WordEntry {
  text: string;
  seq: number;
  ord: number;
  common: number | null;
  commonTags: string;
  conjugateP: boolean;
  nokanji: boolean;
  type: 'kanji' | 'kana';
}

class TrieNode {
  children: Map<string, TrieNode> = new Map();
  // Words that end at this node
  entries: WordEntry[] = [];
}

class WordTrie {
  private root = new TrieNode();
  private stats = {
    totalNodes: 0,
    totalEntries: 0,
    memoryEstimate: 0
  };

  /**
   * Insert a word into the trie
   */
  insert(entry: WordEntry): void {
    let node = this.root;
    
    // Walk down the trie, creating nodes as needed
    for (const char of entry.text) {
      if (!node.children.has(char)) {
        node.children.set(char, new TrieNode());
        this.stats.totalNodes++;
      }
      node = node.children.get(char)!;
    }
    
    // Add entry at the end node
    node.entries.push(entry);
    this.stats.totalEntries++;
  }

  /**
   * Find all words that are substrings of the input text
   * This is equivalent to findSubstringWords() in the current implementation
   */
  findSubstrings(text: string, maxLength: number = 50): Map<string, WordEntry[]> {
    const results = new Map<string, WordEntry[]>();

    // For each starting position
    for (let start = 0; start < text.length; start++) {
      let node = this.root;
      let currentText = '';

      // Walk the trie as far as possible
      for (let end = start; end < Math.min(text.length, start + maxLength); end++) {
        const char = text[end];
        currentText += char;

        // Follow the trie
        if (!node.children.has(char)) {
          break;
        }
        node = node.children.get(char)!;

        // If there are entries at this node, we found matching words
        if (node.entries.length > 0) {
          results.set(currentText, node.entries);
        }
      }
    }

    return results;
  }

  /**
   * Get memory usage statistics
   */
  getStats() {
    const nodeSize = 48; // Approximate: Map overhead + object overhead
    const entrySize = 60; // Approximate: all fields
    
    this.stats.memoryEstimate = 
      (this.stats.totalNodes * nodeSize) + 
      (this.stats.totalEntries * entrySize);

    return {
      ...this.stats,
      memoryMB: (this.stats.memoryEstimate / 1024 / 1024).toFixed(2)
    };
  }

  /**
   * Serialize to binary format for fast loading
   */
  serialize(): Uint8Array {
    // This would be a compact binary format:
    // - Header with counts
    // - Node structure (children indices)
    // - Entry data (packed structs)
    throw new Error('Not implemented - just a placeholder');
  }

  /**
   * Deserialize from binary format
   */
  static deserialize(_data: Uint8Array): WordTrie {
    throw new Error('Not implemented - just a placeholder');
  }
}

/**
 * Build trie from PostgreSQL database
 * This would be run once during build/initialization
 */
async function buildTrieFromDatabase(): Promise<WordTrie> {
  const postgres = (await import('postgres')).default;
  const sql = postgres(process.env.ICHIRAN_DB_URL || 'postgresql://postgres:password@localhost:6777/jmdict_test');
  
  const trie = new WordTrie();
  
  console.log('Loading kanji_text entries...');
  const kanjiEntries = await sql<any[]>`
    SELECT text, seq, ord, common, common_tags, conjugate_p, nokanji
    FROM kanji_text
  `;
  
  for (const row of kanjiEntries) {
    trie.insert({
      text: row.text,
      seq: row.seq,
      ord: row.ord,
      common: row.common,
      commonTags: row.common_tags || '',
      conjugateP: row.conjugate_p,
      nokanji: row.nokanji,
      type: 'kanji'
    });
  }
  
  console.log('Loading kana_text entries...');
  const kanaEntries = await sql<any[]>`
    SELECT text, seq, ord, common, common_tags, conjugate_p, nokanji
    FROM kana_text
  `;
  
  for (const row of kanaEntries) {
    trie.insert({
      text: row.text,
      seq: row.seq,
      ord: row.ord,
      common: row.common,
      commonTags: row.common_tags || '',
      conjugateP: row.conjugate_p,
      nokanji: row.nokanji,
      type: 'kana'
    });
  }
  
  await sql.end();
  
  return trie;
}

/**
 * Benchmark comparison
 */
async function benchmark() {
  console.log('Building trie from database...');
  const startBuild = Date.now();
  const trie = await buildTrieFromDatabase();
  const buildTime = Date.now() - startBuild;
  
  const stats = trie.getStats();
  console.log('\nTrie Statistics:');
  console.log(`  Total nodes: ${stats.totalNodes.toLocaleString()}`);
  console.log(`  Total entries: ${stats.totalEntries.toLocaleString()}`);
  console.log(`  Estimated memory: ${stats.memoryMB} MB`);
  console.log(`  Build time: ${buildTime}ms`);
  
  // Test lookup
  const testInputs = [
    '日本語を勉強しています',
    '今日は良い天気ですね',
    '食べ物が美味しい',
    '東京に行きたい'
  ];
  
  console.log('\nLookup Benchmark:');
  for (const input of testInputs) {
    const start = Date.now();
    const results = trie.findSubstrings(input);
    const time = Date.now() - start;
    
    console.log(`  Input: ${input}`);
    console.log(`  Found: ${results.size} substrings`);
    console.log(`  Time: ${time}ms`);
  }
  
  // Compare with database lookup
  console.log('\nDatabase Lookup Benchmark (for comparison):');
  const postgres = (await import('postgres')).default;
  const sql = postgres(process.env.ICHIRAN_DB_URL || 'postgresql://postgres:password@localhost:6777/jmdict_test');
  
  for (const input of testInputs) {
    // Generate substrings
    const substrings: string[] = [];
    for (let start = 0; start < input.length; start++) {
      for (let end = start + 1; end <= Math.min(input.length, start + 50); end++) {
        substrings.push(input.slice(start, end));
      }
    }
    
    const start = Date.now();
    const uniqueSubstrings = [...new Set(substrings)];
    
    // Query database (this is what the current code does)
    const kanaResults = await sql<any[]>`
      SELECT * FROM kana_text WHERE text IN ${sql(uniqueSubstrings)}
    `;
    const kanjiResults = await sql<any[]>`
      SELECT * FROM kanji_text WHERE text IN ${sql(uniqueSubstrings)}
    `;
    
    const time = Date.now() - start;
    const totalResults = kanaResults.length + kanjiResults.length;
    
    console.log(`  Input: ${input}`);
    console.log(`  Found: ${totalResults} entries`);
    console.log(`  Time: ${time}ms`);
  }
  
  await sql.end();
}

/**
 * Example: How this would integrate into the existing code
 */
/*
// In packages/core/src/dict/lookup.ts:

let wordTrie: WordTrie | null = null;

async function ensureTrieLoaded() {
  if (!wordTrie) {
    // Load from pre-built binary file
    const fs = await import('fs/promises');
    const data = await fs.readFile('./data/word-trie.bin');
    wordTrie = WordTrie.deserialize(new Uint8Array(data));
  }
}

export async function findSubstringWords(
  str: string,
  sticky: number[] = []
): Promise<SubstringHash> {
  await ensureTrieLoaded();
  
  // Use trie instead of database
  const results = wordTrie!.findSubstrings(str);
  
  // Convert to existing SubstringHash format
  const substringHash = new Map<string, SubstringHashEntry[]>();
  for (const [text, entries] of results) {
    substringHash.set(
      text,
      entries.map(e => ({
        table: e.type === 'kanji' ? 'kanji_text' : 'kana_text',
        row: e as any
      }))
    );
  }
  
  return substringHash;
}
*/

// Run benchmark if executed directly
if (require.main === module) {
  benchmark().catch(console.error);
}

export { WordTrie, WordEntry, buildTrieFromDatabase };

