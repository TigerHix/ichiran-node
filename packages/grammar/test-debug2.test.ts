import { describe, it, expect } from 'bun:test';
import { GinzaClient } from './src/ginza/client.js';
import { GrammarEngine } from './src/program.js';
import { BUNPRO_JLPT4 } from './src/rules/bunpro/jlpt4/index.js';

describe('DEBUG: だいたい', () => {
  it('should match with full ruleset', async () => {
    const client = new GinzaClient();
    await client.start();
    const engine = await GrammarEngine.create([BUNPRO_JLPT4], { client });

    const sentence = "文化祭の準備をしている生徒：「だいたいでいいから、午前中までにはおわらせておいて。」";
    console.log("Testing:", sentence);
    
    const hits = await engine.match(sentence);
    console.log("Total hits:", hits.length);
    for (const hit of hits) {
      console.log("  -", hit.ruleId);
    }
    
    const daitaiHit = hits.find((h) => h.ruleId === 'だいたい');
    console.log("だいたい hit:", daitaiHit ? "YES" : "NO");
    
    expect(daitaiHit).toBeDefined();

    await client.stop();
  });
});
