#!/usr/bin/env node
import { GinzaClient } from './src/ginza/client.js';

const client = new GinzaClient();

async function analyze(text) {
  const result = await client.parse(text);
  console.log(`\n=== ${text} ===`);
  console.log(JSON.stringify(result, null, 2));
}

await analyze('行かないわけにはいかない');
await analyze('しないわけにはいきません');
await analyze('食べないわけにはいかない');
await analyze('勉強しないわけにはいかない事情がある');
await client.close();
