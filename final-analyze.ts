#!/usr/bin/env tsx
/**
 * Final analysis using romanizeStar like the CLI does
 */

import { romanizeStar } from './src/romanize.js';
import { transformRomanizeStarResult } from './src/presentation/transformers.js';

async function analyzeSegmentation(sentence: string, description: string) {
  console.log('\n' + '='.repeat(80));
  console.log(description);
  console.log(`Sentence: ${sentence}`);
  console.log('='.repeat(80));

  const result = await romanizeStar(sentence, { limit: 1 });
  const transformed = await transformRomanizeStarResult(result);

  // transformed is an array of segmentation alternatives
  if (!transformed || transformed.length === 0) {
    console.log('❌ No segmentations found!');
    return;
  }

  const firstAlt = transformed[0];
  const segments = firstAlt.segments || [];

  console.log(`\nNumber of segments: ${segments.length}`);

  for (let i = 0; i < segments.length; i++) {
    const seg = segments[i];
    console.log(`\n[${i}] ${seg.text}`);
    console.log(`   Reading: ${seg.reading || 'none'}`);
    console.log(`   POS: ${seg.entry?.pos || 'none'}`);

    if (seg.entry) {
      const pos = seg.entry.pos || '';
      const isVerb = /\[v[1-9t]/.test(pos) || /\[vi,/.test(pos) || /\[vt,/.test(pos);
      const isAdj = /\[adj-/.test(pos);
      const isAux = /\[aux/.test(pos);
      const isExp = /\[exp/.test(pos);
      const isPron = /\[pn/.test(pos);
      const isNoun = /\[n\]/.test(pos) || /\[n,/.test(pos);

      console.log(`   Type: verb=${isVerb}, adj=${isAdj}, aux=${isAux}, exp=${isExp}, pron=${isPron}, noun=${isNoun}`);

      if (seg.entry.conj && seg.entry.conj.length > 0) {
        console.log(`   Conjugation: ${seg.entry.conj[0].reading} (${seg.entry.conj[0].prop?.[0]?.type || 'unknown'})`);
      }
    }
  }

  // Analyze specific patterns
  console.log('\n' + '-'.repeat(80));
  console.log('Pattern analysis:');

  for (let i = 0; i < segments.length; i++) {
    const seg = segments[i];
    const prevSeg = i > 0 ? segments[i - 1] : null;
    const nextSeg = i < segments.length - 1 ? segments[i + 1] : null;

    // Check for ので
    if (seg.text === 'ので') {
      console.log(`\n  ⚠️  Found 'ので' at index ${i}`);
      console.log(`     This is the CAUSAL connector (should not match のです pattern)`);
      console.log(`     POS: ${seg.entry?.pos}`);
    }

    // Check for の + です sequence
    if (seg.text === 'の' && nextSeg && nextSeg.text === 'です') {
      console.log(`\n  👀 Found 'の + です' sequence at indices ${i}, ${i + 1}`);

      if (prevSeg) {
        console.log(`     Previous token: ${prevSeg.text}`);
        console.log(`     Previous POS: ${prevSeg.entry?.pos}`);

        const prevPos = prevSeg.entry?.pos || '';
        const prevIsVerb = /\[v[1-9t]/.test(prevPos) || /\[vi,/.test(prevPos) || /\[vt,/.test(prevPos);
        const prevIsAdj = /\[adj-/.test(prevPos);
        const prevIsAux = /\[aux/.test(prevPos);
        const prevIsNoun = /\[n\]/.test(prevPos) || /\[n,/.test(prevPos);

        if (prevIsVerb || prevIsAdj || prevIsAux) {
          console.log(`     ✅ Previous token IS a predicate (should match explanatory のです)`);
        } else if (prevIsNoun) {
          console.log(`     ❌ Previous token is a bare noun (possessive/interrogative の, NOT explanatory)`);
        } else {
          console.log(`     ⚠️  Previous token type: verb=${prevIsVerb}, adj=${prevIsAdj}, aux=${prevIsAux}, noun=${prevIsNoun}`);
        }
      }
    }

    // Check for のです/んです as single token
    if (seg.text === 'のです' || seg.text === 'んです') {
      console.log(`\n  💡 Found '${seg.text}' as SINGLE TOKEN at index ${i}`);
      console.log(`     POS: ${seg.entry?.pos}`);

      if (prevSeg) {
        console.log(`     Previous token: ${prevSeg.text}`);
        console.log(`     Previous POS: ${prevSeg.entry?.pos}`);

        const prevPos = prevSeg.entry?.pos || '';
        const prevIsVerb = /\[v[1-9t]/.test(prevPos) || /\[vi,/.test(prevPos) || /\[vt,/.test(prevPos);
        const prevIsAdj = /\[adj-/.test(prevPos);
        const prevIsAux = /\[aux/.test(prevPos);

        if (prevIsVerb || prevIsAdj || prevIsAux) {
          console.log(`     ✅ Previous token IS a predicate (should match)`);
        } else {
          console.log(`     ❌ Previous token is NOT a predicate`);
        }
      }
    }

    // Check for 何
    if (seg.text === '何' || (seg.text && seg.text.includes('何'))) {
      console.log(`\n  🔍 Found '何' (nani/nan) at index ${i}`);
      console.log(`     Text: ${seg.text}`);
      console.log(`     Reading: ${seg.reading}`);
      console.log(`     POS: ${seg.entry?.pos}`);

      const pos = seg.entry?.pos || '';
      const isPron = /\[pn/.test(pos);
      const isNoun = /\[n\]/.test(pos) || /\[n,/.test(pos);
      const isExp = /\[exp/.test(pos);

      console.log(`     Type: pronoun=${isPron}, noun=${isNoun}, expression=${isExp}`);

      if (isPron || isNoun) {
        console.log(`     ⚠️  This is a PRONOUN/NOUN, not an expression!`);
        console.log(`     If followed by の, it's interrogative/possessive, NOT explanatory`);
      }
    }
  }
}

async function main() {
  console.log('N4 のです/んです - Final Debug Analysis');
  console.log('======================================\n');

  await analyzeSegmentation(
    '急いでいたので、丁寧に説明できなかったのです。',
    'FALSE NEGATIVE: Should match のです at END but doesn\'t'
  );

  await analyzeSegmentation(
    'これは何のですか。',
    'FALSE POSITIVE: Should NOT match (interrogative の)'
  );

  await analyzeSegmentation(
    '今日は早く帰るんです。',
    'CONTROL: SHOULD match (explanatory んです)'
  );

  await analyzeSegmentation(
    'これは母のです。',
    'CONTROL: should NOT match (possessive の)'
  );

  await analyzeSegmentation(
    '雨なので、今日は中止します。',
    'CONTROL: should NOT match (causal ので)'
  );
}

main().catch(console.error);
