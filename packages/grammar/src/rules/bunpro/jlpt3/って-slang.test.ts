import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './って-slang.js';
import { BUNPRO_JLPT3 } from './index.js';

const negatives = [
  // Formal quotation と (different grammar)
  // 「こんにちは」と言う (Say "hello")
  // Note: These might not appear in test data but are theoretically distinguishable

  // Hearsay だって (different grammar - see jlpt3-だって)
  // 全員合格だってよ。(I heard everyone passed.)
  // 俺だって行きたくないよ。(Even I don't want to go.)
  // Note: だって has different POS/lemma pattern

  // Quotation compound という (e.g., というのは, といった)
  // 田中という人 (a person called Tanaka)
  // This is formal と + 言う, not casual topic って

  // Instrumental/locative で + topic は (different particles)
  // 東京では電車が便利です。(In Tokyo, trains are convenient.)

  // Note: The key discriminator is:
  // - って (casual topic) has tag=助詞-副助詞
  // - と (quotation) has tag=助詞-格助詞
  // - だって (hearsay) has lemma=だって, not just って
];

describe('bunpro.jlpt3', () => {
  const engine = useSharedEngine([BUNPRO_JLPT3]);
  describeRule(rule, 'JLPT3', BUNPRO_JLPT3.id, engine.get, { negatives });
});
