import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './お-願う.js';
import { BUNPRO_JLPT2 } from './index.js';

const positives = [
  // お + verb-stem + 願います (polite form)
  'お待ち願います。',
  'お待ち願います。鈴木社長はもうすぐ到着するので、もう少々お待ち願います。',
  'お確かめ願います。こちらからのメールが届いているか、お確かめ願います。',
  'お返事願います。一週間以内にお返事願います。',
  'お察し願います。はっきりとは申し上げられません。お察し願います。',
  'お静かに願います。ここは図書館です。お静かに願います。',
  'お待ち願えますか。面会をご希望の方は担当者が会議中ですので、待合室でお待ち願えますか？',

  // ご + noun/verb-stem + 願います (polite form)
  'ご確認願います。この契約書にご記入された情報に間違いがないか、ご確認願います。',
  'ご協力願います。歩道での禁煙にご協力願います。',
  'ご提出願います。PDFでご提出願います。',
  'ご検討願います。これが我が社の企画書です。ご検討願います。',
  'ご対応願います。できる限り早めにご対応願います。',
  'ご連絡願います。問題が起こった場合はご連絡願います。',
  'ご配慮願います。妻は体調が悪いので、ご配慮願います。',
  'ご協力願います。公共の場での禁酒にご協力願います。',

  // Without prefix (direct 願います)
  'サイン願います。この契約書の下の方と、この書類の下の方にサイン願います。',
  'サイン願います。書類のこの下にサイン願います。',
];

const negatives = [
  // Regular てください pattern (not お-願う)
  '待ってください。',
  '書いてください。',
  '来てください。',

  // Regular お-ください pattern (honorific but different verb)
  'お入りください。',
  'お座りください。',
  'ご覧ください。',

  // お-する humble pattern (different grammar)
  'お電話します。',
  'ご確認する。',
  'お待ちしました。',

  // Regular negau verb usage (not honorific request)
  '幸せを願う。',
  '成功を願っている。',
  '平和を願う。',

  // TODO: These should be excluded but currently aren't
  // 'お願いします。',
  // 'お願いいたします。',
];

describe('bunpro.jlpt2', () => {
  const engine = useSharedEngine([BUNPRO_JLPT2]);
  describeRule(rule, 'JLPT2', BUNPRO_JLPT2.id, engine.get, { positives, negatives });
});
