import { describe, test } from 'bun:test';
import { useSharedEngine } from './src/rules/bunpro/_test/engine.js';

const sentences = [
  '彼とは一度きりしか会えていない。',
  '父がついに寝たきりになってしまった。',
  '昨日、晩ご飯食べたきり何も食べてないから、めちゃお腹が空いた。',
  'お祖母ちゃんは去年病気で倒れて、寝たきりになった。',
  '一人きりになれる時間が欲しい。',
  '二人っきりの時間はいいね！',
  '寂しい時、一人っきりで泣く。',
  'やっと二人っきりになれたね。',
  '彼女とは喧嘩したっきり会っていません。',
  'チャンスは一度きりしかないから緊張する。',
  'その店に一回行ったきり、行っていない。',
  '5年前に会ったきりだから、また会いたいですね。',
  '一度習ったきりだから、やり方を忘れてしまいました。',
  '3年前に飲んだっきり薬を飲む機会がない。',
  '「また後で電話します」と言ったきり全然連絡がありません。',
  '朝ご飯を食べたっきり何も食べていないから、お腹がすいている。',
  'それっきり彼は部屋から出なくなった。',
];

describe('Analyze kiri sentences', () => {
  const engine = useSharedEngine([]);

  for (const sent of sentences) {
    test(sent, async () => {
      const doc = await engine.get().analyze(sent);
      console.log('\n' + '='.repeat(80));
      console.log('SENTENCE:', sent);
      console.log('='.repeat(80));
      const tokens = doc.tokens.map(t => ({
        text: t.text,
        lemma: t.lemma,
        pos: t.pos,
        dep: t.dep,
        inflectionForm: t.inflectionForm,
        head: t.head,
        id: t.id,
      }));
      console.log(JSON.stringify(tokens, null, 2));
    });
  }
});
