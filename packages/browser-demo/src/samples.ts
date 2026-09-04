import type { SampleId } from '@ichiran/presentation';

export interface AnalyzerSample {
  readonly id: SampleId;
  readonly text: string;
}

export const ANALYZER_SAMPLES: readonly AnalyzerSample[] = [
  { id: 'everyday', text: '今日は公園で友達と話しました。' },
  { id: 'inflection', text: '昨日は雨が降らなかったので、家で本を読んでいました。' },
  { id: 'counters', text: 'りんごを三個と鉛筆を二本買いました。' },
  { id: 'numbers', text: '会議は午後三時半から二時間の予定です。' },
  { id: 'ambiguous', text: '生で食べられる魚を市場で買った。' },
  { id: 'mixedScripts', text: 'きょうは日本語の勉強をしています。' },
  { id: 'punctuation', text: '「もう帰る？」と彼女が聞いた。' },
  { id: 'colloquial', text: 'そんなこと言われても、わかんないよ。' },
  { id: 'compound', text: '東京都内の美術館を見学しました。' },
  { id: 'kana', text: 'ゆっくりあるいてかえろう。' }
];
