export interface AnalyzerSample {
  readonly label: string;
  readonly text: string;
}

export const ANALYZER_SAMPLES: readonly AnalyzerSample[] = [
  { label: 'Everyday', text: '今日は公園で友達と話しました。' },
  { label: 'Inflection', text: '昨日は雨が降らなかったので、家で本を読んでいました。' },
  { label: 'Counters', text: 'りんごを三個と鉛筆を二本買いました。' },
  { label: 'Numbers', text: '会議は午後三時半から二時間の予定です。' },
  { label: 'Ambiguous reading', text: '生で食べられる魚を市場で買った。' },
  { label: 'Mixed scripts', text: 'きょうは日本語の勉強をしています。' },
  { label: 'Punctuation', text: '「もう帰る？」と彼女が聞いた。' },
  { label: 'Colloquial', text: 'そんなこと言われても、わかんないよ。' },
  { label: 'Compound', text: '東京都内の美術館を見学しました。' },
  { label: 'Kana', text: 'ゆっくりあるいてかえろう。' }
];
