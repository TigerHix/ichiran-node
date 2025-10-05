// ichiran/dict/wordInfo - WordInfo class and processing functions
// Extracted from presentation.ts to break circular dependency
// between presentation.ts and segmentation.ts

import { getCharClass } from '../characters.js';

// =============================================================================
// WordInfo Class
// =============================================================================

export class WordInfo {
  type: 'kanji' | 'kana' | 'gap';
  text: string;
  trueText?: string | null;
  kana: string | string[];
  seq?: number | number[] | null;
  conjugations?: number[] | ':root' | null;
  score: number;
  components?: WordInfo[];
  alternative?: boolean;
  primary?: boolean;
  start?: number;
  end?: number;
  counter?: [string, boolean] | null;
  skipped: number;

  constructor(data: Partial<WordInfo> & { type: WordInfo['type']; text: string; kana: WordInfo['kana'] }) {
    this.type = data.type;
    this.text = data.text;
    this.trueText = data.trueText;
    this.kana = data.kana;
    this.seq = data.seq;
    this.conjugations = data.conjugations;
    this.score = data.score ?? 0;
    this.components = data.components;
    this.alternative = data.alternative;
    this.primary = data.primary ?? true;
    this.start = data.start;
    this.end = data.end;
    this.counter = data.counter;
    this.skipped = data.skipped ?? 0;
  }

  // Line 1260-1278: defun word-info-json
  toJSON(): any {
    return {
      type: this.type.toUpperCase(),
      text: this.text,
      truetext: this.trueText,
      kana: this.kana,
      seq: this.seq,
      conjugations: this.conjugations === ':root' ? 'ROOT' : this.conjugations,
      score: this.score,
      components: this.components?.map(c => c.toJSON()),
      alternative: this.alternative,
      primary: this.primary,
      start: this.start,
      end: this.end,
      counter: this.counter,
      skipped: this.skipped
    };
  }
}

// =============================================================================
// Processing Functions
// =============================================================================

// Line 1415-1440: defun process-word-info
export function processWordInfo(wiList: WordInfo[]): WordInfo[] {
  // Handle 何 (nani/nan) reading selection
  for (let i = 0; i < wiList.length; i++) {
    const wi = wiList[i];
    const wiNext = i < wiList.length - 1 ? wiList[i + 1] : null;

    if (wiNext && wi.text === '何') {
      const kn = Array.isArray(wiNext.kana) ? wiNext.kana : [wiNext.kana];
      let nani = false;
      let nan = false;

      for (const kana of kn) {
        if (kana.length > 0) {
          const firstChar = kana[0];
          const fcClass = getCharClass(firstChar);

          const nanClasses = [
            'ba', 'bi', 'bu', 'be', 'bo',
            'pa', 'pi', 'pu', 'pe', 'po',
            'da', 'dji', 'dzu', 'de', 'do',
            'za', 'ji', 'zu', 'ze', 'zo',
            'ta', 'chi', 'tsu', 'te', 'to',
            'na', 'nu', 'ne', 'no',
            'ra', 'ri', 'ru', 're', 'ro'
          ];

          if (nanClasses.includes(fcClass)) {
            nan = true;
          } else {
            nani = true;
          }
        }
      }

      const naniKana = nan && nani ? 'なに' :
                      nan ? 'なん' :
                      nani ? 'なに' : null;

      if (naniKana) {
        wi.kana = naniKana;
      }
    }
  }

  return wiList;
}
