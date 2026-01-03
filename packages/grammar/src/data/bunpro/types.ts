export type BunproLevel = 'JLPT1' | 'JLPT2' | 'JLPT3' | 'JLPT4' | 'JLPT5' | 'Non-JLPT';

export type BunproStudySentence = {
  sentence: string;
  answer: string;
};

export type BunproGrammarItem = {
  id: string; // bunpro slug
  level: BunproLevel;
  title?: string;
  meaning?: string;
  answerForms: string[];
  sentences: BunproStudySentence[];
};

