import type { AnalysisPath, AnalysisToken } from './analyzer-service.js';
import { partOfSpeechLabel } from './dictionary-labels.js';

export interface AnalysisPathChoice {
  readonly index: number;
  readonly label: string;
}

function tokenPartOfSpeech(token: AnalysisToken): readonly string[] {
  if (token.entity) return ['Named Entity'];
  return [...new Set(token.pos.map(partOfSpeechLabel))];
}

function surfaceKey(path: AnalysisPath): string {
  return JSON.stringify(path.tokens.map(token => token.text));
}

function readingKey(path: AnalysisPath): string {
  return JSON.stringify(path.tokens.map(token => token.reading));
}

function consumerKey(path: AnalysisPath): string {
  return JSON.stringify(path.tokens.map(token => [
    token.text,
    token.reading,
    [...tokenPartOfSpeech(token)].sort(),
    token.entity
  ]));
}

function choiceLabel(path: AnalysisPath, paths: readonly AnalysisPath[]): string {
  const surfacePeers = paths.filter(candidate => surfaceKey(candidate) === surfaceKey(path));
  if (surfacePeers.length === 1) return path.tokens.map(token => token.text).join(' / ');

  const readingPeers = surfacePeers.filter(candidate => readingKey(candidate) === readingKey(path));
  return path.tokens.map((token, tokenIndex) => {
    const readings = new Set(surfacePeers.map(candidate => candidate.tokens[tokenIndex]!.reading));
    const partsOfSpeech = new Set(readingPeers.map(candidate => JSON.stringify([
      [...tokenPartOfSpeech(candidate.tokens[tokenIndex]!)].sort(),
      candidate.tokens[tokenIndex]!.entity
    ])));
    const details: string[] = [];
    if (readings.size > 1) details.push(token.reading || 'No reading');
    if (partsOfSpeech.size > 1) {
      const labels = tokenPartOfSpeech(token);
      details.push(labels.length > 0 ? labels.join(', ') : 'Unclassified');
    }
    return details.length > 0 ? `${token.text}（${details.join(' · ')}）` : token.text;
  }).join(' / ');
}

export function analysisPathChoices(
  paths: readonly AnalysisPath[],
  selectedPathIndex: number
): readonly AnalysisPathChoice[] {
  const representatives = new Map<string, number>();
  paths.forEach((path, index) => {
    const key = consumerKey(path);
    if (!representatives.has(key) || index === selectedPathIndex) representatives.set(key, index);
  });
  const indexes = [...representatives.values()];
  const visiblePaths = indexes.map(index => paths[index]!);
  return indexes.map((index, visibleIndex) => ({
    index,
    label: choiceLabel(visiblePaths[visibleIndex]!, visiblePaths)
  }));
}
