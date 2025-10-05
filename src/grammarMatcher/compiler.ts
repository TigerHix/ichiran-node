import type {
  PatternNode,
  Token,
  PredicateContext,
  MatchOutcome,
  SequenceNode,
  AltNode,
  TokenNode,
  RepeatNode,
  OptionalNode,
  CaptureNode,
  PeekNode,
  NotNode,
  AnchorNode,
  MacroNode,
} from './types.js';
import { evaluatePredicates, resolvePredicate } from './predicates.js';

export type Matcher = (tokens: Token[], index: number) => Promise<MatchOutcome[]>;

function escapeKeyPart(part: string): string {
  return part.replace(/([@|\\:])/g, '\\$1');
}

function captureSignature(capture: MatchOutcome['captures'][number]): string {
  return `${escapeKeyPart(capture.label)}@${capture.start}-${capture.end}`;
}

function normalizeOutcomeKey(outcome: MatchOutcome): string {
  const captureKey = outcome.captures.map(captureSignature).join('|');
  return `${outcome.index}:${captureKey}`;
}

export function compilePattern(node: PatternNode): Matcher {
  if ('sequence' in node) return compileSequence(node);
  if ('alt' in node) return compileAlt(node);
  if ('token' in node) return compileToken(node);
  if ('repeat' in node) return compileRepeat(node);
  if ('optional' in node) return compileOptional(node);
  if ('capture' in node) return compileCapture(node);
  if ('peek' in node) return compilePeek(node);
  if ('not' in node) return compileNot(node);
  if ('anchor' in node) return compileAnchor(node);
  if ('macro' in node) return compileMacro(node);
  throw new Error(`Unsupported pattern node: ${JSON.stringify(node)}`);
}

function compileSequence(node: SequenceNode): Matcher {
  const matchers = node.sequence.map(compilePattern);
  return async (tokens, index) => {
    const outcomes: MatchOutcome[] = [];

    async function dfs(depth: number, currentIndex: number, captures: MatchOutcome['captures'], preference: number): Promise<void> {
      if (depth === matchers.length) {
        outcomes.push({ index: currentIndex, captures, preference });
        return;
      }

      const matcher = matchers[depth];
      const results = await matcher(tokens, currentIndex);
      for (const result of results) {
        await dfs(
          depth + 1,
          result.index,
          captures.concat(result.captures),
          preference + (result.preference ?? 0),
        );
      }
    }

    await dfs(0, index, [], 0);

    return outcomes;
  };
}

function compileAlt(node: AltNode): Matcher {
  const matchers = node.alt.map(compilePattern);
  return async (tokens, index) => {
    const outcomes: MatchOutcome[] = [];
    for (const matcher of matchers) {
      outcomes.push(...await matcher(tokens, index));
    }
    return outcomes;
  };
}

function compileToken(node: TokenNode): Matcher {
  const predicates = node.token.map(resolvePredicate);
  return async (tokens, index) => {
    const token = tokens[index];
    if (!token) return [];
    const ctx: PredicateContext = {
      tokens,
      index,
      prev: tokens[index - 1],
      next: tokens[index + 1],
    };
    const passed = await evaluatePredicates(token, ctx, predicates);
    return passed
      ? [{ index: index + 1, captures: [], preference: 0 }]
      : [];
  };
}

function compileRepeat(node: RepeatNode): Matcher {
  const { pattern, min = 0, max = 4, greedy = true, until } = node.repeat;
  const matcher = compilePattern(pattern);
  const untilMatcher = until ? compilePattern(until) : null;
  return async (tokens, index) => {
    const outcomes: MatchOutcome[] = [];
    const basePreference = greedy ? 1 : -1;

    async function dfs(currentIndex: number, count: number, captures: MatchOutcome['captures'], preference: number): Promise<void> {
      if (count >= min) {
        outcomes.push({ index: currentIndex, captures: captures.slice(), preference });
      }
      if (count === max) return;

      // Stop repeating if the until lookahead matches at the current position
      if (untilMatcher && (await untilMatcher(tokens, currentIndex)).length > 0) {
        return;
      }

      const matches = await matcher(tokens, currentIndex);
      for (const match of matches) {
        if (match.index === currentIndex) continue;
        const combinedPreference = preference + (match.preference ?? 0);
        await dfs(match.index, count + 1, captures.concat(match.captures), combinedPreference);
      }
    }

    await dfs(index, 0, [], basePreference);

    outcomes.sort((a, b) => {
      const idxDiff = a.index - b.index;
      if (idxDiff !== 0) return greedy ? -idxDiff : idxDiff;
      const capDiff = a.captures.length - b.captures.length;
      return greedy ? -capDiff : capDiff;
    });

    const seen = new Map<string, number>();
    const deduped: MatchOutcome[] = [];
    for (const outcome of outcomes) {
      const key = normalizeOutcomeKey(outcome);
      const existingIndex = seen.get(key);
      if (existingIndex === undefined) {
        deduped.push(outcome);
        seen.set(key, deduped.length - 1);
        continue;
      }

      const existing = deduped[existingIndex];
      if ((outcome.preference ?? 0) > (existing.preference ?? 0)) {
        deduped[existingIndex] = outcome;
      }
    }

    return deduped;
  };
}

function compileOptional(node: OptionalNode): Matcher {
  const matcher = compilePattern(node.optional);
  return async (tokens, index) => [{ index, captures: [], preference: 0 }, ...await matcher(tokens, index)];
}

function compileCapture(node: CaptureNode): Matcher {
  const matcher = compilePattern(node.pattern);
  const label = node.capture;
  return async (tokens, index) => {
    const results = await matcher(tokens, index);
    return results.map(result => ({
      index: result.index,
      preference: result.preference,
      captures: result.captures.concat({
        label,
        start: index,
        end: result.index,
        tokens: tokens.slice(index, result.index),
      }),
    }));
  };
}

function compilePeek(node: PeekNode): Matcher {
  const matcher = compilePattern(node.peek);
  return async (tokens, index) => ((await matcher(tokens, index)).length > 0 ? [{ index, captures: [], preference: 0 }] : []);
}

function compileNot(node: NotNode): Matcher {
  const matcher = compilePattern(node.not);
  return async (tokens, index) => ((await matcher(tokens, index)).length === 0 ? [{ index, captures: [], preference: 0 }] : []);
}

function compileAnchor(node: AnchorNode): Matcher {
  return async (tokens, index) => {
    if (node.anchor === 'start') {
      return index === 0 ? [{ index, captures: [], preference: 0 }] : [];
    }
    if (node.anchor === 'end') {
      return index === tokens.length ? [{ index, captures: [], preference: 0 }] : [];
    }
    return [];
  };
}

// Built-in macros. Keep minimal, reusable across grammars
function compileMacro(node: MacroNode): Matcher {
  switch (node.macro) {
    case 'NP': {
      // Reusable noun phrase: optional components, then a terminating NP token
      // sequence: [ repeat(alt(head|modifier|connector|pref)), isNounPhrase ]
      const seq = {
        sequence: [
          {
            optional: {
              repeat: {
                pattern: {
                  alt: [
                    { token: ['isNominalHead'] },
                    { token: ['isNounModifier'] },
                    { macro: 'AttributiveClause' } as any,
                    { token: ['isNounConnector'] },
                    { token: ['pos:pref'] },
                  ],
                },
                min: 1,
                max: 10,
              },
            },
          },
          { token: ['isNounPhrase'] },
        ],
      } as SequenceNode;
      return compileSequence(seq);
    }
    case 'NPCase': {
      const seq = { sequence: [ { macro: 'NP' } as any, { token: ['isCaseParticle'] } ] } as SequenceNode;
      return compileSequence(seq);
    }
    case 'NPTopic': {
      const seq = { sequence: [ { macro: 'NP' } as any, { token: ['isTopicMarker'] } ] } as SequenceNode;
      return compileSequence(seq);
    }
    case 'ClauseAdjunct': {
      const npCaseThenTopic = { sequence: [ { macro: 'NPCase' } as any, { token: ['isTopicMarker'] } ] } as SequenceNode;
      const alt = {
        alt: [
          { token: ['isAdverbModifier'] },
          { macro: 'NPCase' } as any,
          npCaseThenTopic,
          { macro: 'NPTopic' } as any,
          { token: ['pos:exp'] },
        ],
      } as AltNode;
      return compileAlt(alt);
    }
    case 'PrePredicateAdjuncts': {
      const rep = {
        repeat: {
          pattern: { macro: 'ClauseAdjunct' } as any,
          min: 0,
          max: 8,
        },
      } as RepeatNode;
      return compileRepeat(rep);
    }
    case 'AttributiveClause': {
      // A shallow approximation of a relative/attributive clause directly modifying a following noun
      // repeat predicate-like tokens until the next token is a noun phrase head
      const rep = {
        repeat: {
          pattern: { token: ['isPredicate'] } as any,
          min: 1,
          max: 10,
          until: { token: ['isNounPhrase'] } as any,
        },
      } as RepeatNode;
      return compileRepeat(rep);
    }
    case 'NaAdjDe': {
      // な-adjective + で (copula conjunctive) connective
      // Excludes である (formal copula)
      const seq = {
        sequence: [
          { token: ['isNaAdjective'] },
          { token: ['isCopulaDe'] },
          { not: { token: ['lemma:ある'] } },
        ],
      } as SequenceNode;
      return compileSequence(seq);
    }
    case 'IAdjKu': {
      // い-adjective く-form (adverbial/stem)
      // Handles single token: よく, 早く, etc.
      return compileToken({ token: ['isIAdjKuForm'] } as TokenNode);
    }
    case 'IAdjKute': {
      // い-adjective くて-form (conjunctive)
      // Handles both single token (よくて) and split (よく + て)
      const alt = {
        alt: [
          { token: ['isIAdjKuteForm'] },
          {
            sequence: [
              { token: ['isIAdjKuForm'] },
              { token: ['text:て'] },
            ],
          },
        ],
      } as AltNode;
      return compileAlt(alt);
    }
    case 'IAdjKunai': {
      // い-adjective くない-form (negative)
      // Handles both single token (よくない) and split (よく + ない)
      // Also handles contrastive は: よくはない (not particularly...)
      const alt = {
        alt: [
          { token: ['isIAdjKunaiForm'] },
          {
            sequence: [
              { token: ['isIAdjKuForm'] },
              { optional: { token: ['text:は'] } },  // contrastive は
              { token: ['lemma:ない'] },
            ],
          },
        ],
      } as AltNode;
      return compileAlt(alt);
    }
    case 'IAdjKunakute': {
      // い-adjective くなくて-form (negative conjunctive)
      // Handles various tokenization splits
      const alt = {
        alt: [
          // Single token くなくて (rare but possible)
          {
            sequence: [
              { token: ['isIAdjKuForm'] },
              { token: ['text:なくて'] },
            ],
          },
          // Split: く + ない + て
          {
            sequence: [
              { token: ['isIAdjKuForm'] },
              { token: ['lemma:ない'] },
              { token: ['text:て'] },
            ],
          },
          // Split: く + なくて
          {
            sequence: [
              { token: ['isIAdjKuForm'] },
              { token: ['text:なく'] },
              { token: ['text:て'] },
            ],
          },
        ],
      } as AltNode;
      return compileAlt(alt);
    }
    case 'IAdjPoliteNegative': {
      // い-adjective polite negative: くないです or くありません
      // Handles both casual polite (くない + です) and formal (く + ありません)
      // Also allows contrastive は (くはない, くはありません)
      const alt = {
        alt: [
          // Casual polite: くない[です/ある]
          {
            sequence: [
              { macro: 'IAdjKunai' } as any,
              {
                optional: {
                  alt: [
                    { token: ['lemma:です'] },
                    { token: ['lemma:ある'] },  // Covers ありません, ありませんでした, etc.
                  ],
                },
              },
            ],
          },
          // Formal: く[は]ありません
          {
            sequence: [
              { macro: 'IAdjKu' } as any,
              { optional: { token: ['text:は'] } },
              { token: ['lemma:ある'] },  // Covers ありません, ありませんでした, etc.
            ],
          },
        ],
      } as AltNode;
      return compileAlt(alt);
    }
    case 'IAdjKuSuru': {
      // い-adjective causative: くする/くします/くした/くして/くしない/くしません etc.
      // Matches only bare する verbs, NOT compound suru verbs like スライスする
      const seq = {
        sequence: [
          { macro: 'IAdjKu' } as any,
          { token: ['lemma:する'] },
        ],
      } as SequenceNode;
      return compileSequence(seq) as Matcher;
    }
    default:
      throw new Error(`Unknown macro: ${node.macro}`);
  }
}
