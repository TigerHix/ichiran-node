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
import { seq, alt, tok, rep, opt, mac } from './patternBuilders.js';

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

    // Deduplicate and prefer higher-preference outcomes
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
      return compileSequence(
        seq([
          opt(
            rep({
              pattern: alt([
                tok(['isNominalHead']),
                tok(['isNounModifier']),
                mac('AttributiveClause'),
                tok(['isNounConnector']),
                tok(['pos:pref']),
              ]),
              min: 1,
              max: 10,
            })
          ),
          tok(['isNounPhrase']),
        ])
      );
    }
    case 'NPCase': {
      return compileSequence(seq([mac('NP'), tok(['isCaseParticle'])]));
    }
    case 'NPTopic': {
      return compileSequence(seq([mac('NP'), tok(['isTopicMarker'])]));
    }
    case 'ClauseAdjunct': {
      return compileAlt(
        alt([
          tok(['isAdverbModifier']),
          mac('NPCase'),
          seq([mac('NPCase'), tok(['isTopicMarker'])]),
          mac('NPTopic'),
          tok(['pos:exp']),
        ])
      );
    }
    case 'PrePredicateAdjuncts': {
      return compileRepeat(
        rep({
          pattern: mac('ClauseAdjunct'),
          min: 0,
          max: 8,
        })
      );
    }
    case 'AttributiveClause': {
      // A shallow approximation of a relative/attributive clause directly modifying a following noun
      // repeat predicate-like tokens until the next token is a noun phrase head
      return compileRepeat(
        rep({
          pattern: tok(['isPredicate']),
          min: 1,
          max: 10,
          until: tok(['isNounPhrase']),
        })
      );
    }
    case 'NaAdjDe': {
      // な-adjective + で (copula conjunctive) connective
      // Excludes である (formal copula)
      return compileSequence(
        seq([
          tok(['isNaAdjective']),
          tok(['isCopulaDe']),
          { not: tok(['lemma:ある']) },
        ])
      );
    }
    case 'NaAdjDeshite': {
      // な-adjective + でして (formal copula conjunctive)
      // Polite/formal variant of で used in formal speech and writing
      return compileAlt(
        alt([
          // Single token: でして
          seq([
            tok(['isNaAdjective']),
            tok(['text:でして']),
          ]),
          // Split tokens: で + して
          seq([
            tok(['isNaAdjective']),
            tok(['text:で']),
            tok(['text:して']),
          ]),
        ])
      );
    }
    case 'NaAdjNegativeConnective': {
      // な-adjective negative connective: ではなくて / じゃなくて
      // Handles both formal (ではなくて) and casual (じゃなくて) forms
      return compileAlt(
        alt([
          // Formal: な-adj + ではなくて (as separate tokens or compound)
          seq([
            tok(['isNaAdjective']),
            alt([
              tok(['text:ではなくて']),
              seq([
                tok(['text:では']),
                tok(['text:なくて']),
              ]),
              seq([
                tok(['text:で']),
                tok(['text:は']),
                tok(['text:なくて']),
              ]),
            ]),
          ]),
          // Casual: な-adj + じゃなくて (as separate tokens or compound)
          seq([
            tok(['isNaAdjective']),
            alt([
              tok(['text:じゃなくて']),
              seq([
                tok(['text:じゃ']),
                tok(['text:なくて']),
              ]),
            ]),
          ]),
        ])
      );
    }
    case 'IAdjKu': {
      // い-adjective く-form (adverbial/stem)
      // Handles single token: よく, 早く, etc.
      return compileToken(tok(['isIAdjKuForm']));
    }
    case 'IAdjKute': {
      // い-adjective くて-form (conjunctive)
      // Handles both single token (よくて) and split (よく + て)
      return compileAlt(
        alt([
          tok(['isIAdjKuteForm']),
          seq([tok(['isIAdjKuForm']), tok(['text:て'])]),
        ])
      );
    }
    case 'IAdjKunai': {
      // い-adjective くない-form (negative)
      // Handles both single token (よくない) and split (よく + ない)
      // Also handles contrastive は: よくはない (not particularly...)
      return compileAlt(
        alt([
          tok(['isIAdjKunaiForm']),
          seq([
            tok(['isIAdjKuForm']),
            opt(tok(['text:は'])),  // contrastive は
            tok(['lemma:ない']),
          ]),
        ])
      );
    }
    case 'IAdjKunakute': {
      // い-adjective くなくて-form (negative conjunctive)
      // Handles various tokenization splits and optional contrastive は
      return compileAlt(
        alt([
          // Two tokens: く + なくて
          seq([tok(['isIAdjKuForm']), tok(['text:なくて'])]),
          // Split: く + [は] + ない + て
          seq([
            tok(['isIAdjKuForm']),
            opt(tok(['text:は'])),  // contrastive は
            tok(['lemma:ない']),
            tok(['text:て']),
          ]),
          // Split: く + [は] + なく + て
          seq([
            tok(['isIAdjKuForm']),
            opt(tok(['text:は'])),  // contrastive は
            tok(['text:なく']),
            tok(['text:て']),
          ]),
        ])
      );
    }
    case 'IAdjPoliteNegative': {
      // い-adjective polite negative: くないです or くありません
      // Handles both casual polite (くない + です) and formal (く + ありません)
      // Also allows contrastive は (くはない, くはありません)
      return compileAlt(
        alt([
          // Casual polite: くない[です/ある]
          seq([
            mac('IAdjKunai'),
            opt(alt([
              tok(['lemma:です']),
              tok(['lemmaNeg:ある']),  // ありません, ありませんでした (negative only)
            ])),
          ]),
          // Formal: く[は]ありません (negative forms of ある only)
          seq([
            mac('IAdjKu'),
            opt(tok(['text:は'])),
            tok(['lemmaNeg:ある']),  // ありません, ありませんでした, etc.
          ]),
        ])
      );
    }
    case 'IAdjKuSuru': {
      // い-adjective causative: くする/くします/くした/くして/くしない/くしません etc.
      // Matches only bare する verbs, NOT compound suru verbs like スライスする
      return compileSequence(seq([mac('IAdjKu'), tok(['lemma:する'])]));
    }
    case 'IAdjKuNaru': {
      // い-adjective + くなる (change of state: "become [adjective]")
      // Handles both separate tokens and compound forms
      return compileAlt(
        alt([
          seq([
            mac('IAdjKu'),
            tok(['isNaruVerb']),
          ]),
          tok(['isIAdjKuVerbCompound', 'isNaruVerb']),
        ])
      );
    }
    case 'NaAdjNi': {
      // な-adjective + に (adverbial form)
      // Used before verbs to describe manner/degree
      return compileSequence(
        seq([
          tok(['isNaAdjective']),
          tok(['text:に', 'isParticle']),
        ])
      );
    }
    case 'NaAdjNiNaru': {
      // な-adjective + になる (change of state: "become [adjective]")
      // Handles both separate tokens and compound expressions
      return compileAlt(
        alt([
          seq([
            tok(['isNaAdjective']),
            tok(['text:に', 'isParticle']),
            tok(['isNaruVerb']),
          ]),
          tok(['pos:exp', 'prefixHasPos:になる:adj-na']),
        ])
      );
    }
    case 'NaAdjNiSuru': {
      // な-adjective + にする (causative: "make [something] [adjective]")
      // Handles both separate tokens (な-adjective + に + する) and compound forms
      return compileAlt(
        alt([
          seq([
            tok(['isNaAdjective']),
            tok(['text:に', 'isParticle']),
            tok(['isSuruVerb']),
          ]),
          tok(['isSuruVerb', 'prefixHasPos:にする:adj-na']),
        ])
      );
    }
    default:
      throw new Error(`Unknown macro: ${node.macro}`);
  }
}
