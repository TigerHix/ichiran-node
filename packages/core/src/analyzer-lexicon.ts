import type {
  PortableAnalysisInflection,
  PortableAnalysisRoot
} from './analyzer-result.js';
import type { PortableAnalyzerAnnotations } from './analyzer-source.js';
import type {
  AnalyzerGeneratedFacts,
  AnalyzerGeneratedMember
} from './analyzer-annotations.js';
import {
  EMPTY_SEQUENCE_FACTS,
  analyzerConjugation,
  dedupeCandidates,
  inflectionProperty,
  positions,
  sequenceFacts,
  union,
  withKey,
  type CandidateSemanticMember,
  type MaterializedCandidate
} from './analyzer-candidate.js';
import type {
  AnalyzerSequenceFacts,
  AnalyzerWordScoreFacts
} from './analyzer-types.js';
import { asHiragana } from './characters.js';
import { MorphologyReader, type MorphologyCandidate } from './morphology.js';
import { RootPayloadReader } from './root-payload.js';
import { AnalyzerSupportReader, type AnalyzerSupportRoute } from './analyzer-support.js';
import { SurfaceIndex, type SurfaceMatch } from './surface-index.js';

export interface AnalyzerLexiconSource {
  readonly surface: SurfaceIndex;
  readonly roots: RootPayloadReader;
  readonly morphology: MorphologyReader;
  readonly support: AnalyzerSupportReader;
  readonly annotations: PortableAnalyzerAnnotations;
}

type ScoreSplitResolver = (
  definitionSeq: number,
  route: AnalyzerSupportRoute,
  surface: string
) => AnalyzerWordScoreFacts['split'];

/** Dictionary and morphology materialization for one analyzer request. */
export class AnalyzerLexicon {
  readonly #surface: SurfaceIndex;
  readonly #roots: RootPayloadReader;
  readonly #morphology: MorphologyReader;
  readonly #support: AnalyzerSupportReader;
  readonly #annotations: PortableAnalyzerAnnotations;
  readonly #scoreSplit: ScoreSplitResolver;
  readonly #lexicalCache = new Map<string, readonly MaterializedCandidate[]>();
  readonly #rootFormCache = new Map<string, number | null>();

  constructor(source: AnalyzerLexiconSource, scoreSplit: ScoreSplitResolver) {
    this.#surface = source.surface;
    this.#roots = source.roots;
    this.#morphology = source.morphology;
    this.#support = source.support;
    this.#annotations = source.annotations;
    this.#scoreSplit = scoreSplit;
  }

  reset(): void {
    this.#lexicalCache.clear();
    this.#rootFormCache.clear();
  }

  lexical(surface: string, known?: SurfaceMatch | null): MaterializedCandidate[] {
    const cached = this.#lexicalCache.get(surface);
    if (cached) return [...cached];
    const match = known === undefined ? this.#surface.lookup(surface) : known;
    if (!match) return [];
    const result: MaterializedCandidate[] = [];
    if (match.direct && match.directRank !== null) {
      const start = this.#roots.surfaceFormStart(match.directRank);
      const count = this.#roots.surfaceFormCount(match.directRank);
      for (let offset = 0; offset < count; offset++) {
        result.push(this.#direct(surface, start + offset));
      }
    }
    let hasMorphologyCandidate = false;
    if (match.morphology) {
      const morphology = this.#morphology.lookup(surface, match.route);
      hasMorphologyCandidate = morphology.length > 0;
      for (const value of morphology) result.push(this.#morph(value));
    }
    const grouped = this.#groupPhysical(dedupeCandidates(result));
    const ordered = hasMorphologyCandidate && grouped.length > 1
      ? this.#sortLookupOrder(match.route, surface, grouped)
      : grouped;
    this.#lexicalCache.set(surface, ordered);
    return [...ordered];
  }

  memberSequenceFacts(
    allMembers: readonly CandidateSemanticMember[],
    selectedMembers: readonly CandidateSemanticMember[]
  ): AnalyzerSequenceFacts[] {
    const archivedIntermediates = new Set(allMembers.flatMap(member => {
      if (member.viaSeq === null || member.entryIndex === null) return [];
      return this.#roots.entryArchived(member.entryIndex) ? [member.viaSeq] : [];
    }));
    return selectedMembers.map(member => {
      const facts = sequenceFacts(this.#roots, member.entryIndex);
      const rootSeq = member.root?.seq ?? member.publicSeq;
      return rootSeq !== null && archivedIntermediates.has(rootSeq)
        ? { ...facts, allArchived: true }
        : facts;
    });
  }

  katakanaProxy(
    surface: string,
    existing: readonly MaterializedCandidate[]
  ): MaterializedCandidate[] {
    const hiragana = asHiragana(surface);
    if (hiragana === surface) return [];
    const excluded = new Set(existing.map(value => value.publicSeq));
    return this.lexical(hiragana)
      .filter(value => value.kind === 'simple'
        && value.inflection.length === 0
        && !excluded.has(value.publicSeq))
      .map(source => {
        const base = source.scoreFacts as AnalyzerWordScoreFacts;
        return withKey({
          ...source,
          kind: 'proxy' as const,
          text: surface,
          reading: surface,
          scoreFacts: {
            ...base,
            text: surface,
            trueText: base.trueText,
            trueTextFollowsText: false
          },
          components: []
        });
      });
  }

  #sortLookupOrder(
    route: AnalyzerSupportRoute,
    surface: string,
    values: readonly MaterializedCandidate[]
  ): MaterializedCandidate[] {
    const lookupOrder = this.#annotations.lookupOrder?.bind(this.#annotations);
    if (!lookupOrder) return [...values];
    const ranked = values.map((value, index) => {
      if (value.lookupLocators.length === 0) {
        throw new Error(`Incomplete analyzer lookup order for ${JSON.stringify(surface)}`);
      }
      const ranks = new Set<number>();
      for (const locator of value.lookupLocators) {
        const rank = lookupOrder(route, surface, locator.rootSeq, locator.aliases);
        if (rank === null) {
          throw new Error(
            `Incomplete analyzer lookup order for ${JSON.stringify(surface)} at `
            + `${locator.rootSeq}:${locator.aliases?.join(',') ?? 'direct'}`
          );
        }
        ranks.add(rank);
      }
      if (ranks.size !== 1) {
        throw new Error(`Physical analyzer group has conflicting lookup orders: ${[...ranks].join(', ')}`);
      }
      return { value, index, rank: ranks.values().next().value! };
    });
    return ranked
      .sort((left, right) => left.rank - right.rank || left.index - right.index)
      .map(({ value }) => value);
  }

  #lookupLocators(values: readonly MaterializedCandidate[]): MaterializedCandidate['lookupLocators'] {
    const seen = new Set<string>();
    const result: Array<MaterializedCandidate['lookupLocators'][number]> = [];
    for (const value of values) {
      for (const locator of value.lookupLocators) {
        const key = `${locator.rootSeq}\u0000${locator.aliases?.join(',') ?? ''}`;
        if (seen.has(key)) continue;
        seen.add(key);
        result.push(locator);
      }
    }
    return result;
  }

  #groupPhysical(values: readonly MaterializedCandidate[]): MaterializedCandidate[] {
    const groups = new Map<string, MaterializedCandidate[]>();
    values.forEach((value, index) => {
      const key = value.physicalGroup !== null
        ? `group:${value.physicalGroup}`
        : value.physicalKey.length > 0
          ? value.physicalKey
          : `unique:${index}`;
      const members = groups.get(key) ?? [];
      members.push(value);
      groups.set(key, members);
    });
    return [...groups.values()].map(group => group.length === 1
      ? group[0]!
      : this.#mergePhysical(group));
  }

  #mergePhysical(values: readonly MaterializedCandidate[]): MaterializedCandidate {
    const ordered = [...values].sort((left, right) => {
      const leftDirect = left.inflection.length === 0 ? 0 : 1;
      const rightDirect = right.inflection.length === 0 ? 0 : 1;
      return leftDirect - rightDirect
        || (left.memberOrd ?? Number.MAX_SAFE_INTEGER)
          - (right.memberOrd ?? Number.MAX_SAFE_INTEGER);
    });
    const base = ordered[0]!;
    const lookupLocators = this.#lookupLocators(values);
    const wordValues = ordered.filter((value): value is MaterializedCandidate & {
      readonly scoreFacts: AnalyzerWordScoreFacts;
    } => value.scoreFacts.kind !== 'compound');
    const wordFacts = wordValues.map(value => value.scoreFacts);
    if (wordFacts.length !== ordered.length) return base;

    const allConjugations = wordFacts.flatMap(value => value.conjugations);
    const secondaryOnly = allConjugations.length > 0
      && allConjugations.every(value => value.via !== null);
    const selectedWordValues = wordValues.filter(value =>
      value.scoreFacts.conjugations.some(conjugation =>
        secondaryOnly ? conjugation.via !== null : conjugation.via === null));
    const targetValue = wordValues.find(value => value.inflection.length === 0) ?? null;
    const scoringWordValues = [
      ...(targetValue ? [targetValue] : []),
      ...selectedWordValues
    ];
    const scoringWordFacts = scoringWordValues.map(value => value.scoreFacts);

    const compareCommon = (left: number, right: number): number =>
      left === 0 ? -1 : right === 0 ? 1 : left - right;
    const inheritedCommon = selectedWordValues.map(value => value.scoreFacts)
      .flatMap(value => value.inheritedCommon === null ? [] : [value.inheritedCommon])
      .sort(compareCommon)[0] ?? null;
    const inheritedOrd = Math.min(...selectedWordValues.map(value => value.scoreFacts)
      .flatMap(value => value.inheritedOrd === null ? [] : [value.inheritedOrd]));
    const baseFacts = wordFacts[0]!;
    const entries = wordFacts.flatMap(value => value.entry ? [value.entry] : []);

    const allMembers = ordered.flatMap(value => value.semanticMembers);
    const selectedMembers = ordered
      .flatMap(value => value.semanticMembers)
      .filter(member => member.inflection.length > 0
        && (secondaryOnly
          ? member.inflection.length > 1
          : member.inflection.length === 1));
    const selectedSequenceFacts = this.memberSequenceFacts(allMembers, selectedMembers);
    const rawRootFacts = ordered
      .flatMap(value => value.semanticMembers)
      .filter(member => member.inflection.length > 0)
      .map(member => sequenceFacts(this.#roots, member.entryIndex));
    const targetFacts = targetValue?.scoreFacts.self ?? baseFacts.self;
    const self: AnalyzerSequenceFacts = {
      allArchived: targetFacts.allArchived
        || rawRootFacts.some(value => value.allArchived),
      preferKana: targetFacts.preferKana,
      preferKanaOnOrdinalZero: targetFacts.preferKanaOnOrdinalZero
    };
    const lineage: AnalyzerSequenceFacts = {
      allArchived: self.allArchived
        && selectedSequenceFacts.length > 0
        && selectedSequenceFacts.every(value => value.allArchived),
      preferKana: self.preferKana
        || selectedSequenceFacts.some(value => value.preferKana),
      preferKanaOnOrdinalZero: self.preferKanaOnOrdinalZero
        || selectedSequenceFacts.some(value => value.preferKanaOnOrdinalZero)
    };
    const scoreFacts: AnalyzerWordScoreFacts = {
      ...baseFacts,
      seq: base.physicalSeq,
      ord: Math.min(...wordFacts.map(value => value.ord)),
      common: targetValue?.scoreFacts.common ?? null,
      nokanji: targetValue?.scoreFacts.nokanji ?? baseFacts.nokanji,
      entry: entries.length === 0 ? null : {
        root: entries.some(value => value.root),
        nKanji: entries[0]!.nKanji,
        primaryNokanji: entries.some(value => value.primaryNokanji)
      },
      conjugationOnly: wordFacts.every(value => value.conjugationOnly),
      conjugations: allConjugations,
      positions: union(...scoringWordFacts.map(value => value.positions)),
      self,
      lineage,
      inheritedCommon,
      inheritedOrd: Number.isFinite(inheritedOrd) ? inheritedOrd : null,
      split: wordFacts.find(value => value.split !== null)?.split ?? null
    };
    const seenMembers = new Set<string>();
    const semanticMembers = ordered
      .flatMap(value => value.semanticMembers)
      .sort((left, right) =>
        (left.memberOrd ?? Number.MAX_SAFE_INTEGER)
          - (right.memberOrd ?? Number.MAX_SAFE_INTEGER))
      .filter(member => {
        const key = JSON.stringify([
          member.publicSeq,
          member.inflection,
          member.stageGroups,
          member.stageKeys ?? [],
          member.stageMemberOrds,
          member.stagePropOrds
        ]);
        if (seenMembers.has(key)) return false;
        seenMembers.add(key);
        return true;
      });
    return withKey({
      ...base,
      physicalGroup: ordered.find(value => value.physicalGroup !== null)?.physicalGroup ?? null,
      lookupLocators,
      memberOrd: ordered
        .flatMap(value => value.memberOrd === null ? [] : [value.memberOrd])
        .sort((left, right) => left - right)[0] ?? null,
      scoreFacts,
      semanticMembers,
      identityRoots: [...new Set(semanticMembers.flatMap(member =>
        member.root === null ? [] : [member.root.seq]))]
    });
  }

  #direct(surface: string, form: number): MaterializedCandidate {
    const entryIndex = this.#roots.formEntryIndex(form);
    const seq = this.#roots.entrySeq(entryIndex);
    const route = this.#roots.formRoute(form);
    const best = this.#roots.resolveSurfaceReference(
      this.#roots.formBestReference(form),
      value => this.#surface.directSurface(value)
    );
    const root = route === 'kanji'
      ? { seq, form: surface, reading: best ?? surface }
      : { seq, form: best ?? surface, reading: surface };
    const facts = sequenceFacts(this.#roots, entryIndex);
    const split = this.#scoreSplit(seq, route, surface);
    const word: AnalyzerWordScoreFacts = {
      kind: 'word', text: surface, trueText: surface, trueTextFollowsText: true,
      route, seq, ord: this.#roots.formOrdinal(form), common: this.#roots.formCommon(form),
      nokanji: this.#roots.formNokanji(form),
      entry: {
        root: true,
        nKanji: this.#roots.entryNKanji(entryIndex),
        primaryNokanji: this.#roots.entryPrimaryNokanji(entryIndex)
      },
      conjugationOnly: false, conjugations: [], positions: positions(this.#roots, entryIndex),
      self: facts, lineage: facts, inheritedCommon: null, inheritedOrd: null,
      split, suruBreak: null
    };
    let reading = root.reading;
    reading = this.#annotations.hint(seq, route, surface, reading) ?? reading;
    return withKey({
      kind: 'simple', text: surface, trueText: surface, route, reading,
      publicSeq: seq, physicalSeq: seq, physicalKey: `seq:${seq}`,
      physicalGroup: null, lookupLocators: [{ rootSeq: seq, aliases: null }],
      memberOrd: null, entryIndex, root, inflection: [], scoreFacts: word,
      components: [], counter: null, suffixClass: this.#support.suffixClass(seq),
      definitionSeq: seq, conjugationSelection: 'default',
      semanticMembers: [{
        entryIndex, root, inflection: [], publicSeq: seq, physicalGroup: null,
        memberOrd: null, targetNKanji: this.#roots.entryNKanji(entryIndex),
        targetNKana: this.#roots.entryNKana(entryIndex), viaSeq: null,
        stageGroups: [], stageMemberOrds: [], stagePropOrds: []
      }]
    });
  }

  #morph(value: MorphologyCandidate): MaterializedCandidate {
    const entryIndex = this.#roots.findEntryIndex(value.rootSeq);
    const collision = this.#support.collision(
      value.rootSeq, value.route, value.surface, value.ruleIds
    );
    let generated: AnalyzerGeneratedFacts | null = null;
    let generatedVia: AnalyzerGeneratedFacts | null = null;
    const aliases = this.#support.generatedAliases(value.ruleIds);
    if (collision === null && this.#annotations.generated) {
      generated = this.#annotations.generated(value.rootSeq, aliases);
      if (aliases.length === 2) {
        generatedVia = this.#annotations.generated(value.rootSeq, [aliases[0]]);
      }
    }
    const physicalSeq = collision?.collisionSeq ?? -value.rootSeq;
    const rootFacts = sequenceFacts(this.#roots, entryIndex >= 0 ? entryIndex : null);
    const self = collision ? {
      allArchived: collision.archived,
      preferKana: collision.preferKana,
      preferKanaOnOrdinalZero: collision.preferKanaOnOrdinalZero
    } : { ...EMPTY_SEQUENCE_FACTS, allArchived: rootFacts.allArchived };
    const lineage = collision ? {
      allArchived: self.allArchived && rootFacts.allArchived,
      preferKana: self.preferKana || rootFacts.preferKana,
      preferKanaOnOrdinalZero: self.preferKanaOnOrdinalZero || rootFacts.preferKanaOnOrdinalZero
    } : rootFacts;
    const rootPos = positions(this.#roots, entryIndex >= 0 ? entryIndex : null);
    const sourceForm = this.#rootForm(value.sourceText, value.rootSeq, value.route);
    const fallbackInflection = value.path.map(inflectionProperty);
    const root: PortableAnalysisRoot = {
      seq: value.rootSeq,
      form: value.sourceForm,
      reading: value.sourceReading
    };
    const targetNKanji = collision?.nKanji
      ?? generated?.nKanji
      ?? (entryIndex >= 0 ? this.#roots.entryNKanji(entryIndex) : null);
    const targetNKana = collision?.nKana
      ?? generated?.nKana
      ?? (entryIndex >= 0 ? this.#roots.entryNKana(entryIndex) : null);
    const exactProperty = (
      member: AnalyzerGeneratedMember,
      fallback: PortableAnalysisInflection
    ): PortableAnalysisInflection => ({
      pos: this.#morphology.position(member.property.posId),
      type: member.property.type,
      negative: member.property.negative,
      formal: member.property.formal,
      ordinal: fallback.ordinal
    });
    const semanticStageKey = (stageAliases: readonly number[]): string =>
      `${value.rootSeq}:${stageAliases.join(',')}`;
    const finalStageKey = semanticStageKey(aliases);
    const prefixStageKey = aliases.length === 2 ? semanticStageKey([aliases[0]]) : null;
    const prefixCollision = value.ruleIds.length === 2 && value.intermediate !== null
      ? this.#support.collision(
          value.rootSeq, value.route, value.intermediate, [value.ruleIds[0]]
        )
      : null;
    const viaSeq = prefixCollision?.collisionSeq ?? null;
    const semanticMembers: CandidateSemanticMember[] = [];
    const finalMembers = collision === null ? generated?.members ?? null : null;
    if (finalMembers && finalMembers.length > 0) {
      for (const finalMember of finalMembers) {
        const finalProperty = exactProperty(finalMember, fallbackInflection.at(-1)!);
        if (fallbackInflection.length === 1) {
          semanticMembers.push({
            entryIndex: entryIndex >= 0 ? entryIndex : null, root,
            inflection: [finalProperty], publicSeq: value.rootSeq,
            physicalGroup: generated?.physicalGroup ?? null, memberOrd: finalMember.memberOrd,
            targetNKanji, targetNKana, viaSeq: null,
            stageGroups: [generated?.physicalGroup ?? null], stageKeys: [finalStageKey],
            stageMemberOrds: [finalMember.memberOrd], stagePropOrds: [finalMember.propOrd]
          });
          continue;
        }
        const prefixMembers = (generatedVia?.members ?? []).filter(prefix =>
          prefix.memberOrd === finalMember.viaMemberOrd);
        if (prefixMembers.length === 0) {
          semanticMembers.push({
            entryIndex: entryIndex >= 0 ? entryIndex : null, root,
            inflection: [fallbackInflection[0]!, finalProperty], publicSeq: value.rootSeq,
            physicalGroup: generated?.physicalGroup ?? null, memberOrd: finalMember.memberOrd,
            targetNKanji, targetNKana, viaSeq,
            stageGroups: [generatedVia?.physicalGroup ?? null, generated?.physicalGroup ?? null],
            stageKeys: [prefixStageKey, finalStageKey],
            stageMemberOrds: [finalMember.viaMemberOrd, finalMember.memberOrd],
            stagePropOrds: [null, finalMember.propOrd]
          });
          continue;
        }
        for (const prefixMember of prefixMembers) {
          semanticMembers.push({
            entryIndex: entryIndex >= 0 ? entryIndex : null, root,
            inflection: [exactProperty(prefixMember, fallbackInflection[0]!), finalProperty],
            publicSeq: value.rootSeq, physicalGroup: generated?.physicalGroup ?? null,
            memberOrd: finalMember.memberOrd, targetNKanji, targetNKana, viaSeq,
            stageGroups: [generatedVia?.physicalGroup ?? null, generated?.physicalGroup ?? null],
            stageKeys: [prefixStageKey, finalStageKey],
            stageMemberOrds: [prefixMember.memberOrd, finalMember.memberOrd],
            stagePropOrds: [prefixMember.propOrd, finalMember.propOrd]
          });
        }
      }
    } else if (
      fallbackInflection.length === 2
      && generatedVia?.members
      && generatedVia.members.length > 0
    ) {
      for (const prefixMember of generatedVia.members) {
        semanticMembers.push({
          entryIndex: entryIndex >= 0 ? entryIndex : null, root,
          inflection: [exactProperty(prefixMember, fallbackInflection[0]!), fallbackInflection[1]!],
          publicSeq: value.rootSeq, physicalGroup: generated?.physicalGroup ?? null,
          memberOrd: null, targetNKanji, targetNKana, viaSeq,
          stageGroups: [generatedVia.physicalGroup, generated?.physicalGroup ?? null],
          stageKeys: [prefixStageKey, finalStageKey], stageMemberOrds: [prefixMember.memberOrd, 0],
          stagePropOrds: [prefixMember.propOrd, 0]
        });
      }
    } else {
      semanticMembers.push({
        entryIndex: entryIndex >= 0 ? entryIndex : null, root,
        inflection: fallbackInflection, publicSeq: value.rootSeq,
        physicalGroup: generated?.physicalGroup ?? null, memberOrd: null,
        targetNKanji, targetNKana, viaSeq: fallbackInflection.length === 2 ? viaSeq : null,
        stageGroups: fallbackInflection.length === 1
          ? [generated?.physicalGroup ?? null]
          : [generatedVia?.physicalGroup ?? null, generated?.physicalGroup ?? null],
        stageKeys: fallbackInflection.length === 1
          ? [finalStageKey]
          : [prefixStageKey, finalStageKey],
        stageMemberOrds: fallbackInflection.map(() => 0),
        stagePropOrds: fallbackInflection.map(() => 0)
      });
    }
    const inflection = semanticMembers[0]?.inflection ?? fallbackInflection;
    const conjugations = semanticMembers.map(member => analyzerConjugation(
      physicalSeq, value.rootSeq, member.inflection.at(-1)!, member.inflection.length > 1
    ));
    const split = collision
      ? this.#scoreSplit(collision.collisionSeq, value.route, value.surface)
        ?? this.#scoreSplit(value.rootSeq, value.route, value.surface)
      : this.#scoreSplit(value.rootSeq, value.route, value.surface);
    const word: AnalyzerWordScoreFacts = {
      kind: 'word', text: value.surface, trueText: value.surface,
      trueTextFollowsText: true, route: value.route, seq: physicalSeq,
      ord: value.ord, common: null,
      nokanji: sourceForm === null
        ? value.route === 'kana' && value.sourceForm === value.sourceReading
        : this.#roots.formNokanji(sourceForm),
      entry: {
        root: collision !== null,
        nKanji: targetNKanji ?? 0,
        primaryNokanji: collision?.primaryNokanji ?? false
      },
      conjugationOnly: true, conjugations,
      positions: union(
        rootPos,
        collision?.pos ?? [],
        semanticMembers.flatMap(member => member.inflection.map(property => property.pos))
      ),
      self, lineage, inheritedCommon: value.common, inheritedOrd: value.ord,
      split, suruBreak: null
    };
    let reading = value.reading;
    reading = this.#annotations.hint(
      collision?.collisionSeq ?? value.rootSeq,
      value.route,
      value.surface,
      reading
    ) ?? reading;
    return withKey({
      kind: 'simple', text: value.surface, trueText: value.surface, route: value.route,
      reading, publicSeq: value.rootSeq, physicalSeq,
      physicalKey: collision
        ? `seq:${collision.collisionSeq}`
        : `semantic:${value.rootSeq}:${aliases.join(',')}`,
      physicalGroup: generated?.physicalGroup ?? null,
      lookupLocators: [collision
        ? { rootSeq: collision.collisionSeq, aliases: null }
        : { rootSeq: value.rootSeq, aliases }],
      memberOrd: semanticMembers[0]?.memberOrd ?? null,
      entryIndex: entryIndex >= 0 ? entryIndex : null, root, inflection,
      scoreFacts: word, components: [], counter: null,
      suffixClass: this.#support.suffixClass(collision?.collisionSeq ?? value.rootSeq),
      definitionSeq: collision?.collisionSeq ?? value.rootSeq,
      conjugationSelection: 'default', semanticMembers
    });
  }

  #rootForm(surface: string, seq: number, route: AnalyzerSupportRoute): number | null {
    const key = `${route}\u0000${seq}\u0000${surface}`;
    const cached = this.#rootFormCache.get(key);
    if (cached !== undefined) return cached;
    const match = this.#surface.lookup(surface);
    if (!match?.direct || match.directRank === null) {
      this.#rootFormCache.set(key, null);
      return null;
    }
    const first = this.#roots.surfaceFormStart(match.directRank);
    const count = this.#roots.surfaceFormCount(match.directRank);
    for (let offset = 0; offset < count; offset++) {
      const form = first + offset;
      const entry = this.#roots.formEntryIndex(form);
      if (this.#roots.entrySeq(entry) === seq && this.#roots.formRoute(form) === route) {
        this.#rootFormCache.set(key, form);
        return form;
      }
    }
    this.#rootFormCache.set(key, null);
    return null;
  }
}
