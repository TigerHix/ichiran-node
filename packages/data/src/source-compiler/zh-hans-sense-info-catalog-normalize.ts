import {
  ZH_HANS_SENSE_INFO_PATTERN_POLICY,
  ZH_HANS_SENSE_INFO_PATTERN_RULES,
  translateZhHansSenseInfoPattern,
  type ZhHansSenseInfoPatternRuleId
} from './zh-hans-sense-info-patterns.js';
import type {
  ZhHansSenseInfoCatalog,
  ZhHansSenseInfoTranslation
} from './zh-hans-sense-info.js';

export interface ZhHansSenseInfoPrunedTranslation extends ZhHansSenseInfoTranslation {
  readonly rule: ZhHansSenseInfoPatternRuleId;
}

export interface ZhHansSenseInfoReviewedTargetUpdate {
  readonly source: string;
  readonly priorTarget: string;
  readonly reviewedTarget: string;
}

export interface ZhHansSenseInfoCatalogNormalization {
  readonly catalog: ZhHansSenseInfoCatalog;
  readonly prunedTranslations: readonly ZhHansSenseInfoPrunedTranslation[];
  readonly reviewedTargetUpdates: readonly ZhHansSenseInfoReviewedTargetUpdate[];
  readonly stats: {
    readonly patternPolicy: typeof ZH_HANS_SENSE_INFO_PATTERN_POLICY;
    readonly inputTranslationCount: number;
    readonly retainedTranslationCount: number;
    readonly prunedTranslationCount: number;
    readonly reviewedTargetUpdateCount: number;
    readonly prunedRuleCounts: Readonly<Record<ZhHansSenseInfoPatternRuleId, number>>;
  };
}

const REVIEWED_TARGET_UPDATES: Readonly<Record<string, {
  readonly priorTarget: string;
  readonly reviewedTarget: string;
}>> = {
  'after an amount': {
    priorTarget: '接在表示金额或数量的词之后',
    reviewedTarget: '接在表示数量、时长或金额的词之后'
  }
};

/**
 * Remove redundant exact translations after an authoring merge. A pattern
 * disagreement is an error instead of an implicit precedence choice: it must
 * be reviewed before either source of truth changes.
 */
export function normalizeZhHansSenseInfoCatalog(
  catalog: ZhHansSenseInfoCatalog
): ZhHansSenseInfoCatalogNormalization {
  const retained: ZhHansSenseInfoTranslation[] = [];
  const pruned: ZhHansSenseInfoPrunedTranslation[] = [];
  const reviewedTargetUpdates: ZhHansSenseInfoReviewedTargetUpdate[] = [];
  const conflicts: {
    readonly source: string;
    readonly catalogTarget: string;
    readonly patternTarget: string;
  }[] = [];
  const prunedRuleCounts = Object.fromEntries(
    ZH_HANS_SENSE_INFO_PATTERN_RULES.map(rule => [rule.id, 0])
  ) as Record<ZhHansSenseInfoPatternRuleId, number>;

  for (const translation of catalog.translations) {
    const direct = translateZhHansSenseInfoPattern(translation.source);
    const reviewedUpdate = REVIEWED_TARGET_UPDATES[translation.source];
    let effectiveTranslation = translation;
    if (reviewedUpdate && translation.target === reviewedUpdate.priorTarget) {
      if (direct?.target !== reviewedUpdate.reviewedTarget) {
        throw new Error(
          `Reviewed target update no longer matches the direct policy: ${translation.source}`
        );
      }
      effectiveTranslation = {
        source: translation.source,
        target: reviewedUpdate.reviewedTarget
      };
      reviewedTargetUpdates.push({
        source: translation.source,
        priorTarget: reviewedUpdate.priorTarget,
        reviewedTarget: reviewedUpdate.reviewedTarget
      });
    }
    if (direct === null) {
      retained.push(effectiveTranslation);
      continue;
    }
    if (direct.target !== effectiveTranslation.target) {
      conflicts.push({
        source: translation.source,
        catalogTarget: effectiveTranslation.target,
        patternTarget: direct.target
      });
      continue;
    }
    pruned.push({ ...effectiveTranslation, rule: direct.rule });
    prunedRuleCounts[direct.rule] += 1;
  }

  if (conflicts.length > 0) {
    const details = conflicts.map(conflict =>
      `${JSON.stringify(conflict.source)}: catalog=${JSON.stringify(conflict.catalogTarget)}, `
      + `pattern=${JSON.stringify(conflict.patternTarget)}`).join('; ');
    throw new Error(
      `Cannot normalize zh-Hans sense-info catalog: ${conflicts.length} `
      + `catalog/direct-rule disagreement(s): ${details}`
    );
  }

  return {
    catalog: { ...catalog, translations: retained },
    prunedTranslations: pruned,
    reviewedTargetUpdates,
    stats: {
      patternPolicy: ZH_HANS_SENSE_INFO_PATTERN_POLICY,
      inputTranslationCount: catalog.translations.length,
      retainedTranslationCount: retained.length,
      prunedTranslationCount: pruned.length,
      reviewedTargetUpdateCount: reviewedTargetUpdates.length,
      prunedRuleCounts
    }
  };
}
