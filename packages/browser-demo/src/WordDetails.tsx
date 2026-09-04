import { Fragment, type ReactElement, type ReactNode } from 'react';
import {
  Check as CheckIcon,
  Copy as CopyIcon,
  TextAa as TextAaIcon,
  X as XIcon
} from '@phosphor-icons/react';
import { Button } from '@/components/ui/button';
import { Skeleton } from '@/components/ui/skeleton';
import { partOfSpeechCategory, type Presentation } from '@ichiran/presentation';
import type {
  AnalysisToken,
  TokenConjugation,
  TokenDetails,
  TokenMeaning
} from './analyzer-service.js';

function DetailSection({ title, children }: { title: string; children: ReactNode }): ReactElement {
  return <section className="detail-section"><h3>{title}</h3>{children}</section>;
}

function PartOfSpeechTag({ value, presentation }: {
  value: string;
  presentation: Presentation;
}): ReactElement {
  return (
    <span className={`pos-tag pos-tag-${partOfSpeechCategory(value)}`}>
      {presentation.partOfSpeechLabel(value)}
    </span>
  );
}

function canonicalCodes(values: readonly string[]): string[] {
  return [...new Set(values)].sort((left, right) => left < right ? -1 : left > right ? 1 : 0);
}

function MeaningList({ meanings, presentation }: {
  meanings: readonly TokenMeaning[];
  presentation: Presentation;
}): ReactElement | null {
  if (meanings.length === 0) return null;

  const groups: Array<{
    key: string;
    pos: readonly string[];
    meanings: Array<{ meaning: TokenMeaning; index: number }>;
  }> = [];
  for (const [index, meaning] of meanings.entries()) {
    const pos = canonicalCodes(meaning.pos);
    const key = JSON.stringify(pos);
    const previous = groups.at(-1);
    if (previous?.key === key) {
      previous.meanings.push({ meaning, index });
    } else {
      groups.push({ key, pos, meanings: [{ meaning, index }] });
    }
  }

  return (
    <div className="token-meanings">
      {groups.map((group, groupIndex) => (
        <div className="token-meaning-group" key={`${group.key}:${groupIndex}`}>
          {group.pos.length > 0 && (
            <div className="meaning-pos-tags">
              {group.pos.map((value, posIndex) => (
                <PartOfSpeechTag key={`${value}:${posIndex}`} value={value} presentation={presentation} />
              ))}
            </div>
          )}
          {group.meanings.map(({ meaning, index }) => (
            <div className="token-meaning" key={`${meaning.gloss}:${index}`}>
              <span className="token-meaning-number">{index + 1}</span>
              <div>
                <p>{meaning.gloss}</p>
                {meaning.fields.length > 0 && (
                  <small>{meaning.fields.map(value => presentation.fieldLabel(value)).join(' · ')}</small>
                )}
                {meaning.info && <small>{meaning.info}</small>}
              </div>
            </div>
          ))}
        </div>
      ))}
    </div>
  );
}

function ConjugationTags({ value, presentation }: {
  value: TokenConjugation;
  presentation: Presentation;
}): ReactElement | null {
  if (value.properties.length === 0) return null;

  const groups: Array<{
    key: string;
    pos: string[];
    types: number[];
    negative: boolean;
    formal: boolean;
  }> = [];
  for (const property of value.properties) {
    const key = `${property.negative}:${property.formal}`;
    let group = groups.find(candidate => candidate.key === key);
    if (!group) {
      group = { key, pos: [], types: [], negative: property.negative, formal: property.formal };
      groups.push(group);
    }
    if (!group.pos.includes(property.pos)) group.pos.push(property.pos);
    if (!group.types.includes(property.type)) group.types.push(property.type);
  }

  return (
    <div className="conjugation-tags">
      {groups.map((group, index) => (
        <Fragment key={`${group.key}:${index}`}>
          {index > 0 && <span className="conjugation-alternative-divider">/</span>}
          {value.meanings.length === 0 && canonicalCodes(group.pos).map(pos => (
            <PartOfSpeechTag key={pos} value={pos} presentation={presentation} />
          ))}
          <span className="pos-tag pos-tag-subtle">
            {group.types.map(presentation.conjugationLabel).join(' / ')}
          </span>
          {group.negative && <span className="pos-tag pos-tag-subtle">{presentation.message('negative')}</span>}
          {group.formal && <span className="pos-tag pos-tag-subtle">{presentation.message('formal')}</span>}
        </Fragment>
      ))}
    </div>
  );
}

function ConjugationCard({ value, presentation }: {
  value: TokenConjugation;
  presentation: Presentation;
}): ReactElement {
  return (
    <div className="token-nested-card">
      {value.root && (
        <div className="nested-token-heading">
          <strong lang="ja">{value.root.text}</strong>
          {value.root.reading !== value.root.text && <span lang="ja">{value.root.reading}</span>}
        </div>
      )}
      <ConjugationTags value={value} presentation={presentation} />
      <MeaningList meanings={value.meanings} presentation={presentation} />
      {value.via.length > 0 && (
        <div className="nested-token-list">
          {value.via.map((child, index) => (
            <ConjugationCard key={`${child.root?.text ?? 'via'}:${index}`} value={child} presentation={presentation} />
          ))}
        </div>
      )}
    </div>
  );
}

function TokenCard({ value, presentation }: {
  value: TokenDetails;
  presentation: Presentation;
}): ReactElement {
  return (
    <div className="token-nested-card">
      <div className="nested-token-heading">
        <strong lang="ja">{value.text}</strong>
        {value.reading && value.reading !== value.text && <span lang="ja">{value.reading}</span>}
      </div>
      {(value.suffixId || value.counter || value.entityKind) && (
        <div className="meaning-pos-tags">
          {value.suffixId && <span className="pos-tag pos-tag-prefix-suffix">{presentation.suffixLabel(value.suffixId)}</span>}
          {value.entityKind && <span className="pos-tag pos-tag-noun">{presentation.entityLabel(value.entityKind)}</span>}
          {value.counter && (
            <span className="pos-tag pos-tag-counter">
              {presentation.message(value.counter.ordinal ? 'ordinalCounter' : 'counter')} · {value.counter.value}
            </span>
          )}
        </div>
      )}
      <MeaningList meanings={value.meanings} presentation={presentation} />
      {value.components.length > 0 && (
        <div className="nested-token-list">
          {value.components.map((component, index) => (
            <TokenCard key={`${component.text}:${index}`} value={component} presentation={presentation} />
          ))}
        </div>
      )}
      {value.conjugations.length > 0 && (
        <div className="nested-token-list">
          {value.conjugations.map((conjugation, index) => (
            <ConjugationCard key={`${conjugation.root?.text ?? 'via'}:${index}`} value={conjugation} presentation={presentation} />
          ))}
        </div>
      )}
    </div>
  );
}

export interface WordDetailsProps {
  readonly token: AnalysisToken | null;
  readonly selectionText: string;
  readonly details: TokenDetails | null;
  readonly loading: boolean;
  readonly error: string | null;
  readonly copied: boolean;
  readonly presentation: Presentation;
  readonly onCopy: () => void;
  readonly onClose: () => void;
  readonly compact?: boolean;
}

export function WordDetails({
  token, selectionText, details, loading, error, copied, presentation, onCopy, onClose, compact = false
}: WordDetailsProps): ReactElement {
  if (!selectionText) {
    return <div className="detail-empty"><TextAaIcon weight="light" /><p>{presentation.message('selectWord')}</p></div>;
  }
  if (!token) {
    return (
      <div className="selection-details">
        <p>{presentation.message('selectedText')}</p><h2 lang="ja">{selectionText}</h2>
        <Button variant="outline" onClick={onCopy}>{copied ? <CheckIcon /> : <CopyIcon />}{presentation.message(copied ? 'copied' : 'copy')}</Button>
      </div>
    );
  }
  const title = details?.text ?? token.text;
  const reading = details?.reading ?? token.reading;
  return (
    <div className={`word-details ${compact ? 'word-details-compact' : ''}`}>
      <div className="detail-heading">
        <div className="detail-title">
          <h2 lang="ja">{title}</h2>
          {reading && reading !== title && <p lang="ja">{reading}</p>}
        </div>
        <div className="detail-actions">
          <Button variant="ghost" size="icon-sm" onClick={onCopy} aria-label={presentation.message('copySelected')}>
            {copied ? <CheckIcon /> : <CopyIcon />}
          </Button>
          {!compact && (
            <Button variant="ghost" size="icon-sm" onClick={onClose} aria-label={presentation.message('closeDetails')}><XIcon /></Button>
          )}
        </div>
      </div>
      <div className="detail-content">
        {loading && (
          <div className="detail-loading" aria-label={presentation.message('loadingDetails')}>
            <Skeleton className="h-4 w-4/5" /><Skeleton className="h-4 w-3/5" /><Skeleton className="h-4 w-2/3" />
          </div>
        )}
        {error && <p className="message error" role="alert">{error}</p>}
        {details && (
          <>
            {(details.suffixId || details.counter || details.entityKind) && (
              <div className="meaning-pos-tags">
                {details.suffixId && <span className="pos-tag pos-tag-prefix-suffix">{presentation.suffixLabel(details.suffixId)}</span>}
                {details.entityKind && <span className="pos-tag pos-tag-noun">{presentation.entityLabel(details.entityKind)}</span>}
                {details.counter && (
                  <span className="pos-tag pos-tag-counter">
                    {presentation.message(details.counter.ordinal ? 'ordinalCounter' : 'counter')} · {details.counter.value}
                  </span>
                )}
              </div>
            )}
            <MeaningList meanings={details.meanings} presentation={presentation} />
            {details.components.length > 0 && (
              <DetailSection title={presentation.message('structure')}>
                <div className="structure-equation">
                  {details.components.map((component, index) => (
                    <Fragment key={`${component.text}:${index}`}>
                      <strong lang="ja">{component.text}</strong>
                      {index < details.components.length - 1 && <i aria-hidden="true">+</i>}
                    </Fragment>
                  ))}
                </div>
                <div className="nested-token-list">
                  {details.components.map((component, index) => (
                    <TokenCard key={`${component.text}:${index}`} value={component} presentation={presentation} />
                  ))}
                </div>
              </DetailSection>
            )}
            {details.conjugations.length > 0 && (
              <DetailSection title={presentation.message('conjugations')}>
                <div className="nested-token-list">
                  {details.conjugations.map((conjugation, index) => (
                    <ConjugationCard key={`${conjugation.root?.text ?? 'via'}:${index}`} value={conjugation} presentation={presentation} />
                  ))}
                </div>
              </DetailSection>
            )}
            {details.alternatives.length > 0 && (
              <DetailSection title={presentation.message('alternatives')}>
                <div className="nested-token-list">
                  {details.alternatives.map((alternative, index) => (
                    <TokenCard key={`${alternative.text}:${index}`} value={alternative} presentation={presentation} />
                  ))}
                </div>
              </DetailSection>
            )}
          </>
        )}
      </div>
    </div>
  );
}
