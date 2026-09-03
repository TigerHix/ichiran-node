import { Fragment, type ReactElement, type ReactNode } from 'react';
import {
  Check as CheckIcon,
  Copy as CopyIcon,
  TextAa as TextAaIcon,
  X as XIcon
} from '@phosphor-icons/react';
import { Button } from '@/components/ui/button';
import { Skeleton } from '@/components/ui/skeleton';
import type { AnalysisToken, DictionaryEntry } from './analyzer-service.js';
import {
  conjugationLabel,
  partOfSpeechCategory,
  partOfSpeechLabel
} from './dictionary-labels.js';

function DetailSection({ title, children }: { title: string; children: ReactNode }): ReactElement {
  return <section className="detail-section"><h3>{title}</h3>{children}</section>;
}

function PartOfSpeechTag({ value }: { value: string }): ReactElement {
  return (
    <span className={`pos-tag pos-tag-${partOfSpeechCategory(value)}`}>
      {partOfSpeechLabel(value)}
    </span>
  );
}

function InflectionTags({ inflection }: {
  inflection: AnalysisToken['inflection'];
}): ReactElement | null {
  if (inflection.length === 0) return null;
  return (
    <div className="conjugation-tags">
      {inflection.map((step, index) => (
        <Fragment key={`${step.pos}:${step.type}:${step.ordinal}:${index}`}>
          <span className="pos-tag pos-tag-subtle">{conjugationLabel(step.type)}</span>
          {step.negative && <span className="pos-tag pos-tag-subtle">Negative</span>}
          {step.formal && <span className="pos-tag pos-tag-subtle">Formal</span>}
        </Fragment>
      ))}
    </div>
  );
}

interface PresentedSense {
  readonly key: string;
  readonly gloss: string;
  readonly pos: readonly string[];
  readonly info: string;
}

export function presentedSenses(
  entry: DictionaryEntry,
  route: AnalysisToken['route'],
  form: string,
  reading: string,
  fallbackPos: readonly string[]
): readonly PresentedSense[] {
  let carriedPos = [...fallbackPos];
  const result: PresentedSense[] = [];
  for (const sense of [...entry.senses].sort((left, right) => left.ord - right.ord)) {
    const pos = sense.properties.filter(property => property.tag === 'pos').map(property => property.text);
    if (pos.length > 0) carriedPos = pos;

    const writtenRestrictions = sense.properties
      .filter(property => property.tag === 'stagk')
      .map(property => property.text);
    const readingRestrictions = sense.properties
      .filter(property => property.tag === 'stagr')
      .map(property => property.text);
    if (route === 'kanji' && writtenRestrictions.length > 0 && !writtenRestrictions.includes(form)) continue;
    if (readingRestrictions.length > 0 && !readingRestrictions.includes(reading)) continue;

    const gloss = [...sense.glosses]
      .sort((left, right) => left.ord - right.ord)
      .map(value => value.text)
      .join('; ');
    if (!gloss) continue;
    result.push({
      key: `${sense.ord}:${gloss}`,
      gloss,
      pos: carriedPos,
      info: sense.properties
        .filter(property => property.tag === 's_inf')
        .map(property => property.text)
        .join('; ')
    });
  }
  return result;
}

function TokenMeanings({ entry, route, form, reading, fallbackPos }: {
  entry: DictionaryEntry | null;
  route: AnalysisToken['route'];
  form: string;
  reading: string;
  fallbackPos: readonly string[];
}): ReactElement | null {
  if (!entry) return null;
  const meanings = presentedSenses(entry, route, form, reading, fallbackPos);
  if (meanings.length === 0) return null;
  return (
    <div className="token-meanings">
      {meanings.map((meaning, index) => (
        <div className="token-meaning" key={meaning.key}>
          <span className="token-meaning-number">{index + 1}</span>
          <div>
            {meaning.pos.length > 0 && (
              <div className="meaning-pos-tags">
                {meaning.pos.map((value, posIndex) => (
                  <PartOfSpeechTag key={`${value}:${posIndex}`} value={value} />
                ))}
              </div>
            )}
            <p>{meaning.gloss}</p>
            {meaning.info && <small>{meaning.info}</small>}
          </div>
        </div>
      ))}
    </div>
  );
}

function NestedTokenCard({
  text, reading, route, pos, inflection = [], entry, definitionForm = text,
  definitionReading = reading
}: {
  text: string;
  reading: string;
  route: AnalysisToken['route'];
  pos: readonly string[];
  inflection?: AnalysisToken['inflection'];
  entry: DictionaryEntry | null;
  definitionForm?: string;
  definitionReading?: string;
}): ReactElement {
  const hasMeanings = entry !== null && entry.senses.length > 0;
  return (
    <div className="token-nested-card">
      <div className="nested-token-heading">
        <strong lang="ja">{text}</strong>
        {reading && reading !== text && <span lang="ja">{reading}</span>}
      </div>
      <InflectionTags inflection={inflection} />
      {!hasMeanings && pos.length > 0 && (
        <div className="meaning-pos-tags">
          {pos.map((value, index) => <PartOfSpeechTag key={`${value}:${index}`} value={value} />)}
        </div>
      )}
      <TokenMeanings
        entry={entry}
        route={route}
        form={definitionForm}
        reading={definitionReading}
        fallbackPos={pos}
      />
    </div>
  );
}

export interface WordDetailsProps {
  readonly token: AnalysisToken | null;
  readonly selectionText: string;
  readonly entry: DictionaryEntry | null;
  readonly relatedEntries: ReadonlyMap<number, DictionaryEntry>;
  readonly loading: boolean;
  readonly error: string | null;
  readonly copied: boolean;
  readonly onCopy: () => void;
  readonly onClose: () => void;
  readonly compact?: boolean;
}

export function WordDetails({
  token, selectionText, entry, relatedEntries, loading, error, copied, onCopy, onClose, compact = false
}: WordDetailsProps): ReactElement {
  if (!selectionText) {
    return <div className="detail-empty"><TextAaIcon weight="light" /><p>Select a word</p></div>;
  }
  if (!token) {
    return (
      <div className="selection-details">
        <p>Selected text</p><h2 lang="ja">{selectionText}</h2>
        <Button variant="outline" onClick={onCopy}>{copied ? <CheckIcon /> : <CopyIcon />}{copied ? 'Copied' : 'Copy'}</Button>
      </div>
    );
  }
  const relatedEntry = (entryIndex: number | null): DictionaryEntry | null => {
    if (entryIndex === null) return null;
    return entryIndex === token.entryIndex ? entry : relatedEntries.get(entryIndex) ?? null;
  };
  return (
    <div className={`word-details ${compact ? 'word-details-compact' : ''}`}>
      <div className="detail-heading">
        <div className="detail-title">
          <h2 lang="ja">{token.text}</h2>
          {token.reading && token.reading !== token.text && <p lang="ja">{token.reading}</p>}
        </div>
        <div className="detail-actions">
          <Button variant="ghost" size="icon-sm" onClick={onCopy} aria-label="Copy selected word">
            {copied ? <CheckIcon /> : <CopyIcon />}
          </Button>
          {!compact && (
            <Button variant="ghost" size="icon-sm" onClick={onClose} aria-label="Close word details"><XIcon /></Button>
          )}
        </div>
      </div>
      <div className="detail-content">
        {loading && (
          <div className="detail-loading" aria-label="Loading word details">
            <Skeleton className="h-4 w-4/5" /><Skeleton className="h-4 w-3/5" /><Skeleton className="h-4 w-2/3" />
          </div>
        )}
        {error && <p className="message error" role="alert">{error}</p>}

        {token.components.length === 0 && (
          <TokenMeanings
            entry={entry}
            route={token.route}
            form={token.root?.form ?? token.text}
            reading={token.root?.reading ?? token.reading}
            fallbackPos={token.pos}
          />
        )}

        {token.components.length > 0 && (
          <DetailSection title="Structure">
            <div className="structure-equation">
              {token.components.map((component, index) => (
                <Fragment key={`${component.text}:${component.entryIndex}:${index}`}>
                  <strong lang="ja">{component.text}</strong>
                  {index < token.components.length - 1 && <i aria-hidden="true">+</i>}
                </Fragment>
              ))}
            </div>
            <div className="nested-token-list">
              {token.components.map((component, index) => (
                <NestedTokenCard
                  key={`${component.text}:${component.entryIndex}:${index}`}
                  text={component.text}
                  reading={component.reading}
                  route={component.route}
                  pos={component.inflection.map(step => step.pos)}
                  inflection={component.inflection}
                  entry={relatedEntry(component.entryIndex)}
                  definitionForm={component.root?.form}
                  definitionReading={component.root?.reading}
                />
              ))}
            </div>
          </DetailSection>
        )}
        {token.inflection.length > 0 && token.root && (
          <DetailSection title="Conjugations">
            <div className="nested-token-list">
              <NestedTokenCard
                text={token.root.form}
                reading={token.root.reading}
                route={token.route}
                pos={token.pos}
                inflection={token.inflection}
                entry={entry}
              />
            </div>
          </DetailSection>
        )}
        {token.alternatives.length > 0 && (
          <DetailSection title="Alternative Meanings">
            <div className="nested-token-list">
              {token.alternatives.map(alternative => (
                <NestedTokenCard
                  key={alternative.candidateId}
                  text={alternative.text}
                  reading={alternative.reading}
                  route={alternative.route}
                  pos={alternative.pos}
                  inflection={alternative.inflection}
                  entry={relatedEntry(alternative.entryIndex)}
                  definitionForm={alternative.root?.form}
                  definitionReading={alternative.root?.reading}
                />
              ))}
            </div>
          </DetailSection>
        )}
      </div>
    </div>
  );
}
