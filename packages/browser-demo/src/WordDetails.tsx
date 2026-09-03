import { Fragment, type ReactElement, type ReactNode } from 'react';
import {
  Check as CheckIcon,
  Copy as CopyIcon,
  TextAa as TextAaIcon,
  X as XIcon
} from '@phosphor-icons/react';
import { Button } from '@/components/ui/button';
import { Skeleton } from '@/components/ui/skeleton';
import type {
  AnalysisToken,
  TokenConjugation,
  TokenDetails,
  TokenMeaning
} from './analyzer-service.js';
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

function MeaningList({ meanings }: { meanings: readonly TokenMeaning[] }): ReactElement | null {
  if (meanings.length === 0) return null;
  return (
    <div className="token-meanings">
      {meanings.map((meaning, index) => (
        <div className="token-meaning" key={`${meaning.gloss}:${meaning.pos.join(',')}:${index}`}>
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
            {meaning.fields.length > 0 && <small>{meaning.fields.join(' · ')}</small>}
            {meaning.info && <small>{meaning.info}</small>}
          </div>
        </div>
      ))}
    </div>
  );
}

function ConjugationTags({ value }: { value: TokenConjugation }): ReactElement | null {
  if (value.properties.length === 0) return null;
  return (
    <div className="conjugation-tags">
      {value.properties.map((property, index) => (
        <Fragment key={`${property.pos}:${property.type}:${index}`}>
          {value.meanings.length === 0 && <PartOfSpeechTag value={property.pos} />}
          <span className="pos-tag pos-tag-subtle">{conjugationLabel(property.type)}</span>
          {property.negative && <span className="pos-tag pos-tag-subtle">Negative</span>}
          {property.formal && <span className="pos-tag pos-tag-subtle">Formal</span>}
        </Fragment>
      ))}
    </div>
  );
}

function ConjugationCard({ value }: { value: TokenConjugation }): ReactElement {
  return (
    <div className="token-nested-card">
      {value.root && (
        <div className="nested-token-heading">
          <strong lang="ja">{value.root.text}</strong>
          {value.root.reading !== value.root.text && <span lang="ja">{value.root.reading}</span>}
        </div>
      )}
      <ConjugationTags value={value} />
      <MeaningList meanings={value.meanings} />
      {value.via.length > 0 && (
        <div className="nested-token-list">
          {value.via.map((child, index) => (
            <ConjugationCard key={`${child.root?.text ?? 'via'}:${index}`} value={child} />
          ))}
        </div>
      )}
    </div>
  );
}

function TokenCard({ value }: { value: TokenDetails }): ReactElement {
  return (
    <div className="token-nested-card">
      <div className="nested-token-heading">
        <strong lang="ja">{value.text}</strong>
        {value.reading && value.reading !== value.text && <span lang="ja">{value.reading}</span>}
      </div>
      {(value.suffix || value.counter) && (
        <div className="meaning-pos-tags">
          {value.suffix && <span className="pos-tag pos-tag-prefix-suffix">{value.suffix}</span>}
          {value.counter && (
            <span className="pos-tag pos-tag-counter">
              {value.counter.ordinal ? 'Ordinal counter' : 'Counter'} · {value.counter.value}
            </span>
          )}
        </div>
      )}
      <MeaningList meanings={value.meanings} />
      {value.components.length > 0 && (
        <div className="nested-token-list">
          {value.components.map((component, index) => (
            <TokenCard key={`${component.text}:${index}`} value={component} />
          ))}
        </div>
      )}
      {value.conjugations.length > 0 && (
        <div className="nested-token-list">
          {value.conjugations.map((conjugation, index) => (
            <ConjugationCard key={`${conjugation.root?.text ?? 'via'}:${index}`} value={conjugation} />
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
  readonly onCopy: () => void;
  readonly onClose: () => void;
  readonly compact?: boolean;
}

export function WordDetails({
  token, selectionText, details, loading, error, copied, onCopy, onClose, compact = false
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
        {details && (
          <>
            {(details.suffix || details.counter) && (
              <div className="meaning-pos-tags">
                {details.suffix && <span className="pos-tag pos-tag-prefix-suffix">{details.suffix}</span>}
                {details.counter && (
                  <span className="pos-tag pos-tag-counter">
                    {details.counter.ordinal ? 'Ordinal counter' : 'Counter'} · {details.counter.value}
                  </span>
                )}
              </div>
            )}
            <MeaningList meanings={details.meanings} />
            {details.components.length > 0 && (
              <DetailSection title="Structure">
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
                    <TokenCard key={`${component.text}:${index}`} value={component} />
                  ))}
                </div>
              </DetailSection>
            )}
            {details.conjugations.length > 0 && (
              <DetailSection title="Conjugations">
                <div className="nested-token-list">
                  {details.conjugations.map((conjugation, index) => (
                    <ConjugationCard key={`${conjugation.root?.text ?? 'via'}:${index}`} value={conjugation} />
                  ))}
                </div>
              </DetailSection>
            )}
            {details.alternatives.length > 0 && (
              <DetailSection title="Alternatives">
                <div className="nested-token-list">
                  {details.alternatives.map((alternative, index) => (
                    <TokenCard key={`${alternative.text}:${index}`} value={alternative} />
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
