import { createPresentation, partOfSpeechCategory } from '@ichiran/presentation';

const english = createPresentation('en');

/** @deprecated Import locale-aware helpers from `@ichiran/presentation`. */
export const partOfSpeechLabel = english.partOfSpeechLabel;
/** @deprecated Import locale-aware helpers from `@ichiran/presentation`. */
export const conjugationLabel = english.conjugationLabel;
export { partOfSpeechCategory };
export type { PartOfSpeechCategory } from '@ichiran/presentation';
