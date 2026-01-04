import { linguisticRule } from '../../../engine/lang.js';

/**
 * JLPT4: まず - First of all / To start with
 *
 * Matches まず (mazu) as an adverb expressing that something should be done first,
 * before anything else.
 *
 * Structures:
 * - まず + Phrase/Sentence (firstly/to start with)
 * - まずは + Phrase/Sentence (emphatic variant)
 *
 * Examples:
 * - まず宿題をしたほうがいい (First, it would be best to do my homework)
 * - まず本を読んで、映画を見に行きましょう (First read the book, then let's go see the movie)
 * - まず買い物して、その後食べに行きましょうか (First let's go shopping then after that, shall we go out to eat?)
 * - まず、この中で一番好きな飲み物を選んでください (To start, please choose your favorite drink from these)
 *
 * Key discriminators:
 * - POS is ADV (adverb)
 * - text is 'まず' (hiragana form)
 * - Optionally followed by particle は (emphatic)
 *
 * Note: Similar to さいしょに (saisho ni) and はじめに (hajime ni) but まず is
 * more conversational and directly connects to sentences without needing the に particle.
 * The meaning emphasizes order of action rather than rank or position.
 */
export default linguisticRule('まず', (r) => {
  const mazu = r.adv({
    text: 'まず',
  }, 'mazu');

  // Capture the adverb itself
  // Note: The emphatic particle は is optional and not part of the core grammar rule
  r.capture(mazu);
});
