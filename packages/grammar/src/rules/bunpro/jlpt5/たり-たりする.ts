import { linguisticRule } from '../../../engine/lang.js';

export default linguisticRule('たり-たりする', (r) => {
  // たり-たりする (doing things like, alternating actions)
  // Verb[た]り + (Verb[た]り +) する (alternating action pattern)
  // This expresses "doing things like A and B" or "activities such as A and B"
  // The final verb する can be conjugated (します, した, etc.)
  //
  // Examples:
  //   テレビを見たり、寝たりする。 (watching TV and sleeping, among other things)
  //   勉強したり、おやつを食べたり、掃除もします。 (studying, eating snacks, and cleaning)
  //   雨が降ったり、止んだりするね。 (raining and stopping repeatedly)
  //   昔あそこの池で泳いだりした。 (swam, among other things - single verb pattern)
  //
  // GiNZA parses multiple patterns for り:
  //
  // 1. Suru-verb + り (standard):
  //    勉強 (VERB) + し (AUX, lemma=する, 連用形) + たり (PART, dep=mark)
  //
  // 2. Regular verb + り:
  //    食べ (VERB, 連用形-一般) + たり (PART, dep=mark)
  //    飲ん (VERB, 連用形-撥音便) + だり (PART, dep=mark)
  //
  // 3. Noun + suru + り (noun as suru-verb):
  //    スポーツ (NOUN) + し (AUX, lemma=する) + たり (ADP, dep=case)
  //
  // The final する can be:
  //   する (dictionary form)
  //   します (polite form)
  //   した (past form)
  //   しました (polite past)
  //   している (progressive)
  //   して (て-form for たりして pattern)
  //   たい (desire form)

  r.either(
    // Pattern 1: Two regular verbs + り (various conjunctive forms)
    (b1) => {
      const verb1 = b1.verb({ inflectionFormOneOf: [
        '連用形-一般',
        '連用形-撥音便',
        '連用形-イ音便',
        '連用形-促音便'
      ]}, 'verb1');
      const tari1 = b1.tok({ textOneOf: ['たり', 'だり'], posOneOf: ['PART', 'ADP'], depOneOf: ['mark', 'case'] }, 'tari1');
      b1.headChild(verb1, tari1, 'mark');
      b1.inOrder(verb1, tari1, 1);

      const verb2 = b1.verb({ inflectionFormOneOf: [
        '連用形-一般',
        '連用形-撥音便',
        '連用形-イ音便',
        '連用形-促音便'
      ]}, 'verb2');
      const tari2 = b1.tok({ textOneOf: ['たり', 'だり'], posOneOf: ['PART', 'ADP'], depOneOf: ['mark', 'case'] }, 'tari2');
      b1.headChild(verb2, tari2, 'mark');
      b1.inOrder(verb2, tari2, 1);

      b1.inOrder(tari1, tari2);

      const suru = b1.tok({
        lemma: 'する',
        posOneOf: ['VERB', 'AUX']
      }, 'suru');
      b1.inOrder(tari2, suru);

      b1.captureSpan('たり-たりする', verb1, suru);
    },
    // Pattern 2: Single regular verb + り + する (single action pattern)
    (b2) => {
      const verb1 = b2.verb({ inflectionFormOneOf: [
        '連用形-一般',
        '連用形-撥音便',
        '連用形-イ音便',
        '連用形-促音便'
      ]}, 'verb1');
      const tari1 = b2.tok({ textOneOf: ['たり', 'だり'], posOneOf: ['PART', 'ADP'], depOneOf: ['mark', 'case'] }, 'tari1');
      b2.headChild(verb1, tari1, 'mark');
      b2.inOrder(verb1, tari1, 1);

      const suru = b2.tok({
        lemma: 'する',
        posOneOf: ['VERB', 'AUX']
      }, 'suru');
      b2.inOrder(tari1, suru);

      b2.captureSpan('たり-たりする', verb1, suru);
    },
    // Pattern 3: Two suru-verbs + り (both are VERB/NOUN + し + り pattern)
    (b3) => {
      // Match verb1 (either VERB or NOUN) with し AUX attached
      b3.either(
        // verb1 is VERB
        (b3a) => {
          const verb1 = b3a.verb({}, 'verb1');
          const shi1 = b3a.aux({ lemma: 'する', inflectionForm: '連用形-一般' }, 'shi1');
          b3a.auxOf(verb1, shi1);

          const tari1 = b3a.tok({ textOneOf: ['たり', 'だり'], posOneOf: ['PART', 'ADP'], depOneOf: ['mark', 'case'] }, 'tari1');
          b3a.headChild(verb1, tari1, 'mark');
          b3a.inOrder(shi1, tari1, 1);

          const verb2 = b3a.tok({}, 'verb2');
          const shi2 = b3a.aux({ lemma: 'する', inflectionForm: '連用形-一般' }, 'shi2');
          b3a.auxOf(verb2, shi2);

          const tari2 = b3a.tok({ textOneOf: ['たり', 'だり'], posOneOf: ['PART', 'ADP'], depOneOf: ['mark', 'case'] }, 'tari2');
          b3a.headChild(verb2, tari2, 'mark');
          b3a.inOrder(shi2, tari2, 1);

          b3a.inOrder(tari1, tari2);

          const suru = b3a.tok({
            lemma: 'する',
            posOneOf: ['VERB', 'AUX']
          }, 'suru');
          b3a.inOrder(tari2, suru);

          b3a.captureSpan('たり-たりする', verb1, suru);
        },
        // verb1 is NOUN
        (b3b) => {
          const verb1 = b3b.noun({}, 'verb1');
          const shi1 = b3b.aux({ lemma: 'する', inflectionForm: '連用形-一般' }, 'shi1');
          b3b.auxOf(verb1, shi1);

          const tari1 = b3b.tok({ textOneOf: ['たり', 'だり'], posOneOf: ['PART', 'ADP'], depOneOf: ['mark', 'case'] }, 'tari1');
          b3b.headChild(verb1, tari1, 'mark');
          b3b.inOrder(shi1, tari1, 1);

          // verb2 can be VERB or NOUN
          b3b.either(
            // verb2 is VERB
            (b3b1) => {
              const verb2 = b3b1.verb({}, 'verb2');
              const shi2 = b3b1.aux({ lemma: 'する', inflectionForm: '連用形-一般' }, 'shi2');
              b3b1.auxOf(verb2, shi2);

              const tari2 = b3b1.tok({ textOneOf: ['たり', 'だり'], posOneOf: ['PART', 'ADP'], depOneOf: ['mark', 'case'] }, 'tari2');
              b3b1.headChild(verb2, tari2, 'mark');
              b3b1.inOrder(shi2, tari2, 1);

              b3b1.inOrder(tari1, tari2);

              const suru = b3b1.tok({
                lemma: 'する',
                posOneOf: ['VERB', 'AUX']
              }, 'suru');
              b3b1.inOrder(tari2, suru);

              b3b1.captureSpan('たり-たりする', verb1, suru);
            },
            // verb2 is NOUN
            (b3b2) => {
              const verb2 = b3b2.noun({}, 'verb2');
              const shi2 = b3b2.aux({ lemma: 'する', inflectionForm: '連用形-一般' }, 'shi2');
              b3b2.auxOf(verb2, shi2);

              const tari2 = b3b2.tok({ textOneOf: ['たり', 'だり'], posOneOf: ['PART', 'ADP'], depOneOf: ['mark', 'case'] }, 'tari2');
              b3b2.headChild(verb2, tari2, 'mark');
              b3b2.inOrder(shi2, tari2, 1);

              b3b2.inOrder(tari1, tari2);

              const suru = b3b2.tok({
                lemma: 'する',
                posOneOf: ['VERB', 'AUX']
              }, 'suru');
              b3b2.inOrder(tari2, suru);

              b3b2.captureSpan('たり-たりする', verb1, suru);
            }
          );
        }
      );
    },
    // Pattern 4: Mixed (suru-verb + regular verb)
    (b4) => {
      const verb1 = b4.tok({}, 'verb1');
      const shi1 = b4.aux({ lemma: 'する', inflectionForm: '連用形-一般' }, 'shi1');
      b4.auxOf(verb1, shi1);

      const tari1 = b4.tok({ textOneOf: ['たり', 'だり'], posOneOf: ['PART', 'ADP'], depOneOf: ['mark', 'case'] }, 'tari1');
      b4.headChild(verb1, tari1, 'mark');
      b4.inOrder(shi1, tari1, 1);

      const verb2 = b4.verb({ inflectionFormOneOf: [
        '連用形-一般',
        '連用形-撥音便',
        '連用形-イ音便',
        '連用形-促音便'
      ]}, 'verb2');
      const tari2 = b4.tok({ textOneOf: ['たり', 'だり'], posOneOf: ['PART', 'ADP'], depOneOf: ['mark', 'case'] }, 'tari2');
      b4.headChild(verb2, tari2, 'mark');
      b4.inOrder(verb2, tari2, 1);

      b4.inOrder(tari1, tari2);

      const suru = b4.tok({
        lemma: 'する',
        posOneOf: ['VERB', 'AUX']
      }, 'suru');
      b4.inOrder(tari2, suru);

      b4.captureSpan('たり-たりする', verb1, suru);
    },
    // Pattern 5: Mixed (regular verb + suru-verb)
    (b5) => {
      const verb1 = b5.verb({ inflectionFormOneOf: [
        '連用形-一般',
        '連用形-撥音便',
        '連用形-イ音便',
        '連用形-促音便'
      ]}, 'verb1');
      const tari1 = b5.tok({ textOneOf: ['たり', 'だり'], posOneOf: ['PART', 'ADP'], depOneOf: ['mark', 'case'] }, 'tari1');
      b5.headChild(verb1, tari1, 'mark');
      b5.inOrder(verb1, tari1, 1);

      const verb2 = b5.tok({}, 'verb2');
      const shi2 = b5.aux({ lemma: 'する', inflectionForm: '連用形-一般' }, 'shi2');
      b5.auxOf(verb2, shi2);

      const tari2 = b5.tok({ textOneOf: ['たり', 'だり'], posOneOf: ['PART', 'ADP'], depOneOf: ['mark', 'case'] }, 'tari2');
      b5.headChild(verb2, tari2, 'mark');
      b5.inOrder(shi2, tari2, 1);

      b5.inOrder(tari1, tari2);

      const suru = b5.tok({
        lemma: 'する',
        posOneOf: ['VERB', 'AUX']
      }, 'suru');
      b5.inOrder(tari2, suru);

      b5.captureSpan('たり-たりする', verb1, suru);
    }
  );
});
