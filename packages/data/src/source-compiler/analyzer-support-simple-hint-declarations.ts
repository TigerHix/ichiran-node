import { insertHintMarkers } from './kanjidic-hints.js';

export type HintCompiler = (kana: string) => string | null;
type HintSpec = readonly [kind: 'space' | 'mod', position: number];
type HintVariables = Record<string, number>;
type HintDefinition =
  | {
      readonly type: 'test';
      readonly value: (
        kana: string,
        length: number
      ) => Readonly<HintVariables> | true | false | null;
    }
  | {
      readonly type: 'hint';
      readonly keyword: 'space' | 'mod';
      readonly position: (
        kana: string,
        length: number,
        variables: Readonly<HintVariables>
      ) => number | null;
    };

interface SimpleHintOptions {
  readonly seqs: number | readonly number[];
  readonly hints: readonly HintDefinition[];
}

const legacy: Array<readonly [seq: number, compile: HintCompiler]> = [];

function defSimpleHint(options: SimpleHintOptions): void {
  const compile: HintCompiler = kana => {
    const variables: HintVariables = {};
    for (const hint of options.hints) {
      if (hint.type !== 'test') continue;
      const value = hint.value(kana, kana.length);
      if (value === null || value === false) return null;
      if (value !== true) Object.assign(variables, value);
    }
    const specs: HintSpec[] = [];
    for (const hint of options.hints) {
      if (hint.type !== 'hint') continue;
      const position = hint.position(kana, kana.length, variables);
      if (position !== null) specs.push([hint.keyword, position]);
    }
    return insertHintMarkers(kana, specs);
  };
  for (const seq of typeof options.seqs === 'number' ? [options.seqs] : options.seqs) {
    legacy.push([seq, compile]);
  }
}

// Lines 1006-1011: expressions ending with は/へ
defSimpleHint({ seqs: [2028920, 2029000], hints: [{ type: 'hint', keyword: 'mod', position: (_k, l) => l - 1 }] });

// Lines 1013-1022: no space - expressions ending with は
defSimpleHint({ seqs: [1289480, 1289400, 1008450, 2215430, 2028950], hints: [{ type: 'test', value: (k) => k.endsWith('は') ? {} : null }, { type: 'hint', keyword: 'mod', position: (_k, l) => l - 1 }] });

// Lines 1024-1088: with space - expressions ending with は
defSimpleHint({ seqs: [1006660, 1008500, 1307530, 1320830, 1324320, 1524990, 1586850, 1877880, 1897510, 1907300, 1912570, 2034440, 2098160, 2105820, 2134680, 2136300, 2176280, 2177410, 2177420, 2177430, 2177440, 2177450, 2256430, 2428890, 2523450, 2557290, 2673120, 2691570, 2702090, 2717440, 2717510, 2828541, 1217970, 1331520, 1907290, 1914670, 1950430, 2136680, 2181810, 2181730, 2576840, 1331510, 1010470, 2008290, 2136690, 2829815, 2830216, 2840063, 2841096, 2841959, 2844687, 2844836, 2850535, 2861249], hints: [{ type: 'test', value: (k) => k.endsWith('は') ? {} : null }, { type: 'hint', keyword: 'space', position: (_k, l) => l - 1 }, { type: 'hint', keyword: 'mod', position: (_k, l) => l - 1 }] });

// Lines 1090-1094: へと
defSimpleHint({ seqs: [2844416], hints: [{ type: 'hint', keyword: 'space', position: (_k, l) => l - 1 }, { type: 'hint', keyword: 'mod', position: () => 0 }] });

// Lines 1096-1102: ところへ, 何方へ
defSimpleHint({ seqs: [2097010, 1009150], hints: [{ type: 'hint', keyword: 'space', position: (_k, l) => l - 1 }, { type: 'hint', keyword: 'mod', position: (_k, l) => l - 1 }] });

// Lines 1104-1112: それはそれは
defSimpleHint({ seqs: [2261800], hints: [{ type: 'hint', keyword: 'space', position: () => 2 }, { type: 'hint', keyword: 'mod', position: () => 2 }, { type: 'hint', keyword: 'space', position: () => 3 }, { type: 'hint', keyword: 'space', position: (_k, l) => l - 1 }, { type: 'hint', keyword: 'mod', position: (_k, l) => l - 1 }] });

// Lines 1114-1136: では/には ending
defSimpleHint({ seqs: [1009480, 1315860, 1406050, 2026610, 2061740, 2097310, 2101020, 2119920, 2134700, 2200100, 2407650, 2553140, 2762790, 1288910, 1423320, 2099850, 1006890], hints: [{ type: 'test', value: (k) => k.endsWith('は') ? {} : null }, { type: 'hint', keyword: 'space', position: (_k, l) => l - 2 }, { type: 'hint', keyword: 'mod', position: (_k, l) => l - 1 }] });

// Lines 1140-1152: では expressions
defSimpleHint({ seqs: [2089020, 2823770, 2098240, 2027020, 2135480, 2397760, 2724540, 2757720], hints: [{ type: 'test', value: (k) => { const deha = k.lastIndexOf('では'); return deha >= 0 ? { deha } : null; } }, { type: 'hint', keyword: 'mod', position: (_k, _l, vars) => vars.deha + 1 }] });

// Lines 1154-1188: ends with ではない
defSimpleHint({ seqs: [2027080, 2126160, 2126140, 2131120, 2136640, 2214830, 2221680, 2416950, 2419210, 2664520, 2682500, 2775790, 1343120, 2112270, 2404260, 2758400, 2827556, 2057560, 2841318, 2088970, 2833095, 2835662, 2841608, 2841609, 2845739, 2849457, 2850045, 2854412], hints: [{ type: 'test', value: (k) => { const deha = k.lastIndexOf('では'); return deha >= 0 ? { deha } : null; } }, { type: 'hint', keyword: 'space', position: (_k, _l, vars) => vars.deha }, { type: 'hint', keyword: 'mod', position: (_k, _l, vars) => vars.deha + 1 }] });

// Lines 1191-1203: では in the middle
defSimpleHint({ seqs: [2037860, 2694350, 2111220, 2694360, 2182700, 2142010], hints: [{ type: 'test', value: (k) => { const deha = k.lastIndexOf('では'); return deha >= 0 ? { deha } : null; } }, { type: 'hint', keyword: 'space', position: (_k, _l, vars) => vars.deha }, { type: 'hint', keyword: 'mod', position: (_k, _l, vars) => vars.deha + 1 }, { type: 'hint', keyword: 'space', position: (_k, _l, vars) => vars.deha + 2 }] });

// Lines 1206-1235: には in the middle
defSimpleHint({ seqs: [2057580, 2067990, 2103020, 2105980, 2152700, 2416920, 2418030, 2792210, 2792420, 2417920, 2598720, 2420170, 2597190, 2597800, 2057570, 2419360, 2121480, 2646440, 2740880, 2416860, 2156910, 2182690, 2848157], hints: [{ type: 'test', value: (k) => { const niha = k.lastIndexOf('には'); return niha >= 0 ? { niha } : null; } }, { type: 'hint', keyword: 'space', position: (_k, _l, vars) => vars.niha }, { type: 'hint', keyword: 'mod', position: (_k, _l, vars) => vars.niha + 1 }, { type: 'hint', keyword: 'space', position: (_k, _l, vars) => vars.niha + 2 }] });

// Lines 1239-1248: starts with には/とは
defSimpleHint({ seqs: [2181860, 2037320, 2125460, 2128060, 2070730], hints: [{ type: 'hint', keyword: 'mod', position: () => 1 }, { type: 'hint', keyword: 'space', position: () => 2 }] });

// Lines 1250-1258: 目には目を
defSimpleHint({ seqs: [2832044], hints: [{ type: 'test', value: (k) => { const niha = k.lastIndexOf('には'); return niha >= 0 ? { niha } : null; } }, { type: 'hint', keyword: 'space', position: (_k, _l, vars) => vars.niha }, { type: 'hint', keyword: 'mod', position: (_k, _l, vars) => vars.niha + 1 }, { type: 'hint', keyword: 'space', position: (_k, _l, vars) => vars.niha + 2 }, { type: 'hint', keyword: 'space', position: (_k, l) => l - 1 }] });

// Lines 1261-1338: は in the middle
defSimpleHint({ seqs: [1008970, 1188440, 1193090, 1394290, 1855940, 1949380, 1981600, 1982230, 2018320, 2062980, 2078930, 2089520, 2089620, 2098150, 2108910, 2115570, 2118120, 2118430, 2118440, 2134480, 2135530, 2136710, 2141360, 2168360, 2173880, 2174570, 2176450, 2177240, 2200690, 2210960, 2213470, 2255320, 2275900, 2403520, 2408680, 2416870, 2416930, 2417040, 2417150, 2417980, 2418090, 2418280, 2418800, 2418920, 2419030, 2419350, 2419390, 2419590, 2419600, 2419610, 2419620, 2420180, 2583560, 2585230, 2593830, 2600530, 2618920, 2618990, 2708230, 2716900, 2737650, 2741810, 2744840, 2827754, 2831359, 2833597, 2839953, 2844002, 2858918, 2862330], hints: [{ type: 'test', value: (k) => { const ha = k.lastIndexOf('は'); return ha >= 0 ? { ha } : null; } }, { type: 'hint', keyword: 'space', position: (_k, _l, vars) => vars.ha }, { type: 'hint', keyword: 'mod', position: (_k, _l, vars) => vars.ha }, { type: 'hint', keyword: 'space', position: (_k, _l, vars) => vars.ha + 1 }] });

// Lines 1342-1350: そうはイカのキンタマ
defSimpleHint({ seqs: [2716860], hints: [{ type: 'test', value: (k) => { const ha = k.indexOf('は'); const no = k.lastIndexOf('の'); return ha >= 0 && no >= 0 ? { ha, no } : null; } }, { type: 'hint', keyword: 'space', position: (_k, _l, vars) => vars.ha }, { type: 'hint', keyword: 'mod', position: (_k, _l, vars) => vars.ha }, { type: 'hint', keyword: 'space', position: (_k, _l, vars) => vars.ha + 1 }, { type: 'hint', keyword: 'space', position: (_k, _l, vars) => vars.no }, { type: 'hint', keyword: 'space', position: (_k, _l, vars) => vars.no + 1 }] });

// Lines 1352-1363: 他所は他所うちはうち
defSimpleHint({ seqs: [2845260], hints: [{ type: 'test', value: (k) => { const ha1 = k.indexOf('は'); const ha2 = k.lastIndexOf('は'); const uu = k.indexOf('う'); return ha1 >= 0 && ha2 >= 0 && uu >= 0 ? { ha1, ha2, uu } : null; } }, { type: 'hint', keyword: 'space', position: (_k, _l, vars) => vars.ha1 }, { type: 'hint', keyword: 'mod', position: (_k, _l, vars) => vars.ha1 }, { type: 'hint', keyword: 'space', position: (_k, _l, vars) => vars.ha1 + 1 }, { type: 'hint', keyword: 'space', position: (_k, _l, vars) => vars.uu }, { type: 'hint', keyword: 'space', position: (_k, _l, vars) => vars.ha2 }, { type: 'hint', keyword: 'mod', position: (_k, _l, vars) => vars.ha2 }, { type: 'hint', keyword: 'space', position: (_k, _l, vars) => vars.ha2 + 1 }] });


export const LEGACY_SIMPLE_HINT_DECLARATIONS = legacy;

const goSentenceHint: HintCompiler = kana => {
  const ha = kana.indexOf('は');
  return ha < 0 ? null : insertHintMarkers(kana, [
    ['space', ha],
    ['mod', ha],
    ['space', ha + 1]
  ]);
};

export const UPSTREAM_260118_SIMPLE_HINT_DECLARATIONS: readonly (
  readonly [seq: number, compile: HintCompiler]
)[] = [
  [2_867_144, goSentenceHint],
  [2_867_149, goSentenceHint]
];
