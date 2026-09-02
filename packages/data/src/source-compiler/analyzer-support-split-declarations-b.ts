import type { SplitRegistration } from './analyzer-support-annotation-model.js';

/** Second half of the qualified legacy split declaration ledger. */
export function addLegacySplitDeclarationsB(defSimpleSplit: SplitRegistration): void {
  // Line 415-419: はないか
  defSimpleSplit({
    seq: 2771940,
    score: -5,
    parts: [
      { type: 'test', condition: (_len, txt) => txt === "はないか" },
      { type: 'part', seqs: 2028920, lengthFn: () => 1 },
      { type: 'part', seqs: 1529520, lengthFn: () => 2 },
      { type: 'part', seqs: 2028970, lengthFn: () => 1 }
    ]
  });

  // Line 421-422: split-nara - なら
  defSimpleSplit({
    seq: 1009470,
    score: 1,
    parts: [
      { type: 'part', seqs: ["なら", 2089020], lengthFn: () => null }
    ]
  });

  // Line 424-427: ならん
  defSimpleSplit({
    seq: 2083990,
    score: 20,
    parts: [
      { type: 'test', condition: (_len, txt) => txt === "ならん" },
      { type: 'part', seqs: 1009470, lengthFn: () => 2 },
      { type: 'part', seqs: 2139720, lengthFn: () => 1 }
    ]
  });

  // Line 429-430: ならんで
  defSimpleSplit({
    seq: 2762260,
    score: 0,
    parts: [
      { type: 'part', seqs: ["ならんで", 1508380], lengthFn: () => null }
    ]
  });

  // Line 432-435: ならんで (kana version)
  defSimpleSplit({
    seq: 1508380,
    score: 10,
    parts: [
      { type: 'test', condition: (_len, _txt, r) => r.route === 'kana' },
      { type: 'part', seqs: 2083990, lengthFn: () => 3 },
      { type: 'part', seqs: 2028980, lengthFn: () => 1 }
    ]
  });

  // Line 438-440: 中でも
  defSimpleSplit({
    seq: 2009290,
    score: 100,
    parts: [
      { type: 'part', seqs: 1423310, lengthFn: (len) => len - 2 },
      { type: 'part', seqs: 1008460, lengthFn: () => null }
    ]
  });

  // Line 442-444: 物好き
  defSimpleSplit({
    seq: 1502500,
    score: 100,
    parts: [
      { type: 'part', seqs: 1502390, lengthFn: (len) => len - 2 },
      { type: 'part', seqs: 1277450, lengthFn: () => 2, conjP: true }
    ]
  });

  // Line 446-449: かもしれない
  defSimpleSplit({
    seq: 1002970,
    score: 600,
    parts: [
      { type: 'test', condition: (_len, _txt, r) => r.route === 'kanji' },
      { type: 'part', seqs: 2143350, lengthFn: () => 2 },
      { type: 'part', seqs: ["知れない", 1420490], lengthFn: () => null }
    ]
  });

  // Line 451-452: しまった
  defSimpleSplit({
    seq: 1005600,
    score: -10,
    parts: [
      { type: 'part', seqs: ["しまった", 1305380], lengthFn: () => null }
    ]
  });

  // Line 454-455: やった
  defSimpleSplit({
    seq: 2016840,
    score: -5,
    parts: [
      { type: 'part', seqs: ["やった", 1012980], lengthFn: () => null }
    ]
  });

  // Line 457-458: あの
  defSimpleSplit({
    seq: 1000430,
    score: -5,
    parts: [
      { type: 'part', seqs: 1000420, lengthFn: () => null }
    ]
  });

  // Line 460-462: あのね
  defSimpleSplit({
    seq: 1612640,
    score: 5,
    parts: [
      { type: 'part', seqs: 1000420, lengthFn: () => 2 },
      { type: 'part', seqs: [2029080, 2029120, 1005110], lengthFn: () => null }
    ]
  });

  // Line 464-466: に+ない
  defSimpleSplit({
    seq: 1314600,
    score: -5,
    parts: [
      { type: 'part', seqs: 2028990, lengthFn: () => 1 },
      { type: 'part', seqs: 1529520, lengthFn: () => null, conjP: true }
    ]
  });

  // Line 468-470: に+ない
  defSimpleSplit({
    seq: 1322540,
    score: -5,
    parts: [
      { type: 'part', seqs: 2028990, lengthFn: () => 1 },
      { type: 'part', seqs: 1529520, lengthFn: () => null, conjP: true }
    ]
  });

  // Line 472-475: 気にします
  defSimpleSplit({
    seq: 1221680,
    score: 50,
    parts: [
      { type: 'part', seqs: 1221520, lengthFn: () => 1 },
      { type: 'part', seqs: 2028990, lengthFn: () => 1 },
      { type: 'part', seqs: 1157170, lengthFn: () => null, conjP: true }
    ]
  });

  // Line 477-480: わけがわからない
  defSimpleSplit({
    seq: 1538340,
    score: 50,
    parts: [
      { type: 'part', seqs: 1538330, lengthFn: (_len, txt) => txt.indexOf('が') },
      { type: 'part', seqs: 2028930, lengthFn: () => 1 },
      { type: 'part', seqs: 1606560, lengthFn: () => null, conjP: true }
    ]
  });

  // Line 482-485: わけのわからない
  defSimpleSplit({
    seq: 2757500,
    score: 50,
    parts: [
      { type: 'part', seqs: 1538330, lengthFn: (_len, txt) => txt.indexOf('の') },
      { type: 'part', seqs: 1469800, lengthFn: () => 1 },
      { type: 'part', seqs: 1606560, lengthFn: () => null, conjP: true }
    ]
  });

  // Line 487-489: 見たところ (commented out)
  // defSimpleSplit({
  //   seq: 1715710,
  //   score: 10,
  //   parts: [
  //     { type: 'part', seqs: ["見た", 1259290], lengthFn: () => 2 },
  //     { type: 'part', seqs: 1343100, lengthFn: () => null }
  //   ]
  // });

  // Line 491-493: 時には
  defSimpleSplit({
    seq: 1315860,
    score: 20,
    parts: [
      { type: 'part', seqs: 1315840, lengthFn: (len) => len - 2 },
      { type: 'part', seqs: 2215430, lengthFn: () => 2 }
    ]
  });

  // Line 495-498: 這います/います
  defSimpleSplit({
    seq: 1474200,
    score: -10,
    parts: [
      { type: 'test', condition: (_len, _txt, r) => r.route === 'kana' },
      { type: 'part', seqs: 2028920, lengthFn: () => 1 },
      { type: 'part', seqs: 1577980, lengthFn: () => null, conjP: true }
    ]
  });

  // Line 500-502: 尽くし
  defSimpleSplit({
    seq: 2276360,
    score: 10,
    parts: [
      { type: 'part', seqs: 2436480, lengthFn: (len) => len - 1 },
      { type: 'part', seqs: 2086640, lengthFn: () => 1 }
    ]
  });

  // Line 504-507: ことし
  defSimpleSplit({
    seq: 1579130,
    score: -1,
    parts: [
      { type: 'test', condition: (_len, txt) => txt === "ことし" },
      { type: 'part', seqs: 1313580, lengthFn: () => 2 },
      { type: 'part', seqs: 2086640, lengthFn: () => 1 }
    ]
  });

  // Line 509-512: 汗を流す
  defSimpleSplit({
    seq: 2668400,
    score: 50,
    parts: [
      { type: 'part', seqs: 1213060, lengthFn: (_len, txt) => txt.indexOf('を') },
      { type: 'part', seqs: 2029010, lengthFn: () => 1 },
      { type: 'part', seqs: 1552120, lengthFn: () => null, conjP: true }
    ]
  });

  // Line 514-517: 気がつく (duplicate - already defined at line 276-279, skip)

  // Line 519-521: 折りたたみ式
  defSimpleSplit({
    seq: 2835890,
    score: 50,
    parts: [
      { type: 'part', seqs: 1385860, lengthFn: () => 5 },
      { type: 'part', seqs: 1319060, lengthFn: () => 1 }
    ]
  });

  // Line 523-527: Helper function optprefix
  function optprefix(prefix: string): (txt: string) => string {
    return (txt: string) => {
      if (txt.startsWith(prefix)) {
        return txt;
      }
      return prefix + txt;
    };
  }

  // Line 529-533: ついてる
  defSimpleSplit({
    seq: 1894260,
    score: 50,
    parts: [
      { type: 'test', condition: (len) => len > 3 },
      { type: 'part', seqs: ["付いて", 1894260], lengthFn: () => 3 },
      { type: 'part', seqs: 1577980, lengthFn: () => null, conjP: true, modify: optprefix("い") }
    ]
  });

  // Line 535-536: 付いて
  defSimpleSplit({
    seq: 1854750,
    score: 20,
    parts: [
      { type: 'part', seqs: ["付いて", 1495740], lengthFn: () => null }
    ]
  });

  // Line 538-540: にしろ
  defSimpleSplit({
    seq: 2526850,
    score: 10,
    parts: [
      { type: 'part', seqs: 2028990, lengthFn: () => 1 },
      { type: 'part', seqs: ["しろ", 1157170], lengthFn: () => null }
    ]
  });

  // Line 542-544: にせよ
  defSimpleSplit({
    seq: 2026650,
    score: 10,
    parts: [
      { type: 'part', seqs: 2028990, lengthFn: () => 1 },
      { type: 'part', seqs: ["せよ", 1157170], lengthFn: () => null }
    ]
  });

  // Line 546-548: 普段着
  defSimpleSplit({
    seq: 1602740,
    score: 50,
    parts: [
      { type: 'part', seqs: 1497180, lengthFn: (len) => len - 1 },
      { type: 'part', seqs: 2093780, lengthFn: () => null }
    ]
  });

  // Line 550-552: なお
  defSimpleSplit({
    seq: 1349300,
    score: 5,
    parts: [
      { type: 'part', seqs: 2029110, lengthFn: () => 1 },
      { type: 'part', seqs: 2826528, lengthFn: () => null }
    ]
  });

  // Line 554-557: 気がある
  defSimpleSplit({
    seq: 1221530,
    score: 50,
    parts: [
      { type: 'part', seqs: 1221520, lengthFn: () => 1 },
      { type: 'part', seqs: 2028930, lengthFn: () => 1 },
      { type: 'part', seqs: 1296400, lengthFn: () => null, conjP: true }
    ]
  });

  // Line 559-562: 気がない
  defSimpleSplit({
    seq: 2272780,
    score: 50,
    parts: [
      { type: 'part', seqs: 1221520, lengthFn: () => 1 },
      { type: 'part', seqs: 2028930, lengthFn: () => 1 },
      { type: 'part', seqs: 1529520, lengthFn: () => null, conjP: true }
    ]
  });

  // Line 564-567: 気はない
  defSimpleSplit({
    seq: 2846470,
    score: 50,
    parts: [
      { type: 'part', seqs: 1221520, lengthFn: () => 1 },
      { type: 'part', seqs: 2028920, lengthFn: () => 1 },
      { type: 'part', seqs: 1529520, lengthFn: () => null, conjP: true }
    ]
  });

  // Line 569-573: 気を使う/気を遣う
  defSimpleSplit({
    seq: 1591980,
    score: 50,
    parts: [
      { type: 'part', seqs: 1221520, lengthFn: () => 1 },
      { type: 'part', seqs: 2029010, lengthFn: () => 1 },
      { type: 'part', seqs: 1305990, lengthFn: () => null, conjP: true }
    ]
  });

  // Line 575-577: 立ちすくむ
  defSimpleSplit({
    seq: 1551500,
    score: 50,
    parts: [
      { type: 'part', seqs: ["立ち", 1597040], lengthFn: () => 2 },
      { type: 'part', seqs: 1570220, lengthFn: () => null, conjP: true }
    ]
  });

  // Line 579-581: 零れ落ちる
  defSimpleSplit({
    seq: 2002270,
    score: 50,
    parts: [
      { type: 'part', seqs: ["零れ", 1557650], lengthFn: (_len, txt) => txt.indexOf('れ') + 1 },
      { type: 'part', seqs: 1548550, lengthFn: () => null, conjP: true }
    ]
  });

  // Line 583-586: につく
  defSimpleSplit({
    seq: 1314770,
    score: -10,
    parts: [
      { type: 'test', condition: (_len, _txt, r) => r.route === 'kana' },
      { type: 'part', seqs: 2028990, lengthFn: () => 1 },
      { type: 'part', seqs: 1495740, lengthFn: () => null, conjP: true }
    ]
  });

  // Line 588-589: つい (with :score marker)
  defSimpleSplit({
    seq: 1008030,
    score: -10,
    parts: [
      { type: 'marker', marker: ':score' }
    ]
  });

  // Line 591-594: ついたて
  defSimpleSplit({
    seq: 1597740,
    score: 5,
    parts: [
      { type: 'test', condition: (_len, _txt, r) => r.route === 'kana' },
      { type: 'part', seqs: 1008030, lengthFn: () => 2 },
      { type: 'part', seqs: 2081610, lengthFn: () => null }
    ]
  });

  // Line 596-601: 雪がない (complex with dual :test)
  defSimpleSplit({
    seq: 1581550,
    score: 10,
    parts: [
      { type: 'test', condition: (_len, txt) => txt.startsWith("雪") },
      { type: 'part', seqs: 1386500, lengthFn: () => 1 },
      { type: 'part', seqs: 2028930, lengthFn: () => 1 },
      { type: 'test', condition: (len) => len > 2, newScore: -2, pushOnFail: ':pscore' },
      { type: 'part', seqs: 1529520, lengthFn: () => null, conjP: true }
    ]
  });

  // Line 603-605: はやめる
  defSimpleSplit({
    seq: 1601080,
    score: -5,
    parts: [
      { type: 'part', seqs: 2028920, lengthFn: () => 1 },
      { type: 'part', seqs: 1310680, lengthFn: () => null, conjP: true }
    ]
  });

  // Line 607-609: 者ども
  defSimpleSplit({
    seq: 2529050,
    score: 30,
    parts: [
      { type: 'part', seqs: 1322990, lengthFn: (_len, txt) => txt.startsWith("もの") ? 2 : 1 },
      { type: 'part', seqs: 1234250, lengthFn: () => null }
    ]
  });

  // Line 611-613: すると
  defSimpleSplit({
    seq: 1006280,
    score: 30,
    parts: [
      { type: 'part', seqs: 1157170, lengthFn: () => 2 },
      { type: 'part', seqs: 1008490, lengthFn: () => 1 }
    ]
  });

  // Line 615-617: 出しな
  defSimpleSplit({
    seq: 2757540,
    score: 90,
    parts: [
      { type: 'part', seqs: 1896380, lengthFn: () => 1 },
      { type: 'part', seqs: 2728200, lengthFn: () => null }
    ]
  });

  // Line 619-621: わかりきる
  defSimpleSplit({
    seq: 1606530,
    score: 100,
    parts: [
      { type: 'part', seqs: ["分かり", 1606560], lengthFn: () => 3 },
      { type: 'part', seqs: 1384830, lengthFn: () => null, conjP: true }
    ]
  });

  // Line 623-625: 落ちこぼれる
  defSimpleSplit({
    seq: 2007500,
    score: 100,
    parts: [
      { type: 'part', seqs: ["落ち", 1548550], lengthFn: () => 2 },
      { type: 'part', seqs: 1557650, lengthFn: () => null, conjP: true }
    ]
  });

  // Line 627-629: あけましておめでとうございます
  defSimpleSplit({
    seq: 1532270,
    score: 100,
    parts: [
      { type: 'part', seqs: ["あけまして", 1202450], lengthFn: () => 5 },
      { type: 'part', seqs: 1001540, lengthFn: () => null }
    ]
  });

  // Line 631-633: よろしくおねがいします
  defSimpleSplit({
    seq: 2133750,
    score: 100,
    parts: [
      { type: 'part', seqs: 1224890, lengthFn: (_len, txt) => txt.indexOf('く') + 1 },
      { type: 'part', seqs: 1001720, lengthFn: () => null }
    ]
  });

  // Line 635-638: 俺たち
  defSimpleSplit({
    seq: 1863230,
    score: 15,
    parts: [
      { type: 'test', condition: (_len, _txt, r) => r.route === 'kana' },
      { type: 'part', seqs: 1576870, lengthFn: () => 2 },
      { type: 'part', seqs: 1416220, lengthFn: () => null }
    ]
  });

  // Line 640-643: お前たち
  defSimpleSplit({
    seq: 2834051,
    score: 15,
    parts: [
      { type: 'test', condition: (_len, _txt, r) => r.route === 'kana' },
      { type: 'part', seqs: 1002290, lengthFn: () => 3 },
      { type: 'part', seqs: 1416220, lengthFn: () => null }
    ]
  });

  // Line 646-648: 割り
  defSimpleSplit({
    seq: 1606800,
    score: 10,
    parts: [
      { type: 'test', condition: (len) => len === 2 },
      { type: 'part', seqs: ["割り", 1208000], lengthFn: () => null }
    ]
  });

  // Line 650-652: 割り切れる
  defSimpleSplit({
    seq: 1207840,
    score: 50,
    parts: [
      { type: 'part', seqs: ["割り", 1208000], lengthFn: () => 2 },
      { type: 'part', seqs: 1384860, lengthFn: () => null, conjP: true }
    ]
  });

  // Line 654-656: あり得ない
  defSimpleSplit({
    seq: 2109610,
    score: 50,
    parts: [
      { type: 'part', seqs: ["有り", 1296400], lengthFn: () => 2 },
      { type: 'part', seqs: 1588760, lengthFn: () => null, conjP: true }
    ]
  });

  // Line 658-660: なので (commented out)
  // defSimpleSplit({
  //   seq: 2827864,
  //   score: 100,
  //   parts: [
  //     { type: 'part', seqs: 2029110, lengthFn: () => 1 },
  //     { type: 'part', seqs: 1009970, lengthFn: () => 2 }
  //   ]
  // });

  // Line 662-665: につまる
  defSimpleSplit({
    seq: 1322560,
    score: -10,
    parts: [
      { type: 'test', condition: (_len, _txt, r) => r.route === 'kana' },
      { type: 'part', seqs: 2028990, lengthFn: () => 1 },
      { type: 'part', seqs: 1226480, lengthFn: () => null, conjP: true }
    ]
  });

  // Line 667-669: その上
  defSimpleSplit({
    seq: 1006880,
    score: 50,
    parts: [
      { type: 'part', seqs: 1006830, lengthFn: () => 2 },
      { type: 'part', seqs: 1352130, lengthFn: () => null }
    ]
  });

  // Line 671-673: はね上がる
  defSimpleSplit({
    seq: 1601010,
    score: 50,
    parts: [
      { type: 'part', seqs: ["跳ね", 1429620], lengthFn: () => 2 },
      { type: 'part', seqs: 1352290, lengthFn: () => null, conjP: true }
    ]
  });

  // Line 675-678: 写真を撮る
  defSimpleSplit({
    seq: 1881690,
    score: 50,
    parts: [
      { type: 'part', seqs: 1321900, lengthFn: (_len, txt) => txt.indexOf('を') },
      { type: 'part', seqs: 2029010, lengthFn: () => 1 },
      { type: 'part', seqs: 1298790, lengthFn: () => null, conjP: true }
    ]
  });

  // Line 680-681: 取り留め
  defSimpleSplit({
    seq: 2834732,
    score: -10,
    parts: [
      { type: 'part', seqs: 1707770, lengthFn: () => null, conjP: true }
    ]
  });

  // Line 685-688: 取り留めのない
  defSimpleSplit({
    seq: 1855670,
    score: 50,
    parts: [
      { type: 'part', seqs: ["取り留め", 1707770], lengthFn: (_len, txt) => txt.indexOf('の') },
      { type: 'part', seqs: 1469800, lengthFn: () => 1 },
      { type: 'part', seqs: 1529520, lengthFn: () => null }
    ]
  });

  // Line 690-693: 取り留めもない
  defSimpleSplit({
    seq: 2855921,
    score: 50,
    parts: [
      { type: 'part', seqs: ["取り留め", 1707770], lengthFn: (_len, txt) => txt.indexOf('も') },
      { type: 'part', seqs: 2028940, lengthFn: () => 1 },
      { type: 'part', seqs: 1529520, lengthFn: () => null }
    ]
  });

}
