import type { SplitRegistration } from './analyzer-support-annotation-model.js';

/**
 * First half of the qualified legacy split declarations. These are intended
 * analyzer semantics transcribed from upstream dict-split.lisp, not database
 * output. Keeping the declarations separate from evaluation makes provenance
 * review possible without carrying reference runtime helpers into the compiler.
 */
export function addLegacySplitDeclarationsA(defSimpleSplit: SplitRegistration): void {
  // Line 96-100: defmacro def-de-split
  // Helper function to create de-split definitions
  function defDeSplit(seq: number, seqA: number, score: number = 20): void {
    defSimpleSplit({
      seq,
      score,
      parts: [
        { type: 'part', seqs: seqA, lengthFn: (len) => len - 1 },
        { type: 'part', seqs: 2028980, lengthFn: () => 1 }  // で particle
      ]
    });
  }

  // Line 102-133: def-de-split instances
  defDeSplit(1163700, 1576150); // 一人で
  defDeSplit(1611020, 1577100); // 何で
  defDeSplit(1004800, 1628530); // これで
  defDeSplit(2810720, 1004820); // 此れまでで
  defDeSplit(1006840, 1006880); // その上で
  defDeSplit(1530610, 1530600); // 無断で
  defDeSplit(1245390, 1245290); // 空で
  defDeSplit(2719270, 1445430); // 土足で
  defDeSplit(1189420, 2416780); // 何用で
  defDeSplit(1272220, 1592990); // 交代で
  defDeSplit(1311360, 1311350); // 私費で
  defDeSplit(1368500, 1368490); // 人前で
  defDeSplit(1395670, 1395660); // 全体で
  defDeSplit(1417790, 1417780); // 単独で
  defDeSplit(1454270, 1454260); // 道理で
  defDeSplit(1479100, 1679020); // 半眼で
  defDeSplit(1510140, 1680900); // 別封で
  defDeSplit(1518550, 1529560); // 無しで
  defDeSplit(1531420, 1531410); // 名義で
  defDeSplit(1597400, 1585205); // 力尽くで
  defDeSplit(1679990, 2582460); // 抜き足で
  defDeSplit(1682060, 2085340); // 金ずくで
  defDeSplit(1736650, 1611710); // 水入らずで
  defDeSplit(1865020, 1590150); // 陰で
  defDeSplit(1878880, 2423450); // 差しで
  defDeSplit(2126220, 1802920); // 捩じり鉢巻きで
  defDeSplit(2136520, 2005870); // もう少しで
  defDeSplit(2513590, 2513650); // 詰め開きで
  defDeSplit(2771850, 2563780); // 気にしないで
  defDeSplit(2810800, 1587590); // 今までで
  defDeSplit(1343110, 1343100); // ところで
  defDeSplit(1270210, 1001640); // お陰で

  // Line 135-137: でございます special case
  defSimpleSplit({
    seq: 2253080,
    score: 20,
    parts: [
      { type: 'part', seqs: 2028980, lengthFn: () => 1 },  // で
      { type: 'part', seqs: 1612690, lengthFn: () => null, conjP: true }  // ございます
    ]
  });

  // Line 139-144: defmacro def-toori-split
  // Helper function for 通り (toori) compounds
  function defTooriSplit(seq: number, seqA: number, seqB: number = 1432930, score: number = 50): void {
    defSimpleSplit({
      seq,
      score,
      parts: [
        {
          type: 'test',
          condition: (_len, _txt, reading) => reading.route === 'kanji'
        },
        { type: 'part', seqs: seqA, lengthFn: (len) => len - 2 },
        { type: 'part', seqs: seqB, lengthFn: () => 2 }
      ]
    });
  }

  // Line 146-169: def-toori-split instances
  defTooriSplit(1260990, 1260670); // 元通り
  defTooriSplit(1414570, 2082450); // 大通り
  defTooriSplit(1424950, 1620400); // 中通り [ちゅう通り]
  defTooriSplit(1424960, 1423310); // 中通り [なか通り]
  defTooriSplit(1820790, 1250090); // 型通り
  defTooriSplit(1489800, 1489340); // 表通り
  defTooriSplit(1523010, 1522150); // 本通り
  defTooriSplit(1808080, 1604890); // 目通り
  defTooriSplit(1368820, 1580640); // 人通り
  defTooriSplit(1550490, 1550190); // 裏通り
  defTooriSplit(1619440, 2069220); // 素通り
  defTooriSplit(1164910, 2821500, 1432920); // 一通り
  defTooriSplit(1462720, 1461140, 1432920); // 二通り

  // Line 171-175: defmacro def-do-split
  // Helper function for ど (do) prefix compounds
  function defDoSplit(seq: number, seqB: number, seqA: number = 2252690, score: number = 30): void {
    defSimpleSplit({
      seq,
      score,
      parts: [
        { type: 'part', seqs: seqA, lengthFn: () => 1 },
        { type: 'part', seqs: seqB, lengthFn: () => null }
      ]
    });
  }

  // Line 177-183: def-do-split instances
  defDoSplit(2142710, 1185200); // ど下手
  defDoSplit(2803190, 1595630); // どすけべ
  defDoSplit(2142680, 1290210); // ど根性
  defDoSplit(2523480, 1442750); // ど田舎

  // Line 194-198: defmacro def-shi-split
  // Helper function for し (shi) stem + verb compounds
  function defShiSplit(seq: number, seqB: number, seqA: [string, ...number[]] = ["し", 1157170], score: number = 30): void {
    defSimpleSplit({
      seq,
      score,
      parts: [
        { type: 'part', seqs: seqA, lengthFn: () => 1 },
        { type: 'part', seqs: seqB, lengthFn: () => null, conjP: true }
      ]
    });
  }

  // Line 200-219: def-shi-split instances
  defShiSplit(1005700, 1156990); // し易い
  defShiSplit(1005830, 1370760); // し吹く
  defShiSplit(1157200, 2772730); // し難い
  defShiSplit(1157220, 1195970); // し過ぎる
  defShiSplit(1157230, 1284430); // し合う
  defShiSplit(1157280, 1370090); // し尽す
  defShiSplit(1157310, 1405800); // し続ける
  defShiSplit(1304890, 1256520); // し兼ねる
  defShiSplit(1304960, 1307550); // し始める
  defShiSplit(1305110, 1338180); // し出す
  defShiSplit(1305280, 1599390); // し直す
  defShiSplit(1305290, 1212670); // し慣れる
  defShiSplit(1594300, 1596510); // し損なう
  defShiSplit(1594310, 1406680); // し損じる
  defShiSplit(1594460, 1372620); // し遂げる
  defShiSplit(1594580, 1277100); // し向ける
  defShiSplit(2518250, 1332760); // し終える
  defShiSplit(1157240, 1600260); // し残す
  defShiSplit(1304820, 1207610); // し掛ける
  defShiSplit(2858937, 1406690); // し損ねる

  // ============================================================================
  // Individual def-simple-split definitions (Lines 221-800)
  // ============================================================================

  // Line 223-225: split-nakunaru - 無くなる
  defSimpleSplit({
    seq: 1529550,
    score: 30,
    parts: [
      { type: 'part', seqs: ["無く", 1529520], lengthFn: () => 2 },
      { type: 'part', seqs: 1375610, lengthFn: () => null, conjP: true }
    ]
  });

  // Line 227-230: split-nakunaru2 - 亡くなる
  defSimpleSplit({
    seq: 1518540,
    score: 10,
    parts: [
      { type: 'test', condition: (_len, _txt, r) => r.route === 'kana' },
      { type: 'part', seqs: ["亡く", 1518450], lengthFn: () => 2 },
      { type: 'part', seqs: 1375610, lengthFn: () => null, conjP: true }
    ]
  });

  // Line 234-237: split-tegakakaru - 手が掛かる
  defSimpleSplit({
    seq: 2089710,
    score: 10,
    parts: [
      { type: 'part', seqs: 1327190, lengthFn: () => 1 },  // 手
      { type: 'part', seqs: 2028930, lengthFn: () => 1 },  // が
      { type: 'part', seqs: 1207590, lengthFn: () => null, conjP: true }
    ]
  });

  // Line 240-242: split-kawaribae - 代わり映え
  defSimpleSplit({
    seq: 1411570,
    score: 10,
    parts: [
      { type: 'part', seqs: [1590770, 1510720], lengthFn: (_len, txt) => txt.indexOf('り') + 1 },
      { type: 'part', seqs: ["映え", 1600620], lengthFn: () => 2 }
    ]
  });

  // Line 244-247: split-hayaimonode - 早いもので
  defSimpleSplit({
    seq: 2815260,
    score: 100,
    parts: [
      { type: 'part', seqs: 1404975, lengthFn: (_len, txt) => txt.indexOf('い') + 1 },
      { type: 'part', seqs: 1502390, lengthFn: (_len, txt) => txt.includes('物') ? 1 : 2 },
      { type: 'part', seqs: 2028980, lengthFn: () => 1 }
    ]
  });

  // Line 249-252: split-dogatsukeru - ドが付ける
  defSimpleSplit({
    seq: 2800540,
    score: 30,
    parts: [
      { type: 'part', seqs: 2252690, lengthFn: () => 1 },
      { type: 'part', seqs: 2028930, lengthFn: () => 1 },
      { type: 'part', seqs: 1495740, lengthFn: () => null, conjP: true }
    ]
  });

  // Line 254-256: split-janaika - じゃないか
  defSimpleSplit({
    seq: 2819990,
    score: 20,
    parts: [
      { type: 'part', seqs: ["じゃない", 2089020], lengthFn: () => 4 },
      { type: 'part', seqs: 2028970, lengthFn: () => 1 }
    ]
  });

  // Line 258-261: split-kaasan - 母さん
  defSimpleSplit({
    seq: 1609470,
    score: 50,
    parts: [
      { type: 'test', condition: (_len, _txt, r) => r.route === 'kanji' },
      { type: 'part', seqs: 1514990, lengthFn: () => 1 },
      { type: 'part', seqs: 1005340, lengthFn: () => 2 }
    ]
  });

  // Line 263-265: split-souda - そうだ
  defSimpleSplit({
    seq: 1006650,
    score: 5,
    parts: [
      { type: 'part', seqs: 2137720, lengthFn: () => 2 },
      { type: 'part', seqs: 2089020, lengthFn: () => null }
    ]
  });

  // Line 267-269: split-soudesu - そうです
  defSimpleSplit({
    seq: 2837492,
    score: 5,
    parts: [
      { type: 'part', seqs: 2137720, lengthFn: () => 2 },
      { type: 'part', seqs: 1628500, lengthFn: () => null }
    ]
  });

  // Line 271-274: split-kinosei - 気のせい
  defSimpleSplit({
    seq: 1221750,
    score: 100,
    parts: [
      { type: 'part', seqs: 1221520, lengthFn: () => 1 },
      { type: 'part', seqs: 1469800, lengthFn: () => 1 },
      { type: 'part', seqs: 1610040, lengthFn: () => 2 }
    ]
  });

  // Line 276-279: split-kigatsuku - 気がつく
  defSimpleSplit({
    seq: 1591050,
    score: 100,
    parts: [
      { type: 'part', seqs: 1221520, lengthFn: () => 1 },
      { type: 'part', seqs: 2028930, lengthFn: () => 1 },
      { type: 'part', seqs: 1495740, lengthFn: () => null, conjP: true }
    ]
  });

  // Line 281-283: split-nanimokamo - なにもかも
  defSimpleSplit({
    seq: 1599590,
    score: 20,
    parts: [
      { type: 'part', seqs: 1188490, lengthFn: (len) => len - 2 },
      { type: 'part', seqs: 2143350, lengthFn: () => 2 }
    ]
  });

  // Line 285-288: split-katawonaraberu - 肩を並べる
  defSimpleSplit({
    seq: 2102910,
    score: 20,
    parts: [
      { type: 'part', seqs: 1258950, lengthFn: (_len, txt) => txt.indexOf('を') },
      { type: 'part', seqs: 2029010, lengthFn: () => 1 },
      { type: 'part', seqs: 1508390, lengthFn: () => null, conjP: true }
    ]
  });

  // Line 290-292: split-moushiwakenasasou - 申し訳なさそう
  defSimpleSplit({
    seq: 2057340,
    score: 300,
    parts: [
      { type: 'part', seqs: 1363050, lengthFn: (_len, txt) => txt.indexOf('な') },
      { type: 'part', seqs: 2246510, lengthFn: () => null }
    ]
  });

  // Line 294-295: split-kimatte - 決まって
  defSimpleSplit({
    seq: 1951150,
    score: 50,
    parts: [
      { type: 'part', seqs: ["決まって", 1591420], lengthFn: () => null }
    ]
  });

  // Line 297-299: split-osoreiru - 恐れ入る
  defSimpleSplit({
    seq: 1236680,
    score: 100,
    parts: [
      { type: 'part', seqs: 1236660, lengthFn: (_len, txt) => txt.indexOf('れ') + 1 },
      { type: 'part', seqs: 1465580, lengthFn: () => null, conjP: true }
    ]
  });

  // Line 301-303: split-nantokanaru - なんとかなる
  defSimpleSplit({
    seq: 2104540,
    score: 20,
    parts: [
      { type: 'part', seqs: 1188420, lengthFn: (_len, txt) => txt.indexOf('か') + 1 },
      { type: 'part', seqs: 1375610, lengthFn: () => null, conjP: true }
    ]
  });

  // Line 305-307: split-hajiketobu - 弾け飛ぶ
  defSimpleSplit({
    seq: 2610760,
    score: 50,
    parts: [
      { type: 'part', seqs: ["弾け", 1419380], lengthFn: (_len, txt) => txt.indexOf('け') + 1 },
      { type: 'part', seqs: 1429700, lengthFn: () => null, conjP: true }
    ]
  });

  // Line 309-311: split-motteiku - 持って行く
  defSimpleSplit({
    seq: 1315700,
    score: 50,
    parts: [
      { type: 'part', seqs: ["持って", 1315720], lengthFn: (_len, txt) => txt.indexOf('て') + 1 },
      { type: 'part', seqs: 1578850, lengthFn: () => null, conjP: true }
    ]
  });

  // Line 313-316: split-hairikomeru - 入り込める
  defSimpleSplit({
    seq: 1465460,
    score: 100,
    parts: [
      { type: 'test', condition: (_len, _txt, r) => r.route === 'kanji' },
      { type: 'part', seqs: ["入り", 1465590], lengthFn: (_len, txt) => txt.indexOf('り') + 1 },
      { type: 'part', seqs: 1288790, lengthFn: () => null, conjP: true }
    ]
  });

  // Line 318-321: split-shinikakaru - 死に掛かる
  defSimpleSplit({
    seq: 1881080,
    score: 30,
    parts: [
      { type: 'part', seqs: 1310720, lengthFn: () => 1 },
      { type: 'part', seqs: 2028990, lengthFn: () => 1 },
      { type: 'part', seqs: 1207590, lengthFn: () => null, conjP: true }
    ]
  });

  // Line 323-326: split-hisshininatte - 必死になって
  defSimpleSplit({
    seq: 1903910,
    score: 50,
    parts: [
      { type: 'part', seqs: 1601890, lengthFn: (_len, txt) => txt.indexOf('に') },
      { type: 'part', seqs: 2028990, lengthFn: () => 1 },
      { type: 'part', seqs: ["なって", 1375610], lengthFn: () => null }
    ]
  });

  // Line 328-330: split-nitotte - にとって
  defSimpleSplit({
    seq: 1009600,
    score: 50,
    parts: [
      { type: 'part', seqs: 2028990, lengthFn: () => 1 },
      { type: 'part', seqs: ["取って", 1326980], lengthFn: () => null }
    ]
  });

  // Line 332-333: i don't remember why this was here
  // defSimpleSplit({
  //   seq: 1009610,
  //   score: 0,  // split-nimo
  //   parts: []
  // });

  // Line 335-338: split-kotonisuru - 事にする
  defSimpleSplit({
    seq: 2215340,
    score: 100,
    parts: [
      { type: 'part', seqs: 1313580, lengthFn: (_len, txt) => txt.indexOf('に') },
      { type: 'part', seqs: 2028990, lengthFn: () => 1 },
      { type: 'part', seqs: 1157170, lengthFn: () => null, conjP: true }
    ]
  });

  // Line 340-342: split-hajikidasu - 弾き出す
  defSimpleSplit({
    seq: 1419350,
    score: 100,
    parts: [
      { type: 'part', seqs: 1901710, lengthFn: (_len, txt) => txt.indexOf('き') + 1 },
      { type: 'part', seqs: 1338180, lengthFn: () => null, conjP: true }
    ]
  });

  // Line 344-346: split-hitotachi - 人たち
  defSimpleSplit({
    seq: 1368740,
    score: 100,
    parts: [
      { type: 'part', seqs: 1580640, lengthFn: (_len, txt) => txt.indexOf('人') >= 0 ? 1 : 2 },
      { type: 'part', seqs: 1416220, lengthFn: (_len, txt) => txt.indexOf('達') >= 0 ? 1 : 2 }
    ]
  });

  // Line 348-350: split-desura - でさえ/ですら
  defSimpleSplit({
    seq: 2034520,
    score: 30,
    parts: [
      { type: 'part', seqs: 2028980, lengthFn: () => 1 },
      { type: 'part', seqs: [2827091], lengthFn: () => null }
    ]
  });

  // Line 352-354: split-gotoni - ごとに
  defSimpleSplit({
    seq: 1524660,
    score: 50,
    parts: [
      { type: 'part', seqs: 1524640, lengthFn: (_len, txt) => txt.indexOf('に') },
      { type: 'part', seqs: 2028990, lengthFn: () => 1 }
    ]
  });

  // Line 356-358: split-osagari - お下がり
  defSimpleSplit({
    seq: 1693800,
    score: 50,
    parts: [
      { type: 'part', seqs: 2826528, lengthFn: () => 1 },
      { type: 'part', seqs: 1609810, lengthFn: () => null }
    ]
  });

  // Line 360-362: split-kaisasae - 買い支え
  defSimpleSplit({
    seq: 1752860,
    score: 50,
    parts: [
      { type: 'part', seqs: 1636070, lengthFn: () => 2 },
      { type: 'part', seqs: ["支え", 1310090], lengthFn: () => null }
    ]
  });

  // Line 364-366: split-toiu - という
  defSimpleSplit({
    seq: 1922760,
    score: 20,
    parts: [
      { type: 'part', seqs: 1008490, lengthFn: () => 1 },
      { type: 'part', seqs: 1587040, lengthFn: () => null, conjP: true }
    ]
  });

  // Line 368-371: split-toiukotoda - ということだ
  defSimpleSplit({
    seq: 2612990,
    score: 30,
    parts: [
      { type: 'part', seqs: 1922760, lengthFn: () => 3 },
      { type: 'part', seqs: 1313580, lengthFn: (len) => len - 4 },
      { type: 'part', seqs: 2089020, lengthFn: () => null }
    ]
  });

  // Line 373-375: split-tonattara - となったら
  defSimpleSplit({
    seq: 2100770,
    score: 50,
    parts: [
      { type: 'part', seqs: 1008490, lengthFn: () => 1 },
      { type: 'part', seqs: ["なったら", 1375610], lengthFn: () => null }
    ]
  });

  // Line 377-379: split-tonaru - となる
  defSimpleSplit({
    seq: 2100900,
    score: 10,
    parts: [
      { type: 'part', seqs: 1008490, lengthFn: () => 1 },
      { type: 'part', seqs: 1375610, lengthFn: () => null, conjP: true }
    ]
  });

  // Line 381-384: 手に入る
  defSimpleSplit({
    seq: 1327220,
    score: 50,
    parts: [
      { type: 'part', seqs: 1327190, lengthFn: () => 1 },
      { type: 'part', seqs: 2028990, lengthFn: () => 1 },
      { type: 'part', seqs: 1465590, lengthFn: () => null, conjP: true }
    ]
  });

  // Line 386-389: 手に入れる
  defSimpleSplit({
    seq: 1327230,
    score: 50,
    parts: [
      { type: 'part', seqs: 1327190, lengthFn: () => 1 },
      { type: 'part', seqs: 2028990, lengthFn: () => 1 },
      { type: 'part', seqs: 1465610, lengthFn: () => null, conjP: true }
    ]
  });

  // Line 391-393: そうなんです
  defSimpleSplit({
    seq: 2433760,
    score: 50,
    parts: [
      { type: 'part', seqs: 1006610, lengthFn: () => 2 },
      { type: 'part', seqs: 2683060, lengthFn: () => null }
    ]
  });

  // Line 395-397: 良さげ
  defSimpleSplit({
    seq: 2088480,
    score: 20,
    parts: [
      { type: 'part', seqs: 1634130, lengthFn: () => 2 },
      { type: 'part', seqs: 2006580, lengthFn: () => 1 }
    ]
  });

  // Line 399-402: のせいで
  defSimpleSplit({
    seq: 2724560,
    score: 30,
    parts: [
      { type: 'part', seqs: 1469800, lengthFn: () => 1 },
      { type: 'part', seqs: 1610040, lengthFn: (len) => len - 2 },
      { type: 'part', seqs: 2028980, lengthFn: () => 1 }
    ]
  });

  // Line 404-405: 少なくない
  defSimpleSplit({
    seq: 2666360,
    score: 30,
    parts: [
      { type: 'part', seqs: ["少なくない", 1348910], lengthFn: () => null }
    ]
  });

  // Line 407-409: split-janai - じゃない
  defSimpleSplit({
    seq: 2755350,
    score: 10,
    parts: [
      { type: 'part', seqs: 2089020, lengthFn: () => 2 },
      { type: 'part', seqs: 1529520, lengthFn: () => null, conjP: true }
    ]
  });

  // Line 411-413: split-jan - じゃん
  defSimpleSplit({
    seq: 2135280,
    score: 10,
    parts: [
      { type: 'part', seqs: 2089020, lengthFn: () => 2 },
      { type: 'part', seqs: 2139720, lengthFn: () => 1 }
    ]
  });


}
