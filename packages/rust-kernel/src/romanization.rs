pub(crate) fn romanize(input: &str) -> String {
    let pairs = [
        ("きゃ", "kya"),
        ("きゅ", "kyu"),
        ("きょ", "kyo"),
        ("しゃ", "sha"),
        ("しゅ", "shu"),
        ("しょ", "sho"),
        ("ちゃ", "cha"),
        ("ちゅ", "chu"),
        ("ちょ", "cho"),
        ("にゃ", "nya"),
        ("にゅ", "nyu"),
        ("にょ", "nyo"),
        ("ひゃ", "hya"),
        ("ひゅ", "hyu"),
        ("ひょ", "hyo"),
        ("みゃ", "mya"),
        ("みゅ", "myu"),
        ("みょ", "myo"),
        ("りゃ", "rya"),
        ("りゅ", "ryu"),
        ("りょ", "ryo"),
        ("ぎゃ", "gya"),
        ("ぎゅ", "gyu"),
        ("ぎょ", "gyo"),
        ("じゃ", "ja"),
        ("じゅ", "ju"),
        ("じょ", "jo"),
        ("びゃ", "bya"),
        ("びゅ", "byu"),
        ("びょ", "byo"),
        ("ぴゃ", "pya"),
        ("ぴゅ", "pyu"),
        ("ぴょ", "pyo"),
    ];
    let singles = [
        ('あ', "a"),
        ('い', "i"),
        ('う', "u"),
        ('え', "e"),
        ('お', "o"),
        ('か', "ka"),
        ('き', "ki"),
        ('く', "ku"),
        ('け', "ke"),
        ('こ', "ko"),
        ('さ', "sa"),
        ('し', "shi"),
        ('す', "su"),
        ('せ', "se"),
        ('そ', "so"),
        ('た', "ta"),
        ('ち', "chi"),
        ('つ', "tsu"),
        ('て', "te"),
        ('と', "to"),
        ('な', "na"),
        ('に', "ni"),
        ('ぬ', "nu"),
        ('ね', "ne"),
        ('の', "no"),
        ('は', "ha"),
        ('ひ', "hi"),
        ('ふ', "fu"),
        ('へ', "he"),
        ('ほ', "ho"),
        ('ま', "ma"),
        ('み', "mi"),
        ('む', "mu"),
        ('め', "me"),
        ('も', "mo"),
        ('や', "ya"),
        ('ゆ', "yu"),
        ('よ', "yo"),
        ('ら', "ra"),
        ('り', "ri"),
        ('る', "ru"),
        ('れ', "re"),
        ('ろ', "ro"),
        ('わ', "wa"),
        ('を', "o"),
        ('ん', "n"),
        ('が', "ga"),
        ('ぎ', "gi"),
        ('ぐ', "gu"),
        ('げ', "ge"),
        ('ご', "go"),
        ('ざ', "za"),
        ('じ', "ji"),
        ('ず', "zu"),
        ('ぜ', "ze"),
        ('ぞ', "zo"),
        ('だ', "da"),
        ('ぢ', "ji"),
        ('づ', "zu"),
        ('で', "de"),
        ('ど', "do"),
        ('ば', "ba"),
        ('び', "bi"),
        ('ぶ', "bu"),
        ('べ', "be"),
        ('ぼ', "bo"),
        ('ぱ', "pa"),
        ('ぴ', "pi"),
        ('ぷ', "pu"),
        ('ぺ', "pe"),
        ('ぽ', "po"),
    ];
    let pair_map: std::collections::HashMap<&str, &str> = pairs.into_iter().collect();
    let single_map: std::collections::HashMap<char, &str> = singles.into_iter().collect();
    let chars: Vec<char> = input.chars().collect();
    let mut result = String::new();
    let mut index = 0;
    let mut geminate = false;
    while index < chars.len() {
        if chars[index] == 'っ' {
            geminate = true;
            index += 1;
            continue;
        }
        let pair = (index + 1 < chars.len()).then(|| {
            let mut value = String::new();
            value.push(chars[index]);
            value.push(chars[index + 1]);
            value
        });
        let (syllable, width) = pair
            .as_deref()
            .and_then(|value| pair_map.get(value).copied())
            .map_or_else(
                || {
                    (
                        single_map
                            .get(&chars[index])
                            .copied()
                            .map(str::to_owned)
                            .unwrap_or_else(|| chars[index].to_string()),
                        1,
                    )
                },
                |value| (value.to_owned(), 2),
            );
        if geminate {
            if let Some(first) = syllable.chars().next() {
                result.push(first);
            }
            geminate = false;
        }
        result.push_str(&syllable);
        index += width;
    }
    result
}

#[cfg(test)]
mod tests {
    use super::romanize;

    #[test]
    fn romanizes_m1_words() {
        assert_eq!(romanize("ねこ"), "neko");
        assert_eq!(romanize("たべた"), "tabeta");
    }
}
