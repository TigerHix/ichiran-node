import { ConjInfoJson, WordInfoGlossJson } from "@ichiran/core"
import { WordInfo } from "@ichiran/core"
import { TransformedRomanizeStarResult, TransformedRomanizeStarResultTokenTuple } from "@ichiran/core"
import { RomanizeStarResult, RomanizeStarResultTokenTuple } from "@ichiran/core"
import { IchiranPos } from "./pos.js"

export interface TransformedRomanizeStarToken {
    word: string
    romanized: string
    info?: WordInfo
    infoJson?: WordInfoGlossJson
    alternatives: any[]
}

export interface GrammarInfo {
    isConjugation: boolean
    conjugationTypes?: string[]
    neg?: boolean
    fml?: boolean
    hasConjugationVia: boolean
    isConjugationVia: boolean
    isAlternative: boolean
    isComponent: boolean
    isSuffix: boolean
    suffix?: string
    word: string
    reading: string
    partOfSpeech: IchiranPos[]
    meanings: Array<{
        text: string;
        partOfSpeech: IchiranPos[];
        info: string
    }>
    conjugations: GrammarInfo[]
    alternatives: GrammarInfo[]
    components: GrammarInfo[]
}

/**
 * Parse a single segmentation alternative from romanizeStar result.
 * 
 * @param result - Raw romanizeStar result
 * @param transformedResult - Transformed romanizeStar result
 * @param segmentIndex - Which segment to get alternatives from (usually 0)
 * @param alternativeIndex - Which alternative to parse from that segment
 * @returns Array of tokens for this specific segmentation alternative
 */
function parseTokenAlternative(
    result: RomanizeStarResult,
    transformedResult: TransformedRomanizeStarResult,
    segmentIndex: number,
    alternativeIndex: number
): TransformedRomanizeStarToken[] {
    const tokens: TransformedRomanizeStarToken[] = []
    
    for (let i = 0; i < result.length; i++) {
        const res = result[i]
        const transformedRes = transformedResult[i]

        if (Array.isArray(transformedRes) && Array.isArray(res)) {
            // This is a word segment with potentially multiple alternatives
            // Pick the specific alternative for the target segment, or the first alternative for others
            const altIdx = (i === segmentIndex) ? alternativeIndex : 0
            if (altIdx >= transformedRes.length) continue
            
            const resSegment = res[altIdx]
            const transformedSegment = transformedRes[altIdx]

            if (!Array.isArray(transformedSegment) || !Array.isArray(resSegment)) continue
            
            const resTupleContainer = resSegment[0]
            const transformedTupleContainer = transformedSegment[0]
            if (!Array.isArray(transformedTupleContainer) || !Array.isArray(resTupleContainer)) continue

            for (let k = 0; k < transformedTupleContainer.length; k++) {
                const resTuple = resTupleContainer[k]
                const transformedTuple = transformedTupleContainer[k]
                if (!Array.isArray(transformedTuple) || !Array.isArray(resTuple)) continue

                if (Array.isArray(transformedTuple) && transformedTuple.length >= 2) {
                    tokens.push(convertTupleToToken(resTuple, transformedTuple))
                }
            }
        } else if (typeof transformedRes === 'string' && typeof res === 'string') {
            // Non-word segment (punctuation, etc.)
            tokens.push({
                word: transformedRes,
                romanized: transformedRes,
                alternatives: [],
            })
        }
    }
    
    return tokens
}

/**
 * Parse all segmentation alternatives from romanizeStar result.
 * Each alternative represents a different way to tokenize the sentence.
 * 
 * @param result - Raw romanizeStar result
 * @param transformedResult - Transformed romanizeStar result
 * @returns Array of token arrays, one for each segmentation alternative with its score
 */
export function parseTokenAlternatives(
    result: RomanizeStarResult,
    transformedResult: TransformedRomanizeStarResult
): Array<{ tokens: TransformedRomanizeStarToken[]; score: number }> {
    const alternatives: Array<{ tokens: TransformedRomanizeStarToken[]; score: number }> = []
    
    // Find the first word segment that has alternatives
    for (let i = 0; i < result.length; i++) {
        const res = result[i]
        const transformedRes = transformedResult[i]
        
        if (Array.isArray(transformedRes) && Array.isArray(res)) {
            // This is a word segment with potentially multiple alternatives
            for (let j = 0; j < transformedRes.length; j++) {
                const tokens = parseTokenAlternative(result, transformedResult, i, j)
                const score = (Array.isArray(res[j]) && res[j].length >= 2) ? (res[j][1] as number) : 0
                alternatives.push({ tokens, score })
            }
            // Only process first word segment with alternatives
            // (most sentences have one main word segment)
            return alternatives
        }
    }
    
    // If no word segments with alternatives, parse the first alternative as default
    if (alternatives.length === 0) {
        const tokens = parseTokenAlternative(result, transformedResult, 0, 0)
        alternatives.push({ tokens, score: 0 })
    }
    
    return alternatives
}

/**
 * Parse tokens from romanizeStar result (returns first/best alternative only).
 * For multiple alternatives, use parseTokenAlternatives instead.
 * 
 * @param result - Raw romanizeStar result
 * @param transformedResult - Transformed romanizeStar result
 * @returns Array of tokens for the best (first) segmentation
 */
export function parseTokens(result: RomanizeStarResult, transformedResult: TransformedRomanizeStarResult): TransformedRomanizeStarToken[] {
    const alternatives = parseTokenAlternatives(result, transformedResult)
    return alternatives[0]?.tokens ?? []
}

function convertTupleToToken(resTuple: RomanizeStarResultTokenTuple, transformedTuple: TransformedRomanizeStarResultTokenTuple): TransformedRomanizeStarToken {
    const [romanized, info, alternatives] = transformedTuple
    let word = info.text
    if (!word && info.alternative && info.alternative.length > 0) {
        word = info.alternative[0].text
    }
    return {
        word,
        romanized: romanized || word,
        info: resTuple[1],
        infoJson: info,
        alternatives: Array.isArray(alternatives) ? alternatives : [],
    }
}

export function parseGrammarInfoFromToken(wordInfo: WordInfoGlossJson | 'gap'): GrammarInfo | undefined {
    if (wordInfo === 'gap') {
        return undefined
    }

    if (!wordInfo.score && wordInfo.alternative && wordInfo.alternative.length > 0) {
        return {
            ...parseGrammarInfoFromToken({
                ...wordInfo.alternative[0]!,
                alternative: wordInfo.alternative.slice(1),
            })!
        }
    }

    const word = wordInfo.text.trim()

    let reading = extractReading(wordInfo, wordInfo.text).trim().replace(/\u000C/g, '')
    if (reading === word) {
        reading = ''
    }

    const meanings = extractMeanings(wordInfo)
    const components = extractComponents(wordInfo)
    const conjugations = extractConjugations(wordInfo)
    const alternatives = extractAlternatives(wordInfo)

    return {
        word,
        reading,
        meanings,
        partOfSpeech: extractPartOfSpeech(wordInfo),
        isConjugation: false,
        hasConjugationVia: false,
        isConjugationVia: false,
        isAlternative: false,
        isComponent: false,
        isSuffix: wordInfo.suffix !== undefined,
        suffix: wordInfo.suffix,
        conjugations: conjugations || [],
        alternatives: alternatives || [],
        components: components || [],
    }
}

function extractReading(wordInfo: WordInfoGlossJson, originalWord: string): string {
    if (wordInfo.kana) {
        const kanaStr = Array.isArray(wordInfo.kana) ? wordInfo.kana[0] : wordInfo.kana
        if (kanaStr && kanaStr !== originalWord) {
            return kanaStr
        }
    }

    if (wordInfo.alternative && wordInfo.alternative.length > 0) {
        for (const alt of wordInfo.alternative) {
            if (alt.text === originalWord && alt.kana) {
                const altKana = Array.isArray(alt.kana) ? alt.kana[0] : alt.kana
                if (altKana && altKana !== originalWord) {
                    return altKana
                }
            }
        }
    }

    if ((wordInfo.kana && wordInfo.kana.length > 0) || isKanaOnly(originalWord)) {
        return ''
    }

    if (wordInfo.conj && wordInfo.conj.length > 0) {
        const conjReading = wordInfo.conj[0].reading
        if (conjReading && conjReading !== originalWord) {
            return conjReading
        }
    }

    return ''
}

function isKanaOnly(text: string): boolean {
    return /^[\u3040-\u309F\u30A0-\u30FF]*$/.test(text)
}

function extractMeanings(wordInfo: WordInfoGlossJson): Array<{
    text: string; partOfSpeech: IchiranPos[]; info: string
}> {
    const meanings: Array<{
        text: string; partOfSpeech: IchiranPos[]; info: string
    }> = []
    const seen = new Set<string>()

    if (wordInfo.gloss) {
        for (const glossItem of wordInfo.gloss) {
            if (glossItem.gloss && !seen.has(glossItem.gloss)) {
                meanings.push({
                    text: glossItem.gloss,
                    partOfSpeech: glossItem.pos ? parsePartOfSpeech(glossItem.pos) : [],
                    info: glossItem.info || '',
                })
                seen.add(glossItem.gloss)
            }
        }
    }

    if (meanings.length === 0) {
        if (wordInfo.score === 0 || !wordInfo.gloss) {
            return []
        } else {
            return [{
                text: 'Error parsing word',
                partOfSpeech: [],
                info: ''
            }]
        }
    }

    return meanings
}

function extractPartOfSpeechFromConjugation(conj: ConjInfoJson): IchiranPos[] {
    const result: IchiranPos[] = []
    if (conj.prop) {
        for (const propItem of conj.prop) {
            if (propItem.pos) {
                result.push(...parsePartOfSpeech(propItem.pos))
            }
        }
    }

    if (conj.gloss) {
        for (const glossItem of conj.gloss) {
            if (glossItem.pos) {
                result.push(...parsePartOfSpeech(glossItem.pos))
            }
        }
    }

    return [...new Set(result)]
}

function extractPartOfSpeech(wordInfo: WordInfoGlossJson): IchiranPos[] {
    // Special case 1: Known single-character particles always return ONLY "prt"
    const knownParticles = ['か', 'が', 'を', 'は', 'も', 'に', 'で', 'と', 'の', 'や', 'へ', 'より', 'まで', 'から', 'ね', 'よ', 'な', 'わ', 'ぞ', 'ぜ', 'さ']
    if (wordInfo.text && knownParticles.includes(wordInfo.text.trim())) {
        // Check if this token actually has "prt" in any of its POS tags
        const allPos: IchiranPos[] = []
        if (wordInfo.gloss) {
            for (const glossItem of wordInfo.gloss) {
                if (glossItem.pos) {
                    allPos.push(...parsePartOfSpeech(glossItem.pos))
                }
            }
        }
        if (allPos.some(p => p === 'prt')) {
            return ['prt']
        }
        // If it doesn't have prt tag, fall through to normal processing
    }

    const result: IchiranPos[] = []
    
    // Special case 2: For compound structures, use POS from main gloss if present,
    // otherwise use first component's POS
    if (wordInfo.components && wordInfo.components.length > 0) {
        // Try to extract POS from main gloss first
        if (wordInfo.gloss) {
            for (const glossItem of wordInfo.gloss) {
                if (glossItem.pos) {
                    result.push(...parsePartOfSpeech(glossItem.pos))
                }
            }
        }
        
        if (wordInfo.conj) {
            for (const conj of wordInfo.conj) {
                const pos = extractPartOfSpeechFromConjugation(conj)
                if (pos) {
                    result.push(...pos)
                }
            }
        }
        
        // If no POS found in main gloss, use first component's POS
        if (result.length === 0 && wordInfo.components.length > 0) {
            const firstComponent = wordInfo.components[0]
            if (firstComponent.gloss) {
                for (const glossItem of firstComponent.gloss) {
                    if (glossItem.pos) {
                        result.push(...parsePartOfSpeech(glossItem.pos))
                    }
                }
            }
            if (firstComponent.conj) {
                for (const conjItem of firstComponent.conj) {
                    if (conjItem.prop) {
                        for (const propItem of conjItem.prop) {
                            if (propItem.pos) {
                                result.push(...parsePartOfSpeech(propItem.pos))
                            }
                        }
                    }
                    if (conjItem.gloss) {
                        for (const glossItem of conjItem.gloss) {
                            if (glossItem.pos) {
                                result.push(...parsePartOfSpeech(glossItem.pos))
                            }
                        }
                    }
                }
            }
        }
        
        // Skip other components processing for compound structures
        return [...new Set(result)]
    }
    
    // Normal processing for non-compound tokens
    if (wordInfo.gloss) {
        for (const glossItem of wordInfo.gloss) {
            if (glossItem.pos) {
                result.push(...parsePartOfSpeech(glossItem.pos))
            }
        }
    }

    if (wordInfo.conj) {
        for (const conj of wordInfo.conj) {
            const pos = extractPartOfSpeechFromConjugation(conj)
            if (pos) {
                result.push(...pos)
            }
        }
    }

    if (wordInfo.alternative) {
        for (const alt of wordInfo.alternative) {
            if (alt.gloss) {
                for (const glossItem of alt.gloss) {
                    if (glossItem.pos) {
                        result.push(...parsePartOfSpeech(glossItem.pos))
                    }
                }
            }
            if (alt.conj) {
                for (const conj of alt.conj) {
                    const pos = extractPartOfSpeechFromConjugation(conj)
                    if (pos) {
                        result.push(...pos)
                    }
                }
            }
        }
    }

    if (wordInfo.components) {
        for (const component of wordInfo.components) {
            if (component.gloss) {
                for (const glossItem of component.gloss) {
                    if (glossItem.pos) {
                        result.push(...parsePartOfSpeech(glossItem.pos))
                    }
                }
            }

            if (component.conj) {
                for (const conjItem of component.conj) {
                    if (conjItem.prop) {
                        for (const propItem of conjItem.prop) {
                            if (propItem.pos) {
                                result.push(...parsePartOfSpeech(propItem.pos))
                            }
                        }
                    }
                    if (conjItem.gloss) {
                        for (const glossItem of conjItem.gloss) {
                            if (glossItem.pos) {
                                result.push(...parsePartOfSpeech(glossItem.pos))
                            }
                        }
                    }
                }
            }
        }
    }
    return [...new Set(result)]
}

function parsePartOfSpeech(pos: string): IchiranPos[] {
    const cleaned = pos.replace(/[\[\]]/g, '').trim()
    const tags = cleaned.split(',').map(tag => tag.trim()).filter(Boolean).map(tag => tag as IchiranPos)
    return tags
}

function convertIchiranConjugationToGrammarToken(conj: ConjInfoJson): GrammarInfo {
    let word = conj.reading || ''
    let reading = conj.reading || ''
    if (word.includes('【')) {
        const parts = word.split('【')
        word = parts[0].trim()
        if (parts[1]) {
            reading = parts[1].replace('】', '').trim()
            if (reading === word) {
                reading = ''
            }
        } else {
            reading = ''
        }
    }

    return {
        isConjugation: true,
        conjugationTypes: conj.prop?.map(prop => prop.type),
        neg: conj.prop?.some(prop => prop.neg) || undefined,
        fml: conj.prop?.some(prop => prop.fml) || undefined,
        hasConjugationVia: Boolean(conj.via?.length),
        isConjugationVia: false,
        isAlternative: false,
        isComponent: false,
        isSuffix: false,
        suffix: undefined,
        word,
        reading,
        partOfSpeech: extractPartOfSpeechFromConjugation(conj) || '',
        meanings: conj.gloss?.map(gloss => ({
            text: gloss.gloss,
            partOfSpeech: parsePartOfSpeech(gloss.pos),
            info: gloss.info || '',
        })) || [],
        conjugations: conj.via?.map(via => ({
            ...convertIchiranConjugationToGrammarToken(via),
            isConjugationVia: true,
        })) || [],
        alternatives: [],
        components: [],
    }
}

function extractConjugations(wordInfo: WordInfoGlossJson): GrammarInfo[] | undefined {
    if (!wordInfo.conj || !wordInfo.conj.length) {
        return undefined
    }

    return wordInfo.conj.map(conj => ({
        ...convertIchiranConjugationToGrammarToken(conj),
        isConjugation: true,
    }))
}

function extractAlternatives(wordInfo: WordInfoGlossJson): GrammarInfo[] | undefined {
    if (!wordInfo.alternative || !wordInfo.alternative.length) {
        return undefined
    }

    return wordInfo.alternative.map(alternative => ({
        ...parseGrammarInfoFromToken(alternative)!,
        isAlternative: true,
    }))
}

function extractComponents(wordInfo: WordInfoGlossJson): GrammarInfo[] | undefined {
    if (!wordInfo.compound || !wordInfo.components || wordInfo.compound.length === 0 || wordInfo.components.length === 0) {
        return undefined
    }

    return wordInfo.components.map(component => ({
        ...parseGrammarInfoFromToken(component)!,
        isComponent: true,
    }))
}