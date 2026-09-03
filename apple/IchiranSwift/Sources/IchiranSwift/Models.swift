import Foundation

public struct IchiranEntityHint: Codable, Sendable, Equatable {
  public let start: Int
  public let end: Int
  public let boost: Double?

  public init(start: Int, end: Int, boost: Double? = nil) {
    self.start = start
    self.end = end
    self.boost = boost
  }
}

public struct IchiranAnalyzeOptions: Codable, Sendable, Equatable {
  public var limit: Int
  public var entities: [IchiranEntityHint]
  public var normalizePunctuation: Bool

  public init(
    limit: Int = 5,
    entities: [IchiranEntityHint] = [],
    normalizePunctuation: Bool = false
  ) {
    self.limit = limit
    self.entities = entities
    self.normalizePunctuation = normalizePunctuation
  }
}

public enum IchiranRomanizationScheme: String, Codable, Sendable, CaseIterable {
  case hepburnBasic = "hepburn-basic"
  case hepburnSimple = "hepburn-simple"
  case hepburnPassport = "hepburn-passport"
  case hepburnTraditional = "hepburn-traditional"
  case hepburnModified = "hepburn-modified"
  case kunreiSiki = "kunrei-siki"
}

public struct IchiranRomanizeOptions: Codable, Sendable, Equatable {
  public var method: IchiranRomanizationScheme?
  public var entities: [IchiranEntityHint]
  public var normalizePunctuation: Bool

  public init(
    method: IchiranRomanizationScheme? = nil,
    entities: [IchiranEntityHint] = [],
    normalizePunctuation: Bool = false
  ) {
    self.method = method
    self.entities = entities
    self.normalizePunctuation = normalizePunctuation
  }
}

public enum IchiranAnalysisRoute: String, Codable, Sendable {
  case kana
  case kanji
  case gap
}

public struct IchiranAnalysisRoot: Codable, Sendable, Equatable {
  public let seq: Int
  public let form: String
  public let reading: String
}

public struct IchiranAnalysisInflection: Codable, Sendable, Equatable {
  public let pos: String
  public let type: Int
  public let negative: Bool?
  public let formal: Bool?
  public let ordinal: Int
}

public struct IchiranAnalysisComponent: Codable, Sendable, Equatable {
  public let text: String
  public let trueText: String?
  public let route: IchiranAnalysisRoute
  public let reading: String
  public let entryIndex: Int?
  public let root: IchiranAnalysisRoot?
  public let inflection: [IchiranAnalysisInflection]
  public let primary: Bool
}

public struct IchiranAnalysisCounter: Codable, Sendable, Equatable {
  public let text: String
  public let auxiliary: Bool

  public init(text: String, auxiliary: Bool) {
    self.text = text
    self.auxiliary = auxiliary
  }

  public init(from decoder: any Decoder) throws {
    var container = try decoder.unkeyedContainer()
    text = try container.decode(String.self)
    auxiliary = try container.decode(Bool.self)
    guard container.isAtEnd else {
      throw DecodingError.dataCorruptedError(
        in: container,
        debugDescription: "Counter must contain exactly two values"
      )
    }
  }

  public func encode(to encoder: any Encoder) throws {
    var container = encoder.unkeyedContainer()
    try container.encode(text)
    try container.encode(auxiliary)
  }
}

public struct IchiranAnalysisAlternative: Codable, Sendable, Equatable {
  public let candidateId: Int
  public let text: String
  public let trueText: String?
  public let route: IchiranAnalysisRoute
  public let reading: String
  public let romanized: String
  public let pos: [String]
  public let score: Double
  public let entryIndex: Int?
  public let root: IchiranAnalysisRoot?
  public let inflection: [IchiranAnalysisInflection]
  public let components: [IchiranAnalysisComponent]
  public let counter: IchiranAnalysisCounter?
}

public struct IchiranAnalysisToken: Codable, Sendable, Equatable, Identifiable {
  public var id: String { "\(start):\(end):\(candidateId.map(String.init) ?? "gap")" }

  public let candidateId: Int?
  public let start: Int
  public let end: Int
  public let text: String
  public let trueText: String?
  public let route: IchiranAnalysisRoute
  public let reading: String
  public let romanized: String
  public let pos: [String]
  public let score: Double
  public let entryIndex: Int?
  public let root: IchiranAnalysisRoot?
  public let inflection: [IchiranAnalysisInflection]
  public let components: [IchiranAnalysisComponent]
  public let alternatives: [IchiranAnalysisAlternative]
  public let skipped: Int
  public let entity: Bool
  public let counter: IchiranAnalysisCounter?
}

public struct IchiranAnalysisPath: Codable, Sendable, Equatable {
  public let score: Double
  public let tokens: [IchiranAnalysisToken]
}

public enum IchiranAnalysisChunk: Codable, Sendable, Equatable {
  case misc(start: Int, end: Int, text: String)
  case word(start: Int, end: Int, text: String, paths: [IchiranAnalysisPath])

  private enum CodingKeys: String, CodingKey {
    case type, start, end, text, paths
  }

  private enum Kind: String, Codable {
    case misc, word
  }

  public init(from decoder: any Decoder) throws {
    let container = try decoder.container(keyedBy: CodingKeys.self)
    let kind = try container.decode(Kind.self, forKey: .type)
    let start = try container.decode(Int.self, forKey: .start)
    let end = try container.decode(Int.self, forKey: .end)
    let text = try container.decode(String.self, forKey: .text)
    switch kind {
    case .misc:
      self = .misc(start: start, end: end, text: text)
    case .word:
      self = .word(
        start: start,
        end: end,
        text: text,
        paths: try container.decode([IchiranAnalysisPath].self, forKey: .paths)
      )
    }
  }

  public func encode(to encoder: any Encoder) throws {
    var container = encoder.container(keyedBy: CodingKeys.self)
    switch self {
    case .misc(let start, let end, let text):
      try container.encode(Kind.misc, forKey: .type)
      try container.encode(start, forKey: .start)
      try container.encode(end, forKey: .end)
      try container.encode(text, forKey: .text)
    case .word(let start, let end, let text, let paths):
      try container.encode(Kind.word, forKey: .type)
      try container.encode(start, forKey: .start)
      try container.encode(end, forKey: .end)
      try container.encode(text, forKey: .text)
      try container.encode(paths, forKey: .paths)
    }
  }
}

public struct IchiranAnalysisResult: Codable, Sendable, Equatable {
  public let input: String
  public let normalized: String
  public let computeMs: Double
  public let chunks: [IchiranAnalysisChunk]
  public let paths: [IchiranAnalysisPath]
}

public enum IchiranDictionaryPropertyTag: String, Codable, Sendable {
  case dial, field, misc, pos
  case senseInfo = "s_inf"
  case stagk, stagr
}

public struct IchiranDictionaryGloss: Codable, Sendable, Equatable {
  public let ord: Int
  public let text: String
}

public struct IchiranDictionaryProperty: Codable, Sendable, Equatable {
  public let tag: IchiranDictionaryPropertyTag
  public let ord: Int
  public let text: String
}

public struct IchiranDictionarySense: Codable, Sendable, Equatable {
  public let ord: Int
  public let glosses: [IchiranDictionaryGloss]
  public let properties: [IchiranDictionaryProperty]
}

public struct IchiranDictionaryForm: Codable, Sendable, Equatable {
  public let route: IchiranAnalysisRoute
  public let text: String
  public let ord: Int
  public let common: Int?
  public let commonTags: String
  public let conjugatable: Bool
  public let nokanji: Bool
  public let best: String?
}

public struct IchiranDictionaryEntry: Codable, Sendable, Equatable {
  public let seq: Int
  public let forms: [IchiranDictionaryForm]
  public let senses: [IchiranDictionarySense]
}
