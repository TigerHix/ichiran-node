import CryptoKit
import Foundation
import IchiranKernel

public actor IchiranAnalyzer {
  private var owner: NativeOwner?

  private init(owner: NativeOwner) {
    self.owner = owner
  }

  public static func open(_ pack: IchiranInstalledPack) async throws -> IchiranAnalyzer {
    let owner = try await Task.detached(priority: .userInitiated) {
      try NativeOwner.open(pack)
    }.value
    return IchiranAnalyzer(owner: owner)
  }

  public func analyze(
    _ text: String,
    options: IchiranAnalyzeOptions = .init()
  ) throws -> IchiranAnalysisResult {
    let units = Array(text.utf16)
    let wire = try Self.optionsData(
      units: units,
      limit: options.limit,
      entities: options.entities,
      normalizePunctuation: options.normalizePunctuation
    )
    let data = try requiredOwner().analyze(units: units, options: wire)
    return try Self.decode(IchiranAnalysisResult.self, from: data)
  }

  public func details(
    _ text: String,
    options: IchiranTokenDetailsOptions
  ) throws -> IchiranTokenDetails {
    guard let pathIndex = UInt32(exactly: options.pathIndex),
      let tokenIndex = UInt32(exactly: options.tokenIndex)
    else {
      throw IchiranAnalyzerError(
        code: .invalidInput,
        message: "pathIndex and tokenIndex must be non-negative 32-bit integers"
      )
    }
    let units = Array(text.utf16)
    let wire = try Self.optionsData(
      units: units,
      limit: options.limit,
      entities: options.entities,
      normalizePunctuation: options.normalizePunctuation
    )
    let data = try requiredOwner().tokenDetails(
      units: units,
      options: wire,
      pathIndex: pathIndex,
      tokenIndex: tokenIndex,
      locale: options.locale
    )
    return try Self.decode(IchiranTokenDetails.self, from: data)
  }

  public func romanize(
    _ text: String,
    options: IchiranRomanizeOptions = .init()
  ) throws -> String {
    let units = Array(text.utf16)
    let wire = try Self.optionsData(
      units: units,
      limit: 1,
      entities: options.entities,
      normalizePunctuation: options.normalizePunctuation
    )
    let data = try requiredOwner().romanize(
      units: units,
      options: wire,
      method: options.method?.rawValue ?? ""
    )
    return try Self.decode(String.self, from: data)
  }

  public func entry(
    _ entryIndex: Int,
    options: IchiranDictionaryEntryOptions = .init()
  ) throws -> IchiranDictionaryEntry {
    guard let index = UInt32(exactly: entryIndex) else {
      throw IchiranAnalyzerError(
        code: .invalidInput,
        message: "entryIndex must be a non-negative 32-bit integer"
      )
    }
    let data = try requiredOwner().entry(index: index, locale: options.locale)
    return try Self.decode(IchiranDictionaryEntry.self, from: data)
  }

  /// Idempotently releases the Rust handles and file-backed dictionary stores.
  /// Actor isolation makes disposal wait for any in-flight operation.
  public func dispose() {
    owner?.close()
    owner = nil
  }

  private func requiredOwner() throws -> NativeOwner {
    guard let owner else {
      throw IchiranAnalyzerError(code: .internal, message: "Analyzer has been disposed")
    }
    return owner
  }

  private struct WireOptions: Encodable {
    let limit: Int
    let entities: [IchiranEntityHint]
    let normalizePunctuation: Bool
  }

  private static func optionsData(
    units: [UInt16],
    limit: Int,
    entities: [IchiranEntityHint],
    normalizePunctuation: Bool
  ) throws -> Data {
    guard units.count <= 4_096 else {
      throw IchiranAnalyzerError(
        code: .invalidInput,
        message: "text must contain at most 4096 UTF-16 code units"
      )
    }
    guard (1...10).contains(limit) else {
      throw IchiranAnalyzerError(
        code: .invalidInput, message: "limit must be an integer from 1 to 10")
    }
    guard entities.count <= 64 else {
      throw IchiranAnalyzerError(
        code: .invalidInput, message: "entities must contain at most 64 hints")
    }
    for (index, entity) in entities.enumerated() {
      guard entity.start >= 0, entity.end > entity.start, entity.end <= units.count else {
        throw IchiranAnalyzerError(
          code: .invalidInput,
          message: "entities[\(index)] must be a non-empty UTF-16 span within the input"
        )
      }
      if let boost = entity.boost, !boost.isFinite || abs(boost) > 1_000_000 {
        throw IchiranAnalyzerError(
          code: .invalidInput,
          message: "entities[\(index)].boost must be finite and between -1000000 and 1000000"
        )
      }
    }
    do {
      return try JSONEncoder().encode(
        WireOptions(
          limit: limit,
          entities: entities,
          normalizePunctuation: normalizePunctuation
        ))
    } catch {
      throw IchiranAnalyzerError(code: .internal, message: "Could not encode analyzer options")
    }
  }

  private static func decode<T: Decodable>(_ type: T.Type, from data: Data) throws -> T {
    do {
      return try JSONDecoder().decode(type, from: data)
    } catch {
      throw IchiranAnalyzerError(
        code: .internal,
        message:
          "Rust returned a result outside the Swift product contract: \(error.localizedDescription)"
      )
    }
  }

  // Qualification-only hooks. They preserve arbitrary UTF-16 code units and
  // exact serialized Rust bytes without becoming public product API.
  func qualificationAnalyzeJSON(utf16Units: [UInt16], optionsJSON: Data) throws -> Data {
    try requiredOwner().analyze(units: utf16Units, options: optionsJSON)
  }

  func qualificationRomanizeJSON(
    utf16Units: [UInt16],
    optionsJSON: Data,
    method: String
  ) throws -> Data {
    try requiredOwner().romanize(units: utf16Units, options: optionsJSON, method: method)
  }

  func qualificationLegacyJSON(utf16Units: [UInt16], optionsJSON: Data) throws -> Data {
    try requiredOwner().legacy(units: utf16Units, options: optionsJSON, locale: "en")
  }

  func qualificationTokenDetailsJSON(
    utf16Units: [UInt16],
    optionsJSON: Data,
    pathIndex: UInt32,
    tokenIndex: UInt32,
    corruptFirstBlock: Bool = false
  ) throws -> Data {
    try requiredOwner().tokenDetails(
      units: utf16Units,
      options: optionsJSON,
      pathIndex: pathIndex,
      tokenIndex: tokenIndex,
      locale: "en",
      corruptFirstBlock: corruptFirstBlock
    )
  }

  func qualificationEntryJSON(_ entryIndex: UInt32) throws -> Data {
    try requiredOwner().entry(index: entryIndex, locale: "en")
  }

  func qualificationLexiconRange(_ entryIndex: UInt32) throws -> (offset: Int, byteLength: Int) {
    try requiredOwner().lexiconRange(index: entryIndex)
  }

  func qualificationDiagnostics() throws -> NativeDiagnostics {
    try requiredOwner().diagnostics
  }
}

struct NativeDiagnostics: Sendable, Equatable {
  let dictionaryFileBytes: Int
  let dictionaryHeaderBytes: Int
  let dictionaryPrefixBytes: Int
  let lexiconPrefixBytes: Int
  let dictionaryBytesRead: Int
  let largestDictionaryRead: Int
  let lastDictionaryRead: Int
  let lastCallWasMainThread: Bool
}

private final class NativeLocaleResource {
  var store: OpaquePointer?
  var file: FileHandle?
  let fileBytes: Int
  let prefixBytes: Int

  init(store: OpaquePointer, file: FileHandle, fileBytes: Int, prefixBytes: Int) {
    self.store = store
    self.file = file
    self.fileBytes = fileBytes
    self.prefixBytes = prefixBytes
  }
}

private final class NativeOwner: @unchecked Sendable {
  private var kernel: OpaquePointer?
  private var lexicon: OpaquePointer?
  private var lexiconFile: FileHandle?
  private var locales: [String: NativeLocaleResource]
  private let dictionaryFileBytes: Int
  private let dictionaryHeaderBytes: Int
  private let dictionaryPrefixBytes: Int
  private let lexiconPrefixBytes: Int
  private var dictionaryBytesRead: Int
  private var largestDictionaryRead: Int
  private var lastDictionaryRead = 0
  private var lastCallWasMainThread = false

  var diagnostics: NativeDiagnostics {
    NativeDiagnostics(
      dictionaryFileBytes: dictionaryFileBytes,
      dictionaryHeaderBytes: dictionaryHeaderBytes,
      dictionaryPrefixBytes: dictionaryPrefixBytes,
      lexiconPrefixBytes: lexiconPrefixBytes,
      dictionaryBytesRead: dictionaryBytesRead,
      largestDictionaryRead: largestDictionaryRead,
      lastDictionaryRead: lastDictionaryRead,
      lastCallWasMainThread: lastCallWasMainThread
    )
  }

  private init(
    kernel: OpaquePointer,
    lexicon: OpaquePointer,
    lexiconFile: FileHandle,
    locales: [String: NativeLocaleResource],
    dictionaryFileBytes: Int,
    dictionaryHeaderBytes: Int,
    dictionaryPrefixBytes: Int,
    lexiconPrefixBytes: Int,
    dictionaryBytesRead: Int,
    largestDictionaryRead: Int
  ) {
    self.kernel = kernel
    self.lexicon = lexicon
    self.lexiconFile = lexiconFile
    self.locales = locales
    self.dictionaryFileBytes = dictionaryFileBytes
    self.dictionaryHeaderBytes = dictionaryHeaderBytes
    self.dictionaryPrefixBytes = dictionaryPrefixBytes
    self.lexiconPrefixBytes = lexiconPrefixBytes
    self.dictionaryBytesRead = dictionaryBytesRead
    self.largestDictionaryRead = largestDictionaryRead
  }

  deinit {
    close()
  }

  static func open(_ pack: IchiranInstalledPack) throws -> NativeOwner {
    guard ichiran_kernel_abi_version() == ICHIRAN_KERNEL_ABI_VERSION else {
      throw IchiranAnalyzerError(
        code: .internal,
        message:
          "Ichiran kernel ABI mismatch: Swift requires version \(ICHIRAN_KERNEL_ABI_VERSION)"
      )
    }
    let hot: Data
    do {
      hot = try Data(contentsOf: pack.hotURL, options: .mappedIfSafe)
    } catch {
      throw IchiranAnalyzerError(
        code: .invalidPack,
        message: "Could not read installed hot.bin: \(error.localizedDescription)"
      )
    }
    guard hot.count == pack.manifest.hot.installedBytes,
      SHA256.hash(data: hot).hex == pack.manifest.hot.installedSHA256
    else {
      throw IchiranAnalyzerError(
        code: .invalidPack, message: "Installed hot.bin identity does not match manifest")
    }

    var kernel: OpaquePointer?
    var lexicon: OpaquePointer?
    var lexiconFile: FileHandle?
    var localeResources: [String: NativeLocaleResource] = [:]
    do {
      let openResult = hot.withUnsafeBytes { bytes in
        ichiran_kernel_open(
          bytes.bindMemory(to: UInt8.self).baseAddress,
          bytes.count,
          &kernel
        )
      }
      _ = try NativeResult.consume(openResult, fallback: .invalidPack)
      guard let kernel else {
        throw IchiranAnalyzerError(code: .internal, message: "Rust returned no kernel handle")
      }

      let lexiconSize = try fileSize(
        at: pack.lexiconURL,
        expected: pack.manifest.lexicon.installedBytes,
        label: "lexicon.bin"
      )
      let openedLexiconFile = try openFile(at: pack.lexiconURL, label: "lexicon.bin")
      lexiconFile = openedLexiconFile
      let lexiconHeader = try readExact(file: openedLexiconFile, offset: 0, count: 96)
      var lexiconPrefixBytes = 0
      let lengthResult = lexiconHeader.withUnsafeBytes { bytes in
        ichiran_lexicon_prefix_length(
          bytes.bindMemory(to: UInt8.self).baseAddress,
          bytes.count,
          lexiconSize,
          &lexiconPrefixBytes
        )
      }
      _ = try NativeResult.consume(lengthResult, fallback: .invalidPack)
      guard lexiconPrefixBytes >= 96, lexiconPrefixBytes < lexiconSize else {
        throw IchiranAnalyzerError(
          code: .invalidPack, message: "Rust returned an invalid lexicon prefix length")
      }
      let lexiconPrefix = try readExact(
        file: openedLexiconFile,
        offset: 0,
        count: lexiconPrefixBytes
      )
      let lexiconResult = lexiconPrefix.withUnsafeBytes { bytes in
        ichiran_lexicon_store_open(
          bytes.bindMemory(to: UInt8.self).baseAddress,
          bytes.count,
          lexiconSize,
          &lexicon
        )
      }
      _ = try NativeResult.consume(lexiconResult, fallback: .invalidPack)
      guard let lexicon else {
        throw IchiranAnalyzerError(code: .internal, message: "Rust returned no lexicon handle")
      }

      let digest = try digestBytes(pack.manifest.lexicon.installedSHA256)
      let entryCount = ichiran_lexicon_store_entry_count(lexicon)
      var dictionaryFileBytes = lexiconSize
      var dictionaryHeaderBytes = 96
      var dictionaryPrefixBytes = lexiconPrefixBytes
      var dictionaryBytesRead = 96 + lexiconPrefixBytes
      var largestDictionaryRead = lexiconPrefixBytes
      for locale in pack.manifest.locales.keys.sorted() {
        let asset = pack.manifest.locales[locale]!
        let url = pack.localeURL(locale)
        let localeSize = try fileSize(
          at: url,
          expected: asset.installedBytes,
          label: "gloss.\(locale).bin"
        )
        let file = try openFile(at: url, label: "gloss.\(locale).bin")
        var localeStore: OpaquePointer?
        do {
          let header = try readExact(file: file, offset: 0, count: 128)
          var prefixBytes = 0
          let prefixLength = header.withUnsafeBytes { bytes in
            ichiran_locale_prefix_length(
              bytes.bindMemory(to: UInt8.self).baseAddress,
              bytes.count,
              localeSize,
              &prefixBytes
            )
          }
          _ = try NativeResult.consume(prefixLength, fallback: .invalidPack)
          guard prefixBytes >= 128, prefixBytes < localeSize else {
            throw IchiranAnalyzerError(
              code: .invalidPack,
              message: "Rust returned an invalid \(locale) locale prefix length"
            )
          }
          let prefix = try readExact(file: file, offset: 0, count: prefixBytes)
          let localeBytes = Data(locale.utf8)
          let opened = prefix.withUnsafeBytes { prefixBuffer in
            digest.withUnsafeBufferPointer { digestBuffer in
              localeBytes.withUnsafeBytes { localeBuffer in
                ichiran_locale_store_open(
                  prefixBuffer.bindMemory(to: UInt8.self).baseAddress,
                  prefixBuffer.count,
                  localeSize,
                  digestBuffer.baseAddress,
                  localeBuffer.bindMemory(to: UInt8.self).baseAddress,
                  localeBuffer.count,
                  entryCount,
                  &localeStore
                )
              }
            }
          }
          _ = try NativeResult.consume(opened, fallback: .invalidPack)
          guard let localeStore else {
            throw IchiranAnalyzerError(
              code: .internal, message: "Rust returned no \(locale) locale handle")
          }
          localeResources[locale] = NativeLocaleResource(
            store: localeStore,
            file: file,
            fileBytes: localeSize,
            prefixBytes: prefixBytes
          )
          dictionaryFileBytes += localeSize
          dictionaryHeaderBytes += 128
          dictionaryPrefixBytes += prefixBytes
          dictionaryBytesRead += 128 + prefixBytes
          largestDictionaryRead = max(largestDictionaryRead, prefixBytes)
        } catch {
          if let localeStore { ichiran_locale_store_free(localeStore) }
          try? file.close()
          throw error
        }
      }
      return NativeOwner(
        kernel: kernel,
        lexicon: lexicon,
        lexiconFile: openedLexiconFile,
        locales: localeResources,
        dictionaryFileBytes: dictionaryFileBytes,
        dictionaryHeaderBytes: dictionaryHeaderBytes,
        dictionaryPrefixBytes: dictionaryPrefixBytes,
        lexiconPrefixBytes: lexiconPrefixBytes,
        dictionaryBytesRead: dictionaryBytesRead,
        largestDictionaryRead: largestDictionaryRead
      )
    } catch {
      for resource in localeResources.values {
        if let store = resource.store { ichiran_locale_store_free(store) }
        try? resource.file?.close()
      }
      if let lexicon { ichiran_lexicon_store_free(lexicon) }
      if let kernel { ichiran_kernel_free(kernel) }
      try? lexiconFile?.close()
      throw error
    }
  }

  func close() {
    for resource in locales.values {
      if let store = resource.store {
        ichiran_locale_store_free(store)
        resource.store = nil
      }
      if let file = resource.file {
        try? file.close()
        resource.file = nil
      }
    }
    locales.removeAll()
    if let lexicon {
      ichiran_lexicon_store_free(lexicon)
      self.lexicon = nil
    }
    if let kernel {
      ichiran_kernel_free(kernel)
      self.kernel = nil
    }
    if let lexiconFile {
      try? lexiconFile.close()
      self.lexiconFile = nil
    }
  }

  func analyze(units: [UInt16], options: Data) throws -> Data {
    let kernel = try requiredKernel()
    lastCallWasMainThread = Thread.isMainThread
    let result = units.withUnsafeBufferPointer { input in
      options.withUnsafeBytes { optionsBytes in
        ichiran_kernel_analyze_utf16(
          kernel,
          input.baseAddress,
          input.count,
          optionsBytes.bindMemory(to: UInt8.self).baseAddress,
          optionsBytes.count
        )
      }
    }
    return try NativeResult.consume(result, fallback: .internal)
  }

  func romanize(units: [UInt16], options: Data, method: String) throws -> Data {
    let kernel = try requiredKernel()
    let methodData = Data(method.utf8)
    lastCallWasMainThread = Thread.isMainThread
    let result = units.withUnsafeBufferPointer { input in
      options.withUnsafeBytes { optionsBytes in
        methodData.withUnsafeBytes { methodBytes in
          ichiran_kernel_romanize_utf16(
            kernel,
            input.baseAddress,
            input.count,
            optionsBytes.bindMemory(to: UInt8.self).baseAddress,
            optionsBytes.count,
            methodBytes.bindMemory(to: UInt8.self).baseAddress,
            methodBytes.count
          )
        }
      }
    }
    return try NativeResult.consume(result, fallback: .internal)
  }

  func entry(index: UInt32, locale: String) throws -> Data {
    let selected = try requiredLocale(locale)
    let fallback = try requiredLocale("en")
    let lexicon = try decodeLexicon(index: index)
    let english = try decodeLocale(index: index, resource: fallback)
    let localized = locale == "en" ? english : try decodeLocale(index: index, resource: selected)
    let entry = try Self.localize(lexicon: lexicon, locale: localized, fallback: english)
    do {
      return try JSONEncoder().encode(entry)
    } catch {
      throw IchiranAnalyzerError(
        code: .internal, message: "Could not encode localized dictionary entry")
    }
  }

  func lexiconRange(index: UInt32) throws -> (offset: Int, byteLength: Int) {
    let value = try nativeLexiconRange(index: index)
    return (Int(value.offset), Int(value.byte_length))
  }

  func tokenDetails(
    units: [UInt16],
    options: Data,
    pathIndex: UInt32,
    tokenIndex: UInt32,
    locale: String,
    corruptFirstBlock: Bool = false
  ) throws -> Data {
    let kernel = try requiredKernel()
    let lexicon = try requiredLexicon()
    let selected = try requiredLocale(locale)
    let fallbackLocale = try requiredLocale("en")
    let selectedStore = try requiredStore(selected)
    let fallbackStore = try requiredStore(fallbackLocale)
    var operation: OpaquePointer?
    lastCallWasMainThread = Thread.isMainThread
    let begun = units.withUnsafeBufferPointer { input in
      options.withUnsafeBytes { optionsBytes in
        ichiran_kernel_token_details_begin_utf16(
          kernel,
          input.baseAddress,
          input.count,
          optionsBytes.bindMemory(to: UInt8.self).baseAddress,
          optionsBytes.count,
          Int(pathIndex),
          Int(tokenIndex),
          &operation
        )
      }
    }
    _ = try NativeResult.consume(begun, fallback: .notFound)
    guard let operation else {
      throw IchiranAnalyzerError(
        code: .internal, message: "Rust returned no token-details operation"
      )
    }
    return try driveLazyOperation(
      operation: operation,
      release: { ichiran_token_details_operation_free($0) },
      fallback: .internal,
      corruptFirstBlock: corruptFirstBlock,
      selected: selected,
      fallbackLocale: fallbackLocale,
      advance: { suppliedStore, suppliedEntry, bytes in
        ichiran_kernel_token_details_step(
          kernel,
          operation,
          lexicon,
          selectedStore,
          fallbackStore,
          suppliedStore,
          suppliedEntry,
          bytes.count == 0 ? nil : bytes.bindMemory(to: UInt8.self).baseAddress,
          bytes.count
        )
      }
    )
  }

  func legacy(units: [UInt16], options: Data, locale: String) throws -> Data {
    let kernel = try requiredKernel()
    let lexicon = try requiredLexicon()
    let selected = try requiredLocale(locale)
    let fallbackLocale = try requiredLocale("en")
    let selectedStore = try requiredStore(selected)
    let fallbackStore = try requiredStore(fallbackLocale)
    var operation: OpaquePointer?
    lastCallWasMainThread = Thread.isMainThread
    let begun = units.withUnsafeBufferPointer { input in
      options.withUnsafeBytes { optionsBytes in
        ichiran_kernel_legacy_begin_utf16(
          kernel,
          input.baseAddress,
          input.count,
          optionsBytes.bindMemory(to: UInt8.self).baseAddress,
          optionsBytes.count,
          nil,
          0,
          &operation
        )
      }
    }
    _ = try NativeResult.consume(begun, fallback: .internal)
    guard let operation else {
      throw IchiranAnalyzerError(
        code: .internal, message: "Rust returned no qualification operation")
    }
    return try driveLazyOperation(
      operation: operation,
      release: { ichiran_legacy_operation_free($0) },
      fallback: .internal,
      corruptFirstBlock: false,
      selected: selected,
      fallbackLocale: fallbackLocale,
      advance: { suppliedStore, suppliedEntry, bytes in
        ichiran_kernel_legacy_step(
          kernel,
          operation,
          lexicon,
          selectedStore,
          fallbackStore,
          suppliedStore,
          suppliedEntry,
          bytes.count == 0 ? nil : bytes.bindMemory(to: UInt8.self).baseAddress,
          bytes.count
        )
      }
    )
  }

  private func driveLazyOperation(
    operation: OpaquePointer,
    release: (OpaquePointer?) -> Void,
    fallback: IchiranAnalyzerError.Code,
    corruptFirstBlock: Bool,
    selected: NativeLocaleResource,
    fallbackLocale: NativeLocaleResource,
    advance: (UInt32, UInt32, UnsafeRawBufferPointer) -> IchiranStepResult
  ) throws -> Data {
    defer { release(operation) }
    var suppliedStore = UInt32(ICHIRAN_DICTIONARY_NONE.rawValue)
    var suppliedEntry = UInt32.max
    var supplied = Data()
    var shouldCorrupt = corruptFirstBlock
    for _ in 0..<4_096 {
      lastCallWasMainThread = Thread.isMainThread
      let step = supplied.withUnsafeBytes { advance(suppliedStore, suppliedEntry, $0) }
      let data = try NativeResult.consume(step: step, fallback: fallback)
      switch step.state {
      case 1:
        return data
      case 2:
        guard data.isEmpty, step.range.byte_length > 0 else {
          throw IchiranAnalyzerError(
            code: .internal, message: "Rust returned an invalid missing-dictionary step"
          )
        }
        suppliedStore = step.store
        suppliedEntry = step.entry_index
        switch step.store {
        case UInt32(ICHIRAN_DICTIONARY_LEXICON.rawValue):
          supplied = try readLexicon(offset: Int(step.range.offset), count: Int(step.range.byte_length))
        case UInt32(ICHIRAN_DICTIONARY_LOCALE.rawValue):
          supplied = try readLocale(selected, offset: Int(step.range.offset), count: Int(step.range.byte_length))
        case UInt32(ICHIRAN_DICTIONARY_FALLBACK.rawValue):
          supplied = try readLocale(fallbackLocale, offset: Int(step.range.offset), count: Int(step.range.byte_length))
        default:
          throw IchiranAnalyzerError(
            code: .internal, message: "Rust requested an unknown dictionary store")
        }
        if shouldCorrupt {
          supplied[0] ^= 0xff
          shouldCorrupt = false
        }
      default:
        throw IchiranAnalyzerError(
          code: .internal, message: "Rust returned an invalid lazy-operation step state"
        )
      }
    }
    throw IchiranAnalyzerError(
      code: .internal, message: "Lazy operation exceeded 4096 dictionary reads"
    )
  }

  private func requiredKernel() throws -> OpaquePointer {
    guard let kernel else {
      throw IchiranAnalyzerError(code: .internal, message: "Analyzer has been disposed")
    }
    return kernel
  }

  private func nativeLexiconRange(index: UInt32) throws -> IchiranDictionaryRange {
    let lexicon = try requiredLexicon()
    var range = IchiranDictionaryRange()
    lastCallWasMainThread = Thread.isMainThread
    let result = ichiran_lexicon_store_range(lexicon, index, &range)
    _ = try NativeResult.consume(result, fallback: .notFound)
    return range
  }

  private func localeRange(index: UInt32, resource: NativeLocaleResource) throws -> IchiranDictionaryRange {
    let store = try requiredStore(resource)
    var range = IchiranDictionaryRange()
    lastCallWasMainThread = Thread.isMainThread
    let result = ichiran_locale_store_range(store, index, &range)
    _ = try NativeResult.consume(result, fallback: .notFound)
    return range
  }

  private func requiredLexicon() throws -> OpaquePointer {
    guard let lexicon else {
      throw IchiranAnalyzerError(code: .internal, message: "Analyzer has been disposed")
    }
    return lexicon
  }

  private func requiredLocale(_ locale: String) throws -> NativeLocaleResource {
    guard locale.range(
      of: #"^[A-Za-z]{2,8}(?:-[A-Za-z0-9]{1,8})*$"#,
      options: .regularExpression
    ) != nil else {
      throw IchiranAnalyzerError(code: .invalidInput, message: "locale must be a valid BCP 47 language tag")
    }
    guard let resource = locales[locale] else {
      throw IchiranAnalyzerError(code: .notFound, message: "Dictionary locale is not installed: \(locale)")
    }
    return resource
  }

  private func requiredStore(_ resource: NativeLocaleResource) throws -> OpaquePointer {
    guard let store = resource.store else {
      throw IchiranAnalyzerError(code: .internal, message: "Analyzer has been disposed")
    }
    return store
  }

  private func readLexicon(offset: Int, count: Int) throws -> Data {
    guard let lexiconFile else {
      throw IchiranAnalyzerError(code: .internal, message: "Analyzer has been disposed")
    }
    return try readDictionary(file: lexiconFile, label: "lexicon.bin", offset: offset, count: count)
  }

  private func readLocale(
    _ resource: NativeLocaleResource,
    offset: Int,
    count: Int
  ) throws -> Data {
    guard let file = resource.file else {
      throw IchiranAnalyzerError(code: .internal, message: "Analyzer has been disposed")
    }
    return try readDictionary(file: file, label: "locale pack", offset: offset, count: count)
  }

  private func readDictionary(
    file: FileHandle,
    label: String,
    offset: Int,
    count: Int
  ) throws -> Data {
    let data: Data
    do {
      data = try Self.readExact(file: file, offset: offset, count: count)
    } catch {
      throw IchiranAnalyzerError(
        code: .invalidPack,
        message: "Could not read the requested \(label) block: \(error.localizedDescription)"
      )
    }
    dictionaryBytesRead += data.count
    largestDictionaryRead = max(largestDictionaryRead, data.count)
    lastDictionaryRead = data.count
    return data
  }

  private func decodeLexicon(index: UInt32) throws -> LexiconEntryWire {
    let lexicon = try requiredLexicon()
    let range = try nativeLexiconRange(index: index)
    let compressed = try readLexicon(offset: Int(range.offset), count: Int(range.byte_length))
    let result = compressed.withUnsafeBytes { bytes in
      ichiran_lexicon_store_decode(
        lexicon,
        index,
        bytes.bindMemory(to: UInt8.self).baseAddress,
        bytes.count
      )
    }
    return try decodeWire(LexiconEntryWire.self, result: result)
  }

  private func decodeLocale(index: UInt32, resource: NativeLocaleResource) throws -> LocaleEntryWire {
    let store = try requiredStore(resource)
    let range = try localeRange(index: index, resource: resource)
    let compressed = try readLocale(resource, offset: Int(range.offset), count: Int(range.byte_length))
    let result = compressed.withUnsafeBytes { bytes in
      ichiran_locale_store_decode(
        store,
        index,
        bytes.bindMemory(to: UInt8.self).baseAddress,
        bytes.count
      )
    }
    return try decodeWire(LocaleEntryWire.self, result: result)
  }

  private func decodeWire<T: Decodable>(_ type: T.Type, result: IchiranResult) throws -> T {
    let data = try NativeResult.consume(result, fallback: .invalidPack)
    do {
      return try JSONDecoder().decode(type, from: data)
    } catch {
      throw IchiranAnalyzerError(code: .invalidPack, message: "Rust returned invalid dictionary JSON")
    }
  }

  private struct LexiconEntryWire: Decodable {
    struct Sense: Decodable {
      let ord: Int
      let properties: [IchiranDictionaryProperty]
    }
    let seq: Int
    let forms: [IchiranDictionaryForm]
    let senses: [Sense]
  }

  private struct LocaleEntryWire: Decodable {
    struct Group: Decodable {
      let targets: [Int]
      let glosses: [IchiranDictionaryGloss]
      let info: [IchiranDictionaryGloss]
    }
    let seq: Int
    let groups: [Group]
  }

  private static func localize(
    lexicon: LexiconEntryWire,
    locale: LocaleEntryWire,
    fallback: LocaleEntryWire
  ) throws -> IchiranDictionaryEntry {
    guard lexicon.seq == locale.seq, lexicon.seq == fallback.seq else {
      throw IchiranAnalyzerError(
        code: .invalidPack, message: "Dictionary entry sequence does not match across stores")
    }
    var senses = lexicon.senses.map { sense in
      let selected = locale.groups.filter { $0.targets.contains(sense.ord) }
      let english = fallback.groups.filter { $0.targets.contains(sense.ord) }
      let glossGroups = selected.contains { !$0.glosses.isEmpty } ? selected : english
      let infoGroups = selected.contains { !$0.info.isEmpty } ? selected : english
      return IchiranDictionarySense(
        ord: sense.ord,
        glosses: glossGroups.flatMap(\.glosses),
        properties: sense.properties + infoGroups.flatMap { group in
          group.info.map {
            IchiranDictionaryProperty(tag: .senseInfo, ord: $0.ord, text: $0.text)
          }
        }
      )
    }
    let selectedEntryGroups = locale.groups.filter(\.targets.isEmpty)
    let englishEntryGroups = fallback.groups.filter(\.targets.isEmpty)
    let entryGroups = selectedEntryGroups.contains { !$0.glosses.isEmpty }
      ? selectedEntryGroups
      : englishEntryGroups
    var nextOrd = (lexicon.senses.map(\.ord).max() ?? -1) + 1
    for group in entryGroups {
      senses.append(
        IchiranDictionarySense(
          ord: nextOrd,
          glosses: group.glosses,
          properties: group.info.map {
            IchiranDictionaryProperty(tag: .senseInfo, ord: $0.ord, text: $0.text)
          }.sorted { $0.ord < $1.ord }
        )
      )
      nextOrd += 1
    }
    return IchiranDictionaryEntry(seq: lexicon.seq, forms: lexicon.forms, senses: senses)
  }

  private static func fileSize(at url: URL, expected: Int, label: String) throws -> Int {
    do {
      let attributes = try FileManager.default.attributesOfItem(atPath: url.path)
      guard let value = attributes[.size] as? NSNumber else {
        throw CocoaError(.fileReadCorruptFile)
      }
      let size = value.intValue
      guard size == expected else {
        throw IchiranAnalyzerError(
          code: .invalidPack, message: "Installed \(label) size does not match manifest")
      }
      return size
    } catch let error as IchiranAnalyzerError {
      throw error
    } catch {
      throw IchiranAnalyzerError(
        code: .invalidPack,
        message: "Could not inspect installed \(label): \(error.localizedDescription)"
      )
    }
  }

  private static func openFile(at url: URL, label: String) throws -> FileHandle {
    do {
      return try FileHandle(forReadingFrom: url)
    } catch {
      throw IchiranAnalyzerError(
        code: .invalidPack,
        message: "Could not open installed \(label): \(error.localizedDescription)"
      )
    }
  }

  private static func digestBytes(_ hex: String) throws -> [UInt8] {
    guard hex.count == 64 else {
      throw IchiranAnalyzerError(code: .invalidPack, message: "Invalid lexicon digest")
    }
    return try stride(from: 0, to: hex.count, by: 2).map { offset in
      let start = hex.index(hex.startIndex, offsetBy: offset)
      let end = hex.index(start, offsetBy: 2)
      guard let value = UInt8(hex[start..<end], radix: 16) else {
        throw IchiranAnalyzerError(code: .invalidPack, message: "Invalid lexicon digest")
      }
      return value
    }
  }

  private static func readExact(file: FileHandle, offset: Int, count: Int) throws -> Data {
    guard offset >= 0, count >= 0 else { throw CocoaError(.fileReadCorruptFile) }
    try file.seek(toOffset: UInt64(offset))
    let data = try file.read(upToCount: count) ?? Data()
    guard data.count == count else { throw CocoaError(.fileReadCorruptFile) }
    return data
  }
}

private enum NativeResult {
  private struct ErrorBody: Decodable {
    let code: String
    let message: String
  }

  static func consume(
    _ result: IchiranResult,
    fallback: IchiranAnalyzerError.Code
  ) throws -> Data {
    let data = copy(result.buffer)
    ichiran_buffer_free(result.buffer)
    guard result.status == 0 else {
      throw error(status: result.status, data: data, fallback: fallback)
    }
    return data
  }

  static func consume(
    step: IchiranStepResult,
    fallback: IchiranAnalyzerError.Code
  ) throws -> Data {
    let data = copy(step.buffer)
    ichiran_buffer_free(step.buffer)
    guard step.status == 0 else {
      throw error(status: step.status, data: data, fallback: fallback)
    }
    return data
  }

  private static func copy(_ buffer: IchiranBuffer) -> Data {
    guard buffer.byte_length > 0, let bytes = buffer.data else { return Data() }
    return Data(bytes: bytes, count: buffer.byte_length)
  }

  private static func error(
    status: UInt32,
    data: Data,
    fallback: IchiranAnalyzerError.Code
  ) -> IchiranAnalyzerError {
    let body = try? JSONDecoder().decode(ErrorBody.self, from: data)
    let code: IchiranAnalyzerError.Code
    switch status {
    case 10:
      code = .invalidInput
    case 9 where fallback == .notFound:
      code = .notFound
    case 1...9:
      code = .invalidPack
    case 11:
      code = .internal
    default:
      code = fallback
    }
    return IchiranAnalyzerError(
      code: code,
      message: body?.message ?? "Ichiran kernel failed with status \(status)"
    )
  }
}

extension Digest {
  fileprivate var hex: String {
    map { String(format: "%02x", $0) }.joined()
  }
}
