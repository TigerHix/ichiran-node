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
      tokenIndex: tokenIndex
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

  public func entry(_ entryIndex: Int) throws -> IchiranDictionaryEntry {
    guard let index = UInt32(exactly: entryIndex) else {
      throw IchiranAnalyzerError(
        code: .invalidInput,
        message: "entryIndex must be a non-negative 32-bit integer"
      )
    }
    let data = try requiredOwner().entry(index: index)
    return try Self.decode(IchiranDictionaryEntry.self, from: data)
  }

  /// Idempotently releases the Rust handles and the file-backed details store.
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
    try requiredOwner().legacy(units: utf16Units, options: optionsJSON)
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
      corruptFirstBlock: corruptFirstBlock
    )
  }

  func qualificationEntryJSON(_ entryIndex: UInt32) throws -> Data {
    try requiredOwner().entry(index: entryIndex)
  }

  func qualificationDetailRange(_ entryIndex: UInt32) throws -> (offset: Int, byteLength: Int) {
    try requiredOwner().detailRange(index: entryIndex)
  }

  func qualificationDiagnostics() throws -> NativeDiagnostics {
    try requiredOwner().diagnostics
  }
}

struct NativeDiagnostics: Sendable, Equatable {
  let detailsFileBytes: Int
  let detailPrefixBytes: Int
  let detailBytesRead: Int
  let largestDetailRead: Int
  let lastDetailRead: Int
  let lastCallWasMainThread: Bool
}

private final class NativeOwner: @unchecked Sendable {
  private var kernel: OpaquePointer?
  private var details: OpaquePointer?
  private var detailsFile: FileHandle?
  private let detailsFileBytes: Int
  private let detailPrefixBytes: Int
  private var detailBytesRead: Int
  private var largestDetailRead: Int
  private var lastDetailRead = 0
  private var lastCallWasMainThread = false

  var diagnostics: NativeDiagnostics {
    NativeDiagnostics(
      detailsFileBytes: detailsFileBytes,
      detailPrefixBytes: detailPrefixBytes,
      detailBytesRead: detailBytesRead,
      largestDetailRead: largestDetailRead,
      lastDetailRead: lastDetailRead,
      lastCallWasMainThread: lastCallWasMainThread
    )
  }

  private init(
    kernel: OpaquePointer,
    details: OpaquePointer,
    detailsFile: FileHandle,
    detailsFileBytes: Int,
    detailPrefixBytes: Int,
    detailBytesRead: Int,
    largestDetailRead: Int
  ) {
    self.kernel = kernel
    self.details = details
    self.detailsFile = detailsFile
    self.detailsFileBytes = detailsFileBytes
    self.detailPrefixBytes = detailPrefixBytes
    self.detailBytesRead = detailBytesRead
    self.largestDetailRead = largestDetailRead
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

    let fileSize: Int
    do {
      let attributes = try FileManager.default.attributesOfItem(atPath: pack.detailsURL.path)
      guard let value = attributes[.size] as? NSNumber else {
        throw CocoaError(.fileReadCorruptFile)
      }
      fileSize = value.intValue
    } catch {
      throw IchiranAnalyzerError(
        code: .invalidPack,
        message: "Could not inspect installed details.bin: \(error.localizedDescription)"
      )
    }
    guard fileSize == pack.manifest.details.installedBytes else {
      throw IchiranAnalyzerError(
        code: .invalidPack, message: "Installed details.bin size does not match manifest")
    }

    let file: FileHandle
    do {
      file = try FileHandle(forReadingFrom: pack.detailsURL)
    } catch {
      throw IchiranAnalyzerError(
        code: .invalidPack,
        message: "Could not open installed details.bin: \(error.localizedDescription)"
      )
    }
    var kernel: OpaquePointer?
    var details: OpaquePointer?
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

      let header = try readExact(file: file, offset: 0, count: 96)
      var prefixBytes = 0
      let lengthResult = header.withUnsafeBytes { bytes in
        ichiran_detail_prefix_length(
          bytes.bindMemory(to: UInt8.self).baseAddress,
          bytes.count,
          fileSize,
          &prefixBytes
        )
      }
      _ = try NativeResult.consume(lengthResult, fallback: .invalidPack)
      guard prefixBytes >= 96, prefixBytes < fileSize else {
        throw IchiranAnalyzerError(
          code: .invalidPack, message: "Rust returned an invalid detail prefix length")
      }
      let prefix = try readExact(file: file, offset: 0, count: prefixBytes)
      let detailsResult = prefix.withUnsafeBytes { bytes in
        ichiran_detail_store_open(
          bytes.bindMemory(to: UInt8.self).baseAddress,
          bytes.count,
          fileSize,
          &details
        )
      }
      _ = try NativeResult.consume(detailsResult, fallback: .invalidPack)
      guard let details else {
        throw IchiranAnalyzerError(code: .internal, message: "Rust returned no detail-store handle")
      }
      return NativeOwner(
        kernel: kernel,
        details: details,
        detailsFile: file,
        detailsFileBytes: fileSize,
        detailPrefixBytes: prefixBytes,
        detailBytesRead: 96 + prefixBytes,
        largestDetailRead: prefixBytes
      )
    } catch {
      if let details { ichiran_detail_store_free(details) }
      if let kernel { ichiran_kernel_free(kernel) }
      try? file.close()
      throw error
    }
  }

  func close() {
    if let details {
      ichiran_detail_store_free(details)
      self.details = nil
    }
    if let kernel {
      ichiran_kernel_free(kernel)
      self.kernel = nil
    }
    if let detailsFile {
      try? detailsFile.close()
      self.detailsFile = nil
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

  func entry(index: UInt32) throws -> Data {
    let details = try requiredDetails()
    let range = try range(index: index, details: details)
    let compressed = try readDetails(offset: Int(range.offset), count: Int(range.byte_length))
    let decoded = compressed.withUnsafeBytes { bytes in
      ichiran_detail_store_decode(
        details,
        index,
        bytes.bindMemory(to: UInt8.self).baseAddress,
        bytes.count
      )
    }
    return try NativeResult.consume(decoded, fallback: .invalidPack)
  }

  func detailRange(index: UInt32) throws -> (offset: Int, byteLength: Int) {
    let details = try requiredDetails()
    let value = try range(index: index, details: details)
    return (Int(value.offset), Int(value.byte_length))
  }

  func tokenDetails(
    units: [UInt16],
    options: Data,
    pathIndex: UInt32,
    tokenIndex: UInt32,
    corruptFirstBlock: Bool = false
  ) throws -> Data {
    let kernel = try requiredKernel()
    let details = try requiredDetails()
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
      advance: { suppliedEntry, bytes in
        ichiran_kernel_token_details_step(
          kernel,
          operation,
          details,
          suppliedEntry,
          bytes.count == 0 ? nil : bytes.bindMemory(to: UInt8.self).baseAddress,
          bytes.count
        )
      }
    )
  }

  func legacy(units: [UInt16], options: Data) throws -> Data {
    let kernel = try requiredKernel()
    let details = try requiredDetails()
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
      advance: { suppliedEntry, bytes in
        ichiran_kernel_legacy_step(
          kernel,
          operation,
          details,
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
    advance: (UInt32, UnsafeRawBufferPointer) -> IchiranStepResult
  ) throws -> Data {
    defer { release(operation) }
    var suppliedEntry = UInt32.max
    var supplied = Data()
    var shouldCorrupt = corruptFirstBlock
    for _ in 0..<4_096 {
      lastCallWasMainThread = Thread.isMainThread
      let step = supplied.withUnsafeBytes { advance(suppliedEntry, $0) }
      let data = try NativeResult.consume(step: step, fallback: fallback)
      switch step.state {
      case 1:
        return data
      case 2:
        guard data.isEmpty, step.range.byte_length > 0 else {
          throw IchiranAnalyzerError(
            code: .internal, message: "Rust returned an invalid missing-detail step"
          )
        }
        suppliedEntry = step.entry_index
        supplied = try readDetails(
          offset: Int(step.range.offset),
          count: Int(step.range.byte_length)
        )
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
      code: .internal, message: "Lazy operation exceeded 4096 detail reads"
    )
  }

  private func requiredKernel() throws -> OpaquePointer {
    guard let kernel else {
      throw IchiranAnalyzerError(code: .internal, message: "Analyzer has been disposed")
    }
    return kernel
  }

  private func range(index: UInt32, details: OpaquePointer) throws -> IchiranDetailRange {
    var range = IchiranDetailRange()
    lastCallWasMainThread = Thread.isMainThread
    let result = ichiran_detail_store_range(details, index, &range)
    _ = try NativeResult.consume(result, fallback: .notFound)
    return range
  }

  private func requiredDetails() throws -> OpaquePointer {
    guard let details else {
      throw IchiranAnalyzerError(code: .internal, message: "Analyzer has been disposed")
    }
    return details
  }

  private func readDetails(offset: Int, count: Int) throws -> Data {
    guard let detailsFile else {
      throw IchiranAnalyzerError(code: .internal, message: "Analyzer has been disposed")
    }
    let data: Data
    do {
      data = try Self.readExact(file: detailsFile, offset: offset, count: count)
    } catch {
      throw IchiranAnalyzerError(
        code: .invalidPack,
        message: "Could not read the requested details.bin block: \(error.localizedDescription)"
      )
    }
    detailBytesRead += data.count
    largestDetailRead = max(largestDetailRead, data.count)
    lastDetailRead = data.count
    return data
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
