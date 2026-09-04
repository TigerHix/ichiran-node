import Darwin
import Foundation
import XCTest

@testable import IchiranSwift

final class IchiranSwiftTests: XCTestCase {
  func testProductOperationsAndLazyDetails() async throws {
    let fixture = try await installedFixture()
    defer { fixture.remove() }
    let analyzer = try await fixture.store.openAnalyzer()

    let ordinary = try await analyzer.analyze("庭には二羽鶏がいる。", options: .init(limit: 3))
    XCTAssertEqual(ordinary.input, "庭には二羽鶏がいる。")
    XCTAssertFalse(ordinary.paths.isEmpty)
    XCTAssertLessThanOrEqual(ordinary.paths.count, 3)

    let morphological = try await analyzer.analyze("食べました", options: .init(limit: 3))
    XCTAssertTrue(morphological.paths.first?.tokens.contains { !$0.inflection.isEmpty } == true)

    let entity = try await analyzer.analyze(
      "東京へ行く",
      options: .init(
        limit: 2,
        entities: [.init(start: 0, end: 2, boost: 10_000)]
      )
    )
    XCTAssertTrue(entity.paths.first?.tokens.contains(where: \.entity) == true)

    let defaultRomanization = try await analyzer.romanize("しんぶん")
    let kunrei = try await analyzer.romanize(
      "しんぶん",
      options: .init(method: .kunreiSiki)
    )
    XCTAssertFalse(defaultRomanization.isEmpty)
    XCTAssertFalse(kunrei.isEmpty)

    let before = try await analyzer.qualificationDiagnostics()
    XCTAssertEqual(before.detailBytesRead, 96 + before.detailPrefixBytes)
    XCTAssertLessThan(before.detailBytesRead, before.detailsFileBytes)

    let tokenDetails = try await analyzer.details(
      "三個",
      options: .init(pathIndex: 0, tokenIndex: 0, limit: 3)
    )
    XCTAssertEqual(tokenDetails.counter?.value, "3")
    XCTAssertEqual(tokenDetails.counter?.ordinal, false)
    let afterTokenDetails = try await analyzer.qualificationDiagnostics()
    XCTAssertGreaterThan(afterTokenDetails.detailBytesRead, before.detailBytesRead)
    XCTAssertLessThan(afterTokenDetails.detailBytesRead, afterTokenDetails.detailsFileBytes)

    let entry = try await analyzer.entry(0)
    XCTAssertGreaterThan(entry.seq, 0)
    let after = try await analyzer.qualificationDiagnostics()
    XCTAssertEqual(after.detailBytesRead - afterTokenDetails.detailBytesRead, after.lastDetailRead)
    XCTAssertLessThan(after.lastDetailRead, after.detailsFileBytes)
    XCTAssertFalse(after.lastCallWasMainThread)

    await analyzer.dispose()
    do {
      _ = try await analyzer.analyze("日本語")
      XCTFail("disposed analyzer accepted a call")
    } catch let error as IchiranAnalyzerError {
      XCTAssertEqual(error.code, .internal)
    }
  }

  func testExactCleanParityCorpus() async throws {
    let fixture = try await installedFixture()
    defer { fixture.remove() }
    let analyzer = try await fixture.store.openAnalyzer()
    addTeardownBlock { await analyzer.dispose() }
    let lines = try corpus(named: "clean-corpus")
    var exact = 0
    var utf16Cases = 0
    for line in lines where !line.hasPrefix("#") && !line.isEmpty {
      let fields = line.split(separator: "\t", omittingEmptySubsequences: false)
      guard fields.count == 4 else { return XCTFail("invalid clean corpus row") }
      let units = try utf16(String(fields[1]))
      let actual = try await analyzer.qualificationAnalyzeJSON(
        utf16Units: units,
        optionsJSON: Data(fields[2].utf8)
      )
      guard actual == Data(fields[3].utf8) else {
        return XCTFail("clean parity mismatch: \(fields[0])")
      }
      exact += 1
      if fields[0].hasPrefix("utf16:") { utf16Cases += 1 }
    }
    XCTAssertEqual(exact, 1_239)
    XCTAssertEqual(utf16Cases, 3)
    print("SWIFT_CLEAN_PARITY exact=1236 utf16=3 total=1239")
  }

  func testExactProductParityCorpus() async throws {
    let fixture = try await installedFixture()
    defer { fixture.remove() }
    let analyzer = try await fixture.store.openAnalyzer()
    addTeardownBlock { await analyzer.dispose() }
    let lines = try corpus(named: "product-corpus")
    var tokenDetails = 0
    var legacy = 0
    var romanization = 0
    var entries = 0
    for line in lines where !line.hasPrefix("#") && !line.isEmpty {
      let fields = line.split(separator: "\t", omittingEmptySubsequences: false)
      switch fields.first {
      case "T":
        guard fields.count == 7,
          let pathIndex = UInt32(fields[4]),
          let tokenIndex = UInt32(fields[5])
        else {
          return XCTFail("invalid token-details corpus row")
        }
        let actual = try await analyzer.qualificationTokenDetailsJSON(
          utf16Units: try utf16(String(fields[2])),
          optionsJSON: Data(fields[3].utf8),
          pathIndex: pathIndex,
          tokenIndex: tokenIndex
        )
        guard actual == Data(fields[6].utf8) else {
          return XCTFail("token-details parity mismatch: \(fields[1])")
        }
        tokenDetails += 1
      case "L":
        guard fields.count == 5 else { return XCTFail("invalid legacy corpus row") }
        let actual = try await analyzer.qualificationLegacyJSON(
          utf16Units: try utf16(String(fields[2])),
          optionsJSON: Data(fields[3].utf8)
        )
        guard actual == Data(fields[4].utf8) else {
          return XCTFail("legacy parity mismatch: \(fields[1])")
        }
        legacy += 1
      case "R":
        guard fields.count == 6 else { return XCTFail("invalid romanization corpus row") }
        let actual = try await analyzer.qualificationRomanizeJSON(
          utf16Units: try utf16(String(fields[2])),
          optionsJSON: Data(fields[3].utf8),
          method: String(fields[4])
        )
        guard actual == Data(fields[5].utf8) else {
          return XCTFail("romanization parity mismatch: \(fields[1])")
        }
        romanization += 1
      case "D":
        guard fields.count == 4, let index = UInt32(fields[2]) else {
          return XCTFail("invalid dictionary corpus row")
        }
        let actual = try await analyzer.qualificationEntryJSON(index)
        guard actual == Data(fields[3].utf8) else {
          return XCTFail("dictionary parity mismatch: \(fields[1])")
        }
        entries += 1
      default:
        return XCTFail("unknown product corpus row")
      }
    }
    XCTAssertEqual(tokenDetails, 8)
    XCTAssertEqual(legacy, 705)
    XCTAssertEqual(romanization, 8)
    XCTAssertEqual(entries, 4)
    print(
      "SWIFT_PRODUCT_PARITY token_details=8 detailed=702 utf16_detailed=3 romanization=5 utf16_romanization=3 entries=4"
    )
  }

  func testReviewedTokenDetailsOracleCasesThroughPublicAPI() async throws {
    let fixture = try await installedFixture()
    defer { fixture.remove() }
    let analyzer = try await fixture.store.openAnalyzer()
    addTeardownBlock { await analyzer.dispose() }
    var results: [String: IchiranTokenDetails] = [:]
    let oracle = try tokenDetailsOracle()
    XCTAssertEqual(oracle.formatVersion, 1)
    XCTAssertEqual(
      oracle.sourcesLockSHA256,
      "73293bfa6253812d60dbb5f604743abad896ddf3351aed9560667dec7ba13f45"
    )

    for value in oracle.cases {
      let actual = try await analyzer.details(
        value.text,
        options: .init(
          pathIndex: value.pathIndex,
          tokenIndex: value.tokenIndex,
          limit: value.options.limit,
          entities: value.options.entities ?? [],
          normalizePunctuation: value.options.normalizePunctuation ?? false
        )
      )
      XCTAssertEqual(actual, value.expected, value.name)
      results[value.name] = actual
    }

    XCTAssertEqual(
      Set(results.keys),
      Set([
        "reading-hint", "true-alternatives-nonzero-path", "restricted-hiragana",
        "restricted-katakana", "nested-via", "compound-suffix", "counter", "entity",
      ])
    )
    XCTAssertFalse(try XCTUnwrap(results["reading-hint"]).reading.contains("\u{200b}"))
    XCTAssertFalse(try XCTUnwrap(results["reading-hint"]).reading.contains("\u{200c}"))
    XCTAssertEqual(
      try XCTUnwrap(results["true-alternatives-nonzero-path"]).alternatives.map(
        \.meanings.first?.gloss),
      ["bridge", "chopsticks"]
    )
    XCTAssertEqual(results["restricted-hiragana"]?.meanings.count, 2)
    XCTAssertEqual(results["restricted-katakana"]?.meanings.count, 1)
    XCTAssertEqual(results["nested-via"]?.conjugations.first?.via.first?.root?.text, "食べる")
    XCTAssertEqual(
      results["compound-suffix"]?.components.last?.suffix,
      "looking like ... / seeming ..."
    )
    XCTAssertEqual(results["counter"]?.counter?.value, "3")
    XCTAssertEqual(results["entity"]?.meanings.first?.pos, ["n-pr"])
    XCTAssertEqual(results["entity"]?.entity, true)
  }

  func testTokenDetailsCorruptionFailureReleasesOperationAndRecovers() async throws {
    let fixture = try await installedFixture()
    defer { fixture.remove() }
    let analyzer = try await fixture.store.openAnalyzer()
    addTeardownBlock { await analyzer.dispose() }
    let line = try XCTUnwrap(
      try corpus(named: "product-corpus").first { $0.hasPrefix("T\ttoken-details:4\t") }
    )
    let fields = line.split(separator: "\t", omittingEmptySubsequences: false)
    XCTAssertEqual(fields.count, 7)
    let units = try utf16(String(fields[2]))
    let options = Data(fields[3].utf8)

    do {
      _ = try await analyzer.qualificationTokenDetailsJSON(
        utf16Units: units,
        optionsJSON: options,
        pathIndex: 0,
        tokenIndex: 0,
        corruptFirstBlock: true
      )
      XCTFail("corrupt token-details block was accepted")
    } catch let error as IchiranAnalyzerError {
      XCTAssertEqual(error.code, .invalidPack)
    }

    let recovered = try await analyzer.qualificationTokenDetailsJSON(
      utf16Units: units,
      optionsJSON: options,
      pathIndex: 0,
      tokenIndex: 0
    )
    XCTAssertEqual(recovered, Data(fields[6].utf8))
  }

  func testMalformedManifestAndPackAreRejected() async throws {
    let invalidManifest = try copyBundledRelease()
    defer { try? FileManager.default.removeItem(at: invalidManifest) }
    let manifestURL = invalidManifest.appendingPathComponent("manifest.json")
    var object = try XCTUnwrap(
      JSONSerialization.jsonObject(with: Data(contentsOf: manifestURL)) as? [String: Any]
    )
    object["packVersion"] = "tampered"
    try JSONSerialization.data(withJSONObject: object).write(to: manifestURL)
    let store = IchiranPackStore(baseDirectory: temporaryURL())
    do {
      _ = try await store.install(from: .directory(invalidManifest))
      XCTFail("unauthenticated manifest installed")
    } catch let error as IchiranPackError {
      XCTAssertEqual(error.code, .invalidManifest)
    }

    let fixture = try await installedFixture()
    defer { fixture.remove() }
    try flipByte(in: fixture.pack.hotURL, offset: 0)
    do {
      _ = try await IchiranAnalyzer.open(fixture.pack)
      XCTFail("malformed installed hot pack opened")
    } catch let error as IchiranAnalyzerError {
      XCTAssertEqual(error.code, .invalidPack)
    }
  }

  func testCorruptDetailBlockIsLazyAndRejected() async throws {
    let fixture = try await installedFixture()
    defer { fixture.remove() }
    let analyzer = try await fixture.store.openAnalyzer()
    let range = try await analyzer.qualificationDetailRange(0)
    let before = try await analyzer.qualificationDiagnostics()
    XCTAssertEqual(before.detailBytesRead, 96 + before.detailPrefixBytes)
    XCTAssertGreaterThanOrEqual(range.offset, before.detailPrefixBytes)
    await analyzer.dispose()

    try flipByte(in: fixture.pack.detailsURL, offset: range.offset + range.byteLength / 2)
    let reopened = try await fixture.store.openAnalyzer()
    addTeardownBlock { await reopened.dispose() }
    let opened = try await reopened.qualificationDiagnostics()
    XCTAssertEqual(opened.detailBytesRead, 96 + opened.detailPrefixBytes)
    do {
      _ = try await reopened.entry(0)
      XCTFail("corrupt detail block decoded")
    } catch let error as IchiranAnalyzerError {
      XCTAssertEqual(error.code, .invalidPack)
    }
  }

  func testFailedReplacementPreservesVerifiedGenerationAndOfflineRelaunch() async throws {
    let fixture = try await installedFixture()
    defer { fixture.remove() }
    let previous = fixture.pack
    let corrupt = try copyBundledRelease()
    defer { try? FileManager.default.removeItem(at: corrupt) }
    let manifest = try manifestObject(in: corrupt)
    let hot = try XCTUnwrap((manifest["hot"] as? [String: Any])?["file"] as? String)
    try flipByte(in: corrupt.appendingPathComponent(hot), offset: 128)
    do {
      _ = try await fixture.store.install(from: .directory(corrupt))
      XCTFail("corrupt replacement installed")
    } catch let error as IchiranPackError {
      XCTAssertEqual(error.code, .verificationFailed)
    }
    let stillActive = try await fixture.store.installedPack()
    XCTAssertEqual(stillActive.generationID, previous.generationID)

    // A fresh store has no network dependency and represents process relaunch.
    let relaunched = IchiranPackStore(baseDirectory: fixture.base)
    let analyzer = try await relaunched.openAnalyzer()
    let result = try await analyzer.analyze("再起動後も使える")
    XCTAssertFalse(result.paths.isEmpty)
    await analyzer.dispose()
  }

  func testRemoteReleaseDownloadPath() async throws {
    let release = try bundledRelease()
    let manifest = try manifestObject(in: release)
    let hot = try XCTUnwrap((manifest["hot"] as? [String: Any])?["file"] as? String)
    let details = try XCTUnwrap((manifest["details"] as? [String: Any])?["file"] as? String)
    let baseURL = URL(string: "https://fixture.invalid/releases/")!
    RemoteFixtureURLProtocol.responses = [
      baseURL.appendingPathComponent("manifest.json"): try Data(
        contentsOf: release.appendingPathComponent("manifest.json")
      ),
      baseURL.appendingPathComponent(hot): try Data(
        contentsOf: release.appendingPathComponent(hot)),
      baseURL.appendingPathComponent(details): try Data(
        contentsOf: release.appendingPathComponent(details)),
    ]
    defer { RemoteFixtureURLProtocol.responses = [:] }
    let configuration = URLSessionConfiguration.ephemeral
    configuration.protocolClasses = [RemoteFixtureURLProtocol.self]
    let session = URLSession(configuration: configuration)
    let directory = temporaryURL()
    defer { try? FileManager.default.removeItem(at: directory) }
    let store = IchiranPackStore(baseDirectory: directory, session: session)
    let pack = try await store.install(
      from: .remote(baseURL.appendingPathComponent("manifest.json"))
    )
    XCTAssertEqual(pack.manifestSHA256, manifest["manifestSha256"] as? String)
    let analyzer = try await store.openAnalyzer()
    let result = try await analyzer.analyze("遠隔配布")
    XCTAssertFalse(result.paths.isEmpty)
    await analyzer.dispose()
  }

  func testConcurrentCallersAndCloseUseSafety() async throws {
    let fixture = try await installedFixture()
    defer { fixture.remove() }
    let analyzer = try await fixture.store.openAnalyzer()
    let completions = await withTaskGroup(of: Int.self, returning: [Int].self) { group in
      for index in 0..<64 {
        group.addTask {
          do {
            switch index % 3 {
            case 0:
              _ = try await analyzer.analyze("並行呼び出し\(index)", options: .init(limit: 2))
            case 1:
              _ = try await analyzer.details(
                "三個", options: .init(pathIndex: 0, tokenIndex: 0, limit: 3)
              )
            default:
              _ = try await analyzer.romanize("並行呼び出し\(index)")
            }
            return 1
          } catch let error as IchiranAnalyzerError where error.code == .internal {
            return 0
          } catch {
            return -1
          }
        }
      }
      group.addTask {
        await analyzer.dispose()
        return 2
      }
      var values: [Int] = []
      for await value in group { values.append(value) }
      return values
    }
    XCTAssertEqual(completions.count, 65)
    XCTAssertFalse(completions.contains(-1))
    XCTAssertEqual(completions.filter { $0 == 2 }.count, 1)
  }

  func testRepeatedCreateAnalyzeEntryDestroyLoops() async throws {
    let fixture = try await installedFixture()
    defer { fixture.remove() }
    for _ in 0..<64 {
      let analyzer = try await IchiranAnalyzer.open(fixture.pack)
      _ = try await analyzer.analyze("繰り返し試験")
      _ = try await analyzer.details(
        "三個", options: .init(pathIndex: 0, tokenIndex: 0, limit: 3)
      )
      _ = try await analyzer.entry(0)
      await analyzer.dispose()
      await analyzer.dispose()
    }
  }

  func testExplicitAstralAndUnpairedUTF16Hooks() async throws {
    let fixture = try await installedFixture()
    defer { fixture.remove() }
    let analyzer = try await fixture.store.openAnalyzer()
    addTeardownBlock { await analyzer.dispose() }
    let options = Data(#"{"limit":1,"entities":[],"normalizePunctuation":false}"#.utf8)
    let astral = try await analyzer.qualificationAnalyzeJSON(
      utf16Units: [0xd83d, 0xde00],
      optionsJSON: options
    )
    XCTAssertTrue(String(decoding: astral, as: UTF8.self).contains(#""input":"😀""#))
    let high = try await analyzer.qualificationAnalyzeJSON(
      utf16Units: [0xd83d],
      optionsJSON: options
    )
    XCTAssertTrue(String(decoding: high, as: UTF8.self).contains(#""input":"\ud83d""#))
    let low = try await analyzer.qualificationRomanizeJSON(
      utf16Units: [0xde00],
      optionsJSON: options,
      method: "hepburn-modified"
    )
    XCTAssertTrue(String(decoding: low, as: UTF8.self).contains(#"\ude00"#))
  }

  func testBackgroundForegroundLifecycleAndNonMainExecution() async throws {
    let fixture = try await installedFixture()
    defer { fixture.remove() }
    let analyzer = try await fixture.store.openAnalyzer()
    _ = try await Task { @MainActor in
      try await analyzer.analyze("背景実行")
    }.value
    let diagnostics = try await analyzer.qualificationDiagnostics()
    XCTAssertFalse(diagnostics.lastCallWasMainThread)
    await analyzer.dispose()

    // A new store/analyzer pair models foreground re-entry after process suspension.
    let foregroundStore = IchiranPackStore(baseDirectory: fixture.base)
    let reopened = try await foregroundStore.openAnalyzer()
    let details = try await reopened.details(
      "三個", options: .init(pathIndex: 0, tokenIndex: 0, limit: 3)
    )
    XCTAssertEqual(details.counter?.value, "3")
    await reopened.dispose()
  }

  func testPerformanceAndMemoryQualification() async throws {
    let fixture = try await installedFixture()
    defer { fixture.remove() }
    let memoryBefore = memoryFootprint()
    let openStart = CFAbsoluteTimeGetCurrent()
    let analyzer = try await fixture.store.openAnalyzer()
    let coldOpenMs = milliseconds(since: openStart)
    let afterOpen = try await analyzer.qualificationDiagnostics()
    XCTAssertEqual(afterOpen.detailBytesRead, 96 + afterOpen.detailPrefixBytes)
    let firstStart = CFAbsoluteTimeGetCurrent()
    _ = try await analyzer.analyze("庭には二羽鶏がいる。", options: .init(limit: 3))
    let firstAnalysisMs = milliseconds(since: firstStart)
    let afterAnalysis = try await analyzer.qualificationDiagnostics()
    XCTAssertEqual(afterAnalysis.detailBytesRead, afterOpen.detailBytesRead)

    var samples: [Double] = []
    for _ in 0..<100 {
      let start = CFAbsoluteTimeGetCurrent()
      _ = try await analyzer.analyze("庭には二羽鶏がいる。", options: .init(limit: 3))
      samples.append(milliseconds(since: start))
    }
    samples.sort()
    let tokenDetailsStart = CFAbsoluteTimeGetCurrent()
    _ = try await analyzer.details(
      "食べさせられました",
      options: .init(pathIndex: 0, tokenIndex: 0, limit: 3)
    )
    let tokenDetailsMs = milliseconds(since: tokenDetailsStart)
    let entryStart = CFAbsoluteTimeGetCurrent()
    _ = try await analyzer.entry(0)
    let entryMs = milliseconds(since: entryStart)
    let diagnostics = try await analyzer.qualificationDiagnostics()
    let memoryAfter = memoryFootprint()
    await analyzer.dispose()

    let metrics: [String: Any] = [
      "coldOpenMs": coldOpenMs,
      "firstAnalysisMs": firstAnalysisMs,
      "warmP50Ms": samples[49],
      "warmP95Ms": samples[94],
      "lazyTokenDetailsMs": tokenDetailsMs,
      "lazyEntryMs": entryMs,
      "residentBeforeBytes": memoryBefore.resident,
      "residentSteadyBytes": memoryAfter.resident,
      "peakResidentBytes": memoryAfter.peak,
      "detailFileBytes": diagnostics.detailsFileBytes,
      "detailBytesRead": diagnostics.detailBytesRead,
    ]
    let encoded = try JSONSerialization.data(withJSONObject: metrics, options: [.sortedKeys])
    print("M5B_METRICS \(String(decoding: encoded, as: UTF8.self))")
    XCTAssertLessThan(diagnostics.detailBytesRead, diagnostics.detailsFileBytes)
  }

  private struct Fixture {
    let base: URL
    let store: IchiranPackStore
    let pack: IchiranInstalledPack

    func remove() {
      try? FileManager.default.removeItem(at: base)
    }
  }

  private struct TokenDetailsOracle: Decodable {
    struct Options: Decodable {
      let limit: Int
      let entities: [IchiranEntityHint]?
      let normalizePunctuation: Bool?
    }

    struct Case: Decodable {
      let name: String
      let text: String
      let options: Options
      let pathIndex: Int
      let tokenIndex: Int
      let expected: IchiranTokenDetails
    }

    let formatVersion: Int
    let sourcesLockSHA256: String
    let cases: [Case]

    private enum CodingKeys: String, CodingKey {
      case formatVersion
      case sourcesLockSHA256 = "sourcesLockSha256"
      case cases
    }
  }

  private func installedFixture() async throws -> Fixture {
    let base = temporaryURL()
    let store = IchiranPackStore(baseDirectory: base)
    let pack = try await store.install(from: .directory(try bundledRelease()))
    return Fixture(base: base, store: store, pack: pack)
  }

  private func bundledRelease() throws -> URL {
    guard
      let manifest = Bundle.main.url(
        forResource: "manifest",
        withExtension: "json",
        subdirectory: "Pack"
      )
    else {
      throw XCTSkip("Validation app has no prepared Pack resource")
    }
    return manifest.deletingLastPathComponent()
  }

  private func corpus(named name: String) throws -> [Substring] {
    let bundle = Bundle(for: Self.self)
    guard let url = bundle.url(forResource: name, withExtension: "tsv", subdirectory: "Generated")
    else {
      throw XCTSkip("Test bundle has no generated \(name).tsv")
    }
    return try String(contentsOf: url, encoding: .utf8)
      .split(separator: "\n", omittingEmptySubsequences: false)
  }

  private func tokenDetailsOracle() throws -> TokenDetailsOracle {
    let bundle = Bundle(for: Self.self)
    guard
      let url = bundle.url(
        forResource: "token-details-oracle",
        withExtension: "json",
        subdirectory: "Generated"
      )
    else {
      throw XCTSkip("Test bundle has no reviewed TokenDetails oracle")
    }
    return try JSONDecoder().decode(TokenDetailsOracle.self, from: Data(contentsOf: url))
  }

  private func utf16(_ hex: String) throws -> [UInt16] {
    if hex.isEmpty { return [] }
    return try hex.split(separator: ",").map { part in
      guard let value = UInt16(part, radix: 16) else {
        throw IchiranAnalyzerError(code: .internal, message: "Invalid corpus UTF-16")
      }
      return value
    }
  }

  private func temporaryURL() -> URL {
    FileManager.default.temporaryDirectory.appendingPathComponent(
      "IchiranSwiftTests-\(UUID().uuidString)",
      isDirectory: true
    )
  }

  private func copyBundledRelease() throws -> URL {
    let destination = temporaryURL()
    try FileManager.default.copyItem(at: bundledRelease(), to: destination)
    return destination
  }

  private func manifestObject(in directory: URL) throws -> [String: Any] {
    try XCTUnwrap(
      JSONSerialization.jsonObject(
        with: Data(contentsOf: directory.appendingPathComponent("manifest.json"))
      ) as? [String: Any])
  }

  private func flipByte(in file: URL, offset: Int) throws {
    let handle = try FileHandle(forUpdating: file)
    defer { try? handle.close() }
    try handle.seek(toOffset: UInt64(offset))
    let original = try XCTUnwrap(try handle.read(upToCount: 1)?.first)
    try handle.seek(toOffset: UInt64(offset))
    try handle.write(contentsOf: Data([original ^ 0xff]))
    try handle.synchronize()
  }

  private func milliseconds(since start: CFAbsoluteTime) -> Double {
    (CFAbsoluteTimeGetCurrent() - start) * 1_000
  }

  private func memoryFootprint() -> (resident: UInt64, peak: UInt64) {
    var info = task_vm_info_data_t()
    var count = mach_msg_type_number_t(
      MemoryLayout<task_vm_info_data_t>.size / MemoryLayout<integer_t>.size
    )
    let status = withUnsafeMutablePointer(to: &info) { pointer in
      pointer.withMemoryRebound(to: integer_t.self, capacity: Int(count)) { rebound in
        task_info(mach_task_self_, task_flavor_t(TASK_VM_INFO), rebound, &count)
      }
    }
    guard status == KERN_SUCCESS else { return (0, 0) }
    return (info.phys_footprint, UInt64(max(0, info.ledger_phys_footprint_peak)))
  }
}

private final class RemoteFixtureURLProtocol: URLProtocol, @unchecked Sendable {
  nonisolated(unsafe) static var responses: [URL: Data] = [:]

  override class func canInit(with request: URLRequest) -> Bool {
    request.url?.host == "fixture.invalid"
  }

  override class func canonicalRequest(for request: URLRequest) -> URLRequest {
    request
  }

  override func startLoading() {
    guard let url = request.url, let data = Self.responses[url] else {
      client?.urlProtocol(self, didFailWithError: URLError(.fileDoesNotExist))
      return
    }
    let response = HTTPURLResponse(
      url: url,
      statusCode: 200,
      httpVersion: "HTTP/1.1",
      headerFields: ["Content-Length": String(data.count)]
    )!
    client?.urlProtocol(self, didReceive: response, cacheStoragePolicy: .notAllowed)
    client?.urlProtocol(self, didLoad: data)
    client?.urlProtocolDidFinishLoading(self)
  }

  override func stopLoading() {}
}
