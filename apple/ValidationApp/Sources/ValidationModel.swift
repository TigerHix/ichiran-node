import Foundation
import IchiranSwift

@MainActor
final class ValidationModel: ObservableObject {
  @Published var text = "庭には二羽鶏がいる。"
  @Published var remoteManifest = ""
  @Published var definitionLocale = "en"
  @Published var analysis: IchiranAnalysisResult?
  @Published var tokenDetails: IchiranTokenDetails?
  @Published var romanized = ""
  @Published var entry: IchiranDictionaryEntry?
  @Published var status = "Opening installed analyzer…"
  @Published var errorMessage: String?
  @Published var isBusy = false

  private let store: IchiranPackStore
  private var analyzer: IchiranAnalyzer?

  init() {
    let support = FileManager.default.urls(
      for: .applicationSupportDirectory,
      in: .userDomainMask
    )[0]
    store = IchiranPackStore(
      baseDirectory: support.appendingPathComponent("IchiranValidation/Analyzer", isDirectory: true)
    )
  }

  func start() async {
    await perform("Opening analyzer") {
      do {
        analyzer = try await store.openAnalyzer()
        let installed = try await store.installedPack()
        status = "Opened \(installed.packVersion) from installed storage"
      } catch let error as IchiranPackError where error.code == .notInstalled {
        try await installBundled()
      }
      try await analyze()
    }
  }

  func installBundled() async throws {
    guard
      let manifest = Bundle.main.url(
        forResource: "manifest",
        withExtension: "json",
        subdirectory: "Pack"
      )
    else {
      throw IchiranPackError(
        code: .notInstalled,
        message: "No bundled pack is present. Run apple/scripts/prepare-test-fixtures.sh first."
      )
    }
    try await install(.directory(manifest.deletingLastPathComponent()))
  }

  func installRemote() async {
    guard let url = URL(string: remoteManifest), !remoteManifest.isEmpty else {
      errorMessage = "Enter a complete manifest.json URL."
      return
    }
    await perform("Installing downloaded pack") {
      try await install(.remote(url))
      try await analyze()
    }
  }

  func analyzeAction() async {
    await perform("Analyzing") { try await analyze() }
  }

  func entryAction(_ entryIndex: Int) async {
    await perform("Loading dictionary entry \(entryIndex)") {
      guard let analyzer else {
        throw IchiranAnalyzerError(code: .internal, message: "Analyzer is not open")
      }
      entry = try await analyzer.entry(
        entryIndex,
        options: .init(locale: definitionLocale)
      )
      status = "Loaded JMdict sequence \(entry?.seq ?? 0)"
    }
  }

  func tokenAction(pathIndex: Int, tokenIndex: Int, entryIndex: Int?) async {
    await perform("Loading canonical token details") {
      guard let analyzer else {
        throw IchiranAnalyzerError(code: .internal, message: "Analyzer is not open")
      }
      tokenDetails = try await analyzer.details(
        text,
        options: .init(
          pathIndex: pathIndex,
          tokenIndex: tokenIndex,
          limit: 3,
          locale: definitionLocale
        )
      )
      if let entryIndex {
        entry = try await analyzer.entry(
          entryIndex,
          options: .init(locale: definitionLocale)
        )
      } else {
        entry = nil
      }
      status = "Loaded Rust-rendered token details"
    }
  }

  private func install(_ source: IchiranPackReleaseSource) async throws {
    let installed = try await store.install(from: source) { progress in
      Task { @MainActor in
        self.status =
          "\(progress.phase.rawValue.capitalized) \(progress.completedBytes)/\(progress.totalBytes) bytes"
      }
    }
    if let analyzer { await analyzer.dispose() }
    analyzer = try await IchiranAnalyzer.open(installed)
    status = "Installed and opened \(installed.packVersion)"
  }

  private func analyze() async throws {
    guard let analyzer else {
      throw IchiranAnalyzerError(code: .internal, message: "Analyzer is not open")
    }
    async let analyzed = analyzer.analyze(text, options: .init(limit: 3))
    async let rendered = analyzer.romanize(text)
    analysis = try await analyzed
    romanized = try await rendered
    tokenDetails = nil
    entry = nil
    status = "Analysis complete"
  }

  private func perform(_ activity: String, operation: () async throws -> Void) async {
    isBusy = true
    errorMessage = nil
    status = activity
    defer { isBusy = false }
    do {
      try await operation()
    } catch let error as IchiranAnalyzerError {
      errorMessage = "Analyzer \(error.code.rawValue): \(error.message)"
      status = "Analyzer failed"
    } catch let error as IchiranPackError {
      errorMessage = "Pack \(error.code.rawValue): \(error.message)"
      status = "Pack operation failed"
    } catch {
      errorMessage = error.localizedDescription
      status = "Operation failed"
    }
  }
}
