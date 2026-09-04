import CZlib
import CryptoKit
import Foundation

public actor IchiranPackStore {
  private struct ActiveMarker: Codable, Sendable {
    let formatVersion: Int
    let generation: String
    let manifestSHA256: String
  }

  private enum Location {
    case directory(URL)
    case remote(URL)

    var manifestURL: URL {
      switch self {
      case .directory(let directory):
        directory.appendingPathComponent("manifest.json", isDirectory: false)
      case .remote(let url):
        url
      }
    }

    func assetURL(named name: String) -> URL {
      switch self {
      case .directory(let directory):
        directory.appendingPathComponent(name, isDirectory: false)
      case .remote(let manifest):
        manifest.deletingLastPathComponent().appendingPathComponent(name, isDirectory: false)
      }
    }
  }

  private let baseDirectory: URL
  private let session: URLSession
  private var installationInProgress = false

  public init(baseDirectory: URL) {
    self.baseDirectory = baseDirectory.standardizedFileURL
    self.session = .shared
  }

  init(baseDirectory: URL, session: URLSession) {
    self.baseDirectory = baseDirectory.standardizedFileURL
    self.session = session
  }

  public func installedPack() throws -> IchiranInstalledPack {
    let markerURL = baseDirectory.appendingPathComponent("active.json", isDirectory: false)
    let marker: ActiveMarker
    do {
      marker = try JSONDecoder().decode(ActiveMarker.self, from: Data(contentsOf: markerURL))
    } catch let error as CocoaError where error.code == .fileReadNoSuchFile {
      throw IchiranPackError(code: .notInstalled, message: "No analyzer pack is installed")
    } catch {
      throw IchiranPackError(
        code: .notInstalled,
        message: "The active analyzer marker is missing or invalid: \(error.localizedDescription)"
      )
    }
    guard marker.formatVersion == 1,
      let generationID = UUID(uuidString: marker.generation),
      generationID.uuidString == marker.generation
    else {
      throw IchiranPackError(code: .notInstalled, message: "The active analyzer marker is invalid")
    }
    let directory =
      generationsDirectory
      .appendingPathComponent(generationID.uuidString, isDirectory: true)
    let manifest: AuthenticatedManifest
    do {
      manifest = try AuthenticatedManifest.authenticate(
        Data(
          contentsOf: directory.appendingPathComponent("manifest.json", isDirectory: false)
        ))
    } catch let error as IchiranPackError {
      throw error
    } catch {
      throw IchiranPackError(
        code: .notInstalled,
        message: "The installed manifest cannot be read: \(error.localizedDescription)"
      )
    }
    guard manifest.manifestSHA256 == marker.manifestSHA256 else {
      throw IchiranPackError(
        code: .notInstalled, message: "The active marker and manifest identities differ")
    }
    try requireSize(
      directory.appendingPathComponent("hot.bin", isDirectory: false),
      manifest.hot.installedBytes,
      label: "hot.bin"
    )
    try requireSize(
      directory.appendingPathComponent("lexicon.bin", isDirectory: false),
      manifest.lexicon.installedBytes,
      label: "lexicon.bin"
    )
    for locale in manifest.locales.keys.sorted() {
      let asset = manifest.locales[locale]!
      try requireSize(
        directory.appendingPathComponent("gloss.\(locale).bin", isDirectory: false),
        asset.installedBytes,
        label: "gloss.\(locale).bin"
      )
    }
    return IchiranInstalledPack(
      generationID: generationID,
      directory: directory,
      manifest: manifest
    )
  }

  public func openAnalyzer() async throws -> IchiranAnalyzer {
    try await IchiranAnalyzer.open(installedPack())
  }

  @discardableResult
  public func install(
    from source: IchiranPackReleaseSource,
    progress: (@Sendable (IchiranPackInstallProgress) -> Void)? = nil
  ) async throws -> IchiranInstalledPack {
    guard !installationInProgress else {
      throw IchiranPackError(
        code: .installationFailed, message: "An analyzer installation is already running")
    }
    installationInProgress = true
    defer { installationInProgress = false }

    let location: Location
    switch source {
    case .directory(let url): location = .directory(url.standardizedFileURL)
    case .remote(let url): location = .remote(url)
    }

    try prepareDirectories()
    let previousID = try? activeGenerationID()
    let generationID = UUID()
    let staging = baseDirectory.appendingPathComponent(
      ".staging-\(generationID.uuidString)",
      isDirectory: true
    )
    let finalDirectory = generationsDirectory.appendingPathComponent(
      generationID.uuidString,
      isDirectory: true
    )
    try? FileManager.default.removeItem(at: staging)
    do {
      try FileManager.default.createDirectory(at: staging, withIntermediateDirectories: false)
    } catch {
      throw IchiranPackError(
        code: .installationFailed,
        message: "Could not create analyzer staging directory: \(error.localizedDescription)"
      )
    }

    do {
      let manifestData = try await fetchManifest(location)
      let manifest = try AuthenticatedManifest.authenticate(manifestData)
      let assets = [(manifest.hot, "hot.bin"), (manifest.lexicon, "lexicon.bin")]
        + manifest.locales.keys.sorted().map { locale in
          (manifest.locales[locale]!, "gloss.\(locale).bin")
        }
      let total = Int64(assets.reduce(0) { $0 + $1.0.downloadBytes })
      var completed: Int64 = 0
      for (asset, name) in assets {
        try await install(
          asset: asset,
          named: name,
          location: location,
          staging: staging,
          completedBefore: completed,
          total: total,
          progress: progress
        )
        completed += Int64(asset.downloadBytes)
      }
      try manifestData.write(
        to: staging.appendingPathComponent("manifest.json", isDirectory: false),
        options: [.atomic]
      )

      progress?(.init(phase: .opening, completedBytes: total, totalBytes: total))
      let candidate = IchiranInstalledPack(
        generationID: generationID,
        directory: staging,
        manifest: manifest
      )
      let analyzer = try await IchiranAnalyzer.open(candidate)
      await analyzer.dispose()

      progress?(.init(phase: .publishing, completedBytes: total, totalBytes: total))
      do {
        try FileManager.default.moveItem(at: staging, to: finalDirectory)
        let marker = ActiveMarker(
          formatVersion: 1,
          generation: generationID.uuidString,
          manifestSHA256: manifest.manifestSHA256
        )
        let markerData = try JSONEncoder().encode(marker)
        try markerData.write(
          to: baseDirectory.appendingPathComponent("active.json", isDirectory: false),
          options: [.atomic]
        )
      } catch {
        try? FileManager.default.removeItem(at: finalDirectory)
        throw IchiranPackError(
          code: .publicationFailed,
          message: "Could not atomically publish analyzer generation: \(error.localizedDescription)"
        )
      }
      cleanupGenerations(keeping: Set([generationID, previousID].compactMap { $0 }))
      return IchiranInstalledPack(
        generationID: generationID,
        directory: finalDirectory,
        manifest: manifest
      )
    } catch {
      try? FileManager.default.removeItem(at: staging)
      throw error
    }
  }

  private var generationsDirectory: URL {
    baseDirectory.appendingPathComponent("generations", isDirectory: true)
  }

  private func prepareDirectories() throws {
    do {
      try FileManager.default.createDirectory(
        at: generationsDirectory,
        withIntermediateDirectories: true
      )
    } catch {
      throw IchiranPackError(
        code: .installationFailed,
        message: "Could not prepare analyzer storage: \(error.localizedDescription)"
      )
    }
  }

  private func activeGenerationID() throws -> UUID {
    let marker = try JSONDecoder().decode(
      ActiveMarker.self,
      from: Data(contentsOf: baseDirectory.appendingPathComponent("active.json"))
    )
    guard marker.formatVersion == 1, let value = UUID(uuidString: marker.generation) else {
      throw IchiranPackError(code: .notInstalled, message: "The active analyzer marker is invalid")
    }
    return value
  }

  private func fetchManifest(_ location: Location) async throws -> Data {
    switch location {
    case .directory:
      do {
        let data = try Data(contentsOf: location.manifestURL)
        guard data.count <= 64 * 1_024 else {
          throw IchiranPackError(code: .invalidManifest, message: "manifest.json exceeds 64 KiB")
        }
        return data
      } catch let error as IchiranPackError {
        throw error
      } catch {
        throw IchiranPackError(
          code: .invalidManifest,
          message: "Could not read local manifest.json: \(error.localizedDescription)"
        )
      }
    case .remote:
      do {
        let (data, response) = try await session.data(from: location.manifestURL)
        try Self.requireHTTP(response, label: "manifest")
        guard data.count <= 64 * 1_024 else {
          throw IchiranPackError(code: .invalidManifest, message: "manifest.json exceeds 64 KiB")
        }
        return data
      } catch let error as IchiranPackError {
        throw error
      } catch {
        throw IchiranPackError(
          code: .downloadFailed,
          message: "Could not download manifest.json: \(error.localizedDescription)"
        )
      }
    }
  }

  private func install(
    asset: AuthenticatedManifest.Asset,
    named installedName: String,
    location: Location,
    staging: URL,
    completedBefore: Int64,
    total: Int64,
    progress: (@Sendable (IchiranPackInstallProgress) -> Void)?
  ) async throws {
    let sourceURL: URL
    var removeSource = false
    switch location {
    case .directory:
      sourceURL = location.assetURL(named: asset.file)
    case .remote:
      do {
        let (temporary, response) = try await session.download(
          from: location.assetURL(named: asset.file)
        )
        try Self.requireHTTP(response, label: asset.file)
        if let http = response as? HTTPURLResponse,
          let contentEncoding = http.value(forHTTPHeaderField: "Content-Encoding"),
          contentEncoding.lowercased() != "identity"
        {
          throw IchiranPackError(
            code: .downloadFailed,
            message: "\(asset.file) must be served as opaque bytes without Content-Encoding"
          )
        }
        sourceURL = temporary
        removeSource = true
      } catch let error as IchiranPackError {
        throw error
      } catch {
        throw IchiranPackError(
          code: .downloadFailed,
          message: "Could not download \(asset.file): \(error.localizedDescription)"
        )
      }
    }
    defer {
      if removeSource { try? FileManager.default.removeItem(at: sourceURL) }
    }

    let downloaded = staging.appendingPathComponent(".download-\(installedName)")
    try copyAndVerifyDownload(
      sourceURL,
      to: downloaded,
      asset: asset,
      completedBefore: completedBefore,
      total: total,
      progress: progress
    )
    progress?(
      .init(
        phase: .verifying,
        completedBytes: completedBefore + Int64(asset.downloadBytes),
        totalBytes: total
      ))
    let installed = staging.appendingPathComponent(installedName, isDirectory: false)
    progress?(
      .init(
        phase: .installing,
        completedBytes: completedBefore + Int64(asset.downloadBytes),
        totalBytes: total
      ))
    switch asset.encoding {
    case .identity:
      try FileManager.default.moveItem(at: downloaded, to: installed)
    case .gzip:
      try decompressGzip(downloaded, to: installed, asset: asset)
      try FileManager.default.removeItem(at: downloaded)
    }
    try verifyInstalled(installed, asset: asset)
  }

  private func copyAndVerifyDownload(
    _ source: URL,
    to destination: URL,
    asset: AuthenticatedManifest.Asset,
    completedBefore: Int64,
    total: Int64,
    progress: (@Sendable (IchiranPackInstallProgress) -> Void)?
  ) throws {
    FileManager.default.createFile(atPath: destination.path, contents: nil)
    let input: FileHandle
    let output: FileHandle
    do {
      input = try FileHandle(forReadingFrom: source)
      output = try FileHandle(forWritingTo: destination)
    } catch {
      throw IchiranPackError(
        code: .downloadFailed,
        message: "Could not open \(asset.file): \(error.localizedDescription)"
      )
    }
    defer {
      try? input.close()
      try? output.close()
    }
    var hash = SHA256()
    var received = 0
    do {
      while let chunk = try input.read(upToCount: 1_048_576), !chunk.isEmpty {
        received += chunk.count
        guard received <= asset.downloadBytes else {
          throw IchiranPackError(
            code: .verificationFailed,
            message: "\(asset.file) exceeds its manifest byte count"
          )
        }
        hash.update(data: chunk)
        try output.write(contentsOf: chunk)
        progress?(
          .init(
            phase: .downloading,
            completedBytes: completedBefore + Int64(received),
            totalBytes: total
          ))
      }
      try output.synchronize()
    } catch let error as IchiranPackError {
      throw error
    } catch {
      throw IchiranPackError(
        code: .downloadFailed,
        message: "Could not copy \(asset.file): \(error.localizedDescription)"
      )
    }
    guard received == asset.downloadBytes else {
      throw IchiranPackError(
        code: .verificationFailed,
        message: "\(asset.file) has \(received) bytes; expected \(asset.downloadBytes)"
      )
    }
    guard hash.finalize().hex == asset.downloadSHA256 else {
      throw IchiranPackError(
        code: .verificationFailed, message: "\(asset.file) checksum does not match")
    }
  }

  private func decompressGzip(
    _ source: URL,
    to destination: URL,
    asset: AuthenticatedManifest.Asset
  ) throws {
    let stream = source.path.withCString { gzopen($0, "rb") }
    guard let stream else {
      throw IchiranPackError(
        code: .installationFailed, message: "Could not open \(asset.file) as gzip")
    }
    FileManager.default.createFile(atPath: destination.path, contents: nil)
    let output: FileHandle
    do {
      output = try FileHandle(forWritingTo: destination)
    } catch {
      gzclose(stream)
      throw IchiranPackError(
        code: .installationFailed,
        message: "Could not create \(destination.lastPathComponent): \(error.localizedDescription)"
      )
    }
    var closed = false
    defer {
      try? output.close()
      if !closed { gzclose(stream) }
    }
    var installed = 0
    var buffer = [UInt8](repeating: 0, count: 1_048_576)
    do {
      while true {
        let count = buffer.withUnsafeMutableBytes { bytes in
          gzread(stream, bytes.baseAddress, UInt32(bytes.count))
        }
        if count < 0 {
          var code: Int32 = 0
          let message = gzerror(stream, &code).map(String.init(cString:)) ?? "invalid gzip data"
          throw IchiranPackError(
            code: .installationFailed, message: "Could not decompress \(asset.file): \(message)")
        }
        if count == 0 { break }
        installed += Int(count)
        guard installed <= asset.installedBytes else {
          throw IchiranPackError(
            code: .verificationFailed,
            message: "\(asset.file) expands beyond its installed byte count"
          )
        }
        try output.write(contentsOf: Data(buffer.prefix(Int(count))))
      }
      try output.synchronize()
      let closeStatus = gzclose(stream)
      closed = true
      guard closeStatus == Z_OK else {
        throw IchiranPackError(
          code: .installationFailed, message: "Could not finish decompressing \(asset.file)")
      }
    } catch let error as IchiranPackError {
      throw error
    } catch {
      throw IchiranPackError(
        code: .installationFailed,
        message: "Could not install \(asset.file): \(error.localizedDescription)"
      )
    }
  }

  private func verifyInstalled(
    _ file: URL,
    asset: AuthenticatedManifest.Asset
  ) throws {
    let input: FileHandle
    do {
      input = try FileHandle(forReadingFrom: file)
    } catch {
      throw IchiranPackError(
        code: .verificationFailed,
        message: "Could not verify \(file.lastPathComponent): \(error.localizedDescription)"
      )
    }
    defer { try? input.close() }
    var hash = SHA256()
    var bytes = 0
    do {
      while let chunk = try input.read(upToCount: 1_048_576), !chunk.isEmpty {
        bytes += chunk.count
        guard bytes <= asset.installedBytes else { break }
        hash.update(data: chunk)
      }
    } catch {
      throw IchiranPackError(
        code: .verificationFailed,
        message: "Could not verify \(file.lastPathComponent): \(error.localizedDescription)"
      )
    }
    guard bytes == asset.installedBytes else {
      throw IchiranPackError(
        code: .verificationFailed,
        message: "\(file.lastPathComponent) has \(bytes) bytes; expected \(asset.installedBytes)"
      )
    }
    guard hash.finalize().hex == asset.installedSHA256 else {
      throw IchiranPackError(
        code: .verificationFailed,
        message: "\(file.lastPathComponent) checksum does not match"
      )
    }
  }

  private func requireSize(_ file: URL, _ expected: Int, label: String) throws {
    do {
      let attributes = try FileManager.default.attributesOfItem(atPath: file.path)
      guard (attributes[.size] as? NSNumber)?.intValue == expected else {
        throw IchiranPackError(
          code: .notInstalled,
          message: "Installed \(label) size does not match its manifest"
        )
      }
    } catch let error as IchiranPackError {
      throw error
    } catch {
      throw IchiranPackError(
        code: .notInstalled,
        message: "Installed \(label) is missing: \(error.localizedDescription)"
      )
    }
  }

  private func cleanupGenerations(keeping ids: Set<UUID>) {
    guard
      let children = try? FileManager.default.contentsOfDirectory(
        at: generationsDirectory,
        includingPropertiesForKeys: nil
      )
    else { return }
    let names = Set(ids.map(\.uuidString))
    for child in children where !names.contains(child.lastPathComponent) {
      try? FileManager.default.removeItem(at: child)
    }
  }

  private static func requireHTTP(_ response: URLResponse, label: String) throws {
    guard let response = response as? HTTPURLResponse else { return }
    guard (200...299).contains(response.statusCode) else {
      throw IchiranPackError(
        code: .downloadFailed,
        message: "\(label) download failed with HTTP \(response.statusCode)"
      )
    }
  }
}

extension Digest {
  fileprivate var hex: String {
    map { String(format: "%02x", $0) }.joined()
  }
}
