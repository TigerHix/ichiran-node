import CryptoKit
import Foundation

public enum IchiranPackReleaseSource: Sendable, Equatable {
  /// A bundle or local directory containing manifest.json and its named assets.
  case directory(URL)
  /// A remote manifest.json; asset URLs are resolved relative to it.
  case remote(URL)
}

public struct IchiranPackInstallProgress: Sendable, Equatable {
  public enum Phase: String, Sendable {
    case downloading
    case verifying
    case installing
    case opening
    case publishing
  }

  public let phase: Phase
  public let completedBytes: Int64
  public let totalBytes: Int64
}

public struct IchiranInstalledPack: Sendable, Equatable {
  public let packVersion: String
  public let sourceCommit: String
  public let manifestSHA256: String
  public let availableLocales: [String]

  let generationID: UUID
  let directory: URL
  let manifest: AuthenticatedManifest

  init(generationID: UUID, directory: URL, manifest: AuthenticatedManifest) {
    self.packVersion = manifest.packVersion
    self.sourceCommit = manifest.sourceCommit
    self.manifestSHA256 = manifest.manifestSHA256
    self.availableLocales = manifest.locales.keys.sorted()
    self.generationID = generationID
    self.directory = directory
    self.manifest = manifest
  }

  var hotURL: URL { directory.appendingPathComponent("hot.bin", isDirectory: false) }
  var lexiconURL: URL { directory.appendingPathComponent("lexicon.bin", isDirectory: false) }
  func localeURL(_ locale: String) -> URL {
    directory.appendingPathComponent("gloss.\(locale).bin", isDirectory: false)
  }
}

struct AuthenticatedManifest: Codable, Sendable, Equatable {
  struct Asset: Codable, Sendable, Equatable {
    enum Encoding: String, Codable, Sendable {
      case identity
      case gzip
    }

    let file: String
    let encoding: Encoding
    let downloadBytes: Int
    let downloadSHA256: String
    let installedBytes: Int
    let installedSHA256: String

    enum CodingKeys: String, CodingKey {
      case file, encoding, downloadBytes, installedBytes
      case downloadSHA256 = "downloadSha256"
      case installedSHA256 = "installedSha256"
    }
  }

  let formatVersion: Int
  let packVersion: String
  let sourceCommit: String
  let sourcesLockSHA256: String
  let manifestSHA256: String
  let hot: Asset
  let lexicon: Asset
  let locales: [String: Asset]

  enum CodingKeys: String, CodingKey {
    case formatVersion, packVersion, sourceCommit, hot, lexicon, locales
    case sourcesLockSHA256 = "sourcesLockSha256"
    case manifestSHA256 = "manifestSha256"
  }

  static func authenticate(_ data: Data) throws -> AuthenticatedManifest {
    let value: Any
    do {
      value = try JSONSerialization.jsonObject(with: data)
    } catch {
      throw IchiranPackError(
        code: .invalidManifest,
        message: "manifest.json is not valid JSON: \(error.localizedDescription)"
      )
    }
    guard let object = value as? [String: Any] else {
      throw invalid("manifest.json must contain one object")
    }
    try exactKeys(
      object,
      [
        "formatVersion", "packVersion", "sourceCommit", "sourcesLockSha256", "manifestSha256",
        "hot", "lexicon", "locales",
      ],
      label: "manifest"
    )
    guard try integer(object["formatVersion"], label: "formatVersion") == 2 else {
      throw invalid("manifest formatVersion must be 2")
    }
    let packVersion = try string(object["packVersion"], label: "packVersion")
    guard !packVersion.isEmpty, packVersion.utf8.count <= 128 else {
      throw invalid("manifest packVersion must contain 1...128 UTF-8 bytes")
    }
    let sourceCommit = try string(object["sourceCommit"], label: "sourceCommit")
    guard isLowerHex(sourceCommit, count: 40) else {
      throw invalid("manifest sourceCommit must be a full lowercase Git object ID")
    }
    let sourcesLock = try string(object["sourcesLockSha256"], label: "sourcesLockSha256")
    let manifestSHA = try string(object["manifestSha256"], label: "manifestSha256")
    guard isLowerHex(sourcesLock, count: 64), isLowerHex(manifestSHA, count: 64) else {
      throw invalid("manifest digests must be lowercase SHA-256 values")
    }
    let hot = try asset(object["hot"], name: "hot")
    let lexicon = try asset(object["lexicon"], name: "lexicon")
    guard let localeObjects = object["locales"] as? [String: Any],
      localeObjects["en"] != nil, localeObjects["zh-Hans"] != nil
    else {
      throw invalid("manifest locales must include en and zh-Hans")
    }
    var locales: [String: Asset] = [:]
    for locale in localeObjects.keys.sorted() {
      guard locale.range(
        of: #"^[A-Za-z]{2,3}(?:-[A-Za-z0-9]{2,8})*$"#,
        options: .regularExpression
      ) != nil else {
        throw invalid("manifest has an invalid locale \(locale)")
      }
      locales[locale] = try asset(localeObjects[locale], name: locale, locale: true)
    }
    let manifest = AuthenticatedManifest(
      formatVersion: 2,
      packVersion: packVersion,
      sourceCommit: sourceCommit,
      sourcesLockSHA256: sourcesLock,
      manifestSHA256: manifestSHA,
      hot: hot,
      lexicon: lexicon,
      locales: locales
    )
    let digest = SHA256.hash(data: Data(manifest.digestInput.utf8)).hex
    guard digest == manifestSHA else {
      throw invalid("manifest checksum does not match its authenticated fields")
    }
    return manifest
  }

  private static func asset(_ value: Any?, name: String, locale: Bool = false) throws -> Asset {
    guard let object = value as? [String: Any] else {
      throw invalid("manifest is missing \(name)")
    }
    try exactKeys(
      object,
      ["file", "encoding", "downloadBytes", "downloadSha256", "installedBytes", "installedSha256"],
      label: name
    )
    let encodingText = try string(object["encoding"], label: "\(name).encoding")
    guard let encoding = Asset.Encoding(rawValue: encodingText) else {
      throw invalid("manifest \(name).encoding must be identity or gzip")
    }
    let file = try string(object["file"], label: "\(name).file")
    let expectedFile = "\(locale ? "gloss." : "")\(name).bin\(encoding == .gzip ? ".gz" : "")"
    guard file == expectedFile else {
      throw invalid("manifest \(name).file must be \(expectedFile)")
    }
    let downloadBytes = try integer(object["downloadBytes"], label: "\(name).downloadBytes")
    let installedBytes = try integer(object["installedBytes"], label: "\(name).installedBytes")
    guard downloadBytes > 0, installedBytes > 0 else {
      throw invalid("manifest \(name) byte counts must be positive")
    }
    let downloadSHA = try string(object["downloadSha256"], label: "\(name).downloadSha256")
    let installedSHA = try string(object["installedSha256"], label: "\(name).installedSha256")
    guard isLowerHex(downloadSHA, count: 64), isLowerHex(installedSHA, count: 64) else {
      throw invalid("manifest \(name) digests must be lowercase SHA-256 values")
    }
    if encoding == .identity,
      downloadBytes != installedBytes || downloadSHA != installedSHA
    {
      throw invalid("manifest \(name) identity sizes and digests must match")
    }
    return Asset(
      file: file,
      encoding: encoding,
      downloadBytes: downloadBytes,
      downloadSHA256: downloadSHA,
      installedBytes: installedBytes,
      installedSHA256: installedSHA
    )
  }

  private static func exactKeys(
    _ object: [String: Any],
    _ expected: Set<String>,
    label: String
  ) throws {
    guard Set(object.keys) == expected else {
      throw invalid("manifest \(label) has missing or unsupported fields")
    }
  }

  private static func integer(_ value: Any?, label: String) throws -> Int {
    guard let number = value as? NSNumber,
      CFGetTypeID(number) != CFBooleanGetTypeID(),
      number.doubleValue.isFinite,
      number.doubleValue.rounded() == number.doubleValue,
      number.doubleValue >= 0,
      number.doubleValue <= Double(Int.max)
    else {
      throw invalid("manifest \(label) must be a non-negative integer")
    }
    return number.intValue
  }

  private static func string(_ value: Any?, label: String) throws -> String {
    guard let value = value as? String else {
      throw invalid("manifest \(label) must be a string")
    }
    return value
  }

  private static func isLowerHex(_ value: String, count: Int) -> Bool {
    value.utf8.count == count
      && value.utf8.allSatisfy { byte in
        (48...57).contains(byte) || (97...102).contains(byte)
      }
  }

  private static func invalid(_ message: String) -> IchiranPackError {
    IchiranPackError(code: .invalidManifest, message: message)
  }

  private var digestInput: String {
    let localeJSON = locales.keys.sorted().map { locale in
      "\(Self.quote(locale)):\(Self.assetJSON(locales[locale]!))"
    }.joined(separator: ",")
    return "{" + "\"formatVersion\":2," + "\"packVersion\":\(Self.quote(packVersion)),"
      + "\"sourceCommit\":\(Self.quote(sourceCommit)),"
      + "\"sourcesLockSha256\":\(Self.quote(sourcesLockSHA256)),"
      + "\"hot\":\(Self.assetJSON(hot)),"
      + "\"lexicon\":\(Self.assetJSON(lexicon)),"
      + "\"locales\":{\(localeJSON)}" + "}"
  }

  private static func assetJSON(_ asset: Asset) -> String {
    "{" + "\"file\":\(quote(asset.file))," + "\"encoding\":\(quote(asset.encoding.rawValue)),"
      + "\"downloadBytes\":\(asset.downloadBytes),"
      + "\"downloadSha256\":\(quote(asset.downloadSHA256)),"
      + "\"installedBytes\":\(asset.installedBytes),"
      + "\"installedSha256\":\(quote(asset.installedSHA256))" + "}"
  }

  private static func quote(_ value: String) -> String {
    var output = "\""
    for scalar in value.unicodeScalars {
      switch scalar.value {
      case 0x08: output += "\\b"
      case 0x09: output += "\\t"
      case 0x0a: output += "\\n"
      case 0x0c: output += "\\f"
      case 0x0d: output += "\\r"
      case 0x22: output += "\\\""
      case 0x5c: output += "\\\\"
      case 0x00...0x1f: output += String(format: "\\u%04x", scalar.value)
      default: output.append(String(scalar))
      }
    }
    output += "\""
    return output
  }
}

extension Digest {
  fileprivate var hex: String {
    map { String(format: "%02x", $0) }.joined()
  }
}
