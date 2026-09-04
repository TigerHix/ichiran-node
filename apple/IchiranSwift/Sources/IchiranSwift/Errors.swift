import Foundation

public struct IchiranAnalyzerError: Error, LocalizedError, Sendable, Equatable {
  public enum Code: String, Codable, Sendable {
    case invalidInput = "invalid-input"
    case invalidPack = "invalid-pack"
    case notFound = "not-found"
    case `internal`
  }

  public let code: Code
  public let message: String

  public init(code: Code, message: String) {
    self.code = code
    self.message = message
  }

  public var errorDescription: String? { message }
}

public struct IchiranPackError: Error, LocalizedError, Sendable, Equatable {
  public enum Code: String, Sendable {
    case notInstalled = "not-installed"
    case invalidManifest = "invalid-manifest"
    case downloadFailed = "download-failed"
    case verificationFailed = "verification-failed"
    case installationFailed = "installation-failed"
    case publicationFailed = "publication-failed"
  }

  public let code: Code
  public let message: String

  public init(code: Code, message: String) {
    self.code = code
    self.message = message
  }

  public var errorDescription: String? { message }
}
