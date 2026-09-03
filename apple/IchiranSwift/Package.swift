// swift-tools-version: 6.0

import PackageDescription

let package = Package(
    name: "IchiranSwift",
    platforms: [
        .iOS(.v17),
        .macOS(.v14),
    ],
    products: [
        .library(name: "IchiranSwift", targets: ["IchiranSwift"]),
    ],
    targets: [
        .binaryTarget(
            name: "IchiranKernel",
            path: "Artifacts/IchiranKernel.xcframework"
        ),
        .systemLibrary(name: "CZlib"),
        .target(
            name: "IchiranSwift",
            dependencies: ["IchiranKernel", "CZlib"]
        ),
    ],
    swiftLanguageModes: [.v6]
)
