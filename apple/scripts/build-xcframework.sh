#!/usr/bin/env bash
set -euo pipefail

repository="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
crate="$repository/packages/rust-kernel"
artifacts="$repository/apple/IchiranSwift/Artifacts"
work="$repository/work/apple/xcframework"
output="$artifacts/IchiranKernel.xcframework"
zip_output="$artifacts/IchiranKernel.xcframework.zip"
audit="$work/audit.txt"
toolchain=1.92.0
targets=(
  aarch64-apple-ios
  aarch64-apple-ios-sim
  x86_64-apple-ios
  aarch64-apple-darwin
  x86_64-apple-darwin
)
symbols=(
  _ichiran_buffer_free
  _ichiran_lexicon_prefix_length
  _ichiran_lexicon_store_decode
  _ichiran_lexicon_store_entry_count
  _ichiran_lexicon_store_free
  _ichiran_lexicon_store_open
  _ichiran_lexicon_store_range
  _ichiran_locale_prefix_length
  _ichiran_locale_store_decode
  _ichiran_locale_store_free
  _ichiran_locale_store_open
  _ichiran_locale_store_range
  _ichiran_kernel_abi_version
  _ichiran_kernel_analyze_utf16
  _ichiran_kernel_free
  _ichiran_kernel_legacy_begin_utf16
  _ichiran_kernel_legacy_step
  _ichiran_kernel_open
  _ichiran_kernel_romanize_utf16
  _ichiran_kernel_token_details_begin_utf16
  _ichiran_kernel_token_details_step
  _ichiran_legacy_operation_free
  _ichiran_token_details_operation_free
)

fail() {
  echo "build-xcframework: $*" >&2
  exit 1
}

[ "$(uname -s)" = Darwin ] || fail "Apple artifacts require macOS"
if [ -x /opt/homebrew/opt/rustup/bin/rustup ]; then
  PATH="/opt/homebrew/opt/rustup/bin:$PATH"
  export PATH
fi
case "$(uname -m)" in
  arm64|x86_64) host_arch="$(uname -m)" ;;
  *) fail "unsupported host architecture: $(uname -m)" ;;
esac
for command in rustup xcodebuild xcrun jq lipo plutil shasum zip; do
  command -v "$command" >/dev/null || fail "missing required tool: $command"
done
rustup run "$toolchain" rustc --version >/dev/null 2>&1 \
  || fail "Rust toolchain $toolchain is not installed"
installed_targets="$(rustup target list --toolchain "$toolchain" --installed)"
for target in "${targets[@]}"; do
  grep -Fxq "$target" <<<"$installed_targets" \
    || fail "Rust target $target is not installed for $toolchain"
done

mkdir -p \
  "$artifacts" \
  "$work/headers/device" \
  "$work/headers/simulator" \
  "$work/headers/macos"
rm -rf "$output" "$zip_output"
cp "$crate/include/ichiran_kernel.h" "$crate/include/module.modulemap" "$work/headers/device/"
cp "$crate/include/ichiran_kernel.h" "$crate/include/module.modulemap" "$work/headers/simulator/"
cp "$crate/include/ichiran_kernel.h" "$crate/include/module.modulemap" "$work/headers/macos/"

for target in "${targets[@]}"; do
  rustup run "$toolchain" cargo build \
    --manifest-path "$crate/Cargo.toml" \
    --release \
    --locked \
    --target "$target"
done

device="$crate/target/aarch64-apple-ios/release/libichiran_kernel.a"
sim_arm="$crate/target/aarch64-apple-ios-sim/release/libichiran_kernel.a"
sim_x86="$crate/target/x86_64-apple-ios/release/libichiran_kernel.a"
simulator="$work/libichiran_kernel-simulator.a"
mac_arm="$crate/target/aarch64-apple-darwin/release/libichiran_kernel.a"
mac_x86="$crate/target/x86_64-apple-darwin/release/libichiran_kernel.a"

for archive in "$device" "$sim_arm" "$sim_x86" "$mac_arm" "$mac_x86"; do
  [ -s "$archive" ] || fail "missing archive: $archive"
done

# Device and simulator archives are never combined. Only the two simulator
# architectures form a universal archive for one XCFramework simulator slice.
lipo -create "$sim_arm" "$sim_x86" -output "$simulator"

audit_arches() {
  local archive="$1"
  local expected="$2"
  local actual
  actual="$(lipo -archs "$archive")"
  [ "$actual" = "$expected" ] \
    || fail "architecture mismatch for $archive: expected '$expected', got '$actual'"
}

audit_symbols() {
  local archive="$1"
  local actual="$work/actual-symbols.txt"
  local expected="$work/expected-symbols.txt"
  xcrun nm -gU "$archive" 2>/dev/null \
    | awk '/_ichiran_/ { print $NF }' \
    | sort -u > "$actual"
  printf '%s\n' "${symbols[@]}" | sort -u > "$expected"
  cmp -s "$expected" "$actual" || {
    diff -u "$expected" "$actual" >&2 || true
    fail "incomplete exported Ichiran symbol set in $archive"
  }
}

audit_arches "$device" arm64
audit_arches "$sim_arm" arm64
audit_arches "$sim_x86" x86_64
audit_arches "$simulator" "x86_64 arm64"
audit_arches "$mac_arm" arm64
audit_arches "$mac_x86" x86_64
for archive in "$device" "$sim_arm" "$sim_x86" "$simulator" "$mac_arm" "$mac_x86"; do
  audit_symbols "$archive"
done

host_archive="$mac_arm"
[ "$host_arch" = x86_64 ] && host_archive="$mac_x86"
xcrun clang -std=c11 -Wall -Wextra -Werror \
  -I "$crate/include" \
  "$repository/apple/Support/abi_probe.c" \
  "$host_archive" \
  -lpthread -lm \
  -o "$work/abi-probe"
"$work/abi-probe" | grep -Fx "Ichiran kernel ABI v7" >/dev/null \
  || fail "ABI runtime probe did not report version 7"

xcodebuild -create-xcframework \
  -library "$device" -headers "$work/headers/device" \
  -library "$simulator" -headers "$work/headers/simulator" \
  -library "$host_archive" -headers "$work/headers/macos" \
  -output "$output"

[ -f "$output/Info.plist" ] || fail "XCFramework creation produced no Info.plist"
# xcodebuild does not keep AvailableLibraries in a stable order when an
# XCFramework has three slices. Canonicalize the array and dictionary keys so
# repeated builds of identical archives also produce an identical plist/zip.
canonical_info="$work/Info.canonical.json"
plutil -convert json -o - "$output/Info.plist" \
  | jq -S '.AvailableLibraries |= sort_by(.LibraryIdentifier)' > "$canonical_info"
plutil -convert xml1 -o "$output/Info.plist" "$canonical_info"
plutil -lint "$output/Info.plist" >/dev/null
xc_device="$output/ios-arm64/libichiran_kernel.a"
xc_simulator="$output/ios-arm64_x86_64-simulator/libichiran_kernel-simulator.a"
xc_macos="$output/macos-$host_arch/libichiran_kernel.a"
for slice in "$xc_device" "$xc_simulator" "$xc_macos"; do
  [ -s "$slice" ] || fail "XCFramework is missing slice: $slice"
  [ -f "$(dirname "$slice")/Headers/ichiran_kernel.h" ] \
    || fail "XCFramework slice is missing ichiran_kernel.h: $slice"
  [ -f "$(dirname "$slice")/Headers/module.modulemap" ] \
    || fail "XCFramework slice is missing module.modulemap: $slice"
  audit_symbols "$slice"
done
audit_arches "$xc_device" arm64
audit_arches "$xc_simulator" "x86_64 arm64"
audit_arches "$xc_macos" "$host_arch"
# Normalize archive entry timestamps and ordering so the distributable zip is
# byte-reproducible when its XCFramework contents are unchanged.
find "$output" -type f -exec touch -t 200001010000 {} +
(
  cd "$artifacts"
  find "$(basename "$output")" -type f | LC_ALL=C sort \
    | zip -X -q "$zip_output" -@
)

tree_hash="$({
  while IFS= read -r file; do
    relative="${file#"$output"/}"
    printf '%s  %s\n' "$(shasum -a 256 "$file" | awk '{print $1}')" "$relative"
  done < <(find "$output" -type f | LC_ALL=C sort)
} | shasum -a 256 | awk '{print $1}')"

{
  echo "toolchain: $(rustup run "$toolchain" rustc --version)"
  echo "xcode: $(xcodebuild -version | tr '\n' ' ')"
  echo "sdk: $(xcrun --sdk iphoneos --show-sdk-version)"
  echo "abi: 7"
  echo "symbols: ${#symbols[@]}"
  echo "xcframework: $output"
  echo "xcframework_tree_bytes: $(find "$output" -type f -exec stat -f %z {} + | awk '{sum += $1} END {print sum}')"
  echo "xcframework_tree_sha256: $tree_hash"
  echo "xcframework_zip_bytes: $(stat -f %z "$zip_output")"
  echo "xcframework_zip_sha256: $(shasum -a 256 "$zip_output" | awk '{print $1}')"
  for archive in "$device" "$sim_arm" "$sim_x86" "$simulator" "$mac_arm" "$mac_x86"; do
    echo "archive: $archive"
    echo "  architectures: $(lipo -archs "$archive")"
    echo "  bytes: $(stat -f %z "$archive")"
    echo "  sha256: $(shasum -a 256 "$archive" | awk '{print $1}')"
  done
} | tee "$audit"
