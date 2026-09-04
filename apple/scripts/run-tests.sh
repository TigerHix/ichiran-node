#!/usr/bin/env bash
set -euo pipefail

repository="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
if [ -x /opt/homebrew/opt/rustup/bin/rustup ]; then
  PATH="/opt/homebrew/opt/rustup/bin:$PATH"
fi
if ! command -v bun >/dev/null && [ -x "${BUN_INSTALL:-$HOME/.bun}/bin/bun" ]; then
  PATH="${BUN_INSTALL:-$HOME/.bun}/bin:$PATH"
fi
export PATH

[ "$#" -ge 1 ] || {
  echo "usage: $0 RELEASE_DIR [--same-pack --source-lock LOCK_FILE]" >&2
  exit 2
}
for command in xcodebuild xcodegen bun; do
  command -v "$command" >/dev/null || { echo "missing required tool: $command" >&2; exit 1; }
done

"$repository/apple/scripts/build-xcframework.sh"
"$repository/apple/scripts/prepare-test-fixtures.sh" "$@"
xcodegen generate --spec "$repository/apple/ValidationApp/project.yml"

destination="${ICHIRAN_IOS_DESTINATION:-platform=iOS Simulator,name=iPhone 17,OS=26.5}"
derived="$repository/work/apple/ValidationDerivedData"
log="$repository/work/apple/swift-tests.log"
mkdir -p "$(dirname "$log")"
xcodebuild \
  -project "$repository/apple/ValidationApp/IchiranValidation.xcodeproj" \
  -scheme IchiranValidation \
  -configuration Debug \
  -destination "$destination" \
  -derivedDataPath "$derived" \
  -parallel-testing-enabled NO \
  test | tee "$log"
