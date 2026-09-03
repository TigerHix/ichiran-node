#!/bin/sh
set -eu

# Run the source release with both host PostgreSQL transports physically absent.
# The mount and network changes exist only inside the unprivileged namespaces.

sc_socket_directory=/run/postgresql

if [ "${SOURCE_COMPILER_POSTGRES_ISOLATED:-}" = 1 ]; then
  if [ -d "$sc_socket_directory" ]; then
    mount --bind "$SOURCE_COMPILER_EMPTY_SOCKET_DIRECTORY" "$sc_socket_directory"
  fi

  # PostgreSQL also defaults to /tmp on some hosts. Replace the host directory
  # with a private disk-backed directory so large compiler spools remain bounded
  # by disk rather than RAM while no host socket crosses the namespace boundary.
  mount --bind "$SOURCE_COMPILER_PRIVATE_TMP_DIRECTORY" /tmp
  chmod 1777 /tmp
  test -w /tmp
  sc_exec_probe=/tmp/.ichiran-exec-probe.$$
  printf '%s\n' '#!/bin/sh' 'exit 0' > "$sc_exec_probe"
  chmod 700 "$sc_exec_probe"
  if ! "$sc_exec_probe" >/dev/null 2>&1; then
    rm -f "$sc_exec_probe"
    echo 'PostgreSQL-unavailable release proof requires executable private /tmp backing storage' >&2
    exit 1
  fi
  rm -f "$sc_exec_probe"

  for sc_socket_directory in /run/postgresql /var/run/postgresql /tmp; do
    for sc_port in 5432 5433; do
      test ! -e "$sc_socket_directory/.s.PGSQL.$sc_port"
    done
  done
  if [ -n "${SOURCE_COMPILER_HOST_TMP_PROBE:-}" ]; then
    test ! -e "$SOURCE_COMPILER_HOST_TMP_PROBE"
  fi
  ip -o link show lo | grep -q 'state DOWN'

  unset DATABASE_URL ICHIRAN_DB_URL PGHOST PGPORT PGSERVICE PGSERVICEFILE
  if [ "${1:-}" = --probe-only ]; then
    printf '%s\n' \
      '{"postgresqlUnavailable":true,"loopback":"down","unixSockets":{"runPostgresql":"masked","varRunPostgresql":"masked","tmp":"private-disk-bind"},"temporaryStorage":"writable-executable","ports":[5432,5433]}'
    exit 0
  fi
  exec sh scripts/source-compiler-release.sh "$@"
fi

if [ "$(uname -s)" != Linux ]; then
  echo 'PostgreSQL-unavailable release proof requires Linux namespaces' >&2
  exit 1
fi
for sc_command in git unshare mount ip bun realpath mktemp mkdir chmod rm; do
  command -v "$sc_command" >/dev/null 2>&1 || {
    echo "PostgreSQL-unavailable release proof requires $sc_command" >&2
    exit 1
  }
done

sc_repository=$(git -C "$(dirname "$0")" rev-parse --show-toplevel)
cd "$sc_repository"
sc_repository_physical=$(realpath "$sc_repository")
case "$sc_repository_physical" in
  /tmp|/tmp/*)
    echo 'PostgreSQL-unavailable release proof cannot run from a repository under /tmp' >&2
    exit 1
    ;;
esac

sc_output=
sc_expect_output=0
for sc_argument in "$@"; do
  if [ "$sc_expect_output" = 1 ]; then
    sc_output=$sc_argument
    sc_expect_output=0
    continue
  fi
  if [ "$sc_argument" = --out ]; then
    sc_expect_output=1
  fi
done
if [ -n "$sc_output" ]; then
  case "$sc_output" in
    /*) sc_output_candidate=$sc_output ;;
    *) sc_output_candidate=$sc_repository/$sc_output ;;
  esac
  sc_output_physical=$(realpath -m "$sc_output_candidate")
  case "$sc_output_physical" in
    /tmp|/tmp/*)
      echo 'PostgreSQL-unavailable release output cannot be under private /tmp' >&2
      exit 1
      ;;
  esac
fi

sc_private_tmp_directory=$(mktemp -d /var/tmp/ichiran-source-private-tmp.XXXXXX)
cleanup_source_private_tmp() {
  rm -rf -- "$sc_private_tmp_directory"
}
trap cleanup_source_private_tmp EXIT HUP INT TERM
case "$(realpath "$sc_private_tmp_directory")" in
  /tmp|/tmp/*)
    echo 'PostgreSQL-unavailable release proof requires private temp storage outside /tmp' >&2
    exit 1
    ;;
esac
sc_empty_socket_directory=$sc_private_tmp_directory/empty-postgresql
mkdir "$sc_empty_socket_directory"

unshare --user --map-root-user --mount --net --fork --propagation private \
  env SOURCE_COMPILER_POSTGRES_ISOLATED=1 \
    SOURCE_COMPILER_EMPTY_SOCKET_DIRECTORY="$sc_empty_socket_directory" \
    SOURCE_COMPILER_PRIVATE_TMP_DIRECTORY="$sc_private_tmp_directory" \
    sh "$0" "$@"
