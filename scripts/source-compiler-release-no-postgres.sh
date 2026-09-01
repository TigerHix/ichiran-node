#!/bin/sh
set -eu

# Run the source release with both host PostgreSQL transports physically absent.
# The mount and network changes exist only inside the unprivileged namespaces.

sc_socket_directory=/run/postgresql

if [ "${SOURCE_COMPILER_POSTGRES_ISOLATED:-}" = 1 ]; then
  if [ -d "$sc_socket_directory" ]; then
    mount --bind "$SOURCE_COMPILER_EMPTY_SOCKET_DIRECTORY" "$sc_socket_directory"
  fi

  for sc_port in 5432 5433; do
    if [ -d "$sc_socket_directory" ]; then
      test ! -S "$sc_socket_directory/.s.PGSQL.$sc_port"
    fi
    ! pg_isready -q -h 127.0.0.1 -p "$sc_port"
    if [ -d "$sc_socket_directory" ]; then
      ! pg_isready -q -h "$sc_socket_directory" -p "$sc_port"
    fi
  done
  ip -o link show lo | grep -q 'state DOWN'

  unset DATABASE_URL ICHIRAN_DB_URL PGHOST PGPORT PGSERVICE PGSERVICEFILE
  if [ "${1:-}" = --probe-only ]; then
    printf '%s\n' \
      '{"postgresqlUnavailable":true,"loopback":"down","unixSockets":"hidden","ports":[5432,5433]}'
    exit 0
  fi
  exec bun scripts/source-compiler-release.ts "$@"
fi

if [ "$(uname -s)" != Linux ]; then
  echo 'PostgreSQL-unavailable release proof requires Linux namespaces' >&2
  exit 1
fi
for sc_command in git unshare mount pg_isready ip bun; do
  command -v "$sc_command" >/dev/null 2>&1 || {
    echo "PostgreSQL-unavailable release proof requires $sc_command" >&2
    exit 1
  }
done

sc_repository=$(git -C "$(dirname "$0")" rev-parse --show-toplevel)
cd "$sc_repository"
sc_empty_socket_directory=$(mktemp -d /tmp/ichiran-no-postgres.XXXXXX)
trap 'rmdir "$sc_empty_socket_directory"' 0

unshare --user --map-root-user --mount --net --fork --propagation private \
  env SOURCE_COMPILER_POSTGRES_ISOLATED=1 \
    SOURCE_COMPILER_EMPTY_SOCKET_DIRECTORY="$sc_empty_socket_directory" \
    sh "$0" "$@"
