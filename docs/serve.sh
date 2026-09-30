#!/usr/bin/env bash
# Serve the language reference on localhost. Every request re-renders
# reference.md with pandoc, and open pages reload themselves when it or
# style.css changes.
#
# Usage: docs/serve.sh [PORT]
#
# The script is also the CGI program: busybox httpd runs it through a symlink
# in a temporary cgi-bin directory.
set -euo pipefail

self="$(readlink -f "$0")"
docs="$(dirname "$self")"

if [[ -z "${GATEWAY_INTERFACE:-}" ]]; then
  port="${1:-8000}"
  root="$(mktemp -d -t hop-docs.XXXXXX)"
  trap 'rm -rf "$root"' EXIT
  mkdir "$root/cgi-bin"
  ln -s "$self" "$root/cgi-bin/reference"
  echo '<meta http-equiv="refresh" content="0; url=/cgi-bin/reference">' >"$root/index.html"
  echo "serving $docs/reference.md at http://localhost:$port"
  busybox httpd -f -p "127.0.0.1:$port" -h "$root"
  exit
fi

stamp="$(stat -c '%Y' "$docs/reference.md" "$docs/style.css" | md5sum | cut -d' ' -f1)"

if [[ "${QUERY_STRING:-}" == stamp ]]; then
  printf 'Content-Type: text/plain\r\n\r\n%s\n' "$stamp"
  exit
fi

header="<style>
$(<"$docs/style.css")
</style>
<script>
setInterval(async () => {
  try {
    const response = await fetch(\"?stamp\");
    if ((await response.text()).trim() !== \"$stamp\") location.reload();
  } catch {
    // The server is down; keep polling until it comes back.
  }
}, 500);
</script>"

if html="$(pandoc -f gfm+attributes -s --toc -N --shift-heading-level-by=-1 -M document-css=false -M pagetitle="Language Reference" -H <(echo "$header") "$docs/reference.md" 2>&1)"; then
  printf 'Content-Type: text/html; charset=utf-8\r\n\r\n%s\n' "$html"
else
  printf 'Status: 500 Internal Server Error\r\nContent-Type: text/plain\r\n\r\n%s\n' "$html"
fi
