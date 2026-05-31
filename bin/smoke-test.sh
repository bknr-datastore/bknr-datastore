#!/usr/bin/env bash
# Two-process smoke test for bknr-repl replication.
#
# Starts a standby, then a primary that commits two messages, and verifies the
# standby applied both over the network. Uses the `bknr-repl` binary on PATH by
# default; override with BKNR_REPL=/path/to/bknr-repl.
set -euo pipefail

BIN="${BKNR_REPL:-bknr-repl}"
PORT="${PORT:-9126}"

SBOUT="$(mktemp)"; PROUT="$(mktemp)"
SDIR="$(mktemp -d)"; PDIR="$(mktemp -d)"
cleanup() { rm -rf "$SBOUT" "$PROUT" "$SDIR" "$PDIR"; }
trap cleanup EXIT

# standby first; it retries until the primary is up. Feed its read-only REPL a
# stdin (sleep then quit) so it stays alive through the test instead of hitting
# EOF immediately.
( sleep 5; echo quit ) | "$BIN" standby --port "$PORT" --dir "$SDIR/" >"$SBOUT" 2>&1 &
SB=$!
sleep 0.3

# primary: feed commands with delays so the standby is connected before commits
( sleep 2; echo "say alpha"; sleep 0.5; echo "say beta"; sleep 0.5; echo "quit" ) \
  | "$BIN" primary --port "$PORT" --dir "$PDIR/" >"$PROUT" 2>&1

wait "$SB" 2>/dev/null || true

echo "--- standby output ---"
cat "$SBOUT"

if grep -q "alpha" "$SBOUT" && grep -q "beta" "$SBOUT"; then
  echo "SMOKE TEST PASSED"
  exit 0
else
  echo "SMOKE TEST FAILED"
  exit 1
fi
