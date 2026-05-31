#!/usr/bin/env bash
# Two-process integration tests for bknr-repl replication: the sync scenarios
# that cannot be unit-tested in one image (two live stores share the global CLOS
# class indices, so they must be separate processes).
#
# Uses the `bknr-repl` binary on PATH; override with BKNR_REPL=/path/to/bknr-repl.
# Build first:  ros build bin/bknr-repl.ros && install -m755 bin/bknr-repl ~/.local/bin/
set -uo pipefail

BIN="${BKNR_REPL:-bknr-repl}"
BASE="$(mktemp -d)"
PASS=0; FAIL=0
trap 'rm -rf "$BASE"' EXIT

ok()   { echo "  OK:   $1"; PASS=$((PASS+1)); }
bad()  { echo "  FAIL: $1"; FAIL=$((FAIL+1)); }
has()  { grep -q -- "$2" "$1"; }

# ---------------------------------------------------------------------------
echo "== Scenario A: late join AFTER a snapshot gets the full pre-snapshot state =="
A_SB="$BASE/a-sb"; A_PR="$BASE/a-pr"; A_OUT="$BASE/a-sb.out"
# primary commits A,B, snapshots (rotating the log), commits C, then keeps serving
( echo "say A"; sleep .3; echo "say B"; sleep .3; echo snapshot; sleep .3; echo "say C"; sleep 4; echo quit ) \
  | "$BIN" primary --reset --port 9151 --dir "$A_PR/" >/dev/null 2>&1 &
sleep 2.5   # connect only AFTER the snapshot
( sleep 2; echo list; sleep .5; echo quit ) \
  | "$BIN" standby --port 9151 --dir "$A_SB/" >"$A_OUT" 2>&1
wait 2>/dev/null
for m in A B C; do has "$A_OUT" "  #. .$m\$" && ok "pre/post-snapshot message $m replicated" || bad "missing $m after snapshot bootstrap"; done

# ---------------------------------------------------------------------------
echo "== Scenario B: reconnect RESUMES the objects missed during primary downtime =="
B_SB="$BASE/b-sb"; B_PR="$BASE/b-pr"; B_OUT="$BASE/b-sb.out"
( sleep 9; echo list; sleep 1; echo quit ) \
  | "$BIN" standby --follow --port 9152 --dir "$B_SB/" >"$B_OUT" 2>&1 &
sleep .3
( sleep 1; echo "say one"; sleep .4; echo "say two"; sleep .6; echo quit ) \
  | "$BIN" primary --reset --port 9152 --dir "$B_PR/" >/dev/null 2>&1     # primary #1, then down
( sleep .5; echo "say three"; sleep .4; echo "say four"; sleep .6; echo quit ) \
  | "$BIN" primary --port 9152 --dir "$B_PR/" >/dev/null 2>&1             # persistent restart, adds more
wait 2>/dev/null
has "$B_OUT" "in sync (resume" && ok "replica resumed (not full re-bootstrap)" || bad "expected a resume on reconnect"
has "$B_OUT" "latest: three" && bad "missed object 'three' shown as LIVE (should be silent catch-up)" || ok "missed object applied silently, not as a live event"
for m in one two three four; do has "$B_OUT" "  #. .$m\$" && ok "final state has $m" || bad "final state missing $m"; done

# ---------------------------------------------------------------------------
echo "== Scenario C: a RESET primary (new epoch) makes the replica re-bootstrap =="
C_SB="$BASE/c-sb"; C_PR="$BASE/c-pr"; C_OUT="$BASE/c-sb.out"
( sleep 8; echo list; sleep 1; echo quit ) \
  | "$BIN" standby --follow --port 9153 --dir "$C_SB/" >"$C_OUT" 2>&1 &
sleep .3
( sleep 1; echo "say old1"; sleep .4; echo "say old2"; sleep .6; echo quit ) \
  | "$BIN" primary --reset --port 9153 --dir "$C_PR/" >/dev/null 2>&1     # primary #1
( sleep .5; echo "say fresh"; sleep .6; echo quit ) \
  | "$BIN" primary --reset --port 9153 --dir "$C_PR/" >/dev/null 2>&1     # #2 RESET -> new epoch + data
wait 2>/dev/null
# second connect must be a bootstrap, and the replica must mirror the NEW primary
[ "$(grep -c 'in sync (bootstrap' "$C_OUT")" -ge 2 ] && ok "re-bootstrapped on epoch change" || bad "expected a second bootstrap"
has "$C_OUT" "  #. .fresh\$" && ok "replica mirrors the new primary's data" || bad "missing new primary data 'fresh'"
# look only at the final list (tail), so we don't match earlier live-event lines
if tail -n 10 "$C_OUT" | grep -q "old1"; then bad "stale data 'old1' still in final state"; else ok "stale data dropped after epoch change"; fi

# ---------------------------------------------------------------------------
echo "== Scenario D: two replicas are served concurrently (fan-out) =="
D_PR="$BASE/d-pr"; D_S1="$BASE/d-s1"; D_S2="$BASE/d-s2"; D_O1="$BASE/d-s1.out"; D_O2="$BASE/d-s2.out"
( sleep 1.5; echo "say multi"; sleep 1; echo quit ) \
  | "$BIN" primary --reset --port 9154 --dir "$D_PR/" >/dev/null 2>&1 &
sleep .3
( sleep 3; echo list; sleep .3; echo quit ) | "$BIN" standby --port 9154 --dir "$D_S1/" >"$D_O1" 2>&1 &
( sleep 3; echo list; sleep .3; echo quit ) | "$BIN" standby --port 9154 --dir "$D_S2/" >"$D_O2" 2>&1 &
wait 2>/dev/null
has "$D_O1" "  #. .multi\$" && ok "replica 1 received the fan-out message" || bad "replica 1 missed it"
has "$D_O2" "  #. .multi\$" && ok "replica 2 received the fan-out message" || bad "replica 2 missed it"

# ---------------------------------------------------------------------------
echo "== Scenario E: the replica REPL outlives the primary, data retained =="
E_PR="$BASE/e-pr"; E_SB="$BASE/e-sb"; E_OUT="$BASE/e-sb.out"
( sleep 5; echo list; sleep .3; echo quit ) \
  | "$BIN" standby --port 9155 --dir "$E_SB/" >"$E_OUT" 2>&1 &
sleep .3
( sleep 1; echo "say durable"; sleep .5; echo quit ) \
  | "$BIN" primary --reset --port 9155 --dir "$E_PR/" >/dev/null 2>&1   # primary dies at ~1.5s
wait 2>/dev/null
has "$E_OUT" "primary disconnected" && ok "replica noticed the primary died" || bad "no disconnect notice"
# the 'list' runs at ~5s, well after the primary died, and must still show the data
tail -n 6 "$E_OUT" | grep -q "durable" && ok "data still queryable after primary death" || bad "data lost after primary death"

# ---------------------------------------------------------------------------
echo "== Scenario F: consistency under a burst of commits (writer queue / fan-out) =="
F_PR="$BASE/f-pr"; F_SB="$BASE/f-sb"; F_OUT="$BASE/f-sb.out"
( sleep 7; echo count; sleep .3; echo quit ) \
  | "$BIN" standby --follow --port 9156 --dir "$F_SB/" >"$F_OUT" 2>&1 &
sleep .3
( sleep 1; for i in $(seq 1 40); do echo "say msg-$i"; done; sleep 2.5; echo quit ) \
  | "$BIN" primary --reset --port 9156 --dir "$F_PR/" >/dev/null 2>&1
wait 2>/dev/null
if grep -q "40 message" "$F_OUT"; then ok "replica applied all 40 burst commits"; else bad "replica lost commits under burst"; fi

# ---------------------------------------------------------------------------
echo "== Scenario G: shared-secret auth gates replication =="
# G1: matching secret -> replica receives data
G_PR="$BASE/g-pr"; G_SB="$BASE/g-sb"; G_OUT="$BASE/g-sb.out"
( sleep 3; echo count; sleep .3; echo quit ) \
  | "$BIN" standby --port 9157 --dir "$G_SB/" --secret s3cret >"$G_OUT" 2>&1 &
sleep .3
( sleep 1; echo "say authed"; sleep .8; echo quit ) \
  | "$BIN" primary --reset --port 9157 --dir "$G_PR/" --secret s3cret >/dev/null 2>&1
wait 2>/dev/null
grep -q "1 message" "$G_OUT" && ok "authenticated replica received data" || bad "auth blocked a valid replica"
# G2: no secret on the replica -> rejected by a secret-protected primary
G_PR2="$BASE/g-pr2"; G_SB2="$BASE/g-sb2"; G_OUT2="$BASE/g-sb2.out"
( sleep 3; echo count; sleep .3; echo quit ) \
  | "$BIN" standby --port 9158 --dir "$G_SB2/" >"$G_OUT2" 2>&1 &
sleep .3
( sleep 1; echo "say secret-data"; sleep .8; echo quit ) \
  | "$BIN" primary --reset --port 9158 --dir "$G_PR2/" --secret s3cret >/dev/null 2>&1
wait 2>/dev/null
if grep -q "secret-data\|1 message" "$G_OUT2"; then bad "unauthenticated replica received data!"; else ok "unauthenticated replica rejected, no data leaked"; fi

# ---------------------------------------------------------------------------
echo "== Scenario H: replication over TLS (+ auth) =="
if command -v openssl >/dev/null 2>&1; then
  H_PR="$BASE/h-pr"; H_SB="$BASE/h-sb"; H_OUT="$BASE/h-sb.out"
  openssl req -x509 -newkey rsa:2048 -keyout "$BASE/key.pem" -out "$BASE/cert.pem" \
    -days 1 -nodes -subj "/CN=primary" >/dev/null 2>&1
  ( sleep 3; echo count; sleep .3; echo quit ) \
    | "$BIN" standby --port 9159 --dir "$H_SB/" --secret s3cret --tls >"$H_OUT" 2>&1 &
  sleep .3
  ( sleep 1; echo "say over-tls"; sleep .8; echo quit ) \
    | "$BIN" primary --reset --port 9159 --dir "$H_PR/" --secret s3cret \
        --tls-cert "$BASE/cert.pem" --tls-key "$BASE/key.pem" >/dev/null 2>&1
  wait 2>/dev/null
  grep -q "1 message" "$H_OUT" && ok "replication works over TLS + auth" || bad "TLS replication failed"
else
  echo "  SKIP: openssl not available"
fi

# ---------------------------------------------------------------------------
echo
echo "integration: $PASS passed, $FAIL failed"
[ "$FAIL" -eq 0 ]
