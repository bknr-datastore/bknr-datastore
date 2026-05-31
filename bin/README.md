# bknr-repl — live replication demo

A tiny CLI that demonstrates bknr.datastore log-shipping replication in two
terminals. One process is the **primary** (accepts writes); the other is a
**standby** that connects over TCP and applies the primary's transactions in
real time, printing each change as it arrives.

The same binary contains both roles and the shared domain code (a trivial
`message` class), so a standby can replay the primary's transactions.

## Build & install

Requires [Roswell](https://github.com/roswell/roswell) (`ros`) with SBCL.

```sh
ros build bin/bknr-repl.ros          # produces bin/bknr-repl
install -m755 bin/bknr-repl ~/.local/bin/bknr-repl
```

The built binary is self-contained: the bknr systems are compiled into the
image, so it does not need the source tree or Quicklisp at runtime.

## Run it in two terminals

Terminal 1 — the primary:

```sh
bknr-repl primary
```

```
primary: serving replicas on 127.0.0.1:9100
commands:  say <text> | list | clients | snapshot | quit
> say hello
  committed #0 (LSN 1)
```

Terminal 2 — the standby:

```sh
bknr-repl standby
```

```
standby: read-only REPL.  commands: list | count | lsn | help | quit
standby> standby: connecting to 127.0.0.1:9100 ...
[replica] LSN 1  new message  |  1 message(s)  |  latest: hello
```

Type more `say ...` lines in the primary and watch them appear in the standby
instantly. A standby that connects *after* the primary already has data — even
after a `snapshot` — is **bootstrapped** with the primary's full current state
(snapshot + log, transferred over the socket), then receives live updates.

The standby applies the stream in a background thread while the foreground is a
**read-only REPL** (`list`, `count`, `lsn`, `quit`). When the primary dies the
stream stops but the REPL stays alive, so you can `list` and confirm the
replicated data is retained.

### Catch-up vs live

Catch-up (the initial bootstrap, and the resume delta on reconnect) is applied
**silently** — it brings the local copy up to the primary's current state without
firing per-event callbacks, so a reconnecting replica does not replay everything
it missed as if it were live. The library signals the catch-up→live boundary
(`on-sync`); only genuinely live transactions after that fire `on-apply`:

```
standby: in sync (bootstrap, LSN 0) — 0 message(s); now streaming live
[replica] LSN 1  new message  |  1 message(s)  |  latest: hello
```

### Reconnect and resume

With `--follow`, the standby reconnects after the primary goes away, with
exponential backoff. On reconnect it **resumes from its last applied LSN** —
the primary sends only the delta it missed — falling back to a full bootstrap if
the primary's identity (epoch) changed or the needed log was rotated away by a
snapshot. So if the primary restarts (persistent) and adds objects while the
replica was disconnected, the replica catches up to exactly those objects.

A `snapshot` on the primary writes an internal `PREPARE-FOR-SNAPSHOT` marker
transaction; the standby labels it `checkpoint PREPARE-FOR-SNAPSHOT` (an LSN bump
with no message change), not a new message.

### Options

```
bknr-repl primary|standby [--dir DIR] [--host HOST] [--port PORT] [--follow] [--reset]
                          [--secret S] [--tls-cert C --tls-key K | --tls]
```

- `--dir`    store directory (default `/tmp/bknr-repl-<role>/`)
- `--host`   bind/connect host (default `127.0.0.1`)
- `--port`   TCP port (default `9100`)
- `--follow` (standby) reconnect + resume after the primary dies
- `--reset`  (primary) wipe the store on start; otherwise the primary **persists**
  across restarts (keeping its data, epoch and LSN so replicas can resume)

## Security

Replication has two independent layers:

- **Authentication** (`--secret S`, or the `BKNR_REPL_SECRET` / `BKNR_REPL_SECRET_FILE`
  env vars). A shared secret gates who may pull the store and which primary a
  replica will trust, via a nonce-based HMAC-SHA256 challenge-response — the
  secret is never sent on the wire. Each side enforces the other's proof; a
  secret-configured server rejects unauthenticated clients, and a secret-
  configured client refuses an unauthenticated/spoofed primary. With no secret,
  authentication is skipped (local/demo only — the server warns).

- **Confidentiality (TLS).** Auth authenticates the peers but the stream is
  otherwise cleartext. For an untrusted network, run over TLS:
  ```sh
  bknr-repl primary --tls-cert cert.pem --tls-key key.pem --secret S ...
  bknr-repl standby --tls --secret S ...
  ```
  TLS runs *under* the auth handshake (provided by the optional
  `bknr.datastore.replication/tls` system, cl+ssl). Alternatively, terminate TLS
  outside the process with stunnel / an SSH or WireGuard tunnel and bind the
  primary to loopback.

> The base transfer streams the full store; only run replication over an
> authenticated and (on untrusted networks) encrypted channel.

## Automated test

`bin/smoke-test.sh` starts a standby and a primary, commits two messages, and
verifies the standby received both over the network:

```sh
bin/smoke-test.sh            # uses bknr-repl on PATH
BKNR_REPL=./bin/bknr-repl bin/smoke-test.sh
```

## How it works

- The **primary** runs `start-replication-server`, which registers a
  commit-observer that fans each committed transaction's encoded bytes out to
  every connected replica. A newly connected replica is first sent the full
  transaction log (catch-up), then live transactions, atomically under the
  store's log guard.
- The **standby** calls `run-replica`, which pumps the socket into
  `apply-replication-stream`: it decodes each transaction and applies it via the
  restore machinery, advancing the LSN and firing **apply observers**. The demo
  registers an apply observer that prints each change — the same hook you would
  use to push server-sent events, maintain derived views, or feed external
  systems.

See `doc/replication-design.md` for the full design and rationale.
