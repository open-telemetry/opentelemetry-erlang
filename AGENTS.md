# AGENTS.md

Guidance for AI coding agents working in this repository.

## What this is

OpenTelemetry SDK/API implementation for Erlang and Elixir. It's a multi-app
rebar3 project (not a single OTP app) — each directory under `apps/` is an
independent, separately-versioned, separately-released Hex/rebar package:

- `opentelemetry` — the SDK
- `opentelemetry_api` — the API, the main entry point for Elixir users
- `opentelemetry_api_experimental` / `opentelemetry_experimental` — experimental
  API/SDK surface (metrics, logs) not yet stable
- `opentelemetry_exporter` — OTLP exporter
- `opentelemetry_zipkin` — Zipkin exporter
- `opentelemetry_semantic_conventions` — generated semantic convention constants

`opentelemetry_api`, `opentelemetry_api_experimental`, and
`opentelemetry_semantic_conventions` are dual-published to Hex for both
rebar3 and mix, so each of those three also has its own `mix.exs`.

Changes are usually scoped to one `apps/*` directory. Check which app(s) a
change touches before assuming a repo-wide build/test is needed.

The OpenTelemety Specification is hosted at https://github.com/open-telemetry/opentelemetry-specification.

The currently supported specification version is `1.8.0`.


## Build

```
rebar3 compile
```

Elixir-side deps (only needed for `opentelemetry_api` mix tests):

```
cd apps/opentelemetry_api && mix deps.get
```

## Tests

The top-level `rebar.config`/`mix.exs` exist purely to run cross-app
integration tests — they are not where per-app test suites live. Each
`apps/<app>` directory has its own `rebar.config` and is its own rebar3
project; run Erlang test suites from inside that directory, not from the
repo root.

- Erlang tests (per app, Common Test): `cd apps/<app> && rebar3 ct`
  - Logs: `apps/<app>/_build/test/logs/index.html`
- Elixir tests for `opentelemetry_api`: `cd apps/opentelemetry_api && mix test`
- Top-level Elixir tests (repo-root integration tests, exercise the SDK from
  Elixir): `mix test --no-start test/otel_tests.exs test/otel_metric_tests.exs`
  (requires `rebar3 as test compile` from the repo root first)

Integration tests assume backing services are up: `docker compose up -d`
(starts a collector used by exporter tests).

## Lint / static analysis

Run these scoped to the app you changed, from inside `apps/<app>`:

- `rebar3 xref`
- `rebar3 as dialyzer dialyzer` (Erlang)
- eqWAlizer (optional, requires the `elp` binary): `elp eqwalize-all`

For `opentelemetry_api` (Elixir):

- `mix dialyzer`
- `mix format --check-formatted` (run `mix format` without
  `--check-formatted` to fix)

CI runs these per-app on OTP 26/27/28 and Elixir 1.14/1.18 matrices — see
`.github/workflows/erlang.yml` and `.github/workflows/elixir.yml`.

The only officially supported OTP and Elixir versions are those listed in the 
CI matrices. The lowest version of each **must** be supported; never use new language
features which are unsupported in those versions.

## Commit / PR conventions

- PR titles **must** follow Conventional Commits using only these types:
  `feat`, `fix`, `chore` (enforced by CI title-lint from `.commit-me.json`).
  Titles with other types (e.g. `docs`, `refactor`, `test`) fail CI.
- Changelogs per app are generated via `git cliff` (`justfile`, `cliff.toml`)
  — don't hand-edit `apps/*/CHANGELOG.md`.

### Commit formatting

We appreciate it if users disclose the use of AI tools when the significant part of a commit is
taken from a tool. When making a commit this should be disclosed through an
Assisted-by: commit message trailer.

Examples:

Assisted-by: ChatGPT 5.2
Assisted-by: Claude Opus 4.5

## Notes for agents

- This is a mono-repo of independently versioned, independently published
  packages: a PR should generally touch a single `apps/*` directory.
- Each app's `rebar.config` declares sibling deps (e.g. `opentelemetry_api`)
  by Hex version constraint, not local path. Testing an app from inside its
  own directory therefore resolves those siblings from Hex (the published
  version), not from your working tree — this is intentional, since it's
  what real downstream users depend on. Only override a sibling dep to the
  local filesystem path if a change genuinely spans apps and you need to
  verify them together. If you do this, be sure to revert that override
  before committing.
- `opentelemetry_experimental` / `opentelemetry_api_experimental` are unstable
  by design (metrics/logs); don't "stabilize" APIs there unless asked.
- `opentelemetry_semantic_conventions` is generated code — check
  `apps/opentelemetry_semantic_conventions/makefile` before hand-editing
  generated modules. It uses `weaver` to codegen so any change you may want to make
  is likely a change needed to the templates, instead. 
- Follow the existing formatting/style in the file you're editing; don't
  run repo-wide reformatting as a side effect of an unrelated change.
