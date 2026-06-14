# MCP Draft 2026-07-28 Implementation Status

This document tracks the ABAP MCP SDK v2 path for MCP draft `2026-07-28`.

Status: **complete for the currently feasible ABAP target**. The live implementation supports the draft features that can realistically be implemented on ABAP ICF without request-scoped SSE, protocol sessions, or application-owned signing/key management. Remaining items are watch items for future draft movement, not known implementation gaps in the current target.

## Implemented

- Dual-era protocol support:
  - Legacy `2025-03-26`, `2025-06-18`, and `2025-11-25` continue through `ZIF_MCP_SERVER` / `ZCL_MCP_SERVER_BASE`.
  - Modern `2026-07-28` uses `ZIF_MCP_SERVER_V2` / `ZCL_MCP_SERVER_BASE_V2`.
- Modern request handling:
  - `ZCL_MCP_MODERN_CONTEXT` parses required `_meta`, headers, client capabilities, extensions, log level, and OpenTelemetry metadata into the v2 context.
  - Modern `_meta` validates `clientInfo`, `clientCapabilities`, and `clientCapabilities.extensions` object shapes.
  - `Mcp-Protocol-Version`, `Mcp-Method`, and `Mcp-Name` are required and compared with the JSON-RPC body where applicable.
  - Context errors use structured exception markers instead of substring matching for protocol/header classification.
  - Modern notifications are validated before returning HTTP `202`.
  - Client-sent JSON-RPC response bodies are rejected on the modern path.
- Modern routing:
  - `server/discover`
  - `tools/list`, `tools/call`
  - `prompts/list`, `prompts/get`
  - `resources/list`, `resources/templates/list`, `resources/read`
  - `completion/complete`
  - Tasks extension methods `tasks/get`, `tasks/update`, and `tasks/cancel`
- Modern result helpers:
  - `ZCL_MCP_RESP_SERVER_DISC`
  - `ZCL_MCP_RESP_V2_COMPLETE`
  - `ZCL_MCP_RESP_V2_INPUT_REQ`
  - `ZCL_MCP_RESP_V2_TASK`
  - `ZCL_MCP_RESP_V2_TASK_GET`
  - `ZCL_MCP_RESP_V2_TOOL`
  - `ZCL_MCP_RESP_V2_ACK`
  - Explicit `ttlMs = 0` cache emission is supported where cache was explicitly set.
  - `ZCL_MCP_RESP_V2_TOOL` emits `resultType = "complete"` only when explicitly requested.
- MRTR and elicitation:
  - `tools/call`, `prompts/get`, and `resources/read` can return and resume `input_required`.
  - Typed retry parsing is implemented.
  - Protected `requestState` helpers are available and used by the v2 test server and persisted task flow.
  - `requestState` validates token shape, payload version, scope, expiry, signature, and nonce replay centrally.
- Tasks extension:
  - Task creation returns `resultType = "task"`.
  - SDK-level routing rejects task results unless the client declared `io.modelcontextprotocol/tasks`.
  - `tasks/get` returns `resultType = "complete"` with working, completed, failed, cancelled, or input-required task state.
  - `tasks/update` accepts `requestState` and `inputResponses`.
  - Persisted tasks can pause for input, resume through `tasks/update`, reject tampered state, and cancel.
- `x-mcp-header`:
  - Tool schemas can mark top-level string, integer, and boolean arguments with string `x-mcp-header` suffixes.
  - `tools/call` parameter-header validation uses `get_tool_input_schema`; the base class keeps a compatible `tools/list` fallback, and servers can override direct schema lookup.
  - Missing and mismatched `Mcp-Param-*` headers are rejected with `HeaderMismatch` `-32001`.
  - Duplicate suffixes, invalid suffix tokens, `number` properties, and nested annotations are rejected.
  - Base64 encoded header values, unsafe/control-character values, numeric integer comparison, JavaScript safe-integer bounds, and strict boolean text handling are implemented.
- HTTP behavior:
  - V2-only `GET` and `DELETE` return `405` with `Allow: POST, OPTIONS`.
  - V2-only `OPTIONS` advertises `POST, OPTIONS`.
  - Preflight `Access-Control-Allow-Headers` uses an explicit modern allow-list plus requested `Mcp-Param-*` headers.
  - Legacy/full endpoints still advertise legacy CORS headers such as `Mcp-Session-Id`.
  - Non-OPTIONS requests enforce configured Origin policy for modern endpoints.
  - Origin allow-list lookup supports exact area/server entries plus `(area,*)`, `(*,server)`, and `(*,*)` fallbacks.
  - Unknown modern methods return HTTP `404` with JSON-RPC `-32601`.
  - Unsupported modern protocol versions return JSON-RPC `-32004` with `error.data.supported/requested`.
  - Missing Tasks extension capability returns JSON-RPC `-32003` with `error.data.requiredCapabilities`.
- Stateless legacy compatibility:
  - `ZCL_MCP_LEGACY_V2_ADAPTER` exists as the first stateless adapter for legacy SDK clients calling v2-only servers.
  - `ZCL_MCP_HTTP_HANDLER` routes non-modern legacy requests to the adapter when no legacy server implementation is bound but a v2 server is available.
  - The adapter handles legacy `initialize`, `ping`, tools, prompts, resources, resource templates, completions, and task translation paths.
  - Legacy clients may call supported adapter methods regardless of the negotiated legacy protocol version. This mirrors common legacy MCP client behavior and avoids relying on old capability/protocol gating that was often incomplete.
  - Task-returning calls are translated into the v2 `io.modelcontextprotocol/tasks` extension capability centrally.
  - V2 task create/get/cancel results are mapped into legacy task result shapes; `tasks/result` exposes terminal payloads or terminal errors.
  - MRTR, elicitation, v2 input-required task states, and `tasks/update` fail clearly because legacy clients cannot complete those v2 continuation flows.
  - No protocol sessions or `Mcp-Session-Id` are created for this compatibility path.
- Demo and SDK documentation:
  - `ZCL_MCP_DEMO_SERVER_V2_BASIC` demonstrates normal v2 server implementation.
  - `ZCL_MCP_DEMO_SERVER_V2_WF` demonstrates MRTR, elicitation, protected `requestState`, task input, and `tasks/update`.
  - Configured demo endpoints are `demo/demo_v2_basic` and `demo/demo_v2_workflow`.
  - V2 user documentation covers implementation, HTTP/header behavior, MRTR, tasks, legacy compatibility, and demo servers.

## Current Test Coverage

ABAP Unit covers core helpers, request-state hardening, base class behavior, and selected modern router/context behavior.

The external raw HTTP suite at `~/mcp_tests/test/src/test/v2_raw_http.test.ts` currently covers 57 v2 scenarios, including:

- discovery, required metadata and metadata shape validation, protocol/header mismatch, unsupported protocol, and error data
- v2-only HTTP methods, CORS preflight, notifications, client response rejection, and unknown methods
- tools, prompts, resources, resource templates, completions, cache hints, and result metadata
- `Mcp-Method`, `Mcp-Name`, and `Mcp-Param-*` validation
- `x-mcp-header` schema rejection cases and value hardening
- MRTR/elicitation retry flows and request-state tamper/replay rejection
- Tasks extension capability checks, SDK-level task result guarding, task creation, polling, input-required state, terminal errors, `tasks/update`, and `tasks/cancel`

Focused test command:

```bash
npm test -- --runInBand src/test/v2_raw_http.test.ts
```

The external SDK compatibility suite at `~/mcp_tests/test/src/test/v2_legacy_compat.test.ts` covers legacy SDK clients calling a v2-only server. It currently verifies:

- SDK initialization for `2025-11-25`, `2025-06-18`, and `2025-03-26`
- stateless operation without `Mcp-Session-Id`
- legacy SDK calls for `ping`, tools, prompts, resources, resource templates, completions, and tasks
- task creation, task polling, task result, task listing, and task cancellation across all supported legacy protocol versions
- legacy stripping/preserving behavior for v2 cache hints and result metadata
- resource-not-found, unknown method/tool, unsupported subscription/logging/sampling/roots, and draft-only method failures
- explicit legacy failures for v2-only MRTR, elicitation/input-required flows, and `tasks/update`

Focused test command:

```bash
npm test -- --runInBand src/test/v2_legacy_compat.test.ts
```

Demo smoke checks were also run against the configured ABAP endpoints at `192.168.56.101`:

- `demo/demo_v2_basic`: `server/discover`, `tools/list`, `echo`, `cache_meta`
- `demo/demo_v2_workflow`: `server/discover`, `tools/list`, direct MRTR, task creation, `tasks/get`, `tasks/update`, completed `tasks/get`, and missing tasks capability error

## Watch Items

These are not current implementation gaps, but should be reviewed if the draft changes or before a release:

- Re-check the draft source of truth because draft protocol, schema, MRTR, discovery, Streamable HTTP, and Tasks can still move.
- Revisit MRTR/elicitation only if the draft adds a required client capability or stricter negotiation model. It is currently application-owned and documented.
- Modern `ping` is not part of the currently checked draft schema. Old SDK clients calling v2-only servers are covered by the stateless legacy adapter.
- Revisit Origin/CORS behavior if the draft tightens browser-origin requirements beyond the current configurable ABAP model.
- Revisit roots/sampling/logging only if a concrete compatibility or v2 requirement appears.
- Revisit MCP Apps after core v2 stabilizes.

## Deferred or Out of Scope

- Request-scoped SSE responses
- Long-lived `subscriptions/listen`
- Server-initiated JSON-RPC requests outside MRTR
- Full legacy protocol sessions on v2-only servers
- Deprecated roots/sampling/logging helper expansion
- MCP Apps extension until core v2 stabilizes and a concrete ABAP use case exists
- Automatic wrapping of legacy list/read helpers for v2 output. New v2 servers should return the modern result shape directly.
- External key-provider or key-rotation hooks for `requestState`; ABAP server implementations use the central integrity, expiry, scope binding, and replay protection helpers.

## Editing Note

The live implementation is in the ABAP system. Local ABAP files in this repository can be outdated; use the ABAP ADT MCP server to inspect current objects before planning or verifying changes.
