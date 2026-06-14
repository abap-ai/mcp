# MCP Draft 2026-07-28 Spec Delta

This document summarizes the draft protocol changes that matter for the ABAP MCP SDK. It is intentionally short and reflects the implementation that now exists in the live ABAP system.

Status: the SDK support for MCP draft `2026-07-28` is considered complete for the currently feasible ABAP target. The explicit exclusions are ABAP/ICF constraints or deliberately deferred draft areas, not known gaps in the implemented v2 path.

Sources to re-check before a release:

- Draft specification: https://modelcontextprotocol.io/specification/draft
- Draft schema: https://modelcontextprotocol.io/specification/draft/schema
- Streamable HTTP: https://modelcontextprotocol.io/specification/draft/basic/transports/streamable-http
- Discovery: https://modelcontextprotocol.io/specification/draft/server/discover
- MRTR: https://modelcontextprotocol.io/specification/draft/basic/patterns/mrtr
- Tasks extension: https://modelcontextprotocol.io/extensions/tasks/overview

## Era Model

The SDK now has two protocol eras:

| Era | Versions | ABAP path |
| --- | -------- | --------- |
| Legacy | `2025-03-26`, `2025-06-18`, `2025-11-25` | `ZIF_MCP_SERVER` / `ZCL_MCP_SERVER_BASE` with `initialize` and optional sessions |
| Modern | `2026-07-28` | `ZIF_MCP_SERVER_V2` / `ZCL_MCP_SERVER_BASE_V2` with stateless per-request metadata |

Modern requests do not use protocol sessions and must not create or echo `Mcp-Session-Id`.

## Modern Request Metadata

Every modern request carries protocol metadata in `params._meta`:

- `io.modelcontextprotocol/protocolVersion`
- `io.modelcontextprotocol/clientInfo`
- `io.modelcontextprotocol/clientCapabilities`

The SDK also parses optional:

- `io.modelcontextprotocol/logLevel`
- `traceparent`
- `tracestate`
- `baggage`

OpenTelemetry and per-request log level are exposed in the v2 context only. The SDK does not attempt runtime OpenTelemetry integration or dynamic ABAP log-level control.

The implementation validates `clientInfo` as an object with non-empty `name` and `version`, `clientCapabilities` as an object, and `clientCapabilities.extensions` as an object when present.

## Header Mirroring

Modern Streamable HTTP mirrors selected body fields into headers:

| Header | Body source |
| ------ | ----------- |
| `Mcp-Protocol-Version` | `_meta["io.modelcontextprotocol/protocolVersion"]` |
| `Mcp-Method` | JSON-RPC `method` |
| `Mcp-Name` | `params.name` for `tools/call` and `prompts/get`; `params.uri` for `resources/read` |
| `Mcp-Param-{Name}` | top-level tool arguments marked with string `x-mcp-header = "{Name}"` |

The body remains the source of truth. Missing or mismatched required mirrored headers return HTTP `400` with JSON-RPC `HeaderMismatch` `-32001`.

Implemented `Mcp-Param-*` behavior:

- string, integer, and boolean top-level properties are supported
- tool input schemas are loaded through `get_tool_input_schema`; `ZCL_MCP_SERVER_BASE_V2` keeps a compatible `tools/list` fallback
- `number` properties are rejected
- duplicate suffixes are rejected case-insensitively
- invalid HTTP field-name suffixes are rejected
- nested annotations are rejected until nested support is deliberately added
- `=?base64?...?=` values are decoded before comparison
- invalid encoded values and unsafe/control-character values are rejected
- integers are compared numerically and must be within JavaScript safe-integer bounds
- boolean headers must be lowercase `true` or `false`

## Streamable HTTP

Modern v2 uses JSON-only Streamable HTTP in this SDK:

- `POST` accepts one JSON-RPC request or notification.
- Notifications return HTTP `202` with no body after metadata/header validation.
- Client-sent JSON-RPC response bodies are invalid.
- JSON-RPC batches remain unsupported.
- Unknown modern methods return HTTP `404` with JSON-RPC `-32601`.
- Unsupported modern protocol versions return HTTP `400` with JSON-RPC `-32004` and `error.data.supported/requested`.
- V2-only `GET` and `DELETE` return HTTP `405` with `Allow: POST, OPTIONS`.
- V2-only `OPTIONS` advertises `POST, OPTIONS` and an explicit modern header allow-list.
- Modern non-OPTIONS requests validate `Origin` against configured allowed origins.
- Origin allow-list matching supports exact area/server entries plus wildcard fallback entries.

Deferred transport features:

- request-scoped SSE responses
- long-lived `subscriptions/listen`
- stream-close cancellation

## Discovery

Modern servers implement `server/discover` instead of requiring `initialize` for server metadata.

`ZCL_MCP_RESP_SERVER_DISC` emits:

- `resultType = "complete"`
- `supportedVersions`
- `capabilities`
- `serverInfo`
- optional `instructions`
- cache hints such as `ttlMs = 0` and `cacheScope = "private"`

Resource templates are exposed through `resources/templates/list`. Discovery advertises `resources`; it does not emit a separate resource-template capability.

## Results and Caching

Modern successful results are polymorphic:

| `resultType` | Meaning |
| ------------ | ------- |
| `complete` | normal successful result |
| `input_required` | MRTR result asking the client for more input |
| `task` | Tasks extension result returning a durable task handle |

Implemented v2 result helpers:

- `ZCL_MCP_RESP_V2_COMPLETE`
- `ZCL_MCP_RESP_V2_INPUT_REQ`
- `ZCL_MCP_RESP_V2_TASK`
- `ZCL_MCP_RESP_V2_TASK_GET`
- `ZCL_MCP_RESP_V2_TOOL`
- `ZCL_MCP_RESP_V2_ACK`

Cache behavior:

- `ttlMs` is valid at `0` and is emitted when cache was explicitly set.
- default conservative scope is `private`.
- `ZCL_MCP_RESP_V2_TOOL` emits `resultType = "complete"` only when `set_complete( )` was called.

V2 handlers should return the modern result shape directly. The SDK does not try to transparently convert every legacy helper result into v2 output.

## MRTR and Elicitation

MRTR replaces independent server-originated JSON-RPC requests. The server can return `resultType = "input_required"` from:

- `tools/call`
- `prompts/get`
- `resources/read`

The result can contain:

- `inputRequests`
- `requestState`

Elicitation is represented as an embedded input request. Retry parsing is implemented for tool, prompt, and resource flows. Application state that affects behavior should use the protected `create_request_state` / `validate_request_state` helpers.

`requestState` is integrity-protected but not encrypted. Validation checks token shape, payload version, area/server/method/user scope, expiry, signature, and nonce replay. Application code must not put secrets in the state payload.

The current ABAP security model intentionally keeps request-state protection central and simple: integrity, expiry, area/server/method/user scoping, and replay protection. External key-provider or key-rotation hooks are not part of the target.

## Tasks Extension

The implementation follows the Tasks extension shape:

- task creation returns `resultType = "task"`
- clients must declare `io.modelcontextprotocol/tasks` before receiving task results
- the router enforces the tasks capability for task results, even when application handlers omit their own check
- `tasks/get` returns `resultType = "complete"` with task state, terminal result/error, or pending input
- `tasks/update` resumes input-required tasks with `requestState` and `inputResponses`
- replayed `requestState` values are rejected
- `tasks/cancel` returns a complete acknowledgement

Persisted tasks can pause for input, expose pending input through `tasks/get`, resume via `tasks/update`, reject tampered state, and cancel.

Legacy task behavior remains on the legacy path for real legacy servers. V2-only servers can also serve stateless legacy SDK clients through the compatibility adapter described below.

## Legacy Compatibility Adapter

`ZCL_MCP_LEGACY_V2_ADAPTER` is the first stateless compatibility path for legacy SDK clients that call a v2-only server. It is intentionally limited:

- legacy clients still use `initialize`
- no compatibility request creates, requires, persists, or returns `Mcp-Session-Id`
- ordinary legacy tools, prompts, resources, resource templates, completions, `ping`, and task methods are mapped to v2 handlers where possible
- supported adapter methods are not gated by the negotiated legacy protocol version, because legacy MCP clients commonly call methods based on SDK support rather than precise server capability negotiation
- task-returning calls are adapted by centrally declaring the v2 `io.modelcontextprotocol/tasks` extension in the compatibility context
- v2 task creation maps to legacy create-task result shape; v2 `tasks/get` and `tasks/cancel` map to root task result shapes; `tasks/result` returns terminal payloads or terminal JSON-RPC errors
- MRTR/input-required results, elicitation, input-required task states, and `tasks/update` are not represented as successful legacy results; they fail clearly
- if a server implements both legacy and v2 APIs, the real legacy implementation remains authoritative for legacy requests
- unsupported legacy protocol versions are rejected during `initialize`

## Demo Servers

Two v2 demo servers are available and configured:

| Area | Service | Class | Purpose |
| ---- | ------- | ----- | ------- |
| `demo` | `demo_v2_basic` | `ZCL_MCP_DEMO_SERVER_V2_BASIC` | normal v2 tools, prompts, resources, templates, completions, cache hints, metadata, and `x-mcp-header` |
| `demo` | `demo_v2_workflow` | `ZCL_MCP_DEMO_SERVER_V2_WF` | MRTR, elicitation, protected `requestState`, persisted task input, and `tasks/update` |

The workflow endpoint keeps the readable service name `demo_v2_workflow`; the class uses `WF` because ABAP class names are limited to 30 characters.

## Error Codes

Modern-specific JSON-RPC errors:

| Code | Name | Use |
| ---- | ---- | --- |
| `-32001` | `HeaderMismatch` | mirrored HTTP headers are missing, malformed, or do not match body fields |
| `-32003` | `MissingRequiredClientCapabilityError` | required client capability is absent |
| `-32004` | `UnsupportedProtocolVersionError` | requested protocol version is unsupported |

Legacy JSON-RPC errors remain unchanged for legacy paths.

## Watch Items

- The draft can still move; re-check schema, Streamable HTTP, MRTR, discovery, and Tasks before release.
- Modern `ping` is not part of the currently checked draft schema. Legacy SDK clients calling v2-only servers are still covered by the compatibility adapter.
- Origin validation requirements may need stricter behavior than the current configurable CORS modes.
- Deprecated roots/sampling/logging should not grow new v2 helper surface unless a concrete compatibility requirement appears.
- MCP Apps is deferred until core v2 stabilizes.

## ABAP Non-Goals

These are intentionally not implemented for the current ABAP target:

- request-scoped SSE responses
- long-lived `subscriptions/listen`
- stream-close cancellation semantics that depend on persistent streaming
- full legacy protocol sessions on v2-only servers
- server-initiated JSON-RPC requests outside MRTR
- external key-provider or key-rotation hooks for `requestState`
- automatic conversion of arbitrary legacy helper output into v2 envelopes
