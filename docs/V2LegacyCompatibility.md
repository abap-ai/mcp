# V2 Legacy Compatibility

The ABAP SDK includes a stateless compatibility adapter for old MCP SDK clients calling v2-only servers.

The adapter exists to keep practical old clients working where the old protocol can represent the result. It is not a full legacy-session emulation layer.

## Routing Model

If a configured endpoint has no legacy server instance but does have a v2 server instance, non-modern legacy requests are routed through `ZCL_MCP_LEGACY_V2_ADAPTER`.

Modern requests still use the v2 path:

- `server/discover`
- required modern `_meta`
- required modern HTTP headers
- stateless v2 context

Legacy requests use compatibility behavior:

- `initialize`
- old SDK method names and result shapes
- no protocol session
- no `Mcp-Session-Id`

If a class implements both legacy and v2 APIs, legacy requests should use the real legacy implementation and modern requests should use the v2 implementation.

## Supported Legacy Protocol Versions

The adapter accepts:

- `2025-03-26`
- `2025-06-18`
- `2025-11-25`

Supported adapter methods are not blocked based on the negotiated legacy version. This is deliberate. Real legacy clients often call whatever their SDK supports, and older capability negotiation did not reliably describe every feature.

Unsupported legacy protocol versions fail during `initialize`.

## Supported Methods

| Legacy method | Compatibility behavior |
| ------------- | ---------------------- |
| `initialize` | returns legacy initialize shape from v2 discovery metadata |
| `ping` | returns empty success result |
| `tools/list` | calls v2 `tools/list` and strips v2-only envelope/cache fields |
| `tools/call` | calls v2 `tools/call` and down-converts complete results |
| `prompts/list` | calls v2 `prompts/list` |
| `prompts/get` | calls v2 `prompts/get` for complete results |
| `resources/list` | calls v2 `resources/list` |
| `resources/templates/list` | calls v2 `resources/templates/list` |
| `resources/read` | calls v2 `resources/read` for complete results |
| `completion/complete` | calls v2 `completion/complete` |
| `tasks/list` | returns persisted task list in legacy shape |
| `tasks/get` | maps v2 task state to legacy task shape |
| `tasks/result` | returns terminal task payload or terminal JSON-RPC error |
| `tasks/cancel` | maps cancellation result where possible |

## Unsupported Methods

| Method or feature | Reason |
| ----------------- | ------ |
| `server/discover` | modern-only discovery |
| `tools/get_input_schema` | modern/internal validation support, not a legacy client method |
| `tasks/update` | legacy clients cannot complete v2 task input continuations |
| `subscriptions/listen` | streaming/subscription model is not implemented in ABAP |
| `logging/setLevel` | deprecated and not added by the v2 adapter |
| `sampling/createMessage` | server-to-client request model is replaced by MRTR |
| `roots/list` | deprecated client feature and not a compatibility target |

## Result Translation

The adapter strips fields old clients do not understand:

- `resultType`
- `ttlMs`
- `cacheScope`

It preserves result data old clients can consume, including tool content, structured content, prompt messages, resources, templates, completion values, and selected `_meta`.

Task translation:

- v2 task creation becomes the legacy create-task result shape
- v2 `tasks/get` and `tasks/cancel` become root task shapes
- v2 `tasks/result` exposes terminal payloads
- failed terminal tasks become JSON-RPC errors

## Modern Features That Cannot Be Down-Converted

These fail clearly for old clients:

| V2 feature | Legacy behavior |
| ---------- | --------------- |
| MRTR `input_required` | JSON-RPC error |
| elicitation | JSON-RPC error via input-required result |
| input-required task state | JSON-RPC error |
| `tasks/update` | method not found |
| request-scoped SSE | not implemented |
| subscriptions | not implemented |
| MCP Apps | not implemented |

If old clients need a workflow, expose a non-interactive tool or prompt variant instead of relying on MRTR.

## Tasks Capability in the Adapter

The v2 router normally requires clients to advertise `io.modelcontextprotocol/tasks` before it returns `resultType = "task"`.

Legacy clients cannot advertise v2 extension capabilities. The adapter therefore declares the tasks extension centrally in its compatibility context. This lets task-returning v2 handlers work for old clients and keeps the compatibility decision in one place.

## Ping

Modern draft `2026-07-28` does not currently expose `ping` as a modern method in the checked draft schema. Old clients may still call `ping`, so the adapter answers it directly.

Do not add a modern v2 `ping` handler unless the draft changes or a concrete client requirement appears.

## Sessions

The compatibility adapter is stateless:

- it does not create an MCP session
- it does not require `Mcp-Session-Id`
- it does not return `Mcp-Session-Id`
- it does not emulate legacy HTTP+SSE session behavior

State that must survive across requests must be represented by explicit task IDs, resource URIs, business keys, or v2 `requestState` for modern clients.

## Testing

The external SDK compatibility suite verifies the adapter with old SDK clients:

```bash
npm test -- --runInBand src/test/v2_legacy_compat.test.ts
```

It covers:

- stateless initialization for all supported legacy protocol versions
- `ping`
- tools, prompts, resources, templates, completions
- task create/get/result/list/cancel
- cache-hint stripping and metadata behavior
- unsupported protocol versions
- draft-only method failures
- unsupported subscription/logging/sampling/roots methods
- explicit failures for MRTR, elicitation/input-required flows, and `tasks/update`
