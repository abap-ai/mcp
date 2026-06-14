# V2 HTTP, Headers, and Security

This page describes the HTTP behavior and validation rules for the ABAP MCP SDK v2 path.

## Streamable HTTP Scope

The v2 route uses Streamable HTTP without protocol sessions.

Supported:

- `POST` with one JSON-RPC request or notification
- `OPTIONS` for CORS preflight
- JSON responses with `Content-Type: application/json`
- notifications returning HTTP `202 Accepted` with no response body
- HTTP `404` plus JSON-RPC `-32601` for unknown modern methods
- HTTP `400` plus modern JSON-RPC errors for protocol/header problems

Not supported:

- long-lived GET streams
- request-scoped SSE response streams
- DELETE session termination for v2
- `Mcp-Session-Id` on v2 responses
- JSON-RPC batches

ABAP ICF is not a good fit for the draft streaming model, so request-scoped SSE and `subscriptions/listen` are out of scope.

## Required Modern Metadata

Every modern request must carry these fields in `params._meta`:

- `io.modelcontextprotocol/protocolVersion`
- `io.modelcontextprotocol/clientInfo`
- `io.modelcontextprotocol/clientCapabilities`

`clientInfo` must be an object with non-empty `name` and `version`. `clientCapabilities` must be an object. If `clientCapabilities.extensions` is present, it must be an object.

The SDK also parses optional:

- `io.modelcontextprotocol/logLevel`
- `traceparent`
- `tracestate`
- `baggage`

These values are available through `get_v2_context( )`.

## Mirrored Headers

The Streamable HTTP binding mirrors selected body fields into HTTP headers. The body remains the source of truth. Missing or mismatched required headers return HTTP `400` with JSON-RPC `HeaderMismatch` (`-32001`).

| Header | Body source | Required for |
| ------ | ----------- | ------------ |
| `Mcp-Protocol-Version` | `_meta["io.modelcontextprotocol/protocolVersion"]` | all modern HTTP requests and notifications |
| `Mcp-Method` | JSON-RPC `method` | all modern HTTP requests and notifications |
| `Mcp-Name` | `params.name` | `tools/call`, `prompts/get` |
| `Mcp-Name` | `params.uri` | `resources/read` |
| `Mcp-Param-{Name}` | `params.arguments.{property}` | marked tool arguments |

Header values such as method and name are case-sensitive. Header names are case-insensitive at the HTTP layer.

## `x-mcp-header` Tool Parameters

Tool schemas can mark selected primitive arguments for HTTP header mirroring by setting `x-mcp-header`.

`ZCL_MCP_DEMO_SERVER_V2_BASIC` exposes the `echo` argument through `Mcp-Param-Message`:

```abap
DATA(builder) = NEW zcl_mcp_schema_builder( ).

builder->add_string( name         = `message`
                     description  = `Message to return from the demo server.`
                     required     = abap_false
                     x_mcp_header = `Message` ).
```

The workflow demo uses the same mechanism for `approval_required`:

```abap
builder->add_string(
  name         = `reason`
  description  = `Reason shown in the demo workflow.`
  required     = abap_false
  x_mcp_header = `Reason` ).
```

These expect headers such as:

- `Mcp-Param-Message` for `echo`
- `Mcp-Param-Reason` for `approval_required`

The current ABAP implementation intentionally supports only top-level string, integer, and boolean properties. Nested `x-mcp-header` annotations are rejected. This keeps the server-side implementation practical and predictable in ABAP.

Rejected schema shapes:

- duplicate suffixes, case-insensitively
- invalid HTTP field-name suffixes
- `number` properties
- nested annotations

Rejected values:

- missing headers when the marked argument is present
- supplied headers when the marked argument is absent
- mismatched header/body values
- invalid Base64 encoded values
- decoded control characters
- integers outside JavaScript safe-integer bounds
- boolean values other than lowercase `true` or `false`

Integer comparison is numeric, so `000123` matches body value `123`.

Do not mirror secrets, credentials, tokens, or sensitive business data into headers.

## Direct Schema Lookup

The router validates parameter headers before `handle_tools_call`. It obtains the tool schema through `get_tool_input_schema`.

For performance, override `handle_tool_input_schema`:

```abap
METHOD handle_tool_input_schema.
  DATA builder TYPE REF TO zcl_mcp_schema_builder.

  builder = NEW zcl_mcp_schema_builder( ).

  CASE tool_name.
    WHEN c_tool_echo.
      builder->add_string( name         = `message`
                           description  = `Message to return from the demo server.`
                           required     = abap_false
                           x_mcp_header = `Message` ).
  ENDCASE.

  result = builder->to_json( ).
ENDMETHOD.
```

The base class fallback calls `tools/list` and extracts the selected tool schema.

## Origin Enforcement

Modern non-OPTIONS requests validate `Origin` against the configured allow-list unless the configuration explicitly ignores Origin validation.

Allowed origins are maintained per area/server and support fallback entries:

- exact `(area, server)`
- `(area, *)`
- `(*, server)`
- `(*, *)`

If an `Origin` header is present and not allowed, the SDK returns HTTP `403 Forbidden`.

For local development, add explicit allowed origins for your test clients instead of disabling validation globally.

## CORS Preflight

V2-only endpoints answer `OPTIONS` with:

- `Allow: POST, OPTIONS`
- `Access-Control-Allow-Methods: POST, OPTIONS`
- explicit modern request headers
- requested `Mcp-Param-*` headers when present in `Access-Control-Request-Headers`

Legacy/full endpoints still advertise legacy headers such as `Mcp-Session-Id`.

## RequestState Security

`requestState` is used by MRTR and task input flows. Server implementations should use the base-class helpers:

```abap
DATA(state) = create_request_state( data        = `my-continuation`
                                    ttl_seconds = 300 ).

DATA(validated) = validate_request_state( request_state = request->get_request_state( ) ).
```

The SDK centrally validates:

- token shape
- payload version
- area/server/method scope
- user scope
- expiry
- signature
- nonce replay after successful validation

`requestState` is integrity-protected but not encrypted. Put only non-secret continuation data into the payload.

External key-provider and key-rotation hooks are not part of the ABAP target. The practical security model is central signing, short TTLs, scope binding, and replay rejection.

## Error Codes

Modern HTTP/protocol-specific errors:

| Code | Meaning |
| ---- | ------- |
| `-32001` | Header mismatch or required mirrored header missing |
| `-32003` | Required client capability missing |
| `-32004` | Unsupported protocol version |

Generic JSON-RPC errors still apply:

| Code | Meaning |
| ---- | ------- |
| `-32700` | Parse error |
| `-32600` | Invalid request |
| `-32601` | Method not found |
| `-32602` | Invalid params |
| `-32603` | Internal error |
