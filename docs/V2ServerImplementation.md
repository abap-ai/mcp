# V2 Server Implementation

Use this guide when writing a new MCP draft `2026-07-28` server in ABAP.

V2 servers should be written against the new stateless shape. Do not start by implementing a legacy server and expecting the SDK to convert every response. Legacy compatibility exists for old clients, but new server logic should return modern v2 results directly.

For copyable working examples, see:

- `ZCL_MCP_DEMO_SERVER_V2_BASIC` for normal server features
- `ZCL_MCP_DEMO_SERVER_V2_WF` for MRTR and task input workflows
- [V2 Demo Servers](V2DemoServers.md) for endpoint names and short excerpts

## Class Shape

Inherit from `ZCL_MCP_SERVER_BASE_V2` and redefine only the handlers your server supports.

```abap
CLASS zcl_my_server_v2 DEFINITION
  PUBLIC
  INHERITING FROM zcl_mcp_server_base_v2
  FINAL
  CREATE PUBLIC.

  PROTECTED SECTION.
    METHODS get_implementation       REDEFINITION.
    METHODS get_capabilities         REDEFINITION.
    METHODS get_instructions         REDEFINITION.
    METHODS handle_tools_list        REDEFINITION.
    METHODS handle_tool_input_schema REDEFINITION.
    METHODS handle_tools_call        REDEFINITION.
ENDCLASS.
```

The public interface is `ZIF_MCP_SERVER_V2`. Application code normally redefines the protected base-class methods instead of implementing the public interface directly.

## Discovery Metadata

Modern clients use `server/discover` instead of `initialize`. The base class implements `server/discover` from these methods:

```abap
METHOD get_implementation.
  result-name        = `My ABAP MCP Server`.
  result-version     = `1.0.0`.
  result-title       = `My ABAP Server`.
  result-description = `Exposes selected SAP functions through MCP v2.`.
  result-website_url = `https://example.invalid/mcp`.
ENDMETHOD.

METHOD get_capabilities.
  result-tools       = abap_true.
  result-prompts     = abap_true.
  result-resources   = abap_true.
  result-completions = abap_true.
  result-tasks       = abap_true.
ENDMETHOD.

METHOD get_instructions.
  result = `Use this server for read-only lookup and approved business actions.`.
ENDMETHOD.
```

Only advertise capabilities you actually implement. If a capability is advertised, clients may call the matching methods.

## Request Context

The SDK validates modern `_meta` and HTTP headers before dispatching to your handlers. Your handler can access the parsed context:

```abap
DATA(context) = me->zif_mcp_server_v2~get_v2_context( ).

" Useful fields:
" context-area
" context-server
" context-protocol_ver
" context-client_info
" context-client_caps
" context-extensions
" context-log_level
" context-traceparent
" context-tracestate
" context-baggage
" context-meta
```

Use the context for authorization-relevant client capability checks, extension checks, logging correlation, or per-request behavior. Do not infer v2 client identity or capabilities from previous requests.

## Tool List

`tools/list` can still use `ZCL_MCP_RESP_LIST_TOOLS`. For v2, make sure the returned JSON is wrapped with `resultType = "complete"` if the helper you use does not already emit it.

`ZCL_MCP_DEMO_SERVER_V2_BASIC` uses this small pattern: build the list through the existing helper, then add the v2 result fields in one place.

```abap
METHOD build_tools_result.
  DATA list_tools TYPE REF TO zcl_mcp_resp_list_tools.
  DATA tools      TYPE zcl_mcp_resp_list_tools=>tools.
  DATA tool       TYPE zcl_mcp_resp_list_tools=>tool.

  list_tools = NEW zcl_mcp_resp_list_tools( ).

  CLEAR tool.
  tool-name         = c_tool_echo.
  tool-title        = `Echo`.
  tool-description  = `Returns a text message. The message can also be mirrored through Mcp-Param-Message.`.
  tool-input_schema = build_tool_schema( c_tool_echo ).
  tool-annotations-readonlyhint = abap_true.
  APPEND tool TO tools.

  list_tools->set_tools( tools ).

  result = list_tools->zif_mcp_internal~generate_json( ).
  set_complete_fields( result ).
ENDMETHOD.
```

For production code, consider a small helper method that creates your tools list so `handle_tools_list` and `handle_tool_input_schema` cannot drift.

The helper that adds the modern fields is intentionally simple:

```abap
METHOD set_complete_fields.
  result_json->set_string( iv_path = `/resultType`
                           iv_val  = zif_mcp_constants=>result_types-complete ).
  result_json->set_integer( iv_path = `/ttlMs`
                            iv_val  = 0 ).
  result_json->set_string( iv_path = `/cacheScope`
                           iv_val  = zif_mcp_constants=>cache_scopes-private ).
ENDMETHOD.
```

## Direct Tool Schema Lookup

`handle_tool_input_schema` is used by the router to validate `Mcp-Param-*` headers before `tools/call`. Override it for efficient direct lookup.

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

    WHEN c_tool_server_time.
      " Empty object schema.

    WHEN c_tool_cache_meta.
      " Empty object schema.
  ENDCASE.

  result = builder->to_json( ).
ENDMETHOD.
```

The base class fallback derives the schema from `tools/list`, which is convenient but can be expensive.

## Tool Calls

Use `ZCL_MCP_RESP_V2_TOOL` for normal modern tool results.

```abap
METHOD handle_tools_call.
  TRY.
      CASE request->get_name( ).
        WHEN c_tool_echo.
          DATA(arguments) = request->get_arguments( ).
          DATA(message) = arguments->get_string( `/message` ).

          IF message IS INITIAL.
            message = `Hello from the ABAP MCP V2 basic demo.`.
          ENDIF.

          response-result = build_text_tool_result( |Echo: { message }| ).

        WHEN c_tool_server_time.
          response-result = build_text_tool_result( |ABAP server date { sy-datum }, time { sy-uzeit }.| ).

        WHEN OTHERS.
          response = method_not_found( request->get_name( ) ).
      ENDCASE.

    CATCH zcx_mcp_ajson_error INTO DATA(json_error).
      response-error-code    = zcl_mcp_jsonrpc=>error_codes-internal_error.
      response-error-message = json_error->get_text( ).
  ENDTRY.
ENDMETHOD.
```

Use `isError` for tool execution failures that are still valid tool results. Use JSON-RPC errors for invalid protocol parameters, unknown methods, missing capabilities, or infrastructure failures.

The demo's text result helper is the smallest normal tool result:

```abap
METHOD build_text_tool_result.
  DATA tool_result TYPE REF TO zcl_mcp_resp_v2_tool.

  tool_result = NEW zcl_mcp_resp_v2_tool( ).
  tool_result->set_complete( ).
  tool_result->set_error( abap_false ).
  tool_result->add_text_content( text ).

  result = tool_result->generate_json( ).
ENDMETHOD.
```

## Cache Hints and Metadata

Modern result helpers can emit cache hints and `_meta`.

```abap
DATA(tool_result) = NEW zcl_mcp_resp_v2_tool( ).
DATA(meta) = zcl_mcp_ajson=>create_empty( ).

meta->set_string( iv_path = `/abap.demo~1kind`
                  iv_val  = `basic-v2-demo` ).
meta->set_string( iv_path = `/abap.demo~1feature`
                  iv_val  = `cache-meta` ).

tool_result->set_complete( ).
tool_result->set_error( abap_false ).
tool_result->add_text_content( `This result demonstrates v2 cache hints and result metadata.` ).
tool_result->set_cache( ttl_ms      = 30000
                        cache_scope = zif_mcp_constants=>cache_scopes-private ).
tool_result->set_meta( meta ).

response-result = tool_result->generate_json( ).
```

`ttlMs = 0` is meaningful and is emitted when cache metadata was explicitly set. Do not rely on initial ABAP integer values to imply cache behavior.

## Prompts, Resources, and Completions

The v2 base class exposes these handlers:

- `handle_prompts_list`
- `handle_prompts_get`
- `handle_resources_list`
- `handle_resources_read`
- `handle_res_tmpls_list`
- `handle_completion`

Existing response helpers can be reused if the result is made modern-compatible by adding `resultType = "complete"` when necessary. New helper code should prefer returning a modern envelope directly.

`prompts/get` and `resources/read` may also return MRTR `input_required`; see [V2 MRTR and Elicitation](V2MRTR.md).

## Error Handling

For unsupported handlers, let the base class return method-not-found. For known application validation errors, set an appropriate JSON-RPC error:

```abap
response-error-code    = zcl_mcp_jsonrpc=>error_codes-invalid_params.
response-error-message = `message is required.`.
```

Useful error codes:

| Code | Constant | Meaning |
| ---- | -------- | ------- |
| `-32601` | `method_not_found` | Method or tool is unavailable |
| `-32602` | `invalid_params` | Request parameters are invalid |
| `-32603` | `internal_error` | Unexpected server failure |
| `-32002` | `resource_not_found` | Resource URI is unknown |
| `-32003` | `missing_client_capability` | Client did not advertise a required capability |

## Compatibility Notes

When a v2-only server is called by old SDK clients through the adapter:

- normal tools, prompts, resources, templates, completions, and many task operations can be translated
- cache hints are stripped because old clients do not understand v2 cache fields
- MRTR and elicitation cannot be represented as successful legacy results
- input-required task states cannot be completed by old clients
- `server/discover` and `tools/get_input_schema` remain modern-only

Design v2 handlers for modern clients first. The adapter is a compatibility bridge, not a replacement for v2 response design.
