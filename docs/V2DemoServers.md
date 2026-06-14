# V2 Demo Servers

The SDK includes two draft `2026-07-28` v2 demo servers. They are configured as stateless v2 endpoints and are intended to be copied from in different situations.

| Area | Service | Class | Purpose |
| ---- | ------- | ----- | ------- |
| `demo` | `demo_v2_basic` | `ZCL_MCP_DEMO_SERVER_V2_BASIC` | Normal v2 server features |
| `demo` | `demo_v2_workflow` | `ZCL_MCP_DEMO_SERVER_V2_WF` | MRTR, elicitation, requestState, and task input |

ABAP class names are limited to 30 characters, so the workflow class uses the short suffix `WF` while the endpoint keeps the clearer service name `demo_v2_workflow`.

## Basic Demo

Use `ZCL_MCP_DEMO_SERVER_V2_BASIC` as the reference for normal v2 server implementation.

It demonstrates:

- `server/discover` metadata through `get_implementation`, `get_capabilities`, and `get_instructions`
- `tools/list`
- direct `handle_tool_input_schema`
- normal `tools/call`
- `x-mcp-header` on the `echo` tool
- cache hints and `_meta` on `cache_meta`
- prompts, resources, resource templates, and completions
- legacy adapter compatibility for normal complete results

Important tool names:

| Tool | What it shows |
| ---- | ------------- |
| `echo` | Normal tool result and `Mcp-Param-Message` mirroring |
| `server_time` | Simple read-only tool with empty input schema |
| `cache_meta` | `ttlMs`, `cacheScope`, and result `_meta` |

The `echo` schema shows the top-level `x-mcp-header` pattern:

```abap
METHOD build_tool_schema.
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

The matching request must include `Mcp-Param-Message` when the body contains `arguments.message`.

Normal tool results should use the v2 result helper:

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

The `cache_meta` tool shows result metadata and cache hints:

```abap
tool_result->set_complete( ).
tool_result->set_error( abap_false ).
tool_result->add_text_content( `This result demonstrates v2 cache hints and result metadata.` ).
tool_result->set_cache( ttl_ms      = 30000
                        cache_scope = zif_mcp_constants=>cache_scopes-private ).
tool_result->set_meta( meta ).
```

For list-style helpers such as `ZCL_MCP_RESP_LIST_TOOLS`, the demo adds the modern result fields centrally:

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

## Workflow Demo

Use `ZCL_MCP_DEMO_SERVER_V2_WF` when implementing interactive v2 workflows.

It demonstrates:

- direct MRTR from `tools/call`
- `elicitation/create` input requests
- protected `requestState`
- task extension capability checks
- creating a persisted task
- `tasks/get` returning `input_required`
- `tasks/update` completing the task
- inherited `tasks/get` and `tasks/cancel` behavior for persisted tasks

Important tool names:

| Tool | What it shows |
| ---- | ------------- |
| `approval_required` | Direct `input_required` result and retry handling |
| `start_input_task` | Persisted task that waits for input and completes through `tasks/update` |

The demo checks the tasks extension before returning a task:

```abap
METHOD client_supports_tasks.
  DATA(context) = me->zif_mcp_server_v2~get_v2_context( ).

  result = xsdbool(
       context-extensions IS BOUND
   AND context-extensions->exists( `/io.modelcontextprotocol~1tasks` ) = abap_true ).
ENDMETHOD.
```

Direct MRTR is returned with a signed `requestState` and an elicitation request:

```abap
input_required = NEW zcl_mcp_resp_v2_input_req( ).
input_required->set_request_state(
  create_request_state( data        = state_data
                        ttl_seconds = 300 ) ).
input_required->add_input_request(
  request_key = c_input_confirm
  method      = elicitation->get_method( )
  params      = elicitation->get_params( ) ).

result = input_required->zif_mcp_modern_result~generate_json( ).
```

The retry path validates the state before reading client input:

```abap
state = validate_request_state( request->get_request_state( ) ).

IF state-data <> c_state_approval.
  response-error-code    = zcl_mcp_jsonrpc=>error_codes-invalid_params.
  response-error-message = `Unexpected requestState payload.`.
  RETURN.
ENDIF.

input_responses = request->get_input_responses( ).
elicitation = NEW zcl_mcp_elicit_result( input_responses->slice( |/{ c_input_confirm }| ) ).
```

The task workflow stores the same input-required shape as the task payload:

```abap
DATA(tasks) = get_tasks( ).
DATA(task_id) = tasks->create_task( tool_name     = c_tool_task
                                    ttl           = 60000
                                    poll_interval = 1000 ).

DATA(pending_input) = build_input_required(
  state_data = c_state_task
  message    = `Approve continuation of the persisted task demo.` ).

zcl_mcp_tasks=>request_input( task_id        = task_id
                              input_required = pending_input ).

response-result = build_task_result( CONV #( task_id ) ).
```

`handle_tasks_update` consumes the input, stores a terminal payload, and completes the task:

```abap
zcl_mcp_tasks=>consume_update(
  task_id         = CONV #( task_id )
  input_responses = request->get_input_responses( )
  request_state   = request->get_request_state( ) ).

zcl_mcp_tasks=>set_payload(
  task_id = CONV #( task_id )
  payload = build_completed_payload( request->get_input_responses( ) ) ).

zcl_mcp_tasks=>update_status(
  task_id = CONV #( task_id )
  status  = zcl_mcp_tasks=>status_completed
  message = `Task completed from tasks/update input.` ).
```

## Smoke Test Endpoints

With the default test system URL, the endpoints are:

```text
http://192.168.56.101:8000/zmcp/demo/demo_v2_basic
http://192.168.56.101:8000/zmcp/demo/demo_v2_workflow
```

Useful smoke checks:

- `server/discover` on both endpoints
- `tools/list` on both endpoints
- `tools/call` `echo` with `Mcp-Param-Message`
- `tools/call` `cache_meta`
- `tools/call` `approval_required` with `Mcp-Param-Reason`
- `tools/call` `start_input_task` with client task extension capability
- `tasks/get` and `tasks/update` for the returned task id

## Legacy Adapter Behavior

The basic demo is the best endpoint for checking old SDK client compatibility. Normal tools, prompts, resources, templates, and completions are translated by the adapter.

The workflow demo intentionally contains features old clients cannot complete:

- direct `input_required`
- elicitation
- task state `input_required`
- `tasks/update`

Old clients should receive clear JSON-RPC errors for these flows. Provide a separate non-interactive tool if a business capability must work for legacy clients.
