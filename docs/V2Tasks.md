# V2 Tasks Extension

The v2 Tasks extension supports long-running work through explicit task handles. It is separate from protocol sessions and works with the stateless v2 request model.

The extension key is:

```text
io.modelcontextprotocol/tasks
```

## Capabilities

Advertise task support in discovery:

```abap
METHOD get_capabilities.
  result-tools = abap_true.
  result-tasks = abap_true.
ENDMETHOD.
```

Modern clients must declare the tasks extension before receiving task results. The router enforces this centrally for `resultType = "task"`, even if the application handler forgets its own check.

`ZCL_MCP_DEMO_SERVER_V2_WF` also performs an application-side check so the error is clear before it creates work:

```abap
METHOD client_supports_tasks.
  DATA(context) = me->zif_mcp_server_v2~get_v2_context( ).

  result = xsdbool(
       context-extensions IS BOUND
   AND context-extensions->exists( `/io.modelcontextprotocol~1tasks` ) = abap_true ).
ENDMETHOD.
```

## Returning a Task From a Tool

Use `ZCL_MCP_RESP_V2_TASK` when a tool starts asynchronous work.

```abap
METHOD handle_tools_call.
  IF request->get_name( ) = c_tool_task.
    IF client_supports_tasks( ) = abap_false.
      response = missing_tasks_capability( ).
      RETURN.
    ENDIF.

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
  ENDIF.
ENDMETHOD.
```

This demo task immediately moves to `input_required`. For normal background work you can leave it in `working`, run a job/RFC, and later store a terminal payload.

## Persisted Task Manager

`ZCL_MCP_SERVER_BASE_V2` exposes `get_tasks( )`, scoped to the current v2 area/server.

```abap
DATA(tasks) = get_tasks( ).
DATA(task_id) = tasks->create_task( tool_name     = request->get_name( )
                                    ttl           = 60000
                                    poll_interval = 1000 ).
```

The task manager persists task lifecycle data in the task table and supports terminal payloads, errors, cancellation, and input-required state. Use `zmcp_clear_mcp_tasks` as a scheduled cleanup job.

## `tasks/get`

The base class implements a persisted-task `tasks/get` handler. Redefine `handle_tasks_get` only when you need custom task lookup.

For custom responses, use `ZCL_MCP_RESP_V2_TASK_GET`.

Working:

```abap
DATA(task_get) = NEW zcl_mcp_resp_v2_task_get( ).

task_get->set_task( task_id          = task_id
                    status           = zif_mcp_types=>task_states-working
                    status_message   = `Still running.`
                    ttl_ms           = 600000
                    poll_interval_ms = 2000 ).

response-result = task_get->zif_mcp_modern_result~generate_json( ).
```

Completed:

```abap
DATA(tool_result) = NEW zcl_mcp_resp_v2_tool( ).
tool_result->set_complete( ).
tool_result->set_error( abap_false ).
tool_result->add_text_content( `Report complete.` ).

DATA(task_get) = NEW zcl_mcp_resp_v2_task_get( ).
task_get->set_task( task_id        = task_id
                    status         = zif_mcp_types=>task_states-completed
                    status_message = `Completed.` ).
task_get->set_result( tool_result->generate_json( ) ).

response-result = task_get->zif_mcp_modern_result~generate_json( ).
```

Failed:

```abap
DATA(task_get) = NEW zcl_mcp_resp_v2_task_get( ).
task_get->set_task( task_id        = task_id
                    status         = zif_mcp_types=>task_states-failed
                    status_message = `Report failed.` ).
task_get->set_error( code    = zcl_mcp_jsonrpc=>error_codes-internal_error
                     message = `Report generation failed.` ).

response-result = task_get->zif_mcp_modern_result~generate_json( ).
```

## Input-Required Tasks

Tasks can pause for client input. The task exposes `inputRequests` and `requestState` through `tasks/get`; the client answers through `tasks/update`.

```abap
DATA(builder) = NEW zcl_mcp_schema_builder( ).
builder->add_boolean(
  name        = `approved`
  description = `Whether the demo workflow may continue.`
  required    = abap_true ).
builder->add_string(
  name        = `comment`
  description = `Optional user comment.`
  required    = abap_false ).

DATA(elicitation) = NEW zcl_mcp_input_elicitation( ).
elicitation->set_form(
  message          = `Approve continuation of the persisted task demo.`
  requested_schema = builder->to_json( ) ).

DATA(input_required) = NEW zcl_mcp_resp_v2_input_req( ).
input_required->set_request_state(
  create_request_state( data        = c_state_task
                        ttl_seconds = 300 ) ).
input_required->add_input_request(
  request_key = c_input_confirm
  method      = elicitation->get_method( )
  params      = elicitation->get_params( ) ).

zcl_mcp_tasks=>request_input( task_id        = task_id
                              input_required = input_required->zif_mcp_modern_result~generate_json( ) ).
```

The base `handle_tasks_get` can expose this pending input. The base `handle_tasks_update` consumes `requestState` and `inputResponses` for persisted tasks.

## `tasks/update`

`tasks/update` is only valid while a task is waiting for input.

The base implementation:

- checks the task exists
- checks the task status is `input_required`
- validates `requestState`
- consumes `inputResponses`
- returns a complete acknowledgement

`ZCL_MCP_DEMO_SERVER_V2_WF` redefines `handle_tasks_update` because it wants to complete the demo task immediately after receiving input:

```abap
state = validate_request_state(
  request_state = request->get_request_state( )
  method        = `tools/call` ).

IF state-data <> c_state_task.
  response-error-code    = zcl_mcp_jsonrpc=>error_codes-invalid_params.
  response-error-message = `Unexpected task requestState payload.`.
  RETURN.
ENDIF.

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

Custom handlers should return `invalid_params` when a task is not waiting for input.

## `tasks/cancel`

The base `handle_tasks_cancel` checks the task and calls central cancellation, then returns a complete acknowledgement.

If your task starts an ABAP background job or RFC, make sure your task executor can actually stop or mark the work as cancelled. Protocol cancellation and business cancellation are not the same thing.

## Legacy Compatibility

The legacy adapter translates practical task operations:

| V2 task behavior | Legacy result |
| ---------------- | ------------- |
| tool returns `resultType = "task"` | legacy create-task shape |
| `tasks/get` working/completed/failed/cancelled | legacy task shape |
| `tasks/result` completed | terminal payload |
| `tasks/result` failed | terminal JSON-RPC error |
| `tasks/list` | legacy task list, excluding input-required tasks |
| `tasks/cancel` | legacy task shape/acknowledgement where possible |

Legacy clients cannot provide v2 `tasks/update` input. Input-required task states fail explicitly for legacy clients.

The adapter declares the v2 tasks extension centrally in the compatibility context so task-returning v2 handlers can be reused for old clients.

## Practical Guidance

- Use tasks for work that may take longer than normal HTTP request timeouts.
- Prefer short polling intervals only while work is expected to finish soon.
- Always set a TTL for externally visible task handles.
- Store final payloads in the task manager when a task completes.
- Do not put secrets in `requestState`.
- Provide a non-interactive fallback if old clients must use a task workflow that otherwise requires `tasks/update`.
