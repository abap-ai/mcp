# MCP Tasks (MCP 2025-11-25)

The Tasks feature lets an MCP server offload long-running work to background jobs and let clients poll for the result. It is part of the MCP 2025-11-25 protocol version.

## Table of Contents

- [Overview](#overview)
- [Task Lifecycle](#task-lifecycle)
- [Units: Milliseconds vs Seconds](#units-milliseconds-vs-seconds)
- [Database Table](#database-table)
- [ZCL_MCP_TASKS](#zcl_mcp_tasks)
- [ZIF_MCP_TASK_EXECUTOR](#zif_mcp_task_executor)
- [Declaring Task Support on a Tool](#declaring-task-support-on-a-tool)
- [Implementing an Async Tool](#implementing-an-async-tool)
- [Client-Side Flow](#client-side-flow)
- [Maintenance](#maintenance)
- [API Reference](#api-reference)

## Overview

When a tool call may take more than a few seconds, the server can:

1. Create a task record (`get_tasks( )->create_task` from `ZCL_MCP_SERVER_BASE`, or an explicitly instantiated `ZCL_MCP_TASKS` object).
2. Launch a background job or RFC that performs the work.
3. Return the task object to the client immediately.
4. The client polls `tasks/get` until the status is `completed` (or `failed`/`cancelled`).
5. The client retrieves the result with `tasks/result`.

The framework handles the `tasks/list`, `tasks/get`, `tasks/result`, and `tasks/cancel` wire protocol. You implement the tool, start the background process, and optionally override `handle_cancel_task` to signal the process to stop.

## Task Lifecycle

```
working → completed
        → failed
        → cancelled       (via tasks/cancel)
```

Note: This ABAP implementation does not support the MCP `input_required` task state. The current HTTP transport does not keep an SSE or long-lived response channel open for task-related follow-up messages. Tasks therefore remain `working` until they reach `completed`, `failed`, or `cancelled`.

Status constants are available on `ZCL_MCP_TASKS` and mirrored in `ZIF_MCP_TYPES=>task_states`:

| Constant | Wire value | Meaning |
| -------- | ---------- | ------- |
| `status_working`        | `working`        | Background job is running |
| `status_completed`      | `completed`      | Payload is ready for retrieval |
| `status_failed`         | `failed`         | Job encountered a non-recoverable error |
| `status_cancelled`      | `cancelled`      | Client requested cancellation |

## Units: Milliseconds vs Seconds

The MCP spec and all public API surfaces use **milliseconds** for time-related fields. Internally the database column `ZMCP_TASKS-TTL` stores **seconds** to fit in a standard INT4 field.

| Field | Public API / wire | Database storage | Notes |
| ----- | ----------------- | ---------------- | ----- |
| `ttl` (create_task, tasks/get, tasks/list) | milliseconds | seconds (converted on write/read) | `create_task( ttl = 3600000 )` stores 3600 s |
| `poll_interval` (create_task, tasks/get) | milliseconds | milliseconds (no conversion) | Stored and returned as-is |

**Common mistake:** passing a bare seconds value (e.g., `300`) to `create_task( ttl = ... )`. Because the parameter is in milliseconds, `300` means 300 ms, which rounds up to 1 second in the database — a completed task result would disappear almost immediately. Use `300000` for 5 minutes or `3600000` for 1 hour.

## Database Table

Tasks are stored in the `ZMCP_TASKS` table. The table is scoped by `AREA` + `SERVER` so task IDs are isolated per server. Do not read or write this table directly — use `ZCL_MCP_TASKS`.

## ZCL_MCP_TASKS

The task manager class. From a `ZCL_MCP_SERVER_BASE` subclass, use the protected helper:

```abap
DATA(tasks) = get_tasks( ).
```

If you use `ZCL_MCP_TASKS` outside the base class, instantiate it with the area and server:

```abap
DATA(tasks) = NEW zcl_mcp_tasks( area = server-area server = server-server ).
```

### Creating a Task

```abap
DATA(task_id) = tasks->create_task(
    tool_name     = 'start_long_export'
    session_id    = server-session_id   " optional
    ttl           = 3600000             " requested TTL in ms; 0 = no expiry
    poll_interval = 5000                " suggested client poll interval in ms
).
```

See [Units: Milliseconds vs Seconds](#units-milliseconds-vs-seconds). `ZMCP_TASKS-TTL` stores seconds; the public API and wire responses use milliseconds. `poll_interval` is stored as-is in milliseconds.

### Updating Status

From the background job (class methods — safe to call without an HTTP context):

```abap
" Report completion with a CallToolResult-shaped payload
DATA(task_result) = NEW zcl_mcp_resp_task_payload( ).
task_result->add_text_content( 'Export finished' ).

DATA(structured) = zcl_mcp_ajson=>create_empty( ).
structured->set_string( iv_path = '/result' iv_val = 'done' ).
task_result->set_structured_content( structured ).

zcl_mcp_tasks=>complete(
    task_id = p_taskid
    result  = task_result ).

" Report failure
zcl_mcp_tasks=>fail(
    task_id = p_taskid
    message = 'Export failed: connection timeout' ).
```

`complete` stores the generated task payload and sets the task status to `completed`. If `task_result->set_is_error( abap_true )` was used, the stored payload is returned from `tasks/result` and the task status becomes `failed`.

### Reading Status (from background job)

```abap
DATA(status) = zcl_mcp_tasks=>get_status( p_taskid ).
IF status = zcl_mcp_tasks=>status_cancelled.
  RETURN.  " bail out early
ENDIF.
```

## ZIF_MCP_TASK_EXECUTOR

Implement this interface if you want a reusable adapter between a tool call and your background processing mechanism. The framework does not instantiate this interface automatically; call `execute` from `handle_call_tool` after `create_task`, and either call your executor from `handle_cancel_task` or handle cancellation directly there.

```abap
CLASS zcl_my_task_executor DEFINITION
  PUBLIC FINAL
  CREATE PUBLIC
  IMPLEMENTING zif_mcp_task_executor.

  PUBLIC SECTION.
    METHODS zif_mcp_task_executor~execute
      IMPORTING task_id   TYPE sysuuid_c32
                tool_name TYPE string
                input     TYPE REF TO zif_mcp_ajson
      RAISING   zcx_mcp_server.

    METHODS zif_mcp_task_executor~cancel
      IMPORTING task_id TYPE sysuuid_c32
      RAISING   zcx_mcp_server.
ENDCLASS.

CLASS zcl_my_task_executor IMPLEMENTATION.
  METHOD zif_mcp_task_executor~execute.
    " Submit a background job and return immediately.
    " The job calls zcl_mcp_tasks=>complete / fail when done.
    SUBMIT zmcp_my_bg_job
        WITH p_taskid = task_id
        VIA JOB 'ZMCP_TASK' NUMBER DATA(jobnr)
        AND RETURN.
  ENDMETHOD.

  METHOD zif_mcp_task_executor~cancel.
    " Signal the job to stop — e.g. write a flag the job polls.
    " The framework sets status to 'cancelled' after this returns.
  ENDMETHOD.
ENDCLASS.
```

## Declaring Task Support on a Tool

Set `execution-task_support` in the tool definition (see [Tools](Tools.md)):

```abap
APPEND VALUE #(
    name      = 'start_long_export'
    execution = VALUE #( task_support = zcl_mcp_resp_list_tools=>task_support-optional )
) TO tools.
```

Also declare task support during initialization when your server accepts task-augmented tool calls:

```abap
response-result->set_capabilities( VALUE #(
    tools = VALUE #( enabled = abap_true )
    tasks = VALUE #( list       = abap_true
                     cancel     = abap_true
                     tools_call = abap_true ) ) ).
```

## Implementing an Async Tool

```abap
METHOD handle_call_tool.
  CASE request->get_name( ).
    WHEN 'start_long_export'.
      TRY.
          IF request->has_task( ) = abap_false.
            response-error-code    = zcl_mcp_jsonrpc=>error_codes-invalid_request.
            response-error-message = 'Call this tool with task metadata for async execution'.
            RETURN.
          ENDIF.

          DATA(tasks) = get_tasks( ).
          DATA(task_id) = tasks->create_task(
              tool_name     = request->get_name( )
              session_id    = server-session_id
              ttl           = COND #( WHEN request->get_task_ttl( ) > 0
                                      THEN request->get_task_ttl( )
                                      ELSE 3600000 )
              poll_interval = 5000 ).

          " Launch background processing
          NEW zcl_my_task_executor( )->zif_mcp_task_executor~execute(
              task_id   = task_id
              tool_name = request->get_name( )
              input     = request->get_arguments( ) ).

          " Return the task object to the client
          DATA(task) = tasks->get( task_id ).
          response-result->set_task_result( task ).

        CATCH zcx_mcp_server INTO DATA(err).
          response-error-code    = zcl_mcp_jsonrpc=>error_codes-internal_error.
          response-error-message = err->get_text( ).
      ENDTRY.
  ENDCASE.
ENDMETHOD.
```

## Client-Side Flow

1. Client calls `tools/call` with a `task` object in the params, for example `{ "name": "...", "arguments": {...}, "task": { "ttl": 3600000 } }`.
2. The tool response contains a `task` object with `taskId`, `status`, `createdAt`, `lastUpdatedAt`, `ttl`, and optionally `pollInterval`.
3. Client polls `tasks/get` with the task ID until `status` is `completed`.
4. Client calls `tasks/result` to retrieve the payload JSON.
5. Optionally, client calls `tasks/cancel` to abort a running task.

The suggested polling interval is returned in the task object as `pollInterval` in milliseconds.

## Maintenance

Use the report `ZMCP_CLEAR_MCP_TASKS` to remove outdated records:

- Completed, failed, and cancelled tasks whose TTL has elapsed (measured from `LAST_UPDATED`) are deleted.
- Terminal tasks without a TTL (TTL = 0 / no expiry) are deleted after a default retention period of **7 days**.
- Working tasks older than **24 hours** are deleted as stuck jobs.

Schedule this report as a regular background job.

## API Reference

### ZCL_MCP_TASKS Instance Methods

| Method | Description |
| ------ | ----------- |
| `create_task(tool_name, session_id?, ttl = 0, poll_interval = 5000)` | Creates a new task record; returns the task ID. `ttl` is in ms (`0` = no expiry); `poll_interval` is the suggested client poll interval in ms |
| `get(task_id)` | Returns task header (status, timestamps, TTL) |
| `get_payload(task_id)` | Returns the result payload JSON for a completed task |
| `list(cursor?)` | Returns a paginated list of tasks for this area+server |

### ZCL_MCP_TASKS Class Methods

| Method | Description |
| ------ | ----------- |
| `get_status(task_id)` | Returns the current status string (safe from batch) |
| `update_status(task_id, status)` | Transitions task to a new status |
| `set_payload(task_id, payload)` | Stores raw payload JSON for a task |
| `complete(task_id, result)` | Stores a `ZCL_MCP_RESP_TASK_PAYLOAD` and marks the task completed, or failed when `result->get_is_error( )` is true |
| `fail(task_id, message)` | Marks task failed with an error message |
| `cancel(task_id)` | Marks task cancelled |
| `delete_outdated_tasks()` | Removes expired tasks; returns count deleted |

### ZCL_MCP_RESP_TASK_PAYLOAD

The result object passed to `complete`. It is shaped like a `CallToolResult`:

| Method | Description |
| ------ | ----------- |
| `add_text_content(text)` | Adds a text content item |
| `set_structured_content(json)` | Sets the machine-readable structured result |
| `set_is_error(is_error)` | Marks the payload as an error result (stored payload is returned, task becomes `failed`) |
| `get_is_error()` | Returns whether the payload is flagged as an error |
| `set_meta(meta)` | Attaches optional `_meta` to the payload |
| `set_related_task(task_id)` | Links the payload to a related task ID |
| `set_from_json(json)` | Builds the payload from a raw `CallToolResult`-shaped JSON object |

### Task Methods and Hook (ZCL_MCP_SERVER_BASE)

The base class provides default implementations that delegate to `ZCL_MCP_TASKS`:

| Method | Description |
| ------ | ----------- |
| `tasks_list` | List tasks scoped to this server and current user |
| `tasks_get` | Return task header by ID |
| `tasks_result` | Return completed or failed task payload |
| `tasks_cancel` | Check ownership, call `handle_cancel_task`, then mark the task cancelled |
| `handle_cancel_task` | Optional override point to signal a background process before cancellation is persisted |
