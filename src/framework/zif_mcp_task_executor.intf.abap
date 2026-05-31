"! <p class="shorttext synchronized">MCP Task Executor Interface</p>
"! Implement this interface to connect task execution to your own
"! background processing (batch jobs, RFCs, etc.).
"! Use zcl_mcp_tasks class methods to report progress back.
INTERFACE zif_mcp_task_executor
  PUBLIC.

  "! <p class="shorttext synchronized">Execute a task</p>
  "! Called by the framework after a task has been created.
  "! Implementations should be non-blocking where possible
  "! start a batch job or RFC and return immediately.
  "! Use zcl_mcp_tasks=>update_status / complete / fail to report back.
  "! @parameter task_id   | <p class="shorttext synchronized">Task ID</p>
  "! @parameter tool_name | <p class="shorttext synchronized">Originating tool name</p>
  "! @parameter input     | <p class="shorttext synchronized">Tool input parameters</p>
  METHODS execute
    IMPORTING task_id   TYPE sysuuid_c32
              tool_name TYPE string
              input     TYPE REF TO zif_mcp_ajson
    RAISING   zcx_mcp_server.

  "! <p class="shorttext synchronized">Cancel a task</p>
  "! Called by the framework on tasks/cancel.
  "! Implementations should signal the background process to stop.
  "! The framework updates the task status to cancelled after this returns.
  "! @parameter task_id | <p class="shorttext synchronized">Task ID</p>
  METHODS cancel
    IMPORTING task_id TYPE sysuuid_c32
    RAISING   zcx_mcp_server.

ENDINTERFACE.
