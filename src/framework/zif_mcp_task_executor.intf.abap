  "! <p class="shorttext synchronized">MCP Task Executor Interface</p>
  "! Optional adapter interface for connecting task execution to your own
  "! background processing mechanism (batch jobs, RFCs, etc.).
  "! The framework does not instantiate or dispatch this interface automatically.
  "! Call execute from handle_call_tool after creating a task, and call cancel
  "! from handle_cancel_task if your background process needs an explicit stop signal.
  "! Use zcl_mcp_tasks class methods to report progress back.
INTERFACE zif_mcp_task_executor
  PUBLIC.

  "! <p class="shorttext synchronized">Execute a task</p>
  "! Call this from your tool implementation after the framework task record
  "! has been created. Implementations should be non-blocking where possible:
  "! start a batch job or RFC and return immediately.
  "! Use zcl_mcp_tasks=>update_status / complete / fail to report back.
  "! @parameter task_id   | <p class="shorttext synchronized">Task ID</p>
  "! @parameter tool_name | <p class="shorttext synchronized">Originating tool name</p>
  "! @parameter input     | <p class="shorttext synchronized">Tool input parameters</p>
  METHODS execute
    IMPORTING task_id   TYPE sysuuid_c32
              tool_name TYPE string
              !input    TYPE REF TO zif_mcp_ajson
    RAISING   zcx_mcp_server.

  "! <p class="shorttext synchronized">Cancel a task</p>
  "! Call this from handle_cancel_task when your background process needs
  "! an explicit stop signal. The framework updates the task status to
  "! cancelled after handle_cancel_task returns without an error.
  "! @parameter task_id | <p class="shorttext synchronized">Task ID</p>
  METHODS cancel
    IMPORTING task_id TYPE sysuuid_c32
    RAISING   zcx_mcp_server.

ENDINTERFACE.
