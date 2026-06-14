"! <p class="shorttext synchronized">MCP draft stateless server interface</p>
"! Server contract for the draft MCP protocol generation. This interface is
"! independent from initialize/session based legacy protocol handling.
INTERFACE zif_mcp_server_v2
  PUBLIC.

  " Request context built from the HTTP request and MCP request _meta.
  TYPES: BEGIN OF v2_context,
           area          TYPE zmcp_area,
           server        TYPE zmcp_server,
           mcp_request   TYPE zcl_mcp_jsonrpc=>request,
           protocol_ver  TYPE string,
           client_info   TYPE REF TO zif_mcp_ajson,
           client_caps   TYPE REF TO zif_mcp_ajson,
           extensions    TYPE REF TO zif_mcp_ajson,
           log_level     TYPE string,
           traceparent   TYPE string,
           tracestate    TYPE string,
           baggage       TYPE string,
           meta          TYPE REF TO zif_mcp_ajson,
           http_request  TYPE REF TO if_http_request,
           http_response TYPE REF TO if_http_response,
           http_server   TYPE REF TO if_http_server,
           cors_mode     TYPE zmcp_conf_cors,
         END OF v2_context.

  " Generic v2 response. Result JSON must already be in draft result shape.
  TYPES: BEGIN OF v2_response,
           result TYPE REF TO zif_mcp_ajson,
           error  TYPE zcl_mcp_jsonrpc=>error,
         END OF v2_response.

  "! <p class="shorttext synchronized">Set request context</p>
  "! Stores the per-request draft MCP context for the current JSON-RPC call.
  "!
  "! @parameter context | <p class="shorttext synchronized">Draft request context</p>
  METHODS set_v2_context
    IMPORTING !context TYPE v2_context.

  "! <p class="shorttext synchronized">Get request context</p>
  "! Returns the per-request draft MCP context currently assigned to this server.
  "!
  "! @parameter result | <p class="shorttext synchronized">Draft request context</p>
  METHODS get_v2_context
    RETURNING VALUE(result) TYPE v2_context.

  "! <p class="shorttext synchronized">Discover server metadata</p>
  "! Handles server/discover and returns supported versions, capabilities,
  "! server identity, instructions, and cache hints.
  "!
  "! @parameter response | <p class="shorttext synchronized">Discovery response</p>
  METHODS server_discover
    RETURNING VALUE(response) TYPE v2_response.

  "! <p class="shorttext synchronized">List prompts</p>
  "! Handles prompts/list for the draft stateless protocol.
  "!
  "! @parameter request  | <p class="shorttext synchronized">Prompt list request</p>
  "! @parameter response | <p class="shorttext synchronized">Prompt list response</p>
  METHODS prompts_list
    IMPORTING !request        TYPE REF TO zcl_mcp_req_list_prompts
    RETURNING VALUE(response) TYPE v2_response.

  "! <p class="shorttext synchronized">Get prompt</p>
  "! Handles prompts/get for the draft stateless protocol. The response may
  "! be complete or input_required when MRTR is needed.
  "!
  "! @parameter request  | <p class="shorttext synchronized">Prompt get request</p>
  "! @parameter response | <p class="shorttext synchronized">Prompt get response</p>
  METHODS prompts_get
    IMPORTING !request        TYPE REF TO zcl_mcp_req_get_prompt
    RETURNING VALUE(response) TYPE v2_response.

  "! <p class="shorttext synchronized">List resources</p>
  "! Handles resources/list for the draft stateless protocol.
  "!
  "! @parameter request  | <p class="shorttext synchronized">Resource list request</p>
  "! @parameter response | <p class="shorttext synchronized">Resource list response</p>
  METHODS resources_list
    IMPORTING !request        TYPE REF TO zcl_mcp_req_list_resources
    RETURNING VALUE(response) TYPE v2_response.

  "! <p class="shorttext synchronized">Read resource</p>
  "! Handles resources/read for the draft stateless protocol. The response may
  "! be complete or input_required when MRTR is needed.
  "!
  "! @parameter request  | <p class="shorttext synchronized">Resource read request</p>
  "! @parameter response | <p class="shorttext synchronized">Resource read response</p>
  METHODS resources_read
    IMPORTING !request        TYPE REF TO zcl_mcp_req_read_resource
    RETURNING VALUE(response) TYPE v2_response.

  "! <p class="shorttext synchronized">List resource templates</p>
  "! Handles resources/templates/list for the draft stateless protocol.
  "!
  "! @parameter request  | <p class="shorttext synchronized">Resource template list request</p>
  "! @parameter response | <p class="shorttext synchronized">Resource template list response</p>
  METHODS resources_tmpls_list
    IMPORTING !request        TYPE REF TO zcl_mcp_req_list_res_tmpls
    RETURNING VALUE(response) TYPE v2_response.

  "! <p class="shorttext synchronized">List tools</p>
  "! Handles tools/list for the draft stateless protocol.
  "!
  "! @parameter request  | <p class="shorttext synchronized">Tool list request</p>
  "! @parameter response | <p class="shorttext synchronized">Tool list response</p>
  METHODS tools_list
    IMPORTING !request        TYPE REF TO zcl_mcp_req_list_tools
    RETURNING VALUE(response) TYPE v2_response.

  "! <p class="shorttext synchronized">Get tool input schema</p>
  "! Returns the input schema for one tool. Implementations may use an indexed
  "! lookup instead of rebuilding the full tools/list result.
  "!
  "! @parameter tool_name           | <p class="shorttext synchronized">Tool name</p>
  "! @parameter result              | <p class="shorttext synchronized">Tool input schema</p>
  "! @raising   zcx_mcp_server      | <p class="shorttext synchronized">Tool schema lookup error</p>
  "! @raising   zcx_mcp_ajson_error | <p class="shorttext synchronized">JSON access error</p>
  METHODS get_tool_input_schema
    IMPORTING tool_name     TYPE string
    RETURNING VALUE(result) TYPE REF TO zif_mcp_ajson
    RAISING   zcx_mcp_server
              zcx_mcp_ajson_error.

  "! <p class="shorttext synchronized">Call tool</p>
  "! Handles tools/call for the draft stateless protocol. The response may be
  "! complete, input_required for MRTR, or task for the tasks extension.
  "!
  "! @parameter request  | <p class="shorttext synchronized">Tool call request</p>
  "! @parameter response | <p class="shorttext synchronized">Tool call response</p>
  METHODS tools_call
    IMPORTING !request        TYPE REF TO zcl_mcp_req_call_tool
    RETURNING VALUE(response) TYPE v2_response.

  "! <p class="shorttext synchronized">Complete reference values</p>
  "! Handles completion/complete for the draft stateless protocol.
  "!
  "! @parameter request  | <p class="shorttext synchronized">Completion request</p>
  "! @parameter response | <p class="shorttext synchronized">Completion response</p>
  METHODS completions_complete
    IMPORTING !request        TYPE REF TO zcl_mcp_req_complete
    RETURNING VALUE(response) TYPE v2_response.

  "! <p class="shorttext synchronized">Get task state</p>
  "! Handles tasks/get for the official tasks extension. The response returns
  "! current task state, pending input, terminal result, or terminal error.
  "!
  "! @parameter request  | <p class="shorttext synchronized">Task get request</p>
  "! @parameter response | <p class="shorttext synchronized">Task get response</p>
  METHODS tasks_get
    IMPORTING !request        TYPE REF TO zcl_mcp_req_get_task
    RETURNING VALUE(response) TYPE v2_response.

  "! <p class="shorttext synchronized">Update task input</p>
  "! Handles tasks/update for the official tasks extension. Used by clients
  "! to provide inputResponses while a task is waiting for input.
  "!
  "! @parameter request  | <p class="shorttext synchronized">Task update request</p>
  "! @parameter response | <p class="shorttext synchronized">Task update response</p>
  METHODS tasks_update
    IMPORTING !request        TYPE REF TO zcl_mcp_req_update_task
    RETURNING VALUE(response) TYPE v2_response.

  "! <p class="shorttext synchronized">Cancel task</p>
  "! Handles tasks/cancel for the official tasks extension.
  "!
  "! @parameter request  | <p class="shorttext synchronized">Task cancel request</p>
  "! @parameter response | <p class="shorttext synchronized">Task cancel response</p>
  METHODS tasks_cancel
    IMPORTING !request        TYPE REF TO zcl_mcp_req_cancel_task
    RETURNING VALUE(response) TYPE v2_response.

  " Runtime configuration for the server instance.
  DATA config TYPE REF TO zcl_mcp_configuration.

ENDINTERFACE.
