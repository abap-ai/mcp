"! <p class="shorttext synchronized" lang="en">MCP Server Interface</p>
"! Provides the MCP server methods. Get an instance via the factory.
"! It is recommended to base your server on the base server class.
INTERFACE zif_mcp_server
  PUBLIC.

  TYPES: BEGIN OF initialize_response,
           result TYPE REF TO zcl_mcp_resp_initialize,
           error  TYPE zcl_mcp_jsonrpc=>error,
         END OF initialize_response.

  TYPES: BEGIN OF list_prompts_response,
           result TYPE REF TO zcl_mcp_resp_list_prompts,
           error  TYPE zcl_mcp_jsonrpc=>error,
         END OF list_prompts_response.

  TYPES: BEGIN OF get_prompt_response,
           result TYPE REF TO zcl_mcp_resp_get_prompt,
           error  TYPE zcl_mcp_jsonrpc=>error,
         END OF get_prompt_response.

  TYPES: BEGIN OF list_resources_response,
           result TYPE REF TO zcl_mcp_resp_list_resources,
           error  TYPE zcl_mcp_jsonrpc=>error,
         END OF list_resources_response.

  TYPES: BEGIN OF list_resources_tmpl_response,
           result TYPE REF TO zcl_mcp_resp_list_res_tmpl,
           error  TYPE zcl_mcp_jsonrpc=>error,
         END OF list_resources_tmpl_response.

  TYPES: BEGIN OF resources_read_response,
           result TYPE REF TO zcl_mcp_resp_read_resource,
           error  TYPE zcl_mcp_jsonrpc=>error,
         END OF resources_read_response.

  TYPES: BEGIN OF list_tools_response,
           result TYPE REF TO zcl_mcp_resp_list_tools,
           error  TYPE zcl_mcp_jsonrpc=>error,
         END OF list_tools_response.

  TYPES: BEGIN OF call_tool_response,
           result TYPE REF TO zcl_mcp_resp_call_tool,
           error  TYPE zcl_mcp_jsonrpc=>error,
         END OF call_tool_response.

  TYPES: BEGIN OF server_data,
           area             TYPE zmcp_area,
           server           TYPE zmcp_server,
           mcp_request      TYPE zcl_mcp_jsonrpc=>request,
           protocol_version TYPE string,
           http_request     TYPE REF TO if_http_request,
           http_response    TYPE REF TO if_http_response,
           http_server      TYPE REF TO if_http_server,
           session_mode     TYPE zmcp_session_mode,
           session_id       TYPE guid_32,
           cors_mode        TYPE zmcp_conf_cors,
         END OF server_data.

  "! <p class="shorttext synchronized">MCP initialize</p>
  "!
  "! @parameter request  | <p class="shorttext synchronized">JSONRPC Request</p>
  "! @parameter response | <p class="shorttext synchronized">JSONRPC Response</p>
  METHODS initialize IMPORTING !request        TYPE REF TO zcl_mcp_req_initialize
                     RETURNING VALUE(response) TYPE initialize_response.

  "! <p class="shorttext synchronized">MCP prompts/list</p>
  "!
  "! @parameter request  | <p class="shorttext synchronized">JSONRPC Request</p>
  "! @parameter response | <p class="shorttext synchronized">JSONRPC Response</p>
  METHODS prompts_list IMPORTING !request        TYPE REF TO zcl_mcp_req_list_prompts
                       RETURNING VALUE(response) TYPE list_prompts_response.

  "! <p class="shorttext synchronized">MCP prompts/get</p>
  "!
  "! @parameter request  | <p class="shorttext synchronized">JSONRPC Request</p>
  "! @parameter response | <p class="shorttext synchronized">JSONRPC Response</p>
  METHODS prompts_get IMPORTING !request        TYPE REF TO zcl_mcp_req_get_prompt
                      RETURNING VALUE(response) TYPE get_prompt_response.

  "! <p class="shorttext synchronized">MCP resources/list</p>
  "!
  "! @parameter request  | <p class="shorttext synchronized">JSONRPC Request</p>
  "! @parameter response | <p class="shorttext synchronized">JSONRPC Response</p>
  METHODS resources_list IMPORTING !request        TYPE REF TO zcl_mcp_req_list_resources
                         RETURNING VALUE(response) TYPE list_resources_response.

  "! <p class="shorttext synchronized">MCP resources/read</p>
  "!
  "! @parameter request  | <p class="shorttext synchronized">JSONRPC Request</p>
  "! @parameter response | <p class="shorttext synchronized">JSONRPC Response</p>
  METHODS resources_read IMPORTING !request        TYPE REF TO zcl_mcp_req_read_resource
                         RETURNING VALUE(response) TYPE resources_read_response.

  "! <p class="shorttext synchronized">MCP resources/templates/list</p>
  "!
  "! @parameter request  | <p class="shorttext synchronized">JSONRPC Request</p>
  "! @parameter response | <p class="shorttext synchronized">JSONRPC Response</p>
  METHODS resources_templates_list IMPORTING !request        TYPE REF TO zcl_mcp_req_list_res_tmpls
                                   RETURNING VALUE(response) TYPE list_resources_tmpl_response.

  "! <p class="shorttext synchronized">MCP tools/list</p>
  "!
  "! @parameter request  | <p class="shorttext synchronized">JSONRPC Request</p>
  "! @parameter response | <p class="shorttext synchronized">JSONRPC Response</p>
  METHODS tools_list IMPORTING !request        TYPE REF TO zcl_mcp_req_list_tools
                     RETURNING VALUE(response) TYPE list_tools_response.

  "! <p class="shorttext synchronized">MCP tools/call</p>
  "!
  "! @parameter request  | <p class="shorttext synchronized">JSONRPC Request</p>
  "! @parameter response | <p class="shorttext synchronized">JSONRPC Response</p>
  METHODS tools_call IMPORTING !request        TYPE REF TO zcl_mcp_req_call_tool
                     RETURNING VALUE(response) TYPE call_tool_response.

  CONSTANTS role_user      TYPE string VALUE `user`.
  CONSTANTS role_assistant TYPE string VALUE `assistant`.


  DATA session TYPE REF TO zcl_mcp_session.
  DATA server  TYPE server_data.
  DATA config  TYPE REF TO zcl_mcp_configuration.

ENDINTERFACE.
