"! <p class="shorttext synchronized">MCP Server Base Logic</p>
CLASS zcl_mcp_server_base DEFINITION ABSTRACT
  PUBLIC
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES zif_mcp_server .

  PROTECTED SECTION.
    "! <p class="shorttext synchronized">Process initialization request</p>
    "! Handles server initialization with client configuration
    "! @parameter request | <p class="shorttext synchronized">Initialize request object</p>
    "! @parameter response | <p class="shorttext synchronized">Initialize response containing server capabilities</p>
    METHODS handle_initialize ABSTRACT
      IMPORTING !request TYPE REF TO zcl_mcp_req_initialize
      CHANGING  response TYPE zif_mcp_server=>initialize_response ##NEEDED.

    "! <p class="shorttext synchronized">List available prompts</p>
    "! Handles request to retrieve all prompts available on the server
    "! @parameter request | <p class="shorttext synchronized">List prompts request object</p>
    "! @parameter response | <p class="shorttext synchronized">Response with available prompts collection</p>
    METHODS handle_list_prompts
      IMPORTING !request TYPE REF TO zcl_mcp_req_list_prompts
      CHANGING  response TYPE zif_mcp_server=>list_prompts_response ##NEEDED.

    "! <p class="shorttext synchronized">Get specific prompt details</p>
    "! Retrieves detailed information for a specific prompt by ID
    "! @parameter request | <p class="shorttext synchronized">Get prompt request with prompt ID</p>
    "! @parameter response | <p class="shorttext synchronized">Response with prompt details</p>
    METHODS handle_get_prompt
      IMPORTING !request TYPE REF TO zcl_mcp_req_get_prompt
      CHANGING  response TYPE zif_mcp_server=>get_prompt_response ##NEEDED.

    "! <p class="shorttext synchronized">List available resources</p>
    "! Handles request to retrieve all resources available on the server
    "! @parameter request | <p class="shorttext synchronized">List resources request object</p>
    "! @parameter response | <p class="shorttext synchronized">Response with available resources collection</p>
    METHODS handle_list_resources
      IMPORTING !request TYPE REF TO zcl_mcp_req_list_resources
      CHANGING  response TYPE zif_mcp_server=>list_resources_response ##NEEDED.

    "! <p class="shorttext synchronized">List resource templates</p>
    "! Retrieves available resource templates for resource creation
    "! @parameter request | <p class="shorttext synchronized">List resource templates request</p>
    "! @parameter response | <p class="shorttext synchronized">Response with available resource templates</p>
    METHODS handle_list_res_tmpls
      IMPORTING !request TYPE REF TO zcl_mcp_req_list_res_tmpls
      CHANGING  response TYPE zif_mcp_server=>list_resources_tmpl_response ##NEEDED.

    "! <p class="shorttext synchronized">Read resource content</p>
    "! Retrieves content and metadata for a specific resource
    "! @parameter request | <p class="shorttext synchronized">Read resource request with resource ID</p>
    "! @parameter response | <p class="shorttext synchronized">Response with resource content and metadata</p>
    METHODS handle_resources_read
      IMPORTING !request TYPE REF TO zcl_mcp_req_read_resource
      CHANGING  response TYPE zif_mcp_server=>resources_read_response ##NEEDED.

    "! <p class="shorttext synchronized">List available tools</p>
    "! Handles request to retrieve all tools available on the server
    "! @parameter request | <p class="shorttext synchronized">List tools request object</p>
    "! @parameter response | <p class="shorttext synchronized">Response with available tools collection</p>
    METHODS handle_list_tools
      IMPORTING !request TYPE REF TO zcl_mcp_req_list_tools
      CHANGING  response TYPE zif_mcp_server=>list_tools_response ##NEEDED.

    "! <p class="shorttext synchronized">Execute tool</p>
    "! Handles request to call a specific tool with parameters
    "! @parameter request | <p class="shorttext synchronized">Call tool request with tool ID and parameters</p>
    "! @parameter response | <p class="shorttext synchronized">Response with tool execution results</p>
    METHODS handle_call_tool
      IMPORTING !request TYPE REF TO zcl_mcp_req_call_tool
      CHANGING  response TYPE zif_mcp_server=>call_tool_response ##NEEDED.

    ALIASES server FOR zif_mcp_server~server.
    ALIASES config FOR zif_mcp_server~config.
    ALIASES session FOR zif_mcp_server~session.
  PRIVATE SECTION.
    METHODS generate_fallback_uuid
      RETURNING
        VALUE(result) TYPE sysuuid_c32.
ENDCLASS.

CLASS zcl_mcp_server_base IMPLEMENTATION.
  METHOD zif_mcp_server~initialize.
    " Handle session logic before handing off to the request handler
    IF server-session_id IS NOT INITIAL.
      server-http_response->set_status( code   = 400
                                                       reason = 'Bad Request' ) ##NO_TEXT.
    ENDIF.

    IF server-session_mode <> zcl_mcp_session=>session_mode_off.
      TRY.
          server-session_id = cl_system_uuid=>create_uuid_c32_static( ).
        CATCH cx_uuid_error.
          server-session_id = generate_fallback_uuid( ).
      ENDTRY.
      TRY.
          session = NEW zcl_mcp_session( session_id   = server-session_id
                                                        session_mode = server-session_mode
                                                        create_new   = abap_true ).
        CATCH zcx_mcp_server INTO DATA(error).
          zif_mcp_server~config->get_logger( )->error( |Failed to create session { error->get_text( ) }| ) ##NO_TEXT.
          server-http_response->set_status( code   = 500
                                                           reason = 'Internal Server Error' ) ##NO_TEXT.
          RETURN.
      ENDTRY.
    ENDIF.

    IF server-session_mode = zcl_mcp_session=>session_mode_icf.
      server-http_server->set_session_stateful( ).
    ENDIF.

    response-result = NEW zcl_mcp_resp_initialize( ).
    handle_initialize( EXPORTING request = request CHANGING response = response ).
  ENDMETHOD.

  METHOD zif_mcp_server~prompts_get.
    " Pre-initialize the response object
    response-result = NEW zcl_mcp_resp_get_prompt( ).

    " Call the handler to modify the response
    handle_get_prompt( EXPORTING request = request
                       CHANGING  response = response ).
  ENDMETHOD.

  METHOD zif_mcp_server~prompts_list.
    " Pre-initialize the response object
    response-result = NEW zcl_mcp_resp_list_prompts( ).

    " Call the handler to modify the response
    handle_list_prompts( EXPORTING request = request
                         CHANGING  response = response ).
  ENDMETHOD.

  METHOD zif_mcp_server~resources_list.
    " Pre-initialize the response object
    response-result = NEW zcl_mcp_resp_list_resources( ).

    " Call the handler to modify the response
    handle_list_resources( EXPORTING request = request
                          CHANGING  response = response ).
  ENDMETHOD.

  METHOD zif_mcp_server~resources_read.
    " Pre-initialize the response object
    response-result = NEW zcl_mcp_resp_read_resource( ).

    " Call the handler to modify the response
    handle_resources_read( EXPORTING request = request
                          CHANGING  response = response ).
  ENDMETHOD.

  METHOD zif_mcp_server~resources_templates_list.
    " Pre-initialize the response object
    response-result = NEW zcl_mcp_resp_list_res_tmpl( ).

    " Call the handler to modify the response
    handle_list_res_tmpls( EXPORTING request = request
                          CHANGING  response = response ).
  ENDMETHOD.

  METHOD zif_mcp_server~tools_call.
    " Pre-initialize the response object
    response-result = NEW zcl_mcp_resp_call_tool( ).

    " Call the handler to modify the response
    handle_call_tool( EXPORTING request = request
                     CHANGING  response = response ).
  ENDMETHOD.

  METHOD zif_mcp_server~tools_list.
    " Pre-initialize the response object
    response-result = NEW zcl_mcp_resp_list_tools( ).

    " Call the handler to modify the response
    handle_list_tools( EXPORTING request = request
                      CHANGING  response = response ).
  ENDMETHOD.

  METHOD handle_list_prompts.
    " Empty implementation - to be implemented by subclasses
  ENDMETHOD.                                       "#EC EMPTY_PROCEDURE

  METHOD handle_get_prompt.
    " Empty implementation - to be implemented by subclasses
    response-error-code = zcl_mcp_jsonrpc=>error_codes-invalid_params.
    response-error-message = |This server does not implement any prompts| ##NO_TEXT.
  ENDMETHOD.

  METHOD handle_list_resources.
    " Empty implementation - to be implemented by subclasses
  ENDMETHOD.                                       "#EC EMPTY_PROCEDURE

  METHOD handle_list_res_tmpls.
    " Empty implementation - to be implemented by subclasses
  ENDMETHOD.                                       "#EC EMPTY_PROCEDURE

  METHOD handle_resources_read.
    " Empty implementation - to be implemented by subclasses
    response-error-code = zcl_mcp_jsonrpc=>error_codes-invalid_params.
    response-error-message = |This server does not implement any resources| ##NO_TEXT.
  ENDMETHOD.

  METHOD handle_list_tools.
    " Empty implementation - to be implemented by subclasses
  ENDMETHOD.                                       "#EC EMPTY_PROCEDURE

  METHOD handle_call_tool.
    " Empty implementation - to be implemented by subclasses
    response-error-code = zcl_mcp_jsonrpc=>error_codes-invalid_params.
    response-error-message = |This server does not implement any tools| ##NO_TEXT.
  ENDMETHOD.

  METHOD generate_fallback_uuid.
    DATA timestamp TYPE timestampl.
    DATA random    TYPE REF TO cl_abap_random.
    DATA hex_chars TYPE string VALUE '0123456789ABCDEF'.
    DATA char_idx  TYPE i.
    DATA hex_char  TYPE c LENGTH 1.

    " Get timestamp for uniqueness
    GET TIME STAMP FIELD timestamp.

    " Create random object with timestamp seed
    random = cl_abap_random=>create( CONV i( timestamp ) ).

    " Generate 32 random hex characters
    DO 32 TIMES.
      " Get random index into hex characters (0-15)
      char_idx = random->intinrange( low  = 0
                                     high = 15 ).
      hex_char = hex_chars+char_idx(1).
      CONCATENATE result hex_char INTO result.
    ENDDO.
  ENDMETHOD.

ENDCLASS.
