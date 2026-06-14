"! <p class="shorttext synchronized">MCP draft server base class</p>
"! Base implementation for stateless MCP draft servers. Subclasses override
"! protected handlers and metadata methods.
CLASS zcl_mcp_server_base_v2 DEFINITION
PUBLIC ABSTRACT
CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES zif_mcp_server_v2.

    ALIASES config FOR zif_mcp_server_v2~config.

  PROTECTED SECTION.
    "! <p class="shorttext synchronized">Get server implementation metadata</p>
    "! Returns mandatory and optional server identity metadata.
    "!
    "! @parameter result | <p class="shorttext synchronized">Server implementation metadata</p>
    METHODS get_implementation ABSTRACT
      RETURNING VALUE(result) TYPE zcl_mcp_resp_server_disc=>implementation.

    "! <p class="shorttext synchronized">Get server capabilities</p>
    "! Returns capabilities advertised by server/discover.
    "!
    "! @parameter result | <p class="shorttext synchronized">Server capabilities</p>
    METHODS get_capabilities
      RETURNING VALUE(result) TYPE zcl_mcp_resp_server_disc=>capabilities.

    "! <p class="shorttext synchronized">Get server instructions</p>
    "! Returns optional LLM-facing usage instructions for server/discover.
    "!
    "! @parameter result | <p class="shorttext synchronized">Server instructions</p>
    METHODS get_instructions
      RETURNING VALUE(result) TYPE string.

    "! <p class="shorttext synchronized">Handle prompts/list</p>
    "! Override to return available prompts.
    "!
    "! @parameter request  | <p class="shorttext synchronized">Prompt list request</p>
    "! @parameter response | <p class="shorttext synchronized">Prompt list response</p>
    METHODS handle_prompts_list
      IMPORTING !request        TYPE REF TO zcl_mcp_req_list_prompts
      RETURNING VALUE(response) TYPE zif_mcp_server_v2=>v2_response.

    "! <p class="shorttext synchronized">Handle prompts/get</p>
    "! Override to return a prompt or an MRTR input_required result.
    "!
    "! @parameter request  | <p class="shorttext synchronized">Prompt get request</p>
    "! @parameter response | <p class="shorttext synchronized">Prompt get response</p>
    METHODS handle_prompts_get
      IMPORTING !request        TYPE REF TO zcl_mcp_req_get_prompt
      RETURNING VALUE(response) TYPE zif_mcp_server_v2=>v2_response.

    "! <p class="shorttext synchronized">Handle resources/list</p>
    "! Override to return available resources.
    "!
    "! @parameter request  | <p class="shorttext synchronized">Resource list request</p>
    "! @parameter response | <p class="shorttext synchronized">Resource list response</p>
    METHODS handle_resources_list
      IMPORTING !request        TYPE REF TO zcl_mcp_req_list_resources
      RETURNING VALUE(response) TYPE zif_mcp_server_v2=>v2_response.

    "! <p class="shorttext synchronized">Handle resources/read</p>
    "! Override to return resource contents or an MRTR input_required result.
    "!
    "! @parameter request  | <p class="shorttext synchronized">Resource read request</p>
    "! @parameter response | <p class="shorttext synchronized">Resource read response</p>
    METHODS handle_resources_read
      IMPORTING !request        TYPE REF TO zcl_mcp_req_read_resource
      RETURNING VALUE(response) TYPE zif_mcp_server_v2=>v2_response.

    "! <p class="shorttext synchronized">Handle resources/templates/list</p>
    "! Override to return available resource templates.
    "!
    "! @parameter request  | <p class="shorttext synchronized">Resource template list request</p>
    "! @parameter response | <p class="shorttext synchronized">Resource template list response</p>
    METHODS handle_res_tmpls_list
      IMPORTING !request        TYPE REF TO zcl_mcp_req_list_res_tmpls
      RETURNING VALUE(response) TYPE zif_mcp_server_v2=>v2_response.

    "! <p class="shorttext synchronized">Handle tools/list</p>
    "! Override to return available tools.
    "!
    "! @parameter request  | <p class="shorttext synchronized">Tool list request</p>
    "! @parameter response | <p class="shorttext synchronized">Tool list response</p>
    METHODS handle_tools_list
      IMPORTING !request        TYPE REF TO zcl_mcp_req_list_tools
      RETURNING VALUE(response) TYPE zif_mcp_server_v2=>v2_response.

    "! <p class="shorttext synchronized">Get tool input schema</p>
    "! Override for a direct per-tool schema lookup. The default implementation
    "! falls back to tools/list for compatibility.
    "!
    "! @parameter tool_name           | <p class="shorttext synchronized">Tool name</p>
    "! @parameter result              | <p class="shorttext synchronized">Tool input schema</p>
    "! @raising   zcx_mcp_server      | <p class="shorttext synchronized">Tool schema lookup error</p>
    "! @raising   zcx_mcp_ajson_error | <p class="shorttext synchronized">JSON access error</p>
    METHODS handle_tool_input_schema
      IMPORTING tool_name     TYPE string
      RETURNING VALUE(result) TYPE REF TO zif_mcp_ajson
      RAISING   zcx_mcp_server
                zcx_mcp_ajson_error.

    "! <p class="shorttext synchronized">Handle tools/call</p>
    "! Override to execute a tool, return MRTR input_required, or return a task.
    "!
    "! @parameter request  | <p class="shorttext synchronized">Tool call request</p>
    "! @parameter response | <p class="shorttext synchronized">Tool call response</p>
    METHODS handle_tools_call
      IMPORTING !request        TYPE REF TO zcl_mcp_req_call_tool
      RETURNING VALUE(response) TYPE zif_mcp_server_v2=>v2_response.

    "! <p class="shorttext synchronized">Handle completion/complete</p>
    "! Override to return completion candidates.
    "!
    "! @parameter request  | <p class="shorttext synchronized">Completion request</p>
    "! @parameter response | <p class="shorttext synchronized">Completion response</p>
    METHODS handle_completion
      IMPORTING !request        TYPE REF TO zcl_mcp_req_complete
      RETURNING VALUE(response) TYPE zif_mcp_server_v2=>v2_response.

    "! <p class="shorttext synchronized">Handle tasks/get</p>
    "! Override to return task state for the tasks extension.
    "!
    "! @parameter request  | <p class="shorttext synchronized">Task get request</p>
    "! @parameter response | <p class="shorttext synchronized">Task get response</p>
    METHODS handle_tasks_get
      IMPORTING !request        TYPE REF TO zcl_mcp_req_get_task
      RETURNING VALUE(response) TYPE zif_mcp_server_v2=>v2_response.

    "! <p class="shorttext synchronized">Handle tasks/update</p>
    "! Override to accept task inputResponses for the tasks extension.
    "!
    "! @parameter request  | <p class="shorttext synchronized">Task update request</p>
    "! @parameter response | <p class="shorttext synchronized">Task update response</p>
    METHODS handle_tasks_update
      IMPORTING !request        TYPE REF TO zcl_mcp_req_update_task
      RETURNING VALUE(response) TYPE zif_mcp_server_v2=>v2_response.

    "! <p class="shorttext synchronized">Handle tasks/cancel</p>
    "! Override to cancel task execution for the tasks extension.
    "!
    "! @parameter request  | <p class="shorttext synchronized">Task cancel request</p>
    "! @parameter response | <p class="shorttext synchronized">Task cancel response</p>
    METHODS handle_tasks_cancel
      IMPORTING !request        TYPE REF TO zcl_mcp_req_cancel_task
      RETURNING VALUE(response) TYPE zif_mcp_server_v2=>v2_response.

    "! <p class="shorttext synchronized">Create method-not-found response</p>
    "! Creates a standard JSON-RPC method-not-found error response.
    "!
    "! @parameter method   | <p class="shorttext synchronized">MCP method name</p>
    "! @parameter response | <p class="shorttext synchronized">Error response</p>
    METHODS method_not_found
      IMPORTING !method         TYPE string
      RETURNING VALUE(response) TYPE zif_mcp_server_v2=>v2_response.

    "! <p class="shorttext synchronized">Create protected requestState</p>
    "! Signs an MRTR requestState for the current v2 context.
    "!
    "! @parameter data                | <p class="shorttext synchronized">Application state payload</p>
    "! @parameter ttl_seconds         | <p class="shorttext synchronized">Validity in seconds</p>
    "! @parameter result              | <p class="shorttext synchronized">Signed requestState token</p>
    "! @raising   zcx_mcp_ajson_error | <p class="shorttext synchronized">JSON build error</p>
    "! @raising   zcx_mcp_server      | <p class="shorttext synchronized">Missing context or signing error</p>
    METHODS create_request_state
      IMPORTING !data         TYPE string
                ttl_seconds   TYPE i DEFAULT 300
      RETURNING VALUE(result) TYPE string
      RAISING   zcx_mcp_ajson_error
                zcx_mcp_server.

    "! <p class="shorttext synchronized">Validate protected requestState</p>
    "! Validates an MRTR requestState against the current v2 context.
    "!
    "! @parameter request_state       | <p class="shorttext synchronized">Signed requestState token</p>
    "! @parameter method              | <p class="shorttext synchronized">Expected original method, defaults to current request method</p>
    "! @parameter result              | <p class="shorttext synchronized">Validated request state</p>
    "! @raising   zcx_mcp_ajson_error | <p class="shorttext synchronized">JSON parse error</p>
    "! @raising   zcx_mcp_server      | <p class="shorttext synchronized">Invalid, expired, or mismatched state</p>
    METHODS validate_request_state
      IMPORTING request_state TYPE string
                !method       TYPE string OPTIONAL
      RETURNING VALUE(result) TYPE zcl_mcp_req_state=>state_data
      RAISING   zcx_mcp_ajson_error
                zcx_mcp_server.

    "! <p class="shorttext synchronized">Access the task manager</p>
    "! Uses the current v2 context area/server.
    "!
    "! @parameter result | <p class="shorttext synchronized">Task manager</p>
    METHODS get_tasks
      RETURNING VALUE(result) TYPE REF TO zcl_mcp_tasks
      RAISING   zcx_mcp_server.

  PRIVATE SECTION.
    DATA int_context TYPE zif_mcp_server_v2=>v2_context.
    DATA tasks       TYPE REF TO zcl_mcp_tasks.
ENDCLASS.

CLASS zcl_mcp_server_base_v2 IMPLEMENTATION.
  METHOD zif_mcp_server_v2~set_v2_context.
    int_context = context.
  ENDMETHOD.

  METHOD zif_mcp_server_v2~get_v2_context.
    result = int_context.
  ENDMETHOD.

  METHOD zif_mcp_server_v2~server_discover.
    DATA discover TYPE REF TO zcl_mcp_resp_server_disc.
    DATA versions TYPE string_table.

    discover = NEW zcl_mcp_resp_server_disc( ).

    APPEND zif_mcp_constants=>latest_modern_protocol_version TO versions.

    discover->set_supported_versions( versions ).
    discover->set_capabilities( get_capabilities( ) ).
    discover->set_implementation( get_implementation( ) ).
    discover->set_instructions( get_instructions( ) ).

    discover->zif_mcp_modern_result~set_cache( ttl_ms      = 0
                                               cache_scope = zif_mcp_constants=>cache_scopes-private ).

    TRY.
        response-result = discover->zif_mcp_modern_result~generate_json( ).
      CATCH zcx_mcp_ajson_error INTO DATA(json_error).
        response-error-code    = zcl_mcp_jsonrpc=>error_codes-internal_error.
        response-error-message = json_error->get_text( ).
    ENDTRY.
  ENDMETHOD.

  METHOD zif_mcp_server_v2~prompts_list.
    response = handle_prompts_list( request ).
  ENDMETHOD.

  METHOD zif_mcp_server_v2~prompts_get.
    response = handle_prompts_get( request ).
  ENDMETHOD.

  METHOD zif_mcp_server_v2~resources_list.
    response = handle_resources_list( request ).
  ENDMETHOD.

  METHOD zif_mcp_server_v2~resources_read.
    response = handle_resources_read( request ).
  ENDMETHOD.

  METHOD zif_mcp_server_v2~resources_tmpls_list.
    response = handle_res_tmpls_list( request ).
  ENDMETHOD.

  METHOD zif_mcp_server_v2~tools_list.
    response = handle_tools_list( request ).
  ENDMETHOD.

  METHOD zif_mcp_server_v2~tools_call.
    response = handle_tools_call( request ).
  ENDMETHOD.

  METHOD zif_mcp_server_v2~completions_complete.
    response = handle_completion( request ).
  ENDMETHOD.

  METHOD zif_mcp_server_v2~tasks_get.
    response = handle_tasks_get( request ).
  ENDMETHOD.

  METHOD zif_mcp_server_v2~tasks_cancel.
    response = handle_tasks_cancel( request ).
  ENDMETHOD.

  METHOD zif_mcp_server_v2~tasks_update.
    response = handle_tasks_update( request ).
  ENDMETHOD.

  METHOD zif_mcp_server_v2~get_tool_input_schema.
    result = handle_tool_input_schema( tool_name ).
  ENDMETHOD.

  METHOD get_capabilities.
  ENDMETHOD.

  METHOD get_instructions.
  ENDMETHOD.

  METHOD handle_prompts_list.
    response = method_not_found( 'prompts/list' ).
  ENDMETHOD.

  METHOD handle_prompts_get.
    response = method_not_found( 'prompts/get' ).
  ENDMETHOD.

  METHOD handle_resources_list.
    response = method_not_found( 'resources/list' ).
  ENDMETHOD.

  METHOD handle_resources_read.
    response = method_not_found( 'resources/read' ).
  ENDMETHOD.

  METHOD handle_res_tmpls_list.
    response = method_not_found( 'resources/templates/list' ).
  ENDMETHOD.

  METHOD handle_tools_list.
    response = method_not_found( 'tools/list' ).
  ENDMETHOD.

  METHOD handle_tool_input_schema.
    DATA list_result TYPE zif_mcp_server_v2=>v2_response.
    DATA params      TYPE REF TO zif_mcp_ajson.
    DATA index       TYPE i.
    DATA tool_path   TYPE string.

    params = zcl_mcp_ajson=>create_empty( ).
    list_result = zif_mcp_server_v2~tools_list( NEW zcl_mcp_req_list_tools( params ) ).

    IF    list_result-error-code    IS NOT INITIAL
       OR list_result-error-message IS NOT INITIAL.
      RAISE EXCEPTION NEW zcx_mcp_server(
                              textid = zcx_mcp_server=>invalid_arguments
                              msgv1  = COND #( WHEN list_result-error-message IS NOT INITIAL
                                               THEN CONV #( list_result-error-message )
                                               ELSE CONV #( |tools/list failed while loading tool schema| ) ) ).
    ENDIF.

    IF list_result-result IS NOT BOUND OR list_result-result->exists( '/tools' ) = abap_false.
      RETURN.
    ENDIF.

    index = 1.
    WHILE abap_true = abap_true.
      tool_path = |/tools/{ index }|.

      IF list_result-result->exists( tool_path ) = abap_false.
        EXIT.
      ENDIF.

      IF list_result-result->get_string( |{ tool_path }/name| ) = tool_name.
        IF list_result-result->exists( |{ tool_path }/inputSchema| ).
          result = list_result-result->slice( |{ tool_path }/inputSchema| ).
        ENDIF.
        RETURN.
      ENDIF.

      index = index + 1.
    ENDWHILE.
  ENDMETHOD.

  METHOD handle_tools_call.
    response = method_not_found( 'tools/call' ).
  ENDMETHOD.

  METHOD handle_completion.
    response = method_not_found( 'completion/complete' ).
  ENDMETHOD.

  METHOD handle_tasks_get.
    DATA task_get TYPE REF TO zcl_mcp_resp_v2_task_get.

    TRY.
        DATA(task_id) = request->get_task_id( ).
        DATA(task)    = get_tasks( )->get( CONV #( task_id ) ).

        task_get = NEW zcl_mcp_resp_v2_task_get( ).
        task_get->set_task( task_id          = task-task_id
                            status           = task-status
                            status_message   = task-status_message
                            ttl_ms           = task-ttl
                            poll_interval_ms = task-poll_interval ).

        CASE task-status.
          WHEN zif_mcp_types=>task_states-completed.
            task_get->set_result( get_tasks( )->get_payload( CONV #( task_id ) ) ).

          WHEN zif_mcp_types=>task_states-failed.
            TRY.
                task_get->set_result( get_tasks( )->get_payload( CONV #( task_id ) ) ).
              CATCH zcx_mcp_server.
                task_get->set_error( code    = zcl_mcp_jsonrpc=>error_codes-internal_error
                                     message = COND #( WHEN task-status_message IS NOT INITIAL
                                                       THEN task-status_message
                                                       ELSE |Task { task_id } failed| ) ).
            ENDTRY.

          WHEN zif_mcp_types=>task_states-cancelled.
            task_get->set_error( code    = zcl_mcp_jsonrpc=>error_codes-internal_error
                                 message = |Task { task_id } was cancelled| ).

          WHEN zif_mcp_types=>task_states-input_required.
            DATA(pending) = get_tasks( )->get_payload( CONV #( task_id ) ).

            IF pending->exists( `/requestState` ).
              task_get->set_request_state( pending->get_string( `/requestState` ) ).
            ENDIF.

            IF pending->exists( `/inputRequests` ).
              DATA(keys) = pending->members( `/inputRequests` ).
              LOOP AT keys INTO DATA(key).
                DATA(base_path) = |/inputRequests/{ key }|.
                DATA params TYPE REF TO zif_mcp_ajson.
                params = zcl_mcp_ajson=>create_empty( ).

                IF pending->exists( |{ base_path }/params| ).
                  params = pending->slice( |{ base_path }/params| ).
                ENDIF.

                task_get->add_input_request( request_key = key
                                             method      = pending->get_string( |{ base_path }/method| )
                                             params      = params ).
              ENDLOOP.
            ENDIF.
        ENDCASE.

        response-result = task_get->zif_mcp_modern_result~generate_json( ).

      CATCH zcx_mcp_server INTO DATA(mcp_error).
        response-error-code    = zcl_mcp_jsonrpc=>error_codes-invalid_params.
        response-error-message = mcp_error->get_text( ).

      CATCH zcx_mcp_ajson_error INTO DATA(json_error).
        response-error-code    = zcl_mcp_jsonrpc=>error_codes-internal_error.
        response-error-message = json_error->get_text( ).
    ENDTRY.
  ENDMETHOD.

  METHOD handle_tasks_update.
    DATA ack TYPE REF TO zcl_mcp_resp_v2_ack.

    TRY.
        DATA(task_id) = request->get_task_id( ).
        DATA(task)    = get_tasks( )->get( CONV #( task_id ) ).

        IF task-status <> zif_mcp_types=>task_states-input_required.
          response-error-code    = zcl_mcp_jsonrpc=>error_codes-invalid_params.
          response-error-message = |Task { task_id } is not waiting for input|.
          RETURN.
        ENDIF.

        zcl_mcp_tasks=>consume_update( task_id         = CONV #( task_id )
                                       input_responses = request->get_input_responses( )
                                       request_state   = request->get_request_state( ) ).

        ack = NEW zcl_mcp_resp_v2_ack( ).
        ack->set_complete( ).
        response-result = ack->generate_json( ).

      CATCH zcx_mcp_server INTO DATA(mcp_error).
        response-error-code    = zcl_mcp_jsonrpc=>error_codes-invalid_params.
        response-error-message = mcp_error->get_text( ).

      CATCH zcx_mcp_ajson_error INTO DATA(json_error).
        response-error-code    = zcl_mcp_jsonrpc=>error_codes-internal_error.
        response-error-message = json_error->get_text( ).
    ENDTRY.
  ENDMETHOD.

  METHOD handle_tasks_cancel.
    DATA ack TYPE REF TO zcl_mcp_resp_v2_ack.

    TRY.
        get_tasks( )->get( CONV #( request->get_task_id( ) ) ).
        zcl_mcp_tasks=>cancel( CONV #( request->get_task_id( ) ) ).

        ack = NEW zcl_mcp_resp_v2_ack( ).
        ack->set_complete( ).
        response-result = ack->generate_json( ).

      CATCH zcx_mcp_server INTO DATA(mcp_error).
        response-error-code    = zcl_mcp_jsonrpc=>error_codes-invalid_params.
        response-error-message = mcp_error->get_text( ).

      CATCH zcx_mcp_ajson_error INTO DATA(json_error).
        response-error-code    = zcl_mcp_jsonrpc=>error_codes-internal_error.
        response-error-message = json_error->get_text( ).
    ENDTRY.
  ENDMETHOD.

  METHOD method_not_found.
    response-error-code    = zcl_mcp_jsonrpc=>error_codes-method_not_found.
    response-error-message = |Method { method } not found.|.
  ENDMETHOD.

  METHOD create_request_state.
    IF    int_context-area               IS INITIAL
       OR int_context-server             IS INITIAL
       OR int_context-mcp_request-method IS INITIAL.
      RAISE EXCEPTION NEW zcx_mcp_server( textid = zcx_mcp_server=>internal_error
                                          msgv1  = `Missing v2 context for requestState` ).
    ENDIF.

    result = zcl_mcp_req_state=>create( area        = CONV #( int_context-area )
                                        server      = CONV #( int_context-server )
                                        method      = int_context-mcp_request-method
                                        data        = data
                                        ttl_seconds = ttl_seconds ).
  ENDMETHOD.

  METHOD validate_request_state.
    DATA expected_method TYPE string.

    IF    int_context-area               IS INITIAL
       OR int_context-server             IS INITIAL
       OR int_context-mcp_request-method IS INITIAL.
      RAISE EXCEPTION NEW zcx_mcp_server( textid = zcx_mcp_server=>internal_error
                                          msgv1  = `Missing v2 context for requestState` ).
    ENDIF.

    expected_method = method.
    IF expected_method IS INITIAL.
      expected_method = int_context-mcp_request-method.
    ENDIF.

    result = zcl_mcp_req_state=>validate( request_state = request_state
                                          area          = CONV #( int_context-area )
                                          server        = CONV #( int_context-server )
                                          method        = expected_method ).
  ENDMETHOD.

  METHOD get_tasks.
    IF int_context-area IS INITIAL OR int_context-server IS INITIAL.
      RAISE EXCEPTION NEW zcx_mcp_server( textid = zcx_mcp_server=>internal_error
                                          msgv1  = `Missing v2 context for task manager` ).
    ENDIF.

    IF tasks IS NOT BOUND.
      tasks = NEW zcl_mcp_tasks( area   = int_context-area
                                 server = int_context-server ).
    ENDIF.

    result = tasks.
  ENDMETHOD.

ENDCLASS.
