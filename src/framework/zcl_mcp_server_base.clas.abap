"! <p class="shorttext synchronized">MCP Server Base Logic</p>
CLASS zcl_mcp_server_base DEFINITION ABSTRACT
  PUBLIC
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES zif_mcp_server .

  PROTECTED SECTION.
    "! <p class="shorttext synchronized">Process initialization request</p>
    "! Handles server initialization with client configuration
    "! @parameter request  | <p class="shorttext synchronized">Initialize request object</p>
    "! @parameter response | <p class="shorttext synchronized">Initialize response containing server capabilities</p>
    METHODS handle_initialize ABSTRACT
      IMPORTING !request  TYPE REF TO zcl_mcp_req_initialize
      CHANGING  !response TYPE zif_mcp_server=>initialize_response ##NEEDED.

    "! <p class="shorttext synchronized">Get's the session mode of the server</p>
    "! Mandatory method to be implemented by subclasses. Use zcl_mcp_session=>session_mode_xxxx
    "! @parameter result | <p class="shorttext synchronized">Session Mode</p>
    METHODS get_session_mode ABSTRACT RETURNING VALUE(result) TYPE zmcp_session_mode.

    "! <p class="shorttext synchronized">List available prompts</p>
    "! Handles request to retrieve all prompts available on the server
    "! @parameter request  | <p class="shorttext synchronized">List prompts request object</p>
    "! @parameter response | <p class="shorttext synchronized">Response with available prompts collection</p>
    METHODS handle_list_prompts
      IMPORTING !request  TYPE REF TO zcl_mcp_req_list_prompts
      CHANGING  !response TYPE zif_mcp_server=>list_prompts_response ##NEEDED.

    "! <p class="shorttext synchronized">Get specific prompt details</p>
    "! Retrieves detailed information for a specific prompt by ID
    "! @parameter request  | <p class="shorttext synchronized">Get prompt request with prompt ID</p>
    "! @parameter response | <p class="shorttext synchronized">Response with prompt details</p>
    METHODS handle_get_prompt
      IMPORTING !request  TYPE REF TO zcl_mcp_req_get_prompt
      CHANGING  !response TYPE zif_mcp_server=>get_prompt_response ##NEEDED.

    "! <p class="shorttext synchronized">List available resources</p>
    "! Handles request to retrieve all resources available on the server
    "! @parameter request  | <p class="shorttext synchronized">List resources request object</p>
    "! @parameter response | <p class="shorttext synchronized">Response with available resources collection</p>
    METHODS handle_list_resources
      IMPORTING !request  TYPE REF TO zcl_mcp_req_list_resources
      CHANGING  !response TYPE zif_mcp_server=>list_resources_response ##NEEDED.

    "! <p class="shorttext synchronized">List resource templates</p>
    "! Retrieves available resource templates for resource creation
    "! @parameter request  | <p class="shorttext synchronized">List resource templates request</p>
    "! @parameter response | <p class="shorttext synchronized">Response with available resource templates</p>
    METHODS handle_list_res_tmpls
      IMPORTING !request  TYPE REF TO zcl_mcp_req_list_res_tmpls
      CHANGING  !response TYPE zif_mcp_server=>list_resources_tmpl_response ##NEEDED.

    "! <p class="shorttext synchronized">Read resource content</p>
    "! Retrieves content and metadata for a specific resource
    "! @parameter request  | <p class="shorttext synchronized">Read resource request with resource ID</p>
    "! @parameter response | <p class="shorttext synchronized">Response with resource content and metadata</p>
    METHODS handle_resources_read
      IMPORTING !request  TYPE REF TO zcl_mcp_req_read_resource
      CHANGING  !response TYPE zif_mcp_server=>resources_read_response ##NEEDED.

    "! <p class="shorttext synchronized">List available tools</p>
    "! Handles request to retrieve all tools available on the server
    "! @parameter request  | <p class="shorttext synchronized">List tools request object</p>
    "! @parameter response | <p class="shorttext synchronized">Response with available tools collection</p>
    METHODS handle_list_tools
      IMPORTING !request  TYPE REF TO zcl_mcp_req_list_tools
      CHANGING  !response TYPE zif_mcp_server=>list_tools_response ##NEEDED.

    "! <p class="shorttext synchronized">Execute tool</p>
    "! Handles request to call a specific tool with parameters
    "! @parameter request  | <p class="shorttext synchronized">Call tool request with tool ID and parameters</p>
    "! @parameter response | <p class="shorttext synchronized">Response with tool execution results</p>
    METHODS handle_call_tool
      IMPORTING !request  TYPE REF TO zcl_mcp_req_call_tool
      CHANGING  !response TYPE zif_mcp_server=>call_tool_response ##NEEDED.

    "! <p class="shorttext synchronized">Cancel task - optional hook</p>
    "! Called before the framework marks the task as cancelled.
    "! Override to signal your background process (batch job, RFC) to stop.
    "! Default implementation does nothing.
    "! @parameter request  | <p class="shorttext synchronized">Cancel task request</p>
    "! @parameter response | <p class="shorttext synchronized">Cancel task response</p>
    METHODS handle_cancel_task
      IMPORTING !request  TYPE REF TO zcl_mcp_req_cancel_task
      CHANGING  !response TYPE zif_mcp_server=>cancel_task_response ##NEEDED.

    "! <p class="shorttext synchronized">Provide completion candidates</p>
    "! Override to support argument auto-complete for prompts and resource templates.
    "! Default implementation returns method_not_found.
    "! @parameter request  | Completion request
    "! @parameter response | Completion response
    METHODS handle_completions_complete
      IMPORTING !request  TYPE REF TO zcl_mcp_req_complete
      CHANGING  !response TYPE zif_mcp_server=>complete_response ##NEEDED.

    "! <p class="shorttext synchronized">Access the task manager</p>
    "! Use in handle_call_tool to create tasks:
    "!   DATA(task_id) = get_tasks( )->create_task( tool_name = ... ).
    METHODS get_tasks
      RETURNING VALUE(result) TYPE REF TO zcl_mcp_tasks.

    ALIASES server  FOR zif_mcp_server~server.
    ALIASES config  FOR zif_mcp_server~config.
    ALIASES session FOR zif_mcp_server~session.

  PRIVATE SECTION.
    DATA tasks TYPE REF TO zcl_mcp_tasks.

    METHODS generate_fallback_uuid
      RETURNING VALUE(result) TYPE sysuuid_c32.

    METHODS get_tool_task_support
      IMPORTING tool_name     TYPE string
      RETURNING VALUE(result) TYPE string.

    METHODS enforce_tool_task_negotiation
      IMPORTING !request  TYPE REF TO zcl_mcp_req_call_tool
      CHANGING  !response TYPE zif_mcp_server=>call_tool_response.

    TYPES: BEGIN OF tool_task_support_entry,
             name         TYPE string,
             task_support TYPE string,
           END OF tool_task_support_entry.
    TYPES tool_task_support_cache_t TYPE HASHED TABLE OF tool_task_support_entry WITH UNIQUE KEY name.

    DATA tool_task_support_cache  TYPE tool_task_support_cache_t.
    DATA tool_task_support_loaded TYPE abap_bool.

    METHODS load_tool_task_support_cache.
ENDCLASS.

CLASS zcl_mcp_server_base IMPLEMENTATION.
  METHOD zif_mcp_server~initialize.
          DATA error TYPE REF TO zcx_mcp_server.
    DATA supported_protocol_versions TYPE STANDARD TABLE OF string WITH DEFAULT KEY.
    DATA temp1 LIKE sy-subrc.
      DATA temp2 TYPE zcl_mcp_session=>session_entry.
    CREATE OBJECT response-result TYPE zcl_mcp_resp_initialize.

    " Handle session logic before handing off to the request handler
    IF server-session_id IS NOT INITIAL.
      server-http_response->set_status( code   = 400
                                        reason = 'Bad Request' ) ##NO_TEXT.
      response-error-code    = zcl_mcp_jsonrpc=>error_codes-invalid_request.
      response-error-message = 'Server already initialized for this session' ##NO_TEXT.
      RETURN.
    ENDIF.

    IF server-session_mode <> zcl_mcp_session=>session_mode_stateless.
      TRY.
          server-session_id = cl_system_uuid=>create_uuid_c32_static( ).
        CATCH cx_uuid_error.
          server-session_id = generate_fallback_uuid( ).
      ENDTRY.
      TRY.
          CREATE OBJECT session TYPE zcl_mcp_session EXPORTING session_id = server-session_id session_mode = server-session_mode create_new = abap_true.
          
        CATCH zcx_mcp_server INTO error.
          zif_mcp_server~config->get_logger( )->error( |Failed to create session { error->get_text( ) }| ) ##NO_TEXT.
          server-http_response->set_status( code   = 500
                                            reason = 'Internal Server Error' ) ##NO_TEXT.
          response-error-code    = zcl_mcp_jsonrpc=>error_codes-internal_error.
          response-error-message = error->get_text( ).
          RETURN.
      ENDTRY.
    ENDIF.

    IF server-session_mode = zcl_mcp_session=>session_mode_icf.
      server-http_server->set_session_stateful( ).
    ENDIF.

    " Determine protocol version
    
    SPLIT zif_mcp_constants=>supported_protocol_versions AT `,` INTO TABLE supported_protocol_versions.
    
    READ TABLE supported_protocol_versions WITH KEY table_line = request->get_protocol_version( ) TRANSPORTING NO FIELDS.
    temp1 = sy-subrc.
    IF temp1 = 0.
      server-protocol_version = request->get_protocol_version( ).
    ELSE.
      " If the requested protocol version is not supported, use the latest supported version
      server-protocol_version = zif_mcp_constants=>latest_protocol_version.
    ENDIF.
    response-result->set_protocol_version( server-protocol_version ).

    IF session IS BOUND.
      
      CLEAR temp2.
      temp2-key = 'protocolVersion'.
      temp2-value = server-protocol_version.
      session->add( temp2 ).
    ENDIF.

    handle_initialize( EXPORTING request  = request
                       CHANGING  response = response ).
  ENDMETHOD.

  METHOD zif_mcp_server~prompts_get.
    " Pre-initialize the response object
    CREATE OBJECT response-result TYPE zcl_mcp_resp_get_prompt.

    " Call the handler to modify the response
    handle_get_prompt( EXPORTING request = request
                       CHANGING  response = response ).
  ENDMETHOD.

  METHOD zif_mcp_server~prompts_list.
    " Pre-initialize the response object
    CREATE OBJECT response-result TYPE zcl_mcp_resp_list_prompts.

    " Call the handler to modify the response
    handle_list_prompts( EXPORTING request = request
                         CHANGING  response = response ).
  ENDMETHOD.

  METHOD zif_mcp_server~resources_list.
    " Pre-initialize the response object
    CREATE OBJECT response-result TYPE zcl_mcp_resp_list_resources.

    " Call the handler to modify the response
    handle_list_resources( EXPORTING request = request
                          CHANGING  response = response ).
  ENDMETHOD.

  METHOD zif_mcp_server~resources_read.
    " Pre-initialize the response object
    CREATE OBJECT response-result TYPE zcl_mcp_resp_read_resource.

    " Call the handler to modify the response
    handle_resources_read( EXPORTING request = request
                          CHANGING  response = response ).
  ENDMETHOD.

  METHOD zif_mcp_server~resources_templates_list.
    " Pre-initialize the response object
    CREATE OBJECT response-result TYPE zcl_mcp_resp_list_res_tmpl.

    " Call the handler to modify the response
    handle_list_res_tmpls( EXPORTING request = request
                          CHANGING  response = response ).
  ENDMETHOD.

  METHOD zif_mcp_server~tools_call.
    CREATE OBJECT response-result TYPE zcl_mcp_resp_call_tool.

    enforce_tool_task_negotiation( EXPORTING request  = request
                                   CHANGING  response = response ).

    IF response-error-code IS NOT INITIAL.
      RETURN.
    ENDIF.

    handle_call_tool( EXPORTING request  = request
                      CHANGING  response = response ).
  ENDMETHOD.

  METHOD zif_mcp_server~tools_list.
    " Pre-initialize the response object
    CREATE OBJECT response-result TYPE zcl_mcp_resp_list_tools.

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
    DATA temp3 TYPE i.

    " Get timestamp for uniqueness
    GET TIME STAMP FIELD timestamp.

    " Create random object with timestamp seed
    
    temp3 = timestamp.
    random = cl_abap_random=>create( temp3 ).

    " Generate 32 random hex characters
    DO 32 TIMES.
      " Get random index into hex characters (0-15)
      char_idx = random->intinrange( low  = 0
                                     high = 15 ).
      hex_char = hex_chars+char_idx(1).
      CONCATENATE result hex_char INTO result.
    ENDDO.
  ENDMETHOD.

  METHOD zif_mcp_server~get_session_mode.
    result = get_session_mode( ).
  ENDMETHOD.

  METHOD zif_mcp_server~tasks_list.
        DATA list TYPE zif_mcp_types=>task_list_result.
        DATA error TYPE REF TO zcx_mcp_server.
    CREATE OBJECT response-result TYPE zcl_mcp_resp_list_tasks.
    TRY.
        
        list = get_tasks( )->list( request->get_cursor( ) ).
        response-result->set_tasks( list-tasks ).
        IF list-next_cursor IS NOT INITIAL.
          response-result->set_next_cursor( list-next_cursor ).
        ENDIF.
        
      CATCH zcx_mcp_server INTO error.
        response-error-code    = zcl_mcp_jsonrpc=>error_codes-internal_error.
        response-error-message = error->get_text( ).
    ENDTRY.
  ENDMETHOD.

  METHOD zif_mcp_server~tasks_get.
        DATA temp4 TYPE sysuuid_c32.
        DATA task TYPE zif_mcp_types=>task.
        DATA error TYPE REF TO zcx_mcp_server.
    CREATE OBJECT response-result TYPE zcl_mcp_resp_get_task.
    TRY.
        
        temp4 = request->get_task_id( ).
        
        task = get_tasks( )->get( temp4 ).
        response-result->set_task( task ).
        
      CATCH zcx_mcp_server INTO error.
        response-error-code    = zcl_mcp_jsonrpc=>error_codes-invalid_params.
        response-error-message = error->get_text( ).
    ENDTRY.
  ENDMETHOD.

  METHOD zif_mcp_server~tasks_result.
        DATA task_id TYPE string.
        DATA temp5 TYPE sysuuid_c32.
        DATA task TYPE zif_mcp_types=>task.
            DATA temp6 TYPE sysuuid_c32.
            DATA payload TYPE REF TO zif_mcp_ajson.
                  DATA temp7 TYPE sysuuid_c32.
                  DATA fail_payload TYPE REF TO zif_mcp_ajson.
        DATA error TYPE REF TO zcx_mcp_server.
        DATA json_error TYPE REF TO zcx_mcp_ajson_error.
    CREATE OBJECT response-result TYPE zcl_mcp_resp_task_payload.

    TRY.
        
        task_id = request->get_task_id( ).
        
        temp5 = task_id.
        
        task    = get_tasks( )->get( temp5 ).

        CASE task-status.
          WHEN zcl_mcp_tasks=>status_completed.
            
            temp6 = task_id.
            
            payload = get_tasks( )->get_payload( temp6 ).
            response-result->set_from_json( payload ).
            response-result->set_related_task( task_id ).

            WHEN zcl_mcp_tasks=>status_failed.
              " Return the stored CallToolResult payload when present (isError: true).
              " Fall back to a protocol-level error when no payload was stored
              " (i.e. task was failed directly via zcl_mcp_tasks=>fail).
              TRY.
                  
                  temp7 = task_id.
                  
                  fail_payload = get_tasks( )->get_payload( temp7 ).
                  response-result->set_from_json( fail_payload ).
                  response-result->set_related_task( task_id ).
                CATCH zcx_mcp_server
                      zcx_mcp_ajson_error.
                  response-error-code = zcl_mcp_jsonrpc=>error_codes-internal_error.
                  IF task-status_message IS NOT INITIAL.
                    response-error-message = task-status_message.
                  ELSE.
                    response-error-message = |Task { task_id } failed| ##NO_TEXT.
                  ENDIF.
              ENDTRY.

          WHEN zcl_mcp_tasks=>status_cancelled.
            response-error-code    = zcl_mcp_jsonrpc=>error_codes-internal_error.
            response-error-message = |Task { task_id } was cancelled| ##NO_TEXT.

          WHEN OTHERS.
            response-error-code    = zcl_mcp_jsonrpc=>error_codes-internal_error.
            response-error-message = |Task { task_id } is not complete; poll tasks/get and retry tasks/result once completed| ##NO_TEXT.
        ENDCASE.

        
      CATCH zcx_mcp_server INTO error.
        response-error-code    = zcl_mcp_jsonrpc=>error_codes-invalid_params.
        response-error-message = error->get_text( ).

        
      CATCH zcx_mcp_ajson_error INTO json_error.
        response-error-code    = zcl_mcp_jsonrpc=>error_codes-internal_error.
        response-error-message = json_error->get_text( ).
    ENDTRY.
  ENDMETHOD.

  METHOD zif_mcp_server~tasks_cancel.
        DATA temp8 TYPE sysuuid_c32.
          DATA temp9 TYPE sysuuid_c32.
          DATA temp10 TYPE sysuuid_c32.
          DATA task TYPE zif_mcp_types=>task.
        DATA error TYPE REF TO zcx_mcp_server.
    CREATE OBJECT response-result TYPE zcl_mcp_resp_cancel_task.
    TRY.
        " Ownership guard - raises task_not_found if task belongs to another user
        
        temp8 = request->get_task_id( ).
        get_tasks( )->get( temp8 ).

        " Give subclass a chance to stop the background process first
        handle_cancel_task( EXPORTING request  = request
                            CHANGING  response = response ).

        " Only update status if the hook did not already set an error
        IF response-error-code IS INITIAL.
          
          temp9 = request->get_task_id( ).
          zcl_mcp_tasks=>cancel( temp9 ).
          
          temp10 = request->get_task_id( ).
          
          task = get_tasks( )->get( temp10 ).
          response-result->set_task( task ).
        ENDIF.
        
      CATCH zcx_mcp_server INTO error.
        response-error-code    = zcl_mcp_jsonrpc=>error_codes-invalid_params.
        response-error-message = error->get_text( ).
    ENDTRY.
  ENDMETHOD.

  METHOD get_tasks.
    IF tasks IS NOT BOUND.
      CREATE OBJECT tasks TYPE zcl_mcp_tasks EXPORTING area = server-area server = server-server.
    ENDIF.
    result = tasks.
  ENDMETHOD.

  METHOD handle_cancel_task.
  ENDMETHOD.                                       "#EC EMPTY_PROCEDURE

  METHOD get_tool_task_support.
    DATA cached_support TYPE zcl_mcp_server_base=>tool_task_support_entry.
    result = zcl_mcp_resp_list_tools=>task_support-forbidden.

    load_tool_task_support_cache( ).

    
    READ TABLE tool_task_support_cache INTO cached_support
         WITH TABLE KEY name = tool_name.
    IF sy-subrc = 0 AND cached_support-task_support IS NOT INITIAL.
      result = cached_support-task_support.
    ENDIF.
  ENDMETHOD.

  METHOD enforce_tool_task_negotiation.
    DATA task_support TYPE string.
    task_support = get_tool_task_support( request->get_name( ) ).

    IF request->has_task( ) = abap_true.
      IF task_support = zcl_mcp_resp_list_tools=>task_support-forbidden.
        response-error-code    = zcl_mcp_jsonrpc=>error_codes-method_not_found.
        response-error-message = |Tool { request->get_name( ) } does not support task execution| ##NO_TEXT.
      ENDIF.
      RETURN.
    ENDIF.

    IF task_support = zcl_mcp_resp_list_tools=>task_support-required.
      response-error-code    = zcl_mcp_jsonrpc=>error_codes-invalid_params.
      response-error-message = |Tool { request->get_name( ) } requires task execution| ##NO_TEXT.
    ENDIF.
  ENDMETHOD.

  METHOD zif_mcp_server~completions_complete.
    CREATE OBJECT response-result TYPE zcl_mcp_resp_complete.
    handle_completions_complete( EXPORTING request  = request
                                 CHANGING  response = response ).
  ENDMETHOD.

  METHOD handle_completions_complete.
    response-error-code    = zcl_mcp_jsonrpc=>error_codes-method_not_found.
    response-error-message = 'This server does not implement completions' ##NO_TEXT.
  ENDMETHOD.

  METHOD load_tool_task_support_cache.
    DATA cursor TYPE zif_mcp_types=>page_cursor.
          DATA params TYPE REF TO zcl_mcp_ajson.
          DATA list_request TYPE REF TO zcl_mcp_req_list_tools.
          DATA temp11 TYPE zif_mcp_server=>list_tools_response.
          DATA list_response LIKE temp11.
          DATA tools TYPE zcl_mcp_resp_list_tools=>tools.
          DATA tool LIKE LINE OF tools.
            DATA temp12 TYPE string.
            DATA task_support LIKE temp12.
            FIELD-SYMBOLS <cached_support> TYPE zcl_mcp_server_base=>tool_task_support_entry.
              DATA temp13 TYPE zcl_mcp_server_base=>tool_task_support_entry.
          DATA next_cursor TYPE zif_mcp_types=>page_cursor.
    IF tool_task_support_loaded = abap_true.
      RETURN.
    ENDIF.

    CLEAR tool_task_support_cache.

    

    TRY.
        DO.
          
          params = zcl_mcp_ajson=>create_empty( ).
          IF cursor IS NOT INITIAL.
            params->set( iv_path = '/cursor'
                         iv_val  = cursor ).
          ENDIF.

          
          CREATE OBJECT list_request TYPE zcl_mcp_req_list_tools EXPORTING JSON = params.
          
          CLEAR temp11.
          CREATE OBJECT temp11-result TYPE zcl_mcp_resp_list_tools.
          
          list_response = temp11.

          handle_list_tools( EXPORTING request  = list_request
                             CHANGING  response = list_response ).

          IF    list_response-error-code IS NOT INITIAL
             OR list_response-result     IS NOT BOUND.
            EXIT.
          ENDIF.

          
          tools = list_response-result->get_tools( ).
          
          LOOP AT tools INTO tool.
            
            IF tool-execution-task_support IS NOT INITIAL.
              temp12 = tool-execution-task_support.
            ELSE.
              temp12 = zcl_mcp_resp_list_tools=>task_support-forbidden.
            ENDIF.
            
            task_support = temp12.

            
            READ TABLE tool_task_support_cache WITH KEY name = tool-name ASSIGNING <cached_support>.
            IF sy-subrc = 0.
              <cached_support>-task_support = task_support.
            ELSE.
              
              CLEAR temp13.
              temp13-name = tool-name.
              temp13-task_support = task_support.
              INSERT temp13 INTO TABLE tool_task_support_cache.
            ENDIF.
          ENDLOOP.

          
          next_cursor = list_response-result->get_next_cursor( ).
          IF next_cursor IS INITIAL OR next_cursor = cursor.
            EXIT.
          ENDIF.
          cursor = next_cursor.
        ENDDO.
      CATCH zcx_mcp_ajson_error
            zcx_mcp_server.
        CLEAR tool_task_support_cache.
    ENDTRY.

    tool_task_support_loaded = abap_true.
  ENDMETHOD.

ENDCLASS.
