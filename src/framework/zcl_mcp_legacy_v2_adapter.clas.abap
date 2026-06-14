"! <p class="shorttext synchronized">Legacy client adapter for v2-only servers</p>
"! Translates SDK-supported legacy stateless requests to the draft v2 server API.
CLASS zcl_mcp_legacy_v2_adapter DEFINITION
  PUBLIC FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    "! <p class="shorttext synchronized">Process legacy request for v2 server</p>
    CLASS-METHODS process_request
      IMPORTING server        TYPE REF TO zif_mcp_server_v2
                !request      TYPE zcl_mcp_jsonrpc=>request
                !area         TYPE zmcp_area
                servername    TYPE zmcp_server
                http_request  TYPE REF TO if_http_request
                http_response TYPE REF TO if_http_response
                http_server   TYPE REF TO if_http_server
                cors_mode     TYPE zmcp_conf_cors
      RETURNING VALUE(result) TYPE string
      RAISING   zcx_mcp_ajson_error.

  PRIVATE SECTION.
    CLASS-METHODS route_request
      IMPORTING server          TYPE REF TO zif_mcp_server_v2
                !request        TYPE zcl_mcp_jsonrpc=>request
                !area           TYPE zmcp_area
                servername      TYPE zmcp_server
                http_request    TYPE REF TO if_http_request
                http_response   TYPE REF TO if_http_response
                http_server     TYPE REF TO if_http_server
                cors_mode       TYPE zmcp_conf_cors
      RETURNING VALUE(response) TYPE zcl_mcp_jsonrpc=>response
      RAISING   zcx_mcp_ajson_error.

    CLASS-METHODS set_context
      IMPORTING server              TYPE REF TO zif_mcp_server_v2
                !request            TYPE zcl_mcp_jsonrpc=>request
                legacy_protocol_ver TYPE string
                !area               TYPE zmcp_area
                servername          TYPE zmcp_server
                http_request        TYPE REF TO if_http_request
                http_response       TYPE REF TO if_http_response
                http_server         TYPE REF TO if_http_server
                cors_mode           TYPE zmcp_conf_cors.

    CLASS-METHODS legacy_protocol
      IMPORTING !request      TYPE zcl_mcp_jsonrpc=>request
                http_request  TYPE REF TO if_http_request
      RETURNING VALUE(result) TYPE string.

    CLASS-METHODS is_supported_legacy_version
      IMPORTING !version      TYPE string
      RETURNING VALUE(result) TYPE abap_bool.

    CLASS-METHODS error_response
      IMPORTING !request        TYPE zcl_mcp_jsonrpc=>request
                !code           TYPE i
                !message        TYPE string
      RETURNING VALUE(response) TYPE zcl_mcp_jsonrpc=>response.

    CLASS-METHODS initialize_response
      IMPORTING server          TYPE REF TO zif_mcp_server_v2
                !request        TYPE zcl_mcp_jsonrpc=>request
      RETURNING VALUE(response) TYPE zcl_mcp_jsonrpc=>response
      RAISING   zcx_mcp_ajson_error.

    CLASS-METHODS dispatch_v2
      IMPORTING server              TYPE REF TO zif_mcp_server_v2
                !request            TYPE zcl_mcp_jsonrpc=>request
                legacy_protocol_ver TYPE string
                !area               TYPE zmcp_area
                servername          TYPE zmcp_server
      RETURNING VALUE(response)     TYPE zcl_mcp_jsonrpc=>response
      RAISING   zcx_mcp_ajson_error
                zcx_mcp_server.

    CLASS-METHODS apply_v2_response
      IMPORTING v2_response     TYPE zif_mcp_server_v2=>v2_response
                !request        TYPE zcl_mcp_jsonrpc=>request
      RETURNING VALUE(response) TYPE zcl_mcp_jsonrpc=>response
      RAISING   zcx_mcp_ajson_error
                zcx_mcp_server.

    CLASS-METHODS strip_v2_envelope
      IMPORTING !json         TYPE REF TO zif_mcp_ajson
      RETURNING VALUE(result) TYPE REF TO zif_mcp_ajson
      RAISING   zcx_mcp_ajson_error.

    CLASS-METHODS list_tasks
      IMPORTING !request      TYPE zcl_mcp_jsonrpc=>request
                !area         TYPE zmcp_area
                servername    TYPE zmcp_server
      RETURNING VALUE(result) TYPE REF TO zif_mcp_ajson
      RAISING   zcx_mcp_ajson_error
                zcx_mcp_server.

    CLASS-METHODS task_result
      IMPORTING server          TYPE REF TO zif_mcp_server_v2
                !request        TYPE zcl_mcp_jsonrpc=>request
      RETURNING VALUE(response) TYPE zcl_mcp_jsonrpc=>response
      RAISING   zcx_mcp_ajson_error
                zcx_mcp_server.

    CLASS-METHODS task_from_v2_result
      IMPORTING v2_result     TYPE REF TO zif_mcp_ajson
                !nested       TYPE abap_bool DEFAULT abap_true
      RETURNING VALUE(result) TYPE REF TO zif_mcp_ajson
      RAISING   zcx_mcp_ajson_error
                zcx_mcp_server.

    CLASS-METHODS legacy_task_from_v2
      IMPORTING v2_result     TYPE REF TO zif_mcp_ajson
      RETURNING VALUE(result) TYPE zif_mcp_types=>task
      RAISING   zcx_mcp_server.

    CLASS-METHODS translate_legacy_task_update
      IMPORTING !request      TYPE zcl_mcp_jsonrpc=>request
                !area         TYPE zmcp_area
                servername    TYPE zmcp_server
      RETURNING VALUE(result) TYPE REF TO zif_mcp_ajson
      RAISING   zcx_mcp_ajson_error
                zcx_mcp_server.
ENDCLASS.


CLASS zcl_mcp_legacy_v2_adapter IMPLEMENTATION.
  METHOD process_request.
    DATA jsonrpc  TYPE REF TO zcl_mcp_jsonrpc.
    DATA response TYPE zcl_mcp_jsonrpc=>response.

    jsonrpc = NEW zcl_mcp_jsonrpc( ).

    response = route_request( server        = server
                              request       = request
                              area          = area
                              servername    = servername
                              http_request  = http_request
                              http_response = http_response
                              http_server   = http_server
                              cors_mode     = cors_mode ).

    result = jsonrpc->serialize_response( response ).
  ENDMETHOD.

  METHOD route_request.
    DATA protocol_ver TYPE string.

    IF server IS NOT BOUND.
      response = error_response( request = request
                                 code    = zcl_mcp_jsonrpc=>error_codes-internal_error
                                 message = 'Draft MCP server is not available' ) ##NO_TEXT.
      RETURN.
    ENDIF.

    protocol_ver = legacy_protocol( request      = request
                                    http_request = http_request ).

    set_context( server              = server
                 request             = request
                 legacy_protocol_ver = protocol_ver
                 area                = area
                 servername          = servername
                 http_request        = http_request
                 http_response       = http_response
                 http_server         = http_server
                 cors_mode           = cors_mode ).

    TRY.
        IF request-method = 'initialize'.
          response = initialize_response( server  = server
                                          request = request ).
          RETURN.
        ENDIF.

        IF is_supported_legacy_version( protocol_ver ) = abap_false.
          response = error_response( request = request
                                     code    = zcl_mcp_jsonrpc=>error_codes-invalid_request
                                     message = |Unsupported Mcp-Protocol-Version { protocol_ver }| ) ##NO_TEXT.
          RETURN.
        ENDIF.

        response = dispatch_v2( server              = server
                                request             = request
                                legacy_protocol_ver = protocol_ver
                                area                = area
                                servername          = servername ).

      CATCH zcx_mcp_server INTO DATA(mcp_error).
        response = error_response( request = request
                                   code    = zcl_mcp_jsonrpc=>error_codes-invalid_params
                                   message = mcp_error->get_text( ) ).

      CATCH zcx_mcp_ajson_error INTO DATA(json_error).
        response = error_response( request = request
                                   code    = zcl_mcp_jsonrpc=>error_codes-invalid_params
                                   message = json_error->get_text( ) ).
    ENDTRY.
  ENDMETHOD.

  METHOD set_context.
    DATA context TYPE zif_mcp_server_v2=>v2_context.

    context-area          = area.
    context-server        = servername.
    context-mcp_request   = request.
    context-protocol_ver  = legacy_protocol_ver.
    context-http_request  = http_request.
    context-http_response = http_response.
    context-http_server   = http_server.
    context-cors_mode     = cors_mode.
    context-meta          = zcl_mcp_ajson=>create_empty( ).
    context-client_info   = zcl_mcp_ajson=>create_empty( ).
    context-client_caps   = zcl_mcp_ajson=>create_empty( ).
    context-extensions    = zcl_mcp_ajson=>create_empty( ).

    TRY.
        context-client_caps->touch_object( '/extensions' ).
        context-client_caps->touch_object( '/extensions/io.modelcontextprotocol~1tasks' ).
        context-extensions->touch_object( '/io.modelcontextprotocol~1tasks' ).
      CATCH zcx_mcp_ajson_error.
        CLEAR context-extensions.
        context-extensions = zcl_mcp_ajson=>create_empty( ).
    ENDTRY.

    server->set_v2_context( context ).
  ENDMETHOD.

  METHOD legacy_protocol.
    IF request-method = 'initialize' AND request-params IS BOUND.
      result = request-params->get_string( '/protocolVersion' ).
    ENDIF.

    IF result IS INITIAL AND http_request IS BOUND.
      result = http_request->get_header_field( zif_mcp_constants=>header_names-protocol_version ).
    ENDIF.

    IF result IS INITIAL.
      result = zif_mcp_constants=>protocol_version_2025_03_26.
    ENDIF.
  ENDMETHOD.

  METHOD is_supported_legacy_version.
    SPLIT zif_mcp_constants=>legacy_protocol_versions AT `,` INTO TABLE DATA(versions).
    result = xsdbool( line_exists( versions[ table_line = version ] ) ).
  ENDMETHOD.

  METHOD error_response.
    response-jsonrpc    = zcl_mcp_jsonrpc=>jsonrpc_version.
    response-id         = request-id.
    response-id_present = request-id_present.
    response-error-code    = code.
    response-error-message = message.
  ENDMETHOD.

  METHOD initialize_response.
    DATA requested_version TYPE string.
    DATA effective_version TYPE string.
    DATA discover          TYPE zif_mcp_server_v2=>v2_response.
    DATA result            TYPE REF TO zif_mcp_ajson.

    IF request-params IS BOUND.
      requested_version = request-params->get_string( '/protocolVersion' ).
    ENDIF.

    IF requested_version IS INITIAL.
      requested_version = zif_mcp_constants=>protocol_version_2025_03_26.
    ENDIF.

    IF is_supported_legacy_version( requested_version ) = abap_false.
      response = error_response( request = request
                                 code    = zcl_mcp_jsonrpc=>error_codes-invalid_request
                                 message = |Unsupported protocolVersion { requested_version }| ) ##NO_TEXT.
      RETURN.
    ENDIF.

    effective_version = requested_version.

    discover = server->server_discover( ).

    IF discover-error-code IS NOT INITIAL OR discover-error-message IS NOT INITIAL.
      response = error_response( request = request
                                 code    = discover-error-code
                                 message = discover-error-message ).
      RETURN.
    ENDIF.

    result = zcl_mcp_ajson=>create_empty( ).
    result->set_string( iv_path = '/protocolVersion'
                        iv_val  = effective_version ).

    result->touch_object( '/capabilities' ).

    IF discover-result IS BOUND.
      IF discover-result->exists( '/capabilities/prompts' ).
        result->touch_object( '/capabilities/prompts' ).
      ENDIF.
      IF discover-result->exists( '/capabilities/resources' ).
        result->touch_object( '/capabilities/resources' ).
      ENDIF.
      IF discover-result->exists( '/capabilities/tools' ).
        result->touch_object( '/capabilities/tools' ).
      ENDIF.
      IF discover-result->exists( '/capabilities/completions' ).
        result->touch_object( '/capabilities/completions' ).
      ENDIF.

      IF     effective_version >= zif_mcp_constants=>protocol_version_2025_11_25
         AND discover-result->exists( '/capabilities/extensions/io.modelcontextprotocol~1tasks' ).
        result->touch_object( '/capabilities/tasks' ).
        result->touch_object( '/capabilities/tasks/list' ).
        result->touch_object( '/capabilities/tasks/cancel' ).
        result->touch_object( '/capabilities/tasks/requests/tools/call' ).
      ENDIF.

      IF discover-result->exists( '/serverInfo' ).
        result->set( iv_path = '/serverInfo'
                     iv_val  = discover-result->slice( '/serverInfo' ) ).
      ENDIF.

      IF discover-result->exists( '/instructions' ).
        result->set_string( iv_path = '/instructions'
                            iv_val  = discover-result->get_string( '/instructions' ) ).
      ENDIF.
    ENDIF.

    response-jsonrpc    = zcl_mcp_jsonrpc=>jsonrpc_version.
    response-id         = request-id.
    response-id_present = request-id_present.
    response-result     = result.
  ENDMETHOD.

  METHOD dispatch_v2.
    " TODO: parameter LEGACY_PROTOCOL_VER is never used (ABAP cleaner)

    DATA v2_response TYPE zif_mcp_server_v2=>v2_response.
    DATA result      TYPE REF TO zif_mcp_ajson.

    CASE request-method.
      WHEN 'ping'.
        response-jsonrpc    = zcl_mcp_jsonrpc=>jsonrpc_version.
        response-id         = request-id.
        response-id_present = request-id_present.
        response-result     = zcl_mcp_ajson=>create_empty( ).
        response-result->touch_object( '' ).
        RETURN.

      WHEN 'prompts/list'.
        v2_response = server->prompts_list( NEW zcl_mcp_req_list_prompts( request-params ) ).

      WHEN 'prompts/get'.
        v2_response = server->prompts_get( NEW zcl_mcp_req_get_prompt( request-params ) ).

      WHEN 'resources/list'.
        v2_response = server->resources_list( NEW zcl_mcp_req_list_resources( request-params ) ).

      WHEN 'resources/read'.
        v2_response = server->resources_read( NEW zcl_mcp_req_read_resource( request-params ) ).

      WHEN 'resources/templates/list'.
        v2_response = server->resources_tmpls_list( NEW zcl_mcp_req_list_res_tmpls( request-params ) ).

      WHEN 'tools/list'.
        v2_response = server->tools_list( NEW zcl_mcp_req_list_tools( request-params ) ).

      WHEN 'tools/call'.
        v2_response = server->tools_call( NEW zcl_mcp_req_call_tool( request-params ) ).

      WHEN 'completion/complete'.
        v2_response = server->completions_complete( NEW zcl_mcp_req_complete( request-params ) ).

      WHEN 'tasks/list'.
        result = list_tasks( request    = request
                             area       = area
                             servername = servername ).

        response-jsonrpc    = zcl_mcp_jsonrpc=>jsonrpc_version.
        response-id         = request-id.
        response-id_present = request-id_present.
        response-result     = result.
        RETURN.

      WHEN 'tasks/get'.
        v2_response = server->tasks_get( NEW zcl_mcp_req_get_task( request-params ) ).

      WHEN 'tasks/result'.
        response = task_result( server  = server
                                request = request ).
        RETURN.

      WHEN 'tasks/cancel'.
        DATA(cancel_request) = NEW zcl_mcp_req_cancel_task( request-params ).

        v2_response = server->tasks_cancel( cancel_request ).

        response-jsonrpc    = zcl_mcp_jsonrpc=>jsonrpc_version.
        response-id         = request-id.
        response-id_present = request-id_present.

        IF v2_response-error-code IS NOT INITIAL OR v2_response-error-message IS NOT INITIAL.
          response-error = v2_response-error.
          RETURN.
        ENDIF.

        DATA(tasks) = NEW zcl_mcp_tasks( area   = area
                                         server = servername ).

        DATA(cancelled_task) = tasks->get( CONV #( cancel_request->get_task_id( ) ) ).

        DATA(cancel_response) = NEW zcl_mcp_resp_cancel_task( ).
        cancel_response->set_task( cancelled_task ).

        response-result = cancel_response->zif_mcp_internal~generate_json( ).
        RETURN.

      WHEN 'tasks/update'.
        DATA(update_params) = translate_legacy_task_update( request    = request
                                                            area       = area
                                                            servername = servername ).

        v2_response = server->tasks_update( NEW zcl_mcp_req_update_task( update_params ) ).

      WHEN OTHERS.
        response = error_response( request = request
                                   code    = zcl_mcp_jsonrpc=>error_codes-method_not_found
                                   message = |Method { request-method } not found.| ) ##NO_TEXT.
        RETURN.
    ENDCASE.

    response = apply_v2_response( v2_response = v2_response
                                  request     = request ).
  ENDMETHOD.

  METHOD apply_v2_response.
    DATA result_type TYPE string.

    response-jsonrpc    = zcl_mcp_jsonrpc=>jsonrpc_version.
    response-id         = request-id.
    response-id_present = request-id_present.

    IF v2_response-error-code IS NOT INITIAL OR v2_response-error-message IS NOT INITIAL.
      response-error = v2_response-error.
      RETURN.
    ENDIF.

    IF v2_response-result IS NOT BOUND.
      response = error_response( request = request
                                 code    = zcl_mcp_jsonrpc=>error_codes-internal_error
                                 message = |Method { request-method } did not return a result.| ) ##NO_TEXT.
      RETURN.
    ENDIF.

    result_type = v2_response-result->get_string( '/resultType' ).

    CASE result_type.
      WHEN zif_mcp_constants=>result_types-input_required.
        response = error_response(
                       request = request
                       code    = zcl_mcp_jsonrpc=>error_codes-invalid_request
                       message = 'This v2 result requires client input and cannot be represented for legacy clients.' ) ##NO_TEXT.
        RETURN.

      WHEN zif_mcp_constants=>result_types-task.
        response-result = task_from_v2_result( v2_result = v2_response-result
                                               nested    = abap_true ).

      WHEN OTHERS.
        IF     request-method = 'tasks/get'
           AND v2_response-result->get_string( '/task/status' ) = zif_mcp_types=>task_states-input_required.
          response = error_response(
              request = request
              code    = zcl_mcp_jsonrpc=>error_codes-invalid_params
              message = 'Task is waiting for v2 client input; legacy clients cannot provide tasks/update input.' ) ##NO_TEXT.
          RETURN.
        ENDIF.

        IF request-method = 'tasks/get'.
          response-result = task_from_v2_result( v2_result = v2_response-result
                                                 nested    = abap_false ).
        ELSE.
          response-result = strip_v2_envelope( v2_response-result ).
        ENDIF.

    ENDCASE.
  ENDMETHOD.

  METHOD strip_v2_envelope.
    result = json->clone( ).

    IF result->exists( '/resultType' ).
      result->delete( '/resultType' ).
    ENDIF.
    IF result->exists( '/ttlMs' ).
      result->delete( '/ttlMs' ).
    ENDIF.
    IF result->exists( '/cacheScope' ).
      result->delete( '/cacheScope' ).
    ENDIF.
  ENDMETHOD.

  METHOD list_tasks.
    DATA list_response TYPE REF TO zcl_mcp_resp_list_tasks.
    DATA list_request  TYPE REF TO zcl_mcp_req_list_tasks.
    DATA list          TYPE zif_mcp_types=>task_list_result.

    list_request = NEW zcl_mcp_req_list_tasks( request-params ).

    DATA(tasks) = NEW zcl_mcp_tasks( area   = area
                                     server = servername ).

    list = tasks->list( list_request->get_cursor( ) ).

    DELETE list-tasks WHERE status = zcl_mcp_tasks=>status_input_required.

    list_response = NEW zcl_mcp_resp_list_tasks( ).
    list_response->set_tasks( list-tasks ).

    IF list-next_cursor IS NOT INITIAL.
      list_response->set_next_cursor( list-next_cursor ).
    ENDIF.

    result = list_response->zif_mcp_internal~generate_json( ).
  ENDMETHOD.

  METHOD task_result.
    DATA get_response TYPE zif_mcp_server_v2=>v2_response.
    DATA status       TYPE string.

    get_response = server->tasks_get( NEW zcl_mcp_req_get_task( request-params ) ).

    response-jsonrpc    = zcl_mcp_jsonrpc=>jsonrpc_version.
    response-id         = request-id.
    response-id_present = request-id_present.

    IF get_response-error-code IS NOT INITIAL OR get_response-error-message IS NOT INITIAL.
      response-error = get_response-error.
      RETURN.
    ENDIF.

    IF get_response-result IS NOT BOUND.
      response = error_response( request = request
                                 code    = zcl_mcp_jsonrpc=>error_codes-internal_error
                                 message = 'tasks/get did not return a result.' ) ##NO_TEXT.
      RETURN.
    ENDIF.

    status = get_response-result->get_string( '/task/status' ).

    CASE status.
      WHEN zcl_mcp_tasks=>status_completed.
        IF get_response-result->exists( '/result' ).
          response-result = get_response-result->slice( '/result' ).
        ELSE.
          response = error_response( request = request
                                     code    = zcl_mcp_jsonrpc=>error_codes-internal_error
                                     message = 'Completed task has no result payload.' ) ##NO_TEXT.
        ENDIF.

      WHEN zcl_mcp_tasks=>status_failed.
        IF get_response-result->exists( '/error' ).
          response-error-code    = get_response-result->get_integer( '/error/code' ).
          response-error-message = get_response-result->get_string( '/error/message' ).
          IF get_response-result->exists( '/error/data' ).
            response-error-data = get_response-result->slice( '/error/data' ).
          ENDIF.
        ELSE.
          response = error_response( request = request
                                     code    = zcl_mcp_jsonrpc=>error_codes-internal_error
                                     message = 'Task failed.' ) ##NO_TEXT.
        ENDIF.

      WHEN zif_mcp_types=>task_states-input_required.
        response = error_response(
            request = request
            code    = zcl_mcp_jsonrpc=>error_codes-invalid_params
            message = 'Task is waiting for v2 client input; legacy clients cannot provide tasks/update input.' ) ##NO_TEXT.

      WHEN zcl_mcp_tasks=>status_cancelled.
        response = error_response( request = request
                                   code    = zcl_mcp_jsonrpc=>error_codes-internal_error
                                   message = 'Task was cancelled.' ) ##NO_TEXT.

      WHEN OTHERS.
        response = error_response(
                       request = request
                       code    = zcl_mcp_jsonrpc=>error_codes-internal_error
                       message = 'Task is not complete; poll tasks/get and retry tasks/result once completed.' ) ##NO_TEXT.
    ENDCASE.
  ENDMETHOD.

  METHOD legacy_task_from_v2.
    DATA now TYPE timestamp.

    IF v2_result IS NOT BOUND OR v2_result->exists( '/task' ) = abap_false.
      RAISE EXCEPTION NEW zcx_mcp_server( textid = zcx_mcp_server=>invalid_arguments
                                          msgv1  = CONV #( 'Missing v2 task object' ) ) ##NO_TEXT.
    ENDIF.

    result-task_id        = v2_result->get_string( '/task/taskId' ).
    result-status         = v2_result->get_string( '/task/status' ).
    result-status_message = v2_result->get_string( '/task/statusMessage' ).

    IF result-task_id IS INITIAL OR result-status IS INITIAL.
      RAISE EXCEPTION NEW zcx_mcp_server( textid = zcx_mcp_server=>invalid_arguments
                                          msgv1  = CONV #( 'Invalid v2 task object' ) ) ##NO_TEXT.
    ENDIF.

    GET TIME STAMP FIELD now.
    result-created_at   = now.
    result-last_updated = now.

    IF v2_result->exists( '/task/ttlMs' ).
      result-ttl         = v2_result->get_integer( '/task/ttlMs' ).
      result-ttl_is_null = abap_false.
    ELSE.
      result-ttl_is_null = abap_true.
    ENDIF.

    IF v2_result->exists( '/task/pollIntervalMs' ).
      result-poll_interval = v2_result->get_integer( '/task/pollIntervalMs' ).
    ENDIF.
  ENDMETHOD.

  METHOD task_from_v2_result.
    DATA task TYPE zif_mcp_types=>task.

    task = legacy_task_from_v2( v2_result ).

    IF task-status = zif_mcp_types=>task_states-input_required.
      IF nested = abap_true.
        task-status = zif_mcp_types=>task_states-working.
      ELSE.
        RAISE EXCEPTION NEW zcx_mcp_server( textid = zcx_mcp_server=>invalid_arguments
                                            msgv1  = CONV #( 'Task requires v2 tasks/update input' ) ) ##NO_TEXT.
      ENDIF.
    ENDIF.

    IF nested = abap_true.
      DATA(create_response) = NEW zcl_mcp_resp_create_task( ).
      create_response->set_task( task ).
      result = create_response->zif_mcp_internal~generate_json( ).
    ELSE.
      DATA(get_response) = NEW zcl_mcp_resp_get_task( ).
      get_response->set_task( task ).
      result = get_response->zif_mcp_internal~generate_json( ).
    ENDIF.
  ENDMETHOD.

  METHOD translate_legacy_task_update.
    DATA task_id   TYPE string.
    DATA candidate TYPE REF TO zif_mcp_ajson.
    DATA confirm   TYPE REF TO zif_mcp_ajson.
    DATA pending   TYPE REF TO zif_mcp_ajson.

    IF request-params IS NOT BOUND.
      RAISE EXCEPTION NEW zcx_mcp_server( textid = zcx_mcp_server=>required_params
                                          msgv1  = 'params' ).
    ENDIF.

    IF request-params->exists( '/taskId' ) = abap_false.
      RAISE EXCEPTION NEW zcx_mcp_server( textid = zcx_mcp_server=>required_params
                                          msgv1  = 'taskId' ).
    ENDIF.

    task_id = request-params->get_string( '/taskId' ).

    result = zcl_mcp_ajson=>create_empty( ).
    result->set_string( iv_path = '/taskId'
                        iv_val  = task_id ).

    IF request-params->exists( '/inputResponses' ).
      result->set( iv_path = '/inputResponses'
                   iv_val  = request-params->slice( '/inputResponses' ) ).
    ELSE.
      IF request-params->exists( '/input' ).
        candidate = request-params->slice( '/input' ).
      ELSEIF request-params->exists( '/response' ).
        candidate = request-params->slice( '/response' ).
      ELSEIF request-params->exists( '/inputResponse' ).
        candidate = request-params->slice( '/inputResponse' ).
      ELSE.
        candidate = request-params->clone( ).
        IF candidate->exists( '/taskId' ).
          candidate->delete( '/taskId' ).
        ENDIF.
        IF candidate->exists( '/requestState' ).
          candidate->delete( '/requestState' ).
        ENDIF.
        IF candidate->exists( '/_meta' ).
          candidate->delete( '/_meta' ).
        ENDIF.
      ENDIF.

      IF candidate->exists( '/action' ) OR candidate->exists( '/content' ).
        result->set( iv_path = '/inputResponses/confirm'
                     iv_val  = candidate ).
      ELSE.
        confirm = zcl_mcp_ajson=>create_empty( ).
        confirm->set_string( iv_path = '/action'
                             iv_val  = zcl_mcp_elicit_result=>actions-accept ).
        confirm->set( iv_path = '/content'
                      iv_val  = candidate ).

        result->set( iv_path = '/inputResponses/confirm'
                     iv_val  = confirm ).
      ENDIF.
    ENDIF.

    IF request-params->exists( '/requestState' ).
      result->set_string( iv_path = '/requestState'
                          iv_val  = request-params->get_string( '/requestState' ) ).
    ELSE.
      DATA(tasks) = NEW zcl_mcp_tasks( area   = area
                                       server = servername ).

      pending = tasks->get_payload( CONV #( task_id ) ).

      IF pending->exists( '/requestState' ).
        result->set_string( iv_path = '/requestState'
                            iv_val  = pending->get_string( '/requestState' ) ).
      ENDIF.
    ENDIF.

    IF request-params->exists( '/_meta' ).
      result->set( iv_path = '/_meta'
                   iv_val  = request-params->slice( '/_meta' ) ).
    ENDIF.
  ENDMETHOD.
ENDCLASS.
