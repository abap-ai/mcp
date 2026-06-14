  CLASS ltcl_mcp_modern_router DEFINITION DEFERRED.
  CLASS zcl_mcp_modern_router DEFINITION LOCAL FRIENDS ltcl_mcp_modern_router.

  CLASS lcl_router_server DEFINITION FINAL.
    PUBLIC SECTION.
      INTERFACES zif_mcp_server_v2.

      DATA discover_called     TYPE abap_bool.
      DATA tools_list_called   TYPE abap_bool.
      DATA tasks_update_called TYPE abap_bool.
      DATA update_params       TYPE REF TO zcl_mcp_req_update_task.
  ENDCLASS.


  CLASS lcl_router_server IMPLEMENTATION.
    METHOD zif_mcp_server_v2~set_v2_context.
    ENDMETHOD.

    METHOD zif_mcp_server_v2~get_v2_context.
    ENDMETHOD.


    METHOD zif_mcp_server_v2~get_tool_input_schema.
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

    METHOD zif_mcp_server_v2~server_discover.
      discover_called = abap_true.

      TRY.
          response-result = zcl_mcp_ajson=>create_empty( ).
          response-result->set_string( iv_path = '/resultType'
                                       iv_val  = zif_mcp_constants=>result_types-complete ).
          response-result->touch_array( '/supportedVersions' ).
          response-result->set_string( iv_path = '/supportedVersions/1'
                                       iv_val  = zif_mcp_constants=>latest_modern_protocol_version ).
          response-result->touch_object( '/capabilities' ).
          response-result->set_string( iv_path = '/serverInfo/name'
                                       iv_val  = 'Router Test Server' ).
          response-result->set_string( iv_path = '/serverInfo/version'
                                       iv_val  = '1.0.0' ).
          response-result->set_integer( iv_path = '/ttlMs'
                                        iv_val  = 0 ).
          response-result->set_string( iv_path = '/cacheScope'
                                       iv_val  = zif_mcp_constants=>cache_scopes-private ).
        CATCH zcx_mcp_ajson_error INTO DATA(error).
          response-error-code    = zcl_mcp_jsonrpc=>error_codes-internal_error.
          response-error-message = error->get_text( ).
      ENDTRY.
    ENDMETHOD.

    METHOD zif_mcp_server_v2~prompts_list.
      response-error-code    = zcl_mcp_jsonrpc=>error_codes-method_not_found.
      response-error-message = 'prompts/list not implemented'.
    ENDMETHOD.

    METHOD zif_mcp_server_v2~prompts_get.
      response-error-code    = zcl_mcp_jsonrpc=>error_codes-method_not_found.
      response-error-message = 'prompts/get not implemented'.
    ENDMETHOD.

    METHOD zif_mcp_server_v2~resources_list.
      response-error-code    = zcl_mcp_jsonrpc=>error_codes-method_not_found.
      response-error-message = 'resources/list not implemented'.
    ENDMETHOD.

    METHOD zif_mcp_server_v2~resources_read.
      response-error-code    = zcl_mcp_jsonrpc=>error_codes-method_not_found.
      response-error-message = 'resources/read not implemented'.
    ENDMETHOD.

    METHOD zif_mcp_server_v2~resources_tmpls_list.
      response-error-code    = zcl_mcp_jsonrpc=>error_codes-method_not_found.
      response-error-message = 'resources/templates/list not implemented'.
    ENDMETHOD.

    METHOD zif_mcp_server_v2~tools_list.
      tools_list_called = abap_true.

      TRY.
          response-result = zcl_mcp_ajson=>create_empty( ).
          response-result->set_string( iv_path = '/resultType'
                                       iv_val  = zif_mcp_constants=>result_types-complete ).
          response-result->touch_array( '/tools' ).
          response-result->set_integer( iv_path = '/ttlMs'
                                        iv_val  = 0 ).
          response-result->set_string( iv_path = '/cacheScope'
                                       iv_val  = zif_mcp_constants=>cache_scopes-private ).
        CATCH zcx_mcp_ajson_error INTO DATA(error).
          response-error-code    = zcl_mcp_jsonrpc=>error_codes-internal_error.
          response-error-message = error->get_text( ).
      ENDTRY.
    ENDMETHOD.

    METHOD zif_mcp_server_v2~tools_call.
      response-error-code    = zcl_mcp_jsonrpc=>error_codes-method_not_found.
      response-error-message = 'tools/call not implemented'.
    ENDMETHOD.

    METHOD zif_mcp_server_v2~completions_complete.
      response-error-code    = zcl_mcp_jsonrpc=>error_codes-method_not_found.
      response-error-message = 'completion/complete not implemented'.
    ENDMETHOD.

    METHOD zif_mcp_server_v2~tasks_get.
      response-error-code    = zcl_mcp_jsonrpc=>error_codes-method_not_found.
      response-error-message = 'tasks/get not implemented'.
    ENDMETHOD.

    METHOD zif_mcp_server_v2~tasks_update.
      tasks_update_called = abap_true.
      update_params = request.

      TRY.
          response-result = zcl_mcp_ajson=>create_empty( ).
          response-result->set_string( iv_path = '/resultType'
                                       iv_val  = zif_mcp_constants=>result_types-complete ).
        CATCH zcx_mcp_ajson_error INTO DATA(error).
          response-error-code    = zcl_mcp_jsonrpc=>error_codes-internal_error.
          response-error-message = error->get_text( ).
      ENDTRY.
    ENDMETHOD.

    METHOD zif_mcp_server_v2~tasks_cancel.
      response-error-code    = zcl_mcp_jsonrpc=>error_codes-method_not_found.
      response-error-message = 'tasks/cancel not implemented'.
    ENDMETHOD.
  ENDCLASS.


  CLASS ltcl_mcp_modern_router DEFINITION FINAL
    FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.

    PRIVATE SECTION.
      DATA server TYPE REF TO lcl_router_server.

      METHODS setup.

      METHODS make_request
        IMPORTING !method       TYPE string
        RETURNING VALUE(result) TYPE zcl_mcp_jsonrpc=>request.

      METHODS make_params_request
        IMPORTING !method       TYPE string
        RETURNING VALUE(result) TYPE zcl_mcp_jsonrpc=>request
        RAISING   zcx_mcp_ajson_error.

      METHODS dispatch_discover       FOR TESTING.
      METHODS dispatch_tools_list     FOR TESTING.
      METHODS dispatch_tasks_update   FOR TESTING RAISING zcx_mcp_ajson_error.
      METHODS unknown_method          FOR TESTING.
      METHODS legacy_methods_rejected FOR TESTING.
      METHODS unbound_server          FOR TESTING.
  ENDCLASS.


  CLASS ltcl_mcp_modern_router IMPLEMENTATION.
    METHOD setup.
      server = NEW lcl_router_server( ).
    ENDMETHOD.

    METHOD make_request.
      result-jsonrpc    = zcl_mcp_jsonrpc=>jsonrpc_version.
      result-method     = method.
      result-id         = '1'.
      result-id_present = abap_true.
      result-params     = zcl_mcp_ajson=>create_empty( ).
    ENDMETHOD.

    METHOD make_params_request.
      result = make_request( method ).
      result-params->set_string( iv_path = '/taskId'
                                 iv_val  = 'TASK-1' ).
      result-params->touch_array( '/inputResponses' ).
      result-params->set_string( iv_path = '/inputResponses/1/requestId'
                                 iv_val  = 'REQ-1' ).
    ENDMETHOD.

    METHOD dispatch_discover.
      DATA(request) = make_request( 'server/discover' ).

      DATA(response) = zcl_mcp_modern_router=>route_request( server  = server
                                                             request = request ).

      cl_abap_unit_assert=>assert_true( act = server->discover_called ).
      cl_abap_unit_assert=>assert_initial( act = response-error-code ).
      cl_abap_unit_assert=>assert_bound( act = response-result ).

      cl_abap_unit_assert=>assert_equals( exp = zif_mcp_constants=>result_types-complete
                                          act = response-result->get_string( '/resultType' ) ).

      cl_abap_unit_assert=>assert_equals( exp = '1'
                                          act = response-id ).
    ENDMETHOD.

    METHOD dispatch_tools_list.
      DATA(request) = make_request( 'tools/list' ).

      DATA(response) = zcl_mcp_modern_router=>route_request( server  = server
                                                             request = request ).

      cl_abap_unit_assert=>assert_true( act = server->tools_list_called ).
      cl_abap_unit_assert=>assert_initial( act = response-error-code ).
      cl_abap_unit_assert=>assert_bound( act = response-result ).
      cl_abap_unit_assert=>assert_true( act = response-result->exists( '/tools' ) ).
    ENDMETHOD.

    METHOD dispatch_tasks_update.
      DATA(request) = make_params_request( 'tasks/update' ).

      TRY.
          request-params = zcl_mcp_ajson=>create_empty( ).
          request-params->set_string( iv_path = '/taskId'
                                      iv_val  = '00000000000000000000000000000001' ).
          request-params->set_string( iv_path = '/inputResponses/confirm/value'
                                      iv_val  = 'REQ-1' ).
        CATCH zcx_mcp_ajson_error INTO DATA(error).
          cl_abap_unit_assert=>fail( error->get_text( ) ).
      ENDTRY.

      DATA(response) = zcl_mcp_modern_router=>route_request( server  = server
                                                             request = request ).

      cl_abap_unit_assert=>assert_true( act = server->tasks_update_called ).
      cl_abap_unit_assert=>assert_initial( act = response-error-code ).
      cl_abap_unit_assert=>assert_bound( act = server->update_params ).

      cl_abap_unit_assert=>assert_equals( exp = '00000000000000000000000000000001'
                                          act = server->update_params->get_task_id( ) ).

      cl_abap_unit_assert=>assert_true( act = server->update_params->has_input_responses( ) ).

      cl_abap_unit_assert=>assert_equals(
          exp = 'REQ-1'
          act = server->update_params->get_input_responses( )->get_string( '/confirm/value' ) ).

      cl_abap_unit_assert=>assert_bound( act = response-result ).
    ENDMETHOD.

    METHOD unknown_method.
      DATA(request) = make_request( 'not/a-method' ).

      DATA(response) = zcl_mcp_modern_router=>route_request( server  = server
                                                             request = request ).

      cl_abap_unit_assert=>assert_equals( exp = zcl_mcp_jsonrpc=>error_codes-method_not_found
                                          act = response-error-code ).
    ENDMETHOD.

    METHOD legacy_methods_rejected.
      DATA methods TYPE STANDARD TABLE OF string WITH EMPTY KEY.

      APPEND 'initialize' TO methods.
      APPEND 'ping' TO methods.
      APPEND 'logging/setLevel' TO methods.
      APPEND 'tasks/list' TO methods.
      APPEND 'tasks/result' TO methods.

      LOOP AT methods INTO DATA(method).
        DATA(request) = make_request( method ).

        DATA(response) = zcl_mcp_modern_router=>route_request( server  = server
                                                               request = request ).

        cl_abap_unit_assert=>assert_equals( exp = zcl_mcp_jsonrpc=>error_codes-method_not_found
                                            act = response-error-code
                                            msg = |{ method } must not be exposed on the v2 route| ).
      ENDLOOP.
    ENDMETHOD.

    METHOD unbound_server.
      DATA no_server TYPE REF TO zif_mcp_server_v2.

      DATA(request) = make_request( 'server/discover' ).

      DATA(response) = zcl_mcp_modern_router=>route_request( server  = no_server
                                                             request = request ).

      cl_abap_unit_assert=>assert_equals( exp = zcl_mcp_jsonrpc=>error_codes-internal_error
                                          act = response-error-code ).
    ENDMETHOD.
  ENDCLASS.
