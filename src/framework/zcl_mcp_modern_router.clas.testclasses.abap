CLASS ltcl_mcp_modern_router DEFINITION DEFERRED.
CLASS zcl_mcp_modern_router DEFINITION LOCAL FRIENDS ltcl_mcp_modern_router.

CLASS lcl_router_server DEFINITION FINAL.
  PUBLIC SECTION.
    INTERFACES zif_mcp_server_v2.

    DATA discover_called     TYPE abap_bool.
    DATA tools_list_called   TYPE abap_bool.
    DATA tasks_update_called TYPE abap_bool.
    DATA update_params       TYPE REF TO zcl_mcp_req_update_task.
    DATA context             TYPE zif_mcp_server_v2=>v2_context.
ENDCLASS.


CLASS lcl_router_server IMPLEMENTATION.
  METHOD zif_mcp_server_v2~set_v2_context.
    me->context = context.
  ENDMETHOD.

  METHOD zif_mcp_server_v2~get_v2_context.
    result = me->context.
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
                                               ELSE CONV #( |tools/list failed while loading tool schema| ) ) ) ##NO_TEXT.
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
                                     iv_val  = 'Router Test Server' ) ##NO_TEXT.
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
    response-error-message = 'prompts/list not implemented' ##NO_TEXT.
  ENDMETHOD.

  METHOD zif_mcp_server_v2~prompts_get.
    response-error-code    = zcl_mcp_jsonrpc=>error_codes-method_not_found.
    response-error-message = 'prompts/get not implemented' ##NO_TEXT.
  ENDMETHOD.

  METHOD zif_mcp_server_v2~resources_list.
    response-error-code    = zcl_mcp_jsonrpc=>error_codes-method_not_found.
    response-error-message = 'resources/list not implemented' ##NO_TEXT.
  ENDMETHOD.

  METHOD zif_mcp_server_v2~resources_read.
    response-error-code    = zcl_mcp_jsonrpc=>error_codes-method_not_found.
    response-error-message = 'resources/read not implemented' ##NO_TEXT.
  ENDMETHOD.

  METHOD zif_mcp_server_v2~resources_tmpls_list.
    response-error-code    = zcl_mcp_jsonrpc=>error_codes-method_not_found.
    response-error-message = 'resources/templates/list not implemented' ##NO_TEXT.
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
    response-error-message = 'tools/call not implemented' ##NO_TEXT.
  ENDMETHOD.

  METHOD zif_mcp_server_v2~completions_complete.
    response-error-code    = zcl_mcp_jsonrpc=>error_codes-method_not_found.
    response-error-message = 'completion/complete not implemented' ##NO_TEXT.
  ENDMETHOD.

  METHOD zif_mcp_server_v2~tasks_get.
    response-error-code    = zcl_mcp_jsonrpc=>error_codes-method_not_found.
    response-error-message = 'tasks/get not implemented' ##NO_TEXT.
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
    response-error-message = 'tasks/cancel not implemented' ##NO_TEXT.
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

    METHODS dispatch_discover        FOR TESTING.
    METHODS dispatch_tools_list      FOR TESTING.
    METHODS dispatch_tasks_update    FOR TESTING RAISING zcx_mcp_ajson_error.
    METHODS unknown_method           FOR TESTING.
    METHODS legacy_methods_rejected  FOR TESTING.
    METHODS unbound_server           FOR TESTING.
    METHODS header_suffix_validation FOR TESTING.
    METHODS argument_header_values   FOR TESTING RAISING zcx_mcp_ajson_error.
    METHODS normalize_header_values  FOR TESTING RAISING zcx_mcp_server.
    METHODS safe_integer_text        FOR TESTING RAISING zcx_mcp_server.
    METHODS compare_param_headers    FOR TESTING RAISING zcx_mcp_server.
    METHODS collect_param_headers    FOR TESTING RAISING zcx_mcp_ajson_error zcx_mcp_server.
    METHODS task_requires_capability FOR TESTING RAISING zcx_mcp_ajson_error.
    METHODS task_with_capability     FOR TESTING RAISING zcx_mcp_ajson_error.

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

    cl_abap_unit_assert=>assert_true( server->discover_called ).
    cl_abap_unit_assert=>assert_initial( response-error-code ).
    cl_abap_unit_assert=>assert_bound( response-result ).

    cl_abap_unit_assert=>assert_equals( exp = zif_mcp_constants=>result_types-complete
                                        act = response-result->get_string( '/resultType' ) ).

    cl_abap_unit_assert=>assert_equals( exp = '1'
                                        act = response-id ).
  ENDMETHOD.

  METHOD dispatch_tools_list.
    DATA(request) = make_request( 'tools/list' ).

    DATA(response) = zcl_mcp_modern_router=>route_request( server  = server
                                                           request = request ).

    cl_abap_unit_assert=>assert_true( server->tools_list_called ).
    cl_abap_unit_assert=>assert_initial( response-error-code ).
    cl_abap_unit_assert=>assert_bound( response-result ).
    cl_abap_unit_assert=>assert_true( response-result->exists( '/tools' ) ).
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

    cl_abap_unit_assert=>assert_true( server->tasks_update_called ).
    cl_abap_unit_assert=>assert_initial( response-error-code ).
    cl_abap_unit_assert=>assert_bound( server->update_params ).

    cl_abap_unit_assert=>assert_equals( exp = '00000000000000000000000000000001'
                                        act = server->update_params->get_task_id( ) ).

    cl_abap_unit_assert=>assert_true( server->update_params->has_input_responses( ) ).

    cl_abap_unit_assert=>assert_equals(
        exp = 'REQ-1'
        act = server->update_params->get_input_responses( )->get_string( '/confirm/value' ) ).

    cl_abap_unit_assert=>assert_bound( response-result ).
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

  METHOD header_suffix_validation.
    cl_abap_unit_assert=>assert_true( zcl_mcp_modern_router=>is_valid_header_suffix( `Trace-Id_1` ) ).

    cl_abap_unit_assert=>assert_false( zcl_mcp_modern_router=>is_valid_header_suffix( `` ) ).

    cl_abap_unit_assert=>assert_false( zcl_mcp_modern_router=>is_valid_header_suffix( `bad value` ) ).

    cl_abap_unit_assert=>assert_false( zcl_mcp_modern_router=>is_valid_header_suffix( `bad:value` ) ).
  ENDMETHOD.

  METHOD argument_header_values.
    DATA(arguments) = zcl_mcp_ajson=>create_empty( ).

    arguments->set_string( iv_path = `/text`
                           iv_val  = `abc` ).
    arguments->set_integer( iv_path = `/count`
                            iv_val  = 42 ).
    arguments->set_boolean( iv_path = `/enabled`
                            iv_val  = abap_true ).
    arguments->set_boolean( iv_path = `/disabled`
                            iv_val  = abap_false ).

    cl_abap_unit_assert=>assert_equals( exp = `abc`
                                        act = zcl_mcp_modern_router=>argument_to_header_value( arguments = arguments
                                                                                               path      = `/text` ) ).

    cl_abap_unit_assert=>assert_equals( exp = `42`
                                        act = zcl_mcp_modern_router=>argument_to_header_value( arguments = arguments
                                                                                               path      = `/count` ) ).

    cl_abap_unit_assert=>assert_equals(
        exp = `true`
        act = zcl_mcp_modern_router=>argument_to_header_value( arguments = arguments
                                                               path      = `/enabled` ) ).

    cl_abap_unit_assert=>assert_equals(
        exp = `false`
        act = zcl_mcp_modern_router=>argument_to_header_value( arguments = arguments
                                                               path      = `/disabled` ) ).
  ENDMETHOD.

  METHOD normalize_header_values.
    cl_abap_unit_assert=>assert_equals( exp = `plain`
                                        act = zcl_mcp_modern_router=>normalize_header_value(
                                                  header_name  = `Mcp-Param-Text`
                                                  header_value = `plain` ) ).

    cl_abap_unit_assert=>assert_equals( exp = `Hello`
                                        act = zcl_mcp_modern_router=>normalize_header_value(
                                                  header_name  = `Mcp-Param-Text`
                                                  header_value = `=?base64?SGVsbG8=?=` ) ).

    TRY.
        zcl_mcp_modern_router=>normalize_header_value( header_name  = `Mcp-Param-Text`
                                                       header_value = `=?base64?***?=` ).
        cl_abap_unit_assert=>fail( `Expected invalid base64 header value` ).
      CATCH zcx_mcp_server.
    ENDTRY.

    TRY.
        zcl_mcp_modern_router=>normalize_header_value( header_name  = `Mcp-Param-Text`
                                                       header_value = |bad{ cl_abap_char_utilities=>newline }value| ).
        cl_abap_unit_assert=>fail( `Expected unsafe control character rejection` ).
      CATCH zcx_mcp_server.
    ENDTRY.
  ENDMETHOD.

  METHOD safe_integer_text.
    zcl_mcp_modern_router=>assert_safe_integer_text( header_name = `Mcp-Param-Count`
                                                     value       = `9007199254740991` ).

    zcl_mcp_modern_router=>assert_safe_integer_text( header_name = `Mcp-Param-Count`
                                                     value       = `-9007199254740991` ).

    TRY.
        zcl_mcp_modern_router=>assert_safe_integer_text( header_name = `Mcp-Param-Count`
                                                         value       = `` ).
        cl_abap_unit_assert=>fail( `Expected empty integer rejection` ).
      CATCH zcx_mcp_server.
    ENDTRY.

    TRY.
        zcl_mcp_modern_router=>assert_safe_integer_text( header_name = `Mcp-Param-Count`
                                                         value       = `12.3` ).
        cl_abap_unit_assert=>fail( `Expected decimal integer rejection` ).
      CATCH zcx_mcp_server.
    ENDTRY.

    TRY.
        zcl_mcp_modern_router=>assert_safe_integer_text( header_name = `Mcp-Param-Count`
                                                         value       = `9007199254740992` ).
        cl_abap_unit_assert=>fail( `Expected unsafe integer rejection` ).
      CATCH zcx_mcp_server.
    ENDTRY.
  ENDMETHOD.

  METHOD compare_param_headers.
    zcl_mcp_modern_router=>compare_param_header( header_name   = `Mcp-Param-Count`
                                                 header_value  = `001`
                                                 body_value    = `1`
                                                 property_type = `integer` ).

    zcl_mcp_modern_router=>compare_param_header( header_name   = `Mcp-Param-Enabled`
                                                 header_value  = `true`
                                                 body_value    = `true`
                                                 property_type = `boolean` ).

    TRY.
        zcl_mcp_modern_router=>compare_param_header( header_name   = `Mcp-Param-Enabled`
                                                     header_value  = `yes`
                                                     body_value    = `true`
                                                     property_type = `boolean` ).
        cl_abap_unit_assert=>fail( `Expected invalid boolean rejection` ).
      CATCH zcx_mcp_server.
    ENDTRY.

    TRY.
        zcl_mcp_modern_router=>compare_param_header( header_name   = `Mcp-Param-Text`
                                                     header_value  = `abc`
                                                     body_value    = `def`
                                                     property_type = `string` ).
        cl_abap_unit_assert=>fail( `Expected string mismatch rejection` ).
      CATCH zcx_mcp_server.
    ENDTRY.
  ENDMETHOD.

  METHOD collect_param_headers.
    DATA(schema) = zcl_mcp_ajson=>create_empty( ).

    schema->set_string( iv_path = `/properties/tenant/type`
                        iv_val  = `string` ).
    schema->set_boolean( iv_path = `/properties/tenant/x-mcp-header`
                         iv_val  = abap_true ).
    schema->set_string( iv_path = `/properties/count/type`
                        iv_val  = `integer` ).
    schema->set_string( iv_path = `/properties/count/x-mcp-header`
                        iv_val  = `Count` ).
    schema->set_string( iv_path = `/properties/enabled/type`
                        iv_val  = `boolean` ).
    schema->set_string( iv_path = `/properties/enabled/x-mcp-header`
                        iv_val  = `Enabled` ).

    DATA(headers) = zcl_mcp_modern_router=>collect_param_headers( schema ).

    READ TABLE headers WITH KEY header_name = `Mcp-Param-tenant`
         TRANSPORTING NO FIELDS.
    cl_abap_unit_assert=>assert_subrc( msg = `tenant header should be collected` ).

    READ TABLE headers WITH KEY header_name = `Mcp-Param-Count`
         TRANSPORTING NO FIELDS.
    cl_abap_unit_assert=>assert_subrc( msg = `count header should be collected` ).

    READ TABLE headers WITH KEY header_name = `Mcp-Param-Enabled`
         TRANSPORTING NO FIELDS.
    cl_abap_unit_assert=>assert_subrc( msg = `enabled header should be collected` ).

    schema = zcl_mcp_ajson=>create_empty( ).
    schema->set_string( iv_path = `/properties/a/type`
                        iv_val  = `string` ).
    schema->set_string( iv_path = `/properties/a/x-mcp-header`
                        iv_val  = `Dup` ).
    schema->set_string( iv_path = `/properties/b/type`
                        iv_val  = `string` ).
    schema->set_string( iv_path = `/properties/b/x-mcp-header`
                        iv_val  = `dup` ).

    TRY.
        zcl_mcp_modern_router=>collect_param_headers( schema ).
        cl_abap_unit_assert=>fail( `Expected duplicate header rejection` ).
      CATCH zcx_mcp_server.
    ENDTRY.

    schema = zcl_mcp_ajson=>create_empty( ).
    schema->set_string( iv_path = `/properties/a/type`
                        iv_val  = `string` ).
    schema->set_string( iv_path = `/properties/a/x-mcp-header`
                        iv_val  = `Bad Header` ).

    TRY.
        zcl_mcp_modern_router=>collect_param_headers( schema ).
        cl_abap_unit_assert=>fail( `Expected invalid header suffix rejection` ).
      CATCH zcx_mcp_server.
    ENDTRY.

    schema = zcl_mcp_ajson=>create_empty( ).
    schema->set_string( iv_path = `/properties/a/type`
                        iv_val  = `object` ).
    schema->set_string( iv_path = `/properties/a/x-mcp-header`
                        iv_val  = `A` ).

    TRY.
        zcl_mcp_modern_router=>collect_param_headers( schema ).
        cl_abap_unit_assert=>fail( `Expected unsupported property type rejection` ).
      CATCH zcx_mcp_server.
    ENDTRY.

    schema = zcl_mcp_ajson=>create_empty( ).
    schema->set_string( iv_path = `/properties/filter/type`
                        iv_val  = `object` ).
    schema->set_string( iv_path = `/properties/filter/properties/tenant/type`
                        iv_val  = `string` ).
    schema->set_boolean( iv_path = `/properties/filter/properties/tenant/x-mcp-header`
                         iv_val  = abap_true ).

    TRY.
        zcl_mcp_modern_router=>collect_param_headers( schema ).
        cl_abap_unit_assert=>fail( `Expected nested x-mcp-header rejection` ).
      CATCH zcx_mcp_server.
    ENDTRY.
  ENDMETHOD.

  METHOD task_requires_capability.
    DATA(request) = make_request( `tools/call` ).
    DATA v2_response TYPE zif_mcp_server_v2=>v2_response.
    DATA response    TYPE zcl_mcp_jsonrpc=>response.

    v2_response-result = zcl_mcp_ajson=>create_empty( ).
    v2_response-result->set_string( iv_path = `/resultType`
                                    iv_val  = zif_mcp_constants=>result_types-task ).

    server->context-extensions = zcl_mcp_ajson=>create_empty( ).

    zcl_mcp_modern_router=>apply_v2_response( EXPORTING server      = server
                                                        v2_response = v2_response
                                                        request     = request
                                              CHANGING  response    = response ).

    cl_abap_unit_assert=>assert_equals( exp = zcl_mcp_jsonrpc=>error_codes-missing_client_capability
                                        act = response-error-code ).

    cl_abap_unit_assert=>assert_equals( exp = zif_mcp_constants=>extensions-tasks
                                        act = response-error-data->get_string( `/requiredCapabilities/1` ) ).
  ENDMETHOD.

  METHOD task_with_capability.
    DATA(request) = make_request( `tools/call` ).
    DATA v2_response TYPE zif_mcp_server_v2=>v2_response.
    DATA response    TYPE zcl_mcp_jsonrpc=>response.

    v2_response-result = zcl_mcp_ajson=>create_empty( ).
    v2_response-result->set_string( iv_path = `/resultType`
                                    iv_val  = zif_mcp_constants=>result_types-task ).
    v2_response-result->set_string( iv_path = `/task/taskId`
                                    iv_val  = `00000000000000000000000000000001` ).

    server->context-extensions = zcl_mcp_ajson=>create_empty( ).
    server->context-extensions->touch_object( `/io.modelcontextprotocol~1tasks` ).

    zcl_mcp_modern_router=>apply_v2_response( EXPORTING server      = server
                                                        v2_response = v2_response
                                                        request     = request
                                              CHANGING  response    = response ).

    cl_abap_unit_assert=>assert_initial( response-error-code ).
    cl_abap_unit_assert=>assert_bound( response-result ).
    cl_abap_unit_assert=>assert_equals( exp = `00000000000000000000000000000001`
                                        act = response-result->get_string( `/task/taskId` ) ).
  ENDMETHOD.
ENDCLASS.
