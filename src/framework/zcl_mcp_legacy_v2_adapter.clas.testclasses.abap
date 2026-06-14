CLASS ltcl_mcp_legacy_v2_adapter DEFINITION DEFERRED.
CLASS zcl_mcp_legacy_v2_adapter DEFINITION LOCAL FRIENDS ltcl_mcp_legacy_v2_adapter.

CLASS lcl_v2_server DEFINITION FINAL.
  PUBLIC SECTION.
    INTERFACES zif_mcp_server_v2.

    DATA context           TYPE zif_mcp_server_v2=>v2_context.
    DATA next_response     TYPE zif_mcp_server_v2=>v2_response.
    DATA discover_response TYPE zif_mcp_server_v2=>v2_response.
    DATA last_method       TYPE string.
ENDCLASS.

CLASS lcl_v2_server IMPLEMENTATION.
  METHOD zif_mcp_server_v2~set_v2_context.
    me->context = context.
  ENDMETHOD.

  METHOD zif_mcp_server_v2~get_v2_context.
    result = me->context.
  ENDMETHOD.

  METHOD zif_mcp_server_v2~server_discover.
    response = discover_response.
  ENDMETHOD.

  METHOD zif_mcp_server_v2~prompts_list.
    last_method = `prompts/list`.
    response = next_response.
  ENDMETHOD.

  METHOD zif_mcp_server_v2~prompts_get.
    last_method = `prompts/get`.
    response = next_response.
  ENDMETHOD.

  METHOD zif_mcp_server_v2~resources_list.
    last_method = `resources/list`.
    response = next_response.
  ENDMETHOD.

  METHOD zif_mcp_server_v2~resources_read.
    last_method = `resources/read`.
    response = next_response.
  ENDMETHOD.

  METHOD zif_mcp_server_v2~resources_tmpls_list.
    last_method = `resources/templates/list`.
    response = next_response.
  ENDMETHOD.

  METHOD zif_mcp_server_v2~tools_list.
    last_method = `tools/list`.
    response = next_response.
  ENDMETHOD.

  METHOD zif_mcp_server_v2~get_tool_input_schema.
    result = zcl_mcp_ajson=>create_empty( ).
  ENDMETHOD.

  METHOD zif_mcp_server_v2~tools_call.
    last_method = `tools/call`.
    response = next_response.
  ENDMETHOD.

  METHOD zif_mcp_server_v2~completions_complete.
    last_method = `completion/complete`.
    response = next_response.
  ENDMETHOD.

  METHOD zif_mcp_server_v2~tasks_get.
    last_method = `tasks/get`.
    response = next_response.
  ENDMETHOD.

  METHOD zif_mcp_server_v2~tasks_update.
    last_method = `tasks/update`.
    response = next_response.
  ENDMETHOD.

  METHOD zif_mcp_server_v2~tasks_cancel.
    last_method = `tasks/cancel`.
    response = next_response.
  ENDMETHOD.
ENDCLASS.

CLASS ltcl_mcp_legacy_v2_adapter DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    METHODS no_server_returns_error     FOR TESTING RAISING zcx_mcp_ajson_error.
    METHODS initialize_maps_discovery   FOR TESTING RAISING zcx_mcp_ajson_error.
    METHODS ping_sets_context           FOR TESTING RAISING zcx_mcp_ajson_error.
    METHODS strips_v2_complete_envelope FOR TESTING RAISING zcx_mcp_ajson_error.
    METHODS rejects_input_required      FOR TESTING RAISING zcx_mcp_ajson_error.
    METHODS completed_task_result       FOR TESTING RAISING zcx_mcp_ajson_error.
    METHODS tasks_update_not_legacy     FOR TESTING RAISING zcx_mcp_ajson_error.

    METHODS make_request
      IMPORTING method        TYPE string
                id            TYPE string DEFAULT `1`
                params        TYPE REF TO zif_mcp_ajson OPTIONAL
      RETURNING VALUE(result) TYPE zcl_mcp_jsonrpc=>request.

    METHODS run_request
      IMPORTING server        TYPE REF TO zif_mcp_server_v2
                request       TYPE zcl_mcp_jsonrpc=>request
      RETURNING VALUE(result) TYPE REF TO zif_mcp_ajson
      RAISING   zcx_mcp_ajson_error.
ENDCLASS.

CLASS ltcl_mcp_legacy_v2_adapter IMPLEMENTATION.
  METHOD make_request.
    result-jsonrpc    = zcl_mcp_jsonrpc=>jsonrpc_version.
    result-method     = method.
    result-id         = id.
    result-id_present = abap_true.

    IF params IS BOUND.
      result-params = params.
    ELSE.
      result-params = zcl_mcp_ajson=>create_empty( ).
    ENDIF.
  ENDMETHOD.

  METHOD run_request.
    DATA http_request  TYPE REF TO if_http_request.
    DATA http_response TYPE REF TO if_http_response.
    DATA http_server   TYPE REF TO if_http_server.

    DATA(json) = zcl_mcp_legacy_v2_adapter=>process_request( server        = server
                                                             request       = request
                                                             area          = `UNIT`
                                                             servername    = `V2`
                                                             http_request  = http_request
                                                             http_response = http_response
                                                             http_server   = http_server
                                                             cors_mode     = zcl_mcp_configuration=>cors_mode_ignore ).

    result = zcl_mcp_ajson=>parse( json ).
  ENDMETHOD.

  METHOD no_server_returns_error.
    DATA server TYPE REF TO zif_mcp_server_v2.

    DATA(result) = run_request( server  = server
                                request = make_request( `ping` ) ).

    cl_abap_unit_assert=>assert_equals( exp = zcl_mcp_jsonrpc=>error_codes-internal_error
                                        act = result->get_integer( `/error/code` ) ).
    cl_abap_unit_assert=>assert_equals( exp = `Draft MCP server is not available`
                                        act = result->get_string( `/error/message` ) ).
  ENDMETHOD.

  METHOD initialize_maps_discovery.
    DATA(server) = NEW lcl_v2_server( ).
    DATA(params) = zcl_mcp_ajson=>create_empty( ).
    DATA(discover) = zcl_mcp_ajson=>create_empty( ).

    params->set_string( iv_path = `/protocolVersion`
                        iv_val  = zif_mcp_constants=>protocol_version_2025_11_25 ).

    discover->touch_object( `/capabilities/tools` ).
    discover->touch_object( `/capabilities/resources` ).
    discover->touch_object( `/capabilities/extensions/io.modelcontextprotocol~1tasks` ).
    discover->set_string( iv_path = `/serverInfo/name`
                          iv_val  = `Unit V2` ).
    discover->set_string( iv_path = `/instructions`
                          iv_val  = `Test instructions` ).

    server->discover_response-result = discover.

    DATA(result) = run_request( server  = server
                                request = make_request( method = `initialize`
                                                        params = params ) ).

    cl_abap_unit_assert=>assert_equals( exp = zif_mcp_constants=>protocol_version_2025_11_25
                                        act = result->get_string( `/result/protocolVersion` ) ).
    cl_abap_unit_assert=>assert_true( result->exists( `/result/capabilities/tools` ) ).
    cl_abap_unit_assert=>assert_true( result->exists( `/result/capabilities/resources` ) ).
    cl_abap_unit_assert=>assert_true( result->exists( `/result/capabilities/tasks` ) ).
    cl_abap_unit_assert=>assert_equals( exp = `Unit V2`
                                        act = result->get_string( `/result/serverInfo/name` ) ).
    cl_abap_unit_assert=>assert_equals( exp = `Test instructions`
                                        act = result->get_string( `/result/instructions` ) ).
  ENDMETHOD.

  METHOD ping_sets_context.
    DATA(server) = NEW lcl_v2_server( ).

    DATA(result) = run_request( server  = server
                                request = make_request( `ping` ) ).

    cl_abap_unit_assert=>assert_equals( exp = zif_mcp_ajson_types=>node_type-object
                                        act = result->get_node_type( `/result` ) ).
    cl_abap_unit_assert=>assert_equals( exp = `UNIT`
                                        act = server->context-area ).
    cl_abap_unit_assert=>assert_equals( exp = `V2`
                                        act = server->context-server ).
    cl_abap_unit_assert=>assert_equals( exp = zif_mcp_constants=>protocol_version_2025_03_26
                                        act = server->context-protocol_ver ).
    cl_abap_unit_assert=>assert_true(
        server->context-client_caps->exists( `/extensions/io.modelcontextprotocol~1tasks` ) ).
  ENDMETHOD.

  METHOD strips_v2_complete_envelope.
    DATA(server) = NEW lcl_v2_server( ).
    DATA(v2_result) = zcl_mcp_ajson=>create_empty( ).

    v2_result->set_string( iv_path = `/resultType`
                           iv_val  = zif_mcp_constants=>result_types-complete ).
    v2_result->set_integer( iv_path = `/ttlMs`
                            iv_val  = 1000 ).
    v2_result->set_string( iv_path = `/cacheScope`
                           iv_val  = zif_mcp_constants=>cache_scopes-private ).
    v2_result->set_string( iv_path = `/tools/1/name`
                           iv_val  = `calc` ).

    server->next_response-result = v2_result.

    DATA(result) = run_request( server  = server
                                request = make_request( `tools/list` ) ).

    cl_abap_unit_assert=>assert_equals( exp = `tools/list`
                                        act = server->last_method ).
    cl_abap_unit_assert=>assert_equals( exp = `calc`
                                        act = result->get_string( `/result/tools/1/name` ) ).
    cl_abap_unit_assert=>assert_false( result->exists( `/result/resultType` ) ).
    cl_abap_unit_assert=>assert_false( result->exists( `/result/ttlMs` ) ).
    cl_abap_unit_assert=>assert_false( result->exists( `/result/cacheScope` ) ).
  ENDMETHOD.

  METHOD rejects_input_required.
    DATA(server) = NEW lcl_v2_server( ).
    DATA(params) = zcl_mcp_ajson=>create_empty( ).
    DATA(v2_result) = zcl_mcp_ajson=>create_empty( ).

    params->set_string( iv_path = `/name`
                        iv_val  = `needs_input` ).

    v2_result->set_string( iv_path = `/resultType`
                           iv_val  = zif_mcp_constants=>result_types-input_required ).

    server->next_response-result = v2_result.

    DATA(result) = run_request( server  = server
                                request = make_request( method = `tools/call`
                                                        params = params ) ).

    cl_abap_unit_assert=>assert_equals( exp = zcl_mcp_jsonrpc=>error_codes-invalid_request
                                        act = result->get_integer( `/error/code` ) ).
    cl_abap_unit_assert=>assert_true(
        boolc( result->get_string( `/error/message` ) CS `cannot be represented for legacy clients` ) ).
  ENDMETHOD.

  METHOD completed_task_result.
    DATA(server) = NEW lcl_v2_server( ).
    DATA(params) = zcl_mcp_ajson=>create_empty( ).
    DATA(v2_result) = zcl_mcp_ajson=>create_empty( ).

    params->set_string( iv_path = `/taskId`
                        iv_val  = `00000000000000000000000000000001` ).

    v2_result->set_string( iv_path = `/task/status`
                           iv_val  = zcl_mcp_tasks=>status_completed ).
    v2_result->set_string( iv_path = `/result/content/1/type`
                           iv_val  = `text` ).
    v2_result->set_string( iv_path = `/result/content/1/text`
                           iv_val  = `done` ).

    server->next_response-result = v2_result.

    DATA(result) = run_request( server  = server
                                request = make_request( method = `tasks/result`
                                                        params = params ) ).

    cl_abap_unit_assert=>assert_equals( exp = `tasks/get`
                                        act = server->last_method ).
    cl_abap_unit_assert=>assert_equals( exp = `done`
                                        act = result->get_string( `/result/content/1/text` ) ).
  ENDMETHOD.

  METHOD tasks_update_not_legacy.
    DATA(server) = NEW lcl_v2_server( ).

    DATA(result) = run_request( server  = server
                                request = make_request( `tasks/update` ) ).

    cl_abap_unit_assert=>assert_equals( exp = zcl_mcp_jsonrpc=>error_codes-method_not_found
                                        act = result->get_integer( `/error/code` ) ).
    cl_abap_unit_assert=>assert_equals( exp = `tasks/update is not available to legacy clients`
                                        act = result->get_string( `/error/message` ) ).
  ENDMETHOD.
ENDCLASS.
