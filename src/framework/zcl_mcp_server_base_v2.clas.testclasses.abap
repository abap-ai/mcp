CLASS ltcl_mcp_server_base_v2 DEFINITION DEFERRED.
CLASS zcl_mcp_server_base_v2 DEFINITION LOCAL FRIENDS ltcl_mcp_server_base_v2.

CLASS lcl_test_server DEFINITION
INHERITING FROM zcl_mcp_server_base_v2 FINAL.

  PUBLIC SECTION.
    DATA tools_called TYPE abap_bool.

  PROTECTED SECTION.
    METHODS get_implementation REDEFINITION.
    METHODS get_capabilities   REDEFINITION.
    METHODS get_instructions   REDEFINITION.
    METHODS handle_tools_list  REDEFINITION.
ENDCLASS.


CLASS lcl_test_server IMPLEMENTATION.

  METHOD get_implementation.
    result-name        = 'Test Draft Server' ##NO_TEXT.
    result-version     = '1.2.3'.
    result-title       = 'Draft Test' ##NO_TEXT.
    result-description = 'Test server for draft MCP base class' ##NO_TEXT.
    result-website_url = 'https://example.invalid/test'.
  ENDMETHOD.

  METHOD get_capabilities.
    result-tools       = abap_true.
    result-prompts     = abap_true.
    result-resources   = abap_true.
    result-completions = abap_true.
    result-tasks       = abap_true.
  ENDMETHOD.

  METHOD get_instructions.
    result = 'Use this server only in unit tests.' ##NO_TEXT.
  ENDMETHOD.

  METHOD handle_tools_list.
    tools_called = abap_true.

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
ENDCLASS.


CLASS ltcl_mcp_server_base_v2 DEFINITION FINAL
FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.

  PRIVATE SECTION.
    DATA cut TYPE REF TO lcl_test_server.

    METHODS setup.

    METHODS context_roundtrip        FOR TESTING.
    METHODS server_discover          FOR TESTING.
    METHODS default_method_not_found FOR TESTING RAISING zcx_mcp_ajson_error zcx_mcp_server.
    METHODS tools_list_dispatch      FOR TESTING RAISING zcx_mcp_ajson_error zcx_mcp_server.
    CLASS-DATA sql_env TYPE REF TO if_osql_test_environment.

    CLASS-METHODS class_setup RAISING cx_static_check.
    CLASS-METHODS class_teardown.

    METHODS set_task_context.

    METHODS make_get_task_request
      IMPORTING task_id       TYPE sysuuid_c32
      RETURNING VALUE(result) TYPE REF TO zcl_mcp_req_get_task
      RAISING   zcx_mcp_ajson_error
                zcx_mcp_server.

    METHODS make_cancel_task_request
      IMPORTING task_id       TYPE sysuuid_c32
      RETURNING VALUE(result) TYPE REF TO zcl_mcp_req_cancel_task
      RAISING   zcx_mcp_ajson_error
                zcx_mcp_server.

    METHODS make_update_task_request
      IMPORTING task_id       TYPE sysuuid_c32
      RETURNING VALUE(result) TYPE REF TO zcl_mcp_req_update_task
      RAISING   zcx_mcp_ajson_error
                zcx_mcp_server.

    METHODS tasks_get_completed   FOR TESTING RAISING cx_static_check.
    METHODS tasks_get_input_req   FOR TESTING RAISING cx_static_check.
    METHODS tasks_update_consumes FOR TESTING RAISING cx_static_check.
    METHODS tasks_cancel_persists FOR TESTING RAISING cx_static_check.
ENDCLASS.

CLASS ltcl_mcp_server_base_v2 IMPLEMENTATION.

  METHOD class_setup.
    sql_env = cl_osql_test_environment=>create( VALUE #( ( 'ZMCP_TASKS' ) ) ).
  ENDMETHOD.

  METHOD class_teardown.
    sql_env->destroy( ).
  ENDMETHOD.

  METHOD setup.
    sql_env->clear_doubles( ).
    cut = NEW lcl_test_server( ).
    set_task_context( ).
  ENDMETHOD.

  METHOD set_task_context.
    DATA context TYPE zif_mcp_server_v2=>v2_context.

    context-area         = 'AREA'.
    context-server       = 'SERVER'.
    context-protocol_ver = zif_mcp_constants=>latest_modern_protocol_version.

    cut->zif_mcp_server_v2~set_v2_context( context ).
  ENDMETHOD.

  METHOD make_get_task_request.
    DATA json TYPE REF TO zif_mcp_ajson.

    json = zcl_mcp_ajson=>create_empty( ).
    json->set_string( iv_path = `/taskId`
                      iv_val  = task_id ).

    result = NEW zcl_mcp_req_get_task( json ).
  ENDMETHOD.

  METHOD make_cancel_task_request.
    DATA json TYPE REF TO zif_mcp_ajson.

    json = zcl_mcp_ajson=>create_empty( ).
    json->set_string( iv_path = `/taskId`
                      iv_val  = task_id ).

    result = NEW zcl_mcp_req_cancel_task( json ).
  ENDMETHOD.

  METHOD make_update_task_request.
    DATA json TYPE REF TO zif_mcp_ajson.

    json = zcl_mcp_ajson=>create_empty( ).
    json->set_string( iv_path = `/taskId`
                      iv_val  = task_id ).
    json->set_string( iv_path = `/requestState`
                      iv_val  = `state-1` ).
    json->set_string( iv_path = `/inputResponses/confirm/action`
                      iv_val  = `accept` ).

    result = NEW zcl_mcp_req_update_task( json ).
  ENDMETHOD.

  METHOD context_roundtrip.
    DATA context TYPE zif_mcp_server_v2=>v2_context.

    context-area         = 'AREA'.
    context-server       = 'SERVER'.
    context-protocol_ver = zif_mcp_constants=>latest_modern_protocol_version.
    context-log_level    = 'debug'.
    context-traceparent  = '00-4bf92f3577b34da6a3ce929d0e0e4736-00f067aa0ba902b7-00' ##NO_TEXT.

    cut->zif_mcp_server_v2~set_v2_context( context ).

    DATA(actual) = cut->zif_mcp_server_v2~get_v2_context( ).

    cl_abap_unit_assert=>assert_equals( exp = context-area
                                        act = actual-area ).

    cl_abap_unit_assert=>assert_equals( exp = context-server
                                        act = actual-server ).

    cl_abap_unit_assert=>assert_equals( exp = context-protocol_ver
                                        act = actual-protocol_ver ).

    cl_abap_unit_assert=>assert_equals( exp = context-log_level
                                        act = actual-log_level ).

    cl_abap_unit_assert=>assert_equals( exp = context-traceparent
                                        act = actual-traceparent ).
  ENDMETHOD.

  METHOD server_discover.
    DATA(response) = cut->zif_mcp_server_v2~server_discover( ).

    cl_abap_unit_assert=>assert_initial( response-error-code ).

    cl_abap_unit_assert=>assert_bound( response-result ).

    cl_abap_unit_assert=>assert_equals( exp = zif_mcp_constants=>result_types-complete
                                        act = response-result->get_string( '/resultType' ) ).

    cl_abap_unit_assert=>assert_equals( exp = zif_mcp_constants=>latest_modern_protocol_version
                                        act = response-result->get_string( '/supportedVersions/1' ) ).

    cl_abap_unit_assert=>assert_equals( exp = 'Test Draft Server'
                                        act = response-result->get_string( '/serverInfo/name' ) ).

    cl_abap_unit_assert=>assert_equals( exp = '1.2.3'
                                        act = response-result->get_string( '/serverInfo/version' ) ).

    cl_abap_unit_assert=>assert_equals( exp = 'Draft Test'
                                        act = response-result->get_string( '/serverInfo/title' ) ).

    cl_abap_unit_assert=>assert_equals( exp = 'Test server for draft MCP base class'
                                        act = response-result->get_string( '/serverInfo/description' ) ).

    cl_abap_unit_assert=>assert_equals( exp = 'https://example.invalid/test'
                                        act = response-result->get_string( '/serverInfo/websiteUrl' ) ).

    cl_abap_unit_assert=>assert_equals( exp = 'Use this server only in unit tests.'
                                        act = response-result->get_string( '/instructions' ) ).

    cl_abap_unit_assert=>assert_true( response-result->exists( '/capabilities/prompts' ) ).

    cl_abap_unit_assert=>assert_true( response-result->exists( '/capabilities/resources' ) ).

    cl_abap_unit_assert=>assert_true( response-result->exists( '/capabilities/completions' ) ).

    cl_abap_unit_assert=>assert_true(
        response-result->exists( '/capabilities/extensions/io.modelcontextprotocol~1tasks' ) ).

    cl_abap_unit_assert=>assert_equals( exp = 0
                                        act = response-result->get_integer( '/ttlMs' ) ).

    cl_abap_unit_assert=>assert_equals( exp = zif_mcp_constants=>cache_scopes-private
                                        act = response-result->get_string( '/cacheScope' ) ).
  ENDMETHOD.

  METHOD default_method_not_found.
    DATA response TYPE zif_mcp_server_v2=>v2_response.

    response = cut->zif_mcp_server_v2~prompts_list( NEW zcl_mcp_req_list_prompts( zcl_mcp_ajson=>create_empty( ) ) ).

    cl_abap_unit_assert=>assert_equals( exp = zcl_mcp_jsonrpc=>error_codes-method_not_found
                                        act = response-error-code ).

    cl_abap_unit_assert=>assert_equals( exp = 'Method prompts/list not found.'
                                        act = response-error-message ).
  ENDMETHOD.

  METHOD tools_list_dispatch.
    DATA response TYPE zif_mcp_server_v2=>v2_response.

    response = cut->zif_mcp_server_v2~tools_list( NEW zcl_mcp_req_list_tools( zcl_mcp_ajson=>create_empty( ) ) ).

    cl_abap_unit_assert=>assert_true( cut->tools_called ).

    cl_abap_unit_assert=>assert_initial( response-error-code ).

    cl_abap_unit_assert=>assert_bound( response-result ).

    cl_abap_unit_assert=>assert_equals( exp = zif_mcp_constants=>result_types-complete
                                        act = response-result->get_string( '/resultType' ) ).

    cl_abap_unit_assert=>assert_true( response-result->exists( '/tools' ) ).
  ENDMETHOD.

  METHOD tasks_get_completed.
    DATA tasks   TYPE REF TO zcl_mcp_tasks.
    DATA payload TYPE REF TO zcl_mcp_resp_task_payload.

    tasks = NEW zcl_mcp_tasks( area   = 'AREA'
                               server = 'SERVER' ).
    DATA(task_id) = tasks->create_task( tool_name = `unit_test` ).

    payload = NEW zcl_mcp_resp_task_payload( ).
    payload->add_text_content( `done` ).

    zcl_mcp_tasks=>complete( task_id = task_id
                             result  = payload ).

    DATA(response) = cut->zif_mcp_server_v2~tasks_get( make_get_task_request( task_id ) ).

    cl_abap_unit_assert=>assert_initial( response-error-code ).
    cl_abap_unit_assert=>assert_bound( response-result ).
    cl_abap_unit_assert=>assert_equals( exp = zif_mcp_constants=>result_types-complete
                                        act = response-result->get_string( `/resultType` ) ).
    cl_abap_unit_assert=>assert_equals( exp = zif_mcp_types=>task_states-completed
                                        act = response-result->get_string( `/task/status` ) ).
    cl_abap_unit_assert=>assert_equals( exp = `done`
                                        act = response-result->get_string( `/result/content/1/text` ) ).
  ENDMETHOD.

  METHOD tasks_get_input_req.
    DATA tasks   TYPE REF TO zcl_mcp_tasks.
    DATA pending TYPE REF TO zif_mcp_ajson.

    tasks = NEW zcl_mcp_tasks( area   = 'AREA'
                               server = 'SERVER' ).
    DATA(task_id) = tasks->create_task( tool_name = `unit_test` ).

    pending = zcl_mcp_ajson=>create_empty( ).
    pending->set_string( iv_path = `/requestState`
                         iv_val  = `state-1` ).
    pending->set_string( iv_path = `/inputRequests/confirm/method`
                         iv_val  = zcl_mcp_input_elicitation=>method_name ).
    pending->touch_object( `/inputRequests/confirm/params` ).

    zcl_mcp_tasks=>request_input( task_id        = task_id
                                  input_required = pending ).

    DATA(response) = cut->zif_mcp_server_v2~tasks_get( make_get_task_request( task_id ) ).

    cl_abap_unit_assert=>assert_initial( response-error-code ).
    cl_abap_unit_assert=>assert_equals( exp = zif_mcp_types=>task_states-input_required
                                        act = response-result->get_string( `/task/status` ) ).
    cl_abap_unit_assert=>assert_equals( exp = `state-1`
                                        act = response-result->get_string( `/requestState` ) ).
    cl_abap_unit_assert=>assert_equals( exp = zcl_mcp_input_elicitation=>method_name
                                        act = response-result->get_string( `/inputRequests/confirm/method` ) ).
  ENDMETHOD.

  METHOD tasks_update_consumes.
    DATA tasks   TYPE REF TO zcl_mcp_tasks.
    DATA pending TYPE REF TO zif_mcp_ajson.

    tasks = NEW zcl_mcp_tasks( area   = 'AREA'
                               server = 'SERVER' ).
    DATA(task_id) = tasks->create_task( tool_name = `unit_test` ).

    pending = zcl_mcp_ajson=>create_empty( ).
    pending->set_string( iv_path = `/inputRequests/confirm/method`
                         iv_val  = zcl_mcp_input_elicitation=>method_name ).

    zcl_mcp_tasks=>request_input( task_id        = task_id
                                  input_required = pending ).

    DATA(response) = cut->zif_mcp_server_v2~tasks_update( make_update_task_request( task_id ) ).

    cl_abap_unit_assert=>assert_initial( response-error-code ).
    cl_abap_unit_assert=>assert_equals( exp = zif_mcp_constants=>result_types-complete
                                        act = response-result->get_string( `/resultType` ) ).

    DATA(task) = tasks->get( task_id ).
    cl_abap_unit_assert=>assert_equals( exp = zif_mcp_types=>task_states-working
                                        act = task-status ).

    DATA(stored) = tasks->get_payload( task_id ).
    cl_abap_unit_assert=>assert_equals( exp = `accept`
                                        act = stored->get_string( `/inputResponses/confirm/action` ) ).
  ENDMETHOD.

  METHOD tasks_cancel_persists.
    DATA tasks TYPE REF TO zcl_mcp_tasks.

    tasks = NEW zcl_mcp_tasks( area   = 'AREA'
                               server = 'SERVER' ).
    DATA(task_id) = tasks->create_task( tool_name = `unit_test` ).

    DATA(response) = cut->zif_mcp_server_v2~tasks_cancel( make_cancel_task_request( task_id ) ).

    cl_abap_unit_assert=>assert_initial( response-error-code ).
    cl_abap_unit_assert=>assert_equals( exp = zif_mcp_constants=>result_types-complete
                                        act = response-result->get_string( `/resultType` ) ).

    DATA(task) = tasks->get( task_id ).
    cl_abap_unit_assert=>assert_equals( exp = zif_mcp_types=>task_states-cancelled
                                        act = task-status ).
  ENDMETHOD.
ENDCLASS.
