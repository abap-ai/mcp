CLASS ltcl_mcp_resp_v2_task_get DEFINITION DEFERRED.
CLASS zcl_mcp_resp_v2_task_get DEFINITION LOCAL FRIENDS ltcl_mcp_resp_v2_task_get.

CLASS ltcl_mcp_resp_v2_task_get DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    CONSTANTS c_task_id       TYPE string VALUE `00000000000000000000000000000001`.
    CONSTANTS c_task_input_id TYPE string VALUE `00000000000000000000000000000002`.
    CONSTANTS c_task_error_id TYPE string VALUE `00000000000000000000000000000003`.

    CONSTANTS c_text_done     TYPE string VALUE `Task complete` ##NO_TEXT.
    CONSTANTS c_msg_completed TYPE string VALUE `Task completed.` ##NO_TEXT.
    CONSTANTS c_msg_waiting   TYPE string VALUE `Task is waiting for client input.` ##NO_TEXT.
    CONSTANTS c_msg_failed    TYPE string VALUE `Task failed.` ##NO_TEXT.
    CONSTANTS c_error_text    TYPE string VALUE `Task failed in ABAP v2 test server.` ##NO_TEXT.

    METHODS completed_task      FOR TESTING RAISING zcx_mcp_ajson_error.
    METHODS input_required_task FOR TESTING RAISING zcx_mcp_ajson_error.
    METHODS failed_task         FOR TESTING RAISING zcx_mcp_ajson_error.
    METHODS cache_and_meta      FOR TESTING RAISING zcx_mcp_ajson_error.

    METHODS assert_json_equals
      IMPORTING !actual  TYPE string
                expected TYPE string
      RAISING   zcx_mcp_ajson_error.
ENDCLASS.


CLASS ltcl_mcp_resp_v2_task_get IMPLEMENTATION.
  METHOD completed_task.
    DATA(task_get) = NEW zcl_mcp_resp_v2_task_get( ).
    DATA(tool_result) = NEW zcl_mcp_resp_v2_tool( ).

    tool_result->set_error( abap_false ).
    tool_result->add_text_content( c_text_done ).

    task_get->set_task( task_id        = c_task_id
                        status         = `completed`
                        status_message = c_msg_completed ).
    task_get->set_result( tool_result->generate_json( ) ).

    DATA(result) = task_get->zif_mcp_modern_result~generate_json( ).

    assert_json_equals(
        actual   = result->stringify( )
        expected = |\{"resultType":"complete","task":\{"taskId":"00000000000000000000000000000001","status":|
        && |"completed","statusMessage":"Task completed."\},"result":\{"content":[\{"type":"text","text":"Task complete"\}],"isError":false\}\}| ).
  ENDMETHOD.

  METHOD input_required_task.
    DATA(task_get) = NEW zcl_mcp_resp_v2_task_get( ).
    DATA(params) = zcl_mcp_ajson=>create_empty( ).

    params->set_string( iv_path = `/mode`
                        iv_val  = `form` ).
    params->set_string( iv_path = `/message`
                        iv_val  = `Confirm task continuation.` ).
    params->set_string( iv_path = `/requestedSchema/type`
                        iv_val  = `object` ).

    task_get->set_task( task_id        = c_task_input_id
                        status         = zif_mcp_types=>task_states-input_required
                        status_message = c_msg_waiting ).
    task_get->set_request_state( `task-state-2` ).
    task_get->add_input_request( request_key = `confirm`
                                 method      = `elicitation/create`
                                 params      = params ).

    DATA(result) = task_get->zif_mcp_modern_result~generate_json( ).

    cl_abap_unit_assert=>assert_equals( exp = c_task_input_id
                                        act = result->get_string( `/task/taskId` ) ).
    cl_abap_unit_assert=>assert_equals( exp = zif_mcp_types=>task_states-input_required
                                        act = result->get_string( `/task/status` ) ).
    cl_abap_unit_assert=>assert_equals( exp = c_msg_waiting
                                        act = result->get_string( `/task/statusMessage` ) ).
    cl_abap_unit_assert=>assert_equals( exp = `task-state-2`
                                        act = result->get_string( `/requestState` ) ).
    cl_abap_unit_assert=>assert_equals( exp = `elicitation/create`
                                        act = result->get_string( `/inputRequests/confirm/method` ) ).
    cl_abap_unit_assert=>assert_equals( exp = `form`
                                        act = result->get_string( `/inputRequests/confirm/params/mode` ) ).
  ENDMETHOD.

  METHOD failed_task.
    DATA(task_get) = NEW zcl_mcp_resp_v2_task_get( ).

    task_get->set_task( task_id        = c_task_error_id
                        status         = `failed`
                        status_message = c_msg_failed ).
    task_get->set_error( code    = zcl_mcp_jsonrpc=>error_codes-internal_error
                         message = c_error_text ).

    DATA(result) = task_get->zif_mcp_modern_result~generate_json( ).

    cl_abap_unit_assert=>assert_equals( exp = c_task_error_id
                                        act = result->get_string( `/task/taskId` ) ).
    cl_abap_unit_assert=>assert_equals( exp = `failed`
                                        act = result->get_string( `/task/status` ) ).
    cl_abap_unit_assert=>assert_equals( exp = c_msg_failed
                                        act = result->get_string( `/task/statusMessage` ) ).
    cl_abap_unit_assert=>assert_equals( exp = zcl_mcp_jsonrpc=>error_codes-internal_error
                                        act = result->get_integer( `/error/code` ) ).
    cl_abap_unit_assert=>assert_equals( exp = c_error_text
                                        act = result->get_string( `/error/message` ) ).
  ENDMETHOD.

  METHOD cache_and_meta.
    DATA(task_get) = NEW zcl_mcp_resp_v2_task_get( ).
    DATA(meta) = zcl_mcp_ajson=>create_empty( ).

    meta->set_string( iv_path = `/source`
                      iv_val  = `unit-test` ).

    task_get->set_task( task_id          = c_task_id
                        status           = `working`
                        status_message   = `Task accepted.`
                        ttl_ms           = 60000
                        poll_interval_ms = 1000 ).
    task_get->set_meta( meta ).
    task_get->set_cache( ttl_ms      = 2500
                         cache_scope = zif_mcp_constants=>cache_scopes-private ).

    DATA(result) = task_get->zif_mcp_modern_result~generate_json( ).

    cl_abap_unit_assert=>assert_equals( exp = c_task_id
                                        act = result->get_string( `/task/taskId` ) ).
    cl_abap_unit_assert=>assert_equals( exp = `working`
                                        act = result->get_string( `/task/status` ) ).
    cl_abap_unit_assert=>assert_equals( exp = 60000
                                        act = result->get_integer( `/task/ttlMs` ) ).
    cl_abap_unit_assert=>assert_equals( exp = 1000
                                        act = result->get_integer( `/task/pollIntervalMs` ) ).
    cl_abap_unit_assert=>assert_equals( exp = 2500
                                        act = result->get_integer( `/ttlMs` ) ).
    cl_abap_unit_assert=>assert_equals( exp = zif_mcp_constants=>cache_scopes-private
                                        act = result->get_string( `/cacheScope` ) ).
    cl_abap_unit_assert=>assert_equals( exp = `unit-test`
                                        act = result->get_string( `/_meta/source` ) ).
  ENDMETHOD.

  METHOD assert_json_equals.
    DATA(actual_obj) = zcl_mcp_ajson=>parse( actual ).
    DATA(expected_obj) = zcl_mcp_ajson=>parse( expected ).

    cl_abap_unit_assert=>assert_equals( exp = expected_obj->stringify( )
                                        act = actual_obj->stringify( ) ).
  ENDMETHOD.
ENDCLASS.
