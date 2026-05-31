CLASS ltcl_resp_cancel_task DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA cut TYPE REF TO zcl_mcp_resp_cancel_task.

    METHODS setup.
    METHODS test_basic_task           FOR TESTING RAISING cx_static_check.
    METHODS test_task_null_ttl        FOR TESTING RAISING cx_static_check.
    METHODS test_task_optional_fields FOR TESTING RAISING cx_static_check.
    METHODS test_with_meta            FOR TESTING RAISING cx_static_check.

    METHODS make_task
      RETURNING VALUE(result) TYPE zif_mcp_types=>task.
ENDCLASS.

CLASS ltcl_resp_cancel_task IMPLEMENTATION.
  METHOD setup.
    cut = NEW #( ).
  ENDMETHOD.

  METHOD make_task.
    result-task_id      = 'task-cancel-1'.
    result-status       = zif_mcp_types=>task_states-cancelled.
    result-created_at   = '20251125080000'.
    result-last_updated = '20251125080100'.
    result-ttl_is_null  = abap_false.
    result-ttl          = 10000.
  ENDMETHOD.

  METHOD test_basic_task.
    " CancelTaskResult = Result & Task - fields at root level
    cut->set_task( make_task( ) ).
    DATA(json) = cut->zif_mcp_internal~generate_json( ).

    cl_abap_unit_assert=>assert_equals( exp = 'task-cancel-1'
                                        act = json->get_string( '/taskId' ) ).
    cl_abap_unit_assert=>assert_equals( exp = zif_mcp_types=>task_states-cancelled
                                        act = json->get_string( '/status' ) ).
    cl_abap_unit_assert=>assert_equals( exp = 10000
                                        act = json->get_integer( '/ttl' ) ).
    cl_abap_unit_assert=>assert_false( act = json->exists( '/task' )
                                       msg = 'no /task wrapper - fields at root' ).
  ENDMETHOD.

  METHOD test_task_null_ttl.
    DATA(task) = make_task( ).
    task-ttl_is_null = abap_true.
    cut->set_task( task ).

    DATA(str) = cut->zif_mcp_internal~generate_json( )->stringify( ).
    cl_abap_unit_assert=>assert_char_cp( exp = '*"ttl":null*'
                                         act = str
                                         msg = 'ttl should serialize as null' ).
  ENDMETHOD.

  METHOD test_task_optional_fields.
    DATA(task) = make_task( ).
    task-status_message = 'Cancelled by user'.
    task-poll_interval  = 1000.
    cut->set_task( task ).

    DATA(json) = cut->zif_mcp_internal~generate_json( ).
    cl_abap_unit_assert=>assert_equals( exp = 'Cancelled by user'
                                        act = json->get_string( '/statusMessage' ) ).
    cl_abap_unit_assert=>assert_equals( exp = 1000
                                        act = json->get_integer( '/pollInterval' ) ).
  ENDMETHOD.

  METHOD test_with_meta.
    cut->set_task( make_task( ) ).
    DATA(meta) = zcl_mcp_ajson=>create_empty( ).
    meta->set( iv_path = '/cancelledBy' iv_val = 'user-123' ).
    cut->set_meta( meta ).

    DATA(json) = cut->zif_mcp_internal~generate_json( ).
    cl_abap_unit_assert=>assert_equals( exp = 'user-123'
                                        act = json->get_string( '/_meta/cancelledBy' ) ).
    cl_abap_unit_assert=>assert_equals( exp = 'task-cancel-1'
                                        act = json->get_string( '/taskId' ) ).
  ENDMETHOD.
ENDCLASS.
