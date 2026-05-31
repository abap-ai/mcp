CLASS ltcl_resp_get_task DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA cut TYPE REF TO zcl_mcp_resp_get_task.

    METHODS setup.
    METHODS test_basic_task           FOR TESTING RAISING cx_static_check.
    METHODS test_task_null_ttl        FOR TESTING RAISING cx_static_check.
    METHODS test_task_optional_fields FOR TESTING RAISING cx_static_check.
    METHODS test_all_statuses         FOR TESTING RAISING cx_static_check.
    METHODS test_with_meta            FOR TESTING RAISING cx_static_check.

    METHODS make_task
      RETURNING VALUE(result) TYPE zif_mcp_types=>task.
ENDCLASS.

CLASS ltcl_resp_get_task IMPLEMENTATION.
  METHOD setup.
    cut = NEW #( ).
  ENDMETHOD.

  METHOD make_task.
    result-task_id      = 'task-get-1'.
    result-status       = zif_mcp_types=>task_states-completed.
    result-created_at   = '20251125090000'.
    result-last_updated = '20251125090500'.
    result-ttl_is_null  = abap_false.
    result-ttl          = 0.
  ENDMETHOD.

  METHOD test_basic_task.
    " GetTaskResult = Result & Task - fields at root level, no /task wrapper
    cut->set_task( make_task( ) ).
    DATA(json) = cut->zif_mcp_internal~generate_json( ).

    cl_abap_unit_assert=>assert_equals( exp = 'task-get-1'
                                        act = json->get_string( '/taskId' ) ).
    cl_abap_unit_assert=>assert_equals( exp = zif_mcp_types=>task_states-completed
                                        act = json->get_string( '/status' ) ).
    cl_abap_unit_assert=>assert_equals( exp = '2025-11-25T09:00:00Z'
                                        act = json->get_string( '/createdAt' ) ).
    cl_abap_unit_assert=>assert_equals( exp = '2025-11-25T09:05:00Z'
                                        act = json->get_string( '/lastUpdatedAt' ) ).
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
    task-status_message = 'Tool call completed successfully'.
    task-poll_interval  = 5000.
    cut->set_task( task ).

    DATA(json) = cut->zif_mcp_internal~generate_json( ).
    cl_abap_unit_assert=>assert_equals( exp = 'Tool call completed successfully'
                                        act = json->get_string( '/statusMessage' ) ).
    cl_abap_unit_assert=>assert_equals( exp = 5000
                                        act = json->get_integer( '/pollInterval' ) ).
  ENDMETHOD.

  METHOD test_all_statuses.
    " Verify each task_states constant round-trips correctly
    DATA task TYPE zif_mcp_types=>task.
    task-created_at   = '20251125000000'.
    task-last_updated = '20251125000000'.

    DATA statuses TYPE STANDARD TABLE OF zif_mcp_types=>task_state WITH EMPTY KEY.
    APPEND zif_mcp_types=>task_states-working        TO statuses.
    APPEND zif_mcp_types=>task_states-input_required TO statuses.
    APPEND zif_mcp_types=>task_states-completed      TO statuses.
    APPEND zif_mcp_types=>task_states-failed         TO statuses.
    APPEND zif_mcp_types=>task_states-cancelled      TO statuses.

    LOOP AT statuses ASSIGNING FIELD-SYMBOL(<status>).
      task-task_id = |task-{ sy-tabix }|.
      task-status  = <status>.
      cut = NEW #( ).
      cut->set_task( task ).
      DATA(json) = cut->zif_mcp_internal~generate_json( ).
      cl_abap_unit_assert=>assert_equals( exp = <status>
                                          act = json->get_string( '/status' )
                                          msg = |Status { <status> } should round-trip| ).
    ENDLOOP.
  ENDMETHOD.

  METHOD test_with_meta.
    cut->set_task( make_task( ) ).
    DATA(meta) = zcl_mcp_ajson=>create_empty( ).
    meta->set( iv_path = '/cacheKey' iv_val = 'ck-42' ).
    cut->set_meta( meta ).

    DATA(json) = cut->zif_mcp_internal~generate_json( ).
    cl_abap_unit_assert=>assert_equals( exp = 'ck-42'
                                        act = json->get_string( '/_meta/cacheKey' ) ).
    cl_abap_unit_assert=>assert_equals( exp = 'task-get-1'
                                        act = json->get_string( '/taskId' ) ).
  ENDMETHOD.
ENDCLASS.
