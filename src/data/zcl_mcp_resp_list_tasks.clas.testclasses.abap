CLASS ltcl_resp_list_tasks DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA cut TYPE REF TO zcl_mcp_resp_list_tasks.

    METHODS setup.
    METHODS test_empty_list           FOR TESTING RAISING cx_static_check.
    METHODS test_single_task          FOR TESTING RAISING cx_static_check.
    METHODS test_multiple_tasks       FOR TESTING RAISING cx_static_check.
    METHODS test_task_null_ttl        FOR TESTING RAISING cx_static_check.
    METHODS test_with_next_cursor     FOR TESTING RAISING cx_static_check.
    METHODS test_with_meta            FOR TESTING RAISING cx_static_check.

    METHODS make_task
      IMPORTING task_id       TYPE string
                status        TYPE zif_mcp_types=>task_state
      RETURNING VALUE(result) TYPE zif_mcp_types=>task.
ENDCLASS.

CLASS ltcl_resp_list_tasks IMPLEMENTATION.
  METHOD setup.
    cut = NEW #( ).
  ENDMETHOD.

  METHOD make_task.
    result-task_id      = task_id.
    result-status       = status.
    result-created_at   = '20251125100000'.
    result-last_updated = '20251125100001'.
    result-ttl_is_null  = abap_false.
    result-ttl          = 30000.
  ENDMETHOD.

  METHOD test_empty_list.
    cut->set_tasks( VALUE #( ) ).
    DATA(json) = cut->zif_mcp_internal~generate_json( ).

    cl_abap_unit_assert=>assert_true( act = json->exists( '/tasks' )
                                      msg = 'tasks array should exist' ).
    cl_abap_unit_assert=>assert_equals( exp = 0
                                        act = lines( json->members( '/tasks' ) )
                                        msg = 'tasks array should be empty' ).
    cl_abap_unit_assert=>assert_false( act = json->exists( '/nextCursor' )
                                       msg = 'nextCursor absent when not set' ).
  ENDMETHOD.

  METHOD test_single_task.
    DATA tasks TYPE zif_mcp_types=>task_list.
    APPEND make_task( task_id = 'task-1'
                      status  = zif_mcp_types=>task_states-working ) TO tasks.
    cut->set_tasks( tasks ).

    DATA(json) = cut->zif_mcp_internal~generate_json( ).
    cl_abap_unit_assert=>assert_equals( exp = 'task-1'
                                        act = json->get_string( '/tasks/1/taskId' ) ).
    cl_abap_unit_assert=>assert_equals( exp = zif_mcp_types=>task_states-working
                                        act = json->get_string( '/tasks/1/status' ) ).
    cl_abap_unit_assert=>assert_equals( exp = 30000
                                        act = json->get_integer( '/tasks/1/ttl' ) ).
  ENDMETHOD.

  METHOD test_multiple_tasks.
    DATA tasks TYPE zif_mcp_types=>task_list.
    APPEND make_task( task_id = 'task-a'
                      status  = zif_mcp_types=>task_states-completed ) TO tasks.
    APPEND make_task( task_id = 'task-b'
                      status  = zif_mcp_types=>task_states-failed ) TO tasks.
    APPEND make_task( task_id = 'task-c'
                      status  = zif_mcp_types=>task_states-working ) TO tasks.
    cut->set_tasks( tasks ).

    DATA(json) = cut->zif_mcp_internal~generate_json( ).
    cl_abap_unit_assert=>assert_equals( exp = 3
                                        act = lines( json->members( '/tasks' ) )
                                        msg = 'should have 3 tasks' ).
    cl_abap_unit_assert=>assert_equals( exp = 'task-a'
                                        act = json->get_string( '/tasks/1/taskId' ) ).
    cl_abap_unit_assert=>assert_equals( exp = 'task-b'
                                        act = json->get_string( '/tasks/2/taskId' ) ).
    cl_abap_unit_assert=>assert_equals( exp = 'task-c'
                                        act = json->get_string( '/tasks/3/taskId' ) ).
    cl_abap_unit_assert=>assert_equals( exp = zif_mcp_types=>task_states-failed
                                        act = json->get_string( '/tasks/2/status' ) ).
  ENDMETHOD.

  METHOD test_task_null_ttl.
    DATA tasks TYPE zif_mcp_types=>task_list.
    DATA task  TYPE zif_mcp_types=>task.
    task = make_task( task_id = 'task-unlimited'
                      status  = zif_mcp_types=>task_states-working ).
    task-ttl_is_null = abap_true.
    APPEND task TO tasks.
    cut->set_tasks( tasks ).

    DATA(str) = cut->zif_mcp_internal~generate_json( )->stringify( ).
    cl_abap_unit_assert=>assert_char_cp( exp = '*"ttl":null*'
                                         act = str
                                         msg = 'ttl should serialize as null' ).
  ENDMETHOD.

  METHOD test_with_next_cursor.
    DATA tasks TYPE zif_mcp_types=>task_list.
    APPEND make_task( task_id = 'task-1'
                      status  = zif_mcp_types=>task_states-completed ) TO tasks.
    cut->set_tasks( tasks ).
    cut->set_next_cursor( 'eyJsYXN0SWQiOiJ0YXNrLTEifQ==' ).

    DATA(json) = cut->zif_mcp_internal~generate_json( ).
    cl_abap_unit_assert=>assert_equals( exp = 'eyJsYXN0SWQiOiJ0YXNrLTEifQ=='
                                        act = json->get_string( '/nextCursor' ) ).
  ENDMETHOD.

  METHOD test_with_meta.
    cut->set_tasks( VALUE #( ) ).
    DATA(meta) = zcl_mcp_ajson=>create_empty( ).
    meta->set( iv_path = '/page' iv_val = '2' ).
    cut->set_meta( meta ).

    DATA(json) = cut->zif_mcp_internal~generate_json( ).
    cl_abap_unit_assert=>assert_equals( exp = '2'
                                        act = json->get_string( '/_meta/page' ) ).
  ENDMETHOD.
ENDCLASS.
