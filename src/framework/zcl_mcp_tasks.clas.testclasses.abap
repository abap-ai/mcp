CLASS ltcl_tasks DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA cut TYPE REF TO zcl_mcp_tasks.

    CLASS-DATA sql_env TYPE REF TO if_osql_test_environment.

    CONSTANTS area   TYPE zmcp_area   VALUE 'ZTEST'.
    CONSTANTS server TYPE zmcp_server VALUE 'ZTEST_SRV'.

    CLASS-METHODS class_setup    RAISING cx_static_check.
    CLASS-METHODS class_teardown RAISING cx_static_check.

    METHODS setup                          RAISING cx_static_check.
    METHODS teardown                       RAISING cx_static_check.

    METHODS test_create_returns_id         FOR TESTING RAISING cx_static_check.
    METHODS test_create_initial_status     FOR TESTING RAISING cx_static_check.
    METHODS test_create_with_session       FOR TESTING RAISING cx_static_check.
    METHODS test_create_with_ttl           FOR TESTING RAISING cx_static_check.
    METHODS test_get_returns_task          FOR TESTING RAISING cx_static_check.
    METHODS test_get_unknown_raises        FOR TESTING RAISING cx_static_check.
    METHODS test_get_payload               FOR TESTING RAISING cx_static_check.
    METHODS test_get_payload_unknown       FOR TESTING RAISING cx_static_check.
    METHODS test_list_returns_tasks        FOR TESTING RAISING cx_static_check.
    METHODS test_list_scoped_to_server     FOR TESTING RAISING cx_static_check.
    METHODS test_list_pagination           FOR TESTING RAISING cx_static_check.
    METHODS test_list_empty                FOR TESTING RAISING cx_static_check.
    METHODS test_update_status_valid       FOR TESTING RAISING cx_static_check.
    METHODS test_terminal_blocked          FOR TESTING RAISING cx_static_check.
    METHODS test_update_sets_message       FOR TESTING RAISING cx_static_check.
    METHODS test_complete                  FOR TESTING RAISING cx_static_check.
    METHODS test_fail                      FOR TESTING RAISING cx_static_check.
    METHODS test_cancel                    FOR TESTING RAISING cx_static_check.
    METHODS test_delete_outdated_terminal  FOR TESTING RAISING cx_static_check.
    METHODS test_delete_outdated_stuck     FOR TESTING RAISING cx_static_check.
    METHODS test_delete_keeps_active       FOR TESTING RAISING cx_static_check.
    METHODS test_create_stores_created_by  FOR TESTING RAISING cx_static_check.
    METHODS test_get_foreign_user          FOR TESTING RAISING cx_static_check.
    METHODS test_get_payload_foreign_user  FOR TESTING RAISING cx_static_check.
    METHODS test_list_scoped_to_user       FOR TESTING RAISING cx_static_check.
    METHODS test_class_methods_ignore_ownr FOR TESTING RAISING cx_static_check.
    METHODS test_create_with_ttl_exact     FOR TESTING RAISING cx_static_check.
    METHODS test_delete_ttl_crosses_hour   FOR TESTING RAISING cx_static_check.
    METHODS test_cancel_idempotent         FOR TESTING RAISING cx_static_check.
    METHODS test_cancel_foreign_user       FOR TESTING RAISING cx_static_check.

    METHODS make_id RETURNING VALUE(result) TYPE sysuuid_c32
                    RAISING   cx_static_check.

    METHODS insert_task IMPORTING task_id      TYPE sysuuid_c32
                                  !status      TYPE zmcp_task_status
                                  created_at   TYPE timestamp OPTIONAL
                                  last_updated TYPE timestamp OPTIONAL
                                  ttl          TYPE i         DEFAULT 0
                                  created_by   TYPE syuname   OPTIONAL.
ENDCLASS.

CLASS ltcl_tasks IMPLEMENTATION.
  METHOD class_setup.
    DATA tables TYPE if_osql_test_environment=>ty_t_sobjnames.
    APPEND 'ZMCP_TASKS' TO tables.
    sql_env = cl_osql_test_environment=>create( tables ).
  ENDMETHOD.

  METHOD class_teardown.
    sql_env->destroy( ).
  ENDMETHOD.

  METHOD setup.
    sql_env->clear_doubles( ).
    cut = NEW zcl_mcp_tasks( area = area server = server ).
  ENDMETHOD.

  METHOD teardown.
  ENDMETHOD.

  METHOD make_id.
    result = cl_system_uuid=>create_uuid_c32_static( ).
  ENDMETHOD.

  METHOD insert_task.
    DATA ts TYPE timestamp.
    IF created_at IS INITIAL.
      GET TIME STAMP FIELD ts.
    ELSE.
      ts = created_at.
    ENDIF.

    DATA row TYPE zmcp_tasks.
    row-task_id       = task_id.
    row-area          = area.
    row-server        = server.
    row-created_by    = COND #( WHEN created_by IS INITIAL THEN sy-uname
                               ELSE created_by ).
    row-status        = status.
    row-created_at    = ts.
    row-last_updated  = COND #( WHEN last_updated IS INITIAL THEN ts ELSE last_updated ).
    row-ttl           = ttl.
    row-poll_interval = 5.

    DATA rows TYPE TABLE OF zmcp_tasks.
    APPEND row TO rows.
    sql_env->insert_test_data( rows ).
  ENDMETHOD.

  METHOD test_create_returns_id.
    DATA(task_id) = cut->create_task( tool_name = 'my_tool' ).
    cl_abap_unit_assert=>assert_not_initial( task_id ).
  ENDMETHOD.

  METHOD test_create_initial_status.
    DATA(task_id) = cut->create_task( tool_name = 'my_tool' ).
    DATA(task)    = cut->get( task_id ).

    cl_abap_unit_assert=>assert_equals( exp = zcl_mcp_tasks=>status_working
                                        act = task-status ).
    cl_abap_unit_assert=>assert_not_initial( task-created_at ).
    cl_abap_unit_assert=>assert_not_initial( task-last_updated ).
  ENDMETHOD.

  METHOD test_create_with_session.
    DATA(session_id) = make_id( ).
    DATA(task_id)    = cut->create_task( tool_name  = 'my_tool'
                                         session_id = session_id ).

    SELECT SINGLE session_id FROM zmcp_tasks
      WHERE task_id = @task_id
      INTO @DATA(stored).
    cl_abap_unit_assert=>assert_equals( exp = session_id act = stored ).
  ENDMETHOD.

  METHOD test_create_with_ttl.
    DATA(task_id) = cut->create_task( tool_name     = 'my_tool'
                                       ttl           = 3600
                                       poll_interval = 10 ).
    DATA(task) = cut->get( task_id ).
    cl_abap_unit_assert=>assert_equals( exp = 4000 act = task-ttl ).
    cl_abap_unit_assert=>assert_equals( exp = 10   act = task-poll_interval ).
  ENDMETHOD.

  METHOD test_get_returns_task.
    DATA(task_id) = cut->create_task( tool_name = 'my_tool' ).
    DATA(task)    = cut->get( task_id ).

    cl_abap_unit_assert=>assert_equals( exp = task_id                         act = task-task_id ).
    cl_abap_unit_assert=>assert_equals( exp = zcl_mcp_tasks=>status_working act = task-status ).
  ENDMETHOD.

  METHOD test_get_unknown_raises.
    TRY.
        cut->get( make_id( ) ).
        cl_abap_unit_assert=>fail( 'Expected zcx_mcp_server for unknown task' ).
      CATCH zcx_mcp_server ##NO_HANDLER.
    ENDTRY.
  ENDMETHOD.

  METHOD test_get_payload.
    DATA(task_id) = cut->create_task( tool_name = 'my_tool' ).
    DATA(payload) = zcl_mcp_ajson=>create_empty( ).
    payload->set_string( iv_path = '/result' iv_val = 'done' ).
    zcl_mcp_tasks=>set_payload( task_id = task_id payload = payload ).

    DATA(loaded) = cut->get_payload( task_id ).
    cl_abap_unit_assert=>assert_equals( exp = 'done'
                                        act = loaded->get_string( '/result' ) ).
  ENDMETHOD.

  METHOD test_get_payload_unknown.
    TRY.
        cut->get_payload( make_id( ) ).
        cl_abap_unit_assert=>fail( 'Expected zcx_mcp_server for unknown task' ).
      CATCH zcx_mcp_server ##NO_HANDLER.
    ENDTRY.
  ENDMETHOD.

  METHOD test_list_returns_tasks.
    cut->create_task( 'tool_a' ).
    cut->create_task( 'tool_b' ).

    DATA(list) = cut->list( ).
    cl_abap_unit_assert=>assert_true( act = xsdbool( lines( list-tasks ) >= 2 ) ).
    cl_abap_unit_assert=>assert_initial( list-next_cursor ).
  ENDMETHOD.

  METHOD test_list_scoped_to_server.
    DATA(other_id) = make_id( ).
    DATA other_row  TYPE zmcp_tasks.
    DATA other_rows TYPE TABLE OF zmcp_tasks.

    other_row-task_id = other_id.
    other_row-area    = 'ZOTHER'.
    other_row-server  = 'ZOTHER_SRV'.
    other_row-status  = zcl_mcp_tasks=>status_working.
    GET TIME STAMP FIELD other_row-created_at.
    other_row-last_updated = other_row-created_at.
    APPEND other_row TO other_rows.
    sql_env->insert_test_data( other_rows ).

    cut->create_task( 'my_tool' ).

    DATA(list) = cut->list( ).
    LOOP AT list-tasks INTO DATA(task).
      cl_abap_unit_assert=>assert_false( act = xsdbool( task-task_id = other_id )
                                         msg = 'Other server task must not appear' ).
    ENDLOOP.
  ENDMETHOD.

  METHOD test_list_pagination.
    DATA base_ts TYPE timestamp VALUE '20250101120000'.
    DATA count   TYPE i.

    count = zcl_mcp_tasks=>page_size + 2.

    DO count TIMES.
      insert_task( task_id    = make_id( )
                   status     = zcl_mcp_tasks=>status_working
                   created_at = base_ts + sy-index ).
    ENDDO.

    DATA(page1) = cut->list( ).
    cl_abap_unit_assert=>assert_equals( exp = zcl_mcp_tasks=>page_size
                                        act = lines( page1-tasks )
                                        msg = 'First page must be full' ).
    cl_abap_unit_assert=>assert_not_initial( page1-next_cursor ).

    DATA(page2) = cut->list( cursor = page1-next_cursor ).
    cl_abap_unit_assert=>assert_true( act = xsdbool( lines( page2-tasks ) >= 2 )
                                      msg = 'Second page must have remaining tasks' ).
    cl_abap_unit_assert=>assert_initial( page2-next_cursor ).
  ENDMETHOD.

  METHOD test_list_empty.
    DATA(list) = cut->list( ).
    cl_abap_unit_assert=>assert_initial( lines( list-tasks ) ).
    cl_abap_unit_assert=>assert_initial( list-next_cursor ).
  ENDMETHOD.

  METHOD test_update_status_valid.
    " working --> cancelled is a valid transition
    DATA(task_id) = cut->create_task( tool_name = 'my_tool' ).
    zcl_mcp_tasks=>update_status( task_id = task_id
                                  status  = zcl_mcp_tasks=>status_cancelled ).
    DATA(task) = cut->get( task_id ).
    cl_abap_unit_assert=>assert_equals( exp = zcl_mcp_tasks=>status_cancelled
                                        act = task-status ).
  ENDMETHOD.

  METHOD test_terminal_blocked.
    " working --> completed is valid, then no further transitions allowed
    DATA(task_id) = cut->create_task( tool_name = 'my_tool' ).
    zcl_mcp_tasks=>update_status( task_id = task_id
                                  status  = zcl_mcp_tasks=>status_completed ).
    TRY.
        zcl_mcp_tasks=>update_status( task_id = task_id
                                      status  = zcl_mcp_tasks=>status_working ).
        cl_abap_unit_assert=>fail( 'Expected zcx_mcp_server - terminal state is final' ).
      CATCH zcx_mcp_server ##NO_HANDLER.
    ENDTRY.
  ENDMETHOD.

  METHOD test_update_sets_message.
    " working --> failed carries a status message
    DATA(task_id) = cut->create_task( tool_name = 'my_tool' ).
    zcl_mcp_tasks=>update_status( task_id = task_id
                                  status  = zcl_mcp_tasks=>status_failed
                                  message = 'Something went wrong' ).
    DATA(task) = cut->get( task_id ).
    cl_abap_unit_assert=>assert_equals( exp = 'Something went wrong'
                                        act = task-status_message ).
  ENDMETHOD.

  METHOD test_complete.
    " Task starts at working - no intermediate status update needed
    DATA(task_id) = cut->create_task( tool_name = 'my_tool' ).

    DATA(task_result) = NEW zcl_mcp_resp_task_payload( ).
    task_result->add_text_content( 'Task done' ).
    DATA(sc) = zcl_mcp_ajson=>create_empty( ).
    sc->set_string( iv_path = '/output'
                    iv_val  = 'hello' ).
    task_result->set_structured_content( sc ).

    zcl_mcp_tasks=>complete( task_id = task_id
                             result  = task_result ).

    DATA(task) = cut->get( task_id ).
    cl_abap_unit_assert=>assert_equals( exp = zcl_mcp_tasks=>status_completed
                                        act = task-status ).

    DATA(loaded) = cut->get_payload( task_id ).
    cl_abap_unit_assert=>assert_true( act = loaded->exists( 'content' )
                                      msg = 'Stored payload must have content array' ).
    cl_abap_unit_assert=>assert_equals( exp = 'hello'
                                        act = loaded->get_string( 'structuredContent/output' ) ).
    cl_abap_unit_assert=>assert_equals( exp = 'Task done'
                                        act = loaded->get_string( 'content/1/text' ) ).
  ENDMETHOD.

  METHOD test_fail.
    DATA(task_id) = cut->create_task( tool_name = 'my_tool' ).
    zcl_mcp_tasks=>fail( task_id = task_id message = 'Something went wrong' ).

    DATA(task) = cut->get( task_id ).
    cl_abap_unit_assert=>assert_equals( exp = zcl_mcp_tasks=>status_failed
                                        act = task-status ).
    cl_abap_unit_assert=>assert_equals( exp = 'Something went wrong'
                                        act = task-status_message ).
  ENDMETHOD.

  METHOD test_cancel.
    DATA(task_id) = cut->create_task( tool_name = 'my_tool' ).
    zcl_mcp_tasks=>cancel( task_id ).

    DATA(task) = cut->get( task_id ).
    cl_abap_unit_assert=>assert_equals( exp = zcl_mcp_tasks=>status_cancelled
                                        act = task-status ).
  ENDMETHOD.

  METHOD test_delete_outdated_terminal.
    DATA(task_id) = make_id( ).
    insert_task( task_id      = task_id
                 status       = zcl_mcp_tasks=>status_completed
                 created_at   = '20240101000000'
                 last_updated = '20240101000001'
                 ttl          = 1 ).

    DATA(deleted) = zcl_mcp_tasks=>delete_outdated_tasks( ).
    cl_abap_unit_assert=>assert_true( xsdbool( deleted > 0 ) ).

    SELECT SINGLE task_id FROM zmcp_tasks WHERE task_id = @task_id INTO @DATA(check).
    cl_abap_unit_assert=>assert_initial( check ).
  ENDMETHOD.

  METHOD test_delete_outdated_stuck.
    DATA(task_id) = make_id( ).
    insert_task( task_id    = task_id
                 status     = zcl_mcp_tasks=>status_working
                 created_at = '20240101000000' ).

    DATA(deleted) = zcl_mcp_tasks=>delete_outdated_tasks( ).
    cl_abap_unit_assert=>assert_true( xsdbool( deleted > 0 ) ).

    SELECT SINGLE task_id FROM zmcp_tasks WHERE task_id = @task_id INTO @DATA(check).
    cl_abap_unit_assert=>assert_initial( check ).
  ENDMETHOD.

  METHOD test_delete_keeps_active.
    DATA(task_id) = cut->create_task( tool_name = 'my_tool' ).

    zcl_mcp_tasks=>delete_outdated_tasks( ).

    DATA(task) = cut->get( task_id ).
    cl_abap_unit_assert=>assert_equals( exp = task_id act = task-task_id ).
  ENDMETHOD.

  METHOD test_create_stores_created_by.
    DATA(task_id) = cut->create_task( tool_name = 'my_tool' ).

    SELECT SINGLE created_by FROM zmcp_tasks
      WHERE task_id = @task_id
      INTO @DATA(stored_user).

    cl_abap_unit_assert=>assert_subrc( exp = 0 act = sy-subrc ).
    cl_abap_unit_assert=>assert_equals(
      exp = sy-uname
      act = stored_user
      msg = 'Task must be stamped with creating user' ).
  ENDMETHOD.

  METHOD test_get_foreign_user.
    DATA(task_id) = make_id( ).
    insert_task( task_id    = task_id
                 status     = zcl_mcp_tasks=>status_working
                 created_by = 'OTHER_USER' ).

    TRY.
        cut->get( task_id ).
        cl_abap_unit_assert=>fail( 'Expected zcx_mcp_server for foreign task' ).
      CATCH zcx_mcp_server ##NO_HANDLER.
    ENDTRY.
  ENDMETHOD.

  METHOD test_get_payload_foreign_user.
    DATA(task_id) = make_id( ).
    insert_task( task_id    = task_id
                 status     = zcl_mcp_tasks=>status_completed
                 created_by = 'OTHER_USER' ).

    TRY.
        cut->get_payload( task_id ).
        cl_abap_unit_assert=>fail( 'Expected zcx_mcp_server for foreign task payload' ).
      CATCH zcx_mcp_server ##NO_HANDLER.
    ENDTRY.
  ENDMETHOD.

  METHOD test_list_scoped_to_user.
    DATA(foreign_id) = make_id( ).
    insert_task( task_id    = foreign_id
                 status     = zcl_mcp_tasks=>status_working
                 created_by = 'OTHER_USER' ).

    DATA(own_id) = cut->create_task( tool_name = 'my_tool' ).

    DATA(list) = cut->list( ).

    cl_abap_unit_assert=>assert_true( act = xsdbool( line_exists( list-tasks[ task_id = own_id ] ) )
                                      msg = 'Own task must appear in list' ).

    cl_abap_unit_assert=>assert_false( act = xsdbool( line_exists( list-tasks[ task_id = foreign_id ] ) )
                                       msg = 'Foreign user task must not appear in list' ).
  ENDMETHOD.

  METHOD test_class_methods_ignore_ownr.
    DATA(task_id) = make_id( ).
    insert_task( task_id    = task_id
                 status     = zcl_mcp_tasks=>status_working
                 created_by = 'OTHER_USER' ).

    " complete must succeed even though we are not OTHER_USER
    DATA(task_result) = NEW zcl_mcp_resp_task_payload( ).
    task_result->add_text_content( 'Background job finished' ).
    TRY.
        zcl_mcp_tasks=>complete( task_id = task_id
                                 result  = task_result ).
      CATCH zcx_mcp_server INTO DATA(error).
        cl_abap_unit_assert=>fail( |Class method must not enforce ownership: { error->get_text( ) }| ).
    ENDTRY.

    SELECT SINGLE status FROM zmcp_tasks
      WHERE task_id = @task_id
      INTO @DATA(new_status).
    cl_abap_unit_assert=>assert_equals( exp = zcl_mcp_tasks=>status_completed
                                        act = new_status
                                        msg = 'Background completion must persist regardless of ownership' ).
  ENDMETHOD.

  METHOD test_create_with_ttl_exact.
    DATA(task_id) = cut->create_task( tool_name = 'my_tool'
                                      ttl       = 60000 ).
    DATA(task) = cut->get( task_id ).

    cl_abap_unit_assert=>assert_equals( exp = 60000
                                        act = task-ttl ).
  ENDMETHOD.

  METHOD test_delete_ttl_crosses_hour.
    DATA(task_id) = make_id( ).
    insert_task( task_id      = task_id
                 status       = zcl_mcp_tasks=>status_completed
                 created_at   = '20250101003000'
                 last_updated = '20250101003000'
                 ttl          = 3600 ).

    DATA(deleted) = zcl_mcp_tasks=>delete_outdated_tasks( ).
    cl_abap_unit_assert=>assert_true( xsdbool( deleted > 0 ) ).

    SELECT SINGLE task_id FROM zmcp_tasks
      WHERE task_id = @task_id
      INTO @DATA(check).
    cl_abap_unit_assert=>assert_initial( check ).
  ENDMETHOD.

  METHOD test_cancel_idempotent.
    DATA(task_id) = cut->create_task( tool_name = 'my_tool' ).

    zcl_mcp_tasks=>cancel( task_id ).
    zcl_mcp_tasks=>cancel( task_id ).

    DATA(task) = cut->get( task_id ).

    cl_abap_unit_assert=>assert_equals( exp = zcl_mcp_tasks=>status_cancelled
                                        act = task-status ).
  ENDMETHOD.

  METHOD test_cancel_foreign_user.
    DATA(task_id) = make_id( ).

    insert_task( task_id    = task_id
                 status     = zcl_mcp_tasks=>status_working
                 created_by = 'OTHER_USER' ).

    TRY.
        zcl_mcp_tasks=>cancel( task_id ).
        cl_abap_unit_assert=>fail( 'Expected task_not_found for foreign user task' ).
      CATCH zcx_mcp_server INTO DATA(error).
        cl_abap_unit_assert=>assert_equals( exp = zcx_mcp_server=>task_not_found
                                            act = error->if_t100_message~t100key ).
    ENDTRY.
  ENDMETHOD.

ENDCLASS.
