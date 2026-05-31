*"* use this source file for your ABAP unit test classes
CLASS ltcl_resp_create_task DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA cut TYPE REF TO zcl_mcp_resp_create_task.

    METHODS setup.
    METHODS test_basic_task          FOR TESTING RAISING cx_static_check.
    METHODS test_task_null_ttl       FOR TESTING RAISING cx_static_check.
    METHODS test_task_with_ttl       FOR TESTING RAISING cx_static_check.
    METHODS test_task_optional_fields FOR TESTING RAISING cx_static_check.
    METHODS test_with_meta           FOR TESTING RAISING cx_static_check.

    METHODS make_task
      RETURNING VALUE(result) TYPE zif_mcp_types=>task.
ENDCLASS.

CLASS ltcl_resp_create_task IMPLEMENTATION.
  METHOD setup.
    cut = NEW #( ).
  ENDMETHOD.

  METHOD make_task.
    result-task_id      = 'task-create-1'.
    result-status       = zif_mcp_types=>task_states-working.
    result-created_at   = '20251125100000'.
    result-last_updated = '20251125100001'.
    result-ttl_is_null  = abap_false.
    result-ttl          = 30000.
  ENDMETHOD.

  METHOD test_basic_task.
    cut->set_task( make_task( ) ).
    DATA(json) = cut->zif_mcp_internal~generate_json( ).

    " Task fields are wrapped under /task
    cl_abap_unit_assert=>assert_equals( exp = 'task-create-1'
                                        act = json->get_string( '/task/taskId' ) ).
    cl_abap_unit_assert=>assert_equals( exp = zif_mcp_types=>task_states-working
                                        act = json->get_string( '/task/status' ) ).
    cl_abap_unit_assert=>assert_equals( exp = '2025-11-25T10:00:00Z'
                                        act = json->get_string( '/task/createdAt' ) ).
    cl_abap_unit_assert=>assert_equals( exp = '2025-11-25T10:00:01Z'
                                        act = json->get_string( '/task/lastUpdatedAt' ) ).
    cl_abap_unit_assert=>assert_equals( exp = 30000
                                        act = json->get_integer( '/task/ttl' ) ).
    cl_abap_unit_assert=>assert_false( act = json->exists( '/task/pollInterval' )
                                       msg = 'pollInterval absent when not set' ).
    cl_abap_unit_assert=>assert_false( act = json->exists( '/task/statusMessage' )
                                       msg = 'statusMessage absent when not set' ).
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

  METHOD test_task_with_ttl.
    DATA(task) = make_task( ).
    task-ttl = 60000.
    cut->set_task( task ).

    DATA(json) = cut->zif_mcp_internal~generate_json( ).
    cl_abap_unit_assert=>assert_equals( exp = 60000
                                        act = json->get_integer( '/task/ttl' ) ).
  ENDMETHOD.

  METHOD test_task_optional_fields.
    DATA(task) = make_task( ).
    task-status_message = 'Processing SAP data'.
    task-poll_interval  = 2000.
    cut->set_task( task ).

    DATA(json) = cut->zif_mcp_internal~generate_json( ).
    cl_abap_unit_assert=>assert_equals( exp = 'Processing SAP data'
                                        act = json->get_string( '/task/statusMessage' ) ).
    cl_abap_unit_assert=>assert_equals( exp = 2000
                                        act = json->get_integer( '/task/pollInterval' ) ).
  ENDMETHOD.

  METHOD test_with_meta.
    cut->set_task( make_task( ) ).
    DATA(meta) = zcl_mcp_ajson=>create_empty( ).
    meta->set( iv_path = '/requestId' iv_val = 'req-99' ).
    cut->set_meta( meta ).

    DATA(json) = cut->zif_mcp_internal~generate_json( ).
    cl_abap_unit_assert=>assert_equals( exp = 'req-99'
                                        act = json->get_string( '/_meta/requestId' ) ).
    " Task still correctly nested
    cl_abap_unit_assert=>assert_equals( exp = 'task-create-1'
                                        act = json->get_string( '/task/taskId' ) ).
  ENDMETHOD.
ENDCLASS.
