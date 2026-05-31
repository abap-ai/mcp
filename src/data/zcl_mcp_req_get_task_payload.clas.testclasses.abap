CLASS ltcl_req_get_task_payload DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    METHODS test_valid_task_id   FOR TESTING RAISING cx_static_check.
    METHODS test_missing_task_id FOR TESTING RAISING cx_static_check.
    METHODS test_empty_task_id   FOR TESTING RAISING cx_static_check.
    METHODS test_with_meta       FOR TESTING RAISING cx_static_check.
    METHODS test_no_meta         FOR TESTING RAISING cx_static_check.
ENDCLASS.

CLASS ltcl_req_get_task_payload IMPLEMENTATION.
  METHOD test_valid_task_id.
    DATA(json) = zcl_mcp_ajson=>parse( '{"taskId":"task-payload-42"}' ).
    DATA(req)  = NEW zcl_mcp_req_get_task_payload( json ).

    cl_abap_unit_assert=>assert_equals( exp = 'task-payload-42'
                                        act = req->get_task_id( ) ).
  ENDMETHOD.

  METHOD test_missing_task_id.
    DATA(json) = zcl_mcp_ajson=>parse( '{}' ).
    TRY.
        DATA(req) = NEW zcl_mcp_req_get_task_payload( json ).
        cl_abap_unit_assert=>fail( 'Expected exception for missing taskId' ).
      CATCH zcx_mcp_server. "#EC EMPTY_CATCH
    ENDTRY.
  ENDMETHOD.

  METHOD test_empty_task_id.
    DATA(json) = zcl_mcp_ajson=>parse( '{"taskId":""}' ).
    TRY.
        DATA(req) = NEW zcl_mcp_req_get_task_payload( json ).
        cl_abap_unit_assert=>fail( 'Expected exception for empty taskId' ).
      CATCH zcx_mcp_server. "#EC EMPTY_CATCH
    ENDTRY.
  ENDMETHOD.

  METHOD test_with_meta.
    DATA(json) = zcl_mcp_ajson=>parse(
      '{"taskId":"task-3","_meta":{"clientRef":"ref-x"}}' ).
    DATA(req) = NEW zcl_mcp_req_get_task_payload( json ).

    cl_abap_unit_assert=>assert_equals( exp = 'task-3'
                                        act = req->get_task_id( ) ).
    cl_abap_unit_assert=>assert_equals( exp = 'ref-x'
                                        act = req->get_meta( )->get_string( '/clientRef' ) ).
  ENDMETHOD.

  METHOD test_no_meta.
    DATA(json) = zcl_mcp_ajson=>parse( '{"taskId":"task-3"}' ).
    DATA(req)  = NEW zcl_mcp_req_get_task_payload( json ).

    cl_abap_unit_assert=>assert_true( act = req->get_meta( )->is_empty( )
                                      msg = '_meta should be empty when absent' ).
  ENDMETHOD.
ENDCLASS.
