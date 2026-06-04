CLASS ltcl_req_cancel_task DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    METHODS test_valid_task_id   FOR TESTING RAISING cx_static_check.
    METHODS test_missing_task_id FOR TESTING RAISING cx_static_check.
    METHODS test_empty_task_id   FOR TESTING RAISING cx_static_check.
    METHODS test_with_meta       FOR TESTING RAISING cx_static_check.
    METHODS test_no_meta         FOR TESTING RAISING cx_static_check.
ENDCLASS.

CLASS ltcl_req_cancel_task IMPLEMENTATION.
  METHOD test_valid_task_id.
    DATA(json) = zcl_mcp_ajson=>parse( '{"taskId":"0123456789ABCDEF0123456789ABCDEF"}' ).
    DATA(req)  = NEW zcl_mcp_req_cancel_task( json ).

    cl_abap_unit_assert=>assert_equals( exp = '0123456789ABCDEF0123456789ABCDEF'
                                        act = req->get_task_id( ) ).
  ENDMETHOD.

  METHOD test_missing_task_id.
    DATA(json) = zcl_mcp_ajson=>parse( '{}' ).
    TRY.
        DATA(req) = NEW zcl_mcp_req_cancel_task( json ).
        cl_abap_unit_assert=>fail( 'Expected exception for missing taskId' ).
      CATCH zcx_mcp_server. "#EC EMPTY_CATCH
    ENDTRY.
  ENDMETHOD.

  METHOD test_empty_task_id.
    DATA(json) = zcl_mcp_ajson=>parse( '{"taskId":""}' ).
    TRY.
        DATA(req) = NEW zcl_mcp_req_cancel_task( json ).
        cl_abap_unit_assert=>fail( 'Expected exception for empty taskId' ).
      CATCH zcx_mcp_server. "#EC EMPTY_CATCH
    ENDTRY.
  ENDMETHOD.

  METHOD test_with_meta.
    DATA(json) = zcl_mcp_ajson=>parse(
      '{"taskId":"0123456789ABCDEF0123456789ABCDEF","_meta":{"reason":"user_request"}}' ).
    DATA(req) = NEW zcl_mcp_req_cancel_task( json ).

    cl_abap_unit_assert=>assert_equals( exp = '0123456789ABCDEF0123456789ABCDEF'
                                        act = req->get_task_id( ) ).
    cl_abap_unit_assert=>assert_equals( exp = 'user_request'
                                        act = req->get_meta( )->get_string( '/reason' ) ).
  ENDMETHOD.

  METHOD test_no_meta.
    DATA(json) = zcl_mcp_ajson=>parse( '{"taskId":"0123456789ABCDEF0123456789ABCDEF"}' ).
    DATA(req)  = NEW zcl_mcp_req_cancel_task( json ).

    cl_abap_unit_assert=>assert_true( act = req->get_meta( )->is_empty( )
                                      msg = '_meta should be empty when absent' ).
  ENDMETHOD.
ENDCLASS.
