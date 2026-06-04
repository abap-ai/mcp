CLASS ltcl_req_list_tasks DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    METHODS test_no_cursor   FOR TESTING RAISING cx_static_check.
    METHODS test_with_cursor FOR TESTING RAISING cx_static_check.
    METHODS test_with_meta   FOR TESTING RAISING cx_static_check.
    METHODS test_no_meta     FOR TESTING RAISING cx_static_check.
ENDCLASS.

CLASS ltcl_req_list_tasks IMPLEMENTATION.
  METHOD test_no_cursor.
    DATA(json) = zcl_mcp_ajson=>parse( '{}' ).
    DATA(req)  = NEW zcl_mcp_req_list_tasks( json ).

    cl_abap_unit_assert=>assert_false( act = req->has_cursor( )
                                       msg = 'has_cursor should be false when absent' ).
    cl_abap_unit_assert=>assert_initial( act = req->get_cursor( )
                                         msg = 'cursor should be initial when absent' ).
  ENDMETHOD.

  METHOD test_with_cursor.
    DATA(json) = zcl_mcp_ajson=>parse( '{"cursor":"eyJsYXN0SWQiOiIxMDAifQ=="}' ).
    DATA(req)  = NEW zcl_mcp_req_list_tasks( json ).

    cl_abap_unit_assert=>assert_true( act = req->has_cursor( )
                                      msg = 'has_cursor should be true when present' ).
    cl_abap_unit_assert=>assert_equals( exp = 'eyJsYXN0SWQiOiIxMDAifQ=='
                                        act = req->get_cursor( ) ).
  ENDMETHOD.

  METHOD test_with_meta.
    DATA(json) = zcl_mcp_ajson=>parse(
      '{"cursor":"tok","_meta":{"progressToken":"pt-1"}}' ).
    DATA(req) = NEW zcl_mcp_req_list_tasks( json ).

    cl_abap_unit_assert=>assert_true( req->has_cursor( ) ).
    cl_abap_unit_assert=>assert_false( act = req->get_meta( )->is_empty( )
                                       msg = '_meta should not be empty' ).
    cl_abap_unit_assert=>assert_equals( exp = 'pt-1'
                                        act = req->get_meta( )->get_string( '/progressToken' ) ).
  ENDMETHOD.

  METHOD test_no_meta.
    DATA(json) = zcl_mcp_ajson=>parse( '{"cursor":"tok"}' ).
    DATA(req)  = NEW zcl_mcp_req_list_tasks( json ).

    cl_abap_unit_assert=>assert_true( act = req->get_meta( )->is_empty( )
                                      msg = '_meta should be empty when absent' ).
  ENDMETHOD.
ENDCLASS.
