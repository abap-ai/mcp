CLASS ltcl_mcp_req_list_prompts DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    METHODS test_with_cursor    FOR TESTING RAISING zcx_mcp_ajson_error zcx_mcp_server.
    METHODS test_without_cursor FOR TESTING RAISING zcx_mcp_ajson_error zcx_mcp_server.
ENDCLASS.

CLASS ltcl_mcp_req_list_prompts IMPLEMENTATION.
  METHOD test_with_cursor.
    " Given
    DATA(json) = zcl_mcp_ajson=>create_empty( ).
    json->set_string( iv_path = 'cursor'
                      iv_val  = 'eyJwYWdlIjogMn0=' ). " Base64 encoded cursor

    " When
    DATA req TYPE REF TO zcl_mcp_req_list_prompts.
    req = NEW #( json ).

    " Then
    cl_abap_unit_assert=>assert_true( act = req->has_cursor( )
                                      msg = 'Cursor should be recognized' ).

    cl_abap_unit_assert=>assert_equals( exp = 'eyJwYWdlIjogMn0='
                                        act = req->get_cursor( )
                                        msg = 'Cursor value should be correctly parsed' ).
  ENDMETHOD.

  METHOD test_without_cursor.
    " Given
    DATA(json) = zcl_mcp_ajson=>create_empty( ).
    " No cursor parameter

    " When
    DATA req TYPE REF TO zcl_mcp_req_list_prompts.
    req = NEW #( json ).

    " Then
    cl_abap_unit_assert=>assert_false( act = req->has_cursor( )
                                       msg = 'No cursor should be recognized' ).

    cl_abap_unit_assert=>assert_initial( act = req->get_cursor( )
                                         msg = 'Cursor value should be initial' ).
  ENDMETHOD.
ENDCLASS.
