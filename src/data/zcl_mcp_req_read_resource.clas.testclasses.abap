CLASS ltcl_mcp_req_read_resource DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    METHODS test_read_resource FOR TESTING RAISING zcx_mcp_ajson_error zcx_mcp_server.
    METHODS test_missing_uri   FOR TESTING RAISING zcx_mcp_ajson_error zcx_mcp_server.
ENDCLASS.

CLASS ltcl_mcp_req_read_resource IMPLEMENTATION.
  METHOD test_read_resource.
    " Given
    DATA(json) = zcl_mcp_ajson=>create_empty( ).
    json->set_string( iv_path = 'uri' iv_val = 'file:///example/resource.txt' ).

    " When
    DATA req TYPE REF TO zcl_mcp_req_read_resource.
    req = NEW #( json ).

    " Then
    cl_abap_unit_assert=>assert_equals(
      act = req->get_uri( )
      exp = 'file:///example/resource.txt'
      msg = 'URI should be correctly parsed' ).
  ENDMETHOD.

  METHOD test_missing_uri.
    " Given
    DATA(json) = zcl_mcp_ajson=>create_empty( ).
    " No URI parameter

    " When & Then
    TRY.
        DATA req TYPE REF TO zcl_mcp_req_read_resource.
        req = NEW #( json ).
        cl_abap_unit_assert=>fail( 'Expected exception for missing URI' ).
      CATCH zcx_mcp_server.
        " Expected - URI is required but missing
    ENDTRY. "#EC EMPTY_CATCH
  ENDMETHOD.
ENDCLASS.
