CLASS ltcl_call_tool_request DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    "! Test basic functionality
    METHODS test_valid_request      FOR TESTING RAISING cx_static_check.
    METHODS test_missing_name       FOR TESTING RAISING cx_static_check.
    METHODS test_empty_name         FOR TESTING RAISING cx_static_check.
    METHODS test_no_arguments       FOR TESTING RAISING cx_static_check.
    METHODS test_with_arguments     FOR TESTING RAISING cx_static_check.
    METHODS test_empty_arguments    FOR TESTING RAISING cx_static_check.

    "! Test _meta field functionality
    METHODS test_no_meta            FOR TESTING RAISING cx_static_check.
    METHODS test_simple_meta        FOR TESTING RAISING cx_static_check.
    METHODS test_meta_no_slash      FOR TESTING RAISING cx_static_check.
    METHODS test_meta_with_slash    FOR TESTING RAISING cx_static_check.
    METHODS test_meta_nested        FOR TESTING RAISING cx_static_check.
    METHODS test_meta_special_chars FOR TESTING RAISING cx_static_check.
    METHODS test_meta_roundtrip     FOR TESTING RAISING cx_static_check.
ENDCLASS.

CLASS ltcl_call_tool_request IMPLEMENTATION.
  METHOD test_valid_request.
    DATA(json_string) = `{"name":"test_tool","arguments":{"param1":"value1"}}`.
    DATA(json) = zcl_mcp_ajson=>parse( json_string ).

    DATA(request) = NEW zcl_mcp_req_call_tool( json ).

    cl_abap_unit_assert=>assert_equals( exp = 'test_tool'
                                        act = request->get_name( ) ).
    cl_abap_unit_assert=>assert_true( request->has_arguments( ) ).
    cl_abap_unit_assert=>assert_equals( exp = 'value1'
                                        act = request->get_arguments( )->get_string( '/param1' ) ).
  ENDMETHOD.

  METHOD test_missing_name.
    DATA(json_string) = `{"arguments":{"param1":"value1"}}`.
    DATA(json) = zcl_mcp_ajson=>parse( json_string ).

    TRY.
        " TODO: variable is assigned but never used (ABAP cleaner)
        DATA(request) = NEW zcl_mcp_req_call_tool( json ).
        cl_abap_unit_assert=>fail( 'Expected exception for missing name' ).
      CATCH zcx_mcp_server INTO DATA(lx_server). " TODO: variable is assigned but never used (ABAP cleaner)
        " Expected exception
    ENDTRY.
  ENDMETHOD.

  METHOD test_empty_name.
    DATA(json_string) = `{"name":"","arguments":{"param1":"value1"}}`.
    DATA(json) = zcl_mcp_ajson=>parse( json_string ).

    TRY.
        " TODO: variable is assigned but never used (ABAP cleaner)
        DATA(request) = NEW zcl_mcp_req_call_tool( json ).
        cl_abap_unit_assert=>fail( 'Expected exception for empty name' ).
      CATCH zcx_mcp_server INTO DATA(lx_server). " TODO: variable is assigned but never used (ABAP cleaner)
        " Expected exception
    ENDTRY.
  ENDMETHOD.

  METHOD test_no_arguments.
    DATA(json_string) = `{"name":"test_tool"}`.
    DATA(json) = zcl_mcp_ajson=>parse( json_string ).

    DATA(request) = NEW zcl_mcp_req_call_tool( json ).

    cl_abap_unit_assert=>assert_equals( exp = 'test_tool'
                                        act = request->get_name( ) ).
    cl_abap_unit_assert=>assert_false( request->has_arguments( ) ).
    cl_abap_unit_assert=>assert_true( request->get_arguments( )->is_empty( ) ).
  ENDMETHOD.

  METHOD test_with_arguments.
    DATA(json_string) = `{"name":"test_tool","arguments":{"param1":"value1","param2":42}}`.
    DATA(json) = zcl_mcp_ajson=>parse( json_string ).

    DATA(request) = NEW zcl_mcp_req_call_tool( json ).

    cl_abap_unit_assert=>assert_equals( exp = 'test_tool'
                                        act = request->get_name( ) ).
    cl_abap_unit_assert=>assert_true( request->has_arguments( ) ).

    DATA(arguments) = request->get_arguments( ).
    cl_abap_unit_assert=>assert_equals( exp = 'value1'
                                        act = arguments->get_string( '/param1' ) ).
    cl_abap_unit_assert=>assert_equals( exp = 42
                                        act = arguments->get_integer( '/param2' ) ).
  ENDMETHOD.

  METHOD test_empty_arguments.
    DATA(json_string) = `{"name":"test_tool","arguments":{}}`.
    DATA(json) = zcl_mcp_ajson=>parse( json_string ).

    DATA(request) = NEW zcl_mcp_req_call_tool( json ).

    cl_abap_unit_assert=>assert_equals( exp = 'test_tool'
                                        act = request->get_name( ) ).
    cl_abap_unit_assert=>assert_true( request->has_arguments( ) ).
  ENDMETHOD.

  " _meta field tests

  METHOD test_no_meta.
    DATA(json_string) = `{"name":"test_tool","arguments":{"param1":"value1"}}`.
    DATA(json) = zcl_mcp_ajson=>parse( json_string ).

    DATA(request) = NEW zcl_mcp_req_call_tool( json ).

    DATA(meta) = request->get_meta( ).
    cl_abap_unit_assert=>assert_true( act = meta->is_empty( )
                                      msg = '_meta should be empty when not provided' ).
  ENDMETHOD.

  METHOD test_simple_meta.
    DATA(json_string) = `{"name":"test_tool","_meta":{"version":"1.0","author":"test"}}`.
    DATA(json) = zcl_mcp_ajson=>parse( json_string ).

    DATA(request) = NEW zcl_mcp_req_call_tool( json ).

    DATA(meta) = request->get_meta( ).
    cl_abap_unit_assert=>assert_false( act = meta->is_empty( )
                                       msg = '_meta should not be empty when provided' ).
    cl_abap_unit_assert=>assert_equals( exp = '1.0'
                                        act = meta->get_string( '/version' )
                                        msg = 'Should retrieve simple _meta fields' ).
    cl_abap_unit_assert=>assert_equals( exp = 'test'
                                        act = meta->get_string( '/author' )
                                        msg = 'Should retrieve simple _meta fields' ).
  ENDMETHOD.

  METHOD test_meta_no_slash.
    " Test MCP-compliant keys without slashes
    DATA(json_string) = `{"name":"test_tool","_meta":{"request-id":"req-123","tool.version":"2.1","api_key":"abc"}}`.
    DATA(json) = zcl_mcp_ajson=>parse( json_string ).

    DATA(request) = NEW zcl_mcp_req_call_tool( json ).

    DATA(meta) = request->get_meta( ).
    cl_abap_unit_assert=>assert_equals( exp = 'req-123'
                                        act = meta->get_string( '/request-id' )
                                        msg = 'Should handle _meta keys with hyphens' ).
    cl_abap_unit_assert=>assert_equals( exp = '2.1'
                                        act = meta->get_string( '/tool.version' )
                                        msg = 'Should handle _meta keys with dots' ).
    cl_abap_unit_assert=>assert_equals( exp = 'abc'
                                        act = meta->get_string( '/api_key' )
                                        msg = 'Should handle _meta keys with underscores' ).
  ENDMETHOD.

  METHOD test_meta_with_slash.
    " Test that slash keys are parsed and accessible via members/tree
    DATA(json_string) = `{"name":"test_tool","_meta":{"api.example.com/version":"v2","simple":"value"}}`.
    DATA(json) = zcl_mcp_ajson=>parse( json_string ).

    DATA(request) = NEW zcl_mcp_req_call_tool( json ).
    DATA(meta) = request->get_meta( ).

    " Verify _meta is not empty
    cl_abap_unit_assert=>assert_false( act = meta->is_empty( )
                                       msg = '_meta should not be empty' ).

    " Verify simple keys still work
    cl_abap_unit_assert=>assert_equals( exp = 'value'
                                        act = meta->get_string( '/simple' )
                                        msg = 'Simple keys should work alongside slash keys' ).

    " Verify slash key is present in members
    DATA(members) = meta->members( '/' ).
    cl_abap_unit_assert=>assert_equals( exp = 2
                                        act = lines( members )
                                        msg = 'Should have 2 meta keys' ).

    READ TABLE members TRANSPORTING NO FIELDS WITH KEY table_line = 'api.example.com/version'.
    cl_abap_unit_assert=>assert_subrc( exp = 0
                                       act = sy-subrc
                                       msg = 'Slash key should be in members list' ).

    " Access slash key via internal tree
    LOOP AT meta->mt_json_tree INTO DATA(node) WHERE path = '/' AND name = 'api.example.com/version'.
      cl_abap_unit_assert=>assert_equals( exp = 'v2'
                                          act = node-value
                                          msg = 'Should access slash key via tree' ).
    ENDLOOP.
  ENDMETHOD.

  METHOD test_meta_nested.
    " Test nested structures in _meta
    DATA(json_string) = `{"name":"test_tool","_meta":{"config":{"retry":3,"timeout":"5m"},"simple":"value"}}`.
    DATA(json) = zcl_mcp_ajson=>parse( json_string ).

    DATA(request) = NEW zcl_mcp_req_call_tool( json ).
    DATA(meta) = request->get_meta( ).

    " Test nested structure access
    cl_abap_unit_assert=>assert_equals( exp = 3
                                        act = meta->get_integer( '/config/retry' )
                                        msg = 'Should access nested _meta retry' ).
    cl_abap_unit_assert=>assert_equals( exp = '5m'
                                        act = meta->get_string( '/config/timeout' )
                                        msg = 'Should access nested _meta timeout' ).
    cl_abap_unit_assert=>assert_equals( exp = 'value'
                                        act = meta->get_string( '/simple' )
                                        msg = 'Should access simple _meta alongside nested' ).
  ENDMETHOD.

  METHOD test_meta_special_chars.
    " Test various special characters valid in _meta keys
    DATA(json_string) = `{"name":"test_tool","_meta":{"key-hyphens":"v1","key_underscores":"v2","key.dots":"v3","key123":"v4"}}`.
    DATA(json) = zcl_mcp_ajson=>parse( json_string ).

    DATA(request) = NEW zcl_mcp_req_call_tool( json ).
    DATA(meta) = request->get_meta( ).

    cl_abap_unit_assert=>assert_equals( exp = 'v1'
                                        act = meta->get_string( '/key-hyphens' )
                                        msg = 'Should handle hyphens in keys' ).
    cl_abap_unit_assert=>assert_equals( exp = 'v2'
                                        act = meta->get_string( '/key_underscores' )
                                        msg = 'Should handle underscores in keys' ).
    cl_abap_unit_assert=>assert_equals( exp = 'v3'
                                        act = meta->get_string( '/key.dots' )
                                        msg = 'Should handle dots in keys' ).
    cl_abap_unit_assert=>assert_equals( exp = 'v4'
                                        act = meta->get_string( '/key123' )
                                        msg = 'Should handle alphanumerics in keys' ).
  ENDMETHOD.

  METHOD test_meta_roundtrip.
    " Test JSON roundtrip preserves slash keys
    DATA(json_string) = `{"name":"test_tool","_meta":{"api.v1/endpoint":"users","normal":"value"}}`.
    DATA(json) = zcl_mcp_ajson=>parse( json_string ).

    DATA(request) = NEW zcl_mcp_req_call_tool( json ).
    DATA(meta) = request->get_meta( ).

    " Stringify and verify slash key is preserved
    DATA(meta_json) = meta->stringify( ).

    " Simple contains checks instead of matchers
    cl_abap_unit_assert=>assert_true( act = boolc( meta_json CS 'api.v1/endpoint' )
                                      msg = 'Stringified JSON should preserve slash keys' ).

    cl_abap_unit_assert=>assert_true( act = boolc( meta_json CS 'users' )
                                      msg = 'Stringified JSON should preserve slash key values' ).

    cl_abap_unit_assert=>assert_true( act = boolc( meta_json CS 'normal' )
                                      msg = 'Stringified JSON should preserve normal keys' ).
  ENDMETHOD.

ENDCLASS.
