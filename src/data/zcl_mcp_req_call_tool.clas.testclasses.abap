CLASS ltcl_mcp_req_call_tool DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    METHODS test_without_arguments FOR TESTING RAISING zcx_mcp_ajson_error zcx_mcp_server.
    METHODS test_with_arguments    FOR TESTING RAISING zcx_mcp_ajson_error zcx_mcp_server.
    METHODS test_missing_name      FOR TESTING RAISING zcx_mcp_ajson_error.
    METHODS test_empty_name        FOR TESTING RAISING zcx_mcp_ajson_error.
ENDCLASS.

CLASS ltcl_mcp_req_call_tool IMPLEMENTATION.
  METHOD test_without_arguments.
    " Given
    DATA(json) = zcl_mcp_ajson=>create_empty( ).
    json->set_string( iv_path = 'method'
                      iv_val  = 'tools/call' ).
    json->set_string( iv_path = 'name'
                      iv_val  = 'calculator' ).

    " When
    DATA req TYPE REF TO zcl_mcp_req_call_tool.
    req = NEW #( json ).

    " Then
    cl_abap_unit_assert=>assert_equals( exp = 'calculator'
                                        act = req->get_name( )
                                        msg = 'Tool name should be correctly parsed' ).

    cl_abap_unit_assert=>assert_false( act = req->has_arguments( )
                                       msg = 'Should not have arguments' ).
  ENDMETHOD.

  METHOD test_with_arguments.
    " Given
    DATA(json) = zcl_mcp_ajson=>create_empty( ).
    json->set_string( iv_path = 'method'
                      iv_val  = 'tools/call' ).
    json->set_string( iv_path = 'name'
                      iv_val  = 'calculator' ).
    " Add different argument types
    json->set_string( iv_path = 'arguments/strArg'
                      iv_val  = 'test' ).
    json->set_integer( iv_path = 'arguments/intArg'
                       iv_val  = 42 ).
    json->set_boolean( iv_path = 'arguments/boolArg'
                       iv_val  = abap_true ).

    " When
    DATA req TYPE REF TO zcl_mcp_req_call_tool.
    req = NEW #( json ).

    " Then
    cl_abap_unit_assert=>assert_equals( exp = 'calculator'
                                        act = req->get_name( )
                                        msg = 'Tool name should be correctly parsed' ).

    cl_abap_unit_assert=>assert_true( act = req->has_arguments( )
                                      msg = 'Should have arguments' ).

    " Check arguments
    DATA(args) = req->get_arguments( ).
    cl_abap_unit_assert=>assert_not_initial( act = args
                                             msg = 'Arguments should not be initial' ).

    " Check the structure was preserved - use members() to get argument keys
    DATA(arg_keys) = args->members( '' ).
    cl_abap_unit_assert=>assert_equals( exp = 3
                                        act = lines( arg_keys )
                                        msg = 'Arguments count should be 3' ).

    cl_abap_unit_assert=>assert_equals( exp = 'test'
                                        act = args->get_string( '/strArg' )
                                        msg = 'String argument should be preserved' ).

    cl_abap_unit_assert=>assert_equals( exp = '42'                      " As string in JSON
                                        act = args->get_string( '/intArg' )
                                        msg = 'Integer argument should be preserved' ).

    cl_abap_unit_assert=>assert_equals( exp = 'true'                    " As string in JSON
                                        act = args->get_string( '/boolArg' )
                                        msg = 'Boolean argument should be preserved' ).
  ENDMETHOD.

  METHOD test_missing_name.
    " Given
    DATA(json) = zcl_mcp_ajson=>create_empty( ).
    json->set_string( iv_path = 'method'
                      iv_val  = 'tools/call' ).
    " No name parameter

    " When & Then
    TRY.
        DATA req TYPE REF TO zcl_mcp_req_call_tool.
        req = NEW #( json ).
        cl_abap_unit_assert=>fail( 'Expected exception for missing name' ).
      CATCH zcx_mcp_server INTO DATA(lx_error).
        " Expected - name is required but missing
        cl_abap_unit_assert=>assert_equals( exp = 'ZMCP'
                                            act = lx_error->if_t100_message~t100key-msgid ).
        cl_abap_unit_assert=>assert_equals( exp = '002'
                                            act = lx_error->if_t100_message~t100key-msgno ). " required_params message number
    ENDTRY.
  ENDMETHOD.

  METHOD test_empty_name.
    " Given
    DATA(json) = zcl_mcp_ajson=>create_empty( ).
    json->set_string( iv_path = 'method'
                      iv_val  = 'tools/call' ).
    json->set_string( iv_path = 'name'
                      iv_val  = '' ). " Empty name

    " When & Then
    TRY.
        DATA req TYPE REF TO zcl_mcp_req_call_tool.
        req = NEW #( json ).
        cl_abap_unit_assert=>fail( 'Expected exception for empty name' ).
      CATCH zcx_mcp_server INTO DATA(lx_error).
        " Expected - name is empty
        cl_abap_unit_assert=>assert_equals( exp = 'ZMCP'
                                            act = lx_error->if_t100_message~t100key-msgid ).
        cl_abap_unit_assert=>assert_equals( exp = '009'
                                            act = lx_error->if_t100_message~t100key-msgno ). " unknown_tool message number
    ENDTRY.
  ENDMETHOD.
ENDCLASS.
