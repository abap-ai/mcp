CLASS ltcl_mcp_req_get_prompt DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    METHODS test_with_arguments    FOR TESTING RAISING zcx_mcp_ajson_error zcx_mcp_server.
    METHODS test_without_arguments FOR TESTING RAISING zcx_mcp_ajson_error zcx_mcp_server.
    METHODS test_missing_name      FOR TESTING RAISING zcx_mcp_ajson_error.
    METHODS test_empty_name        FOR TESTING RAISING zcx_mcp_ajson_error.
ENDCLASS.

CLASS ltcl_mcp_req_get_prompt IMPLEMENTATION.

  METHOD test_with_arguments.

    " Given
    DATA(json) = zcl_mcp_ajson=>create_empty( ).
    json->set_string( iv_path = '/name'
                      iv_val  = 'greeting' ).
    json->set_string( iv_path = '/arguments/user'
                      iv_val  = 'John' ).
    json->set_string( iv_path = '/arguments/language'
                      iv_val  = 'English' ).

    " When
    DATA req TYPE REF TO zcl_mcp_req_get_prompt.
    req = NEW #( json ).

    " Then
    cl_abap_unit_assert=>assert_equals( exp = 'greeting'
                                        act = req->get_name( )
                                        msg = 'Name should be correctly parsed' ).

    cl_abap_unit_assert=>assert_true( act = req->has_arguments( )
                                      msg = 'Should have arguments' ).

    DATA(args) = req->get_arguments( ).
    cl_abap_unit_assert=>assert_equals( exp = 2
                                        act = lines( args )
                                        msg = 'Should have 2 arguments' ).

    " Find the arguments by key
    DATA user_found     TYPE abap_bool VALUE abap_false.
    DATA language_found TYPE abap_bool VALUE abap_false.

    LOOP AT args ASSIGNING FIELD-SYMBOL(<arg>).
      CASE <arg>-key.
        WHEN 'user'.
          user_found = abap_true.
          cl_abap_unit_assert=>assert_equals( exp = 'John'
                                              act = <arg>-value
                                              msg = 'User argument should have correct value' ).
        WHEN 'language'.
          language_found = abap_true.
          cl_abap_unit_assert=>assert_equals( exp = 'English'
                                              act = <arg>-value
                                              msg = 'Language argument should have correct value' ).
      ENDCASE.
    ENDLOOP.

    cl_abap_unit_assert=>assert_true( act = user_found
                                      msg = 'User argument should be found' ).

    cl_abap_unit_assert=>assert_true( act = language_found
                                      msg = 'Language argument should be found' ).
  ENDMETHOD.

  METHOD test_without_arguments.
    " Given
    DATA(json) = zcl_mcp_ajson=>create_empty( ).
    json->set_string( iv_path = '/name'
                      iv_val  = 'simple_greeting' ).

    " When
    DATA req TYPE REF TO zcl_mcp_req_get_prompt.
    req = NEW #( json ).

    " Then
    cl_abap_unit_assert=>assert_equals( exp = 'simple_greeting'
                                        act = req->get_name( )
                                        msg = 'Name should be correctly parsed' ).

    cl_abap_unit_assert=>assert_false( act = req->has_arguments( )
                                       msg = 'Should not have arguments' ).

    DATA(args) = req->get_arguments( ).
    cl_abap_unit_assert=>assert_initial( act = args
                                         msg = 'Arguments should be initial' ).
  ENDMETHOD.

  METHOD test_missing_name.
    " Given
    DATA(json) = zcl_mcp_ajson=>create_empty( ).
    " No name parameter

    " When & Then
    TRY.
        DATA req TYPE REF TO zcl_mcp_req_get_prompt.
        req = NEW #( json ).
        cl_abap_unit_assert=>fail( 'Expected exception for missing name' ).
      CATCH zcx_mcp_server INTO DATA(lx_error).
        " Expected - name is required but missing
        cl_abap_unit_assert=>assert_equals( exp = 'ZMCP'
                                            act = lx_error->if_t100_message~t100key-msgid ).
        " required_params message number
        cl_abap_unit_assert=>assert_equals( exp = '002'
                                            act = lx_error->if_t100_message~t100key-msgno ).
    ENDTRY.
  ENDMETHOD.

  METHOD test_empty_name.
    " Given
    DATA(json) = zcl_mcp_ajson=>create_empty( ).
    json->set_string( iv_path = '/name'
                      iv_val  = '' ). " Empty name

    " When & Then
    TRY.
        DATA req TYPE REF TO zcl_mcp_req_get_prompt.
        req = NEW #( json ).
        cl_abap_unit_assert=>fail( 'Expected exception for empty name' ).
      CATCH zcx_mcp_server INTO DATA(lx_error).
        " Expected - name is empty
        cl_abap_unit_assert=>assert_equals( exp = 'ZMCP'
                                            act = lx_error->if_t100_message~t100key-msgid ).
        " prompt_name_invalid message number
        cl_abap_unit_assert=>assert_equals( exp = '001'
                                            act = lx_error->if_t100_message~t100key-msgno ).
    ENDTRY.
  ENDMETHOD.
ENDCLASS.
