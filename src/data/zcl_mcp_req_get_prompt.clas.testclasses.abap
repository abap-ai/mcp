CLASS ltcl_mcp_req_get_prompt DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    METHODS test_with_arguments        FOR TESTING RAISING zcx_mcp_ajson_error zcx_mcp_server.
    METHODS test_without_arguments     FOR TESTING RAISING zcx_mcp_ajson_error zcx_mcp_server.
    METHODS test_missing_name          FOR TESTING RAISING zcx_mcp_ajson_error.
    METHODS test_empty_name            FOR TESTING RAISING zcx_mcp_ajson_error.
    METHODS test_meta_default          FOR TESTING RAISING zcx_mcp_ajson_error zcx_mcp_server.
    METHODS test_meta_provided         FOR TESTING RAISING zcx_mcp_ajson_error zcx_mcp_server.
    METHODS test_no_retry_data         FOR TESTING RAISING zcx_mcp_ajson_error zcx_mcp_server.
    METHODS test_request_state_retry   FOR TESTING RAISING zcx_mcp_ajson_error zcx_mcp_server.
    METHODS test_input_responses_retry FOR TESTING RAISING zcx_mcp_ajson_error zcx_mcp_server.
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

  METHOD test_meta_default.
    DATA(json) = zcl_mcp_ajson=>create_empty( ).
    json->set_string( iv_path = '/name'
                      iv_val  = 'simple_greeting' ).

    DATA(req) = NEW zcl_mcp_req_get_prompt( json ).
    DATA(meta) = req->get_meta( ).

    cl_abap_unit_assert=>assert_bound( act = meta
                                       msg = '_meta should always be bound' ).
    cl_abap_unit_assert=>assert_true( act = meta->is_empty( )
                                      msg = '_meta should be empty when not supplied' ).
  ENDMETHOD.

  METHOD test_meta_provided.
    DATA(json) = zcl_mcp_ajson=>create_empty( ).
    json->set_string( iv_path = '/name'
                      iv_val  = 'greeting' ).
    json->set_string( iv_path = '/_meta/client'
                      iv_val  = 'adt' ).
    json->set_string( iv_path = '/_meta/trace_id'
                      iv_val  = 'trace-1' ).

    DATA(req) = NEW zcl_mcp_req_get_prompt( json ).

    cl_abap_unit_assert=>assert_equals( exp = 'adt'
                                        act = req->get_meta( )->get_string( '/client' )
                                        msg = '_meta client should be parsed' ).
    cl_abap_unit_assert=>assert_equals( exp = 'trace-1'
                                        act = req->get_meta( )->get_string( '/trace_id' )
                                        msg = '_meta trace should be parsed' ).
  ENDMETHOD.

  METHOD test_no_retry_data.
    DATA(json) = zcl_mcp_ajson=>create_empty( ).
    json->set_string( iv_path = '/name'
                      iv_val  = 'greeting' ).

    DATA(req) = NEW zcl_mcp_req_get_prompt( json ).

    cl_abap_unit_assert=>assert_false( act = req->is_retry( )
                                       msg = 'Request should not be retry without MRTR fields' ).
    cl_abap_unit_assert=>assert_false( act = req->has_input_responses( )
                                       msg = 'No inputResponses should be flagged' ).
    cl_abap_unit_assert=>assert_initial( act = req->get_request_state( )
                                         msg = 'requestState should be initial' ).
    cl_abap_unit_assert=>assert_bound( act = req->get_input_responses( )
                                       msg = 'inputResponses JSON should be bound' ).
    cl_abap_unit_assert=>assert_true( act = req->get_input_responses( )->is_empty( )
                                      msg = 'inputResponses should be empty by default' ).
  ENDMETHOD.

  METHOD test_request_state_retry.
    DATA(json) = zcl_mcp_ajson=>create_empty( ).
    json->set_string( iv_path = '/name'
                      iv_val  = 'greeting' ).
    json->set_string( iv_path = '/requestState'
                      iv_val  = 'state-1' ).

    DATA(req) = NEW zcl_mcp_req_get_prompt( json ).

    cl_abap_unit_assert=>assert_true( act = req->is_retry( )
                                      msg = 'requestState should mark request as retry' ).
    cl_abap_unit_assert=>assert_equals( exp = 'state-1'
                                        act = req->get_request_state( )
                                        msg = 'requestState should be parsed' ).
    cl_abap_unit_assert=>assert_false( act = req->has_input_responses( )
                                       msg = 'requestState alone should not flag inputResponses' ).
  ENDMETHOD.

  METHOD test_input_responses_retry.
    DATA(json) = zcl_mcp_ajson=>parse(
                     `{"name":"greeting","inputResponses":{"confirm":{"approved":true,"comment":"ok"}}}` ).

    DATA(req) = NEW zcl_mcp_req_get_prompt( json ).
    DATA(input_responses) = req->get_input_responses( ).

    cl_abap_unit_assert=>assert_true( act = req->is_retry( )
                                      msg = 'inputResponses should mark request as retry' ).
    cl_abap_unit_assert=>assert_true( act = req->has_input_responses( )
                                      msg = 'inputResponses should be flagged' ).
    cl_abap_unit_assert=>assert_bound( act = input_responses
                                       msg = 'inputResponses should be bound' ).
    cl_abap_unit_assert=>assert_true( act = input_responses->get_boolean( '/confirm/approved' )
                                      msg = 'Nested input response boolean should be parsed' ).
    cl_abap_unit_assert=>assert_equals( exp = 'ok'
                                        act = input_responses->get_string( '/confirm/comment' )
                                        msg = 'Nested input response string should be parsed' ).
  ENDMETHOD.
ENDCLASS.
