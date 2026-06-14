CLASS ltcl_mcp_req_read_resource DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    METHODS test_read_resource         FOR TESTING RAISING zcx_mcp_ajson_error zcx_mcp_server.
    METHODS test_missing_uri           FOR TESTING RAISING zcx_mcp_ajson_error zcx_mcp_server.
    METHODS test_empty_uri             FOR TESTING RAISING zcx_mcp_ajson_error.
    METHODS test_meta_default          FOR TESTING RAISING zcx_mcp_ajson_error zcx_mcp_server.
    METHODS test_meta_provided         FOR TESTING RAISING zcx_mcp_ajson_error zcx_mcp_server.
    METHODS test_no_retry_data         FOR TESTING RAISING zcx_mcp_ajson_error zcx_mcp_server.
    METHODS test_request_state_retry   FOR TESTING RAISING zcx_mcp_ajson_error zcx_mcp_server.
    METHODS test_input_responses_retry FOR TESTING RAISING zcx_mcp_ajson_error zcx_mcp_server.
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
        " Expected
    ENDTRY ##NO_HANDLER.
  ENDMETHOD.

  METHOD test_empty_uri.
    DATA(json) = zcl_mcp_ajson=>create_empty( ).
    json->set_string( iv_path = 'uri'
                      iv_val  = '' ).

    TRY.
        DATA(req) = NEW zcl_mcp_req_read_resource( json ).
        cl_abap_unit_assert=>fail( 'Expected exception for empty URI' ).
      CATCH zcx_mcp_server INTO DATA(error).
        cl_abap_unit_assert=>assert_equals( exp = zcx_mcp_server=>required_params
                                            act = error->if_t100_message~t100key
                                            msg = 'Empty URI should raise required_params' ).
    ENDTRY.
  ENDMETHOD.

  METHOD test_meta_default.
    DATA(json) = zcl_mcp_ajson=>create_empty( ).
    json->set_string( iv_path = 'uri'
                      iv_val  = 'file:///example/resource.txt' ).

    DATA(req) = NEW zcl_mcp_req_read_resource( json ).
    DATA(meta) = req->get_meta( ).

    cl_abap_unit_assert=>assert_bound( act = meta
                                       msg = '_meta should always be bound' ).
    cl_abap_unit_assert=>assert_true( act = meta->is_empty( )
                                      msg = '_meta should be empty when not supplied' ).
  ENDMETHOD.

  METHOD test_meta_provided.
    DATA(json) = zcl_mcp_ajson=>create_empty( ).
    json->set_string( iv_path = 'uri'
                      iv_val  = 'file:///example/resource.txt' ).
    json->set_string( iv_path = '/_meta/client'
                      iv_val  = 'adt' ).
    json->set_string( iv_path = '/_meta/trace_id'
                      iv_val  = 'trace-1' ).

    DATA(req) = NEW zcl_mcp_req_read_resource( json ).

    cl_abap_unit_assert=>assert_equals( exp = 'adt'
                                        act = req->get_meta( )->get_string( '/client' )
                                        msg = '_meta client should be parsed' ).
    cl_abap_unit_assert=>assert_equals( exp = 'trace-1'
                                        act = req->get_meta( )->get_string( '/trace_id' )
                                        msg = '_meta trace should be parsed' ).
  ENDMETHOD.

  METHOD test_no_retry_data.
    DATA(json) = zcl_mcp_ajson=>create_empty( ).
    json->set_string( iv_path = 'uri'
                      iv_val  = 'file:///example/resource.txt' ).

    DATA(req) = NEW zcl_mcp_req_read_resource( json ).

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
    DATA(json) = zcl_mcp_ajson=>parse( `{"uri":"file:///example/resource.txt","requestState":"state-1"}` ).

    DATA(req) = NEW zcl_mcp_req_read_resource( json ).

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
        `{"uri":"file:///example/resource.txt","inputResponses":{"confirm":{"approved":true,"comment":"ok"}}}` ).

    DATA(req) = NEW zcl_mcp_req_read_resource( json ).
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
