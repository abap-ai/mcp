CLASS ltcl_mcp_req_mrtr_retry DEFINITION FINAL
  FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.

  PRIVATE SECTION.
    METHODS test_empty_request_defaults      FOR TESTING RAISING zcx_mcp_ajson_error.
    METHODS test_unbound_json_defaults       FOR TESTING RAISING zcx_mcp_ajson_error.
    METHODS test_request_state_retry         FOR TESTING RAISING zcx_mcp_ajson_error.
    METHODS test_input_responses_retry       FOR TESTING RAISING zcx_mcp_ajson_error.
    METHODS test_empty_input_responses_rtr FOR TESTING RAISING zcx_mcp_ajson_error.
    METHODS test_full_retry_request          FOR TESTING RAISING zcx_mcp_ajson_error.
ENDCLASS.

CLASS ltcl_mcp_req_mrtr_retry IMPLEMENTATION.
  METHOD test_empty_request_defaults.
    DATA(json) = zcl_mcp_ajson=>create_empty( ).

    DATA(cut) = NEW zcl_mcp_req_mrtr_retry( json ).

    cl_abap_unit_assert=>assert_false( act = cut->is_retry( )
                                       msg = 'Empty request should not be retry' ).
    cl_abap_unit_assert=>assert_false( act = cut->has_input_responses( )
                                       msg = 'Empty request should not have inputResponses' ).
    cl_abap_unit_assert=>assert_initial( act = cut->get_request_state( )
                                         msg = 'requestState should be initial' ).
    cl_abap_unit_assert=>assert_bound( act = cut->get_input_responses( )
                                       msg = 'inputResponses JSON should be bound' ).
    cl_abap_unit_assert=>assert_true( act = cut->get_input_responses( )->is_empty( )
                                      msg = 'inputResponses should be empty by default' ).
  ENDMETHOD.

  METHOD test_unbound_json_defaults.
    DATA json TYPE REF TO zif_mcp_ajson.

    DATA(cut) = NEW zcl_mcp_req_mrtr_retry( json ).

    cl_abap_unit_assert=>assert_false( act = cut->is_retry( )
                                       msg = 'Unbound JSON should not be retry' ).
    cl_abap_unit_assert=>assert_false( act = cut->has_input_responses( )
                                       msg = 'Unbound JSON should not have inputResponses' ).
    cl_abap_unit_assert=>assert_initial( act = cut->get_request_state( )
                                         msg = 'requestState should be initial' ).
    cl_abap_unit_assert=>assert_bound( act = cut->get_input_responses( )
                                       msg = 'inputResponses JSON should be bound' ).
    cl_abap_unit_assert=>assert_true( act = cut->get_input_responses( )->is_empty( )
                                      msg = 'inputResponses should be empty by default' ).
  ENDMETHOD.

  METHOD test_request_state_retry.
    DATA(json) = zcl_mcp_ajson=>parse( `{"requestState":"state-1"}` ).

    DATA(cut) = NEW zcl_mcp_req_mrtr_retry( json ).

    cl_abap_unit_assert=>assert_true( act = cut->is_retry( )
                                      msg = 'requestState should mark request as retry' ).
    cl_abap_unit_assert=>assert_equals( exp = 'state-1'
                                        act = cut->get_request_state( )
                                        msg = 'requestState should be parsed' ).
    cl_abap_unit_assert=>assert_false( act = cut->has_input_responses( )
                                       msg = 'requestState alone should not flag inputResponses' ).
    cl_abap_unit_assert=>assert_true( act = cut->get_input_responses( )->is_empty( )
                                      msg = 'inputResponses should be empty when absent' ).
  ENDMETHOD.

  METHOD test_input_responses_retry.
    DATA(json) = zcl_mcp_ajson=>parse( `{"inputResponses":{"confirm":{"approved":true,"comment":"ok"}}}` ).

    DATA(cut) = NEW zcl_mcp_req_mrtr_retry( json ).
    DATA(input_responses) = cut->get_input_responses( ).

    cl_abap_unit_assert=>assert_true( act = cut->is_retry( )
                                      msg = 'inputResponses should mark request as retry' ).
    cl_abap_unit_assert=>assert_true( act = cut->has_input_responses( )
                                      msg = 'inputResponses should be flagged' ).
    cl_abap_unit_assert=>assert_initial( act = cut->get_request_state( )
                                         msg = 'requestState should be initial when absent' ).
    cl_abap_unit_assert=>assert_bound( act = input_responses
                                       msg = 'inputResponses should be bound' ).
    cl_abap_unit_assert=>assert_true( act = input_responses->get_boolean( '/confirm/approved' )
                                      msg = 'Nested boolean should be parsed' ).
    cl_abap_unit_assert=>assert_equals( exp = 'ok'
                                        act = input_responses->get_string( '/confirm/comment' )
                                        msg = 'Nested string should be parsed' ).
  ENDMETHOD.

  METHOD test_empty_input_responses_rtr.
    DATA(json) = zcl_mcp_ajson=>parse( `{"inputResponses":{}}` ).

    DATA(cut) = NEW zcl_mcp_req_mrtr_retry( json ).
    DATA(input_responses) = cut->get_input_responses( ).

    cl_abap_unit_assert=>assert_true( act = cut->is_retry( )
                                      msg = 'Even empty inputResponses should mark retry' ).
    cl_abap_unit_assert=>assert_true( act = cut->has_input_responses( )
                                      msg = 'inputResponses presence should be tracked' ).
    cl_abap_unit_assert=>assert_bound( act = input_responses
                                       msg = 'inputResponses should be bound' ).
    cl_abap_unit_assert=>assert_equals( exp = zif_mcp_ajson_types=>node_type-object
                                        act = input_responses->get_node_type( '/' )
                                        msg = 'Empty inputResponses should be an object' ).
    cl_abap_unit_assert=>assert_equals( exp = 0
                                        act = lines( input_responses->members( '/' ) )
                                        msg = 'Empty inputResponses object should have no members' ).
  ENDMETHOD.

  METHOD test_full_retry_request.
    DATA(json) = zcl_mcp_ajson=>parse( `{"requestState":"state-2","inputResponses":{"field":{"value":"abc"}}}` ).

    DATA(cut) = NEW zcl_mcp_req_mrtr_retry( json ).

    cl_abap_unit_assert=>assert_true( act = cut->is_retry( )
                                      msg = 'Combined retry data should mark retry' ).
    cl_abap_unit_assert=>assert_true( act = cut->has_input_responses( )
                                      msg = 'inputResponses should be flagged' ).
    cl_abap_unit_assert=>assert_equals( exp = 'state-2'
                                        act = cut->get_request_state( )
                                        msg = 'requestState should be parsed' ).
    cl_abap_unit_assert=>assert_equals( exp = 'abc'
                                        act = cut->get_input_responses( )->get_string( '/field/value' )
                                        msg = 'inputResponses should be sliced from root' ).
  ENDMETHOD.

ENDCLASS.
