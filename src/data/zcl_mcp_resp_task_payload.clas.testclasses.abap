CLASS ltcl_resp_task_payload DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA cut TYPE REF TO zcl_mcp_resp_task_payload.

    METHODS setup.
    METHODS test_empty_has_content_array FOR TESTING RAISING cx_static_check.
    METHODS test_text_content_only       FOR TESTING RAISING cx_static_check.
    METHODS test_multiple_text_items     FOR TESTING RAISING cx_static_check.
    METHODS test_structured_content_only FOR TESTING RAISING cx_static_check.
    METHODS test_text_and_structured     FOR TESTING RAISING cx_static_check.
    METHODS test_is_error_flag           FOR TESTING RAISING cx_static_check.
    METHODS test_is_error_absent         FOR TESTING RAISING cx_static_check.
    METHODS test_with_meta               FOR TESTING RAISING cx_static_check.
ENDCLASS.

CLASS ltcl_resp_task_payload IMPLEMENTATION.
  METHOD setup.
    cut = NEW #( ).
  ENDMETHOD.

  METHOD test_empty_has_content_array.
    " Even with nothing set the response must be a valid CallToolResult shell
    DATA(json) = cut->zif_mcp_internal~generate_json( ).
    cl_abap_unit_assert=>assert_not_initial( act = json
                                             msg = 'result must be bound' ).
    cl_abap_unit_assert=>assert_true( act = json->exists( 'content' )
                                      msg = 'content array must always be present' ).
    cl_abap_unit_assert=>assert_equals( exp = 0
                                        act = lines( json->members( 'content' ) )
                                        msg = 'content array must be empty' ).
    cl_abap_unit_assert=>assert_false( act = json->exists( 'structuredContent' )
                                       msg = 'structuredContent must be absent when not set' ).
    cl_abap_unit_assert=>assert_false( act = json->exists( 'isError' )
                                       msg = 'isError must be absent when not set' ).
  ENDMETHOD.

  METHOD test_text_content_only.
    cut->add_text_content( 'Flight report complete' ).

    DATA(json) = cut->zif_mcp_internal~generate_json( ).
    cl_abap_unit_assert=>assert_equals( exp = 1
                                        act = lines( json->members( 'content' ) )
                                        msg = 'one content item expected' ).
    cl_abap_unit_assert=>assert_equals( exp = 'text'
                                        act = json->get_string( 'content/1/type' ) ).
    cl_abap_unit_assert=>assert_equals( exp = 'Flight report complete'
                                        act = json->get_string( 'content/1/text' ) ).
    cl_abap_unit_assert=>assert_false( act = json->exists( 'structuredContent' )
                                       msg = 'structuredContent must be absent' ).
  ENDMETHOD.

  METHOD test_multiple_text_items.
    cut->add_text_content( 'Line one' ).
    cut->add_text_content( 'Line two' ).

    DATA(json) = cut->zif_mcp_internal~generate_json( ).
    cl_abap_unit_assert=>assert_equals( exp = 2
                                        act = lines( json->members( 'content' ) )
                                        msg = 'two content items expected' ).
    cl_abap_unit_assert=>assert_equals( exp = 'Line one'
                                        act = json->get_string( 'content/1/text' ) ).
    cl_abap_unit_assert=>assert_equals( exp = 'Line two'
                                        act = json->get_string( 'content/2/text' ) ).
  ENDMETHOD.

  METHOD test_structured_content_only.
    DATA(payload) = zcl_mcp_ajson=>create_empty( ).
    payload->set( iv_path = '/airline'
                  iv_val  = 'AA' ).
    payload->set( iv_path = '/total_flights'
                  iv_val  = 6 ).
    cut->set_structured_content( payload ).

    DATA(json) = cut->zif_mcp_internal~generate_json( ).
    cl_abap_unit_assert=>assert_true( act = json->exists( 'structuredContent' )
                                      msg = 'structuredContent must be present' ).
    cl_abap_unit_assert=>assert_equals( exp = 'AA'
                                        act = json->get_string( 'structuredContent/airline' ) ).
    cl_abap_unit_assert=>assert_equals( exp = '6'
                                        act = json->get_string( 'structuredContent/total_flights' ) ).
    cl_abap_unit_assert=>assert_equals( exp = 0
                                        act = lines( json->members( 'content' ) )
                                        msg = 'content array must be empty' ).
  ENDMETHOD.

  METHOD test_text_and_structured.
    DATA(payload) = zcl_mcp_ajson=>create_empty( ).
    payload->set( iv_path = '/count' iv_val = 42 ).
    cut->set_structured_content( payload ).
    cut->add_text_content( 'Found 42 records' ).

    DATA(json) = cut->zif_mcp_internal~generate_json( ).
    cl_abap_unit_assert=>assert_equals( exp = 1
                                        act = lines( json->members( 'content' ) ) ).
    cl_abap_unit_assert=>assert_equals( exp = 'Found 42 records'
                                        act = json->get_string( 'content/1/text' ) ).
    cl_abap_unit_assert=>assert_equals( exp = '42'
                                        act = json->get_string( 'structuredContent/count' ) ).
  ENDMETHOD.

  METHOD test_is_error_flag.
    cut->add_text_content( 'Airline code not found' ).
    cut->set_is_error( ).

    DATA(json) = cut->zif_mcp_internal~generate_json( ).
    cl_abap_unit_assert=>assert_true( act = json->get_boolean( 'isError' )
                                      msg = 'isError must be true' ).
    cl_abap_unit_assert=>assert_equals( exp = 'Airline code not found'
                                        act = json->get_string( 'content/1/text' ) ).
  ENDMETHOD.

  METHOD test_is_error_absent.
    cut->add_text_content( 'All good' ).

    DATA(json) = cut->zif_mcp_internal~generate_json( ).
    cl_abap_unit_assert=>assert_false( act = json->exists( 'isError' )
                                       msg = 'isError must be absent when not set' ).
  ENDMETHOD.

  METHOD test_with_meta.
    DATA(payload) = zcl_mcp_ajson=>create_empty( ).
    payload->set( iv_path = '/result' iv_val = 'ok' ).
    cut->set_structured_content( payload ).

    DATA(meta) = zcl_mcp_ajson=>create_empty( ).
    meta->set( iv_path = '/requestId' iv_val = 'req-123' ).
    cut->set_meta( meta ).

    DATA(json) = cut->zif_mcp_internal~generate_json( ).
    cl_abap_unit_assert=>assert_equals( exp = 'ok'
                                        act = json->get_string( 'structuredContent/result' ) ).
    cl_abap_unit_assert=>assert_equals( exp = 'req-123'
                                        act = json->get_string( '_meta/requestId' ) ).
  ENDMETHOD.
ENDCLASS.
