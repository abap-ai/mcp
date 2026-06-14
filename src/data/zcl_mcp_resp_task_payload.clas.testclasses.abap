CLASS ltcl_resp_task_payload DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA cut TYPE REF TO zcl_mcp_resp_task_payload.

    METHODS setup.
    METHODS test_empty_has_content_array   FOR TESTING RAISING cx_static_check.
    METHODS test_text_content_only         FOR TESTING RAISING cx_static_check.
    METHODS test_multiple_text_items       FOR TESTING RAISING cx_static_check.
    METHODS test_structured_content_only   FOR TESTING RAISING cx_static_check.
    METHODS test_text_and_structured       FOR TESTING RAISING cx_static_check.
    METHODS test_is_error_flag             FOR TESTING RAISING cx_static_check.
    METHODS test_is_error_absent           FOR TESTING RAISING cx_static_check.
    METHODS test_with_meta                 FOR TESTING RAISING cx_static_check.
    METHODS test_get_is_error_state        FOR TESTING RAISING cx_static_check.
    METHODS test_set_from_json             FOR TESTING RAISING cx_static_check.
    METHODS test_set_from_json_with_meta   FOR TESTING RAISING cx_static_check.
    METHODS test_set_related_task          FOR TESTING RAISING cx_static_check.
    METHODS test_related_task_prebuilt_mta FOR TESTING RAISING cx_static_check.
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

  METHOD test_get_is_error_state.
    cl_abap_unit_assert=>assert_false( act = cut->get_is_error( )
                                       msg = 'isError should default to false' ).

    cut->set_is_error( ).

    cl_abap_unit_assert=>assert_true( act = cut->get_is_error( )
                                      msg = 'isError getter should reflect true state' ).

    cut->set_is_error( abap_false ).

    cl_abap_unit_assert=>assert_false( act = cut->get_is_error( )
                                       msg = 'isError getter should reflect explicit false state' ).
  ENDMETHOD.

  METHOD test_set_from_json.
    DATA(prebuilt) = zcl_mcp_ajson=>parse(
        `{"content":[{"type":"text","text":"stored result"}],"structuredContent":{"count":3},"isError":true}` ).

    cut->set_from_json( prebuilt ).

    DATA(json) = cut->zif_mcp_internal~generate_json( ).

    cl_abap_unit_assert=>assert_equals( exp = 'stored result'
                                        act = json->get_string( '/content/1/text' )
                                        msg = 'Prebuilt content should be returned' ).
    cl_abap_unit_assert=>assert_equals( exp = '3'
                                        act = json->get_string( '/structuredContent/count' )
                                        msg = 'Prebuilt structured content should be returned' ).
    cl_abap_unit_assert=>assert_true( act = json->get_boolean( '/isError' )
                                      msg = 'Prebuilt isError should be returned' ).
  ENDMETHOD.

  METHOD test_set_from_json_with_meta.
    DATA(prebuilt) = zcl_mcp_ajson=>parse(
                         `{"content":[{"type":"text","text":"stored result"}],"_meta":{"old":"value"}}` ).

    DATA(meta) = zcl_mcp_ajson=>create_empty( ).
    meta->set_string( iv_path = '/requestId'
                      iv_val  = 'req-123' ).

    cut->set_from_json( prebuilt ).
    cut->set_meta( meta ).

    DATA(json) = cut->zif_mcp_internal~generate_json( ).

    cl_abap_unit_assert=>assert_equals( exp = 'stored result'
                                        act = json->get_string( '/content/1/text' )
                                        msg = 'Prebuilt content should be preserved' ).
    cl_abap_unit_assert=>assert_equals( exp = 'req-123'
                                        act = json->get_string( '/_meta/requestId' )
                                        msg = 'Explicit meta should be applied to prebuilt JSON' ).
    cl_abap_unit_assert=>assert_false( act = json->exists( '/_meta/old' )
                                       msg = 'Explicit meta should replace prebuilt _meta' ).
  ENDMETHOD.

  METHOD test_set_related_task.
    cut->set_related_task( '00000000000000000000000000000001' ).

    DATA(json) = cut->zif_mcp_internal~generate_json( ).

    cl_abap_unit_assert=>assert_equals( exp = '00000000000000000000000000000001'
                                        act = json->get_string( '/_meta/io.modelcontextprotocol~1related-task/taskId' )
                                        msg = 'Related task should be written under _meta' ).
  ENDMETHOD.

  METHOD test_related_task_prebuilt_mta.
    DATA(prebuilt) = zcl_mcp_ajson=>parse(
                         `{"content":[{"type":"text","text":"stored result"}],"_meta":{"requestId":"req-1"}}` ).

    cut->set_from_json( prebuilt ).
    cut->set_related_task( '00000000000000000000000000000002' ).

    DATA(json) = cut->zif_mcp_internal~generate_json( ).

    cl_abap_unit_assert=>assert_equals( exp = 'req-1'
                                        act = json->get_string( '/_meta/requestId' )
                                        msg = 'Existing prebuilt _meta should be preserved' ).
    cl_abap_unit_assert=>assert_equals( exp = '00000000000000000000000000000002'
                                        act = json->get_string( '/_meta/io.modelcontextprotocol~1related-task/taskId' )
                                        msg = 'Related task should be added to prebuilt _meta' ).
  ENDMETHOD.
ENDCLASS.
