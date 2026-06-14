CLASS ltcl_mcp_resp_v2_tool DEFINITION DEFERRED.
CLASS zcl_mcp_resp_v2_tool DEFINITION LOCAL FRIENDS ltcl_mcp_resp_v2_tool.

CLASS ltcl_mcp_resp_v2_tool DEFINITION FINAL
FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.

  PRIVATE SECTION.
    METHODS text_result         FOR TESTING RAISING zcx_mcp_ajson_error.
    METHODS error_result        FOR TESTING RAISING zcx_mcp_ajson_error.
    METHODS complete_cache_meta FOR TESTING RAISING zcx_mcp_ajson_error.
    METHODS structured_content  FOR TESTING RAISING zcx_mcp_ajson_error.

    METHODS assert_json_equals
      IMPORTING !actual  TYPE string
                expected TYPE string
      RAISING   zcx_mcp_ajson_error.
  ENDCLASS.


CLASS ltcl_mcp_resp_v2_tool IMPLEMENTATION.
  METHOD text_result.
    DATA(result_builder) = NEW zcl_mcp_resp_v2_tool( ).

    result_builder->set_error( abap_false ).
    result_builder->add_text_content( `Hello v2` ).

    DATA(result) = result_builder->generate_json( ).

    assert_json_equals( actual   = result->stringify( )
                        expected = `{"content":[{"type":"text","text":"Hello v2"}],"isError":false}` ).
  ENDMETHOD.

  METHOD error_result.
    DATA(result_builder) = NEW zcl_mcp_resp_v2_tool( ).

    result_builder->set_error( abap_true ).
    result_builder->add_text_content( `Tool failed` ).

    DATA(result) = result_builder->generate_json( ).

    assert_json_equals( actual   = result->stringify( )
                        expected = `{"content":[{"type":"text","text":"Tool failed"}],"isError":true}` ).
  ENDMETHOD.

  METHOD complete_cache_meta.
    DATA(result_builder) = NEW zcl_mcp_resp_v2_tool( ).
    DATA(meta) = zcl_mcp_ajson=>create_empty( ).

    meta->set_string( iv_path = `/source`
                      iv_val  = `unit-test` ).

    result_builder->set_complete( ).
    result_builder->set_error( abap_false ).
    result_builder->add_text_content( `Cacheable result` ).
    result_builder->set_cache( ttl_ms      = 2500
                               cache_scope = zif_mcp_constants=>cache_scopes-private ).
    result_builder->set_meta( meta ).

    DATA(result) = result_builder->generate_json( ).

    assert_json_equals(
        actual   = result->stringify( )
        expected = |\{"content":[\{"type":"text","text":"Cacheable result"\}],"isError":false,|
         && |"resultType":"complete","ttlMs":2500,"cacheScope":"private","_meta":\{"source":"unit-test"\}\}| ).
  ENDMETHOD.

  METHOD structured_content.
    DATA(result_builder) = NEW zcl_mcp_resp_v2_tool( ).
    DATA(structured) = zcl_mcp_ajson=>create_empty( ).

    structured->set_string( iv_path = `/value`
                            iv_val  = `abc` ).

    result_builder->set_error( abap_false ).
    result_builder->set_structured_content( structured_content = structured
                                            add_text_content   = abap_false ).

    DATA(result) = result_builder->generate_json( ).

    assert_json_equals( actual   = result->stringify( )
                        expected = `{"content":[],"structuredContent":{"value":"abc"},"isError":false}` ).
  ENDMETHOD.

  METHOD assert_json_equals.
    DATA(actual_obj) = zcl_mcp_ajson=>parse( actual ).
    DATA(expected_obj) = zcl_mcp_ajson=>parse( expected ).

    cl_abap_unit_assert=>assert_equals( exp = expected_obj->stringify( )
                                        act = actual_obj->stringify( ) ).
  ENDMETHOD.
ENDCLASS.
