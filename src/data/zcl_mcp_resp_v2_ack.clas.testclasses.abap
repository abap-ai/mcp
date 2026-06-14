CLASS ltcl_mcp_resp_v2_ack DEFINITION DEFERRED.
CLASS zcl_mcp_resp_v2_ack DEFINITION LOCAL FRIENDS ltcl_mcp_resp_v2_ack.

CLASS ltcl_mcp_resp_v2_ack DEFINITION FINAL
FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.

  PRIVATE SECTION.
    METHODS empty_ack               FOR TESTING RAISING zcx_mcp_ajson_error.
    METHODS complete_ack            FOR TESTING RAISING zcx_mcp_ajson_error.
    METHODS cache_and_meta          FOR TESTING RAISING zcx_mcp_ajson_error.
    METHODS content_variants        FOR TESTING RAISING zcx_mcp_ajson_error.
    METHODS cache_default_scope     FOR TESTING RAISING zcx_mcp_ajson_error.
    METHODS modern_result_interface FOR TESTING RAISING zcx_mcp_ajson_error.

    METHODS assert_json_equals
      IMPORTING !actual  TYPE string
                expected TYPE string
      RAISING   zcx_mcp_ajson_error.
ENDCLASS.

CLASS ltcl_mcp_resp_v2_ack IMPLEMENTATION.
  METHOD empty_ack.
    DATA ack    TYPE REF TO zcl_mcp_resp_v2_ack.
    DATA result TYPE REF TO zif_mcp_ajson.

    ack = NEW zcl_mcp_resp_v2_ack( ).
    result = ack->generate_json( ).

    assert_json_equals( actual   = result->stringify( )
                        expected = `{}` ).
  ENDMETHOD.

  METHOD complete_ack.
    DATA ack    TYPE REF TO zcl_mcp_resp_v2_ack.
    DATA result TYPE REF TO zif_mcp_ajson.

    ack = NEW zcl_mcp_resp_v2_ack( ).
    ack->set_complete( ).
    result = ack->generate_json( ).

    cl_abap_unit_assert=>assert_equals( exp = zif_mcp_constants=>result_types-complete
                                        act = result->get_string( `/resultType` ) ).
  ENDMETHOD.

  METHOD cache_and_meta.
    DATA ack    TYPE REF TO zcl_mcp_resp_v2_ack.
    DATA meta   TYPE REF TO zif_mcp_ajson.
    DATA result TYPE REF TO zif_mcp_ajson.

    ack = NEW zcl_mcp_resp_v2_ack( ).
    meta = zcl_mcp_ajson=>create_empty( ).
    meta->set_string( iv_path = `/source`
                      iv_val  = `unit-test` ).

    ack->set_complete( ).
    ack->set_cache( ttl_ms      = 2500
                    cache_scope = zif_mcp_constants=>cache_scopes-private ).
    ack->set_meta( meta ).

    result = ack->generate_json( ).

    cl_abap_unit_assert=>assert_equals( exp = zif_mcp_constants=>result_types-complete
                                        act = result->get_string( `/resultType` ) ).
    cl_abap_unit_assert=>assert_equals( exp = 2500
                                        act = result->get_integer( `/ttlMs` ) ).
    cl_abap_unit_assert=>assert_equals( exp = zif_mcp_constants=>cache_scopes-private
                                        act = result->get_string( `/cacheScope` ) ).
    cl_abap_unit_assert=>assert_equals( exp = `unit-test`
                                        act = result->get_string( `/_meta/source` ) ).
  ENDMETHOD.

  METHOD assert_json_equals.
    DATA actual_obj   TYPE REF TO zif_mcp_ajson.
    DATA expected_obj TYPE REF TO zif_mcp_ajson.

    actual_obj = zcl_mcp_ajson=>parse( actual ).
    expected_obj = zcl_mcp_ajson=>parse( expected ).

    cl_abap_unit_assert=>assert_equals( exp = expected_obj->stringify( )
                                        act = actual_obj->stringify( ) ).
  ENDMETHOD.

  METHOD content_variants.
    DATA(result_builder) = NEW zcl_mcp_resp_v2_tool( ).

    result_builder->add_image_content( data      = `aW1hZ2U=`
                                       mime_type = `image/png` ).
    result_builder->add_audio_content( data      = `YXVkaW8=`
                                       mime_type = `audio/wav` ).
    result_builder->add_resource_link( uri         = `file:///report.pdf`
                                       name        = `report`
                                       title       = `Report`
                                       description = `Monthly report`
                                       mime_type   = `application/pdf`
                                       size        = 42 ).
    result_builder->add_text_resource( uri       = `file:///readme.txt`
                                       text      = `Read me`
                                       mime_type = `text/plain` ).
    result_builder->add_blob_resource( uri       = `file:///archive.bin`
                                       blob      = `YmxvYg==`
                                       mime_type = `application/octet-stream` ).

    DATA(result) = result_builder->generate_json( ).

    cl_abap_unit_assert=>assert_equals( exp = `image`
                                        act = result->get_string( `/content/1/type` ) ).
    cl_abap_unit_assert=>assert_equals( exp = `aW1hZ2U=`
                                        act = result->get_string( `/content/1/data` ) ).
    cl_abap_unit_assert=>assert_equals( exp = `image/png`
                                        act = result->get_string( `/content/1/mimeType` ) ).

    cl_abap_unit_assert=>assert_equals( exp = `audio`
                                        act = result->get_string( `/content/2/type` ) ).
    cl_abap_unit_assert=>assert_equals( exp = `YXVkaW8=`
                                        act = result->get_string( `/content/2/data` ) ).
    cl_abap_unit_assert=>assert_equals( exp = `audio/wav`
                                        act = result->get_string( `/content/2/mimeType` ) ).

    cl_abap_unit_assert=>assert_equals( exp = `resource_link`
                                        act = result->get_string( `/content/3/type` ) ).
    cl_abap_unit_assert=>assert_equals( exp = `file:///report.pdf`
                                        act = result->get_string( `/content/3/uri` ) ).
    cl_abap_unit_assert=>assert_equals( exp = `report`
                                        act = result->get_string( `/content/3/name` ) ).
    cl_abap_unit_assert=>assert_equals( exp = `Report`
                                        act = result->get_string( `/content/3/title` ) ).
    cl_abap_unit_assert=>assert_equals( exp = `Monthly report`
                                        act = result->get_string( `/content/3/description` ) ).
    cl_abap_unit_assert=>assert_equals( exp = `application/pdf`
                                        act = result->get_string( `/content/3/mimeType` ) ).
    cl_abap_unit_assert=>assert_equals( exp = 42
                                        act = result->get_integer( `/content/3/size` ) ).

    cl_abap_unit_assert=>assert_equals( exp = `resource`
                                        act = result->get_string( `/content/4/type` ) ).
    cl_abap_unit_assert=>assert_equals( exp = `file:///readme.txt`
                                        act = result->get_string( `/content/4/resource/uri` ) ).
    cl_abap_unit_assert=>assert_equals( exp = `Read me`
                                        act = result->get_string( `/content/4/resource/text` ) ).
    cl_abap_unit_assert=>assert_equals( exp = `text/plain`
                                        act = result->get_string( `/content/4/resource/mimeType` ) ).

    cl_abap_unit_assert=>assert_equals( exp = `resource`
                                        act = result->get_string( `/content/5/type` ) ).
    cl_abap_unit_assert=>assert_equals( exp = `file:///archive.bin`
                                        act = result->get_string( `/content/5/resource/uri` ) ).
    cl_abap_unit_assert=>assert_equals( exp = `YmxvYg==`
                                        act = result->get_string( `/content/5/resource/blob` ) ).
    cl_abap_unit_assert=>assert_equals( exp = `application/octet-stream`
                                        act = result->get_string( `/content/5/resource/mimeType` ) ).
  ENDMETHOD.

  METHOD cache_default_scope.
    DATA(result_builder) = NEW zcl_mcp_resp_v2_tool( ).

    result_builder->add_text_content( `cache default` ).
    result_builder->set_cache( ttl_ms      = 100
                               cache_scope = `` ).

    DATA(result) = result_builder->generate_json( ).

    cl_abap_unit_assert=>assert_equals( exp = 100
                                        act = result->get_integer( `/ttlMs` ) ).
    cl_abap_unit_assert=>assert_equals( exp = zif_mcp_constants=>cache_scopes-private
                                        act = result->get_string( `/cacheScope` ) ).
  ENDMETHOD.

  METHOD modern_result_interface.
    DATA(result_builder) = NEW zcl_mcp_resp_v2_tool( ).
    DATA(modern_result) = CAST zif_mcp_modern_result( result_builder ).
    DATA(meta) = zcl_mcp_ajson=>create_empty( ).

    meta->set_string( iv_path = `/source`
                      iv_val  = `interface` ).

    result_builder->add_text_content( `from interface` ).
    modern_result->set_cache( ttl_ms      = 750
                              cache_scope = zif_mcp_constants=>cache_scopes-private ).
    modern_result->set_meta( meta ).

    DATA(result) = modern_result->generate_json( ).

    cl_abap_unit_assert=>assert_equals( exp = `from interface`
                                        act = result->get_string( `/content/1/text` ) ).
    cl_abap_unit_assert=>assert_equals( exp = 750
                                        act = result->get_integer( `/ttlMs` ) ).
    cl_abap_unit_assert=>assert_equals( exp = zif_mcp_constants=>cache_scopes-private
                                        act = result->get_string( `/cacheScope` ) ).
    cl_abap_unit_assert=>assert_equals( exp = `interface`
                                        act = result->get_string( `/_meta/source` ) ).
  ENDMETHOD.
ENDCLASS.
