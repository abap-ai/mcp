CLASS ltcl_mcp_resp_v2_ack DEFINITION DEFERRED.
CLASS zcl_mcp_resp_v2_ack DEFINITION LOCAL FRIENDS ltcl_mcp_resp_v2_ack.

CLASS ltcl_mcp_resp_v2_ack DEFINITION FINAL
FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.

  PRIVATE SECTION.
    METHODS empty_ack      FOR TESTING RAISING zcx_mcp_ajson_error.
    METHODS complete_ack   FOR TESTING RAISING zcx_mcp_ajson_error.
    METHODS cache_and_meta FOR TESTING RAISING zcx_mcp_ajson_error.

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
ENDCLASS.
