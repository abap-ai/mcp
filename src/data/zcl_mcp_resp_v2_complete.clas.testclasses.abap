CLASS ltcl_mcp_resp_v2_complete DEFINITION DEFERRED.
CLASS zcl_mcp_resp_v2_complete DEFINITION LOCAL FRIENDS ltcl_mcp_resp_v2_complete.

CLASS ltcl_mcp_resp_v2_complete DEFINITION FINAL
FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.

  PRIVATE SECTION.
    METHODS empty_complete_result   FOR TESTING.
    METHODS payload_complete_result FOR TESTING.
    METHODS cache_and_meta          FOR TESTING.

    METHODS assert_json_equals
      IMPORTING !actual  TYPE string
                expected TYPE string.
ENDCLASS.


CLASS ltcl_mcp_resp_v2_complete IMPLEMENTATION.
  METHOD empty_complete_result.
    DATA cut  TYPE REF TO zcl_mcp_resp_v2_complete.
    DATA json TYPE REF TO zif_mcp_ajson.

    cut = NEW zcl_mcp_resp_v2_complete( ).

    TRY.
        json = cut->zif_mcp_modern_result~generate_json( ).
        assert_json_equals( actual   = json->stringify( )
                            expected = `{"resultType":"complete"}` ).
      CATCH zcx_mcp_ajson_error INTO DATA(error).
        cl_abap_unit_assert=>fail( error->get_text( ) ).
    ENDTRY.
  ENDMETHOD.

  METHOD payload_complete_result.
    DATA cut     TYPE REF TO zcl_mcp_resp_v2_complete.
    DATA payload TYPE REF TO zif_mcp_ajson.
    DATA json    TYPE REF TO zif_mcp_ajson.

    cut = NEW zcl_mcp_resp_v2_complete( ).
    payload = zcl_mcp_ajson=>create_empty( ).

    TRY.
        payload->set_string( iv_path = `/value`
                             iv_val  = `test` ).
        cut->set_payload( payload ).

        json = cut->zif_mcp_modern_result~generate_json( ).

        assert_json_equals( actual   = json->stringify( )
                            expected = `{"resultType":"complete","value":"test"}` ).

        cl_abap_unit_assert=>assert_false( payload->exists( `/resultType` ) ).
      CATCH zcx_mcp_ajson_error INTO DATA(error).
        cl_abap_unit_assert=>fail( error->get_text( ) ).
    ENDTRY.
  ENDMETHOD.

  METHOD cache_and_meta.
    DATA cut  TYPE REF TO zcl_mcp_resp_v2_complete.
    DATA meta TYPE REF TO zif_mcp_ajson.
    DATA json TYPE REF TO zif_mcp_ajson.

    cut = NEW zcl_mcp_resp_v2_complete( ).
    meta = zcl_mcp_ajson=>create_empty( ).

    TRY.
        meta->set_string( iv_path = `/vendor~1trace`
                          iv_val  = `abc` ).

        cut->zif_mcp_modern_result~set_meta( meta ).
        cut->zif_mcp_modern_result~set_cache( ttl_ms      = 5000
                                              cache_scope = zif_mcp_constants=>cache_scopes-public ).

        json = cut->zif_mcp_modern_result~generate_json( ).

        assert_json_equals(
            actual   = json->stringify( )
            expected = `{"resultType":"complete","ttlMs":5000,"cacheScope":"public","_meta":{"vendor/trace":"abc"}}` ).
      CATCH zcx_mcp_ajson_error INTO DATA(error).
        cl_abap_unit_assert=>fail( error->get_text( ) ).
    ENDTRY.
  ENDMETHOD.

  METHOD assert_json_equals.
    TRY.
        DATA(actual_obj) = zcl_mcp_ajson=>parse( actual ).
        DATA(expected_obj) = zcl_mcp_ajson=>parse( expected ).

        cl_abap_unit_assert=>assert_equals( exp = expected_obj->stringify( )
                                            act = actual_obj->stringify( ) ).
      CATCH zcx_mcp_ajson_error INTO DATA(error).
        cl_abap_unit_assert=>fail( error->get_text( ) ).
    ENDTRY.
  ENDMETHOD.
ENDCLASS.
