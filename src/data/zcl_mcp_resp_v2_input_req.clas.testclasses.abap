CLASS ltcl_mcp_resp_v2_input_req DEFINITION DEFERRED.
CLASS zcl_mcp_resp_v2_input_req DEFINITION LOCAL FRIENDS ltcl_mcp_resp_v2_input_req.

CLASS ltcl_mcp_resp_v2_input_req DEFINITION FINAL
FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.

  PRIVATE SECTION.
    METHODS empty_input_required FOR TESTING.
    METHODS with_input_request   FOR TESTING.
    METHODS with_empty_params    FOR TESTING.
    METHODS cache_and_meta       FOR TESTING.

    METHODS assert_json_equals
      IMPORTING !actual  TYPE string
                expected TYPE string.
ENDCLASS.


CLASS ltcl_mcp_resp_v2_input_req IMPLEMENTATION.
  METHOD empty_input_required.
    DATA cut  TYPE REF TO zcl_mcp_resp_v2_input_req.
    DATA json TYPE REF TO zif_mcp_ajson.

    cut = NEW zcl_mcp_resp_v2_input_req( ).

    TRY.
        json = cut->zif_mcp_modern_result~generate_json( ).
        assert_json_equals( actual   = json->stringify( )
                            expected = `{"resultType":"input_required","inputRequests":{}}` ).
      CATCH zcx_mcp_ajson_error INTO DATA(error).
        cl_abap_unit_assert=>fail( error->get_text( ) ).
    ENDTRY.
  ENDMETHOD.

  METHOD with_input_request.
    DATA cut    TYPE REF TO zcl_mcp_resp_v2_input_req.
    DATA params TYPE REF TO zif_mcp_ajson.
    DATA json   TYPE REF TO zif_mcp_ajson.

    cut = NEW zcl_mcp_resp_v2_input_req( ).
    params = zcl_mcp_ajson=>create_empty( ).

    TRY.
        params->set_string( iv_path = `/message`
                            iv_val  = `Confirm` ).
        params->set_string( iv_path = `/requestedSchema/type`
                            iv_val  = `object` ).

        cut->set_request_state( `state-1` ).
        cut->add_input_request( request_key = `confirm`
                                method      = `elicitation/create`
                                params      = params ).

        json = cut->zif_mcp_modern_result~generate_json( ).

        assert_json_equals(
            actual   = json->stringify( )
            expected = `{"resultType":"input_required","requestState":"state-1","inputRequests":{"confirm":{"method":"elicitation/create","params":{"message":"Confirm","requestedSchema":{"type":"object"}}}}}` ).
      CATCH zcx_mcp_ajson_error INTO DATA(error).
        cl_abap_unit_assert=>fail( error->get_text( ) ).
    ENDTRY.
  ENDMETHOD.

  METHOD with_empty_params.
    DATA cut      TYPE REF TO zcl_mcp_resp_v2_input_req.
    DATA requests TYPE zcl_mcp_resp_v2_input_req=>input_requests.
    DATA json     TYPE REF TO zif_mcp_ajson.

    cut = NEW zcl_mcp_resp_v2_input_req( ).

    APPEND VALUE #( request_key = `empty`
                    method      = `sampling/createMessage` ) TO requests.

    TRY.
        cut->set_input_requests( requests ).
        json = cut->zif_mcp_modern_result~generate_json( ).

        assert_json_equals(
            actual   = json->stringify( )
            expected = `{"resultType":"input_required","inputRequests":{"empty":{"method":"sampling/createMessage","params":{}}}}` ).
      CATCH zcx_mcp_ajson_error INTO DATA(error).
        cl_abap_unit_assert=>fail( error->get_text( ) ).
    ENDTRY.
  ENDMETHOD.

  METHOD cache_and_meta.
    DATA cut  TYPE REF TO zcl_mcp_resp_v2_input_req.
    DATA meta TYPE REF TO zif_mcp_ajson.
    DATA json TYPE REF TO zif_mcp_ajson.

    cut = NEW zcl_mcp_resp_v2_input_req( ).
    meta = zcl_mcp_ajson=>create_empty( ).

    TRY.
        meta->set_string( iv_path = `/vendor~1trace`
                          iv_val  = `abc` ).

        cut->zif_mcp_modern_result~set_meta( meta ).
        cut->zif_mcp_modern_result~set_cache( ttl_ms      = 100
                                              cache_scope = zif_mcp_constants=>cache_scopes-private ).

        json = cut->zif_mcp_modern_result~generate_json( ).

        assert_json_equals(
            actual   = json->stringify( )
            expected = `{"resultType":"input_required","inputRequests":{},"ttlMs":100,"cacheScope":"private","_meta":{"vendor/trace":"abc"}}` ).
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
