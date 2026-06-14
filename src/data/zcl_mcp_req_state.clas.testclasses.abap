CLASS ltcl_mcp_req_state DEFINITION FINAL
  FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.

  PRIVATE SECTION.
    CONSTANTS c_area   TYPE string VALUE `UT_AREA`.
    CONSTANTS c_server TYPE string VALUE `UT_SERVER`.
    CONSTANTS c_method TYPE string VALUE `tools/call`.
    CONSTANTS c_data   TYPE string VALUE `unit-test-state`.

    CLASS-DATA sql_env TYPE REF TO if_osql_test_environment.

    CLASS-METHODS class_setup RAISING cx_static_check.
    CLASS-METHODS class_teardown.

    METHODS setup.

    METHODS valid_roundtrip              FOR TESTING RAISING zcx_mcp_ajson_error zcx_mcp_server.
    METHODS tampered_payload_rejected    FOR TESTING RAISING zcx_mcp_ajson_error zcx_mcp_server.
    METHODS tampered_signature_rejected  FOR TESTING RAISING zcx_mcp_ajson_error zcx_mcp_server.
    METHODS missing_payload_rejected     FOR TESTING RAISING zcx_mcp_ajson_error zcx_mcp_server.
    METHODS missing_signature_rejected   FOR TESTING RAISING zcx_mcp_ajson_error zcx_mcp_server.
    METHODS missing_payload_fields       FOR TESTING RAISING zcx_mcp_ajson_error zcx_mcp_server.
    METHODS unsupported_version_rejected FOR TESTING RAISING zcx_mcp_ajson_error zcx_mcp_server.
    METHODS scope_mismatch_rejected      FOR TESTING RAISING zcx_mcp_ajson_error zcx_mcp_server.
    METHODS expired_token_rejected       FOR TESTING RAISING zcx_mcp_ajson_error zcx_mcp_server.
    METHODS replay_rejected              FOR TESTING RAISING zcx_mcp_ajson_error zcx_mcp_server.
    METHODS delete_expired_nonces        FOR TESTING.

    METHODS create_state
      IMPORTING ttl_seconds   TYPE i DEFAULT 300
      RETURNING VALUE(result) TYPE string
      RAISING   zcx_mcp_ajson_error
                zcx_mcp_server.

    METHODS expect_invalid
      IMPORTING request_state TYPE string
                !area         TYPE string  DEFAULT c_area
                server        TYPE string  DEFAULT c_server
                !method       TYPE string  DEFAULT c_method
                !uname        TYPE syuname DEFAULT sy-uname
      RAISING   zcx_mcp_ajson_error.

    METHODS set_payload_string
      IMPORTING request_state TYPE string
                !path         TYPE string
                !value        TYPE string
      RETURNING VALUE(result) TYPE string
      RAISING   zcx_mcp_ajson_error.

    METHODS delete_json_path
      IMPORTING request_state TYPE string
                !path         TYPE string
      RETURNING VALUE(result) TYPE string
      RAISING   zcx_mcp_ajson_error.
ENDCLASS.


CLASS ltcl_mcp_req_state IMPLEMENTATION.
  METHOD class_setup.
    sql_env = cl_osql_test_environment=>create( VALUE #( ( 'ZMCP_REQ_NONCES' ) ) ).
  ENDMETHOD.

  METHOD class_teardown.
    sql_env->destroy( ).
  ENDMETHOD.

  METHOD setup.
    sql_env->clear_doubles( ).
  ENDMETHOD.

  METHOD create_state.
    result = zcl_mcp_req_state=>create( area        = c_area
                                        server      = c_server
                                        method      = c_method
                                        data        = c_data
                                        ttl_seconds = ttl_seconds ).
  ENDMETHOD.

  METHOD expect_invalid.
    TRY.
        zcl_mcp_req_state=>validate( request_state = request_state
                                     area          = area
                                     server        = server
                                     method        = method
                                     uname         = uname ).

        cl_abap_unit_assert=>fail( 'requestState should have been rejected' ).

      CATCH zcx_mcp_server. "#EC EMPTY_CATCH
    ENDTRY.
  ENDMETHOD.

  METHOD set_payload_string.
    DATA(json) = zcl_mcp_ajson=>parse( request_state ).

    json->set_string( iv_path = path
                      iv_val  = value ).

    result = json->stringify( ).
  ENDMETHOD.

  METHOD delete_json_path.
    DATA(json) = zcl_mcp_ajson=>parse( request_state ).

    json->delete( path ).

    result = json->stringify( ).
  ENDMETHOD.

  METHOD valid_roundtrip.
    DATA(request_state) = create_state( ).

    DATA(actual) = zcl_mcp_req_state=>validate( request_state = request_state
                                                area          = c_area
                                                server        = c_server
                                                method        = c_method ).

    cl_abap_unit_assert=>assert_equals( exp = c_area
                                        act = actual-area ).

    cl_abap_unit_assert=>assert_equals( exp = c_server
                                        act = actual-server ).

    cl_abap_unit_assert=>assert_equals( exp = c_method
                                        act = actual-method ).

    cl_abap_unit_assert=>assert_equals( exp = sy-uname
                                        act = actual-uname ).

    cl_abap_unit_assert=>assert_equals( exp = c_data
                                        act = actual-data ).

    cl_abap_unit_assert=>assert_not_initial( actual-nonce ).
    cl_abap_unit_assert=>assert_not_initial( actual-expires_at ).
  ENDMETHOD.

  METHOD tampered_payload_rejected.
    DATA(request_state) = create_state( ).
    DATA(tampered) = set_payload_string( request_state = request_state
                                         path          = `/payload/data`
                                         value         = `changed-state` ).

    expect_invalid( tampered ).
  ENDMETHOD.

  METHOD tampered_signature_rejected.
    DATA(request_state) = create_state( ).
    DATA(json) = zcl_mcp_ajson=>parse( request_state ).
    DATA(signature) = json->get_string( `/sig` ).
    DATA(last_offset) = strlen( signature ) - 1.
    DATA(prefix_len) = last_offset.
    DATA(prefix) = ``.
    DATA(last_char) = ``.
    DATA(new_last_char) = ``.

    IF prefix_len > 0.
      prefix = signature(prefix_len).
    ENDIF.

    last_char = signature+last_offset(1).

    IF last_char = `A`.
      new_last_char = `B`.
    ELSE.
      new_last_char = `A`.
    ENDIF.

    signature = |{ prefix }{ new_last_char }|.

    json->set_string( iv_path = `/sig`
                      iv_val  = signature ).

    expect_invalid( json->stringify( ) ).
  ENDMETHOD.

  METHOD missing_payload_rejected.
    DATA(request_state) = create_state( ).
    DATA(tampered) = delete_json_path( request_state = request_state
                                       path          = `/payload` ).

    expect_invalid( tampered ).
  ENDMETHOD.

  METHOD missing_signature_rejected.
    DATA(request_state) = create_state( ).
    DATA(tampered) = delete_json_path( request_state = request_state
                                       path          = `/sig` ).

    expect_invalid( tampered ).
  ENDMETHOD.

  METHOD missing_payload_fields.
    DATA(request_state) = create_state( ).

    expect_invalid( delete_json_path( request_state = request_state
                                      path          = `/payload/v` ) ).

    request_state = create_state( ).
    expect_invalid( delete_json_path( request_state = request_state
                                      path          = `/payload/nonce` ) ).

    request_state = create_state( ).
    expect_invalid( delete_json_path( request_state = request_state
                                      path          = `/payload/data` ) ).
  ENDMETHOD.

  METHOD unsupported_version_rejected.
    DATA(request_state) = create_state( ).
    DATA(tampered) = set_payload_string( request_state = request_state
                                         path          = `/payload/v`
                                         value         = `2` ).

    expect_invalid( tampered ).
  ENDMETHOD.

  METHOD scope_mismatch_rejected.
    DATA(request_state) = create_state( ).

    expect_invalid( request_state = request_state
                    area          = `OTHER_AREA` ).

    request_state = create_state( ).
    expect_invalid( request_state = request_state
                    server        = `OTHER_SERVER` ).

    request_state = create_state( ).
    expect_invalid( request_state = request_state
                    method        = `prompts/get` ).

    request_state = create_state( ).
    expect_invalid( request_state = request_state
                    uname         = `OTHERUSER` ).
  ENDMETHOD.

  METHOD expired_token_rejected.
    DATA(request_state) = create_state( ttl_seconds = 1 ).

    WAIT UP TO 2 SECONDS.

    expect_invalid( request_state ).
  ENDMETHOD.

  METHOD replay_rejected.
    DATA(request_state) = create_state( ).

    zcl_mcp_req_state=>validate( request_state = request_state
                                 area          = c_area
                                 server        = c_server
                                 method        = c_method ).

    expect_invalid( request_state ).
  ENDMETHOD.

  METHOD delete_expired_nonces.
    DATA expired TYPE zmcp_req_nonces.
    DATA active  TYPE zmcp_req_nonces.
    DATA rows    TYPE TABLE OF zmcp_req_nonces.

    expired-client      = sy-mandt.
    expired-nonce       = 'UT_EXPIRED_NONCE'.
    expired-area        = c_area.
    expired-server      = c_server.
    expired-rpc_method  = c_method.
    expired-uname       = sy-uname.
    expired-expires_at  = '20000101000000'.
    expired-consumed_at = '20000101000000'.

    active-client      = sy-mandt.
    active-nonce       = 'UT_ACTIVE_NONCE'.
    active-area        = c_area.
    active-server      = c_server.
    active-rpc_method  = c_method.
    active-uname       = sy-uname.
    active-expires_at  = '99991231235959'.
    active-consumed_at = '20000101000000'.

    APPEND expired TO rows.
    APPEND active TO rows.
    sql_env->insert_test_data( rows ).

    DATA(deleted) = zcl_mcp_req_state=>delete_expired_nonces( ).

    cl_abap_unit_assert=>assert_equals( exp = 1
                                        act = deleted ).

    SELECT COUNT( * ) FROM zmcp_req_nonces
      WHERE nonce = 'UT_EXPIRED_NONCE'
      INTO @DATA(expired_count).

    cl_abap_unit_assert=>assert_equals( exp = 0
                                        act = expired_count ).

    SELECT COUNT( * ) FROM zmcp_req_nonces
      WHERE nonce = 'UT_ACTIVE_NONCE'
      INTO @DATA(active_count).

    cl_abap_unit_assert=>assert_equals( exp = 1
                                        act = active_count ).
  ENDMETHOD.
ENDCLASS.
