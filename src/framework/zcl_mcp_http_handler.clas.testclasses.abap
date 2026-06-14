*"* use this source file for your ABAP unit test classes
CLASS ltcl_mcp_http_handler DEFINITION DEFERRED.
CLASS zcl_mcp_http_handler DEFINITION LOCAL FRIENDS ltcl_mcp_http_handler.
CLASS ltcl_mcp_http_handler DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA cut TYPE REF TO zcl_mcp_http_handler.

    METHODS setup.
    " Path parsing tests
    METHODS test_valid_mcp_path            FOR TESTING.
    METHODS test_invalid_mcp_paths         FOR TESTING.
    METHODS test_classify_batch            FOR TESTING.
    METHODS test_classify_id_zero          FOR TESTING.
    METHODS test_classify_notification     FOR TESTING.
    METHODS test_classify_response         FOR TESTING.
    METHODS test_classify_invalid_json     FOR TESTING.

    METHODS test_validate_session_valid    FOR TESTING.
    METHODS test_validate_session_invalid  FOR TESTING.

    METHODS test_recover_jsonrpc_id        FOR TESTING RAISING zcx_mcp_ajson_error.

    METHODS test_create_error_json_with_id FOR TESTING RAISING zcx_mcp_ajson_error.
    METHODS test_create_error_json_no_id   FOR TESTING RAISING zcx_mcp_ajson_error.
ENDCLASS.

CLASS ltcl_mcp_http_handler IMPLEMENTATION.
  METHOD setup.
    cut = NEW zcl_mcp_http_handler( ).
    cut->jsonrpc = NEW zcl_mcp_jsonrpc( ).
  ENDMETHOD.

  METHOD test_valid_mcp_path.
    DATA area   TYPE zmcp_area.
    DATA server TYPE zmcp_server.
    DATA valid  TYPE abap_bool.

    " Test case: Standard valid path
    cut->parse_mcp_path( EXPORTING path   = '/llm/completion'
                         IMPORTING area   = area
                                   server = server
                                   valid  = valid ).

    cl_abap_unit_assert=>assert_equals( exp = 'llm'
                                        act = area
                                        msg = 'Area should be extracted correctly' ).

    cl_abap_unit_assert=>assert_equals( exp = 'completion'
                                        act = server
                                        msg = 'Server name should be extracted correctly' ).

    cl_abap_unit_assert=>assert_true( act = valid
                                      msg = 'Path should be recognized as valid' ).

    " Test case: Path with trailing slash
    cut->parse_mcp_path( EXPORTING path   = '/analytics/embedding/'
                         IMPORTING area   = area
                                   server = server
                                   valid  = valid ).

    cl_abap_unit_assert=>assert_equals( exp = 'analytics'
                                        act = area
                                        msg = 'Area should be extracted correctly with trailing slash' ).

    cl_abap_unit_assert=>assert_equals( exp = 'embedding'
                                        act = server
                                        msg = 'Server name should be extracted correctly with trailing slash' ).

    cl_abap_unit_assert=>assert_true( act = valid
                                      msg = 'Path with trailing slash should be recognized as valid' ).
  ENDMETHOD.

  METHOD test_invalid_mcp_paths.
    DATA area   TYPE zmcp_area.
    DATA server TYPE zmcp_server.
    DATA valid  TYPE abap_bool.

    " Test case: Missing server name
    cut->parse_mcp_path( EXPORTING path   = '/llm'
                         IMPORTING area   = area
                                   server = server
                                   valid  = valid ).

    cl_abap_unit_assert=>assert_false( act = valid
                                       msg = 'Path without server name should be invalid' ).

    " Test case: Wrong prefix
    cut->parse_mcp_path( EXPORTING path   = '/llm/completion/blubb'
                         IMPORTING area   = area
                                   server = server
                                   valid  = valid ).

    cl_abap_unit_assert=>assert_false( act = valid
                                       msg = 'Path with wrong prefix should be invalid' ).

    " Test case: Empty path
    cut->parse_mcp_path( EXPORTING path   = ''
                         IMPORTING area   = area
                                   server = server
                                   valid  = valid ).

    cl_abap_unit_assert=>assert_false( act = valid
                                       msg = 'Empty path should be invalid' ).
  ENDMETHOD.

  METHOD test_classify_batch.
    DATA has_requests  TYPE abap_bool.
    DATA has_responses TYPE abap_bool.
    DATA has_notifs    TYPE abap_bool.

    " JSON-RPC batch arrays are intentionally unsupported by the MCP HTTP handler.
    cut->classify_message(
      EXPORTING
        json          = '[{"jsonrpc":"2.0","method":"method1","id":"1"},{"jsonrpc":"2.0","method":"method2","id":"2"}]'
      IMPORTING
        has_requests  = has_requests
        has_responses = has_responses
        has_notifs    = has_notifs ).

    cl_abap_unit_assert=>assert_false( act = has_requests
                                       msg = 'Batch array must not be classified as request' ).

    cl_abap_unit_assert=>assert_false( act = has_responses
                                       msg = 'Batch array must not be classified as response' ).

    cl_abap_unit_assert=>assert_false( act = has_notifs
                                       msg = 'Batch array must not be classified as notification' ).
  ENDMETHOD.

  METHOD test_classify_id_zero.
    DATA has_requests  TYPE abap_bool.
    DATA has_responses TYPE abap_bool.
    DATA has_notifs    TYPE abap_bool.

    cut->classify_message( EXPORTING json          = '{"jsonrpc":"2.0","method":"ping","id":0}'
                           IMPORTING has_requests  = has_requests
                                     has_responses = has_responses
                                     has_notifs    = has_notifs ).

    cl_abap_unit_assert=>assert_true( act = has_requests
                                      msg = 'id 0 is a valid JSON-RPC request id' ).

    cl_abap_unit_assert=>assert_false( has_responses ).
    cl_abap_unit_assert=>assert_false( has_notifs ).
  ENDMETHOD.

  METHOD test_classify_notification.
    DATA has_requests  TYPE abap_bool.
    DATA has_responses TYPE abap_bool.
    DATA has_notifs    TYPE abap_bool.

    cut->classify_message( EXPORTING json          = `{"jsonrpc":"2.0","method":"notifications/initialized"}`
                           IMPORTING has_requests  = has_requests
                                     has_responses = has_responses
                                     has_notifs    = has_notifs ).

    cl_abap_unit_assert=>assert_false( has_requests ).
    cl_abap_unit_assert=>assert_false( has_responses ).
    cl_abap_unit_assert=>assert_true( has_notifs ).
  ENDMETHOD.

  METHOD test_classify_response.
    DATA has_requests  TYPE abap_bool.
    DATA has_responses TYPE abap_bool.
    DATA has_notifs    TYPE abap_bool.

    cut->classify_message( EXPORTING json          = `{"jsonrpc":"2.0","result":{},"id":1}`
                           IMPORTING has_requests  = has_requests
                                     has_responses = has_responses
                                     has_notifs    = has_notifs ).

    cl_abap_unit_assert=>assert_false( has_requests ).
    cl_abap_unit_assert=>assert_true( has_responses ).
    cl_abap_unit_assert=>assert_false( has_notifs ).

    cut->classify_message( EXPORTING json          = `{"jsonrpc":"2.0","error":{"code":-32600,"message":"Invalid Request"},"id":1}`
                           IMPORTING has_requests  = has_requests
                                     has_responses = has_responses
                                     has_notifs    = has_notifs ).

    cl_abap_unit_assert=>assert_false( has_requests ).
    cl_abap_unit_assert=>assert_true( has_responses ).
    cl_abap_unit_assert=>assert_false( has_notifs ).
  ENDMETHOD.

  METHOD test_classify_invalid_json.
    DATA has_requests  TYPE abap_bool.
    DATA has_responses TYPE abap_bool.
    DATA has_notifs    TYPE abap_bool.

    cut->classify_message( EXPORTING json          = `{"jsonrpc":"2.0",`
                           IMPORTING has_requests  = has_requests
                                     has_responses = has_responses
                                     has_notifs    = has_notifs ).

    cl_abap_unit_assert=>assert_false( has_requests ).
    cl_abap_unit_assert=>assert_false( has_responses ).
    cl_abap_unit_assert=>assert_false( has_notifs ).
  ENDMETHOD.

  METHOD test_validate_session_valid.
    TRY.
        cut->validate_session_id( `0123456789abcdef0123456789ABCDEF` ).
      CATCH zcx_mcp_server INTO DATA(error).
        cl_abap_unit_assert=>fail( error->get_text( ) ).
    ENDTRY.
  ENDMETHOD.

  METHOD test_validate_session_invalid.
    TRY.
        cut->validate_session_id( `` ).
        cl_abap_unit_assert=>fail( `Expected empty session id to be rejected` ).
      CATCH zcx_mcp_server INTO DATA(error).
        cl_abap_unit_assert=>assert_equals( exp = zcx_mcp_server=>session_unknown
                                            act = error->if_t100_message~t100key ).
    ENDTRY.

    TRY.
        cut->validate_session_id( `1234` ).
        cl_abap_unit_assert=>fail( `Expected short session id to be rejected` ).
      CATCH zcx_mcp_server INTO error.
        cl_abap_unit_assert=>assert_equals( exp = zcx_mcp_server=>session_unknown
                                            act = error->if_t100_message~t100key ).
    ENDTRY.

    TRY.
        cut->validate_session_id( `0123456789abcdef0123456789abcdeg` ).
        cl_abap_unit_assert=>fail( `Expected non-hex session id to be rejected` ).
      CATCH zcx_mcp_server INTO error.
        cl_abap_unit_assert=>assert_equals( exp = zcx_mcp_server=>session_unknown
                                            act = error->if_t100_message~t100key ).
    ENDTRY.
  ENDMETHOD.

  METHOD test_recover_jsonrpc_id.
    DATA id         TYPE string.
    DATA id_present TYPE abap_bool.

    cut->recover_jsonrpc_id( EXPORTING json_obj   = zcl_mcp_ajson=>parse( `{"id":"abc"}` )
                             IMPORTING id         = id
                                       id_present = id_present ).

    cl_abap_unit_assert=>assert_equals( exp = `abc`
                                        act = id ).
    cl_abap_unit_assert=>assert_true( id_present ).

    cut->recover_jsonrpc_id( EXPORTING json_obj   = zcl_mcp_ajson=>parse( `{"id":0}` )
                             IMPORTING id         = id
                                       id_present = id_present ).

    cl_abap_unit_assert=>assert_equals( exp = `0`
                                        act = id ).
    cl_abap_unit_assert=>assert_true( id_present ).

    cut->recover_jsonrpc_id( EXPORTING json_obj   = zcl_mcp_ajson=>parse( `{"id":null}` )
                             IMPORTING id         = id
                                       id_present = id_present ).

    cl_abap_unit_assert=>assert_initial( id ).
    cl_abap_unit_assert=>assert_false( id_present ).

    cut->recover_jsonrpc_id( EXPORTING json_obj   = zcl_mcp_ajson=>parse( `{"id":{"bad":true}}` )
                             IMPORTING id         = id
                                       id_present = id_present ).

    cl_abap_unit_assert=>assert_initial( id ).
    cl_abap_unit_assert=>assert_false( id_present ).
  ENDMETHOD.

  METHOD test_create_error_json_with_id.
    DATA(error_json) = cut->create_error_json( code    = zcl_mcp_jsonrpc=>error_codes-invalid_request
                                               message = `Invalid Request`
                                               json    = `{"jsonrpc":"2.0","method":"ping","id":0}` ).

    DATA(result) = zcl_mcp_ajson=>parse( error_json ).

    cl_abap_unit_assert=>assert_equals( exp = `2.0`
                                        act = result->get_string( `/jsonrpc` ) ).
    cl_abap_unit_assert=>assert_equals( exp = zcl_mcp_jsonrpc=>error_codes-invalid_request
                                        act = result->get_integer( `/error/code` ) ).
    cl_abap_unit_assert=>assert_equals( exp = `Invalid Request`
                                        act = result->get_string( `/error/message` ) ).
    cl_abap_unit_assert=>assert_equals( exp = 0
                                        act = result->get_integer( `/id` ) ).
  ENDMETHOD.

  METHOD test_create_error_json_no_id.
    DATA(error_json) = cut->create_error_json( code    = zcl_mcp_jsonrpc=>error_codes-parse_error
                                               message = `Invalid JSON`
                                               json    = `{"jsonrpc":"2.0",` ).

    DATA(result) = zcl_mcp_ajson=>parse( error_json ).

    cl_abap_unit_assert=>assert_equals( exp = zcl_mcp_jsonrpc=>error_codes-parse_error
                                        act = result->get_integer( `/error/code` ) ).
    cl_abap_unit_assert=>assert_equals( exp = `Invalid JSON`
                                        act = result->get_string( `/error/message` ) ).
    cl_abap_unit_assert=>assert_false( act = result->exists( `/id` )
                                       msg = `Malformed JSON should not recover an id` ).
  ENDMETHOD.
ENDCLASS.
