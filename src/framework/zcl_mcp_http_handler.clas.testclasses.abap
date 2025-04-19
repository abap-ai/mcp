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
    METHODS test_valid_mcp_path    FOR TESTING.
    METHODS test_invalid_mcp_paths FOR TESTING.
    METHODS test_classify_batch    FOR TESTING.
ENDCLASS.

CLASS ltcl_mcp_http_handler IMPLEMENTATION.
  METHOD setup.
    cut = NEW zcl_mcp_http_handler( ).
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
    DATA: has_requests  TYPE abap_bool,
          has_responses TYPE abap_bool,
          has_notifs    TYPE abap_bool.

    " Batch of requests
    cut->classify_message(
      EXPORTING
        json = '[{"jsonrpc":"2.0","method":"method1","id":"1"},{"jsonrpc":"2.0","method":"method2","id":"2"}]'
      IMPORTING
        has_requests = has_requests
        has_responses = has_responses
        has_notifs = has_notifs
    ).

    cl_abap_unit_assert=>assert_true( act = has_requests
                                      msg = 'Batch array should be classified as containing requests' ).

    cl_abap_unit_assert=>assert_false(
      act = has_responses
      msg = 'Batch of requests should not be classified as responses'
    ).

    cl_abap_unit_assert=>assert_false(
      act = has_notifs
      msg = 'Batch of requests should not be classified as notifications'
    ).
  ENDMETHOD.
ENDCLASS.
