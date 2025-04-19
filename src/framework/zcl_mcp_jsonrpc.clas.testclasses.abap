CLASS ltcl_mcp_jsonrpc DEFINITION DEFERRED.
CLASS zcl_mcp_jsonrpc DEFINITION LOCAL FRIENDS ltcl_mcp_jsonrpc.
CLASS ltcl_mcp_jsonrpc DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA cut TYPE REF TO zcl_mcp_jsonrpc.

    " Test setup method - runs before each test
    METHODS setup.

    " Test methods for core functionality
    METHODS create_request           FOR TESTING.
    METHODS create_notification      FOR TESTING.
    METHODS create_success_response  FOR TESTING RAISING zcx_mcp_ajson_error.
    METHODS create_error_response    FOR TESTING RAISING zcx_mcp_ajson_error.

    " Test methods for JSON operations
    METHODS serialize_request        FOR TESTING RAISING zcx_mcp_ajson_error.
    METHODS parse_request            FOR TESTING RAISING zcx_mcp_ajson_error.
    METHODS parse_request_numeric_id FOR TESTING RAISING zcx_mcp_ajson_error.
    METHODS serialize_response       FOR TESTING RAISING zcx_mcp_ajson_error.
    METHODS parse_response           FOR TESTING RAISING zcx_mcp_ajson_error.

    " Test methods for batch operations
    METHODS parse_batch_request      FOR TESTING RAISING zcx_mcp_ajson_error.
    METHODS serialize_batch_response FOR TESTING RAISING zcx_mcp_ajson_error.

    " Helper methods
    METHODS assert_json_equals
      IMPORTING !actual  TYPE string
                expected TYPE string
      RAISING   zcx_mcp_ajson_error.
ENDCLASS.

CLASS ltcl_mcp_jsonrpc IMPLEMENTATION.
  METHOD setup.
    cut = NEW zcl_mcp_jsonrpc( ).
  ENDMETHOD.

  METHOD create_request.
    " Test creating a standard request
    DATA(request) = cut->create_request( method = 'test.method'
                                         id     = '123' ).

    " Assert
    cl_abap_unit_assert=>assert_equals( exp = '2.0'
                                        act = request-jsonrpc ).

    cl_abap_unit_assert=>assert_equals( exp = 'test.method'
                                        act = request-method ).

    cl_abap_unit_assert=>assert_equals( exp = '123'
                                        act = request-id ).
  ENDMETHOD.

  METHOD create_notification.
    " Test creating a notification (no ID)
    DATA(request) = cut->create_request( method = 'test.notification' ).

    " Assert
    cl_abap_unit_assert=>assert_equals( exp = '2.0'
                                        act = request-jsonrpc ).

    cl_abap_unit_assert=>assert_equals( exp = 'test.notification'
                                        act = request-method ).

    cl_abap_unit_assert=>assert_initial( request-id ).
  ENDMETHOD.

  METHOD serialize_request.
    " Arrange - create a request with params
    DATA(request) = cut->create_request( method = 'test.method'
                                         id     = '123' ).

    " Set params via ajson
    DATA(params) = zcl_mcp_ajson=>create_empty( ).
    params->set_string( iv_path = '/name'
                        iv_val  = 'value' ).
    request-params = params.

    " Act - serialize the request
    DATA(json) = cut->serialize_request( request ).

    " Assert - check JSON structure
    assert_json_equals( actual   = json
                        expected = '{"jsonrpc":"2.0","method":"test.method","params":{"name":"value"},"id":123}' ).
  ENDMETHOD.

  METHOD parse_request.
    " Arrange - JSON request string
    DATA(json) = `{"jsonrpc":"2.0","method":"test.method","params":{"name":"value"},"id":"123"}`.

    " Act - parse the JSON to a request object
    DATA(request) = cut->parse_request( json ).

    " Assert - check parsed values
    cl_abap_unit_assert=>assert_equals( exp = '2.0'
                                        act = request-jsonrpc ).

    cl_abap_unit_assert=>assert_equals( exp = 'test.method'
                                        act = request-method ).

    cl_abap_unit_assert=>assert_equals( exp = '123'
                                        act = request-id ).

    cl_abap_unit_assert=>assert_bound( request-params ).

    cl_abap_unit_assert=>assert_equals( exp = 'value'
                                        act = request-params->get_string( '/name' ) ).
  ENDMETHOD.

  METHOD parse_request_numeric_id.
    " Arrange - JSON request with numeric ID
    DATA(json) = `{"jsonrpc":"2.0","method":"test.method","id":123}`.

    " Act - parse the JSON to a request object
    DATA(request) = cut->parse_request( json ).

    " Assert - check ID is parsed correctly
    cl_abap_unit_assert=>assert_equals( exp = '123'
                                        act = request-id ).
  ENDMETHOD.

  METHOD create_success_response.
    " Arrange - create a result object
    DATA(result_json) = zcl_mcp_ajson=>create_empty( ).
    result_json->set_string( iv_path = '/status'
                             iv_val  = 'success' ).

    " Act - create a success response
    DATA(response) = cut->create_success_response( id     = '123'
                                                   result = result_json ).

    " Assert
    cl_abap_unit_assert=>assert_equals( exp = '2.0'
                                        act = response-jsonrpc ).

    cl_abap_unit_assert=>assert_equals( exp = '123'
                                        act = response-id ).

    cl_abap_unit_assert=>assert_bound( response-result ).

    cl_abap_unit_assert=>assert_equals( exp = 'success'
                                        act = response-result->get_string( '/status' ) ).
  ENDMETHOD.

  METHOD create_error_response.
    " Arrange - create error data
    DATA(error_data) = zcl_mcp_ajson=>create_empty( ).
    error_data->set_string( iv_path = '/details'
                            iv_val  = 'Error details' ).

    " Act - create an error response
    DATA(response) = cut->create_error_response( id      = '123'
                                                 code    = zcl_mcp_jsonrpc=>error_codes-method_not_found
                                                 message = 'Method not found'
                                                 data    = error_data ).

    " Assert
    cl_abap_unit_assert=>assert_equals( exp = '2.0'
                                        act = response-jsonrpc ).

    cl_abap_unit_assert=>assert_equals( exp = '123'
                                        act = response-id ).

    cl_abap_unit_assert=>assert_equals( exp = zcl_mcp_jsonrpc=>error_codes-method_not_found
                                        act = response-error-code ).

    cl_abap_unit_assert=>assert_equals( exp = 'Method not found'
                                        act = response-error-message ).

    cl_abap_unit_assert=>assert_bound( response-error-data ).

    cl_abap_unit_assert=>assert_equals( exp = 'Error details'
                                        act = response-error-data->get_string( '/details' ) ).
  ENDMETHOD.

  METHOD serialize_response.
    " Arrange - create a success response with result
    DATA(result_json) = zcl_mcp_ajson=>create_empty( ).
    result_json->set_string( iv_path = '/status'
                             iv_val  = 'success' ).

    DATA(response) = cut->create_success_response( id     = '123'
                                                   result = result_json ).

    " Act - serialize the response
    DATA(json) = cut->serialize_response( response ).

    " Assert - check JSON structure
    assert_json_equals( actual   = json
                        expected = '{"jsonrpc":"2.0","result":{"status":"success"},"id":123}' ).
  ENDMETHOD.

  METHOD parse_response.
    " Arrange - JSON response string
    DATA(json) = `{"jsonrpc":"2.0","result":{"status":"success"},"id":"123"}`.

    " Act - parse the JSON to a response object
    DATA(response) = cut->parse_response( json ).

    " Assert - check parsed values
    cl_abap_unit_assert=>assert_equals( exp = '2.0'
                                        act = response-jsonrpc ).

    cl_abap_unit_assert=>assert_equals( exp = '123'
                                        act = response-id ).

    cl_abap_unit_assert=>assert_bound( response-result ).

    cl_abap_unit_assert=>assert_equals( exp = 'success'
                                        act = response-result->get_string( '/status' ) ).
  ENDMETHOD.

  METHOD parse_batch_request.
    " Arrange - JSON batch request string
    DATA(json) = `[{"jsonrpc":"2.0","method":"method1","id":1},{"jsonrpc":"2.0","method":"method2","id":2}]`.

    " Act - parse the JSON to a batch of request objects
    DATA(requests) = cut->parse_batch_request( json ).

    " Assert - check parsed values
    cl_abap_unit_assert=>assert_equals( exp = 2
                                        act = lines( requests ) ).

    cl_abap_unit_assert=>assert_equals( exp = 'method1'
                                        act = requests[ 1 ]-method ).

    cl_abap_unit_assert=>assert_equals( exp = 'method2'
                                        act = requests[ 2 ]-method ).
  ENDMETHOD.

  METHOD serialize_batch_response.
    " Arrange - create multiple responses
    DATA(result1) = zcl_mcp_ajson=>create_empty( ).
    result1->set_string( iv_path = '/status'
                         iv_val  = 'success1' ).

    DATA(result2) = zcl_mcp_ajson=>create_empty( ).
    result2->set_string( iv_path = '/status'
                         iv_val  = 'success2' ).

    DATA(response1) = cut->create_success_response( id     = '1'
                                                    result = result1 ).

    DATA(response2) = cut->create_success_response( id     = '2'
                                                    result = result2 ).

    DATA(responses) = VALUE zcl_mcp_jsonrpc=>responses( ( response1 )
                                                        ( response2 ) ).

    " Act - serialize the batch response
    DATA(json) = cut->serialize_batch_response( responses ).

    " Assert - check JSON structure
    assert_json_equals(
        actual   = json
        expected = '[{"jsonrpc":"2.0","result":{"status":"success1"},"id":1},{"jsonrpc":"2.0","result":{"status":"success2"},"id":2}]' ).
  ENDMETHOD.

  METHOD assert_json_equals.
    " Parse both JSON strings to objects for comparison
    DATA(actual_obj) = zcl_mcp_ajson=>parse( actual ).
    DATA(expected_obj) = zcl_mcp_ajson=>parse( expected ).

    " Convert back to normalized string for comparison
    DATA(normalized_actual) = actual_obj->stringify( ).
    DATA(normalized_expected) = expected_obj->stringify( ).

    " Assert equality
    cl_abap_unit_assert=>assert_equals( exp = normalized_expected
                                        act = normalized_actual ).
  ENDMETHOD.
ENDCLASS.
