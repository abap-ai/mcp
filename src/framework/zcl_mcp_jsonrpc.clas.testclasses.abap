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
    METHODS create_request                 FOR TESTING.
    METHODS create_notification            FOR TESTING.
    METHODS create_success_response        FOR TESTING RAISING zcx_mcp_ajson_error.
    METHODS create_error_response          FOR TESTING RAISING zcx_mcp_ajson_error.

    " Test methods for JSON operations
    METHODS serialize_request              FOR TESTING RAISING zcx_mcp_ajson_error.
    METHODS parse_request                  FOR TESTING RAISING zcx_mcp_ajson_error.
    METHODS parse_request_numeric_id       FOR TESTING RAISING zcx_mcp_ajson_error.
    METHODS serialize_response             FOR TESTING RAISING zcx_mcp_ajson_error.
    METHODS parse_response                 FOR TESTING RAISING zcx_mcp_ajson_error.

    " Test methods for batch operations
    METHODS parse_batch_request            FOR TESTING RAISING zcx_mcp_ajson_error.
    METHODS serialize_batch_response       FOR TESTING RAISING zcx_mcp_ajson_error.

    " Test different ID formats
    METHODS parse_request_id_leading_zeros FOR TESTING RAISING zcx_mcp_ajson_error.
    METHODS parse_request_alphanumeric_id  FOR TESTING RAISING zcx_mcp_ajson_error.
    METHODS parse_request_uuid_id          FOR TESTING RAISING zcx_mcp_ajson_error.
    METHODS serialize_request_id_formats   FOR TESTING RAISING zcx_mcp_ajson_error.
    METHODS serialize_response_id_formats  FOR TESTING RAISING zcx_mcp_ajson_error.

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

  METHOD parse_request_id_leading_zeros.
    " Arrange - JSON request with string ID that has leading zeros
    DATA(json) = `{"jsonrpc":"2.0","method":"test.method","id":"0123"}`.

    " Act - parse the JSON to a request object
    DATA(request) = cut->parse_request( json ).

    " Assert - check ID is parsed correctly as a string, preserving leading zeros
    cl_abap_unit_assert=>assert_equals( exp = '0123'
                                        act = request-id ).

    " Serialize and check that it's treated as a string in JSON
    DATA(serialized) = cut->serialize_request( request ).
    assert_json_equals( actual   = serialized
                        expected = '{"jsonrpc":"2.0","method":"test.method","id":"0123"}' ).
  ENDMETHOD.

  METHOD parse_request_alphanumeric_id.
    " Arrange - JSON request with alphanumeric ID
    DATA(json) = `{"jsonrpc":"2.0","method":"test.method","id":"abc123"}`.

    " Act - parse the JSON to a request object
    DATA(request) = cut->parse_request( json ).

    " Assert - check ID is parsed correctly
    cl_abap_unit_assert=>assert_equals( exp = 'abc123'
                                        act = request-id ).

    " Serialize and check that it's treated as a string in JSON
    DATA(serialized) = cut->serialize_request( request ).
    assert_json_equals( actual   = serialized
                        expected = '{"jsonrpc":"2.0","method":"test.method","id":"abc123"}' ).
  ENDMETHOD.

  METHOD parse_request_uuid_id.
    " Arrange - JSON request with UUID-style ID
    DATA(json) = `{"jsonrpc":"2.0","method":"test.method","id":"550e8400-e29b-41d4-a716-446655440000"}`.

    " Act - parse the JSON to a request object
    DATA(request) = cut->parse_request( json ).

    " Assert - check ID is parsed correctly
    cl_abap_unit_assert=>assert_equals( exp = '550e8400-e29b-41d4-a716-446655440000'
                                        act = request-id ).

    " Serialize and check that it's treated as a string in JSON
    DATA(serialized) = cut->serialize_request( request ).
    assert_json_equals(
        actual   = serialized
        expected = '{"jsonrpc":"2.0","method":"test.method","id":"550e8400-e29b-41d4-a716-446655440000"}' ).
  ENDMETHOD.

  METHOD serialize_request_id_formats.
    " Test different ID formats in serialization

    " 1. Numeric ID
    DATA(request1) = cut->create_request( method = 'test.method'
                                          id     = '123' ).
    DATA(json1) = cut->serialize_request( request1 ).
    assert_json_equals( actual   = json1
                        expected = '{"jsonrpc":"2.0","method":"test.method","id":123}' ).

    " 2. ID with leading zeros
    DATA(request2) = cut->create_request( method = 'test.method'
                                          id     = '0123' ).
    DATA(json2) = cut->serialize_request( request2 ).
    assert_json_equals( actual   = json2
                        expected = '{"jsonrpc":"2.0","method":"test.method","id":"0123"}' ).

    " 3. ID with special formatting (plus sign)
    DATA(request3) = cut->create_request( method = 'test.method'
                                          id     = '+123' ).
    DATA(json3) = cut->serialize_request( request3 ).
    assert_json_equals( actual   = json3
                        expected = '{"jsonrpc":"2.0","method":"test.method","id":"+123"}' ).

    " 4. Alphanumeric ID
    DATA(request4) = cut->create_request( method = 'test.method'
                                          id     = 'abc123' ).
    DATA(json4) = cut->serialize_request( request4 ).
    assert_json_equals( actual   = json4
                        expected = '{"jsonrpc":"2.0","method":"test.method","id":"abc123"}' ).
  ENDMETHOD.

  METHOD serialize_response_id_formats.
    " Setup a simple result for all responses
    DATA(result_json) = zcl_mcp_ajson=>create_empty( ).
    result_json->set_string( iv_path = '/status'
                             iv_val  = 'success' ).

    " 1. Numeric ID
    DATA(response1) = cut->create_success_response( id     = '123'
                                                    result = result_json ).
    DATA(json1) = cut->serialize_response( response1 ).
    assert_json_equals( actual   = json1
                        expected = '{"jsonrpc":"2.0","result":{"status":"success"},"id":123}' ).

    " 2. ID with leading zeros
    DATA(response2) = cut->create_success_response( id     = '0123'
                                                    result = result_json ).
    DATA(json2) = cut->serialize_response( response2 ).
    assert_json_equals( actual   = json2
                        expected = '{"jsonrpc":"2.0","result":{"status":"success"},"id":"0123"}' ).

    " 3. Alphanumeric ID
    DATA(response3) = cut->create_success_response( id     = 'abc123'
                                                    result = result_json ).
    DATA(json3) = cut->serialize_response( response3 ).
    assert_json_equals( actual   = json3
                        expected = '{"jsonrpc":"2.0","result":{"status":"success"},"id":"abc123"}' ).
  ENDMETHOD.
ENDCLASS.
