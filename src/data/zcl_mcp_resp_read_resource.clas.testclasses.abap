CLASS ltcl_read_resource_result DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA cut  TYPE REF TO zcl_mcp_resp_read_resource.
    DATA json TYPE REF TO zif_mcp_ajson.

    METHODS setup.
    METHODS teardown.
    METHODS test_single_text_resource FOR TESTING RAISING cx_static_check.
    METHODS test_single_blob_resource FOR TESTING RAISING cx_static_check.
    METHODS test_multiple_resources   FOR TESTING RAISING cx_static_check.
    METHODS test_with_metadata        FOR TESTING RAISING cx_static_check.
    METHODS test_set_contents         FOR TESTING RAISING cx_static_check.
ENDCLASS.

CLASS ltcl_read_resource_result IMPLEMENTATION.
  METHOD setup.
    cut = NEW zcl_mcp_resp_read_resource( ).
    json = zcl_mcp_ajson=>create_empty( ).
  ENDMETHOD.

  METHOD teardown.
    CLEAR: cut, json.
  ENDMETHOD.

  METHOD test_single_text_resource.
    " Given
    cut->add_text_resource( uri       = 'https://example.com/document.txt'
                            text      = 'This is a sample text document'
                            mime_type = 'text/plain' ).

    " When
    json = cut->zif_mcp_internal~generate_json( ).
    DATA(json_string) = json->stringify( ).

    " Then
    DATA(result) = zcl_mcp_ajson=>parse( json_string ).

    cl_abap_unit_assert=>assert_equals( exp = 'https://example.com/document.txt'
                                        act = result->get( '/contents/1/uri' ) ).

    cl_abap_unit_assert=>assert_equals( exp = 'This is a sample text document'
                                        act = result->get( '/contents/1/text' ) ).

    cl_abap_unit_assert=>assert_equals( exp = 'text/plain'
                                        act = result->get( '/contents/1/mimeType' ) ).
  ENDMETHOD.

  METHOD test_single_blob_resource.
    " Given
    cut->add_blob_resource( uri       = 'https://example.com/image.jpg'
                            blob      = 'SGVsbG8gV29ybGQ=' " Base64 for "Hello World"
                            mime_type = 'image/jpeg' ).

    " When
    json = cut->zif_mcp_internal~generate_json( ).
    DATA(lv_json_string) = json->stringify( ).

    " Then
    DATA(lo_result) = zcl_mcp_ajson=>parse( lv_json_string ).

    cl_abap_unit_assert=>assert_equals( exp = 'https://example.com/image.jpg'
                                        act = lo_result->get( '/contents/1/uri' ) ).

    cl_abap_unit_assert=>assert_equals( exp = 'SGVsbG8gV29ybGQ='
                                        act = lo_result->get( '/contents/1/blob' ) ).

    cl_abap_unit_assert=>assert_equals( exp = 'image/jpeg'
                                        act = lo_result->get( '/contents/1/mimeType' ) ).
  ENDMETHOD.

  METHOD test_multiple_resources.
    " Given
    cut->add_text_resource( uri       = 'https://example.com/document1.txt'
                            text      = 'First document'
                            mime_type = 'text/plain' ).

    cut->add_blob_resource( uri       = 'https://example.com/image.jpg'
                            blob      = 'SGVsbG8gV29ybGQ='
                            mime_type = 'image/jpeg' ).

    cut->add_text_resource( uri       = 'https://example.com/document2.html'
                            text      = '<html><body>Hello</body></html>'
                            mime_type = 'text/html' ).

    " When
    json = cut->zif_mcp_internal~generate_json( ).
    DATA(json_string) = json->stringify( ).

    " Then
    DATA(result) = zcl_mcp_ajson=>parse( json_string ).

    " First resource (text)
    cl_abap_unit_assert=>assert_equals( exp = 'https://example.com/document1.txt'
                                        act = result->get( '/contents/1/uri' ) ).
    cl_abap_unit_assert=>assert_equals( exp = 'First document'
                                        act = result->get( '/contents/1/text' ) ).
    cl_abap_unit_assert=>assert_equals( exp = 'text/plain'
                                        act = result->get( '/contents/1/mimeType' ) ).

    " Second resource (blob)
    cl_abap_unit_assert=>assert_equals( exp = 'https://example.com/image.jpg'
                                        act = result->get( '/contents/2/uri' ) ).
    cl_abap_unit_assert=>assert_equals( exp = 'SGVsbG8gV29ybGQ='
                                        act = result->get( '/contents/2/blob' ) ).
    cl_abap_unit_assert=>assert_equals( exp = 'image/jpeg'
                                        act = result->get( '/contents/2/mimeType' ) ).

    " Third resource (text)
    cl_abap_unit_assert=>assert_equals( exp = 'https://example.com/document2.html'
                                        act = result->get( '/contents/3/uri' ) ).
    cl_abap_unit_assert=>assert_equals( exp = '<html><body>Hello</body></html>'
                                        act = result->get( '/contents/3/text' ) ).
    cl_abap_unit_assert=>assert_equals( exp = 'text/html'
                                        act = result->get( '/contents/3/mimeType' ) ).
  ENDMETHOD.

  METHOD test_with_metadata.
    " Given
    cut->add_text_resource( uri       = 'https://example.com/document.txt'
                            text      = 'Sample document'
                            mime_type = 'text/plain' ).

    " Create metadata
    DATA(meta) = zcl_mcp_ajson=>create_empty( ).
    meta->set( iv_path = '/version'
               iv_val  = '1.0' ).
    meta->set( iv_path = '/timestamp'
               iv_val  = '2023-05-15T12:30:45Z' ).
    cut->set_meta( meta ).

    " When
    json = cut->zif_mcp_internal~generate_json( ).
    DATA(json_string) = json->stringify( ).

    " Then
    DATA(result) = zcl_mcp_ajson=>parse( json_string ).

    " Check resource content
    cl_abap_unit_assert=>assert_equals( exp = 'https://example.com/document.txt'
                                        act = result->get( '/contents/1/uri' ) ).

    " Check metadata
    cl_abap_unit_assert=>assert_equals( exp = '1.0'
                                        act = result->get( '/_meta/version' ) ).
    cl_abap_unit_assert=>assert_equals( exp = '2023-05-15T12:30:45Z'
                                        act = result->get( '/_meta/timestamp' ) ).
  ENDMETHOD.

  METHOD test_set_contents.
    " Given
    DATA(contents) = VALUE zcl_mcp_resp_read_resource=>resource_contents( ).

    " Create text resource
    DATA(text_content) = VALUE zcl_mcp_resp_read_resource=>resource_content_wrapper( type = 'text' ).
    DATA(text_resource) = NEW zcl_mcp_resp_read_resource=>text_resource_contents(
                                  uri      = 'https://example.com/text.txt'
                                  mime_type = 'text/plain'
                                  text     = 'Text content' ).
    text_content-resource = text_resource.
    APPEND text_content TO contents.

    " Create blob resource
    DATA(blob_content) = VALUE zcl_mcp_resp_read_resource=>resource_content_wrapper( type = 'blob' ).
    DATA(blob_resource) = NEW zcl_mcp_resp_read_resource=>blob_resource_contents(
                                  uri      = 'https://example.com/image.png'
                                  mime_type = 'image/png'
                                  blob     = 'R0lGODlhAQABAIAAAP///wAAACH5BAEAAAAALAAAAAABAAEAAAICRAEAOw==' ).
    blob_content-resource = blob_resource.
    APPEND blob_content TO contents.

    cut->set_contents( contents ).

    " When
    json = cut->zif_mcp_internal~generate_json( ).
    DATA(json_string) = json->stringify( ).

    " Then
    DATA(result) = zcl_mcp_ajson=>parse( json_string ).

    " Text resource
    cl_abap_unit_assert=>assert_equals( exp = 'https://example.com/text.txt'
                                        act = result->get( '/contents/1/uri' ) ).
    cl_abap_unit_assert=>assert_equals( exp = 'Text content'
                                        act = result->get( '/contents/1/text' ) ).

    " Blob resource
    cl_abap_unit_assert=>assert_equals( exp = 'https://example.com/image.png'
                                        act = result->get( '/contents/2/uri' ) ).
    cl_abap_unit_assert=>assert_equals( exp = 'R0lGODlhAQABAIAAAP///wAAACH5BAEAAAAALAAAAAABAAEAAAICRAEAOw=='
                                        act = result->get( '/contents/2/blob' ) ).
  ENDMETHOD.
ENDCLASS.
