CLASS ltcl_read_resource_result DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA cut  TYPE REF TO zcl_mcp_resp_read_resource.
    DATA json TYPE REF TO zif_mcp_ajson.

    METHODS setup.
    METHODS teardown.
    METHODS test_single_text_resource      FOR TESTING RAISING cx_static_check.
    METHODS test_single_blob_resource      FOR TESTING RAISING cx_static_check.
    METHODS test_multiple_resources        FOR TESTING RAISING cx_static_check.
    METHODS test_with_metadata             FOR TESTING RAISING cx_static_check.
    METHODS test_set_contents              FOR TESTING RAISING cx_static_check.
    METHODS test_text_resource_with_meta   FOR TESTING RAISING cx_static_check.
    METHODS test_blob_resource_with_meta   FOR TESTING RAISING cx_static_check.
    METHODS test_mixed_resources_with_meta FOR TESTING RAISING cx_static_check.
    METHODS test_response_and_resource_met FOR TESTING RAISING cx_static_check.
    METHODS test_set_contents_with_meta    FOR TESTING RAISING cx_static_check.
    METHODS test_resources_without_mime    FOR TESTING RAISING cx_static_check.
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

    " Ensure no metadata is present
    cl_abap_unit_assert=>assert_false( result->exists( '/contents/1/_meta' ) ).
    cl_abap_unit_assert=>assert_false( result->exists( '/_meta' ) ).
  ENDMETHOD.

  METHOD test_single_blob_resource.
    " Given
    cut->add_blob_resource( uri       = 'https://example.com/image.jpg'
                            blob      = 'SGVsbG8gV29ybGQ=' " Base64 for "Hello World"
                            mime_type = 'image/jpeg' ).

    " When
    json = cut->zif_mcp_internal~generate_json( ).
    DATA(json_string) = json->stringify( ).

    " Then
    DATA(result) = zcl_mcp_ajson=>parse( json_string ).

    cl_abap_unit_assert=>assert_equals( exp = 'https://example.com/image.jpg'
                                        act = result->get( '/contents/1/uri' ) ).

    cl_abap_unit_assert=>assert_equals( exp = 'SGVsbG8gV29ybGQ='
                                        act = result->get( '/contents/1/blob' ) ).

    cl_abap_unit_assert=>assert_equals( exp = 'image/jpeg'
                                        act = result->get( '/contents/1/mimeType' ) ).

    " Ensure no metadata is present
    cl_abap_unit_assert=>assert_false( result->exists( '/contents/1/_meta' ) ).
    cl_abap_unit_assert=>assert_false( result->exists( '/_meta' ) ).
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

    " Create response-level metadata
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

    " Check response-level metadata
    cl_abap_unit_assert=>assert_equals( exp = '1.0'
                                        act = result->get( '/_meta/version' ) ).
    cl_abap_unit_assert=>assert_equals( exp = '2023-05-15T12:30:45Z'
                                        act = result->get( '/_meta/timestamp' ) ).
  ENDMETHOD.

  METHOD test_text_resource_with_meta.
    " Given
    DATA(resource_meta) = zcl_mcp_ajson=>create_empty( ).
    resource_meta->set( iv_path = '/author'
                        iv_val  = 'John Doe' ).
    resource_meta->set( iv_path = '/lastModified'
                        iv_val  = '2023-12-01T10:00:00Z' ).

    cut->add_text_resource( uri       = 'https://example.com/document.txt'
                            text      = 'Document with metadata'
                            mime_type = 'text/plain'
                            meta      = resource_meta ).

    " When
    json = cut->zif_mcp_internal~generate_json( ).
    DATA(json_string) = json->stringify( ).

    " Then
    DATA(result) = zcl_mcp_ajson=>parse( json_string ).

    " Check resource content
    cl_abap_unit_assert=>assert_equals( exp = 'https://example.com/document.txt'
                                        act = result->get( '/contents/1/uri' ) ).
    cl_abap_unit_assert=>assert_equals( exp = 'Document with metadata'
                                        act = result->get( '/contents/1/text' ) ).

    " Check resource-level metadata
    cl_abap_unit_assert=>assert_equals( exp = 'John Doe'
                                        act = result->get( '/contents/1/_meta/author' ) ).
    cl_abap_unit_assert=>assert_equals( exp = '2023-12-01T10:00:00Z'
                                        act = result->get( '/contents/1/_meta/lastModified' ) ).
  ENDMETHOD.

  METHOD test_blob_resource_with_meta.
    " Given
    DATA(resource_meta) = zcl_mcp_ajson=>create_empty( ).
    resource_meta->set( iv_path = '/width'
                        iv_val  = 1920 ).
    resource_meta->set( iv_path = '/height'
                        iv_val  = 1080 ).
    resource_meta->set( iv_path = '/format'
                        iv_val  = 'JPEG' ).

    cut->add_blob_resource( uri       = 'https://example.com/photo.jpg'
                            blob      = 'iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAYAAAAfFcSJAAAADUlEQVR42mP8/5+hHgAHggJ/PchI7wAAAABJRU5ErkJggg=='
                            mime_type = 'image/jpeg'
                            meta      = resource_meta ).

    " When
    json = cut->zif_mcp_internal~generate_json( ).
    DATA(json_string) = json->stringify( ).

    " Then
    DATA(result) = zcl_mcp_ajson=>parse( json_string ).

    " Check resource content
    cl_abap_unit_assert=>assert_equals( exp = 'https://example.com/photo.jpg'
                                        act = result->get( '/contents/1/uri' ) ).
    cl_abap_unit_assert=>assert_equals( exp = 'image/jpeg'
                                        act = result->get( '/contents/1/mimeType' ) ).

    " Check resource-level metadata
    cl_abap_unit_assert=>assert_equals( exp = 1920
                                        act = result->get( '/contents/1/_meta/width' ) ).
    cl_abap_unit_assert=>assert_equals( exp = 1080
                                        act = result->get( '/contents/1/_meta/height' ) ).
    cl_abap_unit_assert=>assert_equals( exp = 'JPEG'
                                        act = result->get( '/contents/1/_meta/format' ) ).
  ENDMETHOD.

  METHOD test_mixed_resources_with_meta.
    " Given - mix of resources with and without metadata
    DATA(text_meta) = zcl_mcp_ajson=>create_empty( ).
    text_meta->set( iv_path = '/category'
                    iv_val  = 'documentation' ).

    cut->add_text_resource( uri       = 'https://example.com/doc1.txt'
                            text      = 'First document'
                            mime_type = 'text/plain'
                            meta      = text_meta ).

    " Resource without metadata
    cut->add_blob_resource( uri       = 'https://example.com/image.png'
                            blob      = 'iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAYAAAAfFcSJAAAADUlEQVR42mNk+M9QDwADhgGAWjR9awAAAABJRU5ErkJggg=='
                            mime_type = 'image/png' ).

    DATA(blob_meta) = zcl_mcp_ajson=>create_empty( ).
    blob_meta->set( iv_path = '/size'
                    iv_val  = 12345 ).

    cut->add_blob_resource( uri       = 'https://example.com/photo.jpg'
                            blob      = 'SGVsbG8gV29ybGQ='
                            mime_type = 'image/jpeg'
                            meta      = blob_meta ).

    " When
    json = cut->zif_mcp_internal~generate_json( ).
    DATA(json_string) = json->stringify( ).

    " Then
    DATA(result) = zcl_mcp_ajson=>parse( json_string ).

    " First resource - has metadata
    cl_abap_unit_assert=>assert_true( result->exists( '/contents/1/_meta' ) ).
    cl_abap_unit_assert=>assert_equals( exp = 'documentation'
                                        act = result->get( '/contents/1/_meta/category' ) ).

    " Second resource - no metadata
    cl_abap_unit_assert=>assert_false( result->exists( '/contents/2/_meta' ) ).

    " Third resource - has metadata
    cl_abap_unit_assert=>assert_true( result->exists( '/contents/3/_meta' ) ).
    cl_abap_unit_assert=>assert_equals( exp = 12345
                                        act = result->get( '/contents/3/_meta/size' ) ).
  ENDMETHOD.

  METHOD test_response_and_resource_met.
    " Given - both response-level and resource-level metadata
    DATA(resource_meta) = zcl_mcp_ajson=>create_empty( ).
    resource_meta->set( iv_path = '/resourceVersion'
                        iv_val  = '2.1' ).

    cut->add_text_resource( uri       = 'https://example.com/document.txt'
                            text      = 'Document content'
                            mime_type = 'text/plain'
                            meta      = resource_meta ).

    DATA(response_meta) = zcl_mcp_ajson=>create_empty( ).
    response_meta->set( iv_path = '/responseVersion'
                        iv_val  = '1.0' ).
    response_meta->set( iv_path = '/server'
                        iv_val  = 'mcp-server' ).

    cut->set_meta( response_meta ).

    " When
    json = cut->zif_mcp_internal~generate_json( ).
    DATA(json_string) = json->stringify( ).

    " Then
    DATA(result) = zcl_mcp_ajson=>parse( json_string ).

    " Check resource-level metadata
    cl_abap_unit_assert=>assert_equals( exp = '2.1'
                                        act = result->get( '/contents/1/_meta/resourceVersion' ) ).

    " Check response-level metadata
    cl_abap_unit_assert=>assert_equals( exp = '1.0'
                                        act = result->get( '/_meta/responseVersion' ) ).
    cl_abap_unit_assert=>assert_equals( exp = 'mcp-server'
                                        act = result->get( '/_meta/server' ) ).
  ENDMETHOD.

  METHOD test_set_contents.
    " Given
    DATA(contents) = VALUE zcl_mcp_resp_read_resource=>resource_contents( ).

    " Create text resource without metadata
    DATA(text_content) = VALUE zcl_mcp_resp_read_resource=>resource_content_wrapper( type = 'text' ).
    DATA(text_resource) = NEW zcl_mcp_resp_read_resource=>text_resource_contents(
                                  uri       = 'https://example.com/text.txt'
                                  mime_type = 'text/plain'
                                  text      = 'Text content' ).
    text_content-resource = text_resource.
    APPEND text_content TO contents.

    " Create blob resource without metadata
    DATA(blob_content) = VALUE zcl_mcp_resp_read_resource=>resource_content_wrapper( type = 'blob' ).
    DATA(blob_resource) = NEW zcl_mcp_resp_read_resource=>blob_resource_contents(
                                  uri       = 'https://example.com/image.png'
                                  mime_type = 'image/png'
                                  blob      = 'R0lGODlhAQABAIAAAP///wAAACH5BAEAAAAALAAAAAABAAEAAAICRAEAOw==' ).
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

  METHOD test_set_contents_with_meta.
    " Given
    DATA(contents) = VALUE zcl_mcp_resp_read_resource=>resource_contents( ).

    " Create text resource with metadata
    DATA(text_meta) = zcl_mcp_ajson=>create_empty( ).
    text_meta->set( iv_path = '/encoding'
                    iv_val  = 'UTF-8' ).

    DATA(text_content) = VALUE zcl_mcp_resp_read_resource=>resource_content_wrapper( type = 'text' ).
    DATA(text_resource) = NEW zcl_mcp_resp_read_resource=>text_resource_contents(
                                  uri       = 'https://example.com/text.txt'
                                  mime_type = 'text/plain'
                                  text      = 'Text with metadata'
                                  meta      = text_meta ).
    text_content-resource = text_resource.
    APPEND text_content TO contents.

    " Create blob resource with metadata
    DATA(blob_meta) = zcl_mcp_ajson=>create_empty( ).
    blob_meta->set( iv_path = '/compression'
                    iv_val  = 'gzip' ).

    DATA(blob_content) = VALUE zcl_mcp_resp_read_resource=>resource_content_wrapper( type = 'blob' ).
    DATA(blob_resource) = NEW zcl_mcp_resp_read_resource=>blob_resource_contents(
                                  uri       = 'https://example.com/data.bin'
                                  mime_type = 'application/octet-stream'
                                  blob      = 'H4sIAAAAAAAAA8tIzcnJVyjPL8pJUQQA2B4LJAAAAAA='
                                  meta      = blob_meta ).
    blob_content-resource = blob_resource.
    APPEND blob_content TO contents.

    cut->set_contents( contents ).

    " When
    json = cut->zif_mcp_internal~generate_json( ).
    DATA(json_string) = json->stringify( ).

    " Then
    DATA(result) = zcl_mcp_ajson=>parse( json_string ).

    " Text resource with metadata
    cl_abap_unit_assert=>assert_equals( exp = 'https://example.com/text.txt'
                                        act = result->get( '/contents/1/uri' ) ).
    cl_abap_unit_assert=>assert_equals( exp = 'Text with metadata'
                                        act = result->get( '/contents/1/text' ) ).
    cl_abap_unit_assert=>assert_equals( exp = 'UTF-8'
                                        act = result->get( '/contents/1/_meta/encoding' ) ).

    " Blob resource with metadata
    cl_abap_unit_assert=>assert_equals( exp = 'https://example.com/data.bin'
                                        act = result->get( '/contents/2/uri' ) ).
    cl_abap_unit_assert=>assert_equals( exp = 'gzip'
                                        act = result->get( '/contents/2/_meta/compression' ) ).
  ENDMETHOD.

  METHOD test_resources_without_mime.
    " Given - resources without MIME type
    cut->add_text_resource( uri  = 'https://example.com/document.txt'
                            text = 'Document without MIME type' ).

    cut->add_blob_resource( uri  = 'https://example.com/file.bin'
                            blob = 'SGVsbG8gV29ybGQ=' ).

    " When
    json = cut->zif_mcp_internal~generate_json( ).
    DATA(json_string) = json->stringify( ).

    " Then
    DATA(result) = zcl_mcp_ajson=>parse( json_string ).

    " Text resource without MIME type
    cl_abap_unit_assert=>assert_equals( exp = 'https://example.com/document.txt'
                                        act = result->get( '/contents/1/uri' ) ).
    cl_abap_unit_assert=>assert_equals( exp = 'Document without MIME type'
                                        act = result->get( '/contents/1/text' ) ).
    cl_abap_unit_assert=>assert_false( result->exists( '/contents/1/mimeType' ) ).

    " Blob resource without MIME type
    cl_abap_unit_assert=>assert_equals( exp = 'https://example.com/file.bin'
                                        act = result->get( '/contents/2/uri' ) ).
    cl_abap_unit_assert=>assert_equals( exp = 'SGVsbG8gV29ybGQ='
                                        act = result->get( '/contents/2/blob' ) ).
    cl_abap_unit_assert=>assert_false( result->exists( '/contents/2/mimeType' ) ).
  ENDMETHOD.
ENDCLASS.
