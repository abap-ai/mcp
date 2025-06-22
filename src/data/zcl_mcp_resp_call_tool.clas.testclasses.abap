CLASS ltcl_call_tool_result DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA cut  TYPE REF TO zcl_mcp_resp_call_tool.
    DATA json TYPE REF TO zif_mcp_ajson.

    METHODS setup.
    METHODS teardown.
    METHODS test_text_content         FOR TESTING RAISING cx_static_check.
    METHODS test_image_content        FOR TESTING RAISING cx_static_check.
    METHODS test_audio_content        FOR TESTING RAISING cx_static_check.
    METHODS test_resource_link        FOR TESTING RAISING cx_static_check.
    METHODS test_text_resource        FOR TESTING RAISING cx_static_check.
    METHODS test_blob_resource        FOR TESTING RAISING cx_static_check.
    METHODS test_structured_content   FOR TESTING RAISING cx_static_check.
    METHODS test_structured_no_text   FOR TESTING RAISING cx_static_check.
    METHODS test_multiple_contents    FOR TESTING RAISING cx_static_check.
    METHODS test_with_error_flag      FOR TESTING RAISING cx_static_check.
    METHODS test_with_metadata        FOR TESTING RAISING cx_static_check.
    METHODS test_content_meta         FOR TESTING RAISING cx_static_check.
    METHODS test_set_content          FOR TESTING RAISING cx_static_check.
    METHODS test_annotations          FOR TESTING RAISING cx_static_check.
    METHODS test_timestamp_annotations FOR TESTING RAISING cx_static_check.
ENDCLASS.

CLASS ltcl_call_tool_result IMPLEMENTATION.
  METHOD setup.
    cut = NEW zcl_mcp_resp_call_tool( ).
    json = zcl_mcp_ajson=>create_empty( ).
  ENDMETHOD.

  METHOD teardown.
    CLEAR: cut, json.
  ENDMETHOD.

  METHOD test_text_content.
    " Given
    cut->add_text_content( 'This is a sample text content' ).

    " When
    json = cut->zif_mcp_internal~generate_json( ).
    DATA json_string TYPE string.
    json_string = json->stringify( ).

    " Then
    DATA result TYPE REF TO zif_mcp_ajson.
    result = zcl_mcp_ajson=>parse( json_string ).

    cl_abap_unit_assert=>assert_equals( exp = 'text'
                                        act = result->get( '/content/1/type' ) ).

    cl_abap_unit_assert=>assert_equals( exp = 'This is a sample text content'
                                        act = result->get( '/content/1/text' ) ).
  ENDMETHOD.

  METHOD test_image_content.
    " Given
    cut->add_image_content( data      = 'SGVsbG8gV29ybGQ='
                            mime_type = 'image/jpeg' ).

    " When
    json = cut->zif_mcp_internal~generate_json( ).
    DATA json_string TYPE string.
    json_string = json->stringify( ).

    " Then
    DATA result TYPE REF TO zif_mcp_ajson.
    result = zcl_mcp_ajson=>parse( json_string ).

    cl_abap_unit_assert=>assert_equals( exp = 'image'
                                        act = result->get( '/content/1/type' ) ).

    cl_abap_unit_assert=>assert_equals( exp = 'SGVsbG8gV29ybGQ='
                                        act = result->get( '/content/1/data' ) ).

    cl_abap_unit_assert=>assert_equals( exp = 'image/jpeg'
                                        act = result->get( '/content/1/mimeType' ) ).
  ENDMETHOD.

  METHOD test_audio_content.
    " Given
    cut->add_audio_content( data      = 'VGVzdCBhdWRpbw=='
                            mime_type = 'audio/wav' ).

    " When
    json = cut->zif_mcp_internal~generate_json( ).
    DATA json_string TYPE string.
    json_string = json->stringify( ).

    " Then
    DATA result TYPE REF TO zif_mcp_ajson.
    result = zcl_mcp_ajson=>parse( json_string ).

    cl_abap_unit_assert=>assert_equals( exp = 'audio'
                                        act = result->get( '/content/1/type' ) ).

    cl_abap_unit_assert=>assert_equals( exp = 'VGVzdCBhdWRpbw=='
                                        act = result->get( '/content/1/data' ) ).

    cl_abap_unit_assert=>assert_equals( exp = 'audio/wav'
                                        act = result->get( '/content/1/mimeType' ) ).
  ENDMETHOD.

  METHOD test_resource_link.
    " Given
    cut->add_resource_link( uri         = 'https://example.com/report.pdf'
                            name        = 'quarterly_report'
                            title       = 'Q1 2024 Financial Report'
                            description = 'Financial report for first quarter'
                            mime_type   = 'application/pdf'
                            size        = 1024 ).

    " When
    json = cut->zif_mcp_internal~generate_json( ).
    DATA json_string TYPE string.
    json_string = json->stringify( ).

    " Then
    DATA result TYPE REF TO zif_mcp_ajson.
    result = zcl_mcp_ajson=>parse( json_string ).

    cl_abap_unit_assert=>assert_equals( exp = 'resource_link'
                                        act = result->get( '/content/1/type' ) ).

    cl_abap_unit_assert=>assert_equals( exp = 'https://example.com/report.pdf'
                                        act = result->get( '/content/1/uri' ) ).

    cl_abap_unit_assert=>assert_equals( exp = 'quarterly_report'
                                        act = result->get( '/content/1/name' ) ).

    cl_abap_unit_assert=>assert_equals( exp = 'Q1 2024 Financial Report'
                                        act = result->get( '/content/1/title' ) ).

    cl_abap_unit_assert=>assert_equals( exp = 'Financial report for first quarter'
                                        act = result->get( '/content/1/description' ) ).

    cl_abap_unit_assert=>assert_equals( exp = 'application/pdf'
                                        act = result->get( '/content/1/mimeType' ) ).

    cl_abap_unit_assert=>assert_equals( exp = '1024'
                                        act = result->get( '/content/1/size' ) ).
  ENDMETHOD.

  METHOD test_text_resource.
    " Given
    cut->add_text_resource( uri       = 'https://example.com/document.txt'
                            text      = 'This is a sample text document'
                            mime_type = 'text/plain' ).

    " When
    json = cut->zif_mcp_internal~generate_json( ).
    DATA json_string TYPE string.
    json_string = json->stringify( ).

    " Then
    DATA result TYPE REF TO zif_mcp_ajson.
    result = zcl_mcp_ajson=>parse( json_string ).

    cl_abap_unit_assert=>assert_equals( exp = 'resource'
                                        act = result->get( '/content/1/type' ) ).

    cl_abap_unit_assert=>assert_equals( exp = 'https://example.com/document.txt'
                                        act = result->get( '/content/1/resource/uri' ) ).

    cl_abap_unit_assert=>assert_equals( exp = 'This is a sample text document'
                                        act = result->get( '/content/1/resource/text' ) ).

    cl_abap_unit_assert=>assert_equals( exp = 'text/plain'
                                        act = result->get( '/content/1/resource/mimeType' ) ).
  ENDMETHOD.

  METHOD test_blob_resource.
    " Given
    cut->add_blob_resource( uri       = 'https://example.com/image.jpg'
                            blob      = 'SGVsbG8gV29ybGQ=' " Base64 for "Hello World"
                            mime_type = 'image/jpeg' ).

    " When
    json = cut->zif_mcp_internal~generate_json( ).
    DATA json_string TYPE string.
    json_string = json->stringify( ).

    " Then
    DATA result TYPE REF TO zif_mcp_ajson.
    result = zcl_mcp_ajson=>parse( json_string ).

    cl_abap_unit_assert=>assert_equals( exp = 'resource'
                                        act = result->get( '/content/1/type' ) ).

    cl_abap_unit_assert=>assert_equals( exp = 'https://example.com/image.jpg'
                                        act = result->get( '/content/1/resource/uri' ) ).

    cl_abap_unit_assert=>assert_equals( exp = 'SGVsbG8gV29ybGQ='
                                        act = result->get( '/content/1/resource/blob' ) ).

    cl_abap_unit_assert=>assert_equals( exp = 'image/jpeg'
                                        act = result->get( '/content/1/resource/mimeType' ) ).
  ENDMETHOD.

  METHOD test_structured_content.
    " Given - structured content with auto text
    DATA structured TYPE REF TO zif_mcp_ajson.
    structured = zcl_mcp_ajson=>create_empty( ).
    structured->set( iv_path = '/status' iv_val = 'success' ).
    structured->set( iv_path = '/data/count' iv_val = 42 ).
    structured->set( iv_path = '/data/items/1' iv_val = 'item1' ).

    cut->set_structured_content( structured_content = structured
                                 add_text_content   = abap_true ).

    " When
    json = cut->zif_mcp_internal~generate_json( ).
    DATA json_string TYPE string.
    json_string = json->stringify( ).

    " Then
    DATA result TYPE REF TO zif_mcp_ajson.
    result = zcl_mcp_ajson=>parse( json_string ).

    " Check structured content
    cl_abap_unit_assert=>assert_equals( exp = 'success'
                                        act = result->get( '/structuredContent/status' ) ).

    cl_abap_unit_assert=>assert_equals( exp = '42'
                                        act = result->get( '/structuredContent/data/count' ) ).

    " Check auto-added text content
    cl_abap_unit_assert=>assert_equals( exp = 'text'
                                        act = result->get( '/content/1/type' ) ).

    " Text should contain the JSON string representation
    DATA text_content TYPE string.
    text_content = result->get( '/content/1/text' ).
    cl_abap_unit_assert=>assert_not_initial( text_content ).
  ENDMETHOD.

  METHOD test_structured_no_text.
    " Given - structured content without auto text
    DATA structured TYPE REF TO zif_mcp_ajson.
    structured = zcl_mcp_ajson=>create_empty( ).
    structured->set( iv_path = '/result' iv_val = 'processed' ).

    cut->set_structured_content( structured_content = structured
                                 add_text_content   = abap_false ).

    " When
    json = cut->zif_mcp_internal~generate_json( ).
    DATA json_string TYPE string.
    json_string = json->stringify( ).

    " Then
    DATA result TYPE REF TO zif_mcp_ajson.
    result = zcl_mcp_ajson=>parse( json_string ).

    " Check structured content exists
    cl_abap_unit_assert=>assert_equals( exp = 'processed'
                                        act = result->get( '/structuredContent/result' ) ).

    " Check no content array exists (empty)
    TRY.
        DATA content_type TYPE string.
        content_type = result->get( '/content/1/type' ).
        cl_abap_unit_assert=>assert_initial( content_type ).
      CATCH zcx_mcp_ajson_error.
        " Expected - no content should exist
    ENDTRY ##NO_HANDLER.
  ENDMETHOD.

  METHOD test_content_meta.
    " Given
    DATA meta TYPE REF TO zif_mcp_ajson.
    meta = zcl_mcp_ajson=>create_empty( ).
    meta->set( iv_path = '/source' iv_val = 'manual_input' ).
    meta->set( iv_path = '/confidence' iv_val = '0.95' ).

    cut->add_text_content( text = 'Text with metadata'
                           meta = meta ).

    " When
    json = cut->zif_mcp_internal~generate_json( ).
    DATA json_string TYPE string.
    json_string = json->stringify( ).

    " Then
    DATA result TYPE REF TO zif_mcp_ajson.
    result = zcl_mcp_ajson=>parse( json_string ).

    cl_abap_unit_assert=>assert_equals( exp = 'Text with metadata'
                                        act = result->get( '/content/1/text' ) ).

    cl_abap_unit_assert=>assert_equals( exp = 'manual_input'
                                        act = result->get( '/content/1/_meta/source' ) ).

    cl_abap_unit_assert=>assert_equals( exp = '0.95'
                                        act = result->get( '/content/1/_meta/confidence' ) ).
  ENDMETHOD.

  METHOD test_multiple_contents.
    " Given
    cut->add_text_content( 'This is a text content' ).

    cut->add_image_content( data      = 'SGVsbG8gV29ybGQ='
                            mime_type = 'image/jpeg' ).

    cut->add_audio_content( data      = 'VGVzdCBhdWRpbw=='
                            mime_type = 'audio/wav' ).

    cut->add_resource_link( uri  = 'https://example.com/doc.pdf'
                            name = 'document' ).

    cut->add_text_resource( uri       = 'https://example.com/document.txt'
                            text      = 'This is a resource text'
                            mime_type = 'text/plain' ).

    " When
    json = cut->zif_mcp_internal~generate_json( ).
    DATA json_string TYPE string.
    json_string = json->stringify( ).

    " Then
    DATA result TYPE REF TO zif_mcp_ajson.
    result = zcl_mcp_ajson=>parse( json_string ).

    " First content (text)
    cl_abap_unit_assert=>assert_equals( exp = 'text'
                                        act = result->get( '/content/1/type' ) ).
    cl_abap_unit_assert=>assert_equals( exp = 'This is a text content'
                                        act = result->get( '/content/1/text' ) ).

    " Second content (image)
    cl_abap_unit_assert=>assert_equals( exp = 'image'
                                        act = result->get( '/content/2/type' ) ).
    cl_abap_unit_assert=>assert_equals( exp = 'SGVsbG8gV29ybGQ='
                                        act = result->get( '/content/2/data' ) ).

    " Third content (audio)
    cl_abap_unit_assert=>assert_equals( exp = 'audio'
                                        act = result->get( '/content/3/type' ) ).
    cl_abap_unit_assert=>assert_equals( exp = 'VGVzdCBhdWRpbw=='
                                        act = result->get( '/content/3/data' ) ).

    " Fourth content (resource link)
    cl_abap_unit_assert=>assert_equals( exp = 'resource_link'
                                        act = result->get( '/content/4/type' ) ).
    cl_abap_unit_assert=>assert_equals( exp = 'document'
                                        act = result->get( '/content/4/name' ) ).

    " Fifth content (embedded resource)
    cl_abap_unit_assert=>assert_equals( exp = 'resource'
                                        act = result->get( '/content/5/type' ) ).
    cl_abap_unit_assert=>assert_equals( exp = 'This is a resource text'
                                        act = result->get( '/content/5/resource/text' ) ).
  ENDMETHOD.

  METHOD test_with_error_flag.
    " Given
    cut->add_text_content( 'Error occurred during processing' ).

    cut->set_error( abap_true ).

    " When
    json = cut->zif_mcp_internal~generate_json( ).
    DATA json_string TYPE string.
    json_string = json->stringify( ).

    " Then
    DATA result TYPE REF TO zif_mcp_ajson.
    result = zcl_mcp_ajson=>parse( json_string ).

    cl_abap_unit_assert=>assert_equals( exp = 'true'
                                        act = result->get( '/isError' ) ).
  ENDMETHOD.

  METHOD test_with_metadata.
    " Given
    cut->add_text_content( 'Sample text' ).

    " Create metadata
    DATA meta TYPE REF TO zif_mcp_ajson.
    meta = zcl_mcp_ajson=>create_empty( ).
    meta->set( iv_path = '/version'
               iv_val  = '1.0' ).
    meta->set( iv_path = '/timestamp'
               iv_val  = '2023-05-15T12:30:45Z' ).
    cut->set_meta( meta ).

    " When
    json = cut->zif_mcp_internal~generate_json( ).
    DATA json_string TYPE string.
    json_string = json->stringify( ).

    " Then
    DATA result TYPE REF TO zif_mcp_ajson.
    result = zcl_mcp_ajson=>parse( json_string ).

    " Check content
    cl_abap_unit_assert=>assert_equals( exp = 'Sample text'
                                        act = result->get( '/content/1/text' ) ).

    " Check metadata
    cl_abap_unit_assert=>assert_equals( exp = '1.0'
                                        act = result->get( '/_meta/version' ) ).
    cl_abap_unit_assert=>assert_equals( exp = '2023-05-15T12:30:45Z'
                                        act = result->get( '/_meta/timestamp' ) ).
  ENDMETHOD.

  METHOD test_set_content.
    " Given
    DATA contents TYPE zcl_mcp_resp_call_tool=>content_list.

    " Create text content
    DATA text_wrapper TYPE zcl_mcp_resp_call_tool=>content_wrapper.
    text_wrapper-type = 'text'.
    DATA text_content TYPE REF TO zcl_mcp_resp_call_tool=>text_content.
    text_content = NEW #( type = 'text'
                          text = 'Set content example' ).
    text_wrapper-content = text_content.
    APPEND text_wrapper TO contents.

    " Create image content
    DATA image_wrapper TYPE zcl_mcp_resp_call_tool=>content_wrapper.
    image_wrapper-type = 'image'.
    DATA image_content TYPE REF TO zcl_mcp_resp_call_tool=>image_content.
    image_content = NEW #( type      = 'image'
                           data      = 'R0lGODlhAQABAIAAAP///wAAACH5BAEAAAAALAAAAAABAAEAAAICRAEAOw=='
                           mime_type = 'image/gif' ).
    image_wrapper-content = image_content.
    APPEND image_wrapper TO contents.

    " Create resource link content
    DATA link_wrapper TYPE zcl_mcp_resp_call_tool=>content_wrapper.
    link_wrapper-type = 'resource_link'.
    DATA link_content TYPE REF TO zcl_mcp_resp_call_tool=>resource_link.
    link_content = NEW #( uri  = 'https://example.com/file.pdf'
                          name = 'test_file' ).
    link_wrapper-content = link_content.
    APPEND link_wrapper TO contents.

    cut->set_content( contents ).

    " When
    json = cut->zif_mcp_internal~generate_json( ).
    DATA json_string TYPE string.
    json_string = json->stringify( ).

    " Then
    DATA result TYPE REF TO zif_mcp_ajson.
    result = zcl_mcp_ajson=>parse( json_string ).

    " Text content
    cl_abap_unit_assert=>assert_equals( exp = 'text'
                                        act = result->get( '/content/1/type' ) ).
    cl_abap_unit_assert=>assert_equals( exp = 'Set content example'
                                        act = result->get( '/content/1/text' ) ).

    " Image content
    cl_abap_unit_assert=>assert_equals( exp = 'image'
                                        act = result->get( '/content/2/type' ) ).
    cl_abap_unit_assert=>assert_equals( exp = 'R0lGODlhAQABAIAAAP///wAAACH5BAEAAAAALAAAAAABAAEAAAICRAEAOw=='
                                        act = result->get( '/content/2/data' ) ).

    " Resource link content
    cl_abap_unit_assert=>assert_equals( exp = 'resource_link'
                                        act = result->get( '/content/3/type' ) ).
    cl_abap_unit_assert=>assert_equals( exp = 'test_file'
                                        act = result->get( '/content/3/name' ) ).
  ENDMETHOD.

  METHOD test_annotations.
    " Given
    DATA annotations TYPE zcl_mcp_resp_call_tool=>annotations.

    annotations-audience = VALUE #( ( `user` ) ( `assistant` ) ).
    annotations-priority = '0.8'.

    cut->add_text_content( text        = 'Text with annotations'
                           annotations = annotations ).

    " When
    json = cut->zif_mcp_internal~generate_json( ).
    DATA json_string TYPE string.
    json_string = json->stringify( ).

    " Then
    DATA result TYPE REF TO zif_mcp_ajson.
    result = zcl_mcp_ajson=>parse( json_string ).

    cl_abap_unit_assert=>assert_equals( exp = 'user'
                                        act = result->get( '/content/1/annotations/audience/1' ) ).

    cl_abap_unit_assert=>assert_equals( exp = 'assistant'
                                        act = result->get( '/content/1/annotations/audience/2' ) ).

    cl_abap_unit_assert=>assert_equals( exp = '0.8'
                                        act = result->get( '/content/1/annotations/priority' ) ).
  ENDMETHOD.

  METHOD test_timestamp_annotations.
    " Given
    DATA annotations TYPE zcl_mcp_resp_call_tool=>annotations.
    annotations-audience = VALUE #( ( `user` ) ).
    annotations-priority = '0.9'.
    " Create a timestamp: 2024-01-15 14:30:45
    annotations-last_modified = '20240115143045'.

    cut->add_text_content( text        = 'Text with timestamp'
                           annotations = annotations ).

    " When
    json = cut->zif_mcp_internal~generate_json( ).
    DATA json_string TYPE string.
    json_string = json->stringify( ).

    " Then
    DATA result TYPE REF TO zif_mcp_ajson.
    result = zcl_mcp_ajson=>parse( json_string ).

    cl_abap_unit_assert=>assert_equals( exp = 'user'
                                        act = result->get( '/content/1/annotations/audience/1' ) ).

    cl_abap_unit_assert=>assert_equals( exp = '0.9'
                                        act = result->get( '/content/1/annotations/priority' ) ).

    " Check timestamp conversion (should be in ISO 8601 format with Z suffix)
    DATA timestamp_result TYPE string.
    timestamp_result = result->get( '/content/1/annotations/lastModified' ).
    cl_abap_unit_assert=>assert_not_initial( timestamp_result ).

    " Should end with Z (UTC indicator)
    cl_abap_unit_assert=>assert_char_cp( act = timestamp_result
                                         exp = '*Z' ).

    " Should contain T separator
    cl_abap_unit_assert=>assert_char_cp( act = timestamp_result
                                         exp = '*T*' ).
  ENDMETHOD.
ENDCLASS.
