CLASS ltcl_get_prompt_result DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA cut  TYPE REF TO zcl_mcp_resp_get_prompt.
    DATA json TYPE REF TO zif_mcp_ajson.

    METHODS setup.
    METHODS test_description              FOR TESTING RAISING zcx_mcp_ajson_error.
    METHODS test_text_message             FOR TESTING RAISING zcx_mcp_ajson_error.
    METHODS test_image_message            FOR TESTING RAISING zcx_mcp_ajson_error.
    METHODS test_text_resource_message    FOR TESTING RAISING zcx_mcp_ajson_error.
    METHODS test_blob_resource_message    FOR TESTING RAISING zcx_mcp_ajson_error.
    METHODS test_mixed_messages           FOR TESTING RAISING zcx_mcp_ajson_error.
    METHODS test_message_with_annotations FOR TESTING RAISING zcx_mcp_ajson_error.
    METHODS test_with_meta                FOR TESTING RAISING zcx_mcp_ajson_error.

    METHODS assert_path_exists
      IMPORTING !path TYPE string.

    METHODS assert_path_equals
      IMPORTING !path  TYPE string
                !value TYPE any.
ENDCLASS.

CLASS ltcl_get_prompt_result IMPLEMENTATION.
  METHOD setup.
    cut = NEW zcl_mcp_resp_get_prompt( ).
    json = zcl_mcp_ajson=>create_empty( ).
  ENDMETHOD.

  METHOD test_description.
    " Arrange
    cut->set_description( 'This is a test prompt description' ).

    " Act
    json = cut->zif_mcp_internal~generate_json( ).

    " Assert
    assert_path_exists( '/description' ).
    assert_path_equals( path  = '/description'
                        value = 'This is a test prompt description' ).
    assert_path_exists( '/messages' ). " Empty messages array should still exist
  ENDMETHOD.

  METHOD test_text_message.
    " Arrange
    cut->add_text_message( role = 'user'
                           text = 'Hello, how can you help me today?' ).

    " Act
    json = cut->zif_mcp_internal~generate_json( ).

    " Assert
    assert_path_exists( '/messages' ).
    assert_path_equals( path  = '/messages/1/role'
                        value = 'user' ).
    assert_path_equals( path  = '/messages/1/content/type'
                        value = 'text' ).
    assert_path_equals( path  = '/messages/1/content/text'
                        value = 'Hello, how can you help me today?' ).
  ENDMETHOD.

  METHOD test_image_message.
    " Arrange
    DATA img_data TYPE string
                  VALUE 'iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAYAAAAfFcSJAAAADUlEQVR42mP8z/C/HgAGgwJ/lK3Q6wAAAABJRU5ErkJggg=='.

    cut->add_image_message( role      = 'user'
                            data      = img_data
                            mime_type = 'image/png' ).

    " Act
    json = cut->zif_mcp_internal~generate_json( ).

    " Assert
    assert_path_exists( '/messages' ).
    assert_path_equals( path  = '/messages/1/role'
                        value = 'user' ).
    assert_path_equals( path  = '/messages/1/content/type'
                        value = 'image' ).
    assert_path_equals( path  = '/messages/1/content/data'
                        value = img_data ).
    assert_path_equals( path  = '/messages/1/content/mimeType'
                        value = 'image/png' ).
  ENDMETHOD.

  METHOD test_text_resource_message.
    " Arrange
    cut->add_text_resource_message( role      = 'user'
                                    uri       = 'file:///example.txt'
                                    text      = 'This is the content of the text file'
                                    mime_type = 'text/plain' ).

    " Act
    json = cut->zif_mcp_internal~generate_json( ).

    " Assert
    assert_path_exists( '/messages' ).
    assert_path_equals( path  = '/messages/1/role'
                        value = 'user' ).
    assert_path_equals( path  = '/messages/1/content/type'
                        value = 'resource' ).
    assert_path_equals( path  = '/messages/1/content/resource/uri'
                        value = 'file:///example.txt' ).
    assert_path_equals( path  = '/messages/1/content/resource/text'
                        value = 'This is the content of the text file' ).
    assert_path_equals( path  = '/messages/1/content/resource/mimeType'
                        value = 'text/plain' ).
  ENDMETHOD.

  METHOD test_blob_resource_message.
    " Arrange
    DATA blob_data TYPE string VALUE 'SGVsbG8gd29ybGQh'. " "Hello world!" in base64

    cut->add_blob_resource_message( role      = 'user'
                                    uri       = 'file:///example.bin'
                                    blob      = blob_data
                                    mime_type = 'application/octet-stream' ).

    " Act
    json = cut->zif_mcp_internal~generate_json( ).

    " Assert
    assert_path_exists( '/messages' ).
    assert_path_equals( path  = '/messages/1/role'
                        value = 'user' ).
    assert_path_equals( path  = '/messages/1/content/type'
                        value = 'resource' ).
    assert_path_equals( path  = '/messages/1/content/resource/uri'
                        value = 'file:///example.bin' ).
    assert_path_equals( path  = '/messages/1/content/resource/blob'
                        value = blob_data ).
    assert_path_equals( path  = '/messages/1/content/resource/mimeType'
                        value = 'application/octet-stream' ).
  ENDMETHOD.

  METHOD test_mixed_messages.
    " Arrange
    " Add a user message
    cut->add_text_message( role = 'user'
                           text = 'Can you analyze this image?' ).

    " Add an image from user
    DATA img_data TYPE string
                  VALUE 'iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAYAAAAfFcSJAAAADUlEQVR42mP8z/C/HgAGgwJ/lK3Q6wAAAABJRU5ErkJggg=='.

    cut->add_image_message( role      = 'user'
                            data      = img_data
                            mime_type = 'image/png' ).

    " Add assistant response
    cut->add_text_message( role = 'assistant'
                           text = 'I can see a simple 1x1 pixel transparent PNG image.' ).

    " Act
    json = cut->zif_mcp_internal~generate_json( ).

    " Assert
    assert_path_exists( '/messages' ).

    " First message (user text)
    assert_path_equals( path  = '/messages/1/role'
                        value = 'user' ).
    assert_path_equals( path  = '/messages/1/content/type'
                        value = 'text' ).
    assert_path_equals( path  = '/messages/1/content/text'
                        value = 'Can you analyze this image?' ).

    " Second message (user image)
    assert_path_equals( path  = '/messages/2/role'
                        value = 'user' ).
    assert_path_equals( path  = '/messages/2/content/type'
                        value = 'image' ).
    assert_path_equals( path  = '/messages/2/content/data'
                        value = img_data ).

    " Third message (assistant response)
    assert_path_equals( path  = '/messages/3/role'
                        value = 'assistant' ).
    assert_path_equals( path  = '/messages/3/content/type'
                        value = 'text' ).
    assert_path_equals( path  = '/messages/3/content/text'
                        value = 'I can see a simple 1x1 pixel transparent PNG image.' ).
  ENDMETHOD.

  METHOD test_message_with_annotations.
    " Arrange
    DATA audiences TYPE zcl_mcp_resp_get_prompt=>annotations-audience.

    APPEND 'user' TO audiences.
    APPEND 'assistant' TO audiences.

    DATA annotations TYPE zcl_mcp_resp_get_prompt=>annotations.
    annotations-audience = audiences.
    annotations-priority = '0.75'.

    cut->add_text_message( role        = 'assistant'
                           text        = 'This message has annotations'
                           annotations = annotations ).

    " Act
    json = cut->zif_mcp_internal~generate_json( ).

    " Assert
    assert_path_exists( '/messages' ).
    assert_path_equals( path  = '/messages/1/role'
                        value = 'assistant' ).
    assert_path_equals( path  = '/messages/1/content/type'
                        value = 'text' ).
    assert_path_equals( path  = '/messages/1/content/text'
                        value = 'This message has annotations' ).

    " Check annotations
    assert_path_exists( '/messages/1/content/annotations' ).
    assert_path_exists( '/messages/1/content/annotations/audience' ).
    assert_path_equals( path  = '/messages/1/content/annotations/audience/1'
                        value = 'user' ).
    assert_path_equals( path  = '/messages/1/content/annotations/audience/2'
                        value = 'assistant' ).
    assert_path_equals( path  = '/messages/1/content/annotations/priority'
                        value = '0.75' ).
  ENDMETHOD.

  METHOD test_with_meta.
    " Arrange
    cut->add_text_message( role = 'user'
                           text = 'Hello' ).

    " Add metadata
    DATA(meta) = zcl_mcp_ajson=>create_empty( ).
    meta->set( iv_path = '/version'
               iv_val  = '1.0' ).
    meta->set( iv_path = '/timestamp'
               iv_val  = '2023-10-15T12:34:56Z' ).

    cut->set_meta( meta ).

    " Act
    json = cut->zif_mcp_internal~generate_json( ).

    " Assert
    assert_path_exists( '/messages' ).
    assert_path_exists( '/_meta' ).
    assert_path_equals( path  = '/_meta/version'
                        value = '1.0' ).
    assert_path_equals( path  = '/_meta/timestamp'
                        value = '2023-10-15T12:34:56Z' ).
  ENDMETHOD.

  METHOD assert_path_exists.
    cl_abap_unit_assert=>assert_true( act = json->exists( path )
                                      msg = |Path { path } should exist| ).
  ENDMETHOD.

  METHOD assert_path_equals.
    cl_abap_unit_assert=>assert_equals( exp = value
                                        act = json->get( path )
                                        msg = |Path { path } should equal expected value| ).
  ENDMETHOD.
ENDCLASS.
