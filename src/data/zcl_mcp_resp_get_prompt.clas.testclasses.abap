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
    METHODS test_audio_message            FOR TESTING RAISING zcx_mcp_ajson_error.
    METHODS test_audio_with_annotations   FOR TESTING RAISING zcx_mcp_ajson_error.
    METHODS test_set_messages_text        FOR TESTING RAISING zcx_mcp_ajson_error.
    METHODS test_set_messages_mixed       FOR TESTING RAISING zcx_mcp_ajson_error.
    METHODS test_set_messages_audio       FOR TESTING RAISING zcx_mcp_ajson_error.
    METHODS test_set_messages_resources   FOR TESTING RAISING zcx_mcp_ajson_error.
    METHODS test_complex_conversation     FOR TESTING RAISING zcx_mcp_ajson_error.
    METHODS test_replace_messages         FOR TESTING RAISING zcx_mcp_ajson_error.

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
    " Empty messages array should still exist
    assert_path_exists( '/messages' ).
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
    DATA blob_data TYPE string VALUE 'SGVsbG8gd29ybGQh'.

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

  METHOD test_audio_message.
    " Arrange
    DATA audio_data TYPE string VALUE 'SGVsbG8gdGhpcyBpcyBhIHRlc3QgYXVkaW8gZmlsZQ=='.

    cut->add_audio_message( role      = 'user'
                            data      = audio_data
                            mime_type = 'audio/mp3' ).

    " Act
    json = cut->zif_mcp_internal~generate_json( ).

    " Assert
    assert_path_exists( '/messages' ).
    assert_path_equals( path  = '/messages/1/role'
                        value = 'user' ).
    assert_path_equals( path  = '/messages/1/content/type'
                        value = 'audio' ).
    assert_path_equals( path  = '/messages/1/content/data'
                        value = audio_data ).
    assert_path_equals( path  = '/messages/1/content/mimeType'
                        value = 'audio/mp3' ).
  ENDMETHOD.

  METHOD test_audio_with_annotations.
    " Arrange
    DATA audio_data TYPE string VALUE 'SGVsbG8gdGhpcyBpcyBhIHRlc3QgYXVkaW8gZmlsZQ=='.

    DATA audiences  TYPE zcl_mcp_resp_get_prompt=>annotations-audience.

    APPEND 'user' TO audiences.

    DATA annotations TYPE zcl_mcp_resp_get_prompt=>annotations.
    annotations-audience = audiences.
    annotations-priority = '0.9'.

    cut->add_audio_message( role        = 'user'
                            data        = audio_data
                            mime_type   = 'audio/mp3'
                            annotations = annotations ).

    " Act
    json = cut->zif_mcp_internal~generate_json( ).

    " Assert
    assert_path_exists( '/messages' ).
    assert_path_equals( path  = '/messages/1/role'
                        value = 'user' ).
    assert_path_equals( path  = '/messages/1/content/type'
                        value = 'audio' ).
    assert_path_equals( path  = '/messages/1/content/mimeType'
                        value = 'audio/mp3' ).

    " Check annotations
    assert_path_exists( '/messages/1/content/annotations' ).
    assert_path_exists( '/messages/1/content/annotations/audience' ).
    assert_path_equals( path  = '/messages/1/content/annotations/audience/1'
                        value = 'user' ).
    assert_path_equals( path  = '/messages/1/content/annotations/priority'
                        value = '0.9' ).
  ENDMETHOD.

  METHOD test_set_messages_text.
    " Arrange
    DATA messages     TYPE zcl_mcp_resp_get_prompt=>prompt_messages.
    DATA message      TYPE zcl_mcp_resp_get_prompt=>prompt_message.
    DATA text_content TYPE REF TO zcl_mcp_resp_get_prompt=>text_content.

    " Create first message (user)
    CREATE DATA text_content.
    text_content->type = 'text'.
    text_content->text = 'Hello, this is user'.

    message-role    = 'user'.
    message-content = text_content.
    APPEND message TO messages.

    " Create second message (assistant)
    CREATE DATA text_content.
    text_content->type = 'text'.
    text_content->text = 'Hello, this is assistant'.

    message-role    = 'assistant'.
    message-content = text_content.
    APPEND message TO messages.

    " Act - Set the messages
    cut->set_messages( messages ).
    json = cut->zif_mcp_internal~generate_json( ).

    " Assert
    assert_path_exists( '/messages' ).
    assert_path_equals( path  = '/messages/1/role'
                        value = 'user' ).
    assert_path_equals( path  = '/messages/1/content/type'
                        value = 'text' ).
    assert_path_equals( path  = '/messages/1/content/text'
                        value = 'Hello, this is user' ).

    assert_path_equals( path  = '/messages/2/role'
                        value = 'assistant' ).
    assert_path_equals( path  = '/messages/2/content/type'
                        value = 'text' ).
    assert_path_equals( path  = '/messages/2/content/text'
                        value = 'Hello, this is assistant' ).
  ENDMETHOD.

  METHOD test_set_messages_mixed.
    " Arrange
    DATA messages      TYPE zcl_mcp_resp_get_prompt=>prompt_messages.
    DATA message       TYPE zcl_mcp_resp_get_prompt=>prompt_message.
    DATA text_content  TYPE REF TO zcl_mcp_resp_get_prompt=>text_content.
    DATA image_content TYPE REF TO zcl_mcp_resp_get_prompt=>image_content.

    " Create text message
    CREATE DATA text_content.
    text_content->type = 'text'.
    text_content->text = 'Can you describe this image?'.

    message-role    = 'user'.
    message-content = text_content.
    APPEND message TO messages.

    " Create image message
    DATA img_data TYPE string
                  VALUE 'iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAYAAAAfFcSJAAAADUlEQVR42mP8z/C/HgAGgwJ/lK3Q6wAAAABJRU5ErkJggg=='.

    CREATE DATA image_content.
    image_content->type      = 'image'.
    image_content->data      = img_data.
    image_content->mime_type = 'image/png'.

    message-role    = 'user'.
    message-content = image_content.
    APPEND message TO messages.

    " Act - Set the messages
    cut->set_messages( messages ).
    json = cut->zif_mcp_internal~generate_json( ).

    " Assert
    assert_path_exists( '/messages' ).
    assert_path_equals( path  = '/messages/1/role'
                        value = 'user' ).
    assert_path_equals( path  = '/messages/1/content/type'
                        value = 'text' ).
    assert_path_equals( path  = '/messages/1/content/text'
                        value = 'Can you describe this image?' ).

    assert_path_equals( path  = '/messages/2/role'
                        value = 'user' ).
    assert_path_equals( path  = '/messages/2/content/type'
                        value = 'image' ).
    assert_path_equals( path  = '/messages/2/content/data'
                        value = img_data ).
    assert_path_equals( path  = '/messages/2/content/mimeType'
                        value = 'image/png' ).
  ENDMETHOD.

  METHOD test_set_messages_audio.
    " Arrange
    DATA messages      TYPE zcl_mcp_resp_get_prompt=>prompt_messages.
    DATA message       TYPE zcl_mcp_resp_get_prompt=>prompt_message.
    DATA audio_content TYPE REF TO zcl_mcp_resp_get_prompt=>audio_content.

    " Create audio message
    DATA audio_data TYPE string VALUE 'SGVsbG8gdGhpcyBpcyBhIHRlc3QgYXVkaW8gZmlsZQ=='.

    CREATE DATA audio_content.
    audio_content->type      = 'audio'.
    audio_content->data      = audio_data.
    audio_content->mime_type = 'audio/mp3'.

    message-role    = 'user'.
    message-content = audio_content.
    APPEND message TO messages.

    " Act - Set the messages
    cut->set_messages( messages ).
    json = cut->zif_mcp_internal~generate_json( ).

    " Assert
    assert_path_exists( '/messages' ).
    assert_path_equals( path  = '/messages/1/role'
                        value = 'user' ).
    assert_path_equals( path  = '/messages/1/content/type'
                        value = 'audio' ).
    assert_path_equals( path  = '/messages/1/content/data'
                        value = audio_data ).
    assert_path_equals( path  = '/messages/1/content/mimeType'
                        value = 'audio/mp3' ).
  ENDMETHOD.

  METHOD test_set_messages_resources.
    " Arrange
    DATA messages         TYPE zcl_mcp_resp_get_prompt=>prompt_messages.
    DATA message          TYPE zcl_mcp_resp_get_prompt=>prompt_message.
    DATA text_res_content TYPE REF TO zcl_mcp_resp_get_prompt=>embedded_resource.
    DATA blob_res_content TYPE REF TO zcl_mcp_resp_get_prompt=>embedded_resource.
    DATA text_resource    TYPE REF TO zcl_mcp_resp_get_prompt=>text_resource_contents.
    DATA blob_resource    TYPE REF TO zcl_mcp_resp_get_prompt=>blob_resource_contents.

    " Create text resource message
    CREATE DATA text_resource.
    text_resource->uri       = 'file:///example.txt'.
    text_resource->text      = 'This is the content of the text file'.
    text_resource->mime_type = 'text/plain'.

    CREATE DATA text_res_content.
    text_res_content->type     = 'resource'.
    text_res_content->resource = text_resource.

    message-role    = 'user'.
    message-content = text_res_content.
    APPEND message TO messages.

    " Create blob resource message
    DATA blob_data TYPE string VALUE 'SGVsbG8gd29ybGQh'.

    CREATE DATA blob_resource.
    blob_resource->uri       = 'file:///example.bin'.
    blob_resource->blob      = blob_data.
    blob_resource->mime_type = 'application/octet-stream'.

    CREATE DATA blob_res_content.
    blob_res_content->type     = 'resource'.
    blob_res_content->resource = blob_resource.

    message-role    = 'user'.
    message-content = blob_res_content.
    APPEND message TO messages.

    " Act - Set the messages
    cut->set_messages( messages ).
    json = cut->zif_mcp_internal~generate_json( ).

    " Assert
    assert_path_exists( '/messages' ).

    " Text resource assertions
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

    " Blob resource assertions
    assert_path_equals( path  = '/messages/2/role'
                        value = 'user' ).
    assert_path_equals( path  = '/messages/2/content/type'
                        value = 'resource' ).
    assert_path_equals( path  = '/messages/2/content/resource/uri'
                        value = 'file:///example.bin' ).
    assert_path_equals( path  = '/messages/2/content/resource/blob'
                        value = blob_data ).
    assert_path_equals( path  = '/messages/2/content/resource/mimeType'
                        value = 'application/octet-stream' ).
  ENDMETHOD.

  METHOD test_complex_conversation.
    " Arrange - A complex conversation with multiple message types

    " User asks a question with text
    cut->add_text_message( role = 'user'
                           text = 'I have an audio recording and an image. Can you analyze both?' ).

    " User sends audio file
    DATA audio_data TYPE string VALUE 'SGVsbG8gdGhpcyBpcyBhIHRlc3QgYXVkaW8gZmlsZQ=='.
    cut->add_audio_message( role      = 'user'
                            data      = audio_data
                            mime_type = 'audio/mp3' ).

    " User sends image
    DATA img_data TYPE string
                  VALUE 'iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAYAAAAfFcSJAAAADUlEQVR42mP8z/C/HgAGgwJ/lK3Q6wAAAABJRU5ErkJggg=='.
    cut->add_image_message( role      = 'user'
                            data      = img_data
                            mime_type = 'image/png' ).

    " Assistant responds
    cut->add_text_message(
        role = 'assistant'
        text = 'I can analyze both. The audio appears to be a voice recording, and the image is a 1x1 pixel PNG.' ).

    " Act
    json = cut->zif_mcp_internal~generate_json( ).

    " Assert
    assert_path_exists( '/messages' ).

    " First message - text
    assert_path_equals( path  = '/messages/1/role'
                        value = 'user' ).
    assert_path_equals( path  = '/messages/1/content/type'
                        value = 'text' ).

    " Second message - audio
    assert_path_equals( path  = '/messages/2/role'
                        value = 'user' ).
    assert_path_equals( path  = '/messages/2/content/type'
                        value = 'audio' ).
    assert_path_equals( path  = '/messages/2/content/data'
                        value = audio_data ).

    " Third message - image
    assert_path_equals( path  = '/messages/3/role'
                        value = 'user' ).
    assert_path_equals( path  = '/messages/3/content/type'
                        value = 'image' ).
    assert_path_equals( path  = '/messages/3/content/data'
                        value = img_data ).

    " Fourth message - assistant response
    assert_path_equals( path  = '/messages/4/role'
                        value = 'assistant' ).
    assert_path_equals( path  = '/messages/4/content/type'
                        value = 'text' ).
  ENDMETHOD.

  METHOD test_replace_messages.
    " Arrange - Add some initial messages
    cut->add_text_message( role = 'user'
                           text = 'Initial message' ).

    " Create new set of messages
    DATA messages     TYPE zcl_mcp_resp_get_prompt=>prompt_messages.
    DATA message      TYPE zcl_mcp_resp_get_prompt=>prompt_message.
    DATA text_content TYPE REF TO zcl_mcp_resp_get_prompt=>text_content.

    CREATE DATA text_content.
    text_content->type = 'text'.
    text_content->text = 'Replacement message'.

    message-role    = 'assistant'.
    message-content = text_content.
    APPEND message TO messages.

    " Act - Replace messages
    cut->set_messages( messages ).
    json = cut->zif_mcp_internal~generate_json( ).

    " Assert - Only the replacement message should be present
    assert_path_exists( '/messages' ).
    assert_path_equals( path  = '/messages/1/role'
                        value = 'assistant' ).
    assert_path_equals( path  = '/messages/1/content/text'
                        value = 'Replacement message' ).

    " Verify there's only one message by checking total array count
    DATA found_messages TYPE i.
    DATA ix             TYPE i VALUE 1.

    WHILE json->exists( |/messages/{ ix }| ).
      found_messages = found_messages + 1.
      ix = ix + 1.
    ENDWHILE.

    cl_abap_unit_assert=>assert_equals( exp = 1
                                        act = found_messages
                                        msg = 'There should be exactly one message after replacement' ).
  ENDMETHOD.
ENDCLASS.
