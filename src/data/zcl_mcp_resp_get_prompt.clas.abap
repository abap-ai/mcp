"! <p class="shorttext synchronized">MCP Get Prompt Result</p>
"! Implementation for the GetPromptResult interface
CLASS zcl_mcp_resp_get_prompt DEFINITION
  PUBLIC FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES zif_mcp_internal.

    TYPES: BEGIN OF annotations,
             audience TYPE STANDARD TABLE OF string WITH EMPTY KEY,
             priority TYPE decfloat16,
           END OF annotations.

    TYPES: BEGIN OF text_resource_contents,
             uri       TYPE string,
             mime_type TYPE string,
             text      TYPE string,
           END OF text_resource_contents.

    TYPES: BEGIN OF blob_resource_contents,
             uri       TYPE string,
             mime_type TYPE string,
             blob      TYPE string,
           END OF blob_resource_contents.

    " Content types for messages
    TYPES: BEGIN OF text_content,
             type        TYPE string,
             text        TYPE string,
             annotations TYPE annotations,
           END OF text_content.

    TYPES: BEGIN OF image_content,
             type        TYPE string,
             data        TYPE string,
             mime_type   TYPE string,
             annotations TYPE annotations,
           END OF image_content.

    TYPES: BEGIN OF embedded_resource,
             type        TYPE string,
             resource    TYPE REF TO data,
             annotations TYPE annotations,
           END OF embedded_resource.

    " Message structure
    TYPES: BEGIN OF prompt_message,
             role    TYPE string,
             content TYPE REF TO data,
           END OF prompt_message.

    TYPES prompt_messages TYPE STANDARD TABLE OF prompt_message WITH EMPTY KEY.

    "! <p class="shorttext synchronized">Set Prompt Description</p>
    "!
    "! @parameter description | <p class="shorttext synchronized">Description of the prompt</p>
    METHODS set_description
      IMPORTING !description TYPE string.

    "! <p class="shorttext synchronized">Set Prompt Messages</p>
    "!
    "! @parameter messages | <p class="shorttext synchronized">List of prompt messages</p>
    METHODS set_messages
      IMPORTING !messages TYPE prompt_messages.

    "! <p class="shorttext synchronized">Add Text Message</p>
    "!
    "! @parameter role        | <p class="shorttext synchronized">Role (user or assistant)</p>
    "! @parameter text        | <p class="shorttext synchronized">Text content</p>
    "! @parameter annotations | <p class="shorttext synchronized">Optional annotations</p>
    METHODS add_text_message
      IMPORTING role        TYPE string
                !text       TYPE string
                annotations TYPE annotations OPTIONAL.

    "! <p class="shorttext synchronized">Add Image Message</p>
    "!
    "! @parameter role        | <p class="shorttext synchronized">Role (user or assistant)</p>
    "! @parameter data        | <p class="shorttext synchronized">Base64-encoded image data</p>
    "! @parameter mime_type   | <p class="shorttext synchronized">MIME type of the image</p>
    "! @parameter annotations | <p class="shorttext synchronized">Optional annotations</p>
    METHODS add_image_message
      IMPORTING role        TYPE string
                !data       TYPE string
                mime_type   TYPE string
                annotations TYPE annotations OPTIONAL.

    "! <p class="shorttext synchronized">Add Text Resource Message</p>
    "!
    "! @parameter role        | <p class="shorttext synchronized">Role (user or assistant)</p>
    "! @parameter uri         | <p class="shorttext synchronized">Resource URI</p>
    "! @parameter text        | <p class="shorttext synchronized">Text content</p>
    "! @parameter mime_type   | <p class="shorttext synchronized">Optional MIME type</p>
    "! @parameter annotations | <p class="shorttext synchronized">Optional annotations</p>
    METHODS add_text_resource_message
      IMPORTING role        TYPE string
                uri         TYPE string
                !text       TYPE string
                mime_type   TYPE string      OPTIONAL
                annotations TYPE annotations OPTIONAL.

    "! <p class="shorttext synchronized">Add Blob Resource Message</p>
    "!
    "! @parameter role        | <p class="shorttext synchronized">Role (user or assistant)</p>
    "! @parameter uri         | <p class="shorttext synchronized">Resource URI</p>
    "! @parameter blob        | <p class="shorttext synchronized">Base64-encoded blob data</p>
    "! @parameter mime_type   | <p class="shorttext synchronized">Optional MIME type</p>
    "! @parameter annotations | <p class="shorttext synchronized">Optional annotations</p>
    METHODS add_blob_resource_message
      IMPORTING role        TYPE string
                uri         TYPE string
                !blob       TYPE string
                mime_type   TYPE string      OPTIONAL
                annotations TYPE annotations OPTIONAL.

    "! <p class="shorttext synchronized">Set Meta Data</p>
    "! Optional metadata to attach to response
    "!
    "! @parameter meta | <p class="shorttext synchronized">Meta data</p>
    METHODS set_meta
      IMPORTING meta TYPE REF TO zif_mcp_ajson.

  PRIVATE SECTION.
    DATA int_description TYPE string.
    DATA int_messages TYPE prompt_messages.
    DATA int_meta TYPE REF TO zif_mcp_ajson.

    METHODS add_annotations_to_json
      IMPORTING
        path        TYPE string
        annotations TYPE annotations
        result      TYPE REF TO zif_mcp_ajson
      RAISING
        zcx_mcp_ajson_error.
ENDCLASS.


CLASS zcl_mcp_resp_get_prompt IMPLEMENTATION.
  METHOD zif_mcp_internal~generate_json.
    result = zcl_mcp_ajson=>create_empty( ).
    " Add description if present
    IF int_description IS NOT INITIAL.
      result->set( iv_path = '/description'
                  iv_val  = int_description ).
    ENDIF.

    " Create messages array
    result->touch_array( '/messages' ).

    " Add all messages
    LOOP AT int_messages ASSIGNING FIELD-SYMBOL(<message>).
      DATA(message_index) = sy-tabix.
      DATA(message_path) = |/messages/{ message_index }|.

      " Add role (required)
      result->set( iv_path         = |{ message_path }/role|
                  iv_val          = <message>-role
                  iv_ignore_empty = abap_false ).

      " Skip if content is not bound
      IF <message>-content IS NOT BOUND.
        CONTINUE.
      ENDIF.

      " Handle content based on its type
      TRY.
          " First try as text content
          DATA(text_content) = CAST text_content( <message>-content ).

          result->set( iv_path         = |{ message_path }/content/type|
                      iv_val          = 'text'
                      iv_ignore_empty = abap_false ).

          result->set( iv_path         = |{ message_path }/content/text|
                      iv_val          = text_content->text
                      iv_ignore_empty = abap_false ).

          " Add annotations if present
          add_annotations_to_json( path        = |{ message_path }/content|
                                   annotations = text_content->annotations
                                   result       = result ).

        CATCH cx_sy_move_cast_error.
          TRY.
              " Try as image content
              DATA(image_content) = CAST image_content( <message>-content ).

              result->set( iv_path         = |{ message_path }/content/type|
                          iv_val          = 'image'
                          iv_ignore_empty = abap_false ).

              result->set( iv_path         = |{ message_path }/content/data|
                          iv_val          = image_content->data
                          iv_ignore_empty = abap_false ).

              result->set( iv_path         = |{ message_path }/content/mimeType|
                          iv_val          = image_content->mime_type
                          iv_ignore_empty = abap_false ).

              " Add annotations if present
              add_annotations_to_json( path        = |{ message_path }/content|
                                       annotations = image_content->annotations
                                       result       = result ).

            CATCH cx_sy_move_cast_error.
              TRY.
                  " Try as resource content
                  DATA(resource_content) = CAST embedded_resource( <message>-content ).

                  result->set( iv_path         = |{ message_path }/content/type|
                              iv_val          = 'resource'
                              iv_ignore_empty = abap_false ).

                  IF resource_content->resource IS BOUND.
                    " Try to determine if it's a text or blob resource
                    TRY.
                        " Try as text resource first
                        DATA(text_resource) = CAST text_resource_contents( resource_content->resource ).

                        " Set resource fields
                        result->set( iv_path         = |{ message_path }/content/resource/uri|
                                    iv_val          = text_resource->uri
                                    iv_ignore_empty = abap_false ).

                        IF text_resource->mime_type IS NOT INITIAL.
                          result->set( iv_path = |{ message_path }/content/resource/mimeType|
                                      iv_val  = text_resource->mime_type ).
                        ENDIF.

                        result->set( iv_path         = |{ message_path }/content/resource/text|
                                    iv_val          = text_resource->text
                                    iv_ignore_empty = abap_false ).

                      CATCH cx_sy_move_cast_error.
                        " Must be a blob resource
                        TRY.
                            DATA(blob_resource) = CAST blob_resource_contents( resource_content->resource ).

                            " Set resource fields
                            result->set( iv_path         = |{ message_path }/content/resource/uri|
                                        iv_val          = blob_resource->uri
                                        iv_ignore_empty = abap_false ).

                            IF blob_resource->mime_type IS NOT INITIAL.
                              result->set( iv_path = |{ message_path }/content/resource/mimeType|
                                          iv_val  = blob_resource->mime_type ).
                            ENDIF.

                            result->set( iv_path         = |{ message_path }/content/resource/blob|
                                        iv_val          = blob_resource->blob
                                        iv_ignore_empty = abap_false ).

                          CATCH cx_sy_move_cast_error.
                            " Unknown resource type - skip
                            CONTINUE.
                        ENDTRY.
                    ENDTRY.
                  ENDIF.

                  " Add annotations for resource
                  add_annotations_to_json( path        = |{ message_path }/content|
                                           annotations = resource_content->annotations
                                           result       = result ).

                CATCH cx_sy_move_cast_error.
                  " Unknown content type - skip
                  CONTINUE.
              ENDTRY.
          ENDTRY.
      ENDTRY.
    ENDLOOP.

    " Add metadata (optional)
    IF int_meta IS BOUND.
      result->set( iv_path = '/_meta'
                  iv_val  = int_meta ).
    ENDIF.
  ENDMETHOD.

  METHOD add_annotations_to_json.
    " Add annotations if not empty
    IF annotations-audience IS INITIAL AND annotations-priority IS INITIAL.
      RETURN.
    ENDIF.

    " Add audience array if not empty
    IF annotations-audience IS NOT INITIAL.
      result->touch_array( |{ path }/annotations/audience| ).

      LOOP AT annotations-audience ASSIGNING FIELD-SYMBOL(<audience>).
        result->set( iv_path = |{ path }/annotations/audience/{ sy-tabix }|
                    iv_val  = <audience> ).
      ENDLOOP.
    ENDIF.

    " Add priority if not empty
    IF annotations-priority IS NOT INITIAL.
      result->set( iv_path = |{ path }/annotations/priority|
                  iv_val  = annotations-priority ).
    ENDIF.
  ENDMETHOD.

  METHOD set_description.
    int_description = description.
  ENDMETHOD.

  METHOD set_messages.
    int_messages = messages.
  ENDMETHOD.

  METHOD add_text_message.
    DATA(message) = VALUE prompt_message( role = role ).
    DATA(content) = NEW text_content( type        = 'text'
                                      text        = text
                                      annotations = annotations ).
    message-content = content.
    APPEND message TO int_messages.
  ENDMETHOD.

  METHOD add_image_message.
    DATA(message) = VALUE prompt_message( role = role ).
    DATA(content) = NEW image_content( type        = 'image'
                                       data        = data
                                       mime_type    = mime_type
                                       annotations = annotations ).
    message-content = content.
    APPEND message TO int_messages.
  ENDMETHOD.

  METHOD add_text_resource_message.
    DATA(message) = VALUE prompt_message( role = role ).
    DATA(resource) = NEW text_resource_contents( uri      = uri
                                                 text     = text
                                                 mime_type = mime_type ).
    DATA(content) = NEW embedded_resource( type        = 'resource'
                                           resource    = resource
                                           annotations = annotations ).
    message-content = content.
    APPEND message TO int_messages.
  ENDMETHOD.

  METHOD add_blob_resource_message.
    DATA(message) = VALUE prompt_message( role = role ).
    DATA(resource) = NEW blob_resource_contents( uri      = uri
                                                 blob     = blob
                                                 mime_type = mime_type ).
    DATA(content) = NEW embedded_resource( type        = 'resource'
                                           resource    = resource
                                           annotations = annotations ).
    message-content = content.
    APPEND message TO int_messages.
  ENDMETHOD.

  METHOD set_meta.
    int_meta = meta.
  ENDMETHOD.
ENDCLASS.
