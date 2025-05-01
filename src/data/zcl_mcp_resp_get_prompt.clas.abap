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

    TYPES: BEGIN OF audio_content,
             type        TYPE string,
             data        TYPE string,
             mime_type   TYPE string,
             annotations TYPE annotations,
           END OF audio_content.

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

    "! <p class="shorttext synchronized">Add Audio Message</p>
    "!
    "! @parameter role        | <p class="shorttext synchronized">Role (user or assistant)</p>
    "! @parameter data        | <p class="shorttext synchronized">Base64-encoded audio data</p>
    "! @parameter mime_type   | <p class="shorttext synchronized">MIME type of the audio file</p>
    "! @parameter annotations | <p class="shorttext synchronized">Optional annotations</p>
    METHODS add_audio_message
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
    " Content type identifiers
    CONSTANTS: BEGIN OF content_type,
                 text     TYPE string VALUE 'text',
                 image    TYPE string VALUE 'image',
                 resource TYPE string VALUE 'resource',
                 audio    TYPE string VALUE 'audio',
               END OF content_type.

    " Message type enum
    CONSTANTS: BEGIN OF message_type,
                 text          TYPE i VALUE 1,
                 image         TYPE i VALUE 2,
                 text_resource TYPE i VALUE 3,
                 blob_resource TYPE i VALUE 4,
                 audio         TYPE i VALUE 5,
               END OF message_type.

    " Unified message storage structure
    TYPES: BEGIN OF unified_message,
             type        TYPE i,           " Message type (see message_type)
             role        TYPE string,      " Role (user or assistant)
             text        TYPE string,      " For text messages
             image_data  TYPE string,      " For image messages
             image_mime  TYPE string,      " For image messages
             audio_data  TYPE string,      " For audio messages
             audio_mime  TYPE string,      " For audio messages
             res_uri     TYPE string,      " For resource messages
             res_text    TYPE string,      " For text resource messages
             res_blob    TYPE string,      " For blob resource messages
             res_mime    TYPE string,      " For resource messages
             annotations TYPE annotations, " Annotations for any type
           END OF unified_message.

    " Internal data storage
    DATA description TYPE string.
    DATA messages    TYPE STANDARD TABLE OF unified_message WITH EMPTY KEY.
    DATA meta        TYPE REF TO zif_mcp_ajson.

    METHODS add_annotations_to_json
      IMPORTING !path       TYPE string
                annotations TYPE annotations
                !result     TYPE REF TO zif_mcp_ajson
      RAISING   zcx_mcp_ajson_error.
ENDCLASS.

CLASS zcl_mcp_resp_get_prompt IMPLEMENTATION.
  METHOD zif_mcp_internal~generate_json.
    result = zcl_mcp_ajson=>create_empty( ).

    " Add description if present
    IF description IS NOT INITIAL.
      result->set( iv_path = '/description'
                   iv_val  = description ).
    ENDIF.

    " Create messages array
    result->touch_array( '/messages' ).

    " Process all messages in order
    LOOP AT messages ASSIGNING FIELD-SYMBOL(<message>).
      DATA(message_path) = |/messages/{ sy-tabix }|.

      " Add role for all message types
      result->set( iv_path         = |{ message_path }/role|
                   iv_val          = <message>-role
                   iv_ignore_empty = abap_false ).

      CASE <message>-type.
        WHEN message_type-text.
          " Handle text message
          result->set( iv_path         = |{ message_path }/content/type|
                       iv_val          = content_type-text
                       iv_ignore_empty = abap_false ).

          result->set( iv_path         = |{ message_path }/content/text|
                       iv_val          = <message>-text
                       iv_ignore_empty = abap_false ).

        WHEN message_type-image.
          " Handle image message
          result->set( iv_path         = |{ message_path }/content/type|
                       iv_val          = content_type-image
                       iv_ignore_empty = abap_false ).

          result->set( iv_path         = |{ message_path }/content/data|
                       iv_val          = <message>-image_data
                       iv_ignore_empty = abap_false ).

          result->set( iv_path         = |{ message_path }/content/mimeType|
                       iv_val          = <message>-image_mime
                       iv_ignore_empty = abap_false ).

        WHEN message_type-audio.
          " Handle audio message
          result->set( iv_path         = |{ message_path }/content/type|
                       iv_val          = content_type-audio
                       iv_ignore_empty = abap_false ).

          result->set( iv_path         = |{ message_path }/content/data|
                       iv_val          = <message>-audio_data
                       iv_ignore_empty = abap_false ).

          result->set( iv_path         = |{ message_path }/content/mimeType|
                       iv_val          = <message>-audio_mime
                       iv_ignore_empty = abap_false ).

        WHEN message_type-text_resource.
          " Handle text resource
          result->set( iv_path         = |{ message_path }/content/type|
                       iv_val          = content_type-resource
                       iv_ignore_empty = abap_false ).

          result->set( iv_path         = |{ message_path }/content/resource/uri|
                       iv_val          = <message>-res_uri
                       iv_ignore_empty = abap_false ).

          result->set( iv_path         = |{ message_path }/content/resource/text|
                       iv_val          = <message>-res_text
                       iv_ignore_empty = abap_false ).

          IF <message>-res_mime IS NOT INITIAL.
            result->set( iv_path = |{ message_path }/content/resource/mimeType|
                         iv_val  = <message>-res_mime ).
          ENDIF.

        WHEN message_type-blob_resource.
          " Handle blob resource
          result->set( iv_path         = |{ message_path }/content/type|
                       iv_val          = content_type-resource
                       iv_ignore_empty = abap_false ).

          result->set( iv_path         = |{ message_path }/content/resource/uri|
                       iv_val          = <message>-res_uri
                       iv_ignore_empty = abap_false ).

          result->set( iv_path         = |{ message_path }/content/resource/blob|
                       iv_val          = <message>-res_blob
                       iv_ignore_empty = abap_false ).

          IF <message>-res_mime IS NOT INITIAL.
            result->set( iv_path = |{ message_path }/content/resource/mimeType|
                         iv_val  = <message>-res_mime ).
          ENDIF.
      ENDCASE.

      " Add annotations if present
      add_annotations_to_json( path        = |{ message_path }/content|
                               annotations = <message>-annotations
                               result      = result ).
    ENDLOOP.

    " Add metadata (optional)
    IF meta IS BOUND.
      result->set( iv_path = '/_meta'
                   iv_val  = meta ).
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
    me->description = description.
  ENDMETHOD.

  METHOD set_messages.
    " Clear existing messages
    CLEAR me->messages.

    " Process each message
    LOOP AT messages ASSIGNING FIELD-SYMBOL(<input_message>).
      " Skip if content is not bound
      IF <input_message>-content IS NOT BOUND.
        CONTINUE.
      ENDIF.

      " Get content as structure
      FIELD-SYMBOLS <content_struct> TYPE any.
      ASSIGN <input_message>-content->* TO <content_struct>.

      " Get content type field
      DATA type TYPE string.
      ASSIGN COMPONENT 'TYPE' OF STRUCTURE <content_struct> TO FIELD-SYMBOL(<type>).
      IF sy-subrc <> 0.
        " Skip if no type field
        CONTINUE.
      ENDIF.
      type = <type>.

      " Process based on content type
      CASE type.
        WHEN content_type-text.
          " Get text field
          ASSIGN COMPONENT 'TEXT' OF STRUCTURE <content_struct> TO FIELD-SYMBOL(<text>).
          IF sy-subrc <> 0.
            CONTINUE.
          ENDIF.

          " Get annotations
          DATA annotations TYPE annotations.
          ASSIGN COMPONENT 'ANNOTATIONS' OF STRUCTURE <content_struct> TO FIELD-SYMBOL(<annotations>).
          IF sy-subrc = 0.
            annotations = <annotations>.
          ENDIF.

          " Add text message
          APPEND VALUE unified_message( type        = message_type-text
                                        role        = <input_message>-role
                                        text        = <text>
                                        annotations = annotations ) TO me->messages.

        WHEN content_type-image.
          " Get image data
          ASSIGN COMPONENT 'DATA' OF STRUCTURE <content_struct> TO FIELD-SYMBOL(<data>).
          IF sy-subrc <> 0.
            CONTINUE.
          ENDIF.

          " Get MIME type
          ASSIGN COMPONENT 'MIME_TYPE' OF STRUCTURE <content_struct> TO FIELD-SYMBOL(<mime_type>).
          IF sy-subrc <> 0.
            CONTINUE.
          ENDIF.

          " Get annotations
          CLEAR annotations.
          ASSIGN COMPONENT 'ANNOTATIONS' OF STRUCTURE <content_struct> TO <annotations>.
          IF sy-subrc = 0.
            annotations = <annotations>.
          ENDIF.

          " Add image message
          APPEND VALUE unified_message( type        = message_type-image
                                        role        = <input_message>-role
                                        image_data  = <data>
                                        image_mime  = <mime_type>
                                        annotations = annotations ) TO me->messages.

        WHEN content_type-audio.
          " Get audio data
          ASSIGN COMPONENT 'DATA' OF STRUCTURE <content_struct> TO <data>.
          IF sy-subrc <> 0.
            CONTINUE.
          ENDIF.

          " Get MIME type
          ASSIGN COMPONENT 'MIME_TYPE' OF STRUCTURE <content_struct> TO <mime_type>.
          IF sy-subrc <> 0.
            CONTINUE.
          ENDIF.

          " Get annotations
          CLEAR annotations.
          ASSIGN COMPONENT 'ANNOTATIONS' OF STRUCTURE <content_struct> TO <annotations>.
          IF sy-subrc = 0.
            annotations = <annotations>.
          ENDIF.

          " Add audio message
          APPEND VALUE unified_message( type        = message_type-audio
                                        role        = <input_message>-role
                                        audio_data  = <data>
                                        audio_mime  = <mime_type>
                                        annotations = annotations ) TO me->messages.

        WHEN content_type-resource.
          " Get resource reference
          ASSIGN COMPONENT 'RESOURCE' OF STRUCTURE <content_struct> TO FIELD-SYMBOL(<resource>).
          IF sy-subrc <> 0 OR <resource> IS INITIAL.
            CONTINUE.
          ENDIF.

          " Get annotations
          CLEAR annotations.
          ASSIGN COMPONENT 'ANNOTATIONS' OF STRUCTURE <content_struct> TO <annotations>.
          IF sy-subrc = 0.
            annotations = <annotations>.
          ENDIF.

          " Access resource structure
          FIELD-SYMBOLS <res_struct> TYPE any.
          ASSIGN <resource>->* TO <res_struct>.
          IF sy-subrc <> 0.
            CONTINUE.
          ENDIF.

          " Check common fields
          ASSIGN COMPONENT 'URI' OF STRUCTURE <res_struct> TO FIELD-SYMBOL(<uri>).
          IF sy-subrc <> 0.
            CONTINUE.
          ENDIF.

          DATA mime_type TYPE string.
          ASSIGN COMPONENT 'MIME_TYPE' OF STRUCTURE <res_struct> TO FIELD-SYMBOL(<res_mime>).
          IF sy-subrc = 0.
            mime_type = <res_mime>.
          ENDIF.

          " Check if it's a text resource
          ASSIGN COMPONENT 'TEXT' OF STRUCTURE <res_struct> TO FIELD-SYMBOL(<res_text>).
          IF sy-subrc = 0.
            " It's a text resource
            APPEND VALUE unified_message( type        = message_type-text_resource
                                          role        = <input_message>-role
                                          res_uri     = <uri>
                                          res_text    = <res_text>
                                          res_mime    = mime_type
                                          annotations = annotations ) TO me->messages.
          ELSE.
            " Check if it's a blob resource
            ASSIGN COMPONENT 'BLOB' OF STRUCTURE <res_struct> TO FIELD-SYMBOL(<res_blob>).
            IF sy-subrc = 0.
              " It's a blob resource
              APPEND VALUE unified_message( type        = message_type-blob_resource
                                            role        = <input_message>-role
                                            res_uri     = <uri>
                                            res_blob    = <res_blob>
                                            res_mime    = mime_type
                                            annotations = annotations ) TO me->messages.
            ENDIF.
          ENDIF.
      ENDCASE.
    ENDLOOP.
  ENDMETHOD.

  METHOD add_text_message.
    APPEND VALUE unified_message( type        = message_type-text
                                  role        = role
                                  text        = text
                                  annotations = annotations ) TO messages.
  ENDMETHOD.

  METHOD add_image_message.
    APPEND VALUE unified_message( type        = message_type-image
                                  role        = role
                                  image_data  = data
                                  image_mime  = mime_type
                                  annotations = annotations ) TO messages.
  ENDMETHOD.

  METHOD add_audio_message.
    APPEND VALUE unified_message( type        = message_type-audio
                                  role        = role
                                  audio_data  = data
                                  audio_mime  = mime_type
                                  annotations = annotations ) TO messages.
  ENDMETHOD.

  METHOD add_text_resource_message.
    APPEND VALUE unified_message( type        = message_type-text_resource
                                  role        = role
                                  res_uri     = uri
                                  res_text    = text
                                  res_mime    = mime_type
                                  annotations = annotations ) TO messages.
  ENDMETHOD.

  METHOD add_blob_resource_message.
    APPEND VALUE unified_message( type        = message_type-blob_resource
                                  role        = role
                                  res_uri     = uri
                                  res_blob    = blob
                                  res_mime    = mime_type
                                  annotations = annotations ) TO messages.
  ENDMETHOD.

  METHOD set_meta.
    me->meta = meta.
  ENDMETHOD.
ENDCLASS.
