"! <p class="shorttext synchronized">MCP Call Tool Result</p>
"! Implementation for the CallToolResult interface
CLASS zcl_mcp_resp_call_tool DEFINITION
  PUBLIC FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES zif_mcp_internal.

    " Types for annotations
    TYPES: BEGIN OF annotations,
             audience TYPE STANDARD TABLE OF string WITH EMPTY KEY,
             priority TYPE decfloat16,
           END OF annotations.

    " Resource contents types
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

    " Content types
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

    " Wrapper for any content type
    TYPES: BEGIN OF content_wrapper,
             type    TYPE string,
             content TYPE REF TO data,
           END OF content_wrapper.

    TYPES content_list TYPE STANDARD TABLE OF content_wrapper WITH EMPTY KEY.

    "! <p class="shorttext synchronized">Set Error Status</p>
    "!
    "! @parameter has_error | <p class="shorttext synchronized">Whether the tool call ended in an error</p>
    METHODS set_error
      IMPORTING has_error TYPE abap_bool DEFAULT abap_true.

    "! <p class="shorttext synchronized">Add Text Content</p>
    "!
    "! @parameter text        | <p class="shorttext synchronized">Text content</p>
    "! @parameter annotations | <p class="shorttext synchronized">Optional annotations</p>
    METHODS add_text_content
      IMPORTING !text       TYPE string
                annotations TYPE annotations OPTIONAL.

    "! <p class="shorttext synchronized">Add Image Content</p>
    "!
    "! @parameter data        | <p class="shorttext synchronized">Base64-encoded image data</p>
    "! @parameter mime_type   | <p class="shorttext synchronized">MIME type of the image</p>
    "! @parameter annotations | <p class="shorttext synchronized">Optional annotations</p>
    METHODS add_image_content
      IMPORTING !data       TYPE string
                mime_type   TYPE string
                annotations TYPE annotations OPTIONAL.

    "! <p class="shorttext synchronized">Add Audio Content</p>
    "!
    "! @parameter data        | <p class="shorttext synchronized">Base64-encoded audio data</p>
    "! @parameter mime_type   | <p class="shorttext synchronized">MIME type of the audio file</p>
    "! @parameter annotations | <p class="shorttext synchronized">Optional annotations</p>
    METHODS add_audio_content
      IMPORTING !data       TYPE string
                mime_type   TYPE string
                annotations TYPE annotations OPTIONAL.

    "! <p class="shorttext synchronized">Add Text Resource</p>
    "!
    "! @parameter uri         | <p class="shorttext synchronized">Resource URI</p>
    "! @parameter text        | <p class="shorttext synchronized">Text content</p>
    "! @parameter mime_type   | <p class="shorttext synchronized">Optional MIME type</p>
    "! @parameter annotations | <p class="shorttext synchronized">Optional annotations</p>
    METHODS add_text_resource
      IMPORTING uri         TYPE string
                !text       TYPE string
                mime_type   TYPE string      OPTIONAL
                annotations TYPE annotations OPTIONAL.

    "! <p class="shorttext synchronized">Add Blob Resource</p>
    "!
    "! @parameter uri         | <p class="shorttext synchronized">Resource URI</p>
    "! @parameter blob        | <p class="shorttext synchronized">Base64-encoded blob data</p>
    "! @parameter mime_type   | <p class="shorttext synchronized">Optional MIME type</p>
    "! @parameter annotations | <p class="shorttext synchronized">Optional annotations</p>
    METHODS add_blob_resource
      IMPORTING uri         TYPE string
                !blob       TYPE string
                mime_type   TYPE string      OPTIONAL
                annotations TYPE annotations OPTIONAL.

    "! <p class="shorttext synchronized">Set Content</p>
    "!
    "! @parameter content | <p class="shorttext synchronized">List of content items</p>
    METHODS set_content
      IMPORTING content TYPE content_list.

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

    " Simple content structures that don't use references
    TYPES: BEGIN OF content_item,
             type        TYPE string,
             text        TYPE string,      " For text content
             image_data  TYPE string,      " For image content
             image_mime  TYPE string,      " For image content
             audio_data  TYPE string,      " For audio content
             audio_mime  TYPE string,      " For audio content
             res_uri     TYPE string,      " For resource content
             res_text    TYPE string,      " For text resource content
             res_mime    TYPE string,      " For resource content
             res_blob    TYPE string,      " For blob resource content
             res_is_blob TYPE abap_bool,   " Flag for blob vs text
             annotations TYPE annotations, " For any content
           END OF content_item.

    " Table for all content, maintaining original order
    DATA content_items TYPE STANDARD TABLE OF content_item WITH EMPTY KEY.
    DATA meta          TYPE REF TO zif_mcp_ajson.
    DATA has_error     TYPE abap_bool.

    METHODS add_annotations_to_json
      IMPORTING !path       TYPE string
                annotations TYPE annotations
                ajson       TYPE REF TO zif_mcp_ajson
      RAISING   zcx_mcp_ajson_error.
ENDCLASS.

CLASS zcl_mcp_resp_call_tool IMPLEMENTATION.
  METHOD zif_mcp_internal~generate_json.
    result = zcl_mcp_ajson=>create_empty( ).

    " Create content array
    result->touch_array( '/content' ).

    " Add all content items in original order
    LOOP AT content_items ASSIGNING FIELD-SYMBOL(<item>) WHERE type IS NOT INITIAL.
      DATA(content_index) = sy-tabix.
      DATA(content_path) = |/content/{ content_index }|.

      " Set common type field
      result->set( iv_path         = |{ content_path }/type|
                   iv_val          = <item>-type
                   iv_ignore_empty = abap_false ).

      " Handle content based on its type
      CASE <item>-type.
        WHEN content_type-text.
          " Text content
          result->set( iv_path         = |{ content_path }/text|
                       iv_val          = <item>-text
                       iv_ignore_empty = abap_false ).

        WHEN content_type-image.
          " Image content
          result->set( iv_path         = |{ content_path }/data|
                       iv_val          = <item>-image_data
                       iv_ignore_empty = abap_false ).

          result->set( iv_path         = |{ content_path }/mimeType|
                       iv_val          = <item>-image_mime
                       iv_ignore_empty = abap_false ).

        WHEN content_type-audio.
          " Audio content
          result->set( iv_path         = |{ content_path }/data|
                       iv_val          = <item>-audio_data
                       iv_ignore_empty = abap_false ).

          result->set( iv_path         = |{ content_path }/mimeType|
                       iv_val          = <item>-audio_mime
                       iv_ignore_empty = abap_false ).

        WHEN content_type-resource.
          " Resource content
          result->set( iv_path         = |{ content_path }/resource/uri|
                       iv_val          = <item>-res_uri
                       iv_ignore_empty = abap_false ).

          IF <item>-res_mime IS NOT INITIAL.
            result->set( iv_path = |{ content_path }/resource/mimeType|
                         iv_val  = <item>-res_mime ).
          ENDIF.

          " Either blob or text resource
          IF <item>-res_is_blob = abap_true.
            result->set( iv_path         = |{ content_path }/resource/blob|
                         iv_val          = <item>-res_blob
                         iv_ignore_empty = abap_false ).
          ELSE.
            result->set( iv_path         = |{ content_path }/resource/text|
                         iv_val          = <item>-res_text
                         iv_ignore_empty = abap_false ).
          ENDIF.
      ENDCASE.

      " Add annotations for any content type
      add_annotations_to_json( path        = content_path
                               annotations = <item>-annotations
                               ajson       = result ).
    ENDLOOP.

    " Add isError flag if true (only if set)
    IF has_error = abap_true.
      result->set( iv_path = '/isError'
                   iv_val  = abap_true ).
    ENDIF.

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
      ajson->touch_array( |{ path }/annotations/audience| ).

      LOOP AT annotations-audience ASSIGNING FIELD-SYMBOL(<audience>).
        ajson->set( iv_path = |{ path }/annotations/audience/{ sy-tabix }|
                    iv_val  = <audience> ).
      ENDLOOP.
    ENDIF.

    " Add priority if not empty
    IF annotations-priority IS NOT INITIAL.
      ajson->set( iv_path = |{ path }/annotations/priority|
                  iv_val  = annotations-priority ).
    ENDIF.
  ENDMETHOD.

  METHOD set_error.
    me->has_error = has_error.
  ENDMETHOD.

  METHOD add_text_content.
    APPEND VALUE content_item( type        = content_type-text
                               text        = text
                               annotations = annotations )
           TO content_items.
  ENDMETHOD.

  METHOD add_image_content.
    APPEND VALUE content_item( type        = content_type-image
                               image_data  = data
                               image_mime  = mime_type
                               annotations = annotations )
           TO content_items.
  ENDMETHOD.

  METHOD add_text_resource.
    APPEND VALUE content_item( type        = content_type-resource
                               res_uri     = uri
                               res_text    = text
                               res_mime    = mime_type
                               res_is_blob = abap_false
                               annotations = annotations )
           TO content_items.
  ENDMETHOD.

  METHOD add_blob_resource.
    APPEND VALUE content_item( type        = content_type-resource
                               res_uri     = uri
                               res_blob    = blob
                               res_mime    = mime_type
                               res_is_blob = abap_true
                               annotations = annotations )
           TO content_items.
  ENDMETHOD.

  METHOD set_content.
    " Clear existing content
    CLEAR content_items.

    " Translate complex structure to simpler internal structure
    LOOP AT content ASSIGNING FIELD-SYMBOL(<content_wrapper>).
      CASE <content_wrapper>-type.
        WHEN content_type-text.
          DATA(text_item) = CAST text_content( <content_wrapper>-content ).

          APPEND VALUE content_item( type        = content_type-text
                                     text        = text_item->text
                                     annotations = text_item->annotations )
                 TO content_items.

        WHEN content_type-image.
          DATA(image_item) = CAST image_content( <content_wrapper>-content ).

          APPEND VALUE content_item( type        = content_type-image
                                     image_data  = image_item->data
                                     image_mime  = image_item->mime_type
                                     annotations = image_item->annotations )
                 TO content_items.

                WHEN content_type-audio.
          DATA(audio_item) = CAST audio_content( <content_wrapper>-content ).

          APPEND VALUE content_item( type        = content_type-image
                                     audio_data  = audio_item->data
                                     audio_mime  = audio_item->mime_type
                                     annotations = audio_item->annotations )
                 TO content_items.

        WHEN content_type-resource.
          DATA(resource_item) = CAST embedded_resource( <content_wrapper>-content ).

          " Try to determine if it's a text or blob resource
          TRY.
              " Try as text resource first
              DATA(text_resource) = CAST text_resource_contents( resource_item->resource ).

              APPEND VALUE content_item( type        = content_type-resource
                                         res_uri     = text_resource->uri
                                         res_text    = text_resource->text
                                         res_mime    = text_resource->mime_type
                                         res_is_blob = abap_false
                                         annotations = resource_item->annotations )
                     TO content_items.

            CATCH cx_sy_move_cast_error.
              " Must be a blob resource
              TRY.
                  DATA(blob_resource) = CAST blob_resource_contents( resource_item->resource ).

                  APPEND VALUE content_item( type        = content_type-resource
                                             res_uri     = blob_resource->uri
                                             res_blob    = blob_resource->blob
                                             res_mime    = blob_resource->mime_type
                                             res_is_blob = abap_true
                                             annotations = resource_item->annotations )
                         TO content_items.

                CATCH cx_sy_move_cast_error.
                  " Unknown resource type - skip
                  CONTINUE.
              ENDTRY.
          ENDTRY.
      ENDCASE.
    ENDLOOP.
  ENDMETHOD.

  METHOD set_meta.
    me->meta = meta.
  ENDMETHOD.

  METHOD add_audio_content.
    APPEND VALUE content_item( type        = content_type-audio
                               audio_data  = data
                               audio_mime  = mime_type
                               annotations = annotations )
           TO content_items.
  ENDMETHOD.

ENDCLASS.
