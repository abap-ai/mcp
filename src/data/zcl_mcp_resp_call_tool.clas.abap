"! <p class="shorttext synchronized">MCP Call Tool Result</p>
"! Implementation for the CallToolResult interface
CLASS zcl_mcp_resp_call_tool DEFINITION
  PUBLIC FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES zif_mcp_internal.

    " Types for annotations
    TYPES: BEGIN OF annotations,
             audience      TYPE STANDARD TABLE OF string WITH DEFAULT KEY,
             priority      TYPE decfloat16,
             last_modified TYPE timestamp,
           END OF annotations.

    " Resource contents types
    TYPES: BEGIN OF text_resource_contents,
             uri       TYPE string,
             mime_type TYPE string,
             text      TYPE string,
             meta      TYPE REF TO zif_mcp_ajson,
           END OF text_resource_contents.

    TYPES: BEGIN OF blob_resource_contents,
             uri       TYPE string,
             mime_type TYPE string,
             blob      TYPE string,
             meta      TYPE REF TO zif_mcp_ajson,
           END OF blob_resource_contents.

    " Resource link type
    TYPES: BEGIN OF resource_link,
             uri         TYPE string,
             name        TYPE string,
             title       TYPE string,
             description TYPE string,
             mime_type   TYPE string,
             size        TYPE i,
             annotations TYPE annotations,
             meta        TYPE REF TO zif_mcp_ajson,
           END OF resource_link.

    " Content types
    TYPES: BEGIN OF text_content,
             type        TYPE string,
             text        TYPE string,
             annotations TYPE annotations,
             meta        TYPE REF TO zif_mcp_ajson,
           END OF text_content.

    TYPES: BEGIN OF image_content,
             type        TYPE string,
             data        TYPE string,
             mime_type   TYPE string,
             annotations TYPE annotations,
             meta        TYPE REF TO zif_mcp_ajson,
           END OF image_content.

    TYPES: BEGIN OF audio_content,
             type        TYPE string,
             data        TYPE string,
             mime_type   TYPE string,
             annotations TYPE annotations,
             meta        TYPE REF TO zif_mcp_ajson,
           END OF audio_content.

    TYPES: BEGIN OF embedded_resource,
             type        TYPE string,
             resource    TYPE REF TO data,
             annotations TYPE annotations,
             meta        TYPE REF TO zif_mcp_ajson,
           END OF embedded_resource.

    " Wrapper for any content type
    TYPES: BEGIN OF content_wrapper,
             type    TYPE string,
             content TYPE REF TO data,
           END OF content_wrapper.

    TYPES content_list TYPE STANDARD TABLE OF content_wrapper WITH DEFAULT KEY.

    "! <p class="shorttext synchronized">Set Error Status</p>
    "!
    "! @parameter has_error | <p class="shorttext synchronized">Whether the tool call ended in an error</p>
    METHODS set_error
      IMPORTING has_error TYPE abap_bool DEFAULT abap_true.

    "! <p class="shorttext synchronized">Set Structured Content</p>
    "!
    "! @parameter structured_content | <p class="shorttext synchronized">Structured result data</p>
    "! @parameter add_text_content   | <p class="shorttext synchronized">Auto-add text representation</p>
    METHODS set_structured_content
      IMPORTING structured_content TYPE REF TO zif_mcp_ajson
                add_text_content   TYPE abap_bool DEFAULT abap_true.

    "! <p class="shorttext synchronized">Add Text Content</p>
    "!
    "! @parameter text        | <p class="shorttext synchronized">Text content</p>
    "! @parameter annotations | <p class="shorttext synchronized">Optional annotations</p>
    "! @parameter meta        | <p class="shorttext synchronized">Optional metadata</p>
    METHODS add_text_content
      IMPORTING !text       TYPE string
                annotations TYPE annotations OPTIONAL
                meta        TYPE REF TO zif_mcp_ajson OPTIONAL.

    "! <p class="shorttext synchronized">Add Image Content</p>
    "!
    "! @parameter data        | <p class="shorttext synchronized">Base64-encoded image data</p>
    "! @parameter mime_type   | <p class="shorttext synchronized">MIME type of the image</p>
    "! @parameter annotations | <p class="shorttext synchronized">Optional annotations</p>
    "! @parameter meta        | <p class="shorttext synchronized">Optional metadata</p>
    METHODS add_image_content
      IMPORTING !data       TYPE string
                mime_type   TYPE string
                annotations TYPE annotations OPTIONAL
                meta        TYPE REF TO zif_mcp_ajson OPTIONAL.

    "! <p class="shorttext synchronized">Add Audio Content</p>
    "!
    "! @parameter data        | <p class="shorttext synchronized">Base64-encoded audio data</p>
    "! @parameter mime_type   | <p class="shorttext synchronized">MIME type of the audio file</p>
    "! @parameter annotations | <p class="shorttext synchronized">Optional annotations</p>
    "! @parameter meta        | <p class="shorttext synchronized">Optional metadata</p>
    METHODS add_audio_content
      IMPORTING !data       TYPE string
                mime_type   TYPE string
                annotations TYPE annotations OPTIONAL
                meta        TYPE REF TO zif_mcp_ajson OPTIONAL.

    "! <p class="shorttext synchronized">Add Resource Link</p>
    "!
    "! @parameter uri         | <p class="shorttext synchronized">Resource URI</p>
    "! @parameter name        | <p class="shorttext synchronized">Resource name</p>
    "! @parameter title       | <p class="shorttext synchronized">Optional human-readable title</p>
    "! @parameter description | <p class="shorttext synchronized">Optional description</p>
    "! @parameter mime_type   | <p class="shorttext synchronized">Optional MIME type</p>
    "! @parameter size        | <p class="shorttext synchronized">Optional size in bytes</p>
    "! @parameter annotations | <p class="shorttext synchronized">Optional annotations</p>
    "! @parameter meta        | <p class="shorttext synchronized">Optional metadata</p>
    METHODS add_resource_link
      IMPORTING uri         TYPE string
                name        TYPE string
                title       TYPE string      OPTIONAL
                description TYPE string      OPTIONAL
                mime_type   TYPE string      OPTIONAL
                size        TYPE i           OPTIONAL
                annotations TYPE annotations OPTIONAL
                meta        TYPE REF TO zif_mcp_ajson OPTIONAL.

    "! <p class="shorttext synchronized">Add Text Resource</p>
    "!
    "! @parameter uri         | <p class="shorttext synchronized">Resource URI</p>
    "! @parameter text        | <p class="shorttext synchronized">Text content</p>
    "! @parameter mime_type   | <p class="shorttext synchronized">Optional MIME type</p>
    "! @parameter annotations | <p class="shorttext synchronized">Optional annotations</p>
    "! @parameter meta        | <p class="shorttext synchronized">Optional metadata</p>
    METHODS add_text_resource
      IMPORTING uri         TYPE string
                !text       TYPE string
                mime_type   TYPE string      OPTIONAL
                annotations TYPE annotations OPTIONAL
                meta        TYPE REF TO zif_mcp_ajson OPTIONAL.

    "! <p class="shorttext synchronized">Add Blob Resource</p>
    "!
    "! @parameter uri         | <p class="shorttext synchronized">Resource URI</p>
    "! @parameter blob        | <p class="shorttext synchronized">Base64-encoded blob data</p>
    "! @parameter mime_type   | <p class="shorttext synchronized">Optional MIME type</p>
    "! @parameter annotations | <p class="shorttext synchronized">Optional annotations</p>
    "! @parameter meta        | <p class="shorttext synchronized">Optional metadata</p>
    METHODS add_blob_resource
      IMPORTING uri         TYPE string
                !blob       TYPE string
                mime_type   TYPE string      OPTIONAL
                annotations TYPE annotations OPTIONAL
                meta        TYPE REF TO zif_mcp_ajson OPTIONAL.

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
                 text          TYPE string VALUE 'text',
                 image         TYPE string VALUE 'image',
                 resource      TYPE string VALUE 'resource',
                 resource_link TYPE string VALUE 'resource_link',
                 audio         TYPE string VALUE 'audio',
               END OF content_type.

    " Simple content structures that don't use references
    TYPES: BEGIN OF content_item,
             type            TYPE string,
             text            TYPE string,      " For text content
             image_data      TYPE string,      " For image content
             image_mime      TYPE string,      " For image content
             audio_data      TYPE string,      " For audio content
             audio_mime      TYPE string,      " For audio content
             res_uri         TYPE string,      " For resource content
             res_text        TYPE string,      " For text resource content
             res_mime        TYPE string,      " For resource content
             res_blob        TYPE string,      " For blob resource content
             res_is_blob     TYPE abap_bool,   " Flag for blob vs text
             " Resource link fields
             res_name        TYPE string,
             res_title       TYPE string,
             res_description TYPE string,
             res_size        TYPE i,
             annotations     TYPE annotations, " For any content
             meta            TYPE REF TO zif_mcp_ajson, " For any content
           END OF content_item.

    " Table for all content, maintaining original order
    DATA content_items      TYPE STANDARD TABLE OF content_item WITH DEFAULT KEY.
    DATA meta               TYPE REF TO zif_mcp_ajson.
    DATA structured_content TYPE REF TO zif_mcp_ajson.
    DATA has_error          TYPE abap_bool.

    METHODS add_annotations_to_json
      IMPORTING !path       TYPE string
                annotations TYPE annotations
                ajson       TYPE REF TO zif_mcp_ajson
      RAISING   zcx_mcp_ajson_error.

    METHODS add_meta_to_json
      IMPORTING !path TYPE string
                meta  TYPE REF TO zif_mcp_ajson
                ajson TYPE REF TO zif_mcp_ajson
      RAISING   zcx_mcp_ajson_error.

    METHODS convert_timestamp_to_iso8601
      IMPORTING timestamp     TYPE timestamp
      RETURNING VALUE(result) TYPE string.
ENDCLASS.

CLASS zcl_mcp_resp_call_tool IMPLEMENTATION.
  METHOD zif_mcp_internal~generate_json.
    FIELD-SYMBOLS <item> LIKE LINE OF content_items.
      DATA content_index LIKE sy-tabix.
      DATA content_path TYPE string.
    result = zcl_mcp_ajson=>create_empty( ).

    " Create content array
    result->touch_array( '/content' ).

    " Add all content items in original order
    
    LOOP AT content_items ASSIGNING <item> WHERE type IS NOT INITIAL.
      
      content_index = sy-tabix.
      
      content_path = |/content/{ content_index }|.

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

        WHEN content_type-resource_link.
          " Resource link content
          result->set( iv_path         = |{ content_path }/uri|
                       iv_val          = <item>-res_uri
                       iv_ignore_empty = abap_false ).

          result->set( iv_path         = |{ content_path }/name|
                       iv_val          = <item>-res_name
                       iv_ignore_empty = abap_false ).

          IF <item>-res_title IS NOT INITIAL.
            result->set( iv_path = |{ content_path }/title|
                         iv_val  = <item>-res_title ).
          ENDIF.

          IF <item>-res_description IS NOT INITIAL.
            result->set( iv_path = |{ content_path }/description|
                         iv_val  = <item>-res_description ).
          ENDIF.

          IF <item>-res_mime IS NOT INITIAL.
            result->set( iv_path = |{ content_path }/mimeType|
                         iv_val  = <item>-res_mime ).
          ENDIF.

          IF <item>-res_size > 0.
            result->set( iv_path = |{ content_path }/size|
                         iv_val  = <item>-res_size ).
          ENDIF.

        WHEN content_type-resource.
          " Embedded resource content
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

          " Add resource meta if present
          IF <item>-meta IS BOUND.
            result->set( iv_path = |{ content_path }/resource/_meta|
                         iv_val  = <item>-meta ).
          ENDIF.
      ENDCASE.

      " Add annotations for any content type
      add_annotations_to_json( path        = content_path
                               annotations = <item>-annotations
                               ajson       = result ).

      " Add content meta if present (except for embedded resources where it's handled above)
      IF <item>-type <> content_type-resource.
        add_meta_to_json( path  = content_path
                          meta  = <item>-meta
                          ajson = result ).
      ENDIF.
    ENDLOOP.

    " Add structured content if present
    IF structured_content IS BOUND.
      result->set( iv_path = '/structuredContent'
                   iv_val  = structured_content ).
    ENDIF.

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
      FIELD-SYMBOLS <audience> LIKE LINE OF annotations-audience.
      DATA iso_timestamp TYPE string.
    " Add annotations if not empty
    IF     annotations-audience      IS INITIAL
       AND annotations-priority      IS INITIAL
       AND annotations-last_modified IS INITIAL.
      RETURN.
    ENDIF.

    " Add audience array if not empty
    IF annotations-audience IS NOT INITIAL.
      ajson->touch_array( |{ path }/annotations/audience| ).

      
      LOOP AT annotations-audience ASSIGNING <audience>.
        ajson->set( iv_path = |{ path }/annotations/audience/{ sy-tabix }|
                    iv_val  = <audience> ).
      ENDLOOP.
    ENDIF.

    " Add priority if not empty
    IF annotations-priority IS NOT INITIAL.
      ajson->set( iv_path = |{ path }/annotations/priority|
                  iv_val  = annotations-priority ).
    ENDIF.

    " Add lastModified if not empty
    IF annotations-last_modified IS NOT INITIAL.
      
      iso_timestamp = convert_timestamp_to_iso8601( annotations-last_modified ).
      ajson->set( iv_path = |{ path }/annotations/lastModified|
                  iv_val  = iso_timestamp ).
    ENDIF.
  ENDMETHOD.

  METHOD add_meta_to_json.
    " Add meta if present
    IF meta IS BOUND.
      ajson->set( iv_path = |{ path }/_meta|
                  iv_val  = meta ).
    ENDIF.
  ENDMETHOD.

  METHOD convert_timestamp_to_iso8601.
    " Convert session timestamp to UTC and format as ISO 8601
    DATA local_date       TYPE sy-datum.
    DATA local_time       TYPE sy-uzeit.
    DATA utc_timestamp    TYPE timestamp.
    DATA timestamp_string TYPE string.

    " Convert timestamp to string first
    timestamp_string = |{ timestamp }|.

    " Pad with leading zeros if needed
    WHILE strlen( timestamp_string ) < 14.
      timestamp_string = |0{ timestamp_string }|.
    ENDWHILE.

    " Extract date and time from timestamp string
    local_date = timestamp_string+0(8).
    local_time = timestamp_string+8(6).

    " Convert local date/time to UTC timestamp
    CONVERT DATE local_date TIME local_time INTO TIME STAMP utc_timestamp TIME ZONE sy-zonlo.

    " Convert UTC timestamp back to string for formatting
    timestamp_string = |{ utc_timestamp }|.
    WHILE strlen( timestamp_string ) < 14.
      timestamp_string = |0{ timestamp_string }|.
    ENDWHILE.

    " Format: YYYYMMDDHHMMSS -> YYYY-MM-DDTHH:MM:SSZ
    result = |{ timestamp_string+0(4) }-{ timestamp_string+4(2) }-{ timestamp_string+6(2) }T{ timestamp_string+8(2) }:{ timestamp_string+10(2) }:{ timestamp_string+12(2) }Z|.
  ENDMETHOD.

  METHOD set_error.
    me->has_error = has_error.
  ENDMETHOD.

  METHOD set_structured_content.
          DATA text_repr TYPE string.
    me->structured_content = structured_content.

    " Auto-add text representation if requested and structured content is provided
    IF add_text_content = abap_true AND structured_content IS BOUND.
      TRY.
          " Convert structured content to string for text representation
          
          text_repr = structured_content->stringify( ).
          add_text_content( text_repr ).
        CATCH zcx_mcp_ajson_error.
          " If conversion fails, add a generic message
          add_text_content( 'Structured content provided' ) ##NO_TEXT.
      ENDTRY.
    ENDIF.
  ENDMETHOD.

  METHOD add_text_content.
    DATA temp1 TYPE content_item.
    CLEAR temp1.
    temp1-type = content_type-text.
    temp1-text = text.
    temp1-annotations = annotations.
    temp1-meta = meta.
    APPEND temp1
           TO content_items.
  ENDMETHOD.

  METHOD add_image_content.
    DATA temp2 TYPE content_item.
    CLEAR temp2.
    temp2-type = content_type-image.
    temp2-image_data = data.
    temp2-image_mime = mime_type.
    temp2-annotations = annotations.
    temp2-meta = meta.
    APPEND temp2
           TO content_items.
  ENDMETHOD.

  METHOD add_audio_content.
    DATA temp3 TYPE content_item.
    CLEAR temp3.
    temp3-type = content_type-audio.
    temp3-audio_data = data.
    temp3-audio_mime = mime_type.
    temp3-annotations = annotations.
    temp3-meta = meta.
    APPEND temp3
           TO content_items.
  ENDMETHOD.

  METHOD add_resource_link.
    DATA temp4 TYPE content_item.
    CLEAR temp4.
    temp4-type = content_type-resource_link.
    temp4-res_uri = uri.
    temp4-res_name = name.
    temp4-res_title = title.
    temp4-res_description = description.
    temp4-res_mime = mime_type.
    temp4-res_size = size.
    temp4-annotations = annotations.
    temp4-meta = meta.
    APPEND temp4
           TO content_items.
  ENDMETHOD.

  METHOD add_text_resource.
    DATA temp5 TYPE content_item.
    CLEAR temp5.
    temp5-type = content_type-resource.
    temp5-res_uri = uri.
    temp5-res_text = text.
    temp5-res_mime = mime_type.
    temp5-res_is_blob = abap_false.
    temp5-annotations = annotations.
    temp5-meta = meta.
    APPEND temp5
           TO content_items.
  ENDMETHOD.

  METHOD add_blob_resource.
    DATA temp6 TYPE content_item.
    CLEAR temp6.
    temp6-type = content_type-resource.
    temp6-res_uri = uri.
    temp6-res_blob = blob.
    temp6-res_mime = mime_type.
    temp6-res_is_blob = abap_true.
    temp6-annotations = annotations.
    temp6-meta = meta.
    APPEND temp6
           TO content_items.
  ENDMETHOD.

  METHOD set_content.
    FIELD-SYMBOLS <content_wrapper> LIKE LINE OF content.
          DATA temp7 TYPE REF TO text_content.
          DATA text_item LIKE temp7.
          DATA temp8 TYPE content_item.
          DATA temp9 TYPE REF TO image_content.
          DATA image_item LIKE temp9.
          DATA temp10 TYPE content_item.
          DATA temp11 TYPE REF TO audio_content.
          DATA audio_item LIKE temp11.
          DATA temp12 TYPE content_item.
          DATA temp13 TYPE REF TO resource_link.
          DATA link_item LIKE temp13.
          DATA temp14 TYPE content_item.
          DATA temp15 TYPE REF TO embedded_resource.
          DATA resource_item LIKE temp15.
              DATA temp16 TYPE REF TO text_resource_contents.
              DATA text_resource LIKE temp16.
              DATA temp17 TYPE content_item.
                  DATA temp18 TYPE REF TO blob_resource_contents.
                  DATA blob_resource LIKE temp18.
                  DATA temp19 TYPE content_item.
    " Clear existing content
    CLEAR content_items.

    " Translate complex structure to simpler internal structure
    
    LOOP AT content ASSIGNING <content_wrapper>.
      CASE <content_wrapper>-type.
        WHEN content_type-text.
          
          temp7 ?= <content_wrapper>-content.
          
          text_item = temp7.

          
          CLEAR temp8.
          temp8-type = content_type-text.
          temp8-text = text_item->text.
          temp8-annotations = text_item->annotations.
          temp8-meta = text_item->meta.
          APPEND temp8
                 TO content_items.

        WHEN content_type-image.
          
          temp9 ?= <content_wrapper>-content.
          
          image_item = temp9.

          
          CLEAR temp10.
          temp10-type = content_type-image.
          temp10-image_data = image_item->data.
          temp10-image_mime = image_item->mime_type.
          temp10-annotations = image_item->annotations.
          temp10-meta = image_item->meta.
          APPEND temp10
                 TO content_items.

        WHEN content_type-audio.
          
          temp11 ?= <content_wrapper>-content.
          
          audio_item = temp11.

          
          CLEAR temp12.
          temp12-type = content_type-audio.
          temp12-audio_data = audio_item->data.
          temp12-audio_mime = audio_item->mime_type.
          temp12-annotations = audio_item->annotations.
          temp12-meta = audio_item->meta.
          APPEND temp12
                 TO content_items.

        WHEN content_type-resource_link.
          
          temp13 ?= <content_wrapper>-content.
          
          link_item = temp13.

          
          CLEAR temp14.
          temp14-type = content_type-resource_link.
          temp14-res_uri = link_item->uri.
          temp14-res_name = link_item->name.
          temp14-res_title = link_item->title.
          temp14-res_description = link_item->description.
          temp14-res_mime = link_item->mime_type.
          temp14-res_size = link_item->size.
          temp14-annotations = link_item->annotations.
          temp14-meta = link_item->meta.
          APPEND temp14
                 TO content_items.

        WHEN content_type-resource.
          
          temp15 ?= <content_wrapper>-content.
          
          resource_item = temp15.

          " Try to determine if it's a text or blob resource
          TRY.
              " Try as text resource first
              
              temp16 ?= resource_item->resource.
              
              text_resource = temp16.

              
              CLEAR temp17.
              temp17-type = content_type-resource.
              temp17-res_uri = text_resource->uri.
              temp17-res_text = text_resource->text.
              temp17-res_mime = text_resource->mime_type.
              temp17-res_is_blob = abap_false.
              temp17-annotations = resource_item->annotations.
              temp17-meta = text_resource->meta.
              APPEND temp17
                     TO content_items.

            CATCH cx_sy_move_cast_error.
              " Must be a blob resource
              TRY.
                  
                  temp18 ?= resource_item->resource.
                  
                  blob_resource = temp18.

                  
                  CLEAR temp19.
                  temp19-type = content_type-resource.
                  temp19-res_uri = blob_resource->uri.
                  temp19-res_blob = blob_resource->blob.
                  temp19-res_mime = blob_resource->mime_type.
                  temp19-res_is_blob = abap_true.
                  temp19-annotations = resource_item->annotations.
                  temp19-meta = blob_resource->meta.
                  APPEND temp19
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

ENDCLASS.
