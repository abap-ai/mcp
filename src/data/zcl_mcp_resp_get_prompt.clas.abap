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
    " Content type identifiers
    CONSTANTS: BEGIN OF content_type,
                 text     TYPE string VALUE 'text',
                 image    TYPE string VALUE 'image',
                 resource TYPE string VALUE 'resource',
               END OF content_type.

    " Message type enum
    CONSTANTS: BEGIN OF message_type,
                 text          TYPE i VALUE 1,
                 image         TYPE i VALUE 2,
                 text_resource TYPE i VALUE 3,
                 blob_resource TYPE i VALUE 4,
               END OF message_type.

    " Simplified message structures without references
    TYPES: BEGIN OF text_message_entry,
             role        TYPE string,
             text        TYPE string,
             annotations TYPE annotations,
           END OF text_message_entry.

    TYPES: BEGIN OF image_message_entry,
             role        TYPE string,
             data        TYPE string,
             mime_type   TYPE string,
             annotations TYPE annotations,
           END OF image_message_entry.

    TYPES: BEGIN OF text_resource_message_entry,
             role        TYPE string,
             uri         TYPE string,
             text        TYPE string,
             mime_type   TYPE string,
             annotations TYPE annotations,
           END OF text_resource_message_entry.

    TYPES: BEGIN OF blob_resource_message_entry,
             role        TYPE string,
             uri         TYPE string,
             blob        TYPE string,
             mime_type   TYPE string,
             annotations TYPE annotations,
           END OF blob_resource_message_entry.

    " Message order tracking
    TYPES: BEGIN OF message_order_entry,
             type  TYPE i,   " Message type (see constants)
             index TYPE i,   " Index in the corresponding table
           END OF message_order_entry.

    " Internal data storage
    DATA description         TYPE string.
    DATA text_messages       TYPE STANDARD TABLE OF text_message_entry WITH EMPTY KEY.
    DATA image_messages      TYPE STANDARD TABLE OF image_message_entry WITH EMPTY KEY.
    DATA text_resources      TYPE STANDARD TABLE OF text_resource_message_entry WITH EMPTY KEY.
    DATA blob_resources      TYPE STANDARD TABLE OF blob_resource_message_entry WITH EMPTY KEY.
    DATA message_order       TYPE STANDARD TABLE OF message_order_entry WITH EMPTY KEY.
    DATA meta                TYPE REF TO zif_mcp_ajson.

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
    IF description IS NOT INITIAL.
      result->set( iv_path = '/description'
                   iv_val  = description ).
    ENDIF.

    " Create messages array
    result->touch_array( '/messages' ).

    " Process messages in the original order they were added
    DATA message_index TYPE i VALUE 0.

    LOOP AT message_order ASSIGNING FIELD-SYMBOL(<order>).
      message_index = message_index + 1.
      DATA(message_path) = |/messages/{ message_index }|.

      CASE <order>-type.
        WHEN message_type-text.
          DATA(text_idx) = <order>-index.
          READ TABLE text_messages INDEX text_idx ASSIGNING FIELD-SYMBOL(<text>).
          IF sy-subrc = 0.
            " Add role
            result->set( iv_path         = |{ message_path }/role|
                         iv_val          = <text>-role
                         iv_ignore_empty = abap_false ).

            " Add content
            result->set( iv_path         = |{ message_path }/content/type|
                         iv_val          = content_type-text
                         iv_ignore_empty = abap_false ).

            result->set( iv_path         = |{ message_path }/content/text|
                         iv_val          = <text>-text
                         iv_ignore_empty = abap_false ).

            " Add annotations if present
            add_annotations_to_json( path        = |{ message_path }/content|
                                     annotations = <text>-annotations
                                     result      = result ).
          ENDIF.

        WHEN message_type-image.
          DATA(image_idx) = <order>-index.
          READ TABLE image_messages INDEX image_idx ASSIGNING FIELD-SYMBOL(<image>).
          IF sy-subrc = 0.
            " Add role
            result->set( iv_path         = |{ message_path }/role|
                         iv_val          = <image>-role
                         iv_ignore_empty = abap_false ).

            " Add content
            result->set( iv_path         = |{ message_path }/content/type|
                         iv_val          = content_type-image
                         iv_ignore_empty = abap_false ).

            result->set( iv_path         = |{ message_path }/content/data|
                         iv_val          = <image>-data
                         iv_ignore_empty = abap_false ).

            result->set( iv_path         = |{ message_path }/content/mimeType|
                         iv_val          = <image>-mime_type
                         iv_ignore_empty = abap_false ).

            " Add annotations if present
            add_annotations_to_json( path        = |{ message_path }/content|
                                     annotations = <image>-annotations
                                     result      = result ).
          ENDIF.

        WHEN message_type-text_resource.
          DATA(text_res_idx) = <order>-index.
          READ TABLE text_resources INDEX text_res_idx ASSIGNING FIELD-SYMBOL(<text_res>).
          IF sy-subrc = 0.
            " Add role
            result->set( iv_path         = |{ message_path }/role|
                         iv_val          = <text_res>-role
                         iv_ignore_empty = abap_false ).

            " Add content type
            result->set( iv_path         = |{ message_path }/content/type|
                         iv_val          = content_type-resource
                         iv_ignore_empty = abap_false ).

            " Add resource details
            result->set( iv_path         = |{ message_path }/content/resource/uri|
                         iv_val          = <text_res>-uri
                         iv_ignore_empty = abap_false ).

            result->set( iv_path         = |{ message_path }/content/resource/text|
                         iv_val          = <text_res>-text
                         iv_ignore_empty = abap_false ).

            IF <text_res>-mime_type IS NOT INITIAL.
              result->set( iv_path = |{ message_path }/content/resource/mimeType|
                           iv_val  = <text_res>-mime_type ).
            ENDIF.

            " Add annotations if present
            add_annotations_to_json( path        = |{ message_path }/content|
                                     annotations = <text_res>-annotations
                                     result      = result ).
          ENDIF.

        WHEN message_type-blob_resource.
          DATA(blob_res_idx) = <order>-index.
          READ TABLE blob_resources INDEX blob_res_idx ASSIGNING FIELD-SYMBOL(<blob_res>).
          IF sy-subrc = 0.
            " Add role
            result->set( iv_path         = |{ message_path }/role|
                         iv_val          = <blob_res>-role
                         iv_ignore_empty = abap_false ).

            " Add content type
            result->set( iv_path         = |{ message_path }/content/type|
                         iv_val          = content_type-resource
                         iv_ignore_empty = abap_false ).

            " Add resource details
            result->set( iv_path         = |{ message_path }/content/resource/uri|
                         iv_val          = <blob_res>-uri
                         iv_ignore_empty = abap_false ).

            result->set( iv_path         = |{ message_path }/content/resource/blob|
                         iv_val          = <blob_res>-blob
                         iv_ignore_empty = abap_false ).

            IF <blob_res>-mime_type IS NOT INITIAL.
              result->set( iv_path = |{ message_path }/content/resource/mimeType|
                           iv_val  = <blob_res>-mime_type ).
            ENDIF.

            " Add annotations if present
            add_annotations_to_json( path        = |{ message_path }/content|
                                     annotations = <blob_res>-annotations
                                     result      = result ).
          ENDIF.
      ENDCASE.
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
    CLEAR: text_messages, image_messages, text_resources, blob_resources, message_order.

    " Process each message and convert to simplified storage
    LOOP AT messages ASSIGNING FIELD-SYMBOL(<message>).
      " Skip if content is not bound
      IF <message>-content IS NOT BOUND.
        CONTINUE.
      ENDIF.

      " Try to determine the content type and process accordingly
      TRY.
          " First try as text content
          DATA(text_content) = CAST text_content( <message>-content ).

          APPEND VALUE text_message_entry(
            role        = <message>-role
            text        = text_content->text
            annotations = text_content->annotations
          ) TO text_messages.

          " Track order
          APPEND VALUE message_order_entry(
            type  = message_type-text
            index = lines( text_messages )
          ) TO message_order.

        CATCH cx_sy_move_cast_error.
          TRY.
              " Try as image content
              DATA(image_content) = CAST image_content( <message>-content ).

              APPEND VALUE image_message_entry(
                role        = <message>-role
                data        = image_content->data
                mime_type   = image_content->mime_type
                annotations = image_content->annotations
              ) TO image_messages.

              " Track order
              APPEND VALUE message_order_entry(
                type  = message_type-image
                index = lines( image_messages )
              ) TO message_order.

            CATCH cx_sy_move_cast_error.
              TRY.
                  " Try as resource content
                  DATA(resource_content) = CAST embedded_resource( <message>-content ).

                  IF resource_content->resource IS BOUND.
                    TRY.
                        " Try as text resource
                        DATA(text_resource) = CAST text_resource_contents( resource_content->resource ).

                        APPEND VALUE text_resource_message_entry(
                          role        = <message>-role
                          uri         = text_resource->uri
                          text        = text_resource->text
                          mime_type   = text_resource->mime_type
                          annotations = resource_content->annotations
                        ) TO text_resources.

                        " Track order
                        APPEND VALUE message_order_entry(
                          type  = message_type-text_resource
                          index = lines( text_resources )
                        ) TO message_order.

                      CATCH cx_sy_move_cast_error.
                        TRY.
                            " Try as blob resource
                            DATA(blob_resource) = CAST blob_resource_contents( resource_content->resource ).

                            APPEND VALUE blob_resource_message_entry(
                              role        = <message>-role
                              uri         = blob_resource->uri
                              blob        = blob_resource->blob
                              mime_type   = blob_resource->mime_type
                              annotations = resource_content->annotations
                            ) TO blob_resources.

                            " Track order
                            APPEND VALUE message_order_entry(
                              type  = message_type-blob_resource
                              index = lines( blob_resources )
                            ) TO message_order.

                          CATCH cx_sy_move_cast_error.
                            " Unknown resource type - skip
                            CONTINUE.
                        ENDTRY.
                    ENDTRY.
                  ENDIF.

                CATCH cx_sy_move_cast_error.
                  " Unknown content type - skip
                  CONTINUE.
              ENDTRY.
          ENDTRY.
      ENDTRY.
    ENDLOOP.
  ENDMETHOD.

  METHOD add_text_message.
    APPEND VALUE text_message_entry(
      role        = role
      text        = text
      annotations = annotations
    ) TO text_messages.

    " Track order
    APPEND VALUE message_order_entry(
      type  = message_type-text
      index = lines( text_messages )
    ) TO message_order.
  ENDMETHOD.

  METHOD add_image_message.
    APPEND VALUE image_message_entry(
      role        = role
      data        = data
      mime_type   = mime_type
      annotations = annotations
    ) TO image_messages.

    " Track order
    APPEND VALUE message_order_entry(
      type  = message_type-image
      index = lines( image_messages )
    ) TO message_order.
  ENDMETHOD.

  METHOD add_text_resource_message.
    APPEND VALUE text_resource_message_entry(
      role        = role
      uri         = uri
      text        = text
      mime_type   = mime_type
      annotations = annotations
    ) TO text_resources.

    " Track order
    APPEND VALUE message_order_entry(
      type  = message_type-text_resource
      index = lines( text_resources )
    ) TO message_order.
  ENDMETHOD.

  METHOD add_blob_resource_message.
    APPEND VALUE blob_resource_message_entry(
      role        = role
      uri         = uri
      blob        = blob
      mime_type   = mime_type
      annotations = annotations
    ) TO blob_resources.

    " Track order
    APPEND VALUE message_order_entry(
      type  = message_type-blob_resource
      index = lines( blob_resources )
    ) TO message_order.
  ENDMETHOD.

  METHOD set_meta.
    me->meta = meta.
  ENDMETHOD.
ENDCLASS.
