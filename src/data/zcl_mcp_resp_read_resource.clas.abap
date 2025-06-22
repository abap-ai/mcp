"! <p class="shorttext synchronized">MCP Read Resource Result</p>
"! Implementation for the ReadResourceResult interface
CLASS zcl_mcp_resp_read_resource DEFINITION
  PUBLIC FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES zif_mcp_internal.

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

    TYPES: BEGIN OF resource_content_wrapper,
             type     TYPE string,
             resource TYPE REF TO data,
           END OF resource_content_wrapper.

    TYPES resource_contents TYPE STANDARD TABLE OF resource_content_wrapper WITH EMPTY KEY.

    "! <p class="shorttext synchronized">Add Text Resource</p>
    "!
    "! @parameter uri       | <p class="shorttext synchronized">URI of the resource</p>
    "! @parameter text      | <p class="shorttext synchronized">Text content</p>
    "! @parameter mime_type | <p class="shorttext synchronized">Optional MIME type</p>
    "! @parameter meta      | <p class="shorttext synchronized">Optional metadata</p>
    METHODS add_text_resource
      IMPORTING uri       TYPE string
                !text     TYPE string
                mime_type TYPE string OPTIONAL
                meta      TYPE REF TO zif_mcp_ajson OPTIONAL.

    "! <p class="shorttext synchronized">Add Blob Resource</p>
    "!
    "! @parameter uri       | <p class="shorttext synchronized">URI of the resource</p>
    "! @parameter blob      | <p class="shorttext synchronized">Base64-encoded blob data</p>
    "! @parameter mime_type | <p class="shorttext synchronized">Optional MIME type</p>
    "! @parameter meta      | <p class="shorttext synchronized">Optional metadata</p>
    METHODS add_blob_resource
      IMPORTING uri       TYPE string
                !blob     TYPE string
                mime_type TYPE string OPTIONAL
                meta      TYPE REF TO zif_mcp_ajson OPTIONAL.

    "! <p class="shorttext synchronized">Set Resource Contents</p>
    "!
    "! @parameter contents | <p class="shorttext synchronized">List of resource contents</p>
    METHODS set_contents
      IMPORTING contents TYPE resource_contents.

    "! <p class="shorttext synchronized">Set Meta Data</p>
    "! Optional metadata to attach to response
    "!
    "! @parameter meta | <p class="shorttext synchronized">Meta data</p>
    METHODS set_meta
      IMPORTING meta TYPE REF TO zif_mcp_ajson.

  PRIVATE SECTION.
    " Content type identifiers
    CONSTANTS: BEGIN OF content_type,
                 text TYPE string VALUE 'text',
                 blob TYPE string VALUE 'blob',
               END OF content_type.

    " Resource type enum for tracking
    CONSTANTS: BEGIN OF resource_type,
                 text TYPE i VALUE 1,
                 blob TYPE i VALUE 2,
               END OF resource_type.

    " Simplified resource structures without references
    TYPES: BEGIN OF text_resource_entry,
             uri       TYPE string,
             text      TYPE string,
             mime_type TYPE string,
             meta      TYPE REF TO zif_mcp_ajson,
           END OF text_resource_entry.

    TYPES: BEGIN OF blob_resource_entry,
             uri       TYPE string,
             blob      TYPE string,
             mime_type TYPE string,
             meta      TYPE REF TO zif_mcp_ajson,
           END OF blob_resource_entry.

    " Resource order tracking
    TYPES: BEGIN OF resource_order_entry,
             type  TYPE i,   " Resource type (see constants)
             index TYPE i,   " Index in the corresponding table
           END OF resource_order_entry.

    " Internal data storage
    DATA text_resources      TYPE STANDARD TABLE OF text_resource_entry WITH EMPTY KEY.
    DATA blob_resources      TYPE STANDARD TABLE OF blob_resource_entry WITH EMPTY KEY.
    DATA resource_order      TYPE STANDARD TABLE OF resource_order_entry WITH EMPTY KEY.
    DATA meta                TYPE REF TO zif_mcp_ajson.
ENDCLASS.

CLASS zcl_mcp_resp_read_resource IMPLEMENTATION.
  METHOD zif_mcp_internal~generate_json.
    result = zcl_mcp_ajson=>create_empty( ).

    " Create contents array
    result->touch_array( '/contents' ).

    " Process resources in original order
    DATA content_index TYPE i VALUE 0.

    LOOP AT resource_order ASSIGNING FIELD-SYMBOL(<order>).
      content_index = content_index + 1.
      DATA(content_path) = |/contents/{ content_index }|.

      CASE <order>-type.
        WHEN resource_type-text.
          DATA(text_idx) = <order>-index.
          READ TABLE text_resources INDEX text_idx ASSIGNING FIELD-SYMBOL(<text>).
          IF sy-subrc = 0.
            " Set URI (required)
            result->set( iv_path         = |{ content_path }/uri|
                         iv_val          = <text>-uri
                         iv_ignore_empty = abap_false ).

            " Set MIME type if present
            IF <text>-mime_type IS NOT INITIAL.
              result->set( iv_path = |{ content_path }/mimeType|
                           iv_val  = <text>-mime_type ).
            ENDIF.

            " Set text content
            result->set( iv_path         = |{ content_path }/text|
                         iv_val          = <text>-text
                         iv_ignore_empty = abap_false ).

            " Set metadata if present
            IF <text>-meta IS BOUND.
              result->set( iv_path = |{ content_path }/_meta|
                           iv_val  = <text>-meta ).
            ENDIF.
          ENDIF.

        WHEN resource_type-blob.
          DATA(blob_idx) = <order>-index.
          READ TABLE blob_resources INDEX blob_idx ASSIGNING FIELD-SYMBOL(<blob>).
          IF sy-subrc = 0.
            " Set URI (required)
            result->set( iv_path         = |{ content_path }/uri|
                         iv_val          = <blob>-uri
                         iv_ignore_empty = abap_false ).

            " Set MIME type if present
            IF <blob>-mime_type IS NOT INITIAL.
              result->set( iv_path = |{ content_path }/mimeType|
                           iv_val  = <blob>-mime_type ).
            ENDIF.

            " Set blob content
            result->set( iv_path         = |{ content_path }/blob|
                         iv_val          = <blob>-blob
                         iv_ignore_empty = abap_false ).

            " Set metadata if present
            IF <blob>-meta IS BOUND.
              result->set( iv_path = |{ content_path }/_meta|
                           iv_val  = <blob>-meta ).
            ENDIF.
          ENDIF.
      ENDCASE.
    ENDLOOP.

    " Add response-level metadata (optional)
    IF meta IS BOUND.
      result->set( iv_path = '/_meta'
                   iv_val  = meta ).
    ENDIF.
  ENDMETHOD.

  METHOD add_text_resource.
    APPEND VALUE text_resource_entry(
      uri       = uri
      text      = text
      mime_type = mime_type
      meta      = meta
    ) TO text_resources.

    " Track order
    APPEND VALUE resource_order_entry(
      type  = resource_type-text
      index = lines( text_resources )
    ) TO resource_order.
  ENDMETHOD.

  METHOD add_blob_resource.
    APPEND VALUE blob_resource_entry(
      uri       = uri
      blob      = blob
      mime_type = mime_type
      meta      = meta
    ) TO blob_resources.

    " Track order
    APPEND VALUE resource_order_entry(
      type  = resource_type-blob
      index = lines( blob_resources )
    ) TO resource_order.
  ENDMETHOD.

  METHOD set_contents.
    " Clear existing content
    CLEAR: text_resources, blob_resources, resource_order.

    " Process each content item and convert to simplified storage
    LOOP AT contents ASSIGNING FIELD-SYMBOL(<content>).
      " Skip if resource is not bound
      IF <content>-resource IS NOT BOUND.
        CONTINUE.
      ENDIF.

      " Process based on the content type
      CASE <content>-type.
        WHEN content_type-text.
          " Handle text resource
          TRY.
              DATA(text_resource) = CAST text_resource_contents( <content>-resource ).

              APPEND VALUE text_resource_entry(
                uri       = text_resource->uri
                text      = text_resource->text
                mime_type = text_resource->mime_type
                meta      = text_resource->meta
              ) TO text_resources.

              " Track order
              APPEND VALUE resource_order_entry(
                type  = resource_type-text
                index = lines( text_resources )
              ) TO resource_order.

            CATCH cx_sy_move_cast_error.
              " Skip if type doesn't match expected structure
              CONTINUE.
          ENDTRY.

        WHEN content_type-blob.
          " Handle blob resource
          TRY.
              DATA(blob_resource) = CAST blob_resource_contents( <content>-resource ).

              APPEND VALUE blob_resource_entry(
                uri       = blob_resource->uri
                blob      = blob_resource->blob
                mime_type = blob_resource->mime_type
                meta      = blob_resource->meta
              ) TO blob_resources.

              " Track order
              APPEND VALUE resource_order_entry(
                type  = resource_type-blob
                index = lines( blob_resources )
              ) TO resource_order.

            CATCH cx_sy_move_cast_error.
              " Skip if type doesn't match expected structure
              CONTINUE.
          ENDTRY.

        WHEN OTHERS.
          " Unknown resource type - skip
          CONTINUE.
      ENDCASE.
    ENDLOOP.
  ENDMETHOD.

  METHOD set_meta.
    me->meta = meta.
  ENDMETHOD.
ENDCLASS.
