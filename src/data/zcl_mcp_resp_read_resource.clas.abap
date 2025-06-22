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

    TYPES resource_contents TYPE STANDARD TABLE OF resource_content_wrapper WITH DEFAULT KEY.

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
    DATA text_resources      TYPE STANDARD TABLE OF text_resource_entry WITH DEFAULT KEY.
    DATA blob_resources      TYPE STANDARD TABLE OF blob_resource_entry WITH DEFAULT KEY.
    DATA resource_order      TYPE STANDARD TABLE OF resource_order_entry WITH DEFAULT KEY.
    DATA meta                TYPE REF TO zif_mcp_ajson.
ENDCLASS.

CLASS zcl_mcp_resp_read_resource IMPLEMENTATION.
  METHOD zif_mcp_internal~generate_json.
    DATA content_index TYPE i VALUE 0.
    FIELD-SYMBOLS <order> LIKE LINE OF resource_order.
      DATA content_path TYPE string.
          DATA text_idx LIKE <order>-index.
          FIELD-SYMBOLS <text> TYPE zcl_mcp_resp_read_resource=>text_resource_entry.
          DATA blob_idx LIKE <order>-index.
          FIELD-SYMBOLS <blob> TYPE zcl_mcp_resp_read_resource=>blob_resource_entry.
    result = zcl_mcp_ajson=>create_empty( ).

    " Create contents array
    result->touch_array( '/contents' ).

    " Process resources in original order
    

    
    LOOP AT resource_order ASSIGNING <order>.
      content_index = content_index + 1.
      
      content_path = |/contents/{ content_index }|.

      CASE <order>-type.
        WHEN resource_type-text.
          
          text_idx = <order>-index.
          
          READ TABLE text_resources INDEX text_idx ASSIGNING <text>.
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
          
          blob_idx = <order>-index.
          
          READ TABLE blob_resources INDEX blob_idx ASSIGNING <blob>.
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
    DATA temp1 TYPE text_resource_entry.
    DATA temp2 TYPE resource_order_entry.
    CLEAR temp1.
    temp1-uri = uri.
    temp1-text = text.
    temp1-mime_type = mime_type.
    temp1-meta = meta.
    APPEND temp1 TO text_resources.

    " Track order
    
    CLEAR temp2.
    temp2-type = resource_type-text.
    temp2-index = lines( text_resources ).
    APPEND temp2 TO resource_order.
  ENDMETHOD.

  METHOD add_blob_resource.
    DATA temp3 TYPE blob_resource_entry.
    DATA temp4 TYPE resource_order_entry.
    CLEAR temp3.
    temp3-uri = uri.
    temp3-blob = blob.
    temp3-mime_type = mime_type.
    temp3-meta = meta.
    APPEND temp3 TO blob_resources.

    " Track order
    
    CLEAR temp4.
    temp4-type = resource_type-blob.
    temp4-index = lines( blob_resources ).
    APPEND temp4 TO resource_order.
  ENDMETHOD.

  METHOD set_contents.
    FIELD-SYMBOLS <content> LIKE LINE OF contents.
              DATA temp5 TYPE REF TO text_resource_contents.
              DATA text_resource LIKE temp5.
              DATA temp6 TYPE text_resource_entry.
              DATA temp7 TYPE resource_order_entry.
              DATA temp8 TYPE REF TO blob_resource_contents.
              DATA blob_resource LIKE temp8.
              DATA temp9 TYPE blob_resource_entry.
              DATA temp10 TYPE resource_order_entry.
    " Clear existing content
    CLEAR: text_resources, blob_resources, resource_order.

    " Process each content item and convert to simplified storage
    
    LOOP AT contents ASSIGNING <content>.
      " Skip if resource is not bound
      IF <content>-resource IS NOT BOUND.
        CONTINUE.
      ENDIF.

      " Process based on the content type
      CASE <content>-type.
        WHEN content_type-text.
          " Handle text resource
          TRY.
              
              temp5 ?= <content>-resource.
              
              text_resource = temp5.

              
              CLEAR temp6.
              temp6-uri = text_resource->uri.
              temp6-text = text_resource->text.
              temp6-mime_type = text_resource->mime_type.
              temp6-meta = text_resource->meta.
              APPEND temp6 TO text_resources.

              " Track order
              
              CLEAR temp7.
              temp7-type = resource_type-text.
              temp7-index = lines( text_resources ).
              APPEND temp7 TO resource_order.

            CATCH cx_sy_move_cast_error.
              " Skip if type doesn't match expected structure
              CONTINUE.
          ENDTRY.

        WHEN content_type-blob.
          " Handle blob resource
          TRY.
              
              temp8 ?= <content>-resource.
              
              blob_resource = temp8.

              
              CLEAR temp9.
              temp9-uri = blob_resource->uri.
              temp9-blob = blob_resource->blob.
              temp9-mime_type = blob_resource->mime_type.
              temp9-meta = blob_resource->meta.
              APPEND temp9 TO blob_resources.

              " Track order
              
              CLEAR temp10.
              temp10-type = resource_type-blob.
              temp10-index = lines( blob_resources ).
              APPEND temp10 TO resource_order.

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
