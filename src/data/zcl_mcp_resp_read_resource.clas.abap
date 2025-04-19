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
           END OF text_resource_contents.

    TYPES: BEGIN OF blob_resource_contents,
             uri       TYPE string,
             mime_type TYPE string,
             blob      TYPE string,
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
    METHODS add_text_resource
      IMPORTING uri       TYPE string
                !text     TYPE string
                mime_type TYPE string OPTIONAL.

    "! <p class="shorttext synchronized">Add Blob Resource</p>
    "!
    "! @parameter uri       | <p class="shorttext synchronized">URI of the resource</p>
    "! @parameter blob      | <p class="shorttext synchronized">Base64-encoded blob data</p>
    "! @parameter mime_type | <p class="shorttext synchronized">Optional MIME type</p>
    METHODS add_blob_resource
      IMPORTING uri       TYPE string
                !blob     TYPE string
                mime_type TYPE string OPTIONAL.

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
    DATA int_contents TYPE resource_contents.
    DATA int_meta TYPE REF TO zif_mcp_ajson.
ENDCLASS.

CLASS zcl_mcp_resp_read_resource IMPLEMENTATION.
  METHOD zif_mcp_internal~generate_json.
    FIELD-SYMBOLS <content> LIKE LINE OF int_contents.
      DATA content_index LIKE sy-tabix.
      DATA content_path TYPE string.
              DATA temp1 TYPE REF TO text_resource_contents.
              DATA text_resource LIKE temp1.
              DATA temp2 TYPE REF TO blob_resource_contents.
              DATA blob_resource LIKE temp2.
    result = zcl_mcp_ajson=>create_empty( ).

    " Create contents array
    result->touch_array( '/contents' ).

    " Add all resource contents
    
    LOOP AT int_contents ASSIGNING <content>.
      
      content_index = sy-tabix.
      
      content_path = |/contents/{ content_index }|.

      " Skip if resource is not bound
      IF <content>-resource IS NOT BOUND.
        CONTINUE.
      ENDIF.

      " Process based on the content type
      CASE <content>-type.
        WHEN 'text'.
          " Handle text resource
          TRY.
              
              temp1 ?= <content>-resource.
              
              text_resource = temp1.

              " Set URI (required)
              result->set( iv_path         = |{ content_path }/uri|
                           iv_val          = text_resource->uri
                           iv_ignore_empty = abap_false ).

              " Set MIME type if present
              IF text_resource->mime_type IS NOT INITIAL.
                result->set( iv_path = |{ content_path }/mimeType|
                             iv_val  = text_resource->mime_type ).
              ENDIF.

              " Set text content
              result->set( iv_path         = |{ content_path }/text|
                           iv_val          = text_resource->text
                           iv_ignore_empty = abap_false ).

            CATCH cx_sy_move_cast_error.
              " Skip if type doesn't match expected structure
              CONTINUE.
          ENDTRY.

        WHEN 'blob'.
          " Handle blob resource
          TRY.
              
              temp2 ?= <content>-resource.
              
              blob_resource = temp2.

              " Set URI (required)
              result->set( iv_path         = |{ content_path }/uri|
                           iv_val          = blob_resource->uri
                           iv_ignore_empty = abap_false ).

              " Set MIME type if present
              IF blob_resource->mime_type IS NOT INITIAL.
                result->set( iv_path = |{ content_path }/mimeType|
                             iv_val  = blob_resource->mime_type ).
              ENDIF.

              " Set blob content
              result->set( iv_path         = |{ content_path }/blob|
                           iv_val          = blob_resource->blob
                           iv_ignore_empty = abap_false ).

            CATCH cx_sy_move_cast_error.
              " Skip if type doesn't match expected structure
              CONTINUE.
          ENDTRY.

        WHEN OTHERS.
          " Unknown resource type - skip
          CONTINUE.
      ENDCASE.
    ENDLOOP.

    " Add metadata (optional)
    IF int_meta IS BOUND.
      result->set( iv_path = '/_meta'
                   iv_val  = int_meta ).
    ENDIF.
  ENDMETHOD.

  METHOD add_text_resource.
    DATA temp3 TYPE resource_content_wrapper.
    DATA resource_content LIKE temp3.
    DATA text_resource TYPE REF TO zcl_mcp_resp_read_resource=>text_resource_contents.
    CLEAR temp3.
    temp3-type = 'text'.
    
    resource_content = temp3.
    
    CREATE OBJECT text_resource TYPE text_resource_contents EXPORTING uri = uri text = text mime_type = mime_type.
    resource_content-resource = text_resource.
    APPEND resource_content TO int_contents.
  ENDMETHOD.

  METHOD add_blob_resource.
    DATA temp4 TYPE resource_content_wrapper.
    DATA resource_content LIKE temp4.
    DATA blob_resource TYPE REF TO zcl_mcp_resp_read_resource=>blob_resource_contents.
    CLEAR temp4.
    temp4-type = 'blob'.
    
    resource_content = temp4.
    
    CREATE OBJECT blob_resource TYPE blob_resource_contents EXPORTING uri = uri blob = blob mime_type = mime_type.
    resource_content-resource = blob_resource.
    APPEND resource_content TO int_contents.
  ENDMETHOD.

  METHOD set_contents.
    int_contents = contents.
  ENDMETHOD.

  METHOD set_meta.
    int_meta = meta.
  ENDMETHOD.
ENDCLASS.
