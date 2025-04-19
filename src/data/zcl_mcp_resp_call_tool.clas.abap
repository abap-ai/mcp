"! <p class="shorttext synchronized">MCP Call Tool Result</p>
"! Implementation for the CallToolResult interface
CLASS zcl_mcp_resp_call_tool DEFINITION
  PUBLIC FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES zif_mcp_internal.

    " Types for annotations
    TYPES: BEGIN OF annotations,
             audience TYPE STANDARD TABLE OF string WITH DEFAULT KEY,
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

    TYPES content_list TYPE STANDARD TABLE OF content_wrapper WITH DEFAULT KEY.

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
    DATA int_content  TYPE content_list.
    DATA int_meta     TYPE REF TO zif_mcp_ajson.
    DATA int_has_error TYPE abap_bool.

    METHODS add_annotations_to_json
      IMPORTING !path       TYPE string
                annotations TYPE annotations
                ajson       TYPE REF TO zif_mcp_ajson
      RAISING   zcx_mcp_ajson_error.
ENDCLASS.

CLASS zcl_mcp_resp_call_tool IMPLEMENTATION.
  METHOD zif_mcp_internal~generate_json.
    FIELD-SYMBOLS <content> LIKE LINE OF int_content.
      DATA content_index LIKE sy-tabix.
      DATA content_path TYPE string.
          DATA temp1 TYPE REF TO text_content.
          DATA text_item LIKE temp1.
          DATA temp2 TYPE REF TO image_content.
          DATA image_item LIKE temp2.
          DATA temp3 TYPE REF TO embedded_resource.
          DATA resource_item LIKE temp3.
                DATA temp4 TYPE REF TO text_resource_contents.
                DATA text_resource LIKE temp4.
                    DATA temp5 TYPE REF TO blob_resource_contents.
                    DATA blob_resource LIKE temp5.
    result = zcl_mcp_ajson=>create_empty( ).
    " Create content array
    result->touch_array( '/content' ).

    " Add all content items
    
    LOOP AT int_content ASSIGNING <content>.
      
      content_index = sy-tabix.
      
      content_path = |/content/{ content_index }|.

      " Skip if content is not bound
      IF <content>-content IS NOT BOUND.
        CONTINUE.
      ENDIF.

      " Set common type field
      result->set( iv_path         = |{ content_path }/type|
                   iv_val          = <content>-type
                   iv_ignore_empty = abap_false ).

      " Handle content based on its type
      CASE <content>-type.
        WHEN 'text'.
          " Text content with direct access
          
          temp1 ?= <content>-content.
          
          text_item = temp1.

          result->set( iv_path         = |{ content_path }/text|
                       iv_val          = text_item->text
                       iv_ignore_empty = abap_false ).

          " Add annotations if present
          add_annotations_to_json( path        = content_path
                                   annotations = text_item->annotations
                                   ajson       = result ).

        WHEN 'image'.
          " Image content with direct access
          
          temp2 ?= <content>-content.
          
          image_item = temp2.

          result->set( iv_path         = |{ content_path }/data|
                       iv_val          = image_item->data
                       iv_ignore_empty = abap_false ).

          result->set( iv_path         = |{ content_path }/mimeType|
                       iv_val          = image_item->mime_type
                       iv_ignore_empty = abap_false ).

          " Add annotations
          add_annotations_to_json( path        = content_path
                                   annotations = image_item->annotations
                                   ajson       = result ).

        WHEN 'resource'.
          " Resource content with direct access
          
          temp3 ?= <content>-content.
          
          resource_item = temp3.

          IF resource_item->resource IS BOUND.
            " Try to determine if it's a text or blob resource
            TRY.
                " Try as text resource first
                
                temp4 ?= resource_item->resource.
                
                text_resource = temp4.

                " Set resource fields
                result->set( iv_path         = |{ content_path }/resource/uri|
                             iv_val          = text_resource->uri
                             iv_ignore_empty = abap_false ).

                IF text_resource->mime_type IS NOT INITIAL.
                  result->set( iv_path = |{ content_path }/resource/mimeType|
                               iv_val  = text_resource->mime_type ).
                ENDIF.

                result->set( iv_path         = |{ content_path }/resource/text|
                             iv_val          = text_resource->text
                             iv_ignore_empty = abap_false ).

              CATCH cx_sy_move_cast_error.
                " Must be a blob resource
                TRY.
                    
                    temp5 ?= resource_item->resource.
                    
                    blob_resource = temp5.

                    " Set resource fields
                    result->set( iv_path         = |{ content_path }/resource/uri|
                                 iv_val          = blob_resource->uri
                                 iv_ignore_empty = abap_false ).

                    IF blob_resource->mime_type IS NOT INITIAL.
                      result->set( iv_path = |{ content_path }/resource/mimeType|
                                   iv_val  = blob_resource->mime_type ).
                    ENDIF.

                    result->set( iv_path         = |{ content_path }/resource/blob|
                                 iv_val          = blob_resource->blob
                                 iv_ignore_empty = abap_false ).

                  CATCH cx_sy_move_cast_error.
                    " Unknown resource type - skip
                    CONTINUE.
                ENDTRY.
            ENDTRY.

            " Add annotations
            add_annotations_to_json( path        = content_path
                                     annotations = resource_item->annotations
                                     ajson       = result ).
          ENDIF.
      ENDCASE.
    ENDLOOP.

    " Add isError flag if true (only if set)
    IF int_has_error = abap_true.
      result->set( iv_path = '/isError'
                   iv_val  = abap_true ).
    ENDIF.

    " Add metadata (optional)
    IF int_meta IS BOUND.
      result->set( iv_path = '/_meta'
                   iv_val  = int_meta ).
    ENDIF.
  ENDMETHOD.


  METHOD add_annotations_to_json.
      FIELD-SYMBOLS <audience> LIKE LINE OF annotations-audience.
    " Add annotations if not empty
    IF annotations-audience IS INITIAL AND annotations-priority IS INITIAL.
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
  ENDMETHOD.

  METHOD set_error.
    int_has_error = has_error.
  ENDMETHOD.

  METHOD add_text_content.
    DATA temp6 TYPE content_wrapper.
    DATA content_item LIKE temp6.
    DATA text_item TYPE REF TO zcl_mcp_resp_call_tool=>text_content.
    CLEAR temp6.
    temp6-type = 'text'.
    
    content_item = temp6.
    
    CREATE OBJECT text_item TYPE text_content EXPORTING type = 'text' text = text annotations = annotations.
    content_item-content = text_item.
    APPEND content_item TO int_content.
  ENDMETHOD.

  METHOD add_image_content.
    DATA temp7 TYPE content_wrapper.
    DATA content_item LIKE temp7.
    DATA image_item TYPE REF TO zcl_mcp_resp_call_tool=>image_content.
    CLEAR temp7.
    temp7-type = 'image'.
    
    content_item = temp7.
    
    CREATE OBJECT image_item TYPE image_content EXPORTING type = 'image' data = data mime_type = mime_type annotations = annotations.
    content_item-content = image_item.
    APPEND content_item TO int_content.
  ENDMETHOD.

  METHOD add_text_resource.
    DATA temp8 TYPE content_wrapper.
    DATA content_item LIKE temp8.
    DATA resource_item TYPE REF TO zcl_mcp_resp_call_tool=>text_resource_contents.
    DATA embedded_resource_item TYPE REF TO zcl_mcp_resp_call_tool=>embedded_resource.
    CLEAR temp8.
    temp8-type = 'resource'.
    
    content_item = temp8.
    
    CREATE OBJECT resource_item TYPE text_resource_contents EXPORTING uri = uri text = text mime_type = mime_type.
    
    CREATE OBJECT embedded_resource_item TYPE embedded_resource EXPORTING type = 'resource' resource = resource_item annotations = annotations.
    content_item-content = embedded_resource_item.
    APPEND content_item TO int_content.
  ENDMETHOD.

  METHOD add_blob_resource.
    DATA temp9 TYPE content_wrapper.
    DATA content_item LIKE temp9.
    DATA resource_item TYPE REF TO zcl_mcp_resp_call_tool=>blob_resource_contents.
    DATA embedded_resource_item TYPE REF TO zcl_mcp_resp_call_tool=>embedded_resource.
    CLEAR temp9.
    temp9-type = 'resource'.
    
    content_item = temp9.
    
    CREATE OBJECT resource_item TYPE blob_resource_contents EXPORTING uri = uri blob = blob mime_type = mime_type.
    
    CREATE OBJECT embedded_resource_item TYPE embedded_resource EXPORTING type = 'resource' resource = resource_item annotations = annotations.
    content_item-content = embedded_resource_item.
    APPEND content_item TO int_content.
  ENDMETHOD.

  METHOD set_content.
    int_content = content.
  ENDMETHOD.

  METHOD set_meta.
    int_meta = meta.
  ENDMETHOD.
ENDCLASS.
