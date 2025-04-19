"! <p class="shorttext synchronized">MCP Get Prompt Result</p>
"! Implementation for the GetPromptResult interface
CLASS zcl_mcp_resp_get_prompt DEFINITION
  PUBLIC FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES zif_mcp_internal.

    TYPES: BEGIN OF annotations,
             audience TYPE STANDARD TABLE OF string WITH DEFAULT KEY,
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

    TYPES prompt_messages TYPE STANDARD TABLE OF prompt_message WITH DEFAULT KEY.

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
    DATA text_messages       TYPE STANDARD TABLE OF text_message_entry WITH DEFAULT KEY.
    DATA image_messages      TYPE STANDARD TABLE OF image_message_entry WITH DEFAULT KEY.
    DATA text_resources      TYPE STANDARD TABLE OF text_resource_message_entry WITH DEFAULT KEY.
    DATA blob_resources      TYPE STANDARD TABLE OF blob_resource_message_entry WITH DEFAULT KEY.
    DATA message_order       TYPE STANDARD TABLE OF message_order_entry WITH DEFAULT KEY.
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
    DATA message_index TYPE i VALUE 0.
    FIELD-SYMBOLS <order> LIKE LINE OF message_order.
      DATA message_path TYPE string.
          DATA text_idx LIKE <order>-index.
          FIELD-SYMBOLS <text> TYPE zcl_mcp_resp_get_prompt=>text_message_entry.
          DATA image_idx LIKE <order>-index.
          FIELD-SYMBOLS <image> TYPE zcl_mcp_resp_get_prompt=>image_message_entry.
          DATA text_res_idx LIKE <order>-index.
          FIELD-SYMBOLS <text_res> TYPE zcl_mcp_resp_get_prompt=>text_resource_message_entry.
          DATA blob_res_idx LIKE <order>-index.
          FIELD-SYMBOLS <blob_res> TYPE zcl_mcp_resp_get_prompt=>blob_resource_message_entry.
    result = zcl_mcp_ajson=>create_empty( ).

    " Add description if present
    IF description IS NOT INITIAL.
      result->set( iv_path = '/description'
                   iv_val  = description ).
    ENDIF.

    " Create messages array
    result->touch_array( '/messages' ).

    " Process messages in the original order they were added
    

    
    LOOP AT message_order ASSIGNING <order>.
      message_index = message_index + 1.
      
      message_path = |/messages/{ message_index }|.

      CASE <order>-type.
        WHEN message_type-text.
          
          text_idx = <order>-index.
          
          READ TABLE text_messages INDEX text_idx ASSIGNING <text>.
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
          
          image_idx = <order>-index.
          
          READ TABLE image_messages INDEX image_idx ASSIGNING <image>.
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
          
          text_res_idx = <order>-index.
          
          READ TABLE text_resources INDEX text_res_idx ASSIGNING <text_res>.
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
          
          blob_res_idx = <order>-index.
          
          READ TABLE blob_resources INDEX blob_res_idx ASSIGNING <blob_res>.
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
      FIELD-SYMBOLS <audience> LIKE LINE OF annotations-audience.
    " Add annotations if not empty
    IF annotations-audience IS INITIAL AND annotations-priority IS INITIAL.
      RETURN.
    ENDIF.

    " Add audience array if not empty
    IF annotations-audience IS NOT INITIAL.
      result->touch_array( |{ path }/annotations/audience| ).

      
      LOOP AT annotations-audience ASSIGNING <audience>.
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
    FIELD-SYMBOLS <message> LIKE LINE OF messages.
          DATA temp1 TYPE REF TO text_content.
          DATA text_content LIKE temp1.
          DATA temp2 TYPE text_message_entry.
          DATA temp3 TYPE message_order_entry.
              DATA temp4 TYPE REF TO image_content.
              DATA image_content LIKE temp4.
              DATA temp5 TYPE image_message_entry.
              DATA temp6 TYPE message_order_entry.
                  DATA temp7 TYPE REF TO embedded_resource.
                  DATA resource_content LIKE temp7.
                        DATA temp8 TYPE REF TO text_resource_contents.
                        DATA text_resource LIKE temp8.
                        DATA temp9 TYPE text_resource_message_entry.
                        DATA temp10 TYPE message_order_entry.
                            DATA temp11 TYPE REF TO blob_resource_contents.
                            DATA blob_resource LIKE temp11.
                            DATA temp12 TYPE blob_resource_message_entry.
                            DATA temp13 TYPE message_order_entry.
    " Clear existing messages
    CLEAR: text_messages, image_messages, text_resources, blob_resources, message_order.

    " Process each message and convert to simplified storage
    
    LOOP AT messages ASSIGNING <message>.
      " Skip if content is not bound
      IF <message>-content IS NOT BOUND.
        CONTINUE.
      ENDIF.

      " Try to determine the content type and process accordingly
      TRY.
          " First try as text content
          
          temp1 ?= <message>-content.
          
          text_content = temp1.

          
          CLEAR temp2.
          temp2-role = <message>-role.
          temp2-text = text_content->text.
          temp2-annotations = text_content->annotations.
          APPEND temp2 TO text_messages.

          " Track order
          
          CLEAR temp3.
          temp3-type = message_type-text.
          temp3-index = lines( text_messages ).
          APPEND temp3 TO message_order.

        CATCH cx_sy_move_cast_error.
          TRY.
              " Try as image content
              
              temp4 ?= <message>-content.
              
              image_content = temp4.

              
              CLEAR temp5.
              temp5-role = <message>-role.
              temp5-data = image_content->data.
              temp5-mime_type = image_content->mime_type.
              temp5-annotations = image_content->annotations.
              APPEND temp5 TO image_messages.

              " Track order
              
              CLEAR temp6.
              temp6-type = message_type-image.
              temp6-index = lines( image_messages ).
              APPEND temp6 TO message_order.

            CATCH cx_sy_move_cast_error.
              TRY.
                  " Try as resource content
                  
                  temp7 ?= <message>-content.
                  
                  resource_content = temp7.

                  IF resource_content->resource IS BOUND.
                    TRY.
                        " Try as text resource
                        
                        temp8 ?= resource_content->resource.
                        
                        text_resource = temp8.

                        
                        CLEAR temp9.
                        temp9-role = <message>-role.
                        temp9-uri = text_resource->uri.
                        temp9-text = text_resource->text.
                        temp9-mime_type = text_resource->mime_type.
                        temp9-annotations = resource_content->annotations.
                        APPEND temp9 TO text_resources.

                        " Track order
                        
                        CLEAR temp10.
                        temp10-type = message_type-text_resource.
                        temp10-index = lines( text_resources ).
                        APPEND temp10 TO message_order.

                      CATCH cx_sy_move_cast_error.
                        TRY.
                            " Try as blob resource
                            
                            temp11 ?= resource_content->resource.
                            
                            blob_resource = temp11.

                            
                            CLEAR temp12.
                            temp12-role = <message>-role.
                            temp12-uri = blob_resource->uri.
                            temp12-blob = blob_resource->blob.
                            temp12-mime_type = blob_resource->mime_type.
                            temp12-annotations = resource_content->annotations.
                            APPEND temp12 TO blob_resources.

                            " Track order
                            
                            CLEAR temp13.
                            temp13-type = message_type-blob_resource.
                            temp13-index = lines( blob_resources ).
                            APPEND temp13 TO message_order.

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
    DATA temp14 TYPE text_message_entry.
    DATA temp15 TYPE message_order_entry.
    CLEAR temp14.
    temp14-role = role.
    temp14-text = text.
    temp14-annotations = annotations.
    APPEND temp14 TO text_messages.

    " Track order
    
    CLEAR temp15.
    temp15-type = message_type-text.
    temp15-index = lines( text_messages ).
    APPEND temp15 TO message_order.
  ENDMETHOD.

  METHOD add_image_message.
    DATA temp16 TYPE image_message_entry.
    DATA temp17 TYPE message_order_entry.
    CLEAR temp16.
    temp16-role = role.
    temp16-data = data.
    temp16-mime_type = mime_type.
    temp16-annotations = annotations.
    APPEND temp16 TO image_messages.

    " Track order
    
    CLEAR temp17.
    temp17-type = message_type-image.
    temp17-index = lines( image_messages ).
    APPEND temp17 TO message_order.
  ENDMETHOD.

  METHOD add_text_resource_message.
    DATA temp18 TYPE text_resource_message_entry.
    DATA temp19 TYPE message_order_entry.
    CLEAR temp18.
    temp18-role = role.
    temp18-uri = uri.
    temp18-text = text.
    temp18-mime_type = mime_type.
    temp18-annotations = annotations.
    APPEND temp18 TO text_resources.

    " Track order
    
    CLEAR temp19.
    temp19-type = message_type-text_resource.
    temp19-index = lines( text_resources ).
    APPEND temp19 TO message_order.
  ENDMETHOD.

  METHOD add_blob_resource_message.
    DATA temp20 TYPE blob_resource_message_entry.
    DATA temp21 TYPE message_order_entry.
    CLEAR temp20.
    temp20-role = role.
    temp20-uri = uri.
    temp20-blob = blob.
    temp20-mime_type = mime_type.
    temp20-annotations = annotations.
    APPEND temp20 TO blob_resources.

    " Track order
    
    CLEAR temp21.
    temp21-type = message_type-blob_resource.
    temp21-index = lines( blob_resources ).
    APPEND temp21 TO message_order.
  ENDMETHOD.

  METHOD set_meta.
    me->meta = meta.
  ENDMETHOD.
ENDCLASS.
