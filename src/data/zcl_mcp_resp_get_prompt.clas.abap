CLASS zcl_mcp_resp_get_prompt DEFINITION
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

    TYPES: BEGIN OF text_content,
             type        TYPE string,
             text        TYPE string,
             annotations TYPE zif_mcp_types=>annotations,
             meta        TYPE REF TO zif_mcp_ajson,
           END OF text_content.

    TYPES: BEGIN OF image_content,
             type        TYPE string,
             data        TYPE string,
             mime_type   TYPE string,
             annotations TYPE zif_mcp_types=>annotations,
             meta        TYPE REF TO zif_mcp_ajson,
           END OF image_content.

    TYPES: BEGIN OF audio_content,
             type        TYPE string,
             data        TYPE string,
             mime_type   TYPE string,
             annotations TYPE zif_mcp_types=>annotations,
             meta        TYPE REF TO zif_mcp_ajson,
           END OF audio_content.

    TYPES: BEGIN OF embedded_resource,
             type        TYPE string,
             resource    TYPE REF TO data,
             annotations TYPE zif_mcp_types=>annotations,
             meta        TYPE REF TO zif_mcp_ajson,
           END OF embedded_resource.

    TYPES: BEGIN OF resource_link_content,
             type        TYPE string,
             uri         TYPE string,
             name        TYPE string,
             title       TYPE string,
             description TYPE string,
             mime_type   TYPE string,
             size        TYPE i,
             annotations TYPE zif_mcp_types=>annotations,
             icons       TYPE zif_mcp_types=>icon_list,
             meta        TYPE REF TO zif_mcp_ajson,
           END OF resource_link_content.

    TYPES: BEGIN OF prompt_message,
             role    TYPE zif_mcp_types=>message_role,
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
    "! @parameter meta        | <p class="shorttext synchronized">Optional meta data</p>
    METHODS add_text_message
      IMPORTING role        TYPE zif_mcp_types=>message_role
                !text       TYPE string
                annotations TYPE zif_mcp_types=>annotations OPTIONAL
                meta        TYPE REF TO zif_mcp_ajson       OPTIONAL.

    "! <p class="shorttext synchronized">Add Image Message</p>
    "!
    "! @parameter role        | <p class="shorttext synchronized">Role (user or assistant)</p>
    "! @parameter data        | <p class="shorttext synchronized">Base64-encoded image data</p>
    "! @parameter mime_type   | <p class="shorttext synchronized">MIME type of the image</p>
    "! @parameter annotations | <p class="shorttext synchronized">Optional annotations</p>
    "! @parameter meta        | <p class="shorttext synchronized">Optional meta data</p>
    METHODS add_image_message
      IMPORTING role        TYPE zif_mcp_types=>message_role
                !data       TYPE string
                mime_type   TYPE string
                annotations TYPE zif_mcp_types=>annotations OPTIONAL
                meta        TYPE REF TO zif_mcp_ajson       OPTIONAL.

    "! <p class="shorttext synchronized">Add Audio Message</p>
    "!
    "! @parameter role        | <p class="shorttext synchronized">Role (user or assistant)</p>
    "! @parameter data        | <p class="shorttext synchronized">Base64-encoded audio data</p>
    "! @parameter mime_type   | <p class="shorttext synchronized">MIME type of the audio file</p>
    "! @parameter annotations | <p class="shorttext synchronized">Optional annotations</p>
    "! @parameter meta        | <p class="shorttext synchronized">Optional meta data</p>
    METHODS add_audio_message
      IMPORTING role        TYPE zif_mcp_types=>message_role
                !data       TYPE string
                mime_type   TYPE string
                annotations TYPE zif_mcp_types=>annotations OPTIONAL
                meta        TYPE REF TO zif_mcp_ajson       OPTIONAL.

    "! <p class="shorttext synchronized">Add Text Resource Message</p>
    "!
    "! @parameter role        | <p class="shorttext synchronized">Role (user or assistant)</p>
    "! @parameter uri         | <p class="shorttext synchronized">Resource URI</p>
    "! @parameter text        | <p class="shorttext synchronized">Text content</p>
    "! @parameter mime_type   | <p class="shorttext synchronized">Optional MIME type</p>
    "! @parameter annotations | <p class="shorttext synchronized">Optional annotations</p>
    "! @parameter meta        | <p class="shorttext synchronized">Optional meta data</p>
    METHODS add_text_resource_message
      IMPORTING role        TYPE zif_mcp_types=>message_role
                uri         TYPE string
                !text       TYPE string
                mime_type   TYPE string                     OPTIONAL
                annotations TYPE zif_mcp_types=>annotations OPTIONAL
                meta        TYPE REF TO zif_mcp_ajson       OPTIONAL.

    "! <p class="shorttext synchronized">Add Blob Resource Message</p>
    "!
    "! @parameter role        | <p class="shorttext synchronized">Role (user or assistant)</p>
    "! @parameter uri         | <p class="shorttext synchronized">Resource URI</p>
    "! @parameter blob        | <p class="shorttext synchronized">Base64-encoded blob data</p>
    "! @parameter mime_type   | <p class="shorttext synchronized">Optional MIME type</p>
    "! @parameter annotations | <p class="shorttext synchronized">Optional annotations</p>
    "! @parameter meta        | <p class="shorttext synchronized">Optional meta data</p>
    METHODS add_blob_resource_message
      IMPORTING role        TYPE zif_mcp_types=>message_role
                uri         TYPE string
                !blob       TYPE string
                mime_type   TYPE string                     OPTIONAL
                annotations TYPE zif_mcp_types=>annotations OPTIONAL
                meta        TYPE REF TO zif_mcp_ajson       OPTIONAL.

    "! <p class="shorttext synchronized">Add Resource Link Message</p>
    "!
    "! @parameter role        | <p class="shorttext synchronized">Role (user or assistant)</p>
    "! @parameter uri         | <p class="shorttext synchronized">Resource URI</p>
    "! @parameter name        | <p class="shorttext synchronized">Resource name</p>
    "! @parameter description | <p class="shorttext synchronized">Optional resource description</p>
    "! @parameter title       | <p class="shorttext synchronized">Optional resource title</p>
    "! @parameter mime_type   | <p class="shorttext synchronized">Optional MIME type</p>
    "! @parameter size        | <p class="shorttext synchronized">Optional size</p>
    "! @parameter annotations | <p class="shorttext synchronized">Optional annotations</p>
    "! @parameter icons       | <p class="shorttext synchronized">Optional icons</p>
    "! @parameter meta        | <p class="shorttext synchronized">Optional meta data</p>
    METHODS add_resource_link_message
      IMPORTING role         TYPE zif_mcp_types=>message_role
                uri          TYPE string
                !name        TYPE string
                !description TYPE string                     OPTIONAL
                !title       TYPE string                     OPTIONAL
                mime_type    TYPE string                     OPTIONAL
                !size        TYPE i                          OPTIONAL
                annotations  TYPE zif_mcp_types=>annotations OPTIONAL
                icons        TYPE zif_mcp_types=>icon_list   OPTIONAL
                meta         TYPE REF TO zif_mcp_ajson       OPTIONAL.

    "! <p class="shorttext synchronized">Set Meta Data</p>
    "! Optional metadata to attach to response
    "!
    "! @parameter meta | <p class="shorttext synchronized">Meta data</p>
    METHODS set_meta
      IMPORTING meta TYPE REF TO zif_mcp_ajson.

  PRIVATE SECTION.
    CONSTANTS: BEGIN OF content_type,
                 text          TYPE string VALUE 'text',
                 image         TYPE string VALUE 'image',
                 resource      TYPE string VALUE 'resource',
                 resource_link TYPE string VALUE 'resource_link',
                 audio         TYPE string VALUE 'audio',
               END OF content_type.

    CONSTANTS: BEGIN OF message_type,
                 text          TYPE i VALUE 1,
                 image         TYPE i VALUE 2,
                 text_resource TYPE i VALUE 3,
                 blob_resource TYPE i VALUE 4,
                 audio         TYPE i VALUE 5,
                 resource_link TYPE i VALUE 6,
               END OF message_type.

    TYPES: BEGIN OF unified_message,
             type         TYPE i,
             role         TYPE zif_mcp_types=>message_role,
             text         TYPE string,
             image_data   TYPE string,
             image_mime   TYPE string,
             audio_data   TYPE string,
             audio_mime   TYPE string,
             res_uri      TYPE string,
             res_text     TYPE string,
             res_blob     TYPE string,
             res_mime     TYPE string,
             res_name     TYPE string,
             res_desc     TYPE string,
             res_title    TYPE string,
             res_size     TYPE i,
             res_icons    TYPE zif_mcp_types=>icon_list,
             annotations  TYPE zif_mcp_types=>annotations,
             content_meta TYPE REF TO zif_mcp_ajson,
           END OF unified_message.

    DATA description TYPE string.
    DATA messages    TYPE STANDARD TABLE OF unified_message WITH DEFAULT KEY.
    DATA meta        TYPE REF TO zif_mcp_ajson.

    METHODS add_annotations_to_json
      IMPORTING !path       TYPE string
                annotations TYPE zif_mcp_types=>annotations
                !result     TYPE REF TO zif_mcp_ajson
      RAISING   zcx_mcp_ajson_error.

    METHODS add_meta_to_json
      IMPORTING !path   TYPE string
                meta    TYPE REF TO zif_mcp_ajson
                !result TYPE REF TO zif_mcp_ajson
      RAISING   zcx_mcp_ajson_error.

ENDCLASS.

CLASS zcl_mcp_resp_get_prompt IMPLEMENTATION.
  METHOD zif_mcp_internal~generate_json.
    FIELD-SYMBOLS <message> LIKE LINE OF messages.
      DATA message_path TYPE string.
            FIELD-SYMBOLS <prl_icon> LIKE LINE OF <message>-res_icons.
              DATA prl_icon_path TYPE string.
                FIELD-SYMBOLS <prl_icon_size> LIKE LINE OF <prl_icon>-sizes.
    result = zcl_mcp_ajson=>create_empty( ).

    " Add description if present
    IF description IS NOT INITIAL.
      result->set( iv_path = '/description'
                   iv_val  = description ).
    ENDIF.

    " Create messages array
    result->touch_array( '/messages' ).

    " Process all messages in order
    
    LOOP AT messages ASSIGNING <message>.
      
      message_path = |/messages/{ sy-tabix }|.

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
          " Handle text resource (embedded)
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
          " Handle blob resource (embedded)
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

        WHEN message_type-resource_link.
          " Handle resource link
          result->set( iv_path         = |{ message_path }/content/type|
                       iv_val          = content_type-resource_link
                       iv_ignore_empty = abap_false ).

          result->set( iv_path         = |{ message_path }/content/uri|
                       iv_val          = <message>-res_uri
                       iv_ignore_empty = abap_false ).

          IF <message>-res_name IS NOT INITIAL.
            result->set( iv_path = |{ message_path }/content/name|
                         iv_val  = <message>-res_name ).
          ENDIF.

          IF <message>-res_desc IS NOT INITIAL.
            result->set( iv_path = |{ message_path }/content/description|
                         iv_val  = <message>-res_desc ).
          ENDIF.

          IF <message>-res_mime IS NOT INITIAL.
            result->set( iv_path = |{ message_path }/content/mimeType|
                         iv_val  = <message>-res_mime ).
          ENDIF.

          IF <message>-res_title IS NOT INITIAL.
            result->set( iv_path = |{ message_path }/content/title|
                         iv_val  = <message>-res_title ).
          ENDIF.

          IF <message>-res_size > 0.
            result->set( iv_path = |{ message_path }/content/size|
                         iv_val  = <message>-res_size ).
          ENDIF.

          IF <message>-res_icons IS NOT INITIAL.
            result->touch_array( |{ message_path }/content/icons| ).
            
            LOOP AT <message>-res_icons ASSIGNING <prl_icon>.
              
              prl_icon_path = |{ message_path }/content/icons/{ sy-tabix }|.
              result->set( iv_path = |{ prl_icon_path }/src|
                           iv_val  = <prl_icon>-src ).
              IF <prl_icon>-mime_type IS NOT INITIAL.
                result->set( iv_path = |{ prl_icon_path }/mimeType|
                             iv_val  = <prl_icon>-mime_type ).
              ENDIF.
              IF <prl_icon>-sizes IS NOT INITIAL.
                result->touch_array( |{ prl_icon_path }/sizes| ).
                
                LOOP AT <prl_icon>-sizes ASSIGNING <prl_icon_size>.
                  result->set( iv_path = |{ prl_icon_path }/sizes/{ sy-tabix }|
                               iv_val  = <prl_icon_size> ).
                ENDLOOP.
              ENDIF.
              IF <prl_icon>-theme IS NOT INITIAL.
                result->set( iv_path = |{ prl_icon_path }/theme|
                             iv_val  = <prl_icon>-theme ).
              ENDIF.
            ENDLOOP.
          ENDIF.
      ENDCASE.

      " Add annotations if present
      add_annotations_to_json( path        = |{ message_path }/content|
                               annotations = <message>-annotations
                               result      = result ).

      " Add content meta if present
      add_meta_to_json( path   = |{ message_path }/content|
                        meta   = <message>-content_meta
                        result = result ).
    ENDLOOP.

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
    IF annotations-audience IS INITIAL AND
       annotations-priority IS INITIAL AND
       annotations-last_modified IS INITIAL.
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

    " Add lastModified if not empty
    IF annotations-last_modified IS NOT INITIAL.
      
      iso_timestamp = zcl_mcp_util=>timestamp_to_iso8601( annotations-last_modified ).
      result->set( iv_path = |{ path }/annotations/lastModified|
                   iv_val  = iso_timestamp ).
    ENDIF.
  ENDMETHOD.

  METHOD add_meta_to_json.
    " Add _meta if present
    IF meta IS BOUND.
      result->set( iv_path = |{ path }/_meta|
                   iv_val  = meta ).
    ENDIF.
  ENDMETHOD.

  METHOD set_description.
    me->description = description.
  ENDMETHOD.

  METHOD set_messages.
    FIELD-SYMBOLS <input_message> LIKE LINE OF messages.
      FIELD-SYMBOLS <content_struct> TYPE any.
      DATA type TYPE string.
      FIELD-SYMBOLS <type> TYPE any.
      DATA content_meta TYPE REF TO zif_mcp_ajson.
      FIELD-SYMBOLS <meta> TYPE any.
          FIELD-SYMBOLS <text> TYPE any.
          DATA annotations TYPE zif_mcp_types=>annotations.
          FIELD-SYMBOLS <annotations> TYPE any.
          DATA temp1 TYPE unified_message.
          FIELD-SYMBOLS <data> TYPE any.
          FIELD-SYMBOLS <mime_type> TYPE any.
          DATA temp2 TYPE unified_message.
          DATA temp3 TYPE unified_message.
          FIELD-SYMBOLS <resource> TYPE any.
          FIELD-SYMBOLS <res_struct> TYPE any.
          FIELD-SYMBOLS <uri> TYPE any.
          DATA mime_type TYPE string.
          FIELD-SYMBOLS <res_mime> TYPE any.
          FIELD-SYMBOLS <res_text> TYPE any.
            DATA temp4 TYPE unified_message.
            FIELD-SYMBOLS <res_blob> TYPE any.
              DATA temp5 TYPE unified_message.
          DATA name TYPE string.
          FIELD-SYMBOLS <name> TYPE any.
          DATA description TYPE string.
          FIELD-SYMBOLS <desc> TYPE any.
          DATA title TYPE string.
          FIELD-SYMBOLS <title> TYPE any.
          DATA res_size TYPE i.
          FIELD-SYMBOLS <res_size> TYPE any.
          DATA rl_icons TYPE zif_mcp_types=>icon_list.
          FIELD-SYMBOLS <rl_icons> TYPE any.
          DATA temp6 TYPE unified_message.
    " Clear existing messages
    CLEAR me->messages.

    " Process each message
    
    LOOP AT messages ASSIGNING <input_message>.
      " Skip if content is not bound
      IF <input_message>-content IS NOT BOUND.
        CONTINUE.
      ENDIF.

      " Get content as structure
      
      ASSIGN <input_message>-content->* TO <content_struct>.

      " Get content type field
      
      
      ASSIGN COMPONENT 'TYPE' OF STRUCTURE <content_struct> TO <type>.
      IF sy-subrc <> 0.
        " Skip if no type field
        CONTINUE.
      ENDIF.
      type = <type>.

      " Get meta if present
      
      
      ASSIGN COMPONENT 'META' OF STRUCTURE <content_struct> TO <meta>.
      IF sy-subrc = 0.
        content_meta = <meta>.
      ENDIF.

      " Process based on content type
      CASE type.
        WHEN content_type-text.
          " Get text field
          
          ASSIGN COMPONENT 'TEXT' OF STRUCTURE <content_struct> TO <text>.
          IF sy-subrc <> 0.
            CONTINUE.
          ENDIF.

          " Get annotations
          
          
          ASSIGN COMPONENT 'ANNOTATIONS' OF STRUCTURE <content_struct> TO <annotations>.
          IF sy-subrc = 0.
            annotations = <annotations>.
          ENDIF.

          " Add text message
          
          CLEAR temp1.
          temp1-type = message_type-text.
          temp1-role = <input_message>-role.
          temp1-text = <text>.
          temp1-annotations = annotations.
          temp1-content_meta = content_meta.
          APPEND temp1 TO me->messages.

        WHEN content_type-image.
          " Get image data
          
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

          " Add image message
          
          CLEAR temp2.
          temp2-type = message_type-image.
          temp2-role = <input_message>-role.
          temp2-image_data = <data>.
          temp2-image_mime = <mime_type>.
          temp2-annotations = annotations.
          temp2-content_meta = content_meta.
          APPEND temp2 TO me->messages.

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
          
          CLEAR temp3.
          temp3-type = message_type-audio.
          temp3-role = <input_message>-role.
          temp3-audio_data = <data>.
          temp3-audio_mime = <mime_type>.
          temp3-annotations = annotations.
          temp3-content_meta = content_meta.
          APPEND temp3 TO me->messages.

        WHEN content_type-resource.
          " Get resource reference
          
          ASSIGN COMPONENT 'RESOURCE' OF STRUCTURE <content_struct> TO <resource>.
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
          
          ASSIGN <resource>->* TO <res_struct>.
          IF sy-subrc <> 0.
            CONTINUE.
          ENDIF.

          " Check common fields
          
          ASSIGN COMPONENT 'URI' OF STRUCTURE <res_struct> TO <uri>.
          IF sy-subrc <> 0.
            CONTINUE.
          ENDIF.

          
          
          ASSIGN COMPONENT 'MIME_TYPE' OF STRUCTURE <res_struct> TO <res_mime>.
          IF sy-subrc = 0.
            mime_type = <res_mime>.
          ENDIF.

          " Check if it's a text resource
          
          ASSIGN COMPONENT 'TEXT' OF STRUCTURE <res_struct> TO <res_text>.
          IF sy-subrc = 0.
            " It's a text resource
            
            CLEAR temp4.
            temp4-type = message_type-text_resource.
            temp4-role = <input_message>-role.
            temp4-res_uri = <uri>.
            temp4-res_text = <res_text>.
            temp4-res_mime = mime_type.
            temp4-annotations = annotations.
            temp4-content_meta = content_meta.
            APPEND temp4 TO me->messages.
          ELSE.
            " Check if it's a blob resource
            
            ASSIGN COMPONENT 'BLOB' OF STRUCTURE <res_struct> TO <res_blob>.
            IF sy-subrc = 0.
              " It's a blob resource
              
              CLEAR temp5.
              temp5-type = message_type-blob_resource.
              temp5-role = <input_message>-role.
              temp5-res_uri = <uri>.
              temp5-res_blob = <res_blob>.
              temp5-res_mime = mime_type.
              temp5-annotations = annotations.
              temp5-content_meta = content_meta.
              APPEND temp5 TO me->messages.
            ENDIF.
          ENDIF.

        WHEN content_type-resource_link.
          " Get URI field
          ASSIGN COMPONENT 'URI' OF STRUCTURE <content_struct> TO <uri>.
          IF sy-subrc <> 0.
            CONTINUE.
          ENDIF.

          " Get optional fields
          
          
          ASSIGN COMPONENT 'NAME' OF STRUCTURE <content_struct> TO <name>.
          IF sy-subrc = 0.
            name = <name>.
          ENDIF.

          
          
          ASSIGN COMPONENT 'DESCRIPTION' OF STRUCTURE <content_struct> TO <desc>.
          IF sy-subrc = 0.
            description = <desc>.
          ENDIF.

          CLEAR mime_type.
          ASSIGN COMPONENT 'MIME_TYPE' OF STRUCTURE <content_struct> TO <res_mime>.
          IF sy-subrc = 0.
            mime_type = <res_mime>.
          ENDIF.

          
          
          ASSIGN COMPONENT 'TITLE' OF STRUCTURE <content_struct> TO <title>.
          IF sy-subrc = 0.
            title = <title>.
          ENDIF.

          
          
          ASSIGN COMPONENT 'SIZE' OF STRUCTURE <content_struct> TO <res_size>.
          IF sy-subrc = 0.
            res_size = <res_size>.
          ENDIF.

          " annotations
          CLEAR annotations.
          ASSIGN COMPONENT 'ANNOTATIONS' OF STRUCTURE <content_struct> TO <annotations>.
          IF sy-subrc = 0.
            annotations = <annotations>.
          ENDIF.

          " icons
          
          
          ASSIGN COMPONENT 'ICONS' OF STRUCTURE <content_struct> TO <rl_icons>.
          IF sy-subrc = 0 AND <rl_icons> IS ASSIGNED.
            rl_icons = <rl_icons>.
          ENDIF.

          " Add resource link message
          
          CLEAR temp6.
          temp6-type = message_type-resource_link.
          temp6-role = <input_message>-role.
          temp6-res_uri = <uri>.
          temp6-res_name = name.
          temp6-res_desc = description.
          temp6-res_mime = mime_type.
          temp6-res_title = title.
          temp6-res_size = res_size.
          temp6-annotations = annotations.
          temp6-res_icons = rl_icons.
          temp6-content_meta = content_meta.
          APPEND temp6 TO me->messages.
      ENDCASE.
    ENDLOOP.
  ENDMETHOD.

  METHOD add_text_message.
    DATA temp7 TYPE unified_message.
    CLEAR temp7.
    temp7-type = message_type-text.
    temp7-role = role.
    temp7-text = text.
    temp7-annotations = annotations.
    temp7-content_meta = meta.
    APPEND temp7 TO messages.
  ENDMETHOD.

  METHOD add_image_message.
    DATA temp8 TYPE unified_message.
    CLEAR temp8.
    temp8-type = message_type-image.
    temp8-role = role.
    temp8-image_data = data.
    temp8-image_mime = mime_type.
    temp8-annotations = annotations.
    temp8-content_meta = meta.
    APPEND temp8 TO messages.
  ENDMETHOD.

  METHOD add_audio_message.
    DATA temp9 TYPE unified_message.
    CLEAR temp9.
    temp9-type = message_type-audio.
    temp9-role = role.
    temp9-audio_data = data.
    temp9-audio_mime = mime_type.
    temp9-annotations = annotations.
    temp9-content_meta = meta.
    APPEND temp9 TO messages.
  ENDMETHOD.

  METHOD add_text_resource_message.
    DATA temp10 TYPE unified_message.
    CLEAR temp10.
    temp10-type = message_type-text_resource.
    temp10-role = role.
    temp10-res_uri = uri.
    temp10-res_text = text.
    temp10-res_mime = mime_type.
    temp10-annotations = annotations.
    temp10-content_meta = meta.
    APPEND temp10 TO messages.
  ENDMETHOD.

  METHOD add_blob_resource_message.
    DATA temp11 TYPE unified_message.
    CLEAR temp11.
    temp11-type = message_type-blob_resource.
    temp11-role = role.
    temp11-res_uri = uri.
    temp11-res_blob = blob.
    temp11-res_mime = mime_type.
    temp11-annotations = annotations.
    temp11-content_meta = meta.
    APPEND temp11 TO messages.
  ENDMETHOD.

  METHOD add_resource_link_message.
    DATA temp12 TYPE unified_message.
    CLEAR temp12.
    temp12-type = message_type-resource_link.
    temp12-role = role.
    temp12-res_title = title.
    temp12-res_uri = uri.
    temp12-res_name = name.
    temp12-res_desc = description.
    temp12-res_mime = mime_type.
    temp12-res_size = size.
    temp12-annotations = annotations.
    temp12-res_icons = icons.
    temp12-content_meta = meta.
    APPEND temp12 TO messages.
  ENDMETHOD.

  METHOD set_meta.
    me->meta = meta.
  ENDMETHOD.
ENDCLASS.
