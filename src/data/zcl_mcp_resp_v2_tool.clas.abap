"! <p class="shorttext synchronized">MCP draft v2 tool result</p>
"! High-level builder for tools/call results in the draft stateless protocol.
"! It reuses the existing CallToolResult content model and adds draft complete
"! result metadata such as resultType, ttlMs, cacheScope, and _meta.
CLASS zcl_mcp_resp_v2_tool DEFINITION
  PUBLIC FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES zif_mcp_modern_result.

    "! <p class="shorttext synchronized">Create tool result builder</p>
    METHODS constructor.

    "! <p class="shorttext synchronized">Generate JSON result</p>
    "!
    "! @parameter result              | <p class="shorttext synchronized">Serialized tools/call result</p>
    "! @raising   zcx_mcp_ajson_error | <p class="shorttext synchronized">Raised when JSON generation fails</p>
    METHODS generate_json
      RETURNING VALUE(result) TYPE REF TO zif_mcp_ajson
      RAISING   zcx_mcp_ajson_error.

    "! <p class="shorttext synchronized">Set complete result envelope</p>
    "! Enables or disables explicit resultType=complete generation.
    "!
    "! @parameter enabled | <p class="shorttext synchronized">Whether to emit resultType=complete</p>
    METHODS set_complete
      IMPORTING !enabled TYPE abap_bool DEFAULT abap_true.

    "! <p class="shorttext synchronized">Set error status</p>
    "!
    "! @parameter has_error | <p class="shorttext synchronized">Whether the tool call ended in an error</p>
    METHODS set_error
      IMPORTING has_error TYPE abap_bool DEFAULT abap_true.

    "! <p class="shorttext synchronized">Set structured content</p>
    "!
    "! @parameter structured_content | <p class="shorttext synchronized">Structured result content</p>
    "! @parameter add_text_content   | <p class="shorttext synchronized">Whether to add a text representation</p>
    METHODS set_structured_content
      IMPORTING structured_content TYPE REF TO zif_mcp_ajson
                add_text_content   TYPE abap_bool DEFAULT abap_true.

    "! <p class="shorttext synchronized">Add text content</p>
    "!
    "! @parameter text        | <p class="shorttext synchronized">Text content</p>
    "! @parameter annotations | <p class="shorttext synchronized">Optional content annotations</p>
    "! @parameter meta        | <p class="shorttext synchronized">Optional content metadata</p>
    METHODS add_text_content
      IMPORTING !text       TYPE string
                annotations TYPE zif_mcp_types=>annotations OPTIONAL
                meta        TYPE REF TO zif_mcp_ajson       OPTIONAL.

    "! <p class="shorttext synchronized">Add image content</p>
    "!
    "! @parameter data        | <p class="shorttext synchronized">Base64 image data</p>
    "! @parameter mime_type   | <p class="shorttext synchronized">Image MIME type</p>
    "! @parameter annotations | <p class="shorttext synchronized">Optional content annotations</p>
    "! @parameter meta        | <p class="shorttext synchronized">Optional content metadata</p>
    METHODS add_image_content
      IMPORTING !data       TYPE string
                mime_type   TYPE string
                annotations TYPE zif_mcp_types=>annotations OPTIONAL
                meta        TYPE REF TO zif_mcp_ajson       OPTIONAL.

    "! <p class="shorttext synchronized">Add audio content</p>
    "!
    "! @parameter data        | <p class="shorttext synchronized">Base64 audio data</p>
    "! @parameter mime_type   | <p class="shorttext synchronized">Audio MIME type</p>
    "! @parameter annotations | <p class="shorttext synchronized">Optional content annotations</p>
    "! @parameter meta        | <p class="shorttext synchronized">Optional content metadata</p>
    METHODS add_audio_content
      IMPORTING !data       TYPE string
                mime_type   TYPE string
                annotations TYPE zif_mcp_types=>annotations OPTIONAL
                meta        TYPE REF TO zif_mcp_ajson       OPTIONAL.

    "! <p class="shorttext synchronized">Add resource link content</p>
    "!
    "! @parameter uri         | <p class="shorttext synchronized">Resource URI</p>
    "! @parameter name        | <p class="shorttext synchronized">Resource name</p>
    "! @parameter title       | <p class="shorttext synchronized">Optional title</p>
    "! @parameter description | <p class="shorttext synchronized">Optional description</p>
    "! @parameter mime_type   | <p class="shorttext synchronized">Optional MIME type</p>
    "! @parameter size        | <p class="shorttext synchronized">Optional size in bytes</p>
    "! @parameter annotations | <p class="shorttext synchronized">Optional annotations</p>
    "! @parameter icons       | <p class="shorttext synchronized">Optional icons</p>
    "! @parameter meta        | <p class="shorttext synchronized">Optional metadata</p>
    METHODS add_resource_link
      IMPORTING uri          TYPE string
                !name        TYPE string
                !title       TYPE string                     OPTIONAL
                !description TYPE string                     OPTIONAL
                mime_type    TYPE string                     OPTIONAL
                !size        TYPE i                          OPTIONAL
                annotations  TYPE zif_mcp_types=>annotations OPTIONAL
                icons        TYPE zif_mcp_types=>icon_list   OPTIONAL
                meta         TYPE REF TO zif_mcp_ajson       OPTIONAL.

    "! <p class="shorttext synchronized">Add embedded text resource</p>
    "!
    "! @parameter uri         | <p class="shorttext synchronized">Resource URI</p>
    "! @parameter text        | <p class="shorttext synchronized">Text resource content</p>
    "! @parameter mime_type   | <p class="shorttext synchronized">Optional MIME type</p>
    "! @parameter annotations | <p class="shorttext synchronized">Optional annotations</p>
    "! @parameter meta        | <p class="shorttext synchronized">Optional metadata</p>
    METHODS add_text_resource
      IMPORTING uri         TYPE string
                !text       TYPE string
                mime_type   TYPE string                     OPTIONAL
                annotations TYPE zif_mcp_types=>annotations OPTIONAL
                meta        TYPE REF TO zif_mcp_ajson       OPTIONAL.

    "! <p class="shorttext synchronized">Add embedded blob resource</p>
    "!
    "! @parameter uri         | <p class="shorttext synchronized">Resource URI</p>
    "! @parameter blob        | <p class="shorttext synchronized">Base64 blob content</p>
    "! @parameter mime_type   | <p class="shorttext synchronized">Optional MIME type</p>
    "! @parameter annotations | <p class="shorttext synchronized">Optional annotations</p>
    "! @parameter meta        | <p class="shorttext synchronized">Optional metadata</p>
    METHODS add_blob_resource
      IMPORTING uri         TYPE string
                !blob       TYPE string
                mime_type   TYPE string                     OPTIONAL
                annotations TYPE zif_mcp_types=>annotations OPTIONAL
                meta        TYPE REF TO zif_mcp_ajson       OPTIONAL.

    "! <p class="shorttext synchronized">Set result metadata</p>
    "!
    "! @parameter meta | <p class="shorttext synchronized">Result metadata</p>
    METHODS set_meta
      IMPORTING meta TYPE REF TO zif_mcp_ajson.

    "! <p class="shorttext synchronized">Set cache hint</p>
    "!
    "! @parameter ttl_ms      | <p class="shorttext synchronized">Cache time-to-live in milliseconds</p>
    "! @parameter cache_scope | <p class="shorttext synchronized">Cache scope</p>
    METHODS set_cache
      IMPORTING ttl_ms      TYPE i
                cache_scope TYPE string.

  PRIVATE SECTION.
    DATA int_result      TYPE REF TO zcl_mcp_resp_call_tool.
    DATA int_meta        TYPE REF TO zif_mcp_ajson.
    DATA int_ttl_ms      TYPE i.
    DATA int_cache_scope TYPE string VALUE zif_mcp_constants=>cache_scopes-private.
    DATA int_complete    TYPE abap_bool.
    DATA int_error_set   TYPE abap_bool.
    DATA int_has_error   TYPE abap_bool.
    DATA int_cache_set   TYPE abap_bool.
ENDCLASS.


CLASS zcl_mcp_resp_v2_tool IMPLEMENTATION.
  METHOD constructor.
    int_result = NEW zcl_mcp_resp_call_tool( ).
  ENDMETHOD.

  METHOD set_complete.
    int_complete = enabled.
  ENDMETHOD.

  METHOD set_error.
    int_error_set = abap_true.
    int_has_error = has_error.
    int_result->set_error( has_error ).
  ENDMETHOD.

  METHOD set_structured_content.
    int_result->set_structured_content( structured_content = structured_content
                                        add_text_content   = add_text_content ).
  ENDMETHOD.

  METHOD add_text_content.
    int_result->add_text_content( text        = text
                                  annotations = annotations
                                  meta        = meta ).
  ENDMETHOD.

  METHOD add_image_content.
    int_result->add_image_content( data        = data
                                   mime_type   = mime_type
                                   annotations = annotations
                                   meta        = meta ).
  ENDMETHOD.

  METHOD add_audio_content.
    int_result->add_audio_content( data        = data
                                   mime_type   = mime_type
                                   annotations = annotations
                                   meta        = meta ).
  ENDMETHOD.

  METHOD add_resource_link.
    int_result->add_resource_link( uri         = uri
                                   name        = name
                                   title       = title
                                   description = description
                                   mime_type   = mime_type
                                   size        = size
                                   annotations = annotations
                                   icons       = icons
                                   meta        = meta ).
  ENDMETHOD.

  METHOD add_text_resource.
    int_result->add_text_resource( uri         = uri
                                   text        = text
                                   mime_type   = mime_type
                                   annotations = annotations
                                   meta        = meta ).
  ENDMETHOD.

  METHOD add_blob_resource.
    int_result->add_blob_resource( uri         = uri
                                   blob        = blob
                                   mime_type   = mime_type
                                   annotations = annotations
                                   meta        = meta ).
  ENDMETHOD.

  METHOD set_meta.
    int_meta = meta.
  ENDMETHOD.

  METHOD set_cache.
    int_cache_set = abap_true.
    int_ttl_ms = ttl_ms.
    IF cache_scope IS INITIAL.
      int_cache_scope = zif_mcp_constants=>cache_scopes-private.
    ELSE.
      int_cache_scope = cache_scope.
    ENDIF.
  ENDMETHOD.

  METHOD generate_json.
    result = int_result->zif_mcp_internal~generate_json( ).

    IF int_error_set = abap_true.
      result->set( iv_path         = `/isError`
                   iv_val          = int_has_error
                   iv_ignore_empty = abap_false ).
    ENDIF.

    IF int_complete = abap_true.
      result->set_string( iv_path = `/resultType`
                          iv_val  = zif_mcp_constants=>result_types-complete ).
    ENDIF.

    IF int_cache_set = abap_true.
      result->set_integer( iv_path = `/ttlMs`
                           iv_val  = int_ttl_ms ).
      result->set_string( iv_path = `/cacheScope`
                          iv_val  = int_cache_scope ).
    ENDIF.

    IF int_meta IS BOUND.
      result->set( iv_path = `/_meta`
                   iv_val  = int_meta ).
    ENDIF.
  ENDMETHOD.

  METHOD zif_mcp_modern_result~generate_json.
    result = generate_json( ).
  ENDMETHOD.

  METHOD zif_mcp_modern_result~set_meta.
    set_meta( meta ).
  ENDMETHOD.

  METHOD zif_mcp_modern_result~set_cache.
    set_cache( ttl_ms      = ttl_ms
               cache_scope = cache_scope ).
  ENDMETHOD.
ENDCLASS.
