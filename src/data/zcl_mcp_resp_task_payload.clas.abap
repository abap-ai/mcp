CLASS zcl_mcp_resp_task_payload DEFINITION
  PUBLIC FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES zif_mcp_internal.

    "! <p class="shorttext synchronized">Add Text Content Item</p>
    "! Appends a plain-text content entry to the result's content array.
    "! May be called multiple times to add several text items.
    "! At least one of add_text_content or set_structured_content should be used.
    "!
    "! @parameter text | <p class="shorttext synchronized">Plain text to include in the result</p>
    METHODS add_text_content
      IMPORTING !text TYPE string.

    "! <p class="shorttext synchronized">Set Structured Content</p>
    "! Sets the structured (machine-readable) result payload.
    "! The JSON object must conform to the output schema declared for the tool.
    "! Can be combined with add_text_content to provide both a human-readable
    "! summary and structured data in the same result.
    "!
    "! @parameter structured_content | <p class="shorttext synchronized">JSON object conforming to the tool output schema</p>
    METHODS set_structured_content
      IMPORTING structured_content TYPE REF TO zif_mcp_ajson.

    "! <p class="shorttext synchronized">Mark Result as Error</p>
    "! Flags this task result as an error outcome.
    "! The content array should describe the error when this flag is set.
    "! Defaults to abap_true when called without an explicit value.
    "!
    "! @parameter is_error | <p class="shorttext synchronized">Set to abap_true to mark result as error</p>
    METHODS set_is_error
      IMPORTING is_error TYPE abap_bool DEFAULT abap_true.

    "! <p class="shorttext synchronized">Set Meta Data</p>
    "! Attaches optional metadata to the response under the _meta key.
    "!
    "! @parameter meta | <p class="shorttext synchronized">Metadata JSON object</p>
    METHODS set_meta
      IMPORTING meta TYPE REF TO zif_mcp_ajson.

    "! <p class="shorttext synchronized">Load Pre-built Result JSON</p>
    "! Used by the framework when retrieving a stored task result from the DB.
    "! The JSON must already be a valid CallToolResult shape as produced by
    "! generate_json. When set, generate_json returns this JSON directly
    "! instead of building from the individual setter methods.
    "!
    "! @parameter json | <p class="shorttext synchronized">Pre-built CallToolResult JSON</p>
    METHODS set_from_json
      IMPORTING !json TYPE REF TO zif_mcp_ajson.

    "! <p class="shorttext synchronized">Set Task in _Meta</p>
    "!
    "! @parameter task_id             | <p class="shorttext synchronized">TaskId</p>
    "! @raising   zcx_mcp_ajson_error | <p class="shorttext synchronized">JSON error</p>
    METHODS set_related_task
      IMPORTING task_id TYPE string
      RAISING   zcx_mcp_ajson_error.

    "! <p class="shorttext synchronized" lang="en">Return internal error</p>
    "!
    "! @parameter result | <p class="shorttext synchronized" lang="en"></p>
    METHODS get_is_error
      RETURNING VALUE(result) TYPE abap_bool.

  PRIVATE SECTION.

    TYPES: BEGIN OF text_item,
             type TYPE string,
             text TYPE string,
           END OF text_item.
    TYPES text_items TYPE STANDARD TABLE OF text_item WITH EMPTY KEY.

    DATA int_content            TYPE text_items.
    DATA int_structured_content TYPE REF TO zif_mcp_ajson.
    DATA int_is_error           TYPE abap_bool.
    DATA int_meta               TYPE REF TO zif_mcp_ajson.
    DATA int_prebuilt           TYPE REF TO zif_mcp_ajson.


ENDCLASS.

CLASS zcl_mcp_resp_task_payload IMPLEMENTATION.
  METHOD zif_mcp_internal~generate_json.
    IF int_prebuilt IS BOUND.
      result = int_prebuilt.
      IF int_meta IS BOUND.
        result->set( iv_path = '/_meta' iv_val = int_meta ).
      ENDIF.
      RETURN.
    ENDIF.

    result = zcl_mcp_ajson=>create_empty( ).
    result->touch_array( 'content' ).
    LOOP AT int_content INTO DATA(item).
      result->push( iv_path = 'content' iv_val = item ).
    ENDLOOP.
    IF int_structured_content IS BOUND.
      result->set( iv_path = 'structuredContent' iv_val = int_structured_content ).
    ENDIF.
    IF int_is_error = abap_true.
      result->set( iv_path = 'isError' iv_val = abap_true ).
    ENDIF.
    IF int_meta IS BOUND.
      result->set( iv_path = '/_meta' iv_val = int_meta ).
    ENDIF.
  ENDMETHOD.

  METHOD add_text_content.
    APPEND VALUE text_item( type = 'text' text = text ) TO int_content.
  ENDMETHOD.

  METHOD set_structured_content.
    int_structured_content = structured_content.
  ENDMETHOD.

  METHOD set_is_error.
    int_is_error = is_error.
  ENDMETHOD.

  METHOD set_meta.
    int_meta = meta.
  ENDMETHOD.

  METHOD set_from_json.
    int_prebuilt = json.
  ENDMETHOD.

  METHOD set_related_task.
    DATA related_task TYPE REF TO zif_mcp_ajson.

    IF int_meta IS NOT BOUND.
      IF int_prebuilt IS BOUND AND int_prebuilt->exists( '/_meta' ).
        int_meta = int_prebuilt->slice( '/_meta' ).
      ELSE.
        int_meta = zcl_mcp_ajson=>create_empty( ).
      ENDIF.
    ENDIF.

    related_task = zcl_mcp_ajson=>create_empty( ).
    related_task->set_string( iv_path = '/taskId'
                              iv_val  = task_id ).

    int_meta->set( iv_path = '/io.modelcontextprotocol~1related-task'
                   iv_val  = related_task ).
  ENDMETHOD.

  METHOD get_is_error.
    result = int_is_error.
  ENDMETHOD.
ENDCLASS.
