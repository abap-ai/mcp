CLASS zcl_mcp_req_list_tasks DEFINITION
  PUBLIC FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    "! <p class="shorttext synchronized">Constructor</p>
    "! Parses the tasks/list request params
    "!
    "! @parameter json | <p class="shorttext synchronized">JSON params</p>
    METHODS constructor
      IMPORTING json TYPE REF TO zif_mcp_ajson
      RAISING   zcx_mcp_ajson_error.

    "! <p class="shorttext synchronized">Check if cursor was provided</p>
    "!
    "! @parameter result | <p class="shorttext synchronized">True if cursor exists</p>
    METHODS has_cursor
      RETURNING VALUE(result) TYPE abap_bool.

    "! <p class="shorttext synchronized">Get cursor for pagination</p>
    "!
    "! @parameter result | <p class="shorttext synchronized">Cursor</p>
    METHODS get_cursor
      RETURNING VALUE(result) TYPE zif_mcp_types=>page_cursor.

    "! <p class="shorttext synchronized">Get _meta fields</p>
    "!
    "! @parameter result | <p class="shorttext synchronized">_meta JSON</p>
    METHODS get_meta
      RETURNING VALUE(result) TYPE REF TO zif_mcp_ajson.

  PRIVATE SECTION.
    DATA int_cursor     TYPE zif_mcp_types=>page_cursor.
    DATA int_has_cursor TYPE abap_bool.
    DATA int_meta       TYPE REF TO zif_mcp_ajson.
ENDCLASS.

CLASS zcl_mcp_req_list_tasks IMPLEMENTATION.
  METHOD constructor.
    IF json->exists( '/cursor' ).
      int_cursor     = json->get_string( '/cursor' ).
      int_has_cursor = abap_true.
    ELSE.
      int_has_cursor = abap_false.
    ENDIF.

    IF json->exists( '/_meta' ).
      int_meta = json->slice( '/_meta' ).
    ELSE.
      int_meta = zcl_mcp_ajson=>create_empty( ).
    ENDIF.
  ENDMETHOD.

  METHOD has_cursor.
    result = int_has_cursor.
  ENDMETHOD.

  METHOD get_cursor.
    result = int_cursor.
  ENDMETHOD.

  METHOD get_meta.
    result = int_meta.
  ENDMETHOD.
ENDCLASS.
