"! <p class="shorttext synchronized">MCP List Resource Templates Request</p>
CLASS zcl_mcp_req_list_res_tmpls DEFINITION
  PUBLIC FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    TYPES page_cursor TYPE string.

    "! <p class="shorttext synchronized">Constructor</p>
    "! Parses the List Resource Templates Request JSON
    "!
    "! @parameter json | <p class="shorttext synchronized">JSON Request</p>
    METHODS constructor
      IMPORTING json TYPE REF TO zif_mcp_ajson
      RAISING   zcx_mcp_ajson_error.

    "! <p class="shorttext synchronized">Get cursor for pagination</p>
    "!
    "! @parameter result | <p class="shorttext synchronized">Cursor</p>
    METHODS get_cursor
      RETURNING VALUE(result) TYPE page_cursor.

    "! <p class="shorttext synchronized">Check if cursor was provided</p>
    "!
    "! @parameter result | <p class="shorttext synchronized">True if cursor exists</p>
    METHODS has_cursor
      RETURNING VALUE(result) TYPE abap_bool.

  PRIVATE SECTION.
    DATA int_cursor TYPE page_cursor.
    DATA int_has_cursor TYPE abap_bool.
ENDCLASS.

CLASS zcl_mcp_req_list_res_tmpls IMPLEMENTATION.
  METHOD constructor.
    " Check if cursor exists in the request
    IF json->exists( 'cursor' ) IS NOT INITIAL.
      int_has_cursor = abap_true.
      int_cursor = json->get_string( 'cursor' ).
    ELSE.
      int_has_cursor = abap_false.
      CLEAR int_cursor.
    ENDIF.
  ENDMETHOD.

  METHOD get_cursor.
    result = int_cursor.
  ENDMETHOD.

  METHOD has_cursor.
    result = int_has_cursor.
  ENDMETHOD.
ENDCLASS.
