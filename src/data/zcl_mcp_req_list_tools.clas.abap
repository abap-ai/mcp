"! <p class="shorttext synchronized">MCP List Tools Request</p>
CLASS zcl_mcp_req_list_tools DEFINITION
  PUBLIC FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    TYPES page_cursor TYPE string.

    "! <p class="shorttext synchronized">Constructor</p>
    "! Parses the List Tools Request JSON
    "!
    "! @parameter json | <p class="shorttext synchronized">JSON Request</p>
    METHODS constructor
      IMPORTING json TYPE REF TO zif_mcp_ajson
      RAISING   zcx_mcp_ajson_error
                zcx_mcp_server.

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

    "! <p class="shorttext synchronized">Get _meta fields</p>
    "!
    "! @parameter result | <p class="shorttext synchronized">_meta JSON</p>
    METHODS get_meta
      RETURNING VALUE(result) TYPE REF TO zif_mcp_ajson.

  PRIVATE SECTION.
    DATA int_cursor     TYPE page_cursor.
    DATA int_has_cursor TYPE abap_bool.
    DATA int_meta       TYPE REF TO zif_mcp_ajson.
ENDCLASS.

CLASS zcl_mcp_req_list_tools IMPLEMENTATION.
  METHOD constructor.
    " Check if cursor exists in the request (optional parameter)
    IF json->exists( 'cursor' ).
      TRY.
          int_cursor = json->get_string( 'cursor' ).
          int_has_cursor = abap_true.
        CATCH zcx_mcp_ajson_error INTO DATA(error).
          " Convert JSON error to a more specific parameter error
          RAISE EXCEPTION NEW zcx_mcp_server( textid   = zcx_mcp_server=>invalid_arguments
                                              msgv1    = 'Invalid cursor format'
                                              previous = error ) ##NO_TEXT.
      ENDTRY.
    ELSE.
      int_has_cursor = abap_false.
      CLEAR int_cursor.
    ENDIF.

    " Check for _meta fields
    IF json->exists( '/_meta' ).
      int_meta = json->slice( '/_meta' ).
    ELSE.
      int_meta = zcl_mcp_ajson=>create_empty( ).
    ENDIF.
  ENDMETHOD.

  METHOD get_cursor.
    result = int_cursor.
  ENDMETHOD.

  METHOD has_cursor.
    result = int_has_cursor.
  ENDMETHOD.

  METHOD get_meta.
    result = int_meta.
  ENDMETHOD.

ENDCLASS.
