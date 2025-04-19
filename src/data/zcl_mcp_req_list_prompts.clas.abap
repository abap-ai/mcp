"! <p class="shorttext synchronized">MCP List Prompts Request</p>
CLASS zcl_mcp_req_list_prompts DEFINITION
  PUBLIC FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    TYPES page_cursor TYPE string.

    "! <p class="shorttext synchronized">Constructor</p>
    "! Parses the List Prompts Request JSON
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

  PRIVATE SECTION.
    DATA int_cursor TYPE page_cursor.
    DATA int_has_cursor TYPE abap_bool.
ENDCLASS.

CLASS zcl_mcp_req_list_prompts IMPLEMENTATION.
  METHOD constructor.
          DATA error TYPE REF TO zcx_mcp_ajson_error.
          DATA temp1 TYPE REF TO zcx_mcp_server.
    " Check if cursor exists in the request (optional parameter)
    IF json->exists( 'cursor' ) IS NOT INITIAL.
      TRY.
          int_cursor = json->get_string( 'cursor' ).
          int_has_cursor = abap_true.
          
        CATCH zcx_mcp_ajson_error INTO error.
          " Convert JSON error to a more specific parameter error
          
          CREATE OBJECT temp1 TYPE zcx_mcp_server EXPORTING textid = zcx_mcp_server=>invalid_arguments msgv1 = 'Invalid cursor format' previous = error.
          RAISE EXCEPTION temp1 ##NO_TEXT.
      ENDTRY.
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
