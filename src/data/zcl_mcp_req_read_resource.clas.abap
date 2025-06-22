"! <p class="shorttext synchronized">MCP Read Resource Request</p>
CLASS zcl_mcp_req_read_resource DEFINITION
  PUBLIC FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    TYPES resource_uri TYPE string.

    "! <p class="shorttext synchronized">Constructor</p>
    "! Parses the Read Resource Request JSON
    "!
    "! @parameter json | <p class="shorttext synchronized">JSON Request</p>
    METHODS constructor
      IMPORTING json TYPE REF TO zif_mcp_ajson
      RAISING   zcx_mcp_ajson_error zcx_mcp_server.

    "! <p class="shorttext synchronized">Get resource URI</p>
    "!
    "! @parameter result | <p class="shorttext synchronized">Resource URI</p>
    METHODS get_uri
      RETURNING VALUE(result) TYPE resource_uri.

    "! <p class="shorttext synchronized">Get _meta fields</p>
    "!
    "! @parameter result | <p class="shorttext synchronized">_meta JSON</p>
    METHODS get_meta
      RETURNING VALUE(result) TYPE REF TO zif_mcp_ajson.

  PRIVATE SECTION.
    DATA int_uri  TYPE resource_uri.
    DATA int_meta TYPE REF TO zif_mcp_ajson.
ENDCLASS.

CLASS zcl_mcp_req_read_resource IMPLEMENTATION.
  METHOD constructor.
    " Check if URI exists - it's a mandatory parameter
    IF json->exists( 'uri' ).
      int_uri = json->get_string( 'uri' ).

      " Additional validation: URI should not be empty
      IF int_uri IS INITIAL.
        RAISE EXCEPTION NEW zcx_mcp_server( textid = zcx_mcp_server=>required_params
                                            msgv1  = 'uri' ).
      ENDIF.
    ELSE.
      RAISE EXCEPTION NEW zcx_mcp_server( textid = zcx_mcp_server=>required_params
                                          msgv1  = 'uri' ).
    ENDIF.

    " Check for _meta fields
    IF json->exists( '/_meta' ).
      int_meta = json->slice( '/_meta' ).
    ELSE.
      int_meta = zcl_mcp_ajson=>create_empty( ).
    ENDIF.
  ENDMETHOD.

  METHOD get_uri.
    result = int_uri.
  ENDMETHOD.

  METHOD get_meta.
    result = int_meta.
  ENDMETHOD.

ENDCLASS.
