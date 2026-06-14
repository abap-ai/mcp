"! <p class="shorttext synchronized">Server Factory</p>
CLASS zcl_mcp_server_factory DEFINITION
PUBLIC FINAL
CREATE PUBLIC.

  PUBLIC SECTION.
    "! <p class="shorttext synchronized">Get a legacy MCP server</p>
    "! Creates a configured server instance implementing ZIF_MCP_SERVER.
    "!
    "! @parameter area   | <p class="shorttext synchronized">MCP area</p>
    "! @parameter server | <p class="shorttext synchronized">MCP server</p>
    "! @parameter result | <p class="shorttext synchronized">Server instance, initial if not found or wrong type</p>
    CLASS-METHODS get_server
      IMPORTING !area         TYPE zmcp_area
                server        TYPE zmcp_server
      RETURNING VALUE(result) TYPE REF TO zif_mcp_server.

    "! <p class="shorttext synchronized">Get a draft MCP server</p>
    "! Creates a configured server instance implementing ZIF_MCP_SERVER_V2.
    "!
    "! @parameter area   | <p class="shorttext synchronized">MCP area</p>
    "! @parameter server | <p class="shorttext synchronized">MCP server</p>
    "! @parameter result | <p class="shorttext synchronized">V2 server instance, initial if not found or wrong type</p>
    CLASS-METHODS get_server_v2
      IMPORTING !area         TYPE zmcp_area
                server        TYPE zmcp_server
      RETURNING VALUE(result) TYPE REF TO zif_mcp_server_v2.
ENDCLASS.

CLASS zcl_mcp_server_factory IMPLEMENTATION.
  METHOD get_server.
    SELECT SINGLE * FROM zmcp_servers
      WHERE area   = @area
        AND server = @server
      INTO @DATA(server_def).

    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    TRY.
        CREATE OBJECT result TYPE (server_def-class).
      CATCH cx_sy_create_object_error.
        RETURN.
    ENDTRY.

    result->config = NEW zcl_mcp_configuration( area   = area
                                                server = server ).
    result->server-cors_mode    = result->config->get_cors_mode( ).
    result->server-session_mode = result->get_session_mode( ).
  ENDMETHOD.

  METHOD get_server_v2.
    DATA object TYPE REF TO object.

    SELECT SINGLE * FROM zmcp_servers
      WHERE area   = @area
        AND server = @server
      INTO @DATA(server_def).

    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    TRY.
        CREATE OBJECT object TYPE (server_def-class).
        result ?= object.
      CATCH cx_sy_create_object_error
            cx_sy_move_cast_error.
        RETURN.
    ENDTRY.

    result->config = NEW zcl_mcp_configuration( area   = area
                                                server = server ).
  ENDMETHOD.
ENDCLASS.
