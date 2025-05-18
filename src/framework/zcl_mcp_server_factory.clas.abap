"! <p class="shorttext synchronized" lang="en">Server Factory</p>
CLASS zcl_mcp_server_factory DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    "! <p class="shorttext synchronized" lang="en">Get an MCP Server</p>
    "!
    "! @parameter area | <p class="shorttext synchronized" lang="en">MCP Area</p>
    "! @parameter server | <p class="shorttext synchronized" lang="en">MCP Server</p>
    "! @parameter result | <p class="shorttext synchronized" lang="en">Server instance - initial means not found</p>
    CLASS-METHODS get_server IMPORTING area TYPE zmcp_area server TYPE zmcp_server RETURNING VALUE(result) TYPE REF TO zif_mcp_server.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_mcp_server_factory IMPLEMENTATION.
  METHOD get_server.
    DATA server_def TYPE zmcp_servers.
    SELECT SINGLE * FROM zmcp_servers INTO server_def WHERE area = area AND server = server .
    IF sy-subrc <> 0.
      " No further error processing, the caller will check the instance
      RETURN.
    ENDIF.
    TRY.
        CREATE OBJECT result TYPE (server_def-class).
      CATCH cx_sy_create_object_error.                 "#EC EMPTY_CATCH
        " No further error processing, the caller will check the instance
        RETURN.
    ENDTRY.

    CREATE OBJECT result->config TYPE zcl_mcp_configuration EXPORTING area = area server = server.
    result->server-cors_mode    = result->config->get_cors_mode( ).
    result->server-session_mode = result->get_session_mode( ).
  ENDMETHOD.

ENDCLASS.
