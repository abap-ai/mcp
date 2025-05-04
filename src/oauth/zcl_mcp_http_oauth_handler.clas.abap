CLASS zcl_mcp_http_oauth_handler DEFINITION
  PUBLIC
  INHERITING FROM zcl_mcp_http_handler
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES if_oauth2_consumer .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.

CLASS zcl_mcp_http_oauth_handler IMPLEMENTATION.
  METHOD if_oauth2_consumer~provide_tadir_key_for_request.
    e_tadir_key-obj_name = 'ZCL_MCP_HTTP_OAUTH_HANDLER'.
    e_tadir_key-object   = 'CLAS'.
    e_tadir_key-pgmid    = 'R3TR'.
  ENDMETHOD.

ENDCLASS.
