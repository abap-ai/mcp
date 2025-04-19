"! <p class="shorttext synchronized" lang="en">Internal MCP interface</p>
INTERFACE zif_mcp_internal
  PUBLIC.
  "! <p class="shorttext synchronized">Generate JSON based on class data</p>
  METHODS generate_json returning value(result) TYPE REF TO zif_mcp_ajson
                        RAISING   zcx_mcp_ajson_error.
ENDINTERFACE.
