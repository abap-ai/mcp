"! <p class="shorttext synchronized" lang="en">MCP General Constants</p>
INTERFACE zif_mcp_constants
  PUBLIC.
  " Protocol Versions, earliest supported version is 2025-03-26 with Streamable HTTP
  CONSTANTS supported_protocol_versions TYPE string VALUE `2025-03-26,2025-06-18`.
  CONSTANTS protocol_version_2025_03_26 TYPE string VALUE `2025-03-26`.
  CONSTANTS protocol_version_2025_06_18 TYPE string VALUE `2025-06-18`.
  CONSTANTS latest_protocol_version     TYPE string VALUE protocol_version_2025_06_18.
  CONSTANTS default_protocol_version    TYPE string VALUE protocol_version_2025_03_26.

ENDINTERFACE.
