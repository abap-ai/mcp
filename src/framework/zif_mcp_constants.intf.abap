  "! <p class="shorttext synchronized" lang="en">MCP General Constants</p>
INTERFACE zif_mcp_constants
  PUBLIC.

  " Legacy protocol versions used by the existing initialize/session path.
  CONSTANTS supported_protocol_versions    TYPE string VALUE `2025-03-26,2025-06-18,2025-11-25`.
  CONSTANTS protocol_version_2025_03_26    TYPE string VALUE `2025-03-26`.
  CONSTANTS protocol_version_2025_06_18    TYPE string VALUE `2025-06-18`.
  CONSTANTS protocol_version_2025_11_25    TYPE string VALUE `2025-11-25`.

  " Draft stateless version. Re-check before release because the draft can move.
  CONSTANTS protocol_version_2026_07_28    TYPE string VALUE `2026-07-28`.

  CONSTANTS legacy_protocol_versions       TYPE string VALUE `2025-03-26,2025-06-18,2025-11-25`.
  CONSTANTS modern_protocol_versions       TYPE string VALUE `2026-07-28`.

  " Keep old public name on the latest legacy version.
  CONSTANTS latest_protocol_version        TYPE string VALUE protocol_version_2025_11_25.
  CONSTANTS latest_legacy_protocol_version TYPE string VALUE protocol_version_2025_11_25.
  CONSTANTS latest_modern_protocol_version TYPE string VALUE protocol_version_2026_07_28.

  CONSTANTS: BEGIN OF result_types,
               complete       TYPE string VALUE `complete`,
               input_required TYPE string VALUE `input_required`,
               task           TYPE string VALUE `task`,
             END OF result_types.

  CONSTANTS: BEGIN OF cache_scopes,
               public  TYPE string VALUE `public`,
               private TYPE string VALUE `private`,
             END OF cache_scopes.

  CONSTANTS: BEGIN OF header_names,
               protocol_version TYPE string VALUE `Mcp-Protocol-Version` ##NO_TEXT,
               session_id       TYPE string VALUE `Mcp-Session-Id` ##NO_TEXT,
               method           TYPE string VALUE `Mcp-Method` ##NO_TEXT,
               name             TYPE string VALUE `Mcp-Name` ##NO_TEXT,
             END OF header_names.

  CONSTANTS: BEGIN OF meta_keys,
               protocol_version    TYPE string VALUE `io.modelcontextprotocol/protocolVersion`,
               client_info         TYPE string VALUE `io.modelcontextprotocol/clientInfo`,
               client_capabilities TYPE string VALUE `io.modelcontextprotocol/clientCapabilities`,
               log_level           TYPE string VALUE `io.modelcontextprotocol/logLevel`,
               traceparent         TYPE string VALUE `traceparent`,
               tracestate          TYPE string VALUE `tracestate`,
               baggage             TYPE string VALUE `baggage`,
             END OF meta_keys.

  " AJSON path form for keys containing slash characters.
  CONSTANTS: BEGIN OF meta_paths,
               protocol_version    TYPE string VALUE `/_meta/io.modelcontextprotocol~1protocolVersion`,
               client_info         TYPE string VALUE `/_meta/io.modelcontextprotocol~1clientInfo`,
               client_capabilities TYPE string VALUE `/_meta/io.modelcontextprotocol~1clientCapabilities`,
               log_level           TYPE string VALUE `/_meta/io.modelcontextprotocol~1logLevel`,
               traceparent         TYPE string VALUE `/_meta/traceparent`,
               tracestate          TYPE string VALUE `/_meta/tracestate`,
               baggage             TYPE string VALUE `/_meta/baggage`,
             END OF meta_paths.

  CONSTANTS: BEGIN OF extensions,
               tasks TYPE string VALUE `io.modelcontextprotocol/tasks`,
             END OF extensions.


  " AJSON path form relative to the _meta object itself.
  CONSTANTS: BEGIN OF meta_member_paths,
               protocol_version    TYPE string VALUE `/io.modelcontextprotocol~1protocolVersion`,
               client_info         TYPE string VALUE `/io.modelcontextprotocol~1clientInfo`,
               client_capabilities TYPE string VALUE `/io.modelcontextprotocol~1clientCapabilities`,
               log_level           TYPE string VALUE `/io.modelcontextprotocol~1logLevel`,
               traceparent         TYPE string VALUE `/traceparent`,
               tracestate          TYPE string VALUE `/tracestate`,
               baggage             TYPE string VALUE `/baggage`,
             END OF meta_member_paths.

ENDINTERFACE.
