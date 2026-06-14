  "! <p class="shorttext synchronized" lang="en">MCP Draft Result Envelope Helpers starting with 2026-07-28</p>
INTERFACE zif_mcp_modern_result
  PUBLIC.

  "! <p class="shorttext synchronized">Generate the MCP draft result JSON</p>
  "! Builds the JSON object that is placed into the JSON-RPC result field.
  "! Implementations must include the appropriate resultType value.
  "!
  "! @parameter result              | <p class="shorttext synchronized">Result JSON object</p>
  "! @raising   zcx_mcp_ajson_error | <p class="shorttext synchronized">JSON build error</p>
  METHODS generate_json
    RETURNING VALUE(result) TYPE REF TO zif_mcp_ajson
    RAISING   zcx_mcp_ajson_error.

  "! <p class="shorttext synchronized">Set result metadata</p>
  "! Stores optional MCP _meta fields that are serialized with the result.
  "!
  "! @parameter meta | <p class="shorttext synchronized">Result metadata JSON</p>
  METHODS set_meta
    IMPORTING meta TYPE REF TO zif_mcp_ajson.

  "! <p class="shorttext synchronized">Set cache hints</p>
  "! Sets CacheableResult hints. Use ttl_ms = 0 and cache_scope = private
  "! for user-dependent or non-cacheable responses.
  "!
  "! @parameter ttl_ms      | <p class="shorttext synchronized">Freshness lifetime in milliseconds</p>
  "! @parameter cache_scope | <p class="shorttext synchronized">Cache scope: public or private</p>
  METHODS set_cache
    IMPORTING ttl_ms      TYPE i
              cache_scope TYPE string.

ENDINTERFACE.
