"! <p class="shorttext synchronized">MCP draft acknowledgement result</p>
"! Builds an empty success result for acknowledgement-style draft methods.
CLASS zcl_mcp_resp_v2_ack DEFINITION
  PUBLIC FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES zif_mcp_modern_result.

    "! <p class="shorttext synchronized">Generate JSON result</p>
    "!
    "! @parameter result              | <p class="shorttext synchronized">Acknowledgement result JSON</p>
    "! @raising   zcx_mcp_ajson_error | <p class="shorttext synchronized">Raised when JSON generation fails</p>
    METHODS generate_json
      RETURNING VALUE(result) TYPE REF TO zif_mcp_ajson
      RAISING   zcx_mcp_ajson_error.

    "! <p class="shorttext synchronized">Set complete result envelope</p>
    "!
    "! @parameter enabled | <p class="shorttext synchronized">Whether to emit resultType=complete</p>
    METHODS set_complete
      IMPORTING !enabled TYPE abap_bool DEFAULT abap_true.

    "! <p class="shorttext synchronized">Set result metadata</p>
    "!
    "! @parameter meta | <p class="shorttext synchronized">Result metadata</p>
    METHODS set_meta
      IMPORTING meta TYPE REF TO zif_mcp_ajson.

    "! <p class="shorttext synchronized">Set cache hint</p>
    "!
    "! @parameter ttl_ms      | <p class="shorttext synchronized">Cache time-to-live in milliseconds</p>
    "! @parameter cache_scope | <p class="shorttext synchronized">Cache scope</p>
    METHODS set_cache
      IMPORTING ttl_ms      TYPE i
                cache_scope TYPE string.

  PRIVATE SECTION.
    DATA int_complete    TYPE abap_bool.
    DATA int_meta        TYPE REF TO zif_mcp_ajson.
    DATA int_ttl_ms      TYPE i.
    DATA int_cache_scope TYPE string VALUE zif_mcp_constants=>cache_scopes-private.
    DATA int_cache_set   TYPE abap_bool.
ENDCLASS.


CLASS zcl_mcp_resp_v2_ack IMPLEMENTATION.
  METHOD set_complete.
    int_complete = enabled.
  ENDMETHOD.

  METHOD set_meta.
    int_meta = meta.
  ENDMETHOD.

  METHOD set_cache.
    int_cache_set = abap_true.
    int_ttl_ms = ttl_ms.
    IF cache_scope IS INITIAL.
      int_cache_scope = zif_mcp_constants=>cache_scopes-private.
    ELSE.
      int_cache_scope = cache_scope.
    ENDIF.
  ENDMETHOD.

  METHOD generate_json.
    result = zcl_mcp_ajson=>create_empty( ).
    result->touch_object( `` ).

    IF int_complete = abap_true.
      result->set_string( iv_path = `/resultType`
                          iv_val  = zif_mcp_constants=>result_types-complete ).
    ENDIF.

    IF int_cache_set = abap_true.
      result->set_integer( iv_path = `/ttlMs`
                           iv_val  = int_ttl_ms ).
      result->set_string( iv_path = `/cacheScope`
                          iv_val  = int_cache_scope ).
    ENDIF.

    IF int_meta IS BOUND.
      result->set( iv_path = `/_meta`
                   iv_val  = int_meta ).
    ENDIF.
  ENDMETHOD.

  METHOD zif_mcp_modern_result~generate_json.
    result = generate_json( ).
  ENDMETHOD.

  METHOD zif_mcp_modern_result~set_meta.
    set_meta( meta ).
  ENDMETHOD.

  METHOD zif_mcp_modern_result~set_cache.
    set_cache( ttl_ms      = ttl_ms
               cache_scope = cache_scope ).
  ENDMETHOD.
ENDCLASS.
