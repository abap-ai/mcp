"! <p class="shorttext synchronized">MCP draft complete result</p>
"! Builds an explicit resultType=complete envelope for draft/MRTR responses.
CLASS zcl_mcp_resp_v2_complete DEFINITION
PUBLIC FINAL
CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES zif_mcp_modern_result.

    "! <p class="shorttext synchronized">Set result payload</p>
    "! Copies the supplied JSON object and adds resultType=complete.
    "!
    "! @parameter payload | <p class="shorttext synchronized">Result payload JSON</p>
    METHODS set_payload
      IMPORTING payload TYPE REF TO zif_mcp_ajson.

  PRIVATE SECTION.
    DATA int_payload     TYPE REF TO zif_mcp_ajson.
    DATA int_meta        TYPE REF TO zif_mcp_ajson.
    DATA int_ttl_ms      TYPE i.
    DATA int_cache_scope TYPE string VALUE 'private'.
    DATA int_cache_set   TYPE abap_bool.
ENDCLASS.


CLASS zcl_mcp_resp_v2_complete IMPLEMENTATION.
  METHOD set_payload.
    int_payload = payload.
  ENDMETHOD.

  METHOD zif_mcp_modern_result~set_meta.
    int_meta = meta.
  ENDMETHOD.

  METHOD zif_mcp_modern_result~set_cache.
    int_cache_set = abap_true.
    int_ttl_ms = ttl_ms.
    IF cache_scope IS INITIAL.
      int_cache_scope = zif_mcp_constants=>cache_scopes-private.
    ELSE.
      int_cache_scope = cache_scope.
    ENDIF.
  ENDMETHOD.

  METHOD zif_mcp_modern_result~generate_json.
    IF int_payload IS BOUND.
      result = int_payload->clone( ).
    ELSE.
      result = zcl_mcp_ajson=>create_empty( ).
    ENDIF.

    result->set_string( iv_path = `/resultType`
                        iv_val  = zif_mcp_constants=>result_types-complete ).

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
ENDCLASS.
