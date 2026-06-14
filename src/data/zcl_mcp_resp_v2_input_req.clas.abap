"! <p class="shorttext synchronized">MCP draft input required result</p>
"! Builds an MRTR resultType=input_required response with named input requests.
CLASS zcl_mcp_resp_v2_input_req DEFINITION
  PUBLIC FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES zif_mcp_modern_result.

    " Input request entry. REQUEST_KEY must already be JSON-pointer escaped if needed.
    TYPES: BEGIN OF input_request,
             request_key TYPE string,
             method      TYPE string,
             params      TYPE REF TO zif_mcp_ajson,
           END OF input_request.
    TYPES input_requests TYPE STANDARD TABLE OF input_request WITH EMPTY KEY.

    "! <p class="shorttext synchronized">Set request state</p>
    "! Stores opaque state that the client returns with follow-up input.
    "!
    "! @parameter request_state | <p class="shorttext synchronized">Opaque request state</p>
    METHODS set_request_state
      IMPORTING request_state TYPE string.

    "! <p class="shorttext synchronized">Add input request</p>
    "! Adds a named request such as elicitation/create to the inputRequests map.
    "!
    "! @parameter request_key | <p class="shorttext synchronized">Input request key</p>
    "! @parameter method      | <p class="shorttext synchronized">MCP method to call for input</p>
    "! @parameter params      | <p class="shorttext synchronized">Input request parameters</p>
    METHODS add_input_request
      IMPORTING request_key TYPE string
                !method     TYPE string
                params      TYPE REF TO zif_mcp_ajson.

    "! <p class="shorttext synchronized">Set input requests</p>
    "! Replaces the current input request list.
    "!
    "! @parameter requests | <p class="shorttext synchronized">Input request entries</p>
    METHODS set_input_requests
      IMPORTING requests TYPE input_requests.

  PRIVATE SECTION.
    DATA int_request_state TYPE string.
    DATA int_requests      TYPE input_requests.
    DATA int_meta          TYPE REF TO zif_mcp_ajson.
    DATA int_ttl_ms        TYPE i.
    DATA int_cache_scope   TYPE string VALUE 'private'.
ENDCLASS.


CLASS zcl_mcp_resp_v2_input_req IMPLEMENTATION.
  METHOD set_request_state.
    int_request_state = request_state.
  ENDMETHOD.

  METHOD add_input_request.
    APPEND VALUE #( request_key = request_key
                    method      = method
                    params      = params ) TO int_requests.
  ENDMETHOD.

  METHOD set_input_requests.
    int_requests = requests.
  ENDMETHOD.

  METHOD zif_mcp_modern_result~set_meta.
    int_meta = meta.
  ENDMETHOD.

  METHOD zif_mcp_modern_result~set_cache.
    int_ttl_ms = ttl_ms.
    IF cache_scope IS INITIAL.
      int_cache_scope = zif_mcp_constants=>cache_scopes-private.
    ELSE.
      int_cache_scope = cache_scope.
    ENDIF.
  ENDMETHOD.

  METHOD zif_mcp_modern_result~generate_json.
    result = zcl_mcp_ajson=>create_empty( ).

    result->set_string( iv_path = `/resultType`
                        iv_val  = zif_mcp_constants=>result_types-input_required ).

    IF int_request_state IS NOT INITIAL.
      result->set_string( iv_path = `/requestState`
                          iv_val  = int_request_state ).
    ENDIF.

    result->touch_object( `/inputRequests` ).

    LOOP AT int_requests ASSIGNING FIELD-SYMBOL(<request>).
      DATA(request_path) = |/inputRequests/{ <request>-request_key }|.

      result->set_string( iv_path = |{ request_path }/method|
                          iv_val  = <request>-method ).

      IF <request>-params IS BOUND.
        result->set( iv_path = |{ request_path }/params|
                     iv_val  = <request>-params ).
      ELSE.
        result->touch_object( |{ request_path }/params| ).
      ENDIF.
    ENDLOOP.

    IF int_ttl_ms > 0.
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
