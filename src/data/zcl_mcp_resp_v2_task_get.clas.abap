"! <p class="shorttext synchronized">MCP draft tasks/get result</p>
"! High-level builder for tasks/get responses in the tasks extension.
CLASS zcl_mcp_resp_v2_task_get DEFINITION
  PUBLIC FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES zif_mcp_modern_result.

    "! <p class="shorttext synchronized">Set task state</p>
    "!
    "! @parameter task_id          | <p class="shorttext synchronized">Task id</p>
    "! @parameter status           | <p class="shorttext synchronized">Task status</p>
    "! @parameter status_message   | <p class="shorttext synchronized">Optional status message</p>
    "! @parameter ttl_ms           | <p class="shorttext synchronized">Optional task TTL in milliseconds</p>
    "! @parameter poll_interval_ms | <p class="shorttext synchronized">Optional polling interval in milliseconds</p>
    METHODS set_task
      IMPORTING task_id          TYPE string
                !status          TYPE string
                status_message   TYPE string OPTIONAL
                ttl_ms           TYPE i      OPTIONAL
                poll_interval_ms TYPE i      OPTIONAL.

    "! <p class="shorttext synchronized">Set completed result</p>
    "!
    "! @parameter payload | <p class="shorttext synchronized">Final result payload of the original request</p>
    METHODS set_result
      IMPORTING payload TYPE REF TO zif_mcp_ajson.

    "! <p class="shorttext synchronized">Set failed task error</p>
    "!
    "! @parameter code    | <p class="shorttext synchronized">JSON-RPC error code</p>
    "! @parameter message | <p class="shorttext synchronized">JSON-RPC error message</p>
    "! @parameter data    | <p class="shorttext synchronized">Optional JSON-RPC error data</p>
    METHODS set_error
      IMPORTING !code    TYPE i
                !message TYPE string
                !data    TYPE REF TO zif_mcp_ajson OPTIONAL.

    "! <p class="shorttext synchronized">Set request state</p>
    "!
    "! @parameter request_state | <p class="shorttext synchronized">Opaque input request state</p>
    METHODS set_request_state
      IMPORTING request_state TYPE string.

    "! <p class="shorttext synchronized">Add input request</p>
    "!
    "! @parameter request_key | <p class="shorttext synchronized">Input request key</p>
    "! @parameter method      | <p class="shorttext synchronized">MCP method the client should call</p>
    "! @parameter params      | <p class="shorttext synchronized">Input request parameters</p>
    METHODS add_input_request
      IMPORTING request_key TYPE string
                !method     TYPE string
                params      TYPE REF TO zif_mcp_ajson.

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
    TYPES: BEGIN OF input_request,
             request_key TYPE string,
             method      TYPE string,
             params      TYPE REF TO zif_mcp_ajson,
           END OF input_request.
    TYPES input_requests TYPE STANDARD TABLE OF input_request WITH EMPTY KEY.

    DATA int_task_id          TYPE string.
    DATA int_status           TYPE string.
    DATA int_status_message   TYPE string.
    DATA int_ttl_ms           TYPE i.
    DATA int_poll_interval_ms TYPE i.
    DATA int_result           TYPE REF TO zif_mcp_ajson.
    DATA int_error            TYPE zcl_mcp_jsonrpc=>error.
    DATA int_request_state    TYPE string.
    DATA int_requests         TYPE input_requests.
    DATA int_meta             TYPE REF TO zif_mcp_ajson.
    DATA int_cache_ttl_ms     TYPE i.
    DATA int_cache_scope      TYPE string VALUE zif_mcp_constants=>cache_scopes-private.
    DATA int_cache_set        TYPE abap_bool.
ENDCLASS.


CLASS zcl_mcp_resp_v2_task_get IMPLEMENTATION.
  METHOD set_task.
    int_task_id          = task_id.
    int_status           = status.
    int_status_message   = status_message.
    int_ttl_ms           = ttl_ms.
    int_poll_interval_ms = poll_interval_ms.
  ENDMETHOD.

  METHOD set_result.
    int_result = payload.
  ENDMETHOD.

  METHOD set_error.
    int_error-code    = code.
    int_error-message = message.
    int_error-data    = data.
  ENDMETHOD.

  METHOD set_request_state.
    int_request_state = request_state.
  ENDMETHOD.

  METHOD add_input_request.
    APPEND VALUE input_request( request_key = request_key
                                method      = method
                                params      = params ) TO int_requests.
  ENDMETHOD.

  METHOD set_meta.
    int_meta = meta.
  ENDMETHOD.

  METHOD set_cache.
    int_cache_set = abap_true.
    int_cache_ttl_ms = ttl_ms.
    IF cache_scope IS INITIAL.
      int_cache_scope = zif_mcp_constants=>cache_scopes-private.
    ELSE.
      int_cache_scope = cache_scope.
    ENDIF.
  ENDMETHOD.

  METHOD zif_mcp_modern_result~set_meta.
    set_meta( meta ).
  ENDMETHOD.

  METHOD zif_mcp_modern_result~set_cache.
    set_cache( ttl_ms      = ttl_ms
               cache_scope = cache_scope ).
  ENDMETHOD.

  METHOD zif_mcp_modern_result~generate_json.
    result = zcl_mcp_ajson=>create_empty( ).

    result->set_string( iv_path = `/resultType`
                        iv_val  = zif_mcp_constants=>result_types-complete ).
    result->set_string( iv_path = `/task/taskId`
                        iv_val  = int_task_id ).
    result->set_string( iv_path = `/task/status`
                        iv_val  = int_status ).

    IF int_status_message IS NOT INITIAL.
      result->set_string( iv_path = `/task/statusMessage`
                          iv_val  = int_status_message ).
    ENDIF.

    IF int_ttl_ms > 0.
      result->set_integer( iv_path = `/task/ttlMs`
                           iv_val  = int_ttl_ms ).
    ENDIF.

    IF int_poll_interval_ms > 0.
      result->set_integer( iv_path = `/task/pollIntervalMs`
                           iv_val  = int_poll_interval_ms ).
    ENDIF.

    IF int_result IS BOUND.
      result->set( iv_path = `/result`
                   iv_val  = int_result ).
    ENDIF.

    IF int_error-code IS NOT INITIAL OR int_error-message IS NOT INITIAL.
      result->set( iv_path = `/error/code`
                   iv_val  = int_error-code ).
      result->set_string( iv_path = `/error/message`
                          iv_val  = int_error-message ).
      IF int_error-data IS BOUND.
        result->set( iv_path = `/error/data`
                     iv_val  = int_error-data ).
      ENDIF.
    ENDIF.

    IF int_request_state IS NOT INITIAL.
      result->set_string( iv_path = `/requestState`
                          iv_val  = int_request_state ).
    ENDIF.

    IF int_requests IS NOT INITIAL.
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
    ENDIF.

    IF int_cache_set = abap_true.
      result->set_integer( iv_path = `/ttlMs`
                           iv_val  = int_cache_ttl_ms ).
      result->set_string( iv_path = `/cacheScope`
                          iv_val  = int_cache_scope ).
    ENDIF.

    IF int_meta IS BOUND.
      result->set( iv_path = `/_meta`
                   iv_val  = int_meta ).
    ENDIF.
  ENDMETHOD.
ENDCLASS.
