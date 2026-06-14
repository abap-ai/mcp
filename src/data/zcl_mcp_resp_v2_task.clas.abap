"! <p class="shorttext synchronized">MCP draft task result</p>
"! Builds a resultType=task response for the official tasks extension.
CLASS zcl_mcp_resp_v2_task DEFINITION
  PUBLIC FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES zif_mcp_modern_result.

    "! <p class="shorttext synchronized">Set task metadata</p>
    "! Stores the task data returned to the client after task creation.
    "!
    "! @parameter task_id          | <p class="shorttext synchronized">Task id</p>
    "! @parameter status           | <p class="shorttext synchronized">Task status</p>
    "! @parameter status_message   | <p class="shorttext synchronized">Optional status message</p>
    "! @parameter ttl_ms           | <p class="shorttext synchronized">Task TTL in milliseconds</p>
    "! @parameter poll_interval_ms | <p class="shorttext synchronized">Recommended poll interval in milliseconds</p>
    METHODS set_task
      IMPORTING task_id          TYPE string
                !status          TYPE string
                status_message   TYPE string OPTIONAL
                ttl_ms           TYPE i      OPTIONAL
                poll_interval_ms TYPE i      OPTIONAL.

  PRIVATE SECTION.
    DATA int_task_id          TYPE string.
    DATA int_status           TYPE string.
    DATA int_status_message   TYPE string.
    DATA int_ttl_ms           TYPE i.
    DATA int_poll_interval_ms TYPE i.
    DATA int_meta             TYPE REF TO zif_mcp_ajson.
    DATA int_cache_ttl_ms     TYPE i.
    DATA int_cache_scope      TYPE string VALUE 'private'.
    DATA int_cache_set        TYPE abap_bool.
ENDCLASS.

CLASS zcl_mcp_resp_v2_task IMPLEMENTATION.
  METHOD set_task.
    int_task_id          = task_id.
    int_status           = status.
    int_status_message   = status_message.
    int_ttl_ms           = ttl_ms.
    int_poll_interval_ms = poll_interval_ms.
  ENDMETHOD.

  METHOD zif_mcp_modern_result~set_meta.
    int_meta = meta.
  ENDMETHOD.

  METHOD zif_mcp_modern_result~set_cache.
    int_cache_set = abap_true.
    int_cache_ttl_ms = ttl_ms.
    IF cache_scope IS INITIAL.
      int_cache_scope = zif_mcp_constants=>cache_scopes-private.
    ELSE.
      int_cache_scope = cache_scope.
    ENDIF.
  ENDMETHOD.

  METHOD zif_mcp_modern_result~generate_json.
    result = zcl_mcp_ajson=>create_empty( ).

    result->set_string( iv_path = `/resultType`
                        iv_val  = zif_mcp_constants=>result_types-task ).

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
