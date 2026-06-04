CLASS zcl_mcp_resp_cancel_task DEFINITION
  PUBLIC FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES zif_mcp_internal.

    "! <p class="shorttext synchronized">Set Task</p>
    "!
    "! @parameter task | <p class="shorttext synchronized">Task data</p>
    METHODS set_task
      IMPORTING task TYPE zif_mcp_types=>task.

    "! <p class="shorttext synchronized">Set Meta Data</p>
    "!
    "! @parameter meta | <p class="shorttext synchronized">Meta data</p>
    METHODS set_meta
      IMPORTING meta TYPE REF TO zif_mcp_ajson.

  PRIVATE SECTION.
    DATA int_task TYPE zif_mcp_types=>task.
    DATA int_meta TYPE REF TO zif_mcp_ajson.

    METHODS serialize_task
      IMPORTING !path TYPE string
                !task TYPE zif_mcp_types=>task
                !json TYPE REF TO zif_mcp_ajson
      RAISING   zcx_mcp_ajson_error.
ENDCLASS.

CLASS zcl_mcp_resp_cancel_task IMPLEMENTATION.
  METHOD zif_mcp_internal~generate_json.
    result = zcl_mcp_ajson=>create_empty( ).
    serialize_task( path = ''
                    task = int_task
                    json = result ).
    IF int_meta IS BOUND.
      result->set( iv_path = '/_meta' iv_val = int_meta ).
    ENDIF.
  ENDMETHOD.

  METHOD serialize_task.
    DATA(p) = COND string( WHEN path IS INITIAL THEN '' ELSE |{ path }| ).
    json->set( iv_path         = |{ p }/taskId|
               iv_val          = task-task_id
               iv_ignore_empty = abap_false ).
    json->set( iv_path         = |{ p }/status|
               iv_val          = task-status
               iv_ignore_empty = abap_false ).
    IF task-status_message IS NOT INITIAL.
      json->set( iv_path = |{ p }/statusMessage|
                 iv_val  = task-status_message ).
    ENDIF.
    json->set( iv_path         = |{ p }/createdAt|
               iv_val          = zcl_mcp_util=>timestamp_to_iso8601( task-created_at )
               iv_ignore_empty = abap_false ).
    json->set( iv_path         = |{ p }/lastUpdatedAt|
               iv_val          = zcl_mcp_util=>timestamp_to_iso8601( task-last_updated )
               iv_ignore_empty = abap_false ).
    IF task-ttl_is_null = abap_true.
      json->set( iv_path      = |{ p }/ttl|
                 iv_val       = ''
                 iv_node_type = zif_mcp_ajson_types=>node_type-null ).
    ELSE.
      json->set( iv_path = |{ p }/ttl|
                 iv_val  = task-ttl ).
    ENDIF.
    IF task-poll_interval > 0.
      json->set( iv_path = |{ p }/pollInterval|
                 iv_val  = task-poll_interval ).
    ENDIF.
  ENDMETHOD.

  METHOD set_task.
    int_task = task.
  ENDMETHOD.

  METHOD set_meta.
    int_meta = meta.
  ENDMETHOD.
ENDCLASS.
