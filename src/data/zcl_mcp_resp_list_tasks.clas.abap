CLASS zcl_mcp_resp_list_tasks DEFINITION
  PUBLIC FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES zif_mcp_internal.

    "! <p class="shorttext synchronized">Set Tasks</p>
    "!
    "! @parameter tasks | <p class="shorttext synchronized">List of tasks</p>
    METHODS set_tasks
      IMPORTING tasks TYPE zif_mcp_types=>task_list.

    "! <p class="shorttext synchronized">Set Next Cursor</p>
    "!
    "! @parameter next_cursor | <p class="shorttext synchronized">Pagination cursor</p>
    METHODS set_next_cursor
      IMPORTING next_cursor TYPE zif_mcp_types=>page_cursor OPTIONAL.

    "! <p class="shorttext synchronized">Set Meta Data</p>
    "!
    "! @parameter meta | <p class="shorttext synchronized">Meta data</p>
    METHODS set_meta
      IMPORTING meta TYPE REF TO zif_mcp_ajson.

  PRIVATE SECTION.
    DATA int_tasks       TYPE zif_mcp_types=>task_list.
    DATA int_next_cursor TYPE zif_mcp_types=>page_cursor.
    DATA int_meta        TYPE REF TO zif_mcp_ajson.

    METHODS serialize_task
      IMPORTING !path TYPE string
                !task TYPE zif_mcp_types=>task
                !json TYPE REF TO zif_mcp_ajson
      RAISING   zcx_mcp_ajson_error.

ENDCLASS.

CLASS zcl_mcp_resp_list_tasks IMPLEMENTATION.
  METHOD zif_mcp_internal~generate_json.
    FIELD-SYMBOLS <task> LIKE LINE OF int_tasks.
    result = zcl_mcp_ajson=>create_empty( ).
    result->touch_array( '/tasks' ).

    
    LOOP AT int_tasks ASSIGNING <task>.
      serialize_task( path = |/tasks/{ sy-tabix }|
                      task = <task>
                      json = result ).
    ENDLOOP.

    IF int_next_cursor IS NOT INITIAL.
      result->set( iv_path = '/nextCursor'
                   iv_val  = int_next_cursor ).
    ENDIF.
    IF int_meta IS BOUND.
      result->set( iv_path = '/_meta' iv_val = int_meta ).
    ENDIF.
  ENDMETHOD.

  METHOD serialize_task.
    json->set( iv_path         = |{ path }/taskId|
               iv_val          = task-task_id
               iv_ignore_empty = abap_false ).
    json->set( iv_path         = |{ path }/status|
               iv_val          = task-status
               iv_ignore_empty = abap_false ).
    IF task-status_message IS NOT INITIAL.
      json->set( iv_path = |{ path }/statusMessage|
                 iv_val  = task-status_message ).
    ENDIF.
    json->set( iv_path         = |{ path }/createdAt|
               iv_val          = zcl_mcp_util=>timestamp_to_iso8601( task-created_at )
               iv_ignore_empty = abap_false ).
    json->set( iv_path         = |{ path }/lastUpdatedAt|
               iv_val          = zcl_mcp_util=>timestamp_to_iso8601( task-last_updated )
               iv_ignore_empty = abap_false ).
    IF task-ttl_is_null = abap_true.
      json->set( iv_path      = |{ path }/ttl|
                 iv_val       = ''
                 iv_node_type = zif_mcp_ajson_types=>node_type-null ).
    ELSE.
      json->set( iv_path = |{ path }/ttl|
                 iv_val  = task-ttl ).
    ENDIF.
    IF task-poll_interval > 0.
      json->set( iv_path = |{ path }/pollInterval|
                 iv_val  = task-poll_interval ).
    ENDIF.
  ENDMETHOD.

  METHOD set_tasks.
    int_tasks = tasks.
  ENDMETHOD.

  METHOD set_next_cursor.
    int_next_cursor = next_cursor.
  ENDMETHOD.

  METHOD set_meta.
    int_meta = meta.
  ENDMETHOD.
ENDCLASS.
