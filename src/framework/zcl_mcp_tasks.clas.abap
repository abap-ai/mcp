"! <p class="shorttext synchronized">MCP Task Manager</p>
"! Handles task persistence and lifecycle for MCP 2025-11-25.
"! Instance methods are used by the framework (HTTP context).
"! Class methods are intentionally usable from batch jobs and
"! background RFCs with no dependency on the server object.
CLASS zcl_mcp_tasks DEFINITION
  PUBLIC
  CREATE PUBLIC.

  PUBLIC SECTION.
    "! Status constants - match wire values in zmcp_task_status domain
    CONSTANTS status_working        TYPE zmcp_task_status VALUE 'working'.
    CONSTANTS status_completed      TYPE zmcp_task_status VALUE 'completed'.
    CONSTANTS status_failed         TYPE zmcp_task_status VALUE 'failed'.
    CONSTANTS status_cancelled      TYPE zmcp_task_status VALUE 'cancelled'.
    CONSTANTS status_input_required TYPE zmcp_task_status VALUE 'input_req'.

    "! Page size for tasks/list
    CONSTANTS page_size             TYPE i                VALUE 50.

    "! <p class="shorttext synchronized">Constructor</p>
    "! @parameter area   | <p class="shorttext synchronized">MCP area</p>
    "! @parameter server | <p class="shorttext synchronized">MCP server</p>
    METHODS constructor
      IMPORTING !area  TYPE zmcp_area
                server TYPE zmcp_server.

    "! <p class="shorttext synchronized">Create a new task</p>
    "! Creates the DB record in status 'working' and returns the new task ID.
    "! @parameter tool_name     | <p class="shorttext synchronized">Originating tool name</p>
    "! @parameter session_id    | <p class="shorttext synchronized">Originating session ID (optional)</p>
    "! @parameter ttl           | <p class="shorttext synchronized">Time to live in milliseconds (0 = no expiry)</p>
    "! @parameter poll_interval | <p class="shorttext synchronized">Suggested poll interval in milliseconds</p>
    "! @parameter result        | <p class="shorttext synchronized">New task ID</p>
    METHODS create_task
      IMPORTING tool_name     TYPE string
                session_id    TYPE sysuuid_c32 OPTIONAL
                ttl           TYPE i           DEFAULT 0
                poll_interval TYPE i           DEFAULT 5000
      RETURNING VALUE(result) TYPE sysuuid_c32
      RAISING   zcx_mcp_server ##NEEDED.

    "! <p class="shorttext synchronized">Get task header (no payload)</p>
    "! @parameter task_id | <p class="shorttext synchronized">Task ID</p>
    "! @parameter result  | <p class="shorttext synchronized">Task data</p>
    METHODS get
      IMPORTING task_id       TYPE sysuuid_c32
      RETURNING VALUE(result) TYPE zif_mcp_types=>task
      RAISING   zcx_mcp_server.

    "! <p class="shorttext synchronized">Get task payload</p>
    "! Only reads the payload column - use after confirming status is 'completed'.
    "! @parameter task_id | <p class="shorttext synchronized">Task ID</p>
    "! @parameter result  | <p class="shorttext synchronized">Payload JSON</p>
    METHODS get_payload
      IMPORTING task_id       TYPE sysuuid_c32
      RETURNING VALUE(result) TYPE REF TO zif_mcp_ajson
      RAISING   zcx_mcp_server.

    "! <p class="shorttext synchronized">List tasks with cursor pagination</p>
    "! Scoped to the area+server this instance was constructed with.
    "! @parameter cursor | <p class="shorttext synchronized">Pagination cursor (empty = first page)</p>
    "! @parameter result | <p class="shorttext synchronized">Task list</p>
    METHODS list
      IMPORTING !cursor       TYPE zif_mcp_types=>page_cursor OPTIONAL
      RETURNING VALUE(result) TYPE zif_mcp_types=>task_list_result
      RAISING   zcx_mcp_server.

    "! <p class="shorttext synchronized">Delete outdated tasks</p>
    "! Removes completed/failed/cancelled tasks whose TTL has elapsed,
    "! and working tasks older than the max lifetime.
    "! @parameter result | <p class="shorttext synchronized">Number of deleted tasks</p>
    CLASS-METHODS delete_outdated_tasks
      RETURNING VALUE(result) TYPE i.

    "! <p class="shorttext synchronized">Update task status</p>
    "! Enforces valid status transitions - raises zcx_mcp_server on illegal moves.
    "! Safe to call from batch jobs and background RFCs.
    "! @parameter task_id | <p class="shorttext synchronized">Task ID</p>
    "! @parameter status  | <p class="shorttext synchronized">New status</p>
    "! @parameter message | <p class="shorttext synchronized">Optional status message</p>
    CLASS-METHODS update_status
      IMPORTING task_id  TYPE sysuuid_c32
                !status  TYPE zmcp_task_status
                !message TYPE string OPTIONAL
      RAISING   zcx_mcp_server.

    "! <p class="shorttext synchronized">Request client input for a task</p>
    "! Stores pending input request JSON and moves the task to input_required.
    "! Safe to call from batch jobs and background RFCs.
    "! @parameter task_id        | <p class="shorttext synchronized">Task ID</p>
    "! @parameter input_required | <p class="shorttext synchronized">InputRequiredResult JSON</p>
    CLASS-METHODS request_input
      IMPORTING task_id        TYPE sysuuid_c32
                input_required TYPE REF TO zif_mcp_ajson
      RAISING   zcx_mcp_server.

    "! <p class="shorttext synchronized">Consume client input for a task</p>
    "! Stores task input responses and moves the task back to working.
    "! Safe to call from batch jobs and background RFCs.
    "! @parameter task_id         | <p class="shorttext synchronized">Task ID</p>
    "! @parameter input_responses | <p class="shorttext synchronized">Input responses JSON</p>
    "! @parameter request_state   | <p class="shorttext synchronized">Opaque request state</p>
    CLASS-METHODS consume_update
      IMPORTING task_id         TYPE sysuuid_c32
                input_responses TYPE REF TO zif_mcp_ajson
                request_state   TYPE string OPTIONAL
      RAISING   zcx_mcp_server.

    "! <p class="shorttext synchronized">Store task payload</p>
    "! Can be called incrementally while the task is working - overwrites any existing payload.
    "! Safe to call from batch jobs and background RFCs.
    "! @parameter task_id | <p class="shorttext synchronized">Task ID</p>
    "! @parameter payload | <p class="shorttext synchronized">Result payload as JSON</p>
    CLASS-METHODS set_payload
      IMPORTING task_id TYPE sysuuid_c32
                payload TYPE REF TO zif_mcp_ajson
      RAISING   zcx_mcp_server.

    "! <p class="shorttext synchronized">Complete a task</p>
    "! Convenience: serialises the result object into a CallToolResult-shaped
    "! payload, stores it and transitions status to 'completed' atomically.
    "! Safe to call from batch jobs and background RFCs.
    "! @parameter task_id | <p class="shorttext synchronized">Task ID</p>
    "! @parameter result  | <p class="shorttext synchronized">Completed task result</p>
    CLASS-METHODS complete
      IMPORTING task_id TYPE sysuuid_c32
                !result TYPE REF TO zcl_mcp_resp_task_payload
      RAISING   zcx_mcp_server.

    "! <p class="shorttext synchronized">Fail a task</p>
    "! Convenience: transitions status to 'failed' with an error message.
    "! Safe to call from batch jobs and background RFCs.
    "! @parameter task_id | <p class="shorttext synchronized">Task ID</p>
    "! @parameter message | <p class="shorttext synchronized">Error description</p>
    CLASS-METHODS fail
      IMPORTING task_id  TYPE sysuuid_c32
                !message TYPE string
      RAISING   zcx_mcp_server.

    "! <p class="shorttext synchronized">Cancel a task</p>
    "! Transitions the current user's working task to 'cancelled'.
    "! Idempotent when the task is already cancelled.
    "! User-scoped cancellation for client/API calls.
    "! @parameter task_id | <p class="shorttext synchronized">Task ID</p>
    CLASS-METHODS cancel
      IMPORTING task_id TYPE sysuuid_c32
      RAISING   zcx_mcp_server.

    "! <p class="shorttext synchronized">Get current task status</p>
    "! Reads the task status without ownership checks.
    "! Safe to call from batch jobs and background RFCs.
    "! @parameter task_id | <p class="shorttext synchronized">Task ID</p>
    "! @parameter result  | <p class="shorttext synchronized">Current task status</p>
    CLASS-METHODS get_status
      IMPORTING task_id       TYPE sysuuid_c32
      RETURNING VALUE(result) TYPE zmcp_task_status
      RAISING   zcx_mcp_server.

  PRIVATE SECTION.
    DATA int_area   TYPE zmcp_area.
    DATA int_server TYPE zmcp_server.

    "! Valid status transitions - key = current, value table = allowed next states
    CLASS-METHODS is_valid_transition
      IMPORTING current       TYPE zmcp_task_status
                next          TYPE zmcp_task_status
      RETURNING VALUE(result) TYPE abap_bool.

    CLASS-METHODS read_task_row
      IMPORTING task_id       TYPE sysuuid_c32
      RETURNING VALUE(result) TYPE zmcp_tasks
      RAISING   zcx_mcp_server.

    CLASS-METHODS row_to_task
      IMPORTING row           TYPE zmcp_tasks
      RETURNING VALUE(result) TYPE zif_mcp_types=>task.

ENDCLASS.

CLASS zcl_mcp_tasks IMPLEMENTATION.

  METHOD constructor.
    int_area   = area.
    int_server = server.
  ENDMETHOD.

  METHOD create_task.

    DATA db_task TYPE zmcp_tasks.

    TRY.
        result = cl_system_uuid=>create_uuid_c32_static( ).
      CATCH cx_uuid_error.
        RAISE EXCEPTION NEW zcx_mcp_server( textid = zcx_mcp_server=>internal_error
                                            msgv1  = 'Failed to generate task ID' ) ##NO_TEXT.
    ENDTRY.

    db_task-client        = sy-mandt.
    db_task-task_id       = result.
    db_task-area          = int_area.
    db_task-server        = int_server.
    db_task-created_by    = sy-uname.
    db_task-session_id    = session_id.
    db_task-status        = status_working.
    db_task-poll_interval = poll_interval.
    " ttl = requested lifetime in milliseconds, 0 = unlimited
    IF ttl > 0.
      db_task-ttl = ( ttl + 999 ) DIV 1000.
    ELSE.
      db_task-ttl = 0.
    ENDIF.
    GET TIME STAMP FIELD db_task-created_at.
    db_task-last_updated = db_task-created_at.

    INSERT zmcp_tasks FROM @db_task.
    IF sy-subrc <> 0.
      RAISE EXCEPTION NEW zcx_mcp_server( textid = zcx_mcp_server=>internal_error
                                          msgv1  = 'Failed to insert task' ) ##NO_TEXT.
    ENDIF.
  ENDMETHOD.

  METHOD get.
    DATA(row) = read_task_row( task_id ).
    IF row-created_by <> sy-uname.
      RAISE EXCEPTION NEW zcx_mcp_server(
        textid = zcx_mcp_server=>task_not_found
        msgv1  = CONV #( task_id ) ).
    ENDIF.
    result = row_to_task( row ).
  ENDMETHOD.

  METHOD get_payload.
    SELECT SINGLE payload, created_by
      FROM zmcp_tasks
      WHERE task_id = @task_id
      INTO @DATA(row).

    IF sy-subrc <> 0 OR row-created_by <> sy-uname.
      RAISE EXCEPTION NEW zcx_mcp_server( textid = zcx_mcp_server=>task_not_found
                                          msgv1  = CONV #( task_id ) ).
    ENDIF.

    IF row-payload IS INITIAL.
      RAISE EXCEPTION NEW zcx_mcp_server( textid = zcx_mcp_server=>internal_error
                                          msgv1  = CONV #( |Task { task_id } has no stored result| ) ) ##NO_TEXT.
    ENDIF.

    TRY.
        result = zcl_mcp_ajson=>parse( row-payload ).
      CATCH zcx_mcp_ajson_error INTO DATA(error).
        RAISE EXCEPTION NEW zcx_mcp_server( textid = zcx_mcp_server=>internal_error
                                            msgv1  = CONV #( error->get_text( ) ) ).
    ENDTRY.
  ENDMETHOD.

  METHOD list.
    DATA rows                TYPE TABLE OF zmcp_tasks.
    DATA cursor_ts           TYPE timestamp.
    DATA cursor_task_id      TYPE sysuuid_c32.
    DATA cursor_ts_text      TYPE string.
    DATA cursor_task_id_text TYPE string.
    DATA fetch               TYPE i.

    IF cursor IS INITIAL.
      cursor_ts      = '99991231235959'.
      cursor_task_id = 'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF'.
    ELSE.
      SPLIT cursor AT '|' INTO cursor_ts_text cursor_task_id_text.

      TRY.
          cursor_ts = CONV timestamp( cursor_ts_text ).
          IF cursor_ts = 0.
            RAISE EXCEPTION NEW zcx_mcp_server( textid = zcx_mcp_server=>invalid_arguments
                                                msgv1  = 'Invalid cursor' ) ##NO_TEXT.
          ENDIF.
        CATCH cx_sy_conversion_no_number
              cx_sy_conversion_overflow.
          RAISE EXCEPTION NEW zcx_mcp_server( textid = zcx_mcp_server=>invalid_arguments
                                              msgv1  = 'Invalid cursor' ) ##NO_TEXT.
      ENDTRY.

      IF cursor_task_id_text IS INITIAL.
        " Backward compatibility for timestamp-only cursors.
        cursor_task_id = 'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF'.
      ELSE.
        FIND REGEX '^[0-9A-Fa-f]{32}$' IN cursor_task_id_text ##NO_TEXT.
        IF sy-subrc <> 0.
          RAISE EXCEPTION NEW zcx_mcp_server( textid = zcx_mcp_server=>invalid_arguments
                                              msgv1  = 'Invalid cursor' ) ##NO_TEXT.
        ENDIF.
        cursor_task_id = to_upper( cursor_task_id_text ).
      ENDIF.
    ENDIF.

    fetch = page_size + 1.

    SELECT task_id, status, status_message, created_at, last_updated, ttl, poll_interval
      FROM zmcp_tasks
      WHERE area       = @int_area
        AND server     = @int_server
        AND created_by = @sy-uname
        AND (    created_at < @cursor_ts
              OR ( created_at = @cursor_ts AND task_id < @cursor_task_id ) )
      ORDER BY created_at DESCENDING,
               task_id DESCENDING
      INTO CORRESPONDING FIELDS OF TABLE @rows
      UP TO @fetch ROWS.                                "#EC CI_NOFIELD

    IF sy-subrc = 0 AND lines( rows ) > page_size.
      DELETE rows INDEX lines( rows ).
      DATA(last_row) = rows[ lines( rows ) ].
      result-next_cursor = |{ last_row-created_at }\|{ last_row-task_id }|.
    ENDIF.

    result-tasks = VALUE #( FOR row IN rows
                            ( row_to_task( row ) ) ).
  ENDMETHOD.

  METHOD update_status.
    DATA(row) = read_task_row( task_id ).

    IF is_valid_transition( current = row-status
                            next    = status ) = abap_false.
      RAISE EXCEPTION NEW zcx_mcp_server( textid = zcx_mcp_server=>internal_error
                                          msgv1  = CONV #( |Invalid status transition { row-status } -> { status }| ) ) ##NO_TEXT.
    ENDIF.

    GET TIME STAMP FIELD DATA(now).

    UPDATE zmcp_tasks SET status         = @status,
                          status_message = @message,
                          last_updated   = @now
      WHERE task_id = @task_id
        AND status  = @row-status.

    IF sy-subrc <> 0.
      DATA(current_row) = read_task_row( task_id ).

      RAISE EXCEPTION NEW zcx_mcp_server(
                              textid = zcx_mcp_server=>internal_error
                              msgv1  = CONV #( |Invalid status transition { current_row-status } -> { status }| ) ) ##NO_TEXT.
    ENDIF.
  ENDMETHOD.

  METHOD set_payload.
    TRY.
        DATA(payload_str) = payload->stringify( ).
      CATCH zcx_mcp_ajson_error INTO DATA(error).
        RAISE EXCEPTION NEW zcx_mcp_server( textid = zcx_mcp_server=>internal_error
                                            msgv1  = CONV #( error->get_text( ) ) ).
    ENDTRY.

    GET TIME STAMP FIELD DATA(now).

    UPDATE zmcp_tasks SET payload      = @payload_str,
                          last_updated = @now
      WHERE task_id = @task_id
        AND status  = @status_working.

    IF sy-subrc <> 0.
      DATA(row) = read_task_row( task_id ).

      RAISE EXCEPTION NEW zcx_mcp_server(
                              textid = zcx_mcp_server=>internal_error
                              msgv1  = CONV #( |Cannot store payload for task { task_id } in status { row-status }| ) ) ##NO_TEXT.
    ENDIF.
  ENDMETHOD.

  METHOD complete.
    TRY.
        DATA(payload) = result->zif_mcp_internal~generate_json( ).
        DATA(payload_str) = payload->stringify( ).
      CATCH zcx_mcp_ajson_error INTO DATA(error).
        RAISE EXCEPTION NEW zcx_mcp_server( textid = zcx_mcp_server=>internal_error
                                            msgv1  = CONV #( error->get_text( ) ) ).
    ENDTRY.

    DATA(final_status) = COND zmcp_task_status(
      WHEN result->get_is_error( ) = abap_true
      THEN status_failed
      ELSE status_completed ).

    DATA(row) = read_task_row( task_id ).
    IF is_valid_transition( current = row-status
                            next    = final_status ) = abap_false.
      RAISE EXCEPTION NEW zcx_mcp_server( textid = zcx_mcp_server=>internal_error
                                          msgv1  = CONV #( |Task has already reached a terminal state| ) ) ##NO_TEXT.
    ENDIF.

    GET TIME STAMP FIELD DATA(now).

    UPDATE zmcp_tasks
      SET payload      = @payload_str,
          status       = @final_status,
          last_updated = @now
      WHERE task_id = @task_id
        AND status  = @row-status.

    IF sy-subrc <> 0.
      RAISE EXCEPTION NEW zcx_mcp_server( textid = zcx_mcp_server=>internal_error
                                          msgv1  = CONV #( |Task has already reached a terminal state| ) ) ##NO_TEXT.
    ENDIF.
  ENDMETHOD.

  METHOD fail.
    DATA(row) = read_task_row( task_id ).
    IF is_valid_transition( current = row-status
                            next    = status_failed ) = abap_false.
      RAISE EXCEPTION NEW zcx_mcp_server( textid = zcx_mcp_server=>internal_error
                                          msgv1  = CONV #( |Task has already reached a terminal state| ) ) ##NO_TEXT.
    ENDIF.

    GET TIME STAMP FIELD DATA(now).

    UPDATE zmcp_tasks
      SET status         = @status_failed,
          status_message = @message,
          last_updated   = @now
      WHERE task_id = @task_id
        AND status  = @row-status.

    IF sy-subrc <> 0.
      RAISE EXCEPTION NEW zcx_mcp_server( textid = zcx_mcp_server=>internal_error
                                          msgv1  = CONV #( |Task has already reached a terminal state| ) ) ##NO_TEXT.
    ENDIF.
  ENDMETHOD.

  METHOD cancel.
    SELECT SINGLE task_id, status, created_by
      FROM zmcp_tasks
      WHERE task_id = @task_id
      INTO @DATA(row).

    IF sy-subrc <> 0 OR row-created_by <> sy-uname.
      RAISE EXCEPTION NEW zcx_mcp_server( textid = zcx_mcp_server=>task_not_found
                                          msgv1  = CONV #( task_id ) ).
    ENDIF.

    IF row-status = status_cancelled.
      RETURN.
    ENDIF.

    IF is_valid_transition( current = row-status
                            next    = status_cancelled ) = abap_false.
      RAISE EXCEPTION NEW zcx_mcp_server( textid = zcx_mcp_server=>internal_error
                                          msgv1  = CONV #( |Task has already reached a terminal state| ) ) ##NO_TEXT.
    ENDIF.

    GET TIME STAMP FIELD DATA(now).

    UPDATE zmcp_tasks
      SET status       = @status_cancelled,
          last_updated = @now
      WHERE task_id    = @task_id
        AND created_by = @sy-uname
        AND status     = @row-status.

    IF sy-subrc = 0.
      RETURN.
    ENDIF.

    SELECT SINGLE status, created_by
      FROM zmcp_tasks
      WHERE task_id = @task_id
      INTO @DATA(current_row).

    IF sy-subrc <> 0 OR current_row-created_by <> sy-uname.
      RAISE EXCEPTION NEW zcx_mcp_server( textid = zcx_mcp_server=>task_not_found
                                          msgv1  = CONV #( task_id ) ).
    ENDIF.

    IF current_row-status = status_cancelled.
      RETURN.
    ENDIF.

    RAISE EXCEPTION NEW zcx_mcp_server( textid = zcx_mcp_server=>internal_error
                                        msgv1  = CONV #( |Task has already reached a terminal state| ) ) ##NO_TEXT.
  ENDMETHOD.

  METHOD delete_outdated_tasks.
    DATA now TYPE timestamp.

    GET TIME STAMP FIELD now.

    " Remove terminal tasks whose TTL has elapsed
    DATA terminal_ttl_tasks TYPE STANDARD TABLE OF zmcp_tasks.
    DATA expired_task_ids   TYPE RANGE OF sysuuid_c32.

    SELECT task_id, last_updated, ttl
      FROM zmcp_tasks
      WHERE status IN ( @status_completed, @status_failed, @status_cancelled )
        AND ttl     > 0
      ORDER BY PRIMARY KEY
      INTO CORRESPONDING FIELDS OF TABLE @terminal_ttl_tasks. "#EC CI_NOFIELD

    IF sy-subrc = 0.
      LOOP AT terminal_ttl_tasks ASSIGNING FIELD-SYMBOL(<terminal_task>).
        DATA(expiry_cutoff) = cl_abap_tstmp=>subtractsecs( tstmp = CONV timestampl( now )
                                                           secs  = <terminal_task>-ttl ).

        DATA db_expiry_cutoff TYPE timestamp.
        cl_abap_tstmp=>move( EXPORTING tstmp_src = expiry_cutoff
                             IMPORTING tstmp_tgt = db_expiry_cutoff ).

        IF <terminal_task>-last_updated < db_expiry_cutoff.
          APPEND VALUE #( sign   = 'I'
                          option = 'EQ'
                          low    = <terminal_task>-task_id ) TO expired_task_ids.
        ENDIF.
      ENDLOOP.

      IF expired_task_ids IS NOT INITIAL.
        DELETE FROM zmcp_tasks
          WHERE task_id IN @expired_task_ids.
        result = sy-dbcnt.
      ENDIF.
    ENDIF.

    " Remove terminal tasks with no TTL after default retention (7 days)
    DATA(terminal_cutoff) = cl_abap_tstmp=>subtractsecs( tstmp = CONV timestampl( now )
                                                         secs  = 604800 ).
    DATA db_terminal_cutoff TYPE timestamp.
    cl_abap_tstmp=>move( EXPORTING tstmp_src = terminal_cutoff
                         IMPORTING tstmp_tgt = db_terminal_cutoff ).

    DELETE FROM zmcp_tasks
      WHERE status       IN ( @status_completed, @status_failed, @status_cancelled )
        AND ttl           = 0
        AND last_updated  < @db_terminal_cutoff.        "#EC CI_NOFIELD
    result = result + sy-dbcnt.

    " Remove stuck working tasks older than 24 hours
    DATA(cutoff) = cl_abap_tstmp=>subtractsecs( tstmp = CONV timestampl( now )
                                                secs  = 86400 ).
    DATA db_cutoff TYPE timestamp.
    cl_abap_tstmp=>move( EXPORTING tstmp_src = cutoff
                         IMPORTING tstmp_tgt = db_cutoff ).

    DELETE FROM zmcp_tasks
      WHERE status     IN ( @status_working, @status_input_required )
        AND created_at  < @db_cutoff.                    "#EC CI_NOFIELD
    result = result + sy-dbcnt.

    IF result > 0.
      COMMIT WORK AND WAIT.
    ENDIF.
  ENDMETHOD.

  METHOD is_valid_transition.
    result = xsdbool(
         (     current = status_working
           AND (    next = status_working
                 OR next = status_input_required
                 OR next = status_completed
                 OR next = status_failed
                 OR next = status_cancelled ) )
      OR (     current = status_input_required
           AND (    next = status_working
                 OR next = status_completed
                 OR next = status_failed
                 OR next = status_cancelled ) ) ).
  ENDMETHOD.

  METHOD read_task_row.
    SELECT SINGLE * FROM zmcp_tasks
      WHERE task_id = @task_id
      INTO CORRESPONDING FIELDS OF @result.

    IF sy-subrc <> 0.
      RAISE EXCEPTION NEW zcx_mcp_server( textid = zcx_mcp_server=>task_not_found
                                          msgv1  = CONV #( task_id ) ).
    ENDIF.
  ENDMETHOD.

  METHOD row_to_task.
    result-task_id        = row-task_id.
    result-status         = COND #(
             WHEN row-status = status_input_required
             THEN zif_mcp_types=>task_states-input_required
             ELSE row-status ).
    result-status_message = row-status_message.
    result-created_at     = row-created_at.
    result-last_updated   = row-last_updated.
    IF row-ttl > 0.
      result-ttl         = row-ttl * 1000.
      result-ttl_is_null = abap_false.
    ELSE.
      result-ttl         = 0.
      result-ttl_is_null = abap_true.
    ENDIF.
    result-poll_interval = row-poll_interval.
  ENDMETHOD.

  METHOD get_status.
    result = read_task_row( task_id )-status.
  ENDMETHOD.

  METHOD request_input.
    DATA(row) = read_task_row( task_id ).

    IF is_valid_transition( current = row-status
                            next    = status_input_required ) = abap_false.
      RAISE EXCEPTION NEW zcx_mcp_server(
          textid = zcx_mcp_server=>internal_error
          msgv1  = CONV #( |Invalid status transition { row-status } -> { status_input_required }| ) ) ##NO_TEXT.
    ENDIF.

    IF input_required IS NOT BOUND.
      RAISE EXCEPTION NEW zcx_mcp_server( textid = zcx_mcp_server=>invalid_arguments
                                          msgv1  = 'input_required' ) ##NO_TEXT.
    ENDIF.

    TRY.
        DATA(payload_str) = input_required->stringify( ).
      CATCH zcx_mcp_ajson_error INTO DATA(error).
        RAISE EXCEPTION NEW zcx_mcp_server( textid = zcx_mcp_server=>internal_error
                                            msgv1  = CONV #( error->get_text( ) ) ).
    ENDTRY.

    GET TIME STAMP FIELD DATA(now).

    UPDATE zmcp_tasks
      SET payload      = @payload_str,
          status       = @status_input_required,
          last_updated = @now
      WHERE task_id = @task_id
        AND status  = @row-status.

    IF sy-subrc <> 0.
      DATA(current_row) = read_task_row( task_id ).
      RAISE EXCEPTION NEW zcx_mcp_server(
          textid = zcx_mcp_server=>internal_error
          msgv1  = CONV #( |Invalid status transition { current_row-status } -> { status_input_required }| ) ) ##NO_TEXT.
    ENDIF.
  ENDMETHOD.

  METHOD consume_update.
    DATA(row) = read_task_row( task_id ).

    IF row-status <> status_input_required.
      RAISE EXCEPTION NEW zcx_mcp_server( textid = zcx_mcp_server=>invalid_arguments
                                          msgv1  = CONV #( |Task { task_id } is not waiting for input| ) ) ##NO_TEXT.
    ENDIF.

    TRY.
        DATA(payload) = zcl_mcp_ajson=>create_empty( ).

        IF input_responses IS BOUND.
          payload->set( iv_path = `/inputResponses`
                        iv_val  = input_responses ).
        ELSE.
          payload->touch_object( `/inputResponses` ).
        ENDIF.

        IF request_state IS NOT INITIAL.
          payload->set_string( iv_path = `/requestState`
                               iv_val  = request_state ).
        ENDIF.

        DATA(payload_str) = payload->stringify( ).

      CATCH zcx_mcp_ajson_error INTO DATA(error).
        RAISE EXCEPTION NEW zcx_mcp_server( textid = zcx_mcp_server=>internal_error
                                            msgv1  = CONV #( error->get_text( ) ) ).
    ENDTRY.

    GET TIME STAMP FIELD DATA(now).

    UPDATE zmcp_tasks
      SET payload      = @payload_str,
          status       = @status_working,
          last_updated = @now
      WHERE task_id = @task_id
        AND status  = @status_input_required.

    IF sy-subrc <> 0.
      DATA(current_row) = read_task_row( task_id ).
      RAISE EXCEPTION NEW zcx_mcp_server(
          textid = zcx_mcp_server=>internal_error
          msgv1  = CONV #( |Invalid status transition { current_row-status } -> { status_working }| ) ) ##NO_TEXT.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
