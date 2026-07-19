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
    CONSTANTS status_working   TYPE zmcp_task_status VALUE 'working'.
    CONSTANTS status_completed TYPE zmcp_task_status VALUE 'completed'.
    CONSTANTS status_failed    TYPE zmcp_task_status VALUE 'failed'.
    CONSTANTS status_cancelled TYPE zmcp_task_status VALUE 'cancelled'.

    "! Page size for tasks/list
    CONSTANTS page_size        TYPE i                VALUE 50.

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
        DATA temp1 TYPE REF TO zcx_mcp_server.
      DATA temp2 TYPE REF TO zcx_mcp_server.

    TRY.
        result = cl_system_uuid=>create_uuid_c32_static( ).
      CATCH cx_uuid_error.
        
        CREATE OBJECT temp1 TYPE zcx_mcp_server EXPORTING textid = zcx_mcp_server=>internal_error msgv1 = 'Failed to generate task ID'.
        RAISE EXCEPTION temp1 ##NO_TEXT.
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

    INSERT zmcp_tasks FROM db_task.
    IF sy-subrc <> 0.
      
      CREATE OBJECT temp2 TYPE zcx_mcp_server EXPORTING textid = zcx_mcp_server=>internal_error msgv1 = 'Failed to insert task'.
      RAISE EXCEPTION temp2 ##NO_TEXT.
    ENDIF.
  ENDMETHOD.

  METHOD get.
    DATA row TYPE zmcp_tasks.
      DATA temp3 TYPE symsgv.
      DATA temp1 TYPE REF TO zcx_mcp_server.
    row = read_task_row( task_id ).
    IF row-created_by <> sy-uname.
      
      temp3 = task_id.
      
      CREATE OBJECT temp1 TYPE zcx_mcp_server EXPORTING textid = zcx_mcp_server=>task_not_found msgv1 = temp3.
      RAISE EXCEPTION temp1.
    ENDIF.
    result = row_to_task( row ).
  ENDMETHOD.

  METHOD get_payload.
    DATA: BEGIN OF row,
            payload TYPE zmcp_tasks-payload,
            created_by TYPE zmcp_tasks-created_by,
          END OF row.
      DATA temp4 TYPE symsgv.
      DATA temp2 TYPE REF TO zcx_mcp_server.
      DATA temp5 TYPE symsgv.
      DATA temp3 TYPE REF TO zcx_mcp_server.
        DATA error TYPE REF TO zcx_mcp_ajson_error.
        DATA temp6 TYPE symsgv.
        DATA temp7 TYPE REF TO zcx_mcp_server.
    SELECT SINGLE payload created_by
      FROM zmcp_tasks INTO row
      WHERE task_id = task_id
      .

    IF sy-subrc <> 0 OR row-created_by <> sy-uname.
      
      temp4 = task_id.
      
      CREATE OBJECT temp2 TYPE zcx_mcp_server EXPORTING textid = zcx_mcp_server=>task_not_found msgv1 = temp4.
      RAISE EXCEPTION temp2.
    ENDIF.

    IF row-payload IS INITIAL.
      
      temp5 = |Task { task_id } has no stored result|.
      
      CREATE OBJECT temp3 TYPE zcx_mcp_server EXPORTING textid = zcx_mcp_server=>internal_error msgv1 = temp5.
      RAISE EXCEPTION temp3.
    ENDIF.

    TRY.
        result = zcl_mcp_ajson=>parse( row-payload ).
        
      CATCH zcx_mcp_ajson_error INTO error.
        
        temp6 = error->get_text( ).
        
        CREATE OBJECT temp7 TYPE zcx_mcp_server EXPORTING textid = zcx_mcp_server=>internal_error msgv1 = temp6.
        RAISE EXCEPTION temp7.
    ENDTRY.
  ENDMETHOD.

  METHOD list.
    TYPES temp1 TYPE TABLE OF zmcp_tasks.
DATA rows                TYPE temp1.
    DATA cursor_ts           TYPE timestamp.
    DATA cursor_task_id      TYPE sysuuid_c32.
    DATA cursor_ts_text      TYPE string.
    DATA cursor_task_id_text TYPE string.
    DATA fetch               TYPE i.
          DATA temp7 TYPE timestamp.
            DATA temp8 TYPE REF TO zcx_mcp_server.
          DATA temp9 TYPE REF TO zcx_mcp_server.
          DATA temp10 TYPE REF TO zcx_mcp_server.
      DATA last_row LIKE LINE OF rows.
      DATA temp12 LIKE LINE OF rows.
      DATA temp13 LIKE sy-tabix.
    DATA temp11 TYPE zif_mcp_types=>task_list.
    DATA row LIKE LINE OF rows.

    IF cursor IS INITIAL.
      cursor_ts      = '99991231235959'.
      cursor_task_id = 'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF'.
    ELSE.
      SPLIT cursor AT '|' INTO cursor_ts_text cursor_task_id_text.

      TRY.
          
          temp7 = cursor_ts_text.
          cursor_ts = temp7.
          IF cursor_ts = 0.
            
            CREATE OBJECT temp8 TYPE zcx_mcp_server EXPORTING textid = zcx_mcp_server=>invalid_arguments msgv1 = 'Invalid cursor'.
            RAISE EXCEPTION temp8 ##NO_TEXT.
          ENDIF.
        CATCH cx_sy_conversion_no_number
              cx_sy_conversion_overflow.
          
          CREATE OBJECT temp9 TYPE zcx_mcp_server EXPORTING textid = zcx_mcp_server=>invalid_arguments msgv1 = 'Invalid cursor'.
          RAISE EXCEPTION temp9 ##NO_TEXT.
      ENDTRY.

      IF cursor_task_id_text IS INITIAL.
        " Backward compatibility for timestamp-only cursors.
        cursor_task_id = 'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF'.
      ELSE.
        FIND REGEX '^[0-9A-Fa-f]{32}$' IN cursor_task_id_text.
        IF sy-subrc <> 0.
          
          CREATE OBJECT temp10 TYPE zcx_mcp_server EXPORTING textid = zcx_mcp_server=>invalid_arguments msgv1 = 'Invalid cursor'.
          RAISE EXCEPTION temp10 ##NO_TEXT.
        ENDIF.
        cursor_task_id = to_upper( cursor_task_id_text ).
      ENDIF.
    ENDIF.

    fetch = page_size + 1.

    SELECT task_id status status_message created_at last_updated ttl poll_interval
      FROM zmcp_tasks INTO CORRESPONDING FIELDS OF TABLE rows
      WHERE area       = int_area
        AND server     = int_server
        AND created_by = sy-uname
        AND (    created_at < cursor_ts
              OR ( created_at = cursor_ts AND task_id < cursor_task_id ) )
      ORDER BY created_at DESCENDING
               task_id DESCENDING
      
      UP TO fetch ROWS.                                "#EC CI_NOFIELD

    IF sy-subrc = 0 AND lines( rows ) > page_size.
      DELETE rows INDEX lines( rows ).
      
      
      
      temp13 = sy-tabix.
      READ TABLE rows INDEX lines( rows ) INTO temp12.
      sy-tabix = temp13.
      IF sy-subrc <> 0.
        RAISE EXCEPTION TYPE cx_sy_itab_line_not_found.
      ENDIF.
      last_row = temp12.
      result-next_cursor = |{ last_row-created_at }\|{ last_row-task_id }|.
    ENDIF.

    
    CLEAR temp11.
    
    LOOP AT rows INTO row.
      INSERT row_to_task( row ) INTO TABLE temp11.
    ENDLOOP.
    result-tasks = temp11.
  ENDMETHOD.

  METHOD update_status.
    DATA row TYPE zmcp_tasks.
      DATA temp13 TYPE symsgv.
      DATA temp15 TYPE REF TO zcx_mcp_server.
    DATA now TYPE p LENGTH 8 DECIMALS 0.
      DATA current_row TYPE zmcp_tasks.
      DATA temp14 TYPE symsgv.
      DATA temp16 TYPE REF TO zcx_mcp_server.
    row = read_task_row( task_id ).

    IF is_valid_transition( current = row-status
                            next    = status ) = abap_false.
      
      temp13 = |Invalid status transition { row-status } -> { status }|.
      
      CREATE OBJECT temp15 TYPE zcx_mcp_server EXPORTING textid = zcx_mcp_server=>internal_error msgv1 = temp13.
      RAISE EXCEPTION temp15 ##NO_TEXT.
    ENDIF.

    
    GET TIME STAMP FIELD now.

    UPDATE zmcp_tasks SET status         = status,
                          status_message = message,
                          last_updated   = now
      WHERE task_id = task_id
        AND status  = row-status.

    IF sy-subrc <> 0.
      
      current_row = read_task_row( task_id ).

      
      temp14 = |Invalid status transition { current_row-status } -> { status }|.
      
      CREATE OBJECT temp16 TYPE zcx_mcp_server EXPORTING textid = zcx_mcp_server=>internal_error msgv1 = temp14.
      RAISE EXCEPTION temp16 ##NO_TEXT.
    ENDIF.
  ENDMETHOD.

  METHOD set_payload.
        DATA payload_str TYPE string.
        DATA error TYPE REF TO zcx_mcp_ajson_error.
        DATA temp15 TYPE symsgv.
        DATA temp17 TYPE REF TO zcx_mcp_server.
    DATA now TYPE p LENGTH 8 DECIMALS 0.
      DATA row TYPE zmcp_tasks.
      DATA temp16 TYPE symsgv.
      DATA temp18 TYPE REF TO zcx_mcp_server.
    TRY.
        
        payload_str = payload->stringify( ).
        
      CATCH zcx_mcp_ajson_error INTO error.
        
        temp15 = error->get_text( ).
        
        CREATE OBJECT temp17 TYPE zcx_mcp_server EXPORTING textid = zcx_mcp_server=>internal_error msgv1 = temp15.
        RAISE EXCEPTION temp17.
    ENDTRY.

    
    GET TIME STAMP FIELD now.

    UPDATE zmcp_tasks SET payload      = payload_str,
                          last_updated = now
      WHERE task_id = task_id
        AND status  = status_working.

    IF sy-subrc <> 0.
      
      row = read_task_row( task_id ).

      
      temp16 = |Cannot store payload for task { task_id } in status { row-status }|.
      
      CREATE OBJECT temp18 TYPE zcx_mcp_server EXPORTING textid = zcx_mcp_server=>internal_error msgv1 = temp16.
      RAISE EXCEPTION temp18 ##NO_TEXT.
    ENDIF.
  ENDMETHOD.

  METHOD complete.
        DATA payload TYPE REF TO zif_mcp_ajson.
        DATA payload_str TYPE string.
        DATA error TYPE REF TO zcx_mcp_ajson_error.
        DATA temp17 TYPE symsgv.
        DATA temp21 TYPE REF TO zcx_mcp_server.
    DATA temp18 TYPE zmcp_task_status.
    DATA final_status LIKE temp18.
    DATA row TYPE zmcp_tasks.
      DATA temp19 TYPE symsgv.
      DATA temp22 TYPE REF TO zcx_mcp_server.
    DATA now TYPE p LENGTH 8 DECIMALS 0.
      DATA temp20 TYPE symsgv.
      DATA temp23 TYPE REF TO zcx_mcp_server.
    TRY.
        
        payload = result->zif_mcp_internal~generate_json( ).
        
        payload_str = payload->stringify( ).
        
      CATCH zcx_mcp_ajson_error INTO error.
        
        temp17 = error->get_text( ).
        
        CREATE OBJECT temp21 TYPE zcx_mcp_server EXPORTING textid = zcx_mcp_server=>internal_error msgv1 = temp17.
        RAISE EXCEPTION temp21.
    ENDTRY.

    
    IF result->get_is_error( ) = abap_true.
      temp18 = status_failed.
    ELSE.
      temp18 = status_completed.
    ENDIF.
    
    final_status = temp18.

    
    row = read_task_row( task_id ).
    IF is_valid_transition( current = row-status
                            next    = final_status ) = abap_false.
      
      temp19 = |Task has already reached a terminal state|.
      
      CREATE OBJECT temp22 TYPE zcx_mcp_server EXPORTING textid = zcx_mcp_server=>internal_error msgv1 = temp19.
      RAISE EXCEPTION temp22 ##NO_TEXT.
    ENDIF.

    
    GET TIME STAMP FIELD now.

    UPDATE zmcp_tasks
      SET payload      = payload_str,
          status       = final_status,
          last_updated = now
      WHERE task_id = task_id
        AND status  = row-status.

    IF sy-subrc <> 0.
      
      temp20 = |Task has already reached a terminal state|.
      
      CREATE OBJECT temp23 TYPE zcx_mcp_server EXPORTING textid = zcx_mcp_server=>internal_error msgv1 = temp20.
      RAISE EXCEPTION temp23 ##NO_TEXT.
    ENDIF.
  ENDMETHOD.

  METHOD fail.
    DATA row TYPE zmcp_tasks.
      DATA temp21 TYPE symsgv.
      DATA temp24 TYPE REF TO zcx_mcp_server.
    DATA now TYPE p LENGTH 8 DECIMALS 0.
      DATA temp22 TYPE symsgv.
      DATA temp25 TYPE REF TO zcx_mcp_server.
    row = read_task_row( task_id ).
    IF is_valid_transition( current = row-status
                            next    = status_failed ) = abap_false.
      
      temp21 = |Task has already reached a terminal state|.
      
      CREATE OBJECT temp24 TYPE zcx_mcp_server EXPORTING textid = zcx_mcp_server=>internal_error msgv1 = temp21.
      RAISE EXCEPTION temp24 ##NO_TEXT.
    ENDIF.

    
    GET TIME STAMP FIELD now.

    UPDATE zmcp_tasks
      SET status         = status_failed,
          status_message = message,
          last_updated   = now
      WHERE task_id = task_id
        AND status  = row-status.

    IF sy-subrc <> 0.
      
      temp22 = |Task has already reached a terminal state|.
      
      CREATE OBJECT temp25 TYPE zcx_mcp_server EXPORTING textid = zcx_mcp_server=>internal_error msgv1 = temp22.
      RAISE EXCEPTION temp25 ##NO_TEXT.
    ENDIF.
  ENDMETHOD.

  METHOD cancel.
    DATA: BEGIN OF row,
            task_id TYPE zmcp_tasks-task_id,
            status TYPE zmcp_tasks-status,
            created_by TYPE zmcp_tasks-created_by,
          END OF row.
      DATA temp23 TYPE symsgv.
      DATA temp27 TYPE REF TO zcx_mcp_server.
      DATA temp24 TYPE symsgv.
      DATA temp28 TYPE REF TO zcx_mcp_server.
    DATA now TYPE p LENGTH 8 DECIMALS 0.
DATA BEGIN OF current_row.
DATA status TYPE zmcp_tasks-status.
DATA created_by TYPE zmcp_tasks-created_by.
DATA END OF current_row.
      DATA temp25 TYPE symsgv.
      DATA temp29 TYPE REF TO zcx_mcp_server.
    DATA temp26 TYPE symsgv.
    DATA temp30 TYPE REF TO zcx_mcp_server.
    SELECT SINGLE task_id status created_by
      FROM zmcp_tasks INTO row
      WHERE task_id = task_id
      .

    IF sy-subrc <> 0 OR row-created_by <> sy-uname.
      
      temp23 = task_id.
      
      CREATE OBJECT temp27 TYPE zcx_mcp_server EXPORTING textid = zcx_mcp_server=>task_not_found msgv1 = temp23.
      RAISE EXCEPTION temp27.
    ENDIF.

    IF row-status = status_cancelled.
      RETURN.
    ENDIF.

    IF is_valid_transition( current = row-status
                            next    = status_cancelled ) = abap_false.
      
      temp24 = |Task has already reached a terminal state|.
      
      CREATE OBJECT temp28 TYPE zcx_mcp_server EXPORTING textid = zcx_mcp_server=>internal_error msgv1 = temp24.
      RAISE EXCEPTION temp28 ##NO_TEXT.
    ENDIF.

    
    GET TIME STAMP FIELD now.

    UPDATE zmcp_tasks
      SET status       = status_cancelled,
          last_updated = now
      WHERE task_id    = task_id
        AND created_by = sy-uname
        AND status     = row-status.

    IF sy-subrc = 0.
      RETURN.
    ENDIF.

    
    SELECT SINGLE status created_by
      FROM zmcp_tasks INTO current_row
      WHERE task_id = task_id
      .

    IF sy-subrc <> 0 OR current_row-created_by <> sy-uname.
      
      temp25 = task_id.
      
      CREATE OBJECT temp29 TYPE zcx_mcp_server EXPORTING textid = zcx_mcp_server=>task_not_found msgv1 = temp25.
      RAISE EXCEPTION temp29.
    ENDIF.

    IF current_row-status = status_cancelled.
      RETURN.
    ENDIF.

    
    temp26 = |Task has already reached a terminal state|.
    
    CREATE OBJECT temp30 TYPE zcx_mcp_server EXPORTING textid = zcx_mcp_server=>internal_error msgv1 = temp26.
    RAISE EXCEPTION temp30 ##NO_TEXT.
  ENDMETHOD.

  METHOD delete_outdated_tasks.
    DATA now TYPE timestamp.
    TYPES temp2 TYPE STANDARD TABLE OF zmcp_tasks.
DATA terminal_ttl_tasks TYPE temp2.
    TYPES temp3 TYPE RANGE OF sysuuid_c32.
DATA expired_task_ids TYPE temp3.
      FIELD-SYMBOLS <terminal_task> LIKE LINE OF terminal_ttl_tasks.
        DATA temp27 TYPE timestampl.
        DATA expiry_cutoff TYPE timestamp.
        DATA db_expiry_cutoff TYPE timestamp.
          DATA temp28 LIKE LINE OF expired_task_ids.
    DATA temp29 TYPE timestampl.
    DATA terminal_cutoff TYPE timestamp.
    DATA db_terminal_cutoff TYPE timestamp.
    DATA temp30 TYPE timestampl.
    DATA cutoff TYPE timestamp.
    DATA db_cutoff TYPE timestamp.

    GET TIME STAMP FIELD now.

    " Remove terminal tasks whose TTL has elapsed
    

    


    SELECT task_id last_updated ttl
      FROM zmcp_tasks INTO CORRESPONDING FIELDS OF TABLE terminal_ttl_tasks
      WHERE status IN (status_completed, status_failed, status_cancelled)
        AND ttl     > 0
      ORDER BY PRIMARY KEY
      . "#EC CI_NOFIELD

    IF sy-subrc = 0.
      
      LOOP AT terminal_ttl_tasks ASSIGNING <terminal_task>.
        
        temp27 = now.
        
        expiry_cutoff = cl_abap_tstmp=>subtractsecs( tstmp = temp27
                                                           secs  = <terminal_task>-ttl ).

        
        cl_abap_tstmp=>move( EXPORTING tstmp_src = expiry_cutoff
                             IMPORTING tstmp_tgt = db_expiry_cutoff ).

        IF <terminal_task>-last_updated < db_expiry_cutoff.
          
          CLEAR temp28.
          temp28-sign = 'I'.
          temp28-option = 'EQ'.
          temp28-low = <terminal_task>-task_id.
          APPEND temp28 TO expired_task_ids.
        ENDIF.
      ENDLOOP.

      IF expired_task_ids IS NOT INITIAL.
        DELETE FROM zmcp_tasks
          WHERE task_id IN expired_task_ids.
        result = sy-dbcnt.
      ENDIF.
    ENDIF.

    " Remove terminal tasks with no TTL after default retention (7 days)
    
    temp29 = now.
    
    terminal_cutoff = cl_abap_tstmp=>subtractsecs( tstmp = temp29
                                                         secs  = 604800 ).
    
    cl_abap_tstmp=>move( EXPORTING tstmp_src = terminal_cutoff
                         IMPORTING tstmp_tgt = db_terminal_cutoff ).

    DELETE FROM zmcp_tasks
      WHERE status       IN (status_completed, status_failed, status_cancelled)
        AND ttl           = 0
        AND last_updated  < db_terminal_cutoff.        "#EC CI_NOFIELD
    result = result + sy-dbcnt.

    " Remove stuck working tasks older than 24 hours
    
    temp30 = now.
    
    cutoff = cl_abap_tstmp=>subtractsecs( tstmp = temp30
                                                secs  = 86400 ).
    
    cl_abap_tstmp=>move( EXPORTING tstmp_src = cutoff
                         IMPORTING tstmp_tgt = db_cutoff ).

    DELETE FROM zmcp_tasks
      WHERE status     = status_working
        AND created_at < db_cutoff.                    "#EC CI_NOFIELD
    result = result + sy-dbcnt.

    IF result > 0.
      COMMIT WORK AND WAIT.
    ENDIF.
  ENDMETHOD.

  METHOD is_valid_transition.
    DATA temp1 TYPE xsdboolean.
    temp1 = boolc( current = status_working AND ( next = status_working OR next = status_completed OR next = status_failed OR next = status_cancelled ) ).
    result = temp1.
  ENDMETHOD.

  METHOD read_task_row.
      DATA temp31 TYPE symsgv.
      DATA temp32 TYPE REF TO zcx_mcp_server.
    SELECT SINGLE * FROM zmcp_tasks INTO CORRESPONDING FIELDS OF result
      WHERE task_id = task_id
      .

    IF sy-subrc <> 0.
      
      temp31 = task_id.
      
      CREATE OBJECT temp32 TYPE zcx_mcp_server EXPORTING textid = zcx_mcp_server=>task_not_found msgv1 = temp31.
      RAISE EXCEPTION temp32.
    ENDIF.
  ENDMETHOD.

  METHOD row_to_task.
    result-task_id        = row-task_id.
    result-status         = row-status.
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

ENDCLASS.
