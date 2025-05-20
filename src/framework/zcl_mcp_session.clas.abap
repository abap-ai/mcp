"! <p class="shorttext synchronized" lang="en">MCP Session Implementation</p>
CLASS zcl_mcp_session DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    CONSTANTS session_mode_stateless TYPE zmcp_session_mode VALUE ''.
    CONSTANTS session_mode_icf       TYPE zmcp_session_mode VALUE 'I'.
    CONSTANTS session_mode_mcp       TYPE zmcp_session_mode VALUE 'M'.
    CONSTANTS session_validity       TYPE i                 VALUE 3600.

    TYPES: BEGIN OF session_entry,
             key   TYPE string,
             value TYPE string,
           END OF session_entry,
           session_entries TYPE HASHED TABLE OF session_entry WITH UNIQUE KEY key.

    "! <p class="shorttext synchronized">Constructor</p>
    "! For session mode M we load the session data from the database.
    "! @parameter session_id   | <p class="shorttext synchronized">Session ID</p>
    "! @parameter session_mode | <p class="shorttext synchronized">Session Mode</p>
    "! @parameter create_new   | <p class="shorttext synchronized">Create new session</p>
    METHODS constructor IMPORTING session_id   TYPE sysuuid_c32
                                  session_mode TYPE zmcp_session_mode
                                  create_new   TYPE abap_bool DEFAULT abap_false
                        RAISING   zcx_mcp_server.

    "! <p class="shorttext synchronized">Get all entries</p>
    "!
    "! @parameter result | <p class="shorttext synchronized">Session Data</p>
    METHODS get_all RETURNING VALUE(result) TYPE session_entries.

    "! <p class="shorttext synchronized">Get an entry by key</p>
    "!
    "! @parameter key    | <p class="shorttext synchronized">Key</p>
    "! @parameter result | <p class="shorttext synchronized">Entry</p>
    METHODS get IMPORTING !key          TYPE string
                RETURNING VALUE(result) TYPE session_entry.

    "! <p class="shorttext synchronized">Add Data to the Session</p>
    "! In case the key already exists, the value will be updated.
    "! @parameter entry | <p class="shorttext synchronized">Entry</p>
    METHODS add IMPORTING !entry TYPE session_entry.

    "! <p class="shorttext synchronized">Remove an entry</p>
    "!
    "! @parameter key | <p class="shorttext synchronized">Key</p>
    METHODS remove IMPORTING !key TYPE string.

    "! <p class="shorttext synchronized">Clear all session data</p>
    "!
    METHODS clear.

    "! <p class="shorttext synchronized">Save session data</p>
    "! This is only relevant for session mode M.
    METHODS save RAISING zcx_mcp_server.

    "! <p class="shorttext synchronized">Delete session from DBy</p>
    METHODS delete.

    "! <p class="shorttext synchronized">Delete outdated sessions</p>
    "! Removes all sessions from the database that have not been updated
    "! for longer than session_validity plus a 10 minute grace period.
    "! @parameter result | <p class="shorttext synchronized">Number of deleted sessions</p>
    CLASS-METHODS delete_outdated_sessions
      RETURNING VALUE(result) TYPE i.

  PRIVATE SECTION.
    DATA session_id   TYPE string.
    DATA session_data TYPE session_entries.
    DATA session_mode TYPE zmcp_session_mode.
    DATA ajson        TYPE REF TO zcl_mcp_ajson.
ENDCLASS.

CLASS zcl_mcp_session IMPLEMENTATION.
  METHOD add.
    IF line_exists( session_data[ key = entry-key ] ).
      MODIFY TABLE session_data FROM entry.
    ELSE.
      INSERT entry INTO TABLE session_data.
    ENDIF.
  ENDMETHOD.

  METHOD clear.
    CLEAR session_data.
  ENDMETHOD.

  METHOD constructor.
    me->session_id   = session_id.
    me->session_mode = session_mode.

    " We only need to load the session data if we are in MCP mode and it is not a new session.
    IF session_mode <> session_mode_mcp OR create_new = abap_true.
      RETURN.
    ENDIF.

    SELECT SINGLE * FROM zmcp_sessions WHERE session_id = @session_id INTO @DATA(session).
    IF sy-subrc <> 0.
      RAISE EXCEPTION NEW zcx_mcp_server( textid = zcx_mcp_server=>session_unknown
                                          msgv1  = CONV #( session_id ) ).
    ENDIF.

    " Verify if the session is still valid
    DATA current_timestamp TYPE timestamp.
    GET TIME STAMP FIELD current_timestamp.
    DATA(timestamp_diff) = cl_abap_tstmp=>subtract( tstmp1 = current_timestamp
                                                    tstmp2 = session-updated ).
    IF timestamp_diff > session_validity.
      RAISE EXCEPTION NEW zcx_mcp_server( textid = zcx_mcp_server=>session_expired
                                          msgv1  = CONV #( session_id ) ).
    ENDIF.

    " Load the session data
    TRY.
        ajson = zcl_mcp_ajson=>parse( session-data ).
        ajson->to_abap( IMPORTING ev_container = session_data ).
      CATCH zcx_mcp_ajson_error.
        RAISE EXCEPTION NEW zcx_mcp_server( textid = zcx_mcp_server=>session_load_error
                                            msgv1  = `Could not parse JSON` ) ##NO_TEXT.
    ENDTRY.
  ENDMETHOD.

  METHOD remove.
    DELETE session_data WHERE key = key.
    IF sy-subrc <> 0. "#EC EMPTY_IF_BRANCH
      " Ignored, if it does not exist we cannot delete id.
      " Additional exception handling is not relevant.
    ENDIF.
  ENDMETHOD.

  METHOD get.
    READ TABLE session_data INTO result WITH KEY key = key.
    IF sy-subrc <> 0.
      CLEAR result.
    ENDIF.
  ENDMETHOD.

  METHOD get_all.
    result = session_data.
  ENDMETHOD.

  METHOD save.
    IF session_mode <> session_mode_mcp.
      RETURN.
    ENDIF.

    TRY.
        ajson = zcl_mcp_ajson=>create_empty( ).
        ajson->set( iv_path = '/'
                    iv_val  = session_data ).
        DATA(value) = ajson->stringify( ).
      CATCH zcx_mcp_ajson_error.
        RAISE EXCEPTION NEW zcx_mcp_server( textid = zcx_mcp_server=>session_save_error
                                            msgv1  = `Could not convert to JSON` ) ##NO_TEXT.
    ENDTRY.

    DATA db_session TYPE zmcp_sessions.
    db_session-session_id = session_id.
    db_session-data       = value.
    GET TIME STAMP FIELD db_session-updated.

    MODIFY zmcp_sessions FROM @db_session.
    IF sy-subrc <> 0.
      RAISE EXCEPTION NEW zcx_mcp_server( textid = zcx_mcp_server=>session_save_error
                                          msgv1  = `DB update failed.` ) ##NO_TEXT.
    ENDIF.
    COMMIT WORK AND WAIT.
  ENDMETHOD.

  METHOD delete.
    IF session_id IS NOT INITIAL.
      DELETE FROM zmcp_sessions WHERE session_id = @session_id.
      IF sy-subrc <> 0.
        " Ignored, we don't want to raise an exception if the session does not exist.
        RETURN.
      ENDIF.
      COMMIT WORK AND WAIT.
    ENDIF.
  ENDMETHOD.

  METHOD delete_outdated_sessions.
    DATA current_timestamp     TYPE timestampl.
    DATA outdated_timestamp    TYPE timestampl.
    DATA db_outdated_timestamp TYPE timestamp.

    " Get current detailed timestamp
    GET TIME STAMP FIELD current_timestamp.

    " Calculate outdated timestamp (current - (validity + 10min))
    DATA(lifetime) = session_validity + 600.
    outdated_timestamp = cl_abap_tstmp=>subtractsecs( tstmp = current_timestamp
                                                      secs  = lifetime ).

    " Convert to database timestamp format properly
    cl_abap_tstmp=>move( EXPORTING tstmp_src = outdated_timestamp
                         IMPORTING tstmp_tgt = db_outdated_timestamp ).

    " Delete outdated sessions
    DELETE FROM zmcp_sessions WHERE updated < @db_outdated_timestamp. "#EC CI_NOWHERE "#EC CI_NOFIELD

    result = sy-dbcnt.

    IF result > 0.
      COMMIT WORK AND WAIT.
    ENDIF.
  ENDMETHOD.


ENDCLASS.
