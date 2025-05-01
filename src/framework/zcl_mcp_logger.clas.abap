"! <p class="shorttext synchronized" lang="en">MCP Logger</p>
"! Simple logger wrapper for Application Log (BAL) with JSON attachment support
"! and configurable log level filtering.
CLASS zcl_mcp_logger DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    CONSTANTS:
      BEGIN OF log_levels,
        off     TYPE i VALUE 0,
        info    TYPE i VALUE 1,
        warning TYPE i VALUE 2,
        error   TYPE i VALUE 3,
      END OF log_levels.

    "! <p class="shorttext synchronized">Create a new logger instance</p>
    "! Initialize a new application log with specified parameters
    "!
    "! @parameter object       | <p class="shorttext synchronized">BAL object (application area)</p>
    "! @parameter subobject    | <p class="shorttext synchronized">BAL subobject (specific component)</p>
    "! @parameter log_level    | <p class="shorttext synchronized">Minimum level for messages to be logged</p>
    "! @parameter external_id  | <p class="shorttext synchronized">External ID for correlation with business processes</p>
    METHODS constructor
      IMPORTING !object     TYPE balobj_d
                subobject   TYPE balsubobj
                log_level   TYPE i         DEFAULT log_levels-info
                external_id TYPE balnrext  OPTIONAL.

    "! <p class="shorttext synchronized">Log an informational message</p>
    "! Creates an info message entry if current log level permits (level >= info)
    "!
    "! @parameter message   | <p class="shorttext synchronized">The message text to be logged</p>
    METHODS info
      IMPORTING !message TYPE string.

    "! <p class="shorttext synchronized">Log a warning message</p>
    "! Creates a warning message entry if current log level permits (level >= warning)
    "!
    "! @parameter message   | <p class="shorttext synchronized">The message text to be logged</p>
    METHODS warning
      IMPORTING !message TYPE string.

    "! <p class="shorttext synchronized">Log an error message</p>
    "! Creates an error message entry if current log level permits (level >= error)
    "!
    "! @parameter message   | <p class="shorttext synchronized">The message text to be logged</p>
    METHODS error
      IMPORTING !message TYPE string.

    "! <p class="shorttext synchronized">Save the log to the database</p>
    "! Persists all collected log entries to the Application Log database tables
    "!
    "! @parameter result | <p class="shorttext synchronized">The log number of the saved log</p>
    METHODS save
      RETURNING VALUE(result) TYPE balognr.

  PRIVATE SECTION.
    DATA object         TYPE balobj_d.
    DATA subobject      TYPE balsubobj.
    DATA log_level      TYPE i.
    DATA external_id    TYPE balnrext.
    DATA log_handle     TYPE balloghndl.
    DATA is_initialized TYPE abap_bool.

    METHODS create_log.

    METHODS add_message
      IMPORTING !message  TYPE string
                msg_type  TYPE symsgty.

    METHODS is_log_level_enabled
      IMPORTING !level        TYPE i
      RETURNING VALUE(result) TYPE abap_bool.

ENDCLASS.

CLASS zcl_mcp_logger IMPLEMENTATION.
  METHOD constructor.
    me->object      = object.
    me->subobject   = subobject.
    me->log_level   = log_level.
    me->external_id = external_id.
    is_initialized = abap_false.
  ENDMETHOD.

  METHOD create_log.
    DATA log_header TYPE bal_s_log.

    IF is_initialized = abap_true.
      RETURN.
    ENDIF.

    log_header-object    = object.
    log_header-subobject = subobject.
    log_header-aldate    = sy-datum.
    log_header-altime    = sy-uzeit.
    log_header-aluser    = sy-uname.
    log_header-alprog    = sy-repid.

    " Set external ID if provided
    IF external_id IS NOT INITIAL.
      log_header-extnumber = external_id.
    ENDIF.

    CALL FUNCTION 'BAL_LOG_CREATE'
      EXPORTING
        i_s_log      = log_header
      IMPORTING
        e_log_handle = log_handle
      EXCEPTIONS
        OTHERS       = 1.

    is_initialized = abap_true.
    IF sy-subrc <> 0.
      log_level = log_levels-off.
    ENDIF.
  ENDMETHOD.

  METHOD is_log_level_enabled.
    result = xsdbool( level >= log_level AND log_level > log_levels-off ).
  ENDMETHOD.

  METHOD add_message.
    DATA msg_details    TYPE bal_s_msg.
    DATA message_length TYPE i.

    IF is_initialized = abap_false.
      create_log( ).
    ENDIF.

    " Prepare message
    msg_details-msgty = msg_type.
    message_length = strlen( message ).
    msg_details-msgid = 'ZMCP'.
    msg_details-msgno = '000'.

    " Handle main message text - safely
    IF message_length > 0.
      IF message_length <= 50.
        msg_details-msgv1 = message.
      ELSE.
        msg_details-msgv1 = message(50).
      ENDIF.
    ENDIF.

    " Handle second segment if needed
    IF message_length > 50.
      IF message_length <= 100.
        msg_details-msgv2 = message+50.
      ELSE.
        msg_details-msgv2 = message+50(50).
      ENDIF.
    ENDIF.

    " Handle third segment if needed
    IF message_length > 100.
      IF message_length <= 150.
        msg_details-msgv3 = message+100.
      ELSE.
        msg_details-msgv3 = message+100(50).
      ENDIF.
    ENDIF.

    " Handle fourth segment if needed
    IF message_length > 150.
      IF message_length <= 200.
        msg_details-msgv4 = message+150.
      ELSE.
        msg_details-msgv4 = message+150(50).
      ENDIF.
    ENDIF.

    " Add the message
    CALL FUNCTION 'BAL_LOG_MSG_ADD'
      EXPORTING
        i_log_handle = log_handle
        i_s_msg      = msg_details
      EXCEPTIONS
        OTHERS       = 1.
    IF sy-subrc <> 0.
      " no error handling, just ignore
    ENDIF.
  ENDMETHOD.

  METHOD info.
    IF is_log_level_enabled( log_levels-info ).
      add_message( message   = message
                   msg_type  = 'I' ).
    ENDIF.
  ENDMETHOD.

  METHOD warning.
    IF is_log_level_enabled( log_levels-warning ).
      add_message(
        message   = message
        msg_type  = 'W'
      ).
    ENDIF.
  ENDMETHOD.

  METHOD error.
    IF is_log_level_enabled( log_levels-error ).
      add_message( message   = message
                   msg_type  = 'E' ).
    ENDIF.
  ENDMETHOD.

  METHOD save.
    DATA log_handles TYPE bal_t_logh.
    DATA log_numbers TYPE bal_t_lgnm.

    IF is_initialized = abap_false.
      result = ''.
      RETURN.
    ENDIF.

    APPEND log_handle TO log_handles.

    CALL FUNCTION 'BAL_DB_SAVE'
      EXPORTING
        i_t_log_handle   = log_handles
      IMPORTING
        e_new_lognumbers = log_numbers
      EXCEPTIONS
        OTHERS           = 1.

    IF sy-subrc = 0 AND lines( log_numbers ) > 0.
      result = log_numbers[ 1 ]-lognumber.
      COMMIT WORK.
    ENDIF.
  ENDMETHOD.

ENDCLASS.
