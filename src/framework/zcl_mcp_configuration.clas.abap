"! <p class="shorttext synchronized" lang="en">MCP Configuration Class</p>
CLASS zcl_mcp_configuration DEFINITION
  PUBLIC FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    TYPES origins TYPE STANDARD TABLE OF string WITH EMPTY KEY.
    CONSTANTS cors_mode_check TYPE zmcp_conf_cors VALUE 'C' ##NEEDED.
    CONSTANTS cors_mode_ignore TYPE zmcp_conf_cors VALUE 'I'.
    CONSTANTS cors_mode_enforce TYPE zmcp_conf_cors VALUE 'E'.

    "! <p class="shorttext synchronized">Constructor</p>
    "!
    "! @parameter area   | <p class="shorttext synchronized">Area</p>
    "! @parameter server | <p class="shorttext synchronized">Server</p>
    METHODS constructor IMPORTING !area  TYPE zmcp_area
                                  server TYPE zmcp_server.

    "! <p class="shorttext synchronized">Get a logger instance</p>
    "!
    "! @parameter result | <p class="shorttext synchronized">Logger</p>
    METHODS get_logger RETURNING VALUE(result) TYPE REF TO zcl_mcp_logger.

    "! <p class="shorttext synchronized">Get Origins</p>
    "!
    "! @parameter result | <p class="shorttext synchronized">Origins</p>
    METHODS get_allowed_origins RETURNING VALUE(result) TYPE origins.

    "! <p class="shorttext synchronized" lang="en">Get CORS Mode</p>
    "!
    "! @parameter result | <p class="shorttext synchronized" lang="en"></p>
    METHODS get_cors_mode RETURNING VALUE(result) TYPE zmcp_conf_cors.

  PRIVATE SECTION.
    DATA logger          TYPE REF TO zcl_mcp_logger.
    DATA area            TYPE zmcp_area.
    DATA server          TYPE zmcp_server.
    DATA allowed_origins TYPE origins.
    DATA configuration   TYPE zmcp_config.

    "! <p class="shorttext synchronized" lang="en">Read configuration from DB</p>
    "!
    METHODS read_configuration.
ENDCLASS.

CLASS zcl_mcp_configuration IMPLEMENTATION.
  METHOD constructor.
    me->area   = area.
    me->server = server.
    read_configuration( ).
  ENDMETHOD.

  METHOD get_allowed_origins.
    IF allowed_origins IS NOT INITIAL.
      result = allowed_origins.
      RETURN.
    ENDIF.

    " Check settings on lowest level first, if none found, check on higher level
    SELECT origin FROM zmcp_origins
      WHERE area   = @area
        AND server = @server
        ORDER BY PRIMARY KEY
        INTO TABLE @allowed_origins.
    IF sy-subrc = 0.
      result = allowed_origins.
      RETURN.
    ENDIF.

    SELECT origin FROM zmcp_origins
      WHERE area   = '*'
        AND server = @server
        ORDER BY PRIMARY KEY
        INTO TABLE @allowed_origins.
    IF sy-subrc = 0.
      result = allowed_origins.
      RETURN.
    ENDIF.

    SELECT origin FROM zmcp_origins
      WHERE area   = '*'
        AND server = '*'
        ORDER BY PRIMARY KEY
        INTO TABLE @allowed_origins.
    IF sy-subrc = 0.
      result = allowed_origins.
      RETURN.
    ENDIF.

    " If no origins found, all are allowed
    allowed_origins = VALUE #( ( `*` ) ).
    result = allowed_origins.
  ENDMETHOD.

  METHOD get_logger.
    IF logger IS NOT BOUND.
      DATA log_level TYPE i.
      CASE configuration-log_level.
        WHEN zcl_mcp_logger=>log_levels-error.
          log_level = 3.
        WHEN zcl_mcp_logger=>log_levels-warning.
          log_level = 2.
        WHEN zcl_mcp_logger=>log_levels-info.
          log_level = 1.
        WHEN OTHERS.
          log_level = 0. " Default to off
      ENDCASE.

      logger = NEW #( log_level    = log_level
                      object       = configuration-object
                      subobject    = configuration-subobject ).
    ENDIF.

    result = logger.
  ENDMETHOD.

  METHOD read_configuration.
    " Read configuration from database or other source
    SELECT SINGLE * FROM zmcp_config
      INTO @configuration.

    IF sy-subrc <> 0.
      " Set default configuration
      configuration-log_level = zcl_mcp_logger=>log_levels-off.
      configuration-object    = 'ZMCP_SERVER'.
      configuration-subobject = 'DEFAULT'.
      configuration-cors_mode = 'C'. " Check CORS if Origin header is present
    ENDIF.

    IF configuration-cors_mode IS INITIAL.
      configuration-cors_mode = cors_mode_ignore.
    ENDIF.
  ENDMETHOD.

  METHOD get_cors_mode.
    result = configuration-cors_mode.
  ENDMETHOD.

ENDCLASS.
