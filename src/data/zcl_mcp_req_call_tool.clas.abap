"! <p class="__shorttext__ synchronized">MCP Call Tool Request</p>
CLASS zcl_mcp_req_call_tool DEFINITION
  PUBLIC FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    "! <p class="__shorttext__ synchronized">Constructor</p>
    "! Parses the Call Tool Request JSON (MCP 2025-11-25)
    "!
    "! @parameter json | <p class="__shorttext__ synchronized">JSON Request</p>
    METHODS constructor
      IMPORTING !json TYPE REF TO zif_mcp_ajson
      RAISING   zcx_mcp_ajson_error
                zcx_mcp_server.

    "! <p class="__shorttext__ synchronized">Get tool name</p>
    "!
    "! @parameter result | <p class="__shorttext__ synchronized">Tool name</p>
    METHODS get_name
      RETURNING VALUE(result) TYPE string.

    "! <p class="__shorttext__ synchronized">Check if arguments were provided</p>
    "!
    "! @parameter result | <p class="__shorttext__ synchronized">True if arguments exist</p>
    METHODS has_arguments
      RETURNING VALUE(result) TYPE abap_bool.

    "! <p class="__shorttext__ synchronized">Get tool arguments</p>
    "!
    "! @parameter result | <p class="__shorttext__ synchronized">Arguments JSON</p>
    METHODS get_arguments
      RETURNING VALUE(result) TYPE REF TO zif_mcp_ajson.

    "! <p class="__shorttext__ synchronized">Get _meta fields</p>
    "! Contains the optional progressToken and any extension fields.
    "!
    "! @parameter result | <p class="__shorttext__ synchronized">_meta JSON</p>
    METHODS get_meta
      RETURNING VALUE(result) TYPE REF TO zif_mcp_ajson.

    "! <p class="__shorttext__ synchronized">Check if task augmentation was requested</p>
    "! New in MCP 2025-11-25: client may request async task-augmented execution.
    "!
    "! @parameter result | <p class="__shorttext__ synchronized">True if task field is present</p>
    METHODS has_task
      RETURNING VALUE(result) TYPE abap_bool.

    "! <p class="__shorttext__ synchronized">Get task TTL in milliseconds</p>
    "! Only meaningful when has_task( ) = abap_true.
    "! A value of 0 means the client did not specify a TTL.
    "!
    "! @parameter result | <p class="__shorttext__ synchronized">Requested task TTL in ms, 0 if absent</p>
    METHODS get_task_ttl
      RETURNING VALUE(result) TYPE i.

    "! <p class="__shorttext__ synchronized">Check retry input responses</p>
    "!
    "! @parameter result | <p class="__shorttext__ synchronized">True if inputResponses was supplied</p>
    METHODS has_input_responses
      RETURNING VALUE(result) TYPE abap_bool.

    "! <p class="__shorttext__ synchronized">Get input responses</p>
    "!
    "! @parameter result | <p class="__shorttext__ synchronized">Input responses JSON object</p>
    METHODS get_input_responses
      RETURNING VALUE(result) TYPE REF TO zif_mcp_ajson.

    "! <p class="__shorttext__ synchronized">Get request state</p>
    "!
    "! @parameter result | <p class="__shorttext__ synchronized">Opaque MRTR requestState token</p>
    METHODS get_request_state
      RETURNING VALUE(result) TYPE string.

    "! <p class="__shorttext__ synchronized">Check retry request</p>
    "!
    "! @parameter result | <p class="__shorttext__ synchronized">True if MRTR retry data was supplied</p>
    METHODS is_retry
      RETURNING VALUE(result) TYPE abap_bool.

  PRIVATE SECTION.
    DATA int_name          TYPE string.
    DATA int_arguments     TYPE REF TO zif_mcp_ajson.
    DATA int_has_arguments TYPE abap_bool.
    DATA int_meta          TYPE REF TO zif_mcp_ajson.
    DATA int_has_task      TYPE abap_bool.
    DATA int_task_ttl      TYPE i.
    DATA int_retry         TYPE REF TO zcl_mcp_req_mrtr_retry.
ENDCLASS.

CLASS zcl_mcp_req_call_tool IMPLEMENTATION.
  METHOD constructor.
    " --- name (mandatory) ---
    IF json->exists( '/name' ).
      int_name = json->get_string( '/name' ).
      IF int_name IS INITIAL.
        RAISE EXCEPTION NEW zcx_mcp_server( textid = zcx_mcp_server=>unknown_tool
                                            msgv1  = 'Tool name cannot be empty' ) ##NO_TEXT.
      ENDIF.
    ELSE.
      RAISE EXCEPTION NEW zcx_mcp_server( textid = zcx_mcp_server=>required_params
                                          msgv1  = 'name' ).
    ENDIF.

    " --- arguments (optional) ---
    IF json->exists( '/arguments' ).
      int_has_arguments = abap_true.
      int_arguments = json->slice( '/arguments' ).
    ELSE.
      int_has_arguments = abap_false.
      int_arguments = zcl_mcp_ajson=>create_empty( ).
    ENDIF.

    " --- _meta (optional, inherited from RequestParams) ---
    " Carries the optional progressToken and any extension fields.
    IF json->exists( '/_meta' ).
      int_meta = json->slice( '/_meta' ).
    ELSE.
      int_meta = zcl_mcp_ajson=>create_empty( ).
    ENDIF.

    int_retry = NEW zcl_mcp_req_mrtr_retry( json ).

    " --- task (optional, new in MCP 2025-11-25) ---
    " Present when the client requests task-augmented (async) execution.
    " TaskMetadata: { ttl?: number }
    IF json->exists( '/task' ).
      int_has_task = abap_true.
      IF json->exists( '/task/ttl' ).
        int_task_ttl = json->get_integer( '/task/ttl' ).
      ENDIF.
    ELSE.
      int_has_task = abap_false.
      int_task_ttl = 0.
    ENDIF.
  ENDMETHOD.

  METHOD get_name.
    result = int_name.
  ENDMETHOD.

  METHOD has_arguments.
    result = int_has_arguments.
  ENDMETHOD.

  METHOD get_arguments.
    result = int_arguments.
  ENDMETHOD.

  METHOD get_meta.
    result = int_meta.
  ENDMETHOD.

  METHOD has_task.
    result = int_has_task.
  ENDMETHOD.

  METHOD get_task_ttl.
    result = int_task_ttl.
  ENDMETHOD.

  METHOD has_input_responses.
    result = int_retry->has_input_responses( ).
  ENDMETHOD.

  METHOD get_input_responses.
    result = int_retry->get_input_responses( ).
  ENDMETHOD.

  METHOD get_request_state.
    result = int_retry->get_request_state( ).
  ENDMETHOD.

  METHOD is_retry.
    result = int_retry->is_retry( ).
  ENDMETHOD.

ENDCLASS.
