"! <p class="shorttext synchronized">MCP draft update task request</p>
"! Parses tasks/update parameters for the official tasks extension.
CLASS zcl_mcp_req_update_task DEFINITION
  PUBLIC FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    "! <p class="shorttext synchronized">Constructor</p>
    "! Parses task input responses from tasks/update request parameters.
    "!
    "! @parameter json                | <p class="shorttext synchronized">JSON request parameters</p>
    "! @raising   zcx_mcp_ajson_error | <p class="shorttext synchronized">JSON parse error</p>
    "! @raising   zcx_mcp_server      | <p class="shorttext synchronized">Missing or invalid request fields</p>
    METHODS constructor
      IMPORTING !json TYPE REF TO zif_mcp_ajson
      RAISING   zcx_mcp_ajson_error
                zcx_mcp_server.

    "! <p class="shorttext synchronized">Get task id</p>
    "!
    "! @parameter result | <p class="shorttext synchronized">Task id</p>
    METHODS get_task_id
      RETURNING VALUE(result) TYPE string.

    "! <p class="shorttext synchronized">Check input responses</p>
    "!
    "! @parameter result | <p class="shorttext synchronized">True if inputResponses was provided</p>
    METHODS has_input_responses
      RETURNING VALUE(result) TYPE abap_bool.

    "! <p class="shorttext synchronized">Get input responses</p>
    "! Returns the raw inputResponses object keyed by input request id.
    "!
    "! @parameter result | <p class="shorttext synchronized">Input responses JSON object</p>
    METHODS get_input_responses
      RETURNING VALUE(result) TYPE REF TO zif_mcp_ajson.

    "! <p class="shorttext synchronized">Get request state</p>
    "! Returns the optional opaque MRTR requestState token.
    "!
    "! @parameter result | <p class="shorttext synchronized">Request state</p>
    METHODS get_request_state
      RETURNING VALUE(result) TYPE string.

    "! <p class="shorttext synchronized">Get _meta fields</p>
    "!
    "! @parameter result | <p class="shorttext synchronized">Request metadata JSON</p>
    METHODS get_meta
      RETURNING VALUE(result) TYPE REF TO zif_mcp_ajson.

  PRIVATE SECTION.
    DATA int_task_id             TYPE string.
    DATA int_has_input_responses TYPE abap_bool.
    DATA int_input_responses     TYPE REF TO zif_mcp_ajson.
    DATA int_request_state       TYPE string.
    DATA int_meta                TYPE REF TO zif_mcp_ajson.
ENDCLASS.

CLASS zcl_mcp_req_update_task IMPLEMENTATION.
  METHOD constructor.
    IF json IS NOT BOUND.
      RAISE EXCEPTION NEW zcx_mcp_server( textid = zcx_mcp_server=>required_params
                                          msgv1  = 'params' ).
    ENDIF.

    IF json->exists( '/taskId' ).
      int_task_id = json->get_string( '/taskId' ).
    ENDIF.

    IF int_task_id IS INITIAL.
      RAISE EXCEPTION NEW zcx_mcp_server( textid = zcx_mcp_server=>required_params
                                          msgv1  = 'taskId' ).
    ENDIF.

    IF strlen( int_task_id ) <> 32.
      RAISE EXCEPTION NEW zcx_mcp_server( textid = zcx_mcp_server=>invalid_arguments
                                          msgv1  = CONV #( |Invalid taskId: { int_task_id }| ) ) ##NO_TEXT.
    ENDIF.

    FIND REGEX '^[0-9A-Fa-f]{32}$' IN int_task_id.
    IF sy-subrc <> 0.
      RAISE EXCEPTION NEW zcx_mcp_server( textid = zcx_mcp_server=>invalid_arguments
                                          msgv1  = CONV #( |Invalid taskId: { int_task_id }| ) ) ##NO_TEXT.
    ENDIF.

    TRANSLATE int_task_id TO UPPER CASE.

    IF json->exists( '/inputResponses' ).
      int_has_input_responses = abap_true.
      int_input_responses = json->slice( '/inputResponses' ).
    ELSE.
      RAISE EXCEPTION NEW zcx_mcp_server( textid = zcx_mcp_server=>required_params
                                          msgv1  = 'inputResponses' ).
    ENDIF.

    IF json->exists( '/requestState' ).
      int_request_state = json->get_string( '/requestState' ).
    ENDIF.

    IF json->exists( '/_meta' ).
      int_meta = json->slice( '/_meta' ).
    ELSE.
      int_meta = zcl_mcp_ajson=>create_empty( ).
    ENDIF.
  ENDMETHOD.

  METHOD get_task_id.
    result = int_task_id.
  ENDMETHOD.

  METHOD has_input_responses.
    result = int_has_input_responses.
  ENDMETHOD.

  METHOD get_input_responses.
    result = int_input_responses.
  ENDMETHOD.

  METHOD get_request_state.
    result = int_request_state.
  ENDMETHOD.

  METHOD get_meta.
    result = int_meta.
  ENDMETHOD.
ENDCLASS.
