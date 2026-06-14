"! <p class="shorttext synchronized">MCP MRTR retry request parser</p>
"! Parses requestState and inputResponses for draft MRTR-capable methods.
CLASS zcl_mcp_req_mrtr_retry DEFINITION
  PUBLIC FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    "! <p class="shorttext synchronized">Constructor</p>
    "! Parses optional MRTR retry fields from request parameters.
    "!
    "! @parameter json                | <p class="shorttext synchronized">Request parameters JSON</p>
    "! @raising   zcx_mcp_ajson_error | <p class="shorttext synchronized">JSON parse error</p>
    METHODS constructor
      IMPORTING !json TYPE REF TO zif_mcp_ajson
      RAISING   zcx_mcp_ajson_error.

    "! <p class="shorttext synchronized">Check retry input responses</p>
    "!
    "! @parameter result | <p class="shorttext synchronized">True if inputResponses was supplied</p>
    METHODS has_input_responses
      RETURNING VALUE(result) TYPE abap_bool.

    "! <p class="shorttext synchronized">Get input responses</p>
    "! Returns the raw inputResponses object keyed by input request id.
    "!
    "! @parameter result | <p class="shorttext synchronized">Input responses JSON object</p>
    METHODS get_input_responses
      RETURNING VALUE(result) TYPE REF TO zif_mcp_ajson.

    "! <p class="shorttext synchronized">Get request state</p>
    "! Returns the optional opaque requestState token.
    "!
    "! @parameter result | <p class="shorttext synchronized">Request state</p>
    METHODS get_request_state
      RETURNING VALUE(result) TYPE string.

    "! <p class="shorttext synchronized">Check retry request</p>
    "! A retry request is present when requestState or inputResponses was supplied.
    "!
    "! @parameter result | <p class="shorttext synchronized">True if the request carries MRTR retry data</p>
    METHODS is_retry
      RETURNING VALUE(result) TYPE abap_bool.

  PRIVATE SECTION.
    DATA int_request_state       TYPE string.
    DATA int_has_input_responses TYPE abap_bool.
    DATA int_input_responses     TYPE REF TO zif_mcp_ajson.
ENDCLASS.


CLASS zcl_mcp_req_mrtr_retry IMPLEMENTATION.
  METHOD constructor.
    IF json IS BOUND AND json->exists( '/requestState' ).
      int_request_state = json->get_string( '/requestState' ).
    ENDIF.

    IF json IS BOUND AND json->exists( '/inputResponses' ).
      int_has_input_responses = abap_true.
      int_input_responses = json->slice( '/inputResponses' ).
    ELSE.
      int_has_input_responses = abap_false.
      int_input_responses = zcl_mcp_ajson=>create_empty( ).
    ENDIF.
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

  METHOD is_retry.
    result = xsdbool( int_request_state IS NOT INITIAL OR int_has_input_responses = abap_true ).
  ENDMETHOD.
ENDCLASS.
