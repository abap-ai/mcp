"! <p class="shorttext synchronized">MCP Read Resource Request</p>
CLASS zcl_mcp_req_read_resource DEFINITION
  PUBLIC FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    TYPES resource_uri TYPE string.

    "! <p class="shorttext synchronized">Constructor</p>
    "! Parses the Read Resource Request JSON
    "!
    "! @parameter json | <p class="shorttext synchronized">JSON Request</p>
    METHODS constructor
      IMPORTING json TYPE REF TO zif_mcp_ajson
      RAISING   zcx_mcp_ajson_error zcx_mcp_server.

    "! <p class="shorttext synchronized">Get resource URI</p>
    "!
    "! @parameter result | <p class="shorttext synchronized">Resource URI</p>
    METHODS get_uri
      RETURNING VALUE(result) TYPE resource_uri.

    "! <p class="shorttext synchronized">Get _meta fields</p>
    "!
    "! @parameter result | <p class="shorttext synchronized">_meta JSON</p>
    METHODS get_meta
      RETURNING VALUE(result) TYPE REF TO zif_mcp_ajson.

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
    DATA int_uri   TYPE resource_uri.
    DATA int_meta  TYPE REF TO zif_mcp_ajson.
    DATA int_retry TYPE REF TO zcl_mcp_req_mrtr_retry.
ENDCLASS.

CLASS zcl_mcp_req_read_resource IMPLEMENTATION.
  METHOD constructor.
    " Check if URI exists - it's a mandatory parameter
    IF json->exists( 'uri' ).
      int_uri = json->get_string( 'uri' ).

      " Additional validation: URI should not be empty
      IF int_uri IS INITIAL.
        RAISE EXCEPTION NEW zcx_mcp_server( textid = zcx_mcp_server=>required_params
                                            msgv1  = 'uri' ).
      ENDIF.
    ELSE.
      RAISE EXCEPTION NEW zcx_mcp_server( textid = zcx_mcp_server=>required_params
                                          msgv1  = 'uri' ).
    ENDIF.

    " Check for _meta fields
    IF json->exists( '/_meta' ).
      int_meta = json->slice( '/_meta' ).
    ELSE.
      int_meta = zcl_mcp_ajson=>create_empty( ).
    ENDIF.

    int_retry = NEW zcl_mcp_req_mrtr_retry( json ).
  ENDMETHOD.

  METHOD get_uri.
    result = int_uri.
  ENDMETHOD.

  METHOD get_meta.
    result = int_meta.
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
