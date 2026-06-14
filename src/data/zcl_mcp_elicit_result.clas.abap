"! <p class="shorttext synchronized">MCP elicitation result parser</p>
"! Parses the client response for an elicitation/create input request.
CLASS zcl_mcp_elicit_result DEFINITION
  PUBLIC FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    CONSTANTS: BEGIN OF actions,
                 accept  TYPE string VALUE `accept`,
                 decline TYPE string VALUE `decline`,
                 cancel  TYPE string VALUE `cancel`,
               END OF actions.

    "! <p class="shorttext synchronized">Constructor</p>
    "! Parses an elicitation result object from inputResponses.
    "!
    "! @parameter json                | <p class="shorttext synchronized">Elicitation result JSON</p>
    "! @raising   zcx_mcp_ajson_error | <p class="shorttext synchronized">JSON parse error</p>
    "! @raising   zcx_mcp_server      | <p class="shorttext synchronized">Missing or invalid action</p>
    METHODS constructor
      IMPORTING !json TYPE REF TO zif_mcp_ajson
      RAISING   zcx_mcp_ajson_error
                zcx_mcp_server.

    "! <p class="shorttext synchronized">Get action</p>
    "!
    "! @parameter result | <p class="shorttext synchronized">accept, decline, or cancel</p>
    METHODS get_action
      RETURNING VALUE(result) TYPE string.

    "! <p class="shorttext synchronized">Check accepted action</p>
    "!
    "! @parameter result | <p class="shorttext synchronized">True if action is accept</p>
    METHODS is_accept
      RETURNING VALUE(result) TYPE abap_bool.

    "! <p class="shorttext synchronized">Check declined action</p>
    "!
    "! @parameter result | <p class="shorttext synchronized">True if action is decline</p>
    METHODS is_decline
      RETURNING VALUE(result) TYPE abap_bool.

    "! <p class="shorttext synchronized">Check cancelled action</p>
    "!
    "! @parameter result | <p class="shorttext synchronized">True if action is cancel</p>
    METHODS is_cancel
      RETURNING VALUE(result) TYPE abap_bool.

    "! <p class="shorttext synchronized">Check content</p>
    "!
    "! @parameter result | <p class="shorttext synchronized">True if content was supplied</p>
    METHODS has_content
      RETURNING VALUE(result) TYPE abap_bool.

    "! <p class="shorttext synchronized">Get content</p>
    "!
    "! @parameter result | <p class="shorttext synchronized">Submitted form content</p>
    METHODS get_content
      RETURNING VALUE(result) TYPE REF TO zif_mcp_ajson.

    "! <p class="shorttext synchronized">Get string content field</p>
    "!
    "! @parameter name                | <p class="shorttext synchronized">Content field name</p>
    "! @parameter result              | <p class="shorttext synchronized">String value</p>
    "! @raising   zcx_mcp_ajson_error | <p class="shorttext synchronized">JSON access error</p>
    METHODS get_string
      IMPORTING !name         TYPE string
      RETURNING VALUE(result) TYPE string
      RAISING   zcx_mcp_ajson_error.

    "! <p class="shorttext synchronized">Get boolean content field</p>
    "!
    "! @parameter name                | <p class="shorttext synchronized">Content field name</p>
    "! @parameter result              | <p class="shorttext synchronized">Boolean value</p>
    "! @raising   zcx_mcp_ajson_error | <p class="shorttext synchronized">JSON access error</p>
    METHODS get_boolean
      IMPORTING !name         TYPE string
      RETURNING VALUE(result) TYPE abap_bool
      RAISING   zcx_mcp_ajson_error.

    "! <p class="shorttext synchronized">Get integer content field</p>
    "!
    "! @parameter name                | <p class="shorttext synchronized">Content field name</p>
    "! @parameter result              | <p class="shorttext synchronized">Integer value</p>
    "! @raising   zcx_mcp_ajson_error | <p class="shorttext synchronized">JSON access error</p>
    METHODS get_integer
      IMPORTING !name         TYPE string
      RETURNING VALUE(result) TYPE i
      RAISING   zcx_mcp_ajson_error.

  PRIVATE SECTION.
    DATA int_action      TYPE string.
    DATA int_has_content TYPE abap_bool.
    DATA int_content     TYPE REF TO zif_mcp_ajson.

    METHODS content_path
      IMPORTING !name         TYPE string
      RETURNING VALUE(result) TYPE string.
ENDCLASS.


CLASS zcl_mcp_elicit_result IMPLEMENTATION.
  METHOD constructor.
    IF json IS NOT BOUND.
      RAISE EXCEPTION NEW zcx_mcp_server( textid = zcx_mcp_server=>required_params
                                          msgv1  = `elicitation result` ).
    ENDIF.

    IF json->exists( `/action` ) = abap_false.
      RAISE EXCEPTION NEW zcx_mcp_server( textid = zcx_mcp_server=>required_params
                                          msgv1  = `action` ).
    ENDIF.

    int_action = json->get_string( `/action` ).

    CASE int_action.
      WHEN actions-accept OR actions-decline OR actions-cancel.
      WHEN OTHERS.
        RAISE EXCEPTION NEW zcx_mcp_server( textid = zcx_mcp_server=>invalid_arguments
                                            msgv1  = |Invalid elicitation action { int_action }| ) ##NO_TEXT.
    ENDCASE.

    IF json->exists( `/content` ).
      int_has_content = abap_true.
      int_content = json->slice( `/content` ).
    ELSE.
      int_has_content = abap_false.
      int_content = zcl_mcp_ajson=>create_empty( ).
    ENDIF.
  ENDMETHOD.

  METHOD get_action.
    result = int_action.
  ENDMETHOD.

  METHOD is_accept.
    result = xsdbool( int_action = actions-accept ).
  ENDMETHOD.

  METHOD is_decline.
    result = xsdbool( int_action = actions-decline ).
  ENDMETHOD.

  METHOD is_cancel.
    result = xsdbool( int_action = actions-cancel ).
  ENDMETHOD.

  METHOD has_content.
    result = int_has_content.
  ENDMETHOD.

  METHOD get_content.
    result = int_content.
  ENDMETHOD.

  METHOD get_string.
    result = int_content->get_string( content_path( name ) ).
  ENDMETHOD.

  METHOD get_boolean.
    result = int_content->get_boolean( content_path( name ) ).
  ENDMETHOD.

  METHOD get_integer.
    result = int_content->get_integer( content_path( name ) ).
  ENDMETHOD.

  METHOD content_path.
    result = |/{ name }|.
  ENDMETHOD.
ENDCLASS.
