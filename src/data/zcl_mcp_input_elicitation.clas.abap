"! <p class="shorttext synchronized">MCP elicitation/create input request</p>
"! Builds the server-to-client elicitation/create request used inside MRTR inputRequests.
CLASS zcl_mcp_input_elicitation DEFINITION
  PUBLIC FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    CONSTANTS method_name TYPE string VALUE `elicitation/create`.

    CONSTANTS: BEGIN OF modes,
                 form TYPE string VALUE `form`,
                 url  TYPE string VALUE `url`,
               END OF modes.

    "! <p class="shorttext synchronized">Set form mode parameters</p>
    "! Builds an elicitation/create request for structured in-client form input.
    "!
    "! @parameter message             | <p class="shorttext synchronized">User-facing explanation</p>
    "! @parameter requested_schema    | <p class="shorttext synchronized">Restricted JSON schema for expected input</p>
    "! @parameter include_mode        | <p class="shorttext synchronized">Emit mode=form explicitly</p>
    "! @raising   zcx_mcp_ajson_error | <p class="shorttext synchronized">JSON build error</p>
    METHODS set_form
      IMPORTING !message         TYPE string
                requested_schema TYPE REF TO zif_mcp_ajson
                include_mode     TYPE abap_bool DEFAULT abap_true
      RAISING   zcx_mcp_ajson_error.

    "! <p class="shorttext synchronized">Set URL mode parameters</p>
    "! Builds an elicitation/create request for out-of-band URL interaction.
    "!
    "! @parameter message             | <p class="shorttext synchronized">User-facing explanation</p>
    "! @parameter url                 | <p class="shorttext synchronized">URL the client should ask the user to open</p>
    "! @parameter elicitation_id      | <p class="shorttext synchronized">Server-unique elicitation id</p>
    "! @raising   zcx_mcp_ajson_error | <p class="shorttext synchronized">JSON build error</p>
    METHODS set_url
      IMPORTING !message       TYPE string
                url            TYPE string
                elicitation_id TYPE string
      RAISING   zcx_mcp_ajson_error.

    "! <p class="shorttext synchronized">Get request method</p>
    "!
    "! @parameter result | <p class="shorttext synchronized">MCP method name</p>
    METHODS get_method
      RETURNING VALUE(result) TYPE string.

    "! <p class="shorttext synchronized">Get request parameters</p>
    "!
    "! @parameter result | <p class="shorttext synchronized">Request parameters JSON</p>
    METHODS get_params
      RETURNING VALUE(result) TYPE REF TO zif_mcp_ajson.

    "! <p class="shorttext synchronized">Generate full request object</p>
    "! Returns an object with method and params, suitable for direct inputRequests insertion tests.
    "!
    "! @parameter result              | <p class="shorttext synchronized">Request JSON object</p>
    "! @raising   zcx_mcp_ajson_error | <p class="shorttext synchronized">JSON build error</p>
    METHODS generate_json
      RETURNING VALUE(result) TYPE REF TO zif_mcp_ajson
      RAISING   zcx_mcp_ajson_error.

  PRIVATE SECTION.
    DATA int_params TYPE REF TO zif_mcp_ajson.
ENDCLASS.


CLASS zcl_mcp_input_elicitation IMPLEMENTATION.
  METHOD set_form.
    int_params = zcl_mcp_ajson=>create_empty( ).

    IF include_mode = abap_true.
      int_params->set_string( iv_path = `/mode`
                              iv_val  = modes-form ).
    ENDIF.

    int_params->set_string( iv_path = `/message`
                            iv_val  = message ).

    int_params->set( iv_path = `/requestedSchema`
                     iv_val  = requested_schema ).
  ENDMETHOD.

  METHOD set_url.
    int_params = zcl_mcp_ajson=>create_empty( ).

    int_params->set_string( iv_path = `/mode`
                            iv_val  = modes-url ).
    int_params->set_string( iv_path = `/message`
                            iv_val  = message ).
    int_params->set_string( iv_path = `/url`
                            iv_val  = url ).
    int_params->set_string( iv_path = `/elicitationId`
                            iv_val  = elicitation_id ).
  ENDMETHOD.

  METHOD get_method.
    result = method_name.
  ENDMETHOD.

  METHOD get_params.
    IF int_params IS BOUND.
      result = int_params.
    ELSE.
      result = zcl_mcp_ajson=>create_empty( ).
    ENDIF.
  ENDMETHOD.

  METHOD generate_json.
    result = zcl_mcp_ajson=>create_empty( ).
    result->set_string( iv_path = `/method`
                        iv_val  = method_name ).
    result->set( iv_path = `/params`
                 iv_val  = get_params( ) ).
  ENDMETHOD.
ENDCLASS.
