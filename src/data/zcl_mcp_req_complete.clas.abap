"! <p class="shorttext synchronized">MCP Completion Request</p>
  "! Parses the completion/complete request (MCP 2025-11-25)
CLASS zcl_mcp_req_complete DEFINITION
PUBLIC FINAL
CREATE PUBLIC.

  PUBLIC SECTION.
    CONSTANTS: BEGIN OF ref_type,
                 prompt   TYPE string VALUE 'ref/prompt',
                 resource TYPE string VALUE 'ref/resource',
               END OF ref_type.

    "! <p class="shorttext synchronized">Constructor</p>
    "! Parses the completion/complete JSON request params.
    "!
    "! @parameter json                | <p class="shorttext synchronized">JSON request params</p>
    "! @raising   zcx_mcp_ajson_error | <p class="shorttext synchronized">JSON parse error</p>
    "! @raising   zcx_mcp_server      | <p class="shorttext synchronized">Missing or invalid fields</p>
    METHODS constructor
      IMPORTING !json TYPE REF TO zif_mcp_ajson
      RAISING   zcx_mcp_ajson_error
                zcx_mcp_server.

    "! <p class="shorttext synchronized">Get reference type</p>
    "! Returns ref/prompt or ref/resource.
    "!
    "! @parameter result | <p class="shorttext synchronized">Reference type constant</p>
    METHODS get_ref_type
      RETURNING VALUE(result) TYPE string.

    "! <p class="shorttext synchronized">Get prompt name</p>
    "! Only populated when get_ref_type returns ref/prompt.
    "!
    "! @parameter result | <p class="shorttext synchronized">Prompt name</p>
    METHODS get_ref_name
      RETURNING VALUE(result) TYPE string.

    "! <p class="shorttext synchronized">Get resource URI template</p>
    "! Only populated when get_ref_type returns ref/resource.
    "!
    "! @parameter result | <p class="shorttext synchronized">Resource URI template</p>
    METHODS get_ref_uri
      RETURNING VALUE(result) TYPE string.

    "! <p class="shorttext synchronized">Get argument name</p>
    "!
    "! @parameter result | <p class="shorttext synchronized">Name of the argument being completed</p>
    METHODS get_argument_name
      RETURNING VALUE(result) TYPE string.

    "! <p class="shorttext synchronized">Get argument value</p>
    "! Partial value typed so far by the client.
    "!
    "! @parameter result | <p class="shorttext synchronized">Partial argument value</p>
    METHODS get_argument_value
      RETURNING VALUE(result) TYPE string.

    "! <p class="shorttext synchronized">Check if context was provided</p>
    "!
    "! @parameter result | <p class="shorttext synchronized">True if context object is present</p>
    METHODS has_context
      RETURNING VALUE(result) TYPE abap_bool.

    "! <p class="shorttext synchronized">Get context JSON</p>
    "! Raw context slice. Read context/arguments/name for already-filled argument values.
    "!
    "! @parameter result | <p class="shorttext synchronized">Context JSON object</p>
    METHODS get_context_json
      RETURNING VALUE(result) TYPE REF TO zif_mcp_ajson.

    "! <p class="shorttext synchronized">Get meta fields</p>
    "!
    "! @parameter result | <p class="shorttext synchronized">_meta JSON</p>
    METHODS get_meta
      RETURNING VALUE(result) TYPE REF TO zif_mcp_ajson.

  PRIVATE SECTION.
    DATA int_ref_type       TYPE string.
    DATA int_ref_name       TYPE string.
    DATA int_ref_uri        TYPE string.
    DATA int_argument_name  TYPE string.
    DATA int_argument_value TYPE string.
    DATA int_has_context    TYPE abap_bool.
    DATA int_context_json   TYPE REF TO zif_mcp_ajson.
    DATA int_meta           TYPE REF TO zif_mcp_ajson.
  ENDCLASS.

  CLASS zcl_mcp_req_complete IMPLEMENTATION.
  METHOD constructor.
    IF json->exists( '/ref/type' ) = abap_false.
      RAISE EXCEPTION NEW zcx_mcp_server( textid = zcx_mcp_server=>required_params
                                          msgv1  = 'ref.type' ).
    ENDIF.
    int_ref_type = json->get_string( '/ref/type' ).

    CASE int_ref_type.
      WHEN ref_type-prompt.
        IF json->exists( '/ref/name' ) = abap_false.
          RAISE EXCEPTION NEW zcx_mcp_server( textid = zcx_mcp_server=>required_params
                                              msgv1  = 'ref.name' ).
        ENDIF.
        int_ref_name = json->get_string( '/ref/name' ).
      WHEN ref_type-resource.
        IF json->exists( '/ref/uri' ) = abap_false.
          RAISE EXCEPTION NEW zcx_mcp_server( textid = zcx_mcp_server=>required_params
                                              msgv1  = 'ref.uri' ).
        ENDIF.
        int_ref_uri = json->get_string( '/ref/uri' ).
      WHEN OTHERS.
        RAISE EXCEPTION NEW zcx_mcp_server( textid = zcx_mcp_server=>invalid_arguments
                                            msgv1  = CONV #( |Unknown ref type: { int_ref_type }| ) ).
    ENDCASE.

    IF json->exists( '/argument/name' ) = abap_false.
      RAISE EXCEPTION NEW zcx_mcp_server( textid = zcx_mcp_server=>required_params
                                          msgv1  = 'argument.name' ).
    ENDIF.
    int_argument_name = json->get_string( '/argument/name' ).

    IF json->exists( '/argument/value' ) = abap_false.
      RAISE EXCEPTION NEW zcx_mcp_server( textid = zcx_mcp_server=>required_params
                                          msgv1  = 'argument.value' ).
    ENDIF.
    int_argument_value = json->get_string( '/argument/value' ).

    IF json->exists( '/context' ).
      int_has_context  = abap_true.
      int_context_json = json->slice( '/context' ).
    ENDIF.

    IF json->exists( '/_meta' ).
      int_meta = json->slice( '/_meta' ).
    ENDIF.
  ENDMETHOD.

  METHOD get_ref_type.
    result = int_ref_type.
  ENDMETHOD.

  METHOD get_ref_name.
    result = int_ref_name.
  ENDMETHOD.

  METHOD get_ref_uri.
    result = int_ref_uri.
  ENDMETHOD.

  METHOD get_argument_name.
    result = int_argument_name.
  ENDMETHOD.

  METHOD get_argument_value.
    result = int_argument_value.
  ENDMETHOD.

  METHOD has_context.
    result = int_has_context.
  ENDMETHOD.

  METHOD get_context_json.
    result = int_context_json.
  ENDMETHOD.

  METHOD get_meta.
    result = int_meta.
  ENDMETHOD.
  ENDCLASS.
