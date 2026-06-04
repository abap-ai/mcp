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
      DATA temp1 TYPE REF TO zcx_mcp_server.
          DATA temp2 TYPE REF TO zcx_mcp_server.
          DATA temp3 TYPE REF TO zcx_mcp_server.
        DATA temp4 TYPE symsgv.
        DATA temp7 TYPE REF TO zcx_mcp_server.
      DATA temp5 TYPE REF TO zcx_mcp_server.
      DATA temp6 TYPE REF TO zcx_mcp_server.
    IF json->exists( '/ref/type' ) = abap_false.
      
      CREATE OBJECT temp1 TYPE zcx_mcp_server EXPORTING textid = zcx_mcp_server=>required_params msgv1 = 'ref.type'.
      RAISE EXCEPTION temp1.
    ENDIF.
    int_ref_type = json->get_string( '/ref/type' ).

    CASE int_ref_type.
      WHEN ref_type-prompt.
        IF json->exists( '/ref/name' ) = abap_false.
          
          CREATE OBJECT temp2 TYPE zcx_mcp_server EXPORTING textid = zcx_mcp_server=>required_params msgv1 = 'ref.name'.
          RAISE EXCEPTION temp2.
        ENDIF.
        int_ref_name = json->get_string( '/ref/name' ).
      WHEN ref_type-resource.
        IF json->exists( '/ref/uri' ) = abap_false.
          
          CREATE OBJECT temp3 TYPE zcx_mcp_server EXPORTING textid = zcx_mcp_server=>required_params msgv1 = 'ref.uri'.
          RAISE EXCEPTION temp3.
        ENDIF.
        int_ref_uri = json->get_string( '/ref/uri' ).
      WHEN OTHERS.
        
        temp4 = |Unknown ref type: { int_ref_type }|.
        
        CREATE OBJECT temp7 TYPE zcx_mcp_server EXPORTING textid = zcx_mcp_server=>invalid_arguments msgv1 = temp4.
        RAISE EXCEPTION temp7.
    ENDCASE.

    IF json->exists( '/argument/name' ) = abap_false.
      
      CREATE OBJECT temp5 TYPE zcx_mcp_server EXPORTING textid = zcx_mcp_server=>required_params msgv1 = 'argument.name'.
      RAISE EXCEPTION temp5.
    ENDIF.
    int_argument_name = json->get_string( '/argument/name' ).

    IF json->exists( '/argument/value' ) = abap_false.
      
      CREATE OBJECT temp6 TYPE zcx_mcp_server EXPORTING textid = zcx_mcp_server=>required_params msgv1 = 'argument.value'.
      RAISE EXCEPTION temp6.
    ENDIF.
    int_argument_value = json->get_string( '/argument/value' ).

    IF json->exists( '/context' ) IS NOT INITIAL.
      int_has_context  = abap_true.
      int_context_json = json->slice( '/context' ).
    ENDIF.

    IF json->exists( '/_meta' ) IS NOT INITIAL.
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
