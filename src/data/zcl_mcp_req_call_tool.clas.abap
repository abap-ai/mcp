"! <p class="shorttext synchronized">MCP Call Tool Request</p>
CLASS zcl_mcp_req_call_tool DEFINITION
  PUBLIC FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    "! <p class="shorttext synchronized">Constructor</p>
    "! Parses the Call Tool Request JSON
    "!
    "! @parameter json | <p class="shorttext synchronized">JSON Request</p>
    METHODS constructor
      IMPORTING json TYPE REF TO zif_mcp_ajson
      RAISING   zcx_mcp_ajson_error
                zcx_mcp_server.

    "! <p class="shorttext synchronized">Get tool name</p>
    "!
    "! @parameter result | <p class="shorttext synchronized">Tool name</p>
    METHODS get_name
      RETURNING VALUE(result) TYPE string.

    "! <p class="shorttext synchronized">Check if arguments were provided</p>
    "!
    "! @parameter result | <p class="shorttext synchronized">True if arguments exist</p>
    METHODS has_arguments
      RETURNING VALUE(result) TYPE abap_bool.

    "! <p class="shorttext synchronized">Get tool arguments</p>
    "!
    "! @parameter result | <p class="shorttext synchronized">Arguments JSON</p>
    METHODS get_arguments
      RETURNING VALUE(result) TYPE REF TO zif_mcp_ajson.

  PRIVATE SECTION.
    DATA int_name          TYPE string.
    DATA int_arguments     TYPE REF TO zif_mcp_ajson.
    DATA int_has_arguments TYPE abap_bool.
ENDCLASS.

CLASS zcl_mcp_req_call_tool IMPLEMENTATION.
  METHOD constructor.
        DATA temp1 TYPE REF TO zcx_mcp_server.
      DATA temp2 TYPE REF TO zcx_mcp_server.
    " Check if name exists - it's a mandatory parameter
    IF json->exists( '/name' ) IS NOT INITIAL.
      int_name = json->get_string( '/name' ).

      " Additional validation: name should not be empty
      IF int_name IS INITIAL.
        
        CREATE OBJECT temp1 TYPE zcx_mcp_server EXPORTING textid = zcx_mcp_server=>unknown_tool msgv1 = 'Tool name cannot be empty'.
        RAISE EXCEPTION temp1 ##NO_TEXT.
      ENDIF.
    ELSE.
      
      CREATE OBJECT temp2 TYPE zcx_mcp_server EXPORTING textid = zcx_mcp_server=>required_params msgv1 = 'name'.
      RAISE EXCEPTION temp2.
    ENDIF.

    " Check for optional arguments
    IF json->exists( '/arguments' ) IS NOT INITIAL.
      int_has_arguments = abap_true.

      " Use slice to preserve the original JSON structure of arguments
      int_arguments = json->slice( '/arguments' ).
    ELSE.
      int_has_arguments = abap_false.
      int_arguments = zcl_mcp_ajson=>create_empty( ).
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
ENDCLASS.
