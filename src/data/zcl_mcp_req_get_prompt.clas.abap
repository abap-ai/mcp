"! <p class="shorttext synchronized">MCP Get Prompt Request</p>
CLASS zcl_mcp_req_get_prompt DEFINITION
  PUBLIC FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    TYPES:
      BEGIN OF prompt_argument,
        key   TYPE string,
        value TYPE string,
      END OF prompt_argument,
      prompt_arguments TYPE STANDARD TABLE OF prompt_argument WITH KEY key.

    "! <p class="shorttext synchronized">Constructor</p>
    "! Parses the Get Prompt Request JSON
    "!
    "! @parameter json | <p class="shorttext synchronized">JSON Request</p>
    METHODS constructor
      IMPORTING json TYPE REF TO zif_mcp_ajson
      RAISING   zcx_mcp_ajson_error
                zcx_mcp_server.

    "! <p class="shorttext synchronized">Get prompt name</p>
    "!
    "! @parameter result | <p class="shorttext synchronized">Prompt name</p>
    METHODS get_name
      RETURNING VALUE(result) TYPE string.

    "! <p class="shorttext synchronized">Check if arguments were provided</p>
    "!
    "! @parameter result | <p class="shorttext synchronized">True if arguments exist</p>
    METHODS has_arguments
      RETURNING VALUE(result) TYPE abap_bool.

    "! <p class="shorttext synchronized">Get prompt arguments</p>
    "!
    "! @parameter result | <p class="shorttext synchronized">Arguments table</p>
    METHODS get_arguments
      RETURNING VALUE(result) TYPE prompt_arguments.

    "! <p class="shorttext synchronized">Get _meta fields</p>
    "!
    "! @parameter result | <p class="shorttext synchronized">_meta JSON</p>
    METHODS get_meta
      RETURNING VALUE(result) TYPE REF TO zif_mcp_ajson.

  PRIVATE SECTION.
    DATA int_name          TYPE string.
    DATA int_arguments     TYPE prompt_arguments.
    DATA int_has_arguments TYPE abap_bool.
    DATA int_meta          TYPE REF TO zif_mcp_ajson.
ENDCLASS.

CLASS zcl_mcp_req_get_prompt IMPLEMENTATION.
  METHOD constructor.
        DATA temp1 TYPE REF TO zcx_mcp_server.
      DATA temp2 TYPE REF TO zcx_mcp_server.
      DATA arg_names TYPE string_table.
          DATA arg_name LIKE LINE OF arg_names.
            DATA arg TYPE prompt_argument.
          DATA ajson_error TYPE REF TO zcx_mcp_ajson_error.
          DATA temp3 TYPE REF TO zcx_mcp_server.
    " Check if name exists - it's a mandatory parameter
    IF json->exists( '/name' ) IS NOT INITIAL.
      int_name = json->get_string( '/name' ).

      " Additional validation: name should not be empty
      IF int_name IS INITIAL.
        
        CREATE OBJECT temp1 TYPE zcx_mcp_server EXPORTING textid = zcx_mcp_server=>prompt_name_invalid msgv1 = 'Name parameter cannot be empty'.
        RAISE EXCEPTION temp1 ##NO_TEXT.
      ENDIF.
    ELSE.
      
      CREATE OBJECT temp2 TYPE zcx_mcp_server EXPORTING textid = zcx_mcp_server=>required_params msgv1 = 'name'.
      RAISE EXCEPTION temp2.
    ENDIF.

    " Check for optional arguments
    IF json->exists( '/arguments' ) IS NOT INITIAL.
      int_has_arguments = abap_true.

      " We need to parse arguments as a map
      
      TRY.
          arg_names = json->members( '/arguments' ).

          
          LOOP AT arg_names INTO arg_name.
            
            arg-key   = arg_name.
            arg-value = json->get_string( |/arguments/{ arg_name }| ).
            APPEND arg TO int_arguments.
          ENDLOOP.
          
        CATCH zcx_mcp_ajson_error INTO ajson_error.
          " Convert JSON errors related to arguments to parameter errors
          
          CREATE OBJECT temp3 TYPE zcx_mcp_server EXPORTING textid = zcx_mcp_server=>invalid_arguments msgv1 = 'Error parsing arguments' previous = ajson_error.
          RAISE EXCEPTION temp3 ##NO_TEXT.
      ENDTRY.
    ELSE.
      int_has_arguments = abap_false.
      CLEAR int_arguments.
    ENDIF.

    " Check for _meta fields
    IF json->exists( '/_meta' ) IS NOT INITIAL.
      int_meta = json->slice( '/_meta' ).
    ELSE.
      int_meta = zcl_mcp_ajson=>create_empty( ).
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
ENDCLASS.
