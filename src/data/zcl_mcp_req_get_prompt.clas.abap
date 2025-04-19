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

  PRIVATE SECTION.
    DATA int_name TYPE string.
    DATA int_arguments TYPE prompt_arguments.
    DATA int_has_arguments TYPE abap_bool.
ENDCLASS.

CLASS zcl_mcp_req_get_prompt IMPLEMENTATION.
  METHOD constructor.
    " Check if name exists - it's a mandatory parameter
    IF json->exists( '/name' ).
      int_name = json->get_string( '/name' ).

      " Additional validation: name should not be empty
      IF int_name IS INITIAL.
        RAISE EXCEPTION NEW zcx_mcp_server( textid = zcx_mcp_server=>prompt_name_invalid
                                            msgv1  = 'Name parameter cannot be empty' ) ##NO_TEXT.
      ENDIF.
    ELSE.
      RAISE EXCEPTION NEW zcx_mcp_server( textid = zcx_mcp_server=>required_params
                                          msgv1  = 'name' ).
    ENDIF.

    " Check for optional arguments
    IF json->exists( '/arguments' ).
      int_has_arguments = abap_true.

      " We need to parse arguments as a map
      DATA arg_names TYPE string_table.
      TRY.
          arg_names = json->members( '/arguments' ).

          LOOP AT arg_names INTO DATA(arg_name).
            DATA arg TYPE prompt_argument.
            arg-key   = arg_name.
            arg-value = json->get_string( |/arguments/{ arg_name }| ).
            APPEND arg TO int_arguments.
          ENDLOOP.
        CATCH zcx_mcp_ajson_error INTO DATA(ajson_error).
          " Convert JSON errors related to arguments to parameter errors
          RAISE EXCEPTION NEW zcx_mcp_server( textid   = zcx_mcp_server=>invalid_arguments
                                              msgv1    = 'Error parsing arguments'
                                              previous = ajson_error ) ##NO_TEXT.
      ENDTRY.
    ELSE.
      int_has_arguments = abap_false.
      CLEAR int_arguments.
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
