"! <p class="shorttext synchronized" lang="en">Demo MCP Server using ICF Sessions</p>
CLASS zcl_mcp_demo_server_icfsession DEFINITION
  PUBLIC
  INHERITING FROM zcl_mcp_server_base FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
  PROTECTED SECTION.
    METHODS handle_initialize REDEFINITION.
    METHODS handle_list_tools REDEFINITION.
    METHODS handle_call_tool  REDEFINITION.
    METHODS: get_session_mode REDEFINITION.

  PRIVATE SECTION.
    "! <p class="shorttext synchronized">Get session details for the current ICF session</p>
    "!
    "! @parameter response | <p class="shorttext synchronized">Response object with session details</p>
    METHODS get_session_details CHANGING !response TYPE zif_mcp_server=>call_tool_response.

    "! <p class="shorttext synchronized">Increment the current value by a given amount</p>
    "!
    "! @parameter request  | <p class="shorttext synchronized">The call tool request containing increment parameter</p>
    "! @parameter response | <p class="shorttext synchronized">Response object with the incremented result</p>
    METHODS increment_example IMPORTING !request  TYPE REF TO zcl_mcp_req_call_tool
                              CHANGING  !response TYPE zif_mcp_server=>call_tool_response.

    "! <p class="shorttext synchronized">Get the JSON schema for the increment example</p>
    "!
    "! @parameter result              | <p class="shorttext synchronized">Schema builder object containing increment definition</p>
    "! @raising   zcx_mcp_ajson_error | <p class="shorttext synchronized">Error during JSON schema creation</p>
    METHODS get_increment_schema
      RETURNING VALUE(result) TYPE REF TO zcl_mcp_schema_builder
      RAISING   zcx_mcp_ajson_error.

    " In ICF Session we can store data here and do not need to pass it around
    DATA current_increment TYPE i.
    DATA increment_schema  TYPE REF TO zcl_mcp_schema_builder.
ENDCLASS.

CLASS zcl_mcp_demo_server_icfsession IMPLEMENTATION.
  METHOD handle_initialize.
    response-result->set_capabilities( VALUE #( prompts   = abap_true
                                                resources = abap_true
                                                tools     = abap_true ) ).
    response-result->set_implementation( VALUE #( name    = `Demo MCP Server - using ICF session logic. Tools only.`
                                                  version = `1.0.0` ) ) ##NO_TEXT.
    response-result->set_instructions(
        `Use the features provided by this server only if explicitly requested. If not sure ask the user!` ) ##NO_TEXT.
  ENDMETHOD.

  METHOD handle_list_tools.
    DATA tools TYPE zcl_mcp_resp_list_tools=>tools.

    APPEND VALUE #( name        = `get_session_details`
                    description = `Get details about the current session.`  ) TO tools ##NO_TEXT.

    " Demo tool with input parameters
    TRY.
        DATA(schema) = NEW zcl_mcp_schema_builder( ).
        schema->add_integer( name        = `increment`
                             description = `Increment value`
                             required    = abap_true ) ##NO_TEXT.

        APPEND VALUE #(
            name         = `increment_example`
            description  = `Every time increments the result by the given number. This is to demonstrate the session logic.`
            input_schema = schema->to_json( ) )
               TO tools ##NO_TEXT.
      CATCH zcx_mcp_ajson_error INTO DATA(error).
        response-error-code    = zcl_mcp_jsonrpc=>error_codes-internal_error.
        response-error-message = error->get_text( ).
    ENDTRY.

    response-result->set_tools( tools ).
  ENDMETHOD.

  METHOD handle_call_tool.
    CASE request->get_name( ).
      WHEN `get_sesion_details`.
        get_session_details( CHANGING response = response ).
      WHEN `increment_example`.
        increment_example( EXPORTING request = request CHANGING response = response ).
      WHEN OTHERS.
        response-error-code    = zcl_mcp_jsonrpc=>error_codes-invalid_params.
        response-error-message = |Tool { request->get_name( ) } not found.| ##NO_TEXT.
    ENDCASE.
  ENDMETHOD.

  METHOD get_session_details.
    response-result->add_text_content( |Session ID: { server-session_id }| ) ##NO_TEXT.
  ENDMETHOD.

  METHOD increment_example.
    DATA(input) = request->get_arguments( ).
    " Validate input parameter via schema validator class
    TRY.
        DATA(schema) = get_increment_schema( ).
        DATA(validator) = NEW zcl_mcp_schema_validator( schema->to_json( ) ).
        DATA(validation_result) = validator->validate( input ).
        IF validation_result = abap_false.
          response-error-code    = zcl_mcp_jsonrpc=>error_codes-invalid_params.
          response-error-message = concat_lines_of( validator->get_errors( ) ).
          RETURN.
        ENDIF.
      CATCH zcx_mcp_ajson_error INTO DATA(error).
        response-error-code    = zcl_mcp_jsonrpc=>error_codes-internal_error.
        response-error-message = error->get_text( ).
        RETURN.
    ENDTRY.

    DATA(increment) = input->get_integer( `increment` ).

    current_increment = current_increment + increment.
    response-result->add_text_content( |Incremented value: { current_increment }| ) ##NO_TEXT.
  ENDMETHOD.

  METHOD get_session_mode.
    result = zcl_mcp_session=>session_mode_icf.
  ENDMETHOD.

  METHOD get_increment_schema.
    IF increment_schema IS BOUND.
      result = increment_schema.
      RETURN.
    ELSE.
      increment_schema = NEW zcl_mcp_schema_builder( ).
      increment_schema->add_integer( name        = `increment`
                                     description = `Increment value`
                                     required    = abap_true
                                     minimum     = 1
                                     maximum     = 1000000 ) ##NO_TEXT.
      result = increment_schema.
    ENDIF.
  ENDMETHOD.

ENDCLASS.
