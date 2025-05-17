"! <p class="shorttext synchronized" lang="en">Demo MCP Server using MCP Sessions</p>
CLASS zcl_mcp_demo_server_mcpsession DEFINITION
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
    METHODS get_session_details CHANGING  response TYPE zif_mcp_server=>call_tool_response.

    METHODS increment_example IMPORTING !request TYPE REF TO zcl_mcp_req_call_tool
                              CHANGING  response TYPE zif_mcp_server=>call_tool_response.
ENDCLASS.



CLASS zcl_mcp_demo_server_mcpsession IMPLEMENTATION.
  METHOD handle_initialize.
    response-result->set_capabilities( VALUE #( prompts   = abap_true
                                                resources = abap_true
                                                tools     = abap_true ) ).
    response-result->set_implementation( VALUE #( name    = `Demo MCP Server - using MCP session logic. Tools only.`
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
    DATA(increment) = input->get_integer( `increment` ).
    IF increment IS INITIAL.
      response-error-code    = zcl_mcp_jsonrpc=>error_codes-invalid_params.
      response-error-message = |Increment value is required.| ##NO_TEXT.
      RETURN.
    ENDIF.

    " Get the last increment value from the session
    DATA(session_increment) = session->get( `increment` ).
    DATA current_increment TYPE i.
    IF session_increment IS INITIAL.
      " No value in the session, set it to 0
      current_increment = 0.
    ELSE.
      current_increment = session_increment-value.
    ENDIF.
    current_increment = current_increment + increment.
    response-result->add_text_content( |Incremented value: { current_increment }| ) ##NO_TEXT.

    " Store the new value in the session
    session->add( VALUE #( key   = `increment`
                                          value = current_increment ) ).
  ENDMETHOD.

  METHOD get_session_mode.
    result = zcl_mcp_session=>session_mode_mcp.
  ENDMETHOD.

ENDCLASS.
