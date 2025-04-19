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

  PRIVATE SECTION.
    METHODS get_session_details CHANGING  response TYPE zif_mcp_server=>call_tool_response.

    METHODS increment_example IMPORTING !request TYPE REF TO zcl_mcp_req_call_tool
                              CHANGING  response TYPE zif_mcp_server=>call_tool_response.

    "In ICF Session we can store data here and do not need to pass it around
    data current_increment TYPE i.
ENDCLASS.



CLASS zcl_mcp_demo_server_icfsession IMPLEMENTATION.
  METHOD handle_initialize.
    DATA temp1 TYPE zcl_mcp_resp_initialize=>capabilities.
    DATA temp2 TYPE zcl_mcp_resp_initialize=>implementation.
    CLEAR temp1.
    temp1-prompts = abap_true.
    temp1-resources = abap_true.
    temp1-tools = abap_true.
    response-result->set_capabilities( temp1 ).
    
    CLEAR temp2.
    temp2-name = `Demo MCP Server - using ICF session logic. Tools only.`.
    temp2-version = `1.0.0`.
    response-result->set_implementation( temp2 ) ##NO_TEXT.
    response-result->set_instructions(
        `Use the features provided by this server only if explicitly requested. If not sure ask the user!` ) ##NO_TEXT.
  ENDMETHOD.

  METHOD handle_list_tools.
    DATA tools TYPE zcl_mcp_resp_list_tools=>tools.

    DATA temp3 TYPE zcl_mcp_resp_list_tools=>tool.
        DATA schema TYPE REF TO zcl_mcp_schema_builder.
        DATA temp4 TYPE zcl_mcp_resp_list_tools=>tool.
        DATA error TYPE REF TO zcx_mcp_ajson_error.
    CLEAR temp3.
    temp3-name = `get_session_details`.
    temp3-description = `Get details about the current session.`.
    APPEND temp3 TO tools ##NO_TEXT.

    " Demo tool with input parameters
    TRY.
        
        CREATE OBJECT schema TYPE zcl_mcp_schema_builder.
        schema->add_integer( name        = `increment`
                             description = `Increment value`
                             required    = abap_true ) ##NO_TEXT.

        
        CLEAR temp4.
        temp4-name = `increment_example`.
        temp4-description = `Every time increments the result by the given number. This is to demonstrate the session logic.`.
        temp4-input_schema = schema->to_json( ).
        APPEND temp4
               TO tools ##NO_TEXT.
        
      CATCH zcx_mcp_ajson_error INTO error.
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
    DATA input TYPE REF TO zif_mcp_ajson.
    DATA increment TYPE i.
    input = request->get_arguments( ).
    
    increment = input->get_integer( `increment` ).
    IF increment IS INITIAL.
      response-error-code    = zcl_mcp_jsonrpc=>error_codes-invalid_params.
      response-error-message = |Increment value is required.| ##NO_TEXT.
      RETURN.
    ENDIF.

    current_increment = current_increment + increment.
    response-result->add_text_content( |Incremented value: { current_increment }| ) ##NO_TEXT.
  ENDMETHOD.

ENDCLASS.
