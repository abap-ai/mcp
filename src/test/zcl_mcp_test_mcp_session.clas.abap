"! <p class="shorttext synchronized" lang="en">Test Server for MCP based sessions</p>
CLASS zcl_mcp_test_mcp_session DEFINITION
  PUBLIC
  INHERITING FROM zcl_mcp_server_base FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.

  PROTECTED SECTION.
    METHODS handle_initialize REDEFINITION.
    METHODS handle_list_tools REDEFINITION.
    METHODS handle_call_tool  REDEFINITION.

  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_mcp_test_mcp_session IMPLEMENTATION.
  METHOD handle_initialize.
    DATA temp1 TYPE zcl_mcp_resp_initialize=>implementation.
    DATA temp2 TYPE zcl_mcp_resp_initialize=>capabilities.
    CLEAR temp1.
    temp1-name = `Simple Test Server to check MCP sessions`.
    temp1-version = `1.0`.
    response-result->set_implementation( temp1 ) ##NO_TEXT.
    
    CLEAR temp2.
    temp2-tools = abap_true.
    response-result->set_capabilities( temp2 ).
  ENDMETHOD.

  METHOD handle_list_tools.
        DATA schema TYPE REF TO zcl_mcp_schema_builder.
        DATA temp1 TYPE REF TO zcl_mcp_schema_builder.
        DATA temp3 TYPE zcl_mcp_resp_list_tools=>tools.
        DATA temp4 LIKE LINE OF temp3.
    TRY.
        
        
        CREATE OBJECT temp1 TYPE zcl_mcp_schema_builder.
        schema = temp1->add_integer( name        = `increment`
                                                                   description = `Increment the given number`
                                                                   required    = abap_true ) ##NO_TEXT.
        
        CLEAR temp3.
        
        temp4-name = `Test MCP Session`.
        temp4-description = `Using MCP sessions we increment by the given number`.
        temp4-input_schema = schema->to_json( ).
        INSERT temp4 INTO TABLE temp3.
        response-result->set_tools(
            temp3 ) ##NO_TEXT.
      CATCH zcx_mcp_ajson_error.
        response-error-code    = zcl_mcp_jsonrpc=>error_codes-internal_error.
        response-error-message = |Error creating tool definition| ##NO_TEXT.
    ENDTRY.
  ENDMETHOD.

  METHOD handle_call_tool.
        DATA input TYPE REF TO zif_mcp_ajson.
        DATA increment TYPE i.
        DATA session_increment TYPE zcl_mcp_session=>session_entry.
        DATA current_increment TYPE i.
        DATA temp5 TYPE zcl_mcp_session=>session_entry.
    CASE request->get_name( ).
      WHEN `Test MCP Session`.
        
        input = request->get_arguments( ).
        
        increment = input->get_integer( `increment` ).
        IF increment IS INITIAL.
          response-error-code    = zcl_mcp_jsonrpc=>error_codes-invalid_params.
          response-error-message = |Increment value is required.| ##NO_TEXT.
          RETURN.
        ENDIF.

        " Get the last increment value from the session
        
        session_increment = session->get( `increment` ).
        
        IF session_increment IS INITIAL.
          " No value in the session, set it to 0
          current_increment = 0.
        ELSE.
          current_increment = session_increment-value.
        ENDIF.
        current_increment = current_increment + increment.
        response-result->add_text_content( |Incremented value: { current_increment }| ) ##NO_TEXT.

        " Store the new value in the session
        
        CLEAR temp5.
        temp5-key = `increment`.
        temp5-value = current_increment.
        session->add( temp5 ).
      WHEN OTHERS.
        response-error-code    = zcl_mcp_jsonrpc=>error_codes-invalid_params.
        response-error-message = |Tool { request->get_name( ) } not found.| ##NO_TEXT.
    ENDCASE.
  ENDMETHOD.

ENDCLASS.
