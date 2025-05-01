"! <p class="shorttext synchronized" lang="en">Test Server for ICF based sessions</p>
CLASS zcl_mcp_test_icf_session DEFINITION
  PUBLIC
  INHERITING FROM zcl_mcp_server_base FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.

  PROTECTED SECTION.
    METHODS handle_initialize REDEFINITION.
    METHODS handle_list_tools REDEFINITION.
    METHODS handle_call_tool  REDEFINITION.

  PRIVATE SECTION.
    DATA curr_increment TYPE i VALUE 0.
ENDCLASS.



CLASS zcl_mcp_test_icf_session IMPLEMENTATION.
  METHOD handle_initialize.
    response-result->set_implementation( implementation = VALUE #( name    = `Simple Test Server to check ICF sessions`
                                                                   version = `1.0` ) ) ##NO_TEXT.
    response-result->set_capabilities( VALUE #( tools = abap_true ) ).
  ENDMETHOD.

  METHOD handle_list_tools.
    TRY.
        DATA(schema) = NEW zcl_mcp_schema_builder( )->add_integer( name        = `increment`
                                                                   description = `Increment the given number`
                                                                   required    = abap_true ) ##NO_TEXT.
        response-result->set_tools(
            VALUE #(
                ( name = `Test ICF Session` description = `Using ICF sessions we increment by the given number` input_schema = schema->to_json( ) ) ) ) ##NO_TEXT.
      CATCH zcx_mcp_ajson_error.
        response-error-code    = zcl_mcp_jsonrpc=>error_codes-internal_error.
        response-error-message = |Error creating tool definition| ##NO_TEXT.
    ENDTRY.
  ENDMETHOD.

  METHOD handle_call_tool.
    CASE request->get_name( ).
      WHEN `Test ICF Session`.
        DATA(input) = request->get_arguments( ).
        DATA(increment) = input->get_integer( `increment` ).
        IF increment IS INITIAL.
          response-error-code    = zcl_mcp_jsonrpc=>error_codes-invalid_params.
          response-error-message = |Increment value is required.| ##NO_TEXT.
          RETURN.
        ENDIF.

        curr_increment = curr_increment + increment.
        response-result->add_text_content( |Incremented value: { curr_increment }| ) ##NO_TEXT.

        " Store the new value in the session
        session->add( VALUE #( key   = `increment`
                               value = curr_increment ) ).
      WHEN OTHERS.
        response-error-code    = zcl_mcp_jsonrpc=>error_codes-invalid_params.
        response-error-message = |Tool { request->get_name( ) } not found.| ##NO_TEXT.
    ENDCASE.
  ENDMETHOD.

ENDCLASS.
