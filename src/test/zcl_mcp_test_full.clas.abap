"! <p class="shorttext synchronized" lang="en">Full Test Server</p>
CLASS zcl_mcp_test_full DEFINITION
  PUBLIC
  INHERITING FROM zcl_mcp_server_base FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.

  PROTECTED SECTION.
    METHODS handle_initialize   REDEFINITION.
    METHODS handle_list_prompts REDEFINITION.
    METHODS handle_get_prompt   REDEFINITION.
    METHODS handle_list_resources REDEFINITION.
    METHODS handle_resources_read REDEFINITION.
    METHODS handle_list_res_tmpls REDEFINITION.
    METHODS handle_list_tools REDEFINITION.
    METHODS handle_call_tool REDEFINITION.

  PRIVATE SECTION.
    METHODS get_gif RETURNING VALUE(result) TYPE string.
ENDCLASS.



CLASS zcl_mcp_test_full IMPLEMENTATION.
  METHOD handle_initialize.
    DATA temp1 TYPE zcl_mcp_resp_initialize=>implementation.
    DATA temp2 TYPE zcl_mcp_resp_initialize=>capabilities.
    CLEAR temp1.
    temp1-name = `Test Server with full feature set`.
    temp1-version = `1.0`.
    response-result->set_implementation( temp1 ) ##NO_TEXT.
    
    CLEAR temp2.
    temp2-prompts = abap_true.
    temp2-resources = abap_true.
    temp2-tools = abap_true.
    response-result->set_capabilities( temp2 ).
    response-result->set_instructions( `Use this server to test the implementation` ) ##NO_TEXT.
  ENDMETHOD.

  METHOD handle_list_prompts.
    DATA temp3 TYPE zcl_mcp_resp_list_prompts=>prompts.
    DATA temp4 LIKE LINE OF temp3.
    DATA temp1 TYPE zcl_mcp_resp_list_prompts=>prompt_arguments.
    DATA temp2 LIKE LINE OF temp1.
    CLEAR temp3.
    
    temp4-name = `simple`.
    temp4-description = `Simple test prompt`.
    INSERT temp4 INTO TABLE temp3.
    temp4-name = `complex`.
    temp4-description = `A more complex test prompt with two arguments`.
    
    CLEAR temp1.
    
    temp2-name = `optional`.
    temp2-description = `Optional argument`.
    temp2-required = abap_false.
    INSERT temp2 INTO TABLE temp1.
    temp2-name = `required`.
    temp2-description = `Required argument`.
    temp2-required = abap_true.
    INSERT temp2 INTO TABLE temp1.
    temp4-arguments = temp1.
    INSERT temp4 INTO TABLE temp3.
    temp4-name = `all_content_types`.
    temp4-description = `A test prompt that returns all five content types`.
    INSERT temp4 INTO TABLE temp3.
    temp4-name = `ordered`.
    temp4-description = `Multiple responses to ensure order is preserved`.
    INSERT temp4 INTO TABLE temp3.
    response-result->set_prompts(
        temp3 ) ##NO_TEXT.
  ENDMETHOD.

  METHOD handle_get_prompt.
    " In this example we handle prompts defined in handle_list_prompts
    DATA arguments TYPE zcl_mcp_req_get_prompt=>prompt_arguments.
        DATA argument TYPE zcl_mcp_req_get_prompt=>prompt_argument.
          DATA required_value LIKE argument-value.
          DATA temp5 TYPE string.
          DATA optional_text LIKE temp5.
        DATA gif TYPE string.
    arguments = request->get_arguments( ).
    CASE request->get_name( ).
      WHEN `simple`.
        " Simple prompt with no arguments
        response-result->set_description( `Simple test prompt` ) ##NO_TEXT.
        response-result->add_text_message( role = zif_mcp_server=>role_user
                                           text = |This is a simple test prompt| ) ##NO_TEXT.
      WHEN `complex`.
        " Complex prompt with required parameter
        
        READ TABLE arguments INTO argument WITH KEY key = `required`.
        IF sy-subrc <> 0.
          response-error-code    = zcl_mcp_jsonrpc=>error_codes-invalid_params.
          response-error-message = |Prompt { request->get_name( ) } requires parameter 'required'| ##NO_TEXT.
        ELSE.
          response-result->set_description( `A more complex test prompt with two arguments` ) ##NO_TEXT.

          " Store the required value
          
          required_value = argument-value.

          " Check for optional parameter
          READ TABLE arguments INTO argument WITH KEY key = `optional`.
          
          IF sy-subrc = 0.
            temp5 = | with optional parameter '{ argument-value }'|.
          ELSE.
            temp5 = ||.
          ENDIF.
          
          optional_text = temp5.

          response-result->add_text_message(
              role = zif_mcp_server=>role_user
              text = |Execute a complex test with required parameter '{ required_value }'{ optional_text }| ) ##NO_TEXT.
        ENDIF.

      WHEN `all_content_types`.
        " Test prompt that returns all five content types
        response-result->add_text_message( role = zif_mcp_server=>role_user
                                           text = |Text Message| ) ##NO_TEXT.

        
        gif = get_gif( ).
        response-result->add_image_message( role      = zif_mcp_server=>role_user
                                            data      = gif
                                            mime_type = 'image/gif' )
        ##NO_TEXT.

        response-result->add_text_resource_message( role      = zif_mcp_server=>role_user
                                                    uri       = 'file://testfile.md'
                                                    text      = '# Test File\n'
                                                    mime_type = 'text/markdown' ).

        " We reuse the gif from image content, usually you'd use e.g. PDFs or other files
        response-result->add_blob_resource_message( role      = zif_mcp_server=>role_user
                                                    uri       = 'file://okay-.gif'
                                                    blob      = gif
                                                    mime_type = 'image/gif' ).

        " We reuse the gif from image content, usually you'd use e.g. wav files
        response-result->add_audio_message( role      = zif_mcp_server=>role_user
                                            data      = gif
                                            mime_type = 'audio/wav' ).

      WHEN `ordered`.
        " Test prompt that returns multiple messages
        response-result->add_text_message( role = zif_mcp_server=>role_user
                                           text = |This is the first message| ) ##NO_TEXT.
        response-result->add_text_resource_message( role      = zif_mcp_server=>role_user
                                                    uri       = 'file://testfile.md'
                                                    text      = 'This is the second message'
                                                    mime_type = 'text/markdown' ) ##NO_TEXT.
        response-result->add_text_message( role = zif_mcp_server=>role_user
                                           text = |This is the third message| ) ##NO_TEXT.
      WHEN OTHERS.
        response-error-code    = zcl_mcp_jsonrpc=>error_codes-invalid_params.
        response-error-message = |Prompt { request->get_name( ) } not found.| ##NO_TEXT.

    ENDCASE.
  ENDMETHOD.

  METHOD get_gif.
    DATA api TYPE REF TO if_mr_api.
    DATA gif TYPE xstring.
    api = cl_mime_repository_api=>get_api( ).
    
    api->get( EXPORTING  i_url              = 'sap/public/bc/WebIcons/w_s_okay.gif'
              IMPORTING  e_content          = gif
              EXCEPTIONS parameter_missing  = 1                " Parameter missing or is initial
                         error_occured      = 2                " Unspecified Error Occurred
                         not_found          = 3                " Object not found
                         permission_failure = 4                " Missing Authorization
                         OTHERS             = 5 ).
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.
    CALL FUNCTION 'SCMS_BASE64_ENCODE_STR'
      EXPORTING
        input  = gif
      IMPORTING
        output = result
      EXCEPTIONS
        OTHERS = 0.
  ENDMETHOD.

  METHOD handle_list_resources.
    DATA temp6 TYPE zcl_mcp_resp_list_resources=>resources.
    DATA temp7 LIKE LINE OF temp6.
    CLEAR temp6.
    
    temp7-name = `OK`.
    temp7-uri = `file://sap/okay.gif`.
    temp7-description = `Okay Gif`.
    temp7-mime_type = `image/gif`.
    INSERT temp7 INTO TABLE temp6.
    temp7-name = `OK2`.
    temp7-uri = `file://sap/okay2.gif`.
    temp7-description = `Same Okay Gif`.
    temp7-mime_type = 'image/gif'.
    INSERT temp7 INTO TABLE temp6.
    temp7-name = `TextFile`.
    temp7-uri = `file://sap/text.json`.
    temp7-description = `Same Json Text`.
    temp7-mime_type = `text/json`.
    INSERT temp7 INTO TABLE temp6.
    response-result->set_resources(
        temp6 ) ##NO_TEXT.
  ENDMETHOD.

  METHOD handle_list_res_tmpls.
    DATA temp8 TYPE zcl_mcp_resp_list_res_tmpl=>resource_templates.
    DATA temp9 LIKE LINE OF temp8.
    CLEAR temp8.
    
    temp9-mime_type = 'image/gif'.
    temp9-name = `Gif`.
    temp9-uritemplate = `file://{path}`.
    temp9-description = `Gifs ...`.
    INSERT temp9 INTO TABLE temp8.
    response-result->set_resource_templates(
        temp8 ) ##NO_TEXT.
  ENDMETHOD.

  METHOD handle_resources_read.
    DATA uri TYPE zcl_mcp_req_read_resource=>resource_uri.
        DATA gif TYPE string.
    uri = request->get_uri( ).
    CASE uri.
      WHEN `file://sap/okay.gif`.
        
        gif = get_gif( ).
        response-result->add_blob_resource( uri       = uri
                                            blob      = gif
                                            mime_type = 'image/gif' ) ##NO_TEXT.
      WHEN `file://sap/okay2.gif`.
        gif = get_gif( ).
        response-result->add_blob_resource( uri       = uri
                                            blob      = gif
                                            mime_type = 'image/gif' ) ##NO_TEXT.
      WHEN `file://sap/text.json`.
        response-result->add_text_resource( uri       = uri
                                            text      = '{ "key": "value" }'
                                            mime_type = 'text/json' ) ##NO_TEXT.
      WHEN OTHERS.
        response-error-code    = zcl_mcp_jsonrpc=>error_codes-invalid_params.
        response-error-message = |Resource { uri } not found.| ##NO_TEXT.
    ENDCASE.
  ENDMETHOD.

  METHOD handle_list_tools.
        DATA schema TYPE REF TO zcl_mcp_schema_builder.
        DATA temp10 TYPE zcl_mcp_resp_list_tools=>tools.
        DATA temp11 LIKE LINE OF temp10.
    TRY.
        " Either use method chaining or ensure to update the schema reference
        
        CREATE OBJECT schema TYPE zcl_mcp_schema_builder.
        schema = schema->add_string( name        = `TextInput`
                                     description = `Input text with a maximum of 100 characters`
                                     required    = abap_true ) ##NO_TEXT.
        schema = schema->begin_array( `TestInputArray` ) ##NO_TEXT.
        schema = schema->add_integer( name        = `Line`
                                      description = `Line number`
                                      required    = abap_true ) ##NO_TEXT.
        schema = schema->add_string( name        = `Text`
                                     description = `Text`
                                     required    = abap_true ) ##NO_TEXT.
        schema = schema->end_array( ).

        
        CLEAR temp10.
        
        temp11-name = `All Content Types`.
        temp11-description = `A test tool that returns all content types`.
        INSERT temp11 INTO TABLE temp10.
        temp11-name = `Input Test`.
        temp11-description = `A test tool with a complex input`.
        temp11-input_schema = schema->to_json( ).
        INSERT temp11 INTO TABLE temp10.
        temp11-name = `Error Test`.
        temp11-description = `A test tool that always return error as truet`.
        INSERT temp11 INTO TABLE temp10.
        response-result->set_tools( temp10 ) ##NO_TEXT.
      CATCH zcx_mcp_ajson_error.
        response-error-code    = zcl_mcp_jsonrpc=>error_codes-internal_error.
        response-error-message = |Error creating tool definition| ##NO_TEXT.
    ENDTRY.
  ENDMETHOD.

  METHOD handle_call_tool.
    DATA tool_name TYPE string.
        DATA gif TYPE string.
        DATA arguments TYPE REF TO zif_mcp_ajson.
        DATA text_input TYPE string.
    tool_name = request->get_name( ).

    CASE tool_name.
      WHEN 'All Content Types'.
        response-result->add_text_content( |Text Message| ) ##NO_TEXT.
        " We reuse the gif from image content, usually you'd use appropriate files for the different content types
        
        gif = get_gif( ).
        response-result->add_image_content( data      = gif
                                            mime_type = 'image/gif' ) ##NO_TEXT.
        response-result->add_text_resource( uri       = 'file://testfile.md'
                                            text      = '# Test File\n'
                                            mime_type = 'text/markdown' ) ##NO_TEXT.
        response-result->add_blob_resource( uri       = 'file://okay-.gif'
                                            blob      = gif
                                            mime_type = 'image/gif' ) ##NO_TEXT.
        response-result->add_audio_content( data      = gif
                                            mime_type = 'audio/wav' ) ##NO_TEXT.

      WHEN 'Error Test'.
        " Note this is for when the tool was called correctly but has internal execution issues.
        " In case of a wrong call return response-error details.
        response-result->set_error( ).
        response-result->add_text_content( |This is an error test| ) ##NO_TEXT.

      WHEN 'Input Test'.
        
        arguments = request->get_arguments( ).
        
        text_input = arguments->get_string( `TextInput` ).
        DATA: BEGIN OF input_line,
                line TYPE i,
                text TYPE string,
              END OF input_line,
              input_array LIKE STANDARD TABLE OF input_line WITH DEFAULT KEY.
        DATA input_table TYPE REF TO zif_mcp_ajson.
        input_table = arguments->slice( `TestInputArray` ).
        TRY.
            input_table->to_abap( IMPORTING ev_container = input_array ).
          CATCH zcx_mcp_ajson_error.
            response-error-code    = zcl_mcp_jsonrpc=>error_codes-invalid_params.
            response-error-message = |Incorrect Input parameter format| ##NO_TEXT.
            RETURN.
        ENDTRY.

        IF text_input IS INITIAL.
          response-error-code    = zcl_mcp_jsonrpc=>error_codes-invalid_params.
          response-error-message = |Input parameter 'TextInput' is required| ##NO_TEXT.
        ELSEIF strlen( text_input ) > 100.
          response-error-code    = zcl_mcp_jsonrpc=>error_codes-invalid_params.
          response-error-message = |Input parameter 'TextInput' is too long| ##NO_TEXT.
        ENDIF.

        FIELD-SYMBOLS <input_array> LIKE LINE OF input_array.
        LOOP AT input_array ASSIGNING <input_array>.
          IF <input_array>-line IS INITIAL OR <input_array>-text IS INITIAL.
            response-error-code    = zcl_mcp_jsonrpc=>error_codes-invalid_params.
            response-error-message = |Input parameter 'TestInputArray' is required| ##NO_TEXT.
            RETURN.
          ENDIF.
          response-result->add_text_content( |Line { <input_array>-line } : { <input_array>-text }| ) ##NO_TEXT.
        ENDLOOP.
      WHEN OTHERS.
        response-error-code    = zcl_mcp_jsonrpc=>error_codes-invalid_params.
        response-error-message = |Tool { tool_name } not found.| ##NO_TEXT.
    ENDCASE.
  ENDMETHOD.

ENDCLASS.
