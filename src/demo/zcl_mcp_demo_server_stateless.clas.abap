CLASS zcl_mcp_demo_server_stateless DEFINITION
  PUBLIC
  INHERITING FROM zcl_mcp_server_base
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
  PROTECTED SECTION.
    METHODS handle_initialize REDEFINITION.
    METHODS handle_list_prompts REDEFINITION.
    METHODS handle_get_prompt REDEFINITION.
    METHODS handle_list_resources REDEFINITION.
    METHODS handle_list_res_tmpls REDEFINITION.
    METHODS handle_resources_read REDEFINITION.
    METHODS handle_list_tools REDEFINITION.
    METHODS handle_call_tool REDEFINITION.
  PRIVATE SECTION.

    METHODS get_server_time IMPORTING !request TYPE REF TO zcl_mcp_req_call_tool
                            CHANGING  response TYPE zif_mcp_server=>call_tool_response.
    METHODS get_flight_conn_details IMPORTING !request TYPE REF TO zcl_mcp_req_call_tool
                                    CHANGING  response TYPE zif_mcp_server=>call_tool_response.
ENDCLASS.



CLASS zcl_mcp_demo_server_stateless IMPLEMENTATION.
  METHOD handle_initialize.
    DATA temp1 TYPE zcl_mcp_resp_initialize=>capabilities.
    DATA temp2 TYPE zcl_mcp_resp_initialize=>implementation.
    CLEAR temp1.
    temp1-prompts = abap_true.
    temp1-resources = abap_true.
    temp1-tools = abap_true.
    response-result->set_capabilities( temp1 ).
    
    CLEAR temp2.
    temp2-name = `Demo MCP Server`.
    temp2-version = `1.0.0`.
    response-result->set_implementation( temp2 ) ##NO_TEXT.
    response-result->set_instructions( `Use the features provided by this server only if explicitely requested. If not sure ask the user!` ) ##NO_TEXT.
  ENDMETHOD.

  METHOD handle_list_prompts.
    " In this demo instance we only have two prompts, therefore
    " we do not consider cursor and max_list_results.
    DATA temp3 TYPE zcl_mcp_resp_list_prompts=>prompts.
    DATA temp4 LIKE LINE OF temp3.
    DATA temp1 TYPE zcl_mcp_resp_list_prompts=>prompt_arguments.
    DATA temp2 LIKE LINE OF temp1.
    DATA temp5 TYPE zcl_mcp_resp_list_prompts=>prompt_arguments.
    DATA temp6 LIKE LINE OF temp5.
    CLEAR temp3.
    
    temp4-name = `greet`.
    temp4-description = `Asks the LLM to greet someone.`.
    
    CLEAR temp1.
    
    temp2-name = `name`.
    temp2-description = `Name of the person to greet`.
    temp2-required = abap_true.
    INSERT temp2 INTO TABLE temp1.
    temp4-arguments = temp1.
    INSERT temp4 INTO TABLE temp3.
    temp4-name = `joke`.
    temp4-description = `Asks the LLM to tell a joke about a specific topic`.
    
    CLEAR temp5.
    
    temp6-name = `topic`.
    temp6-description = `Topic to joke about`.
    temp6-required = abap_true.
    INSERT temp6 INTO TABLE temp5.
    temp4-arguments = temp5.
    INSERT temp4 INTO TABLE temp3.
    response-result->set_prompts(
        temp3 ) ##NO_TEXT.
  ENDMETHOD.

  METHOD handle_get_prompt.
    " In this example we always return a text prompt only.
    DATA arguments TYPE zcl_mcp_req_get_prompt=>prompt_arguments.
        DATA argument TYPE zcl_mcp_req_get_prompt=>prompt_argument.
    arguments = request->get_arguments( ).
    CASE request->get_name( ).
      WHEN `greet`.
        
        READ TABLE arguments INTO argument WITH KEY key = `name`.
        IF sy-subrc <> 0.
          response-error-code    = zcl_mcp_jsonrpc=>error_codes-invalid_params.
          response-error-message = |Prompt { request->get_name( ) } requires parameter 'name'| ##NO_TEXT.
        ELSE.
          response-result->set_description( `Greet prompt` ) ##NO_TEXT.

          response-result->add_text_message( role = zif_llm_mcp_server=>role_user
                                             text = |Happily greet { argument-value } and wish them a great day| ) ##NO_TEXT.
        ENDIF.
      WHEN `joke`.
        READ TABLE arguments INTO argument WITH KEY key = `topic`.
        IF sy-subrc <> 0.
          response-error-code    = zcl_mcp_jsonrpc=>error_codes-invalid_params.
          response-error-message = |Prompt { request->get_name( ) } requires parameter 'topic'| ##NO_TEXT.
        ELSE.
          response-result->set_description( `Joke prompt` ) ##NO_TEXT.

          response-result->add_text_message( role = zif_llm_mcp_server=>role_user
                                             text = |Tell a great joke about the topic { argument-value }| ) ##NO_TEXT.
        ENDIF.
      WHEN OTHERS.
        response-error-code    = zcl_mcp_jsonrpc=>error_codes-invalid_params.
        response-error-message = |Prompt { request->get_name( ) } unknown.| ##NO_TEXT.
    ENDCASE.
  ENDMETHOD.

  METHOD handle_list_resources.
    " In this demo instance we only have one static resource, therefore
    " we do not consider cursor and max_list_results.

    DATA temp5 TYPE zcl_mcp_resp_list_resources=>resources.
    DATA temp6 LIKE LINE OF temp5.
    CLEAR temp5.
    
    temp6-uri = `abap://classes/zcl_demo`.
    temp6-name = `zcl_demo.class`.
    temp6-description = `Demo Class`.
    temp6-mime_type = `text/x-abap`.
    INSERT temp6 INTO TABLE temp5.
    response-result->set_resources(
        temp5 ) ##NO_TEXT.
  ENDMETHOD.

  METHOD handle_list_res_tmpls.
    " In this demo instance we only have one templated resource, therefore
    " we do not consider cursor and max_list_results.

    DATA temp7 TYPE zcl_mcp_resp_list_res_tmpl=>resource_templates.
    DATA temp8 LIKE LINE OF temp7.
    CLEAR temp7.
    
    temp8-uritemplate = `file://sales_reciept/{sales_order}`.
    temp8-name = `Sales Reciepts`.
    temp8-description = `Reciepts for Sales Order`.
    temp8-mime_type = `application/pdf`.
    INSERT temp8 INTO TABLE temp7.
    response-result->set_resource_templates(
        temp7 ) ##NO_TEXT.
  ENDMETHOD.

  METHOD handle_resources_read.
      DATA text TYPE string.
    " For now just one example supported.
    " No example for the dynamic resource for now.

    IF request->get_uri( ) = `abap://classes/zcl_demo`.
      
      text = |CLASS zcl_demo DEFINITION PUBLIC FINAL CREATE PUBLIC.\n|
                      && |  PUBLIC SECTION.\n|
                      && |    METHODS: get_text RETURNING VALUE(rv_text) TYPE string.\n|
                      && |ENDCLASS.\n\n|
                      && |CLASS zcl_demo IMPLEMENTATION.\n|
                      && |  METHOD get_text.\n|
                      && |    rv_text = 'Hello World'.\n|
                      && |  ENDMETHOD.\n|
                      && |ENDCLASS.| ##NO_TEXT.

      response-result->add_text_resource( uri       = request->get_uri( )
                                          mime_type = `text/x-abap`
                                          text      = text ).
    ELSE.
      response-error-code    = zcl_mcp_jsonrpc=>error_codes-invalid_params.
      response-error-message = |Resource { request->get_uri( ) } not found.| ##NO_TEXT.
    ENDIF.
  ENDMETHOD.

  METHOD handle_list_tools.
    DATA tools TYPE zcl_mcp_resp_list_tools=>tools.

    " Demo Tool without any input parameter
    DATA temp9 TYPE zcl_mcp_resp_list_tools=>tool.
        DATA schema TYPE REF TO zcl_mcp_schema_builder.
        DATA temp10 TYPE string_table.
        DATA temp12 TYPE zcl_mcp_resp_list_tools=>tool.
        DATA error TYPE REF TO zcx_mcp_ajson_error.
    CLEAR temp9.
    temp9-name = `get_server_time`.
    temp9-description = `Get the current server date and time in internal format.`.
    APPEND temp9 TO tools ##NO_TEXT.

    " Demo tool with input parameters
    TRY.
        
        CREATE OBJECT schema TYPE zcl_mcp_schema_builder.
        
        CLEAR temp10.
        INSERT `AA` INTO TABLE temp10.
        INSERT `AB` INTO TABLE temp10.
        INSERT `AC` INTO TABLE temp10.
        schema->add_string( name        = `airline_code`
                            description = `Airline Code`
                            required    = abap_true
                            enum        = temp10 ) ##NO_TEXT.
        schema->add_integer( name        = `flight_number`
                             description = `Flight Number`
                             required    = abap_true ) ##NO_TEXT.

        
        CLEAR temp12.
        temp12-name = `get_flight_conn_details`.
        temp12-description = `Get details of one specific flight connection`.
        temp12-input_schema = schema->to_json( ).
        APPEND temp12
               TO tools ##NO_TEXT.
        
      CATCH zcx_mcp_ajson_error INTO error.
        response-error-code    = zcl_mcp_jsonrpc=>error_codes-internal_error.
        response-error-message = error->get_text( ).
    ENDTRY.

    response-result->set_tools( tools ).
  ENDMETHOD.

  METHOD handle_call_tool.
    CASE request->get_name( ).
      WHEN `get_server_time`.
        get_server_time( EXPORTING request = request CHANGING response = response ).
      WHEN `get_flight_conn_details`.
        get_flight_conn_details( EXPORTING request = request CHANGING response = response ).
      WHEN OTHERS.
        response-error-code    = zcl_mcp_jsonrpc=>error_codes-invalid_params.
        response-error-message = |Tool { request->get_name( ) } not found.| ##NO_TEXT.
    ENDCASE.
  ENDMETHOD.

  METHOD get_flight_conn_details.
    DATA input TYPE REF TO zif_mcp_ajson.
    DATA airline_code TYPE string.
    DATA flight_number TYPE i.
    DATA connid TYPE s_conn_id.
TYPES BEGIN OF temp13.
TYPES carrid TYPE sflight-carrid.
TYPES connid TYPE sflight-connid.
TYPES fldate TYPE sflight-fldate.
TYPES price TYPE sflight-price.
TYPES currency TYPE sflight-currency.
TYPES planetype TYPE sflight-planetype.
TYPES END OF temp13.
    DATA flights TYPE STANDARD TABLE OF temp13 WITH DEFAULT KEY.
    DATA markdown TYPE string.
    FIELD-SYMBOLS <flight> LIKE LINE OF flights.
    input = request->get_arguments( ).
    
    airline_code = input->get_string( `airline_code` ).
    
    flight_number = input->get_integer( `flight_number` ).

    " Validate input parameter

    IF airline_code <> `AA` AND airline_code <> `AB` AND airline_code <> `AC`.
      response-error-code    = zcl_mcp_jsonrpc=>error_codes-invalid_params.
      response-error-message = |Only AA, AB, AC allowed. Got { airline_code }| ##NO_TEXT.
      RETURN.
    ENDIF.

    IF flight_number > 9999 OR flight_number < 0.
      response-error-code    = zcl_mcp_jsonrpc=>error_codes-invalid_params.
      response-error-message = |Flight Connection Number must be between 0 and 9999| ##NO_TEXT.
      RETURN.
    ENDIF.

    
    connid = flight_number.

    " Select only the required fields
    
    
    SELECT carrid connid fldate price currency planetype
      FROM sflight INTO TABLE flights
      WHERE carrid = airline_code AND connid = connid
      ORDER BY fldate
      .

    IF sy-subrc <> 0.
      response-result->add_text_content( |No flights found for airline { airline_code } and connection { connid }| ) ##NO_TEXT.
      RETURN.
    ENDIF.

    " Create markdown table
    
    markdown = |## Flight Connection Details\n\n|.

    " Add table headers
    markdown = |{ markdown }\| Airline \| Connection \| Flight Date \| Price \| Currency \| Plane Type \|\n| ##NO_TEXT.
    markdown = |{ markdown }\|---------\|------------\|-------------\|-------\|----------\|------------\|\n| ##NO_TEXT.

    " Add table rows
    
    LOOP AT flights ASSIGNING <flight>.
      markdown = markdown &&
        |\| { <flight>-carrid } \| { <flight>-connid } \| { <flight>-fldate DATE = USER } \| { <flight>-price } \| { <flight>-currency } \| { <flight>-planetype } \|\n|.
    ENDLOOP.

    " Handle no results case
    IF sy-subrc <> 0.
      markdown = |{ markdown }\| No flights found for airline { airline_code } and connection { connid } \|\n| ##NO_TEXT.
    ENDIF.

    response-result->add_text_content( markdown ).
  ENDMETHOD.

  METHOD get_server_time.
    response-result->add_text_content( |Current Server Date: { sy-datum } Time: { sy-uzeit } in internal format.| ) ##NO_TEXT.
  ENDMETHOD.

ENDCLASS.
