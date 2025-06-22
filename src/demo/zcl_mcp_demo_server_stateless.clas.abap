"! <p class="shorttext synchronized" lang="en">Demo MCP Server</p>
CLASS zcl_mcp_demo_server_stateless DEFINITION
  PUBLIC
  INHERITING FROM zcl_mcp_server_base
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
  PROTECTED SECTION.
    METHODS handle_initialize     REDEFINITION.
    METHODS handle_list_prompts   REDEFINITION.
    METHODS handle_get_prompt     REDEFINITION.
    METHODS handle_list_resources REDEFINITION.
    METHODS handle_list_res_tmpls REDEFINITION.
    METHODS handle_resources_read REDEFINITION.
    METHODS handle_list_tools     REDEFINITION.
    METHODS handle_call_tool      REDEFINITION.
    METHODS get_session_mode      REDEFINITION.

  PRIVATE SECTION.
    "! <p class="shorttext synchronized">Retrieves current server time</p>
    "! Fetches the system date and time and returns it in internal format
    "!
    "! @parameter response | <p class="shorttext synchronized">Response object to be filled with server time</p>
    METHODS get_server_time CHANGING !response TYPE zif_mcp_server=>call_tool_response
                            RAISING
                                     zcx_mcp_ajson_error.

    "! <p class="shorttext synchronized">Fetches flight connection details</p>
    "! Retrieves information about a specific flight connection based on airline code and flight number
    "!
    "! @parameter request  | <p class="shorttext synchronized">Request object containing airline code and flight number</p>
    "! @parameter response | <p class="shorttext synchronized">Response object to be filled with flight details</p>
    METHODS get_flight_conn_details IMPORTING !request  TYPE REF TO zcl_mcp_req_call_tool
                                    CHANGING  !response TYPE zif_mcp_server=>call_tool_response
                                    RAISING
                                              zcx_mcp_ajson_error.

    "! <p class="shorttext synchronized">Creates schema for flight connection query</p>
    "! Builds a schema definition that validates flight connection query parameters
    "!
    "! @parameter result              | <p class="shorttext synchronized">Schema builder object with flight connection parameters</p>
    "! @raising   zcx_mcp_ajson_error | <p class="shorttext synchronized">Error when creating JSON schema</p>
    METHODS get_flight_conn_schema RETURNING VALUE(result) TYPE REF TO zcl_mcp_schema_builder
                                   RAISING   zcx_mcp_ajson_error.
ENDCLASS.



CLASS zcl_mcp_demo_server_stateless IMPLEMENTATION.
  METHOD handle_initialize.
    response-result->set_capabilities( VALUE #( prompts   = abap_true
                                                resources = abap_true
                                                tools     = abap_true ) ).
    response-result->set_implementation( VALUE #( name    = `Demo MCP Server`
                                                  version = `1.0.0` ) ) ##NO_TEXT.
    " TODO: check spelling: explicitely (typo) -> explicitly (ABAP cleaner)
    response-result->set_instructions(
        `Use the features provided by this server only if explicitely requested. If not sure ask the user!` ) ##NO_TEXT.
  ENDMETHOD.

  METHOD handle_list_prompts.
    " In this demo instance we only have two prompts, therefore
    " we do not consider cursor and max_list_results.
    response-result->set_prompts(
        VALUE #(
            ( name        = `greet`
              description = `Asks the LLM to greet someone.`
              title       = `greet someone`
              arguments   = VALUE #(
                  ( name = `name` description = `Name of the person to greet` required = abap_true title = `Name to greet` ) ) )
            ( name        = `joke`
              description = `Asks the LLM to tell a joke about a specific topic`
              title       = `Tell a joke`
              arguments   = VALUE #(
                  ( name = `topic` description = `Topic to joke about` required = abap_true title = `Joke topic` ) ) ) ) ) ##NO_TEXT.
  ENDMETHOD.

  METHOD handle_get_prompt.
    " In this example we always return a text prompt only.
    DATA(arguments) = request->get_arguments( ).
    CASE request->get_name( ).
      WHEN `greet`.
        READ TABLE arguments INTO DATA(argument) WITH KEY key = `name`.
        IF sy-subrc <> 0.
          response-error-code    = zcl_mcp_jsonrpc=>error_codes-invalid_params.
          response-error-message = |Prompt { request->get_name( ) } requires parameter 'name'| ##NO_TEXT.
        ELSE.
          response-result->set_description( `Greet prompt` ) ##NO_TEXT.

          response-result->add_text_message( role = zif_mcp_server=>role_user
                                             text = |Happily greet { argument-value } and wish them a great day| ) ##NO_TEXT.
        ENDIF.
      WHEN `joke`.
        READ TABLE arguments INTO argument WITH KEY key = `topic`.
        IF sy-subrc <> 0.
          response-error-code    = zcl_mcp_jsonrpc=>error_codes-invalid_params.
          response-error-message = |Prompt { request->get_name( ) } requires parameter 'topic'| ##NO_TEXT.
        ELSE.
          response-result->set_description( `Joke prompt` ) ##NO_TEXT.

          response-result->add_text_message( role = zif_mcp_server=>role_user
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

    response-result->set_resources( VALUE #( ( uri         = `abap://classes/zcl_demo`
                                               name        = `zcl_demo.class`
                                               description = `Demo Class`
                                               mime_type   = `text/x-abap` ) ) ) ##NO_TEXT.
  ENDMETHOD.

  METHOD handle_list_res_tmpls.
    " In this demo instance we only have one templated resource, therefore
    " we do not consider cursor and max_list_results.

    response-result->set_resource_templates( VALUE #( ( uritemplate = `file://sales_receipt/{sales_order}`
                                                        name        = `Sales Receipts`
                                                        description = `Receipts for Sales Order`
                                                        mime_type   = `application/pdf` ) ) ) ##NO_TEXT.
  ENDMETHOD.

  METHOD handle_resources_read.
    " For now just one example supported.
    " No example for the dynamic resource for now.

    IF request->get_uri( ) = `abap://classes/zcl_demo`.
      DATA(text) = |CLASS zcl_demo DEFINITION PUBLIC FINAL CREATE PUBLIC.\n|
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
    TRY.
        DATA(output_schema_servertime) = NEW zcl_mcp_schema_builder( ).
        output_schema_servertime->add_string( name        = `server_date`
                                              description = `Current server date, format YYYY-MM-DD`
                                              required    = abap_true ) ##NO_TEXT.

        output_schema_servertime->add_string( name        = `server_time`
                                              description = `Current server time, format HH:MM:SS`
                                              required    = abap_true ) ##NO_TEXT.

        APPEND VALUE #( name          = `get_server_time`
                        title         = `Get Server Time`
                        description   = `Get the current server date and time in internal format.`
                        output_schema = output_schema_servertime->to_json( )  ) TO tools ##NO_TEXT.
      CATCH zcx_mcp_ajson_error INTO DATA(schema_error).
        response-error-code    = zcl_mcp_jsonrpc=>error_codes-internal_error.
        response-error-message = schema_error->get_text( ).
        RETURN.
    ENDTRY.

    " Demo tool with input parameters
    " Note: The input schema is defined in the get_flight_conn_schema method
    TRY.
        DATA(output_schema_flight_conn) = NEW zcl_mcp_schema_builder( ).
        output_schema_flight_conn->begin_array( description = `Flights Table`
                                                name        = `Flights` ) ##NO_TEXT.
        output_schema_flight_conn->add_string( name        = `carrid`
                                               description = `Airline Code`
                                               required    = abap_true ) ##NO_TEXT.
        output_schema_flight_conn->add_string( name        = `connid`
                                               description = `Flight Connection ID`
                                               required    = abap_true ) ##NO_TEXT.
        output_schema_flight_conn->add_string( name        = `fldate`
                                               description = `Flight Date (YYYY-MM-DD)`
                                               required    = abap_true ) ##NO_TEXT.
        output_schema_flight_conn->add_number( name        = `price`
                                               description = `Flight Price`
                                               required    = abap_true ) ##NO_TEXT.
        output_schema_flight_conn->add_string( name        = `currency`
                                               description = `Currency Code`
                                               required    = abap_true ) ##NO_TEXT.
        output_schema_flight_conn->add_string( name        = `planetype`
                                               description = `Type of Plane`
                                               required    = abap_true ) ##NO_TEXT.
        output_schema_flight_conn->end_array( ) ##NO_TEXT.

        APPEND VALUE #( name          = `get_flight_conn_details`
                        description   = `Get details of one specific flight connection`
                        title         = `Get Flight Connection Details`
                        input_schema  = get_flight_conn_schema( )->to_json( )
                        output_schema = output_schema_flight_conn->to_json( ) )
               TO tools ##NO_TEXT.
      CATCH zcx_mcp_ajson_error INTO DATA(error).
        response-error-code    = zcl_mcp_jsonrpc=>error_codes-internal_error.
        response-error-message = error->get_text( ).
    ENDTRY.

    response-result->set_tools( tools ).
  ENDMETHOD.

  METHOD handle_call_tool.
    TRY.
        CASE request->get_name( ).
          WHEN `get_server_time`.
            get_server_time( CHANGING response = response ).
          WHEN `get_flight_conn_details`.
            get_flight_conn_details( EXPORTING request  = request
                                     CHANGING  response = response ).
          WHEN OTHERS.
            response-error-code    = zcl_mcp_jsonrpc=>error_codes-invalid_params.
            response-error-message = |Tool { request->get_name( ) } not found.| ##NO_TEXT.
        ENDCASE.
      CATCH zcx_mcp_ajson_error INTO DATA(error).
        response-error-code    = zcl_mcp_jsonrpc=>error_codes-internal_error.
        response-error-message = error->get_text( ).
    ENDTRY.
  ENDMETHOD.

  METHOD get_flight_conn_details.
    DATA(input) = request->get_arguments( ).

    " Validate input parameter via schema validator class
    TRY.
        DATA(schema) = get_flight_conn_schema( ).
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

    DATA(airline_code) = input->get_string( `airline_code` ).
    DATA(flight_number) = input->get_integer( `flight_number` ).

    DATA connid TYPE s_conn_id.
    connid = flight_number.

    " Select only the required fields
    SELECT carrid, connid, fldate, price, currency, planetype
      FROM sflight
      WHERE carrid = @airline_code AND connid = @connid
      ORDER BY fldate
      INTO TABLE @DATA(flights).

    IF sy-subrc <> 0.
      response-result->add_text_content( |No flights found for airline { airline_code } and connection { connid }| ) ##NO_TEXT.
      RETURN.
    ENDIF.

    " Create markdown table
    DATA(markdown) = |## Flight Connection Details\n\n|.

    " Add table headers
    markdown = |{ markdown }\| Airline \| Connection \| Flight Date \| Price \| Currency \| Plane Type \|\n| ##NO_TEXT.
    markdown = |{ markdown }\|---------\|------------\|-------------\|-------\|----------\|------------\|\n| ##NO_TEXT.

    " Add table rows
    LOOP AT flights ASSIGNING FIELD-SYMBOL(<flight>).
      markdown = markdown &&
        |\| { <flight>-carrid } \| { <flight>-connid } \| { <flight>-fldate DATE = USER } \| { <flight>-price } \| { <flight>-currency } \| { <flight>-planetype } \|\n|.
    ENDLOOP.

    " Handle no results case
    IF sy-subrc <> 0.
      markdown = |{ markdown }\| No flights found for airline { airline_code } and connection { connid } \|\n| ##NO_TEXT.
    ENDIF.

    " Add structed content based on the output schema. Do not add text content as we already have markdown above.
    DATA(structured_content) = zcl_mcp_ajson=>create_empty( ).
    structured_content->set( iv_path = `Flights`
                             iv_val  = flights ) ##NO_TEXT.
    response-result->set_structured_content( structured_content = structured_content
                                             add_text_content   = abap_false ).

    response-result->add_text_content( markdown ).
  ENDMETHOD.

  METHOD get_server_time.
    response-result->add_text_content( |Current Server Date: { sy-datum } Time: { sy-uzeit } in internal format.| ) ##NO_TEXT.
    DATA(structured_content) = zcl_mcp_ajson=>create_empty( ).
    structured_content->set( iv_path = `server_date`
                             iv_val  = sy-datum ).
    structured_content->set( iv_path = `server_time`
                             iv_val  = sy-uzeit ).
    " As we manually create an alternative text content above, we disable automatic text content generation
    response-result->set_structured_content( structured_content = structured_content
                                             add_text_content   = abap_false ).
  ENDMETHOD.

  METHOD get_flight_conn_schema.
    DATA(schema) = NEW zcl_mcp_schema_builder( ).
    schema->add_string( name        = `airline_code`
                        description = `Airline Code`
                        required    = abap_true
                        enum        = VALUE #( ( `AA` ) ( `AB` ) ( `AC` ) ) ) ##NO_TEXT.
    schema->add_integer( name        = `flight_number`
                         description = `Flight Number`
                         minimum     = 0
                         maximum     = 9999
                         required    = abap_true ) ##NO_TEXT.
    result = schema.
  ENDMETHOD.

  METHOD get_session_mode.
    result = zcl_mcp_session=>session_mode_stateless.
  ENDMETHOD.

ENDCLASS.
