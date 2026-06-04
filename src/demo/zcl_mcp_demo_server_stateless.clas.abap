"! <p class="shorttext synchronized" lang="en">Demo MCP Server</p>
CLASS zcl_mcp_demo_server_stateless DEFINITION
  PUBLIC
  INHERITING FROM zcl_mcp_server_base
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
  PROTECTED SECTION.
    METHODS handle_initialize           REDEFINITION.
    METHODS handle_list_prompts         REDEFINITION.
    METHODS handle_get_prompt           REDEFINITION.
    METHODS handle_list_resources       REDEFINITION.
    METHODS handle_list_res_tmpls       REDEFINITION.
    METHODS handle_resources_read       REDEFINITION.
    METHODS handle_list_tools           REDEFINITION.
    METHODS handle_call_tool            REDEFINITION.
    METHODS handle_completions_complete REDEFINITION.
    METHODS get_session_mode            REDEFINITION.

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

    "! <p class="shorttext synchronized">Starts an async flight report task</p>
    "! Creates a task, fetches flight data for the airline, and completes
    "! the task immediately (demo: synchronous simulation of async work).
    "! @parameter request  | <p class="shorttext synchronized">Request with airline_code input</p>
    "! @parameter response | <p class="shorttext synchronized">Response containing the task ID</p>
    METHODS start_flight_report IMPORTING !request  TYPE REF TO zcl_mcp_req_call_tool
                                CHANGING  !response TYPE zif_mcp_server=>call_tool_response
                                RAISING   zcx_mcp_ajson_error.
ENDCLASS.



CLASS ZCL_MCP_DEMO_SERVER_STATELESS IMPLEMENTATION.


  METHOD get_flight_conn_details.
    DATA input TYPE REF TO zif_mcp_ajson.
        DATA schema TYPE REF TO zcl_mcp_schema_builder.
        DATA validator TYPE REF TO zcl_mcp_schema_validator.
        DATA validation_result TYPE abap_bool.
        DATA error TYPE REF TO zcx_mcp_ajson_error.
    DATA airline_code TYPE string.
    DATA flight_number TYPE i.
    DATA connid TYPE s_conn_id.
TYPES BEGIN OF temp12.
TYPES carrid TYPE sflight-carrid.
TYPES connid TYPE sflight-connid.
TYPES fldate TYPE sflight-fldate.
TYPES price TYPE sflight-price.
TYPES currency TYPE sflight-currency.
TYPES planetype TYPE sflight-planetype.
TYPES END OF temp12.
    DATA flights TYPE STANDARD TABLE OF temp12 WITH DEFAULT KEY.
    DATA markdown TYPE string.
    FIELD-SYMBOLS <flight> LIKE LINE OF flights.
    DATA structured_content TYPE REF TO zcl_mcp_ajson.
    input = request->get_arguments( ).

    " Validate input parameter via schema validator class
    TRY.

        schema = get_flight_conn_schema( ).

        CREATE OBJECT validator TYPE zcl_mcp_schema_validator EXPORTING SCHEMA = schema->to_json( ).

        validation_result = validator->validate( input ).
        IF validation_result = abap_false.
          response-error-code    = zcl_mcp_jsonrpc=>error_codes-invalid_params.
          response-error-message = concat_lines_of( validator->get_errors( ) ).
          RETURN.
        ENDIF.

      CATCH zcx_mcp_ajson_error INTO error.
        response-error-code    = zcl_mcp_jsonrpc=>error_codes-internal_error.
        response-error-message = error->get_text( ).
        RETURN.
    ENDTRY.


    airline_code = input->get_string( `airline_code` ).

    flight_number = input->get_integer( `flight_number` ).


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

    " Add structed content based on the output schema. Do not add text content as we already have markdown above.

    structured_content = zcl_mcp_ajson=>create_empty( ).
    structured_content->set( iv_path = `Flights`
                             iv_val  = flights ) ##NO_TEXT.
    response-result->set_structured_content( structured_content = structured_content
                                             add_text_content   = abap_false ).

    response-result->add_text_content( markdown ).
  ENDMETHOD.


  METHOD get_flight_conn_schema.
    DATA schema TYPE REF TO zcl_mcp_schema_builder.
    DATA temp13 TYPE string_table.
    CREATE OBJECT schema TYPE zcl_mcp_schema_builder.

    CLEAR temp13.
    INSERT `AA` INTO TABLE temp13.
    INSERT `AB` INTO TABLE temp13.
    INSERT `AC` INTO TABLE temp13.
    schema->add_string( name        = `airline_code`
                        description = `Airline Code`
                        required    = abap_true
                        enum        = temp13 ) ##NO_TEXT.
    schema->add_integer( name        = `flight_number`
                         description = `Flight Number`
                         minimum     = 0
                         maximum     = 9999
                         required    = abap_true ) ##NO_TEXT.
    result = schema.
  ENDMETHOD.


  METHOD get_server_time.
    DATA structured_content TYPE REF TO zcl_mcp_ajson.
    response-result->add_text_content( |Current Server Date: { sy-datum } Time: { sy-uzeit } in internal format.| ) ##NO_TEXT.

    structured_content = zcl_mcp_ajson=>create_empty( ).
    structured_content->set( iv_path = `server_date`
                             iv_val  = sy-datum ).
    structured_content->set( iv_path = `server_time`
                             iv_val  = sy-uzeit ).
    " As we manually create an alternative text content above, we disable automatic text content generation
    response-result->set_structured_content( structured_content = structured_content
                                             add_text_content   = abap_false ).
  ENDMETHOD.


  METHOD get_session_mode.
    result = zcl_mcp_session=>session_mode_stateless.
  ENDMETHOD.


  METHOD handle_call_tool.
        DATA error TYPE REF TO zcx_mcp_ajson_error.
    TRY.
        CASE request->get_name( ).
          WHEN `get_server_time`.
            get_server_time( CHANGING response = response ).
          WHEN `get_flight_conn_details`.
            get_flight_conn_details( EXPORTING request  = request
                                     CHANGING  response = response ).
          WHEN `start_flight_report`.
            start_flight_report( EXPORTING request  = request
                                 CHANGING  response = response ).
          WHEN OTHERS.
            response-error-code    = zcl_mcp_jsonrpc=>error_codes-invalid_params.
            response-error-message = |Tool { request->get_name( ) } not found.| ##NO_TEXT.
        ENDCASE.

      CATCH zcx_mcp_ajson_error INTO error.
        response-error-code    = zcl_mcp_jsonrpc=>error_codes-internal_error.
        response-error-message = error->get_text( ).
    ENDTRY.
  ENDMETHOD.


  METHOD handle_completions_complete.
    DATA arg_name TYPE string.
    DATA arg_value TYPE string.
              DATA temp18 TYPE zcl_mcp_resp_complete=>completion_values.
              DATA names LIKE temp18.
              DATA name LIKE LINE OF names.
              DATA like_pattern TYPE string.
TYPES BEGIN OF temp20.
TYPES carrname TYPE scarr-carrname.
TYPES END OF temp20.
              DATA carriers TYPE STANDARD TABLE OF temp20 WITH DEFAULT KEY.
              FIELD-SYMBOLS <carrier> LIKE LINE OF carriers.
                DATA temp21 TYPE string.
          DATA temp22 TYPE zcl_mcp_resp_complete=>completion_values.
          DATA orders LIKE temp22.
          DATA order_no LIKE LINE OF orders.
    arg_name  = request->get_argument_name( ).

    arg_value = request->get_argument_value( ).

    CASE request->get_ref_type( ).

      WHEN zcl_mcp_req_complete=>ref_type-prompt.
        CASE request->get_ref_name( ).

          WHEN `greet`.
            IF arg_name = `name`.

              CLEAR temp18.
              INSERT `Alice` INTO TABLE temp18.
              INSERT `Bob` INTO TABLE temp18.
              INSERT `Charlie` INTO TABLE temp18.
              INSERT `Dave` INTO TABLE temp18.
              INSERT `Eve` INTO TABLE temp18.

              names = temp18.

              LOOP AT names INTO name.
                IF NOT ( arg_value IS INITIAL OR name CP |{ arg_value }*| ).
                  CONTINUE.
                ENDIF.
                response-result->add_value( name ).
              ENDLOOP.
            ENDIF.

          WHEN `joke`.
            IF arg_name = `topic`.
              " Pull real airline names from the demo flight dataset as topic suggestions

              like_pattern = |{ arg_value }%|.


              SELECT carrname FROM scarr INTO TABLE carriers
                WHERE carrname LIKE like_pattern
                ORDER BY carrid.

              LOOP AT carriers ASSIGNING <carrier>.

                temp21 = <carrier>-carrname.
                response-result->add_value( temp21 ).
              ENDLOOP.
            ENDIF.

        ENDCASE.

      WHEN zcl_mcp_req_complete=>ref_type-resource.
        IF     request->get_ref_uri( ) = `file://sales_receipt/{sales_order}`
           AND arg_name                = `sales_order`.

          CLEAR temp22.
          INSERT `0000000010` INTO TABLE temp22.
          INSERT `0000000020` INTO TABLE temp22.
          INSERT `0000000030` INTO TABLE temp22.

          orders = temp22.

          LOOP AT orders INTO order_no.
            IF NOT ( arg_value IS INITIAL OR order_no CP |{ arg_value }*| ).
              CONTINUE.
            ENDIF.
            response-result->add_value( order_no ).
          ENDLOOP.
        ENDIF.

    ENDCASE.
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

          response-result->add_text_message( role = zif_mcp_types=>role_user
                                             text = |Happily greet { argument-value } and wish them a great day| ) ##NO_TEXT.
        ENDIF.
      WHEN `joke`.
        READ TABLE arguments INTO argument WITH KEY key = `topic`.
        IF sy-subrc <> 0.
          response-error-code    = zcl_mcp_jsonrpc=>error_codes-invalid_params.
          response-error-message = |Prompt { request->get_name( ) } requires parameter 'topic'| ##NO_TEXT.
        ELSE.
          response-result->set_description( `Joke prompt` ) ##NO_TEXT.

          response-result->add_text_message( role = zif_mcp_types=>role_user
                                             text = |Tell a great joke about the topic { argument-value }| ) ##NO_TEXT.
        ENDIF.
      WHEN OTHERS.
        response-error-code    = zcl_mcp_jsonrpc=>error_codes-invalid_params.
        response-error-message = |Prompt { request->get_name( ) } unknown.| ##NO_TEXT.
    ENDCASE.
  ENDMETHOD.


  METHOD handle_initialize.
    DATA temp1 TYPE zcl_mcp_resp_initialize=>capabilities.
    DATA temp2 TYPE zcl_mcp_resp_initialize=>implementation.
    CLEAR temp1.
    CLEAR temp1-prompts.
    temp1-prompts-enabled = abap_true.
    CLEAR temp1-resources.
    temp1-resources-enabled = abap_true.
    CLEAR temp1-tools.
    temp1-tools-enabled = abap_true.
    CLEAR temp1-tasks.
    temp1-tasks-list = abap_true.
    temp1-tasks-cancel = abap_true.
    temp1-tasks-tools_call = abap_true.
    temp1-completions = abap_true.
    response-result->set_capabilities( temp1 ).


    CLEAR temp2.
    temp2-name = `Demo MCP Server`.
    temp2-version = `1.1.0`.
    response-result->set_implementation( temp2 ) ##NO_TEXT.
    response-result->set_instructions(
        `Use the features provided by this server only if explicitly requested. If not sure ask the user!` ) ##NO_TEXT.
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
    temp4-title = `greet someone`.

    CLEAR temp1.

    temp2-name = `name`.
    temp2-description = `Name of the person to greet`.
    temp2-required = abap_true.
    temp2-title = `Name to greet`.
    INSERT temp2 INTO TABLE temp1.
    temp4-arguments = temp1.
    INSERT temp4 INTO TABLE temp3.
    temp4-name = `joke`.
    temp4-description = `Asks the LLM to tell a joke about a specific topic`.
    temp4-title = `Tell a joke`.

    CLEAR temp5.

    temp6-name = `topic`.
    temp6-description = `Topic to joke about`.
    temp6-required = abap_true.
    temp6-title = `Joke topic`.
    INSERT temp6 INTO TABLE temp5.
    temp4-arguments = temp5.
    INSERT temp4 INTO TABLE temp3.
    response-result->set_prompts(
        temp3 ) ##NO_TEXT.
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
    response-result->set_resources( temp5 ) ##NO_TEXT.
  ENDMETHOD.


  METHOD handle_list_res_tmpls.
    " In this demo instance we only have one templated resource, therefore
    " we do not consider cursor and max_list_results.

    DATA temp7 TYPE zcl_mcp_resp_list_res_tmpl=>resource_templates.
    DATA temp8 LIKE LINE OF temp7.
    CLEAR temp7.

    temp8-uritemplate = `file://sales_receipt/{sales_order}`.
    temp8-name = `Sales Receipts`.
    temp8-description = `Receipts for Sales Order`.
    temp8-mime_type = `application/pdf`.
    INSERT temp8 INTO TABLE temp7.
    response-result->set_resource_templates( temp7 ) ##NO_TEXT.
  ENDMETHOD.


  METHOD handle_list_tools.
    DATA tools TYPE zcl_mcp_resp_list_tools=>tools.
        DATA output_schema_servertime TYPE REF TO zcl_mcp_schema_builder.
        DATA temp9 TYPE zcl_mcp_resp_list_tools=>tool.
        DATA schema_error TYPE REF TO zcx_mcp_ajson_error.
        DATA output_schema_flight_conn TYPE REF TO zcl_mcp_schema_builder.
        DATA temp10 TYPE zcl_mcp_resp_list_tools=>tool.
        DATA error TYPE REF TO zcx_mcp_ajson_error.
        DATA output_schema_report TYPE REF TO zcl_mcp_schema_builder.
        DATA temp11 TYPE zcl_mcp_resp_list_tools=>tool.
        DATA report_error TYPE REF TO zcx_mcp_ajson_error.

    " Demo Tool without any input parameter
    TRY.

        CREATE OBJECT output_schema_servertime TYPE zcl_mcp_schema_builder.
        output_schema_servertime->add_string( name        = `server_date`
                                              description = `Current server date, format YYYY-MM-DD`
                                              required    = abap_true ) ##NO_TEXT.

        output_schema_servertime->add_string( name        = `server_time`
                                              description = `Current server time, format HH:MM:SS`
                                              required    = abap_true ) ##NO_TEXT.


        CLEAR temp9.
        temp9-name = `get_server_time`.
        temp9-title = `Get Server Time`.
        temp9-description = `Get the current server date and time in internal format.`.
        temp9-output_schema = output_schema_servertime->to_json( ).
        APPEND temp9 TO tools ##NO_TEXT.

      CATCH zcx_mcp_ajson_error INTO schema_error.
        response-error-code    = zcl_mcp_jsonrpc=>error_codes-internal_error.
        response-error-message = schema_error->get_text( ).
        RETURN.
    ENDTRY.

    " Demo tool with input parameters
    " Note: The input schema is defined in the get_flight_conn_schema method
    TRY.

        CREATE OBJECT output_schema_flight_conn TYPE zcl_mcp_schema_builder.
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
        output_schema_flight_conn->end_array( ).


        CLEAR temp10.
        temp10-name = `get_flight_conn_details`.
        temp10-description = `Get details of one specific flight connection`.
        temp10-title = `Get Flight Connection Details`.
        temp10-input_schema = get_flight_conn_schema( )->to_json( ).
        temp10-output_schema = output_schema_flight_conn->to_json( ).
        APPEND temp10
               TO tools ##NO_TEXT.

      CATCH zcx_mcp_ajson_error INTO error.
        response-error-code    = zcl_mcp_jsonrpc=>error_codes-internal_error.
        response-error-message = error->get_text( ).
    ENDTRY.

    " Async task demo tool
    TRY.

        CREATE OBJECT output_schema_report TYPE zcl_mcp_schema_builder.
        output_schema_report->add_string( name        = `airline`
                                          description = `Airline Code`
                                          required    = abap_true ) ##NO_TEXT.
        output_schema_report->add_integer( name        = `total_flights`
                                           description = `Total number of flights found`
                                           required    = abap_true ) ##NO_TEXT.
        output_schema_report->begin_array( name        = `flights`
                                           description = `Flight records` ) ##NO_TEXT.
        output_schema_report->add_string( name        = `carrid`
                                          description = `Airline Code`
                                          required    = abap_true ) ##NO_TEXT.
        output_schema_report->add_string( name        = `connid`
                                          description = `Flight Connection ID`
                                          required    = abap_true ) ##NO_TEXT.
        output_schema_report->add_string( name        = `fldate`
                                          description = `Flight Date`
                                          required    = abap_true ) ##NO_TEXT.
        output_schema_report->add_number( name        = `price`
                                          description = `Flight Price`
                                          required    = abap_true ) ##NO_TEXT.
        output_schema_report->add_string( name        = `currency`
                                          description = `Currency Code`
                                          required    = abap_true ) ##NO_TEXT.
        output_schema_report->add_string( name        = `planetype`
                                          description = `Type of Plane`
                                          required    = abap_true ) ##NO_TEXT.
        output_schema_report->end_array( ).


        CLEAR temp11.
        temp11-name = `start_flight_report`.
        temp11-title = `Start Flight Report`.
        temp11-description = |Starts an async task that compiles all flight data for an airline. | && |Returns a task_id immediately - use tasks/get to poll status | && |and tasks/result to retrieve the completed report.|.
        temp11-input_schema = get_flight_conn_schema( )->to_json( ).
        temp11-output_schema = output_schema_report->to_json( ).
        CLEAR temp11-execution.
        temp11-execution-task_support = zcl_mcp_resp_list_tools=>task_support-optional.
        APPEND temp11  " <-- add
               TO tools ##NO_TEXT.

      CATCH zcx_mcp_ajson_error INTO report_error.
        response-error-code    = zcl_mcp_jsonrpc=>error_codes-internal_error.
        response-error-message = report_error->get_text( ).
        RETURN.
    ENDTRY.

    response-result->set_tools( tools ).
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


  METHOD start_flight_report.
        DATA schema TYPE REF TO zcl_mcp_schema_builder.
        DATA validator TYPE REF TO zcl_mcp_schema_validator.
        DATA val_error TYPE REF TO zcx_mcp_ajson_error.
    DATA airline_code TYPE string.
          DATA temp15 TYPE i.
          DATA ttl LIKE temp15.
          DATA task_id TYPE sysuuid_c32.
TYPES BEGIN OF temp16.
TYPES carrid TYPE sflight-carrid.
TYPES connid TYPE sflight-connid.
TYPES fldate TYPE sflight-fldate.
TYPES price TYPE sflight-price.
TYPES currency TYPE sflight-currency.
TYPES planetype TYPE sflight-planetype.
TYPES END OF temp16.
          DATA flights TYPE STANDARD TABLE OF temp16 WITH DEFAULT KEY.
          DATA payload TYPE REF TO zcl_mcp_ajson.
          DATA task_result TYPE REF TO zcl_mcp_resp_task_payload.
          DATA task_error TYPE REF TO zcx_mcp_server.
          DATA task TYPE zif_mcp_types=>task.
          DATA get_error TYPE REF TO zcx_mcp_server.
TYPES BEGIN OF temp17.
TYPES carrid TYPE sflight-carrid.
TYPES connid TYPE sflight-connid.
TYPES fldate TYPE sflight-fldate.
TYPES price TYPE sflight-price.
TYPES currency TYPE sflight-currency.
TYPES planetype TYPE sflight-planetype.
TYPES END OF temp17.
      DATA sync_flights TYPE STANDARD TABLE OF temp17 WITH DEFAULT KEY.
          DATA sync_payload TYPE REF TO zcl_mcp_ajson.
          DATA json_error TYPE REF TO zcx_mcp_ajson_error.
    " Validate input
    TRY.

        schema    = get_flight_conn_schema( ).

        CREATE OBJECT validator TYPE zcl_mcp_schema_validator EXPORTING SCHEMA = schema->to_json( ).
        IF validator->validate( request->get_arguments( ) ) = abap_false.
          response-error-code    = zcl_mcp_jsonrpc=>error_codes-invalid_params.
          response-error-message = concat_lines_of( validator->get_errors( ) ).
          RETURN.
        ENDIF.

      CATCH zcx_mcp_ajson_error INTO val_error.
        response-error-code    = zcl_mcp_jsonrpc=>error_codes-internal_error.
        response-error-message = val_error->get_text( ).
        RETURN.
    ENDTRY.


    airline_code = request->get_arguments( )->get_string( `airline_code` ).

    IF request->has_task( ) IS NOT INITIAL.
      " Client requested async execution - create task and return immediately.
      " The actual work happens here synchronously for demo purposes,
      " but in production this is where you'd hand off to a batch job.
      TRY.

          IF request->get_task_ttl( ) > 0.
            temp15 = request->get_task_ttl( ).
          ELSE.
            temp15 = 300000.
          ENDIF.

          ttl = temp15.

          task_id = get_tasks( )->create_task( tool_name = request->get_name( )
                                                     ttl       = ttl ).

          zcl_mcp_tasks=>update_status( task_id = task_id
                                        status  = zcl_mcp_tasks=>status_working
                                        message = |Fetching flights for { airline_code }| ) ##NO_TEXT.



          SELECT carrid connid fldate price currency planetype
            FROM sflight INTO TABLE flights
            WHERE carrid = airline_code
            ORDER BY connid fldate
             ##SUBRC_OK.


          payload = zcl_mcp_ajson=>create_empty( ).
          payload->set_string( iv_path = `/airline`
                               iv_val  = airline_code ).
          payload->set_integer( iv_path = `/total_flights`
                                iv_val  = lines( flights ) ).
          payload->set( iv_path = `/flights`
                        iv_val  = flights ).


          CREATE OBJECT task_result TYPE zcl_mcp_resp_task_payload.
          task_result->set_structured_content( payload ).
          zcl_mcp_tasks=>complete( task_id = task_id
                                   result  = task_result ).


        CATCH zcx_mcp_server INTO task_error.
          response-error-code    = zcl_mcp_jsonrpc=>error_codes-internal_error.
          response-error-message = task_error->get_text( ).
          RETURN.
      ENDTRY.

      TRY.

          task = get_tasks( )->get( task_id ).
          response-result->set_task_result( task ).

        CATCH zcx_mcp_server INTO get_error.
          response-error-code    = zcl_mcp_jsonrpc=>error_codes-internal_error.
          response-error-message = get_error->get_text( ).
      ENDTRY.

    ELSE.
      " Synchronous execution - return result directly


      SELECT carrid connid fldate price currency planetype
        FROM sflight INTO TABLE sync_flights
        WHERE carrid = airline_code
        ORDER BY connid fldate
         ##SUBRC_OK.

      TRY.

          sync_payload = zcl_mcp_ajson=>create_empty( ).
          sync_payload->set_string( iv_path = `/airline`
                                    iv_val  = airline_code ).
          sync_payload->set_integer( iv_path = `/total_flights`
                                     iv_val  = lines( sync_flights ) ).
          sync_payload->set( iv_path = `/flights`
                             iv_val  = sync_flights ).

          response-result->set_structured_content( structured_content = sync_payload
                                                   add_text_content   = abap_false ).
          response-result->add_text_content( |Found { lines( sync_flights ) } flights for airline { airline_code }.| ) ##NO_TEXT.

        CATCH zcx_mcp_ajson_error INTO json_error.
          response-error-code    = zcl_mcp_jsonrpc=>error_codes-internal_error.
          response-error-message = json_error->get_text( ).
      ENDTRY.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
