"! <p class="shorttext synchronized">MCP V2 workflow demo server</p>
"! Demonstrates advanced stateless MCP draft v2 workflows:
"! direct MRTR input_required, elicitation, protected requestState, task
"! creation, tasks/get polling, tasks/update input, and tasks/cancel through
"! the inherited base implementation.
CLASS zcl_mcp_demo_server_v2_wf DEFINITION
  PUBLIC
  INHERITING FROM zcl_mcp_server_base_v2 FINAL
  CREATE PUBLIC.

  PROTECTED SECTION.
    METHODS get_implementation       REDEFINITION.
    METHODS get_capabilities         REDEFINITION.
    METHODS get_instructions         REDEFINITION.
    METHODS handle_tools_list        REDEFINITION.
    METHODS handle_tool_input_schema REDEFINITION.
    METHODS handle_tools_call        REDEFINITION.
    METHODS handle_tasks_update      REDEFINITION.

  PRIVATE SECTION.
    CONSTANTS c_tool_approval  TYPE string VALUE `approval_required`.
    CONSTANTS c_tool_task      TYPE string VALUE `start_input_task`.
    CONSTANTS c_input_confirm  TYPE string VALUE `confirm`.
    CONSTANTS c_state_approval TYPE string VALUE `demo-approval`.
    CONSTANTS c_state_task     TYPE string VALUE `demo-task-input`.

    "! <p class="shorttext synchronized">Convert exception into v2 response</p>
    METHODS json_error
      IMPORTING !error          TYPE REF TO cx_root
      RETURNING VALUE(response) TYPE zif_mcp_server_v2=>v2_response.

    "! <p class="shorttext synchronized">Check task extension capability</p>
    METHODS client_supports_tasks
      RETURNING VALUE(result) TYPE abap_bool.

    "! <p class="shorttext synchronized">Return missing task capability error</p>
    METHODS missing_tasks_capability
      RETURNING VALUE(response) TYPE zif_mcp_server_v2=>v2_response.

    "! <p class="shorttext synchronized">Build tool input schema</p>
    METHODS build_tool_schema
      IMPORTING tool_name     TYPE string
      RETURNING VALUE(result) TYPE REF TO zif_mcp_ajson
      RAISING   zcx_mcp_ajson_error.

    "! <p class="shorttext synchronized">Build tools/list result</p>
    METHODS build_tools_result
      RETURNING VALUE(result) TYPE REF TO zif_mcp_ajson
      RAISING   zcx_mcp_ajson_error.

    "! <p class="shorttext synchronized">Build text tool result</p>
    METHODS build_text_result
      IMPORTING !text         TYPE string
      RETURNING VALUE(result) TYPE REF TO zif_mcp_ajson
      RAISING   zcx_mcp_ajson_error.

    "! <p class="shorttext synchronized">Build elicitation input_required result</p>
    METHODS build_input_required
      IMPORTING state_data    TYPE string
                message       TYPE string
      RETURNING VALUE(result) TYPE REF TO zif_mcp_ajson
      RAISING   zcx_mcp_ajson_error
                zcx_mcp_server.

    "! <p class="shorttext synchronized">Build task creation result</p>
    METHODS build_task_result
      IMPORTING task_id       TYPE string
      RETURNING VALUE(result) TYPE REF TO zif_mcp_ajson
      RAISING   zcx_mcp_ajson_error.

    "! <p class="shorttext synchronized">Create completed task payload</p>
    METHODS build_completed_payload
      IMPORTING input_responses TYPE REF TO zif_mcp_ajson
      RETURNING VALUE(result)   TYPE REF TO zif_mcp_ajson
      RAISING   zcx_mcp_ajson_error
                zcx_mcp_server.

    "! <p class="shorttext synchronized">Build acknowledgement result</p>
    METHODS build_ack
      RETURNING VALUE(result) TYPE REF TO zif_mcp_ajson
      RAISING   zcx_mcp_ajson_error.
ENDCLASS.


CLASS zcl_mcp_demo_server_v2_wf IMPLEMENTATION.
  METHOD get_implementation.
    result-name        = `ABAP MCP V2 Workflow Demo`.
    result-version     = `1.0.0`.
    result-title       = `ABAP MCP V2 Workflow Demo`.
    result-description = `Advanced stateless MCP draft v2 demo for MRTR, elicitation, requestState, and tasks.`.
    result-website_url = `https://github.com/b-tocs/abap_mcp`.
  ENDMETHOD.

  METHOD get_capabilities.
    result-tools = abap_true.
    result-tasks = abap_true.
  ENDMETHOD.

  METHOD get_instructions.
    result = `Use this server to test MCP v2 input_required and task workflows. Legacy clients can list tools, but cannot complete these workflows.`.
  ENDMETHOD.

  METHOD handle_tools_list.
    TRY.
        response-result = build_tools_result( ).
      CATCH zcx_mcp_ajson_error INTO DATA(error).
        response = json_error( error ).
    ENDTRY.
  ENDMETHOD.

  METHOD handle_tool_input_schema.
    result = build_tool_schema( tool_name ).
  ENDMETHOD.

  METHOD handle_tools_call.
    TRY.
        CASE request->get_name( ).
          WHEN c_tool_approval.
            IF request->is_retry( ) = abap_true.
              DATA(state) = validate_request_state( request->get_request_state( ) ).

              IF state-data <> c_state_approval.
                response-error-code    = zcl_mcp_jsonrpc=>error_codes-invalid_params.
                response-error-message = `Unexpected requestState payload.`.
                RETURN.
              ENDIF.

              DATA(input_responses) = request->get_input_responses( ).
              DATA(elicitation) = NEW zcl_mcp_elicit_result( input_responses->slice( |/{ c_input_confirm }| ) ).

              IF elicitation->is_accept( ) = abap_true.
                DATA(approved_text) = COND string(
                  WHEN elicitation->get_boolean( `approved` ) = abap_true
                  THEN `approved`
                  ELSE `not approved` ).

                response-result = build_text_result(
                  |Approval workflow completed: { approved_text }. Comment: { elicitation->get_string( `comment` ) }| ).
              ELSE.
                response-result = build_text_result(
                  |Approval workflow ended with action { elicitation->get_action( ) }.| ).
              ENDIF.

              RETURN.
            ENDIF.

            response-result = build_input_required(
              state_data = c_state_approval
              message    = `Approve the direct MRTR demo action.` ).

          WHEN c_tool_task.
            IF client_supports_tasks( ) = abap_false.
              response = missing_tasks_capability( ).
              RETURN.
            ENDIF.

            DATA(tasks) = get_tasks( ).
            DATA(task_id) = tasks->create_task( tool_name     = c_tool_task
                                                ttl           = 60000
                                                poll_interval = 1000 ).

            DATA(pending_input) = build_input_required(
              state_data = c_state_task
              message    = `Approve continuation of the persisted task demo.` ).

            zcl_mcp_tasks=>request_input( task_id        = task_id
                                          input_required = pending_input ).

            response-result = build_task_result( CONV #( task_id ) ).

          WHEN OTHERS.
            response = method_not_found( request->get_name( ) ).
        ENDCASE.

      CATCH zcx_mcp_ajson_error INTO DATA(json_error).
        response = json_error( json_error ).

      CATCH zcx_mcp_server INTO DATA(mcp_error).
        response-error-code    = zcl_mcp_jsonrpc=>error_codes-invalid_params.
        response-error-message = mcp_error->get_text( ).
    ENDTRY.
  ENDMETHOD.

  METHOD handle_tasks_update.
    TRY.
        DATA(task_id) = request->get_task_id( ).

        DATA(state) = validate_request_state(
          request_state = request->get_request_state( )
          method        = `tools/call` ).

        IF state-data <> c_state_task.
          response-error-code    = zcl_mcp_jsonrpc=>error_codes-invalid_params.
          response-error-message = `Unexpected task requestState payload.`.
          RETURN.
        ENDIF.

        zcl_mcp_tasks=>consume_update(
          task_id         = CONV #( task_id )
          input_responses = request->get_input_responses( )
          request_state   = request->get_request_state( ) ).

        zcl_mcp_tasks=>set_payload(
          task_id = CONV #( task_id )
          payload = build_completed_payload( request->get_input_responses( ) ) ).

        zcl_mcp_tasks=>update_status(
          task_id = CONV #( task_id )
          status  = zcl_mcp_tasks=>status_completed
          message = `Task completed from tasks/update input.` ).

        response-result = build_ack( ).

      CATCH zcx_mcp_ajson_error INTO DATA(json_error).
        response = json_error( json_error ).

      CATCH zcx_mcp_server INTO DATA(mcp_error).
        response-error-code    = zcl_mcp_jsonrpc=>error_codes-invalid_params.
        response-error-message = mcp_error->get_text( ).
    ENDTRY.
  ENDMETHOD.

  METHOD json_error.
    response-error-code    = zcl_mcp_jsonrpc=>error_codes-internal_error.
    response-error-message = error->get_text( ).
  ENDMETHOD.

  METHOD client_supports_tasks.
    DATA(context) = me->zif_mcp_server_v2~get_v2_context( ).

    result = xsdbool(
         context-extensions IS BOUND
     AND context-extensions->exists( `/io.modelcontextprotocol~1tasks` ) = abap_true ).
  ENDMETHOD.

  METHOD missing_tasks_capability.
    response-error-code    = zcl_mcp_jsonrpc=>error_codes-missing_client_capability.
    response-error-message = `Client did not declare io.modelcontextprotocol/tasks.`.

    TRY.
        response-error-data = zcl_mcp_ajson=>create_empty( ).
        response-error-data->touch_array( `/requiredCapabilities` ).
        response-error-data->set_string(
          iv_path = `/requiredCapabilities/1`
          iv_val  = `io.modelcontextprotocol/tasks` ).
      CATCH zcx_mcp_ajson_error.
        CLEAR response-error-data.
    ENDTRY.
  ENDMETHOD.

  METHOD build_tool_schema.
    DATA builder TYPE REF TO zcl_mcp_schema_builder.

    builder = NEW zcl_mcp_schema_builder( ).

    CASE tool_name.
      WHEN c_tool_approval.
        builder->add_string(
          name         = `reason`
          description  = `Reason shown in the demo workflow.`
          required     = abap_false
          x_mcp_header = `Reason` ).

      WHEN c_tool_task.
        " Empty object schema.

      WHEN OTHERS.
        " Empty object schema.
    ENDCASE.

    result = builder->to_json( ).
  ENDMETHOD.

  METHOD build_tools_result.
    DATA list_tools TYPE REF TO zcl_mcp_resp_list_tools.
    DATA tools      TYPE zcl_mcp_resp_list_tools=>tools.
    DATA tool       TYPE zcl_mcp_resp_list_tools=>tool.

    list_tools = NEW zcl_mcp_resp_list_tools( ).

    CLEAR tool.
    tool-name         = c_tool_approval.
    tool-title        = `Approval Required`.
    tool-description  = `Returns input_required and resumes after an elicitation/create response.`.
    tool-input_schema = build_tool_schema( c_tool_approval ).
    tool-annotations-readonlyhint = abap_true.
    APPEND tool TO tools.

    CLEAR tool.
    tool-name         = c_tool_task.
    tool-title        = `Start Input Task`.
    tool-description  = `Creates a persisted task that waits for client input and completes through tasks/update.`.
    tool-input_schema = build_tool_schema( c_tool_task ).
    tool-execution-task_support = zcl_mcp_resp_list_tools=>task_support-required.
    APPEND tool TO tools.

    list_tools->set_tools( tools ).

    result = list_tools->zif_mcp_internal~generate_json( ).
    result->set_string( iv_path = `/resultType`
                        iv_val  = zif_mcp_constants=>result_types-complete ).
    result->set_integer( iv_path = `/ttlMs`
                         iv_val  = 0 ).
    result->set_string( iv_path = `/cacheScope`
                        iv_val  = zif_mcp_constants=>cache_scopes-private ).
  ENDMETHOD.

  METHOD build_text_result.
    DATA tool_result TYPE REF TO zcl_mcp_resp_v2_tool.

    tool_result = NEW zcl_mcp_resp_v2_tool( ).
    tool_result->set_complete( ).
    tool_result->set_error( abap_false ).
    tool_result->add_text_content( text ).

    result = tool_result->generate_json( ).
  ENDMETHOD.

  METHOD build_input_required.
    DATA input_required TYPE REF TO zcl_mcp_resp_v2_input_req.
    DATA elicitation    TYPE REF TO zcl_mcp_input_elicitation.
    DATA builder        TYPE REF TO zcl_mcp_schema_builder.

    builder = NEW zcl_mcp_schema_builder( ).
    builder->add_boolean(
      name        = `approved`
      description = `Whether the demo workflow may continue.`
      required    = abap_true ).
    builder->add_string(
      name        = `comment`
      description = `Optional user comment.`
      required    = abap_false ).

    elicitation = NEW zcl_mcp_input_elicitation( ).
    elicitation->set_form(
      message          = message
      requested_schema = builder->to_json( ) ).

    input_required = NEW zcl_mcp_resp_v2_input_req( ).
    input_required->set_request_state(
      create_request_state( data        = state_data
                            ttl_seconds = 300 ) ).
    input_required->add_input_request(
      request_key = c_input_confirm
      method      = elicitation->get_method( )
      params      = elicitation->get_params( ) ).

    result = input_required->zif_mcp_modern_result~generate_json( ).
  ENDMETHOD.

  METHOD build_task_result.
    DATA task_result TYPE REF TO zcl_mcp_resp_v2_task.

    task_result = NEW zcl_mcp_resp_v2_task( ).
    task_result->set_task(
      task_id          = task_id
      status           = zif_mcp_types=>task_states-input_required
      status_message   = `Task is waiting for client input.`
      ttl_ms           = 60000
      poll_interval_ms = 1000 ).

    result = task_result->zif_mcp_modern_result~generate_json( ).
  ENDMETHOD.

  METHOD build_completed_payload.
    DATA elicitation TYPE REF TO zcl_mcp_elicit_result.
    DATA text        TYPE string.

    elicitation = NEW zcl_mcp_elicit_result(
      input_responses->slice( |/{ c_input_confirm }| ) ).

    IF elicitation->is_accept( ) = abap_true.
      text = |Persisted task completed. Approved={ elicitation->get_boolean( `approved` ) }, comment={ elicitation->get_string( `comment` ) }.|.
    ELSE.
      text = |Persisted task completed with elicitation action { elicitation->get_action( ) }.|.
    ENDIF.

    result = build_text_result( text ).
  ENDMETHOD.

  METHOD build_ack.
    DATA ack TYPE REF TO zcl_mcp_resp_v2_ack.

    ack = NEW zcl_mcp_resp_v2_ack( ).
    ack->set_complete( ).

    result = ack->generate_json( ).
  ENDMETHOD.
ENDCLASS.
