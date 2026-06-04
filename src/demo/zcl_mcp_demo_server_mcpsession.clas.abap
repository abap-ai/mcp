"! <p class="shorttext synchronized" lang="en">Demo MCP Server using MCP Sessions</p>
CLASS zcl_mcp_demo_server_mcpsession DEFINITION
  PUBLIC
  INHERITING FROM zcl_mcp_server_base FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
  PROTECTED SECTION.
    METHODS handle_initialize  REDEFINITION.
    METHODS handle_list_tools  REDEFINITION.
    METHODS handle_call_tool   REDEFINITION.
    METHODS get_session_mode   REDEFINITION.
    METHODS handle_cancel_task REDEFINITION.

  PRIVATE SECTION.
    "! <p class="shorttext synchronized">Get session details for the current MCP session</p>
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

    "! <p class="shorttext synchronized">Start slow background computation</p>
    "! Creates a task, submits a background job, and returns immediately.
    "! The job computes the square of the input after a random 30-60s delay.
    "!
    "! @parameter request  | <p class="shorttext synchronized">Request containing the value to compute</p>
    "! @parameter response | <p class="shorttext synchronized">Response with task ID for polling</p>
    METHODS start_slow_computation
      IMPORTING !request  TYPE REF TO zcl_mcp_req_call_tool
      CHANGING  !response TYPE zif_mcp_server=>call_tool_response
      RAISING   zcx_mcp_ajson_error.

    "! <p class="shorttext synchronized">Build input schema for slow computation tool</p>
    "!
    "! @parameter result              | <p class="shorttext synchronized">Schema builder with value parameter</p>
    "! @raising   zcx_mcp_ajson_error | <p class="shorttext synchronized">Error building schema</p>
    METHODS get_computation_schema
      RETURNING VALUE(result) TYPE REF TO zcl_mcp_schema_builder
      RAISING   zcx_mcp_ajson_error.
ENDCLASS.



CLASS zcl_mcp_demo_server_mcpsession IMPLEMENTATION.
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
    response-result->set_capabilities( temp1 ).

    CLEAR temp2.
    temp2-name = `Demo MCP Server - using MCP session logic. Tools only.`.
    temp2-version = `1.1.0`.
    response-result->set_implementation( temp2 ) ##NO_TEXT.
    response-result->set_instructions(
        `Use the features provided by this server only if explicitly requested. If not sure ask the user!` ) ##NO_TEXT.
  ENDMETHOD.

  METHOD handle_list_tools.
    DATA tools TYPE zcl_mcp_resp_list_tools=>tools.

    DATA temp3 TYPE zcl_mcp_resp_list_tools=>tool.
        DATA temp4 TYPE zcl_mcp_resp_list_tools=>tool.
        DATA output_schema TYPE REF TO zcl_mcp_schema_builder.
        DATA temp5 TYPE zcl_mcp_resp_list_tools=>tool.
        DATA error TYPE REF TO zcx_mcp_ajson_error.
    CLEAR temp3.
    temp3-name = `get_session_details`.
    temp3-description = `Get details about the current session.`.
    APPEND temp3 TO tools ##NO_TEXT.

    TRY.

        CLEAR temp4.
        temp4-name = `increment_example`.
        temp4-description = `Every time increments the result by the given number. Demonstrates session logic.`.
        temp4-input_schema = get_increment_schema( )->to_json( ).
        APPEND temp4
               TO tools ##NO_TEXT.


        CREATE OBJECT output_schema TYPE zcl_mcp_schema_builder.
        output_schema->add_integer( name        = `input_value`
                                    description = `The value that was submitted`
                                    required    = abap_true ) ##NO_TEXT.
        output_schema->add_integer( name        = `computed_value`
                                    description = `Square of the input value`
                                    required    = abap_true ) ##NO_TEXT.
        output_schema->add_integer( name        = `wait_seconds`
                                    description = `Seconds the background job waited`
                                    required    = abap_true ) ##NO_TEXT.


        CLEAR temp5.
        temp5-name = `start_slow_computation`.
        temp5-title = `Start Slow Computation`.
        temp5-description = |Squares the input value in a background job after a random 30-60s delay. | && |Use tasks/get to poll status and tasks/result for the result.|.
        temp5-input_schema = get_computation_schema( )->to_json( ).
        temp5-output_schema = output_schema->to_json( ).
        CLEAR temp5-execution.
        temp5-execution-task_support = zcl_mcp_resp_list_tools=>task_support-optional.
        APPEND temp5
               TO tools ##NO_TEXT.

      CATCH zcx_mcp_ajson_error INTO error.
        response-error-code    = zcl_mcp_jsonrpc=>error_codes-internal_error.
        response-error-message = error->get_text( ).
        RETURN.
    ENDTRY.

    response-result->set_tools( tools ).
  ENDMETHOD.

  METHOD handle_call_tool.
        DATA error TYPE REF TO zcx_mcp_ajson_error.
    TRY.
        CASE request->get_name( ).
          WHEN `get_session_details`.
            get_session_details( CHANGING response = response ).
          WHEN `increment_example`.
            increment_example( EXPORTING request  = request
                               CHANGING  response = response ).
          WHEN `start_slow_computation`.
            start_slow_computation( EXPORTING request  = request
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

  METHOD get_session_details.
    response-result->add_text_content( |Session ID: { server-session_id }| ) ##NO_TEXT.
  ENDMETHOD.

  METHOD increment_example.
    DATA input TYPE REF TO zif_mcp_ajson.
        DATA schema TYPE REF TO zcl_mcp_schema_builder.
        DATA validator TYPE REF TO zcl_mcp_schema_validator.
        DATA validation_result TYPE abap_bool.
        DATA error TYPE REF TO zcx_mcp_ajson_error.
    DATA increment TYPE i.
    DATA session_increment TYPE zcl_mcp_session=>session_entry.
    DATA current_increment TYPE i.
    DATA temp6 TYPE zcl_mcp_session=>session_entry.
    input = request->get_arguments( ).

    " Validate input parameter via schema validator class
    TRY.

        schema = get_increment_schema( ).

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


    increment = input->get_integer( `increment` ).
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

    CLEAR temp6.
    temp6-key = `increment`.
    temp6-value = current_increment.
    session->add( temp6 ).
  ENDMETHOD.

  METHOD get_session_mode.
    result = zcl_mcp_session=>session_mode_mcp.
  ENDMETHOD.

  METHOD get_increment_schema.
    CREATE OBJECT result TYPE zcl_mcp_schema_builder.
    result->add_integer( name        = `increment`
                         description = `Increment value`
                         required    = abap_true
                         minimum     = 1
                         maximum     = 1000000 ) ##NO_TEXT.
  ENDMETHOD.

  METHOD start_slow_computation.
    DATA value TYPE i.
      DATA task_id TYPE sysuuid_c32.
      DATA job_name TYPE btcjob VALUE 'ZMCP_DEMO_TASK' .
      DATA job_count TYPE btcjobcnt.
          DATA temp7 TYPE i.
          DATA ttl LIKE temp7.
          DATA task_error TYPE REF TO zcx_mcp_server.
          DATA task TYPE zif_mcp_types=>task.
          DATA get_error TYPE REF TO zcx_mcp_server.
      DATA temp8 TYPE i.
      DATA random TYPE REF TO cl_abap_random.
      DATA wait_secs TYPE i.
          DATA sc TYPE REF TO zcl_mcp_ajson.
          DATA json_error TYPE REF TO zcx_mcp_ajson_error.
    value = request->get_arguments( )->get_integer( `value` ).

    IF request->has_task( ) IS NOT INITIAL.
      " Async path - submit background job and return task ID immediately




      TRY.

          IF request->get_task_ttl( ) > 0.
            temp7 = request->get_task_ttl( ).
          ELSE.
            temp7 = 300000.
          ENDIF.

          ttl = temp7.
          task_id = get_tasks( )->create_task( tool_name = request->get_name( )
                                               ttl       = ttl ).

        CATCH zcx_mcp_server INTO task_error.
          response-error-code    = zcl_mcp_jsonrpc=>error_codes-internal_error.
          response-error-message = task_error->get_text( ).
          RETURN.
      ENDTRY.

      CALL FUNCTION 'JOB_OPEN'
        EXPORTING  jobname  = job_name
        IMPORTING  jobcount = job_count
        EXCEPTIONS OTHERS   = 1.

      IF sy-subrc <> 0.
        TRY.
            zcl_mcp_tasks=>fail( task_id = task_id
                                 message = 'Failed to open background job' ) ##NO_TEXT.
          CATCH zcx_mcp_server ##NO_HANDLER.
        ENDTRY.
        response-error-code    = zcl_mcp_jsonrpc=>error_codes-internal_error.
        response-error-message = 'Failed to open background job' ##NO_TEXT.
        RETURN.
      ENDIF.

      SUBMIT zmcp_demo_bg_task
             WITH p_taskid = task_id
             WITH p_value  = value
             VIA JOB job_name NUMBER job_count
             AND RETURN.

      CALL FUNCTION 'JOB_CLOSE'
        EXPORTING  jobname   = job_name
                   jobcount  = job_count
                   strtimmed = abap_true
        EXCEPTIONS OTHERS    = 1.

      IF sy-subrc <> 0.
        TRY.
            zcl_mcp_tasks=>fail( task_id = task_id
                                 message = 'Failed to schedule background job' ) ##NO_TEXT.
          CATCH zcx_mcp_server ##NO_HANDLER.
        ENDTRY.
        response-error-code    = zcl_mcp_jsonrpc=>error_codes-internal_error.
        response-error-message = 'Failed to schedule background job' ##NO_TEXT.
        RETURN.
      ENDIF.

      TRY.

          task = get_tasks( )->get( task_id ).
          response-result->set_task_result( task ).

        CATCH zcx_mcp_server INTO get_error.
          response-error-code    = zcl_mcp_jsonrpc=>error_codes-internal_error.
          response-error-message = get_error->get_text( ).
      ENDTRY.

    ELSE.
      " Synchronous path - block and wait (demonstrates why async exists)

      temp8 = sy-uzeit.

      random    = cl_abap_random=>create( seed = temp8 ).

      wait_secs = random->intinrange( low  = 30
                                            high = 60 ).
      WAIT UP TO wait_secs SECONDS.

      TRY.

          sc = zcl_mcp_ajson=>create_empty( ).
          sc->set_integer( iv_path = `/input_value`
                           iv_val  = value ).
          sc->set_integer( iv_path = `/computed_value`
                           iv_val  = value * value ).
          sc->set_integer( iv_path = `/wait_seconds`
                           iv_val  = wait_secs ).

          response-result->set_structured_content( structured_content = sc
                                                   add_text_content   = abap_false ).
          response-result->add_text_content(
              |{ value }^2 = { value * value }, computed in { wait_secs }s (sync - consider using async!)| ) ##NO_TEXT.

        CATCH zcx_mcp_ajson_error INTO json_error.
          response-error-code    = zcl_mcp_jsonrpc=>error_codes-internal_error.
          response-error-message = json_error->get_text( ).
      ENDTRY.
    ENDIF.
  ENDMETHOD.

METHOD handle_cancel_task.
  " The background job checks task status before completing, so simply
  " marking as cancelled in the DB is sufficient - the job will see it
  " and exit cleanly after its WAIT UP TO returns.
ENDMETHOD.                                         "#EC EMPTY_PROCEDURE

  METHOD get_computation_schema.
    CREATE OBJECT result TYPE zcl_mcp_schema_builder.
    result->add_integer( name        = `value`
                         description = `Value to square`
                         required    = abap_true
                         minimum     = 1
                         maximum     = 10000 ) ##NO_TEXT.
  ENDMETHOD.

ENDCLASS.
