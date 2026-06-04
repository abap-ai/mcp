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
    response-result->set_capabilities( VALUE #( prompts   = VALUE #( enabled = abap_true )
                                                resources = VALUE #( enabled = abap_true )
                                                tools     = VALUE #( enabled = abap_true )
                                                tasks     = VALUE #( list       = abap_true
                                                                     cancel     = abap_true
                                                                     tools_call = abap_true ) ) ).
    response-result->set_implementation( VALUE #( name    = `Demo MCP Server - using MCP session logic. Tools only.`
                                                  version = `1.1.0` ) ) ##NO_TEXT.
    response-result->set_instructions(
        `Use the features provided by this server only if explicitly requested. If not sure ask the user!` ) ##NO_TEXT.
  ENDMETHOD.

  METHOD handle_list_tools.
    DATA tools TYPE zcl_mcp_resp_list_tools=>tools.

    APPEND VALUE #( name        = `get_session_details`
                    description = `Get details about the current session.` ) TO tools ##NO_TEXT.

    TRY.
        APPEND VALUE #(
            name         = `increment_example`
            description  = `Every time increments the result by the given number. Demonstrates session logic.`
            input_schema = get_increment_schema( )->to_json( ) )
               TO tools ##NO_TEXT.

        DATA(output_schema) = NEW zcl_mcp_schema_builder( ).
        output_schema->add_integer( name        = `input_value`
                                    description = `The value that was submitted`
                                    required    = abap_true ) ##NO_TEXT.
        output_schema->add_integer( name        = `computed_value`
                                    description = `Square of the input value`
                                    required    = abap_true ) ##NO_TEXT.
        output_schema->add_integer( name        = `wait_seconds`
                                    description = `Seconds the background job waited`
                                    required    = abap_true ) ##NO_TEXT.

        APPEND VALUE #( name          = `start_slow_computation`
                        title         = `Start Slow Computation`
                        description   = |Squares the input value in a background job after a random 30-60s delay. |
                                     && |Use tasks/get to poll status and tasks/result for the result.|
                        input_schema  = get_computation_schema( )->to_json( )
                        output_schema = output_schema->to_json( )
                        execution     = VALUE #( task_support = zcl_mcp_resp_list_tools=>task_support-optional ) )
               TO tools ##NO_TEXT.
      CATCH zcx_mcp_ajson_error INTO DATA(error).
        response-error-code    = zcl_mcp_jsonrpc=>error_codes-internal_error.
        response-error-message = error->get_text( ).
        RETURN.
    ENDTRY.

    response-result->set_tools( tools ).
  ENDMETHOD.

  METHOD handle_call_tool.
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
      CATCH zcx_mcp_ajson_error INTO DATA(error).
        response-error-code    = zcl_mcp_jsonrpc=>error_codes-internal_error.
        response-error-message = error->get_text( ).
    ENDTRY.
  ENDMETHOD.

  METHOD get_session_details.
    response-result->add_text_content( |Session ID: { server-session_id }| ) ##NO_TEXT.
  ENDMETHOD.

  METHOD increment_example.
    DATA(input) = request->get_arguments( ).

    " Validate input parameter via schema validator class
    TRY.
        DATA(schema) = get_increment_schema( ).
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

    DATA(increment) = input->get_integer( `increment` ).
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

  METHOD get_increment_schema.
    result = NEW zcl_mcp_schema_builder( ).
    result->add_integer( name        = `increment`
                         description = `Increment value`
                         required    = abap_true
                         minimum     = 1
                         maximum     = 1000000 ) ##NO_TEXT.
  ENDMETHOD.

  METHOD start_slow_computation.
    DATA(value) = request->get_arguments( )->get_integer( `value` ).

    IF request->has_task( ).
      " Async path - submit background job and return task ID immediately
      DATA task_id   TYPE sysuuid_c32.
      DATA job_name  TYPE btcjob VALUE 'ZMCP_DEMO_TASK' ##NO_TEXT.
      DATA job_count TYPE btcjobcnt.

      TRY.
          DATA(ttl) = COND i( WHEN request->get_task_ttl( ) > 0
                              THEN request->get_task_ttl( )
                              ELSE 300 ).
          task_id = get_tasks( )->create_task( tool_name = request->get_name( )
                                               ttl       = ttl ).
        CATCH zcx_mcp_server INTO DATA(task_error).
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
          DATA(task) = get_tasks( )->get( task_id ).
          response-result->set_task_result( task ).
        CATCH zcx_mcp_server INTO DATA(get_error).
          response-error-code    = zcl_mcp_jsonrpc=>error_codes-internal_error.
          response-error-message = get_error->get_text( ).
      ENDTRY.

    ELSE.
      " Synchronous path - block and wait (demonstrates why async exists)
      DATA(random)    = cl_abap_random=>create( seed = CONV i( sy-uzeit ) ).
      DATA(wait_secs) = random->intinrange( low  = 30
                                            high = 60 ).
      WAIT UP TO wait_secs SECONDS.

      TRY.
          DATA(sc) = zcl_mcp_ajson=>create_empty( ).
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
        CATCH zcx_mcp_ajson_error INTO DATA(json_error).
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
    result = NEW zcl_mcp_schema_builder( ).
    result->add_integer( name        = `value`
                         description = `Value to square`
                         required    = abap_true
                         minimum     = 1
                         maximum     = 10000 ) ##NO_TEXT.
  ENDMETHOD.

ENDCLASS.
