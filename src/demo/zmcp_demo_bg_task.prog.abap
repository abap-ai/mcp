REPORT zmcp_demo_bg_task.

PARAMETERS: p_taskid TYPE sysuuid_c32,
            p_value  TYPE i.

CONSTANTS total_secs TYPE i VALUE 60.
CONSTANTS poll_secs  TYPE i VALUE 5.

START-OF-SELECTION.
  TRY.
      IF zcl_mcp_tasks=>get_status( p_taskid ) = zcl_mcp_tasks=>status_cancelled.
        RETURN.
      ENDIF.
    CATCH zcx_mcp_server.
      RETURN.
  ENDTRY.

  " Poll every 5 seconds; bail out early on cancellation
  DATA elapsed TYPE i VALUE 0.
  WHILE elapsed < total_secs.
    WAIT UP TO poll_secs SECONDS.
    elapsed = elapsed + poll_secs.
    TRY.
        IF zcl_mcp_tasks=>get_status( p_taskid ) = zcl_mcp_tasks=>status_cancelled.
          RETURN.
        ENDIF.
      CATCH zcx_mcp_server.
        RETURN.
    ENDTRY.
  ENDWHILE.

  TRY.
      DATA(sc) = zcl_mcp_ajson=>create_empty( ).
      sc->set_integer( iv_path = `/input_value`
                       iv_val  = p_value ).
      sc->set_integer( iv_path = `/computed_value`
                       iv_val  = p_value * p_value ).
      sc->set_integer( iv_path = `/wait_seconds`
                       iv_val  = total_secs ).

      DATA(task_result) = NEW zcl_mcp_resp_task_payload( ).
      task_result->set_structured_content( sc ).
      task_result->add_text_content( |{ p_value }^2 = { p_value * p_value }, computed in { total_secs }s| ) ##NO_TEXT.

      zcl_mcp_tasks=>complete( task_id = p_taskid
                               result  = task_result ).
      COMMIT WORK AND WAIT.
    CATCH zcx_mcp_server.
      " Cancelled between last poll and complete - nothing to do
  ENDTRY.
