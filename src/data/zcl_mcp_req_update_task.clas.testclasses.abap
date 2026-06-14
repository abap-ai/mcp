CLASS ltcl_mcp_req_update_task DEFINITION DEFERRED.
CLASS zcl_mcp_req_update_task DEFINITION LOCAL FRIENDS ltcl_mcp_req_update_task.

CLASS ltcl_mcp_req_update_task DEFINITION FINAL
  FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.

  PRIVATE SECTION.
    METHODS parse_full_request      FOR TESTING.
    METHODS parse_lowercase_task_id FOR TESTING.
    METHODS missing_task_id         FOR TESTING.
    METHODS invalid_task_id         FOR TESTING.
    METHODS missing_input_responses FOR TESTING.
ENDCLASS.

CLASS ltcl_mcp_req_update_task IMPLEMENTATION.
  METHOD parse_full_request.
    DATA json TYPE REF TO zif_mcp_ajson.
    DATA cut  TYPE REF TO zcl_mcp_req_update_task.

    TRY.
        json = zcl_mcp_ajson=>parse(
                   `{"taskId":"00000000000000000000000000000001","inputResponses":{"confirm":{"approved":true}},"requestState":"state-1","_meta":{"vendor/trace":"abc"}}` ).

        cut = NEW zcl_mcp_req_update_task( json ).

        cl_abap_unit_assert=>assert_equals( exp = `00000000000000000000000000000001`
                                            act = cut->get_task_id( ) ).
        cl_abap_unit_assert=>assert_true( cut->has_input_responses( ) ).
        cl_abap_unit_assert=>assert_bound( cut->get_input_responses( ) ).
        cl_abap_unit_assert=>assert_true( cut->get_input_responses( )->get_boolean( `/confirm/approved` ) ).
        cl_abap_unit_assert=>assert_equals( exp = `state-1`
                                            act = cut->get_request_state( ) ).
        cl_abap_unit_assert=>assert_equals( exp = `abc`
                                            act = cut->get_meta( )->get_string( `/vendor~1trace` ) ).
      CATCH zcx_mcp_ajson_error
            zcx_mcp_server INTO DATA(error).
        cl_abap_unit_assert=>fail( error->get_text( ) ).
    ENDTRY.
  ENDMETHOD.

  METHOD parse_lowercase_task_id.
    DATA json TYPE REF TO zif_mcp_ajson.
    DATA cut  TYPE REF TO zcl_mcp_req_update_task.

    TRY.
        json = zcl_mcp_ajson=>parse( `{"taskId":"abcdefabcdefabcdefabcdefabcdef12","inputResponses":{}}` ).

        cut = NEW zcl_mcp_req_update_task( json ).

        cl_abap_unit_assert=>assert_equals( exp = `ABCDEFABCDEFABCDEFABCDEFABCDEF12`
                                            act = cut->get_task_id( ) ).
        cl_abap_unit_assert=>assert_bound( cut->get_meta( ) ).
      CATCH zcx_mcp_ajson_error
            zcx_mcp_server INTO DATA(error).
        cl_abap_unit_assert=>fail( error->get_text( ) ).
    ENDTRY.
  ENDMETHOD.

  METHOD missing_task_id.
    TRY.
        DATA(json) = zcl_mcp_ajson=>parse( `{"inputResponses":{}}` ).
        DATA(cut) = NEW zcl_mcp_req_update_task( json ).
        cl_abap_unit_assert=>fail( `Expected missing taskId error` ).
      CATCH zcx_mcp_server INTO DATA(error).
        cl_abap_unit_assert=>assert_equals( exp = zcx_mcp_server=>required_params
                                            act = error->if_t100_message~t100key ).
      CATCH zcx_mcp_ajson_error INTO DATA(root_error).
        cl_abap_unit_assert=>fail( root_error->get_text( ) ).
    ENDTRY.
  ENDMETHOD.

  METHOD invalid_task_id.
    TRY.
        DATA(json) = zcl_mcp_ajson=>parse( `{"taskId":"not-a-task","inputResponses":{}}` ).
        DATA(cut) = NEW zcl_mcp_req_update_task( json ).
        cl_abap_unit_assert=>fail( `Expected invalid taskId error` ).
      CATCH zcx_mcp_server INTO DATA(error).
        cl_abap_unit_assert=>assert_equals( exp = zcx_mcp_server=>invalid_arguments
                                            act = error->if_t100_message~t100key ).
      CATCH zcx_mcp_ajson_error INTO DATA(root_error).
        cl_abap_unit_assert=>fail( root_error->get_text( ) ).
    ENDTRY.
  ENDMETHOD.

  METHOD missing_input_responses.
    TRY.
        DATA(json) = zcl_mcp_ajson=>parse( `{"taskId":"00000000000000000000000000000001"}` ).
        DATA(cut) = NEW zcl_mcp_req_update_task( json ).
        cl_abap_unit_assert=>fail( `Expected missing inputResponses error` ).
      CATCH zcx_mcp_server INTO DATA(error).
        cl_abap_unit_assert=>assert_equals( exp = zcx_mcp_server=>required_params
                                            act = error->if_t100_message~t100key ).
      CATCH zcx_mcp_ajson_error INTO DATA(root_error).
        cl_abap_unit_assert=>fail( root_error->get_text( ) ).
    ENDTRY.
  ENDMETHOD.
ENDCLASS.
