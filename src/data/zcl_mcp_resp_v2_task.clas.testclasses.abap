CLASS ltcl_mcp_resp_v2_task DEFINITION DEFERRED.
CLASS zcl_mcp_resp_v2_task DEFINITION LOCAL FRIENDS ltcl_mcp_resp_v2_task.

CLASS ltcl_mcp_resp_v2_task DEFINITION FINAL
  FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.

  PRIVATE SECTION.
    METHODS minimal_task   FOR TESTING.
    METHODS full_task      FOR TESTING.
    METHODS cache_and_meta FOR TESTING.

    METHODS assert_json_equals
      IMPORTING !actual  TYPE string
                expected TYPE string.
ENDCLASS.


CLASS ltcl_mcp_resp_v2_task IMPLEMENTATION.
  METHOD minimal_task.
    DATA cut  TYPE REF TO zcl_mcp_resp_v2_task.
    DATA json TYPE REF TO zif_mcp_ajson.

    cut = NEW zcl_mcp_resp_v2_task( ).

    TRY.
        cut->set_task( task_id = `00000000000000000000000000000001`
                       status  = `working` ).

        json = cut->zif_mcp_modern_result~generate_json( ).

        assert_json_equals(
            actual   = json->stringify( )
            expected = `{"resultType":"task","task":{"taskId":"00000000000000000000000000000001","status":"working"}}` ).
      CATCH zcx_mcp_ajson_error INTO DATA(error).
        cl_abap_unit_assert=>fail( error->get_text( ) ).
    ENDTRY.
  ENDMETHOD.

  METHOD full_task.
    DATA cut  TYPE REF TO zcl_mcp_resp_v2_task.
    DATA json TYPE REF TO zif_mcp_ajson.

    cut = NEW zcl_mcp_resp_v2_task( ).

    TRY.
        cut->set_task( task_id          = `00000000000000000000000000000001`
                       status           = `working`
                       status_message   = `Task accepted.`
                       ttl_ms           = 60000
                       poll_interval_ms = 1000 ).

        json = cut->zif_mcp_modern_result~generate_json( ).

        assert_json_equals(
            actual   = json->stringify( )
            expected = `{"resultType":"task","task":{"taskId":"00000000000000000000000000000001","status":"working","statusMessage":"Task accepted.","ttlMs":60000,"pollIntervalMs":1000}}` ).
      CATCH zcx_mcp_ajson_error INTO DATA(error).
        cl_abap_unit_assert=>fail( error->get_text( ) ).
    ENDTRY.
  ENDMETHOD.

  METHOD cache_and_meta.
    DATA cut  TYPE REF TO zcl_mcp_resp_v2_task.
    DATA meta TYPE REF TO zif_mcp_ajson.
    DATA json TYPE REF TO zif_mcp_ajson.

    cut = NEW zcl_mcp_resp_v2_task( ).
    meta = zcl_mcp_ajson=>create_empty( ).

    TRY.
        meta->set_string( iv_path = `/vendor~1trace`
                          iv_val  = `abc` ).

        cut->set_task( task_id = `00000000000000000000000000000001`
                       status  = `working` ).
        cut->zif_mcp_modern_result~set_meta( meta ).
        cut->zif_mcp_modern_result~set_cache( ttl_ms      = 1000
                                              cache_scope = zif_mcp_constants=>cache_scopes-private ).

        json = cut->zif_mcp_modern_result~generate_json( ).

        assert_json_equals(
            actual   = json->stringify( )
            expected = `{"resultType":"task","task":{"taskId":"00000000000000000000000000000001","status":"working"},"ttlMs":1000,"cacheScope":"private","_meta":{"vendor/trace":"abc"}}` ).
      CATCH zcx_mcp_ajson_error INTO DATA(error).
        cl_abap_unit_assert=>fail( error->get_text( ) ).
    ENDTRY.
  ENDMETHOD.

  METHOD assert_json_equals.
    TRY.
        DATA(actual_obj) = zcl_mcp_ajson=>parse( actual ).
        DATA(expected_obj) = zcl_mcp_ajson=>parse( expected ).

        cl_abap_unit_assert=>assert_equals( exp = expected_obj->stringify( )
                                            act = actual_obj->stringify( ) ).
      CATCH zcx_mcp_ajson_error INTO DATA(error).
        cl_abap_unit_assert=>fail( error->get_text( ) ).
    ENDTRY.
  ENDMETHOD.
ENDCLASS.
