CLASS ltcl_mcp_elicit_result DEFINITION FINAL
  FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.

  PRIVATE SECTION.
    METHODS accept_with_content FOR TESTING RAISING cx_static_check.
    METHODS decline_no_content  FOR TESTING RAISING cx_static_check.
    METHODS cancel_no_content   FOR TESTING RAISING cx_static_check.
    METHODS missing_action      FOR TESTING.
    METHODS invalid_action      FOR TESTING.
ENDCLASS.


CLASS ltcl_mcp_elicit_result IMPLEMENTATION.
  METHOD accept_with_content.
    DATA json TYPE REF TO zif_mcp_ajson.
    DATA cut  TYPE REF TO zcl_mcp_elicit_result.

    json = zcl_mcp_ajson=>parse( `{"action":"accept","content":{"approved":true,"topic":"draft","count":3}}` ).

    cut = NEW zcl_mcp_elicit_result( json ).

    cl_abap_unit_assert=>assert_equals( exp = zcl_mcp_elicit_result=>actions-accept
                                        act = cut->get_action( ) ).
    cl_abap_unit_assert=>assert_true( cut->is_accept( ) ).
    cl_abap_unit_assert=>assert_false( cut->is_decline( ) ).
    cl_abap_unit_assert=>assert_false( cut->is_cancel( ) ).
    cl_abap_unit_assert=>assert_true( cut->has_content( ) ).
    cl_abap_unit_assert=>assert_true( cut->get_boolean( `approved` ) ).
    cl_abap_unit_assert=>assert_equals( exp = `draft`
                                        act = cut->get_string( `topic` ) ).
    cl_abap_unit_assert=>assert_equals( exp = 3
                                        act = cut->get_integer( `count` ) ).
  ENDMETHOD.

  METHOD decline_no_content.
    DATA json TYPE REF TO zif_mcp_ajson.
    DATA cut  TYPE REF TO zcl_mcp_elicit_result.

    json = zcl_mcp_ajson=>parse( `{"action":"decline"}` ).
    cut = NEW zcl_mcp_elicit_result( json ).

    cl_abap_unit_assert=>assert_true( cut->is_decline( ) ).
    cl_abap_unit_assert=>assert_false( cut->has_content( ) ).
    cl_abap_unit_assert=>assert_bound( cut->get_content( ) ).
  ENDMETHOD.

  METHOD cancel_no_content.
    DATA json TYPE REF TO zif_mcp_ajson.
    DATA cut  TYPE REF TO zcl_mcp_elicit_result.

    json = zcl_mcp_ajson=>parse( `{"action":"cancel"}` ).
    cut = NEW zcl_mcp_elicit_result( json ).

    cl_abap_unit_assert=>assert_true( cut->is_cancel( ) ).
    cl_abap_unit_assert=>assert_false( cut->has_content( ) ).
  ENDMETHOD.

  METHOD missing_action.
    DATA json TYPE REF TO zif_mcp_ajson.

    TRY.
        json = zcl_mcp_ajson=>parse( `{"content":{"approved":true}}` ).
        NEW zcl_mcp_elicit_result( json ).
        cl_abap_unit_assert=>fail( `Expected missing action error` ).
      CATCH zcx_mcp_server ##NO_HANDLER.
      CATCH zcx_mcp_ajson_error INTO DATA(json_error).
        cl_abap_unit_assert=>fail( json_error->get_text( ) ).
    ENDTRY.
  ENDMETHOD.

  METHOD invalid_action.
    DATA json TYPE REF TO zif_mcp_ajson.

    TRY.
        json = zcl_mcp_ajson=>parse( `{"action":"maybe"}` ).
        NEW zcl_mcp_elicit_result( json ).
        cl_abap_unit_assert=>fail( `Expected invalid action error` ).
      CATCH zcx_mcp_server ##NO_HANDLER.
      CATCH zcx_mcp_ajson_error INTO DATA(json_error).
        cl_abap_unit_assert=>fail( json_error->get_text( ) ).
    ENDTRY.
  ENDMETHOD.
ENDCLASS.
