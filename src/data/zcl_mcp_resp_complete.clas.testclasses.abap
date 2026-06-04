CLASS ltcl_mcp_test_resp_complete DEFINITION FINAL
 FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.

  PRIVATE SECTION.
    DATA cut  TYPE REF TO zcl_mcp_resp_complete.
    DATA json TYPE REF TO zif_mcp_ajson.

    METHODS setup.

    METHODS empty_values_array        FOR TESTING RAISING cx_static_check.
    METHODS single_value              FOR TESTING RAISING cx_static_check.
    METHODS multiple_values_add       FOR TESTING RAISING cx_static_check.
    METHODS set_values_replaces       FOR TESTING RAISING cx_static_check.
    METHODS total_emitted_when_set    FOR TESTING RAISING cx_static_check.
    METHODS total_omitted_when_zero   FOR TESTING RAISING cx_static_check.
    METHODS has_more_emitted_true     FOR TESTING RAISING cx_static_check.
    METHODS has_more_omitted_false    FOR TESTING RAISING cx_static_check.
    METHODS meta_emitted_when_bound   FOR TESTING RAISING cx_static_check.
    METHODS meta_omitted_when_unbound FOR TESTING RAISING cx_static_check.
    METHODS all_fields_combined       FOR TESTING RAISING cx_static_check.
  ENDCLASS.

  CLASS ltcl_mcp_test_resp_complete IMPLEMENTATION.
  METHOD setup.
    cut  = NEW zcl_mcp_resp_complete( ).
    json = zcl_mcp_ajson=>create_empty( ).
  ENDMETHOD.

  METHOD empty_values_array.
    json = cut->zif_mcp_internal~generate_json( ).
    cl_abap_unit_assert=>assert_true( act = json->exists( '/completion/values' )
                                      msg = 'values array must always be present' ).
    cl_abap_unit_assert=>assert_equals( exp = 'array'
                                        act = json->get_node_type( '/completion/values' )
                                        msg = 'values must be an array node' ).
    cl_abap_unit_assert=>assert_false( act = json->exists( '/completion/values/1' )
                                       msg = 'array must be empty' ).
  ENDMETHOD.

  METHOD single_value.
    cut->add_value( 'dev' ).
    json = cut->zif_mcp_internal~generate_json( ).
    cl_abap_unit_assert=>assert_equals( exp = 'dev'
                                        act = json->get_string( '/completion/values/1' )
                                        msg = 'first candidate' ).
    cl_abap_unit_assert=>assert_false( act = json->exists( '/completion/values/2' )
                                       msg = 'no second element' ).
  ENDMETHOD.

  METHOD multiple_values_add.
    cut->add_value( 'dev' ).
    cut->add_value( 'test' ).
    cut->add_value( 'prod' ).
    json = cut->zif_mcp_internal~generate_json( ).
    cl_abap_unit_assert=>assert_equals( exp = 'dev'
                                        act = json->get_string( '/completion/values/1' )
                                        msg = '1' ).
    cl_abap_unit_assert=>assert_equals( exp = 'test'
                                        act = json->get_string( '/completion/values/2' )
                                        msg = '2' ).
    cl_abap_unit_assert=>assert_equals( exp = 'prod'
                                        act = json->get_string( '/completion/values/3' )
                                        msg = '3' ).
    cl_abap_unit_assert=>assert_false( act = json->exists( '/completion/values/4' )
                                       msg = 'no fourth element' ).
  ENDMETHOD.

  METHOD set_values_replaces.
    cut->add_value( 'old' ).
    cut->set_values( VALUE #( ( `new1` ) ( `new2` ) ) ).
    json = cut->zif_mcp_internal~generate_json( ).
    cl_abap_unit_assert=>assert_equals( exp = 'new1'
                                        act = json->get_string( '/completion/values/1' )
                                        msg = '1' ).
    cl_abap_unit_assert=>assert_equals( exp = 'new2'
                                        act = json->get_string( '/completion/values/2' )
                                        msg = '2' ).
    cl_abap_unit_assert=>assert_false( act = json->exists( '/completion/values/3' )
                                       msg = 'old value must be gone' ).
  ENDMETHOD.

  METHOD total_emitted_when_set.
    cut->add_value( 'a' ).
    cut->set_total( 42 ).
    json = cut->zif_mcp_internal~generate_json( ).
    cl_abap_unit_assert=>assert_true( act = json->exists( '/completion/total' )
                                      msg = 'total must be present' ).
    cl_abap_unit_assert=>assert_equals( exp = 42
                                        act = json->get_integer( '/completion/total' )
                                        msg = 'total value' ).
  ENDMETHOD.

  METHOD total_omitted_when_zero.
    cut->set_total( 0 ).
    json = cut->zif_mcp_internal~generate_json( ).
    cl_abap_unit_assert=>assert_false( act = json->exists( '/completion/total' )
                                       msg = 'total must be absent when zero' ).
  ENDMETHOD.

    METHOD has_more_emitted_true.
      cut->set_has_more( abap_true ).
      json = cut->zif_mcp_internal~generate_json( ).
      cl_abap_unit_assert=>assert_true(
        act = json->exists( '/completion/hasMore' )
        msg = 'hasMore must be present' ).
    cl_abap_unit_assert=>assert_true( act = json->get_boolean( '/completion/hasMore' )
                                      msg = 'hasMore value' ).
    ENDMETHOD.

  METHOD has_more_omitted_false.
    cut->set_has_more( abap_false ).
    json = cut->zif_mcp_internal~generate_json( ).
    cl_abap_unit_assert=>assert_false( act = json->exists( '/completion/hasMore' )
                                       msg = 'hasMore must be absent when false' ).
  ENDMETHOD.

  METHOD meta_emitted_when_bound.
    DATA(meta) = zcl_mcp_ajson=>create_empty( ).
    meta->set_string( iv_path = '/token'
                      iv_val  = 'xyz' ).
    cut->set_meta( meta ).
    json = cut->zif_mcp_internal~generate_json( ).
    cl_abap_unit_assert=>assert_true( act = json->exists( '/_meta' )
                                      msg = '_meta must be present' ).
    cl_abap_unit_assert=>assert_equals( exp = 'xyz'
                                        act = json->get_string( '/_meta/token' )
                                        msg = '_meta token value' ).
  ENDMETHOD.

  METHOD meta_omitted_when_unbound.
    json = cut->zif_mcp_internal~generate_json( ).
    cl_abap_unit_assert=>assert_false( act = json->exists( '/_meta' )
                                       msg = '_meta must be absent when not set' ).
  ENDMETHOD.

  METHOD all_fields_combined.
    DATA(meta) = zcl_mcp_ajson=>create_empty( ).
    meta->set_string( iv_path = '/progressToken'
                      iv_val  = 'tok1' ).

    cut->set_values( VALUE #( ( `alpha` ) ( `beta` ) ) ).
    cut->set_total( 10 ).
    cut->set_has_more( abap_true ).
    cut->set_meta( meta ).

    json = cut->zif_mcp_internal~generate_json( ).

    cl_abap_unit_assert=>assert_equals( exp = 'alpha'
                                        act = json->get_string( '/completion/values/1' )
                                        msg = 'v1' ).
    cl_abap_unit_assert=>assert_equals( exp = 'beta'
                                        act = json->get_string( '/completion/values/2' )
                                        msg = 'v2' ).
    cl_abap_unit_assert=>assert_equals( exp = 10
                                        act = json->get_integer( '/completion/total' )
                                        msg = 'total' ).
    cl_abap_unit_assert=>assert_true( act = json->get_boolean( '/completion/hasMore' )
                                      msg = 'hasMore' ).
    cl_abap_unit_assert=>assert_equals( exp = 'tok1'
                                        act = json->get_string( '/_meta/progressToken' )
                                        msg = 'meta token' ).
  ENDMETHOD.
ENDCLASS.
