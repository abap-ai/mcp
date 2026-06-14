  CLASS ltcl_mcp_test_req_complete DEFINITION
    FINAL
    FOR TESTING
    RISK LEVEL HARMLESS
    DURATION SHORT.

  PRIVATE SECTION.
    "--- helpers ---
    METHODS prompt_json
      IMPORTING !name         TYPE string DEFAULT 'my_prompt'
                arg_name      TYPE string DEFAULT 'env'
                arg_value     TYPE string DEFAULT 'pr'
      RETURNING VALUE(result) TYPE REF TO zif_mcp_ajson
      RAISING   zcx_mcp_ajson_error.

    METHODS resource_json
      IMPORTING uri           TYPE string DEFAULT 'file:///{path}'
                arg_name      TYPE string DEFAULT 'path'
                arg_value     TYPE string DEFAULT '/tmp'
      RETURNING VALUE(result) TYPE REF TO zif_mcp_ajson
      RAISING   zcx_mcp_ajson_error.

    "--- constructor / getters ---
    METHODS prompt_ref_parsed       FOR TESTING RAISING cx_static_check.
    METHODS resource_ref_parsed     FOR TESTING RAISING cx_static_check.
    METHODS argument_value_optional FOR TESTING RAISING cx_static_check.
    METHODS context_parsed          FOR TESTING RAISING cx_static_check.
    METHODS no_context              FOR TESTING RAISING cx_static_check.
    METHODS meta_parsed             FOR TESTING RAISING cx_static_check.
    METHODS no_meta                 FOR TESTING RAISING cx_static_check.

    "--- validation ---
    METHODS missing_ref_type        FOR TESTING RAISING cx_static_check.
    METHODS unknown_ref_type        FOR TESTING RAISING cx_static_check.
    METHODS missing_ref_name        FOR TESTING RAISING cx_static_check.
    METHODS missing_ref_uri         FOR TESTING RAISING cx_static_check.
    METHODS missing_argument_name   FOR TESTING RAISING cx_static_check.
    METHODS empty_argument_value    FOR TESTING RAISING cx_static_check.
  ENDCLASS.

  CLASS ltcl_mcp_test_req_complete IMPLEMENTATION.
  METHOD prompt_json.
    result = zcl_mcp_ajson=>create_empty( ).
    result->set_string( iv_path = '/ref/type'
                        iv_val  = zcl_mcp_req_complete=>ref_type-prompt ).
    result->set_string( iv_path = '/ref/name'
                        iv_val  = name ).
    result->set_string( iv_path = '/argument/name'
                        iv_val  = arg_name ).
    result->set_string( iv_path = '/argument/value'
                        iv_val  = arg_value ).
  ENDMETHOD.

  METHOD resource_json.
    result = zcl_mcp_ajson=>create_empty( ).
    result->set_string( iv_path = '/ref/type'
                        iv_val  = zcl_mcp_req_complete=>ref_type-resource ).
    result->set_string( iv_path = '/ref/uri'
                        iv_val  = uri ).
    result->set_string( iv_path = '/argument/name'
                        iv_val  = arg_name ).
    result->set_string( iv_path = '/argument/value'
                        iv_val  = arg_value ).
  ENDMETHOD.

  METHOD prompt_ref_parsed.
    DATA(req) = NEW zcl_mcp_req_complete( prompt_json( name      = 'greet'
                                                       arg_name  = 'lang'
                                                       arg_value = 'en' ) ).
    cl_abap_unit_assert=>assert_equals( exp = zcl_mcp_req_complete=>ref_type-prompt
                                        act = req->get_ref_type( )
                                        msg = 'ref_type' ).
    cl_abap_unit_assert=>assert_equals( exp = 'greet'
                                        act = req->get_ref_name( )
                                        msg = 'ref_name' ).
    cl_abap_unit_assert=>assert_equals( exp = ''
                                        act = req->get_ref_uri( )
                                        msg = 'ref_uri empty' ).
    cl_abap_unit_assert=>assert_equals( exp = 'lang'
                                        act = req->get_argument_name( )
                                        msg = 'arg_name' ).
    cl_abap_unit_assert=>assert_equals( exp = 'en'
                                        act = req->get_argument_value( )
                                        msg = 'arg_value' ).
  ENDMETHOD.

    METHOD resource_ref_parsed.
    DATA(req) = NEW zcl_mcp_req_complete( resource_json( uri       = 'file:///{path}'
                                                         arg_name  = 'path'
                                                         arg_value = '/ho' ) ).
      cl_abap_unit_assert=>assert_equals( exp = zcl_mcp_req_complete=>ref_type-resource
                                          act = req->get_ref_type( )
                                          msg = 'ref_type' ).
      cl_abap_unit_assert=>assert_equals( exp = 'file:///{path}' act = req->get_ref_uri( )  msg = 'ref_uri' ).
      cl_abap_unit_assert=>assert_equals( exp = ''               act = req->get_ref_name( ) msg = 'ref_name empty' ).
      cl_abap_unit_assert=>assert_equals( exp = 'path'           act = req->get_argument_name( )  msg = 'arg_name' ).
      cl_abap_unit_assert=>assert_equals( exp = '/ho'            act = req->get_argument_value( ) msg = 'arg_value' ).
    ENDMETHOD.

  METHOD argument_value_optional.
    DATA(json) = zcl_mcp_ajson=>create_empty( ).
    json->set_string( iv_path = '/ref/type'
                      iv_val  = zcl_mcp_req_complete=>ref_type-prompt ).
    json->set_string( iv_path = '/ref/name'
                      iv_val  = 'p' ).
    json->set_string( iv_path = '/argument/name'
                      iv_val  = 'x' ).

    TRY.
        NEW zcl_mcp_req_complete( json ).
        cl_abap_unit_assert=>fail( 'Exception expected for missing argument.value' ).
      CATCH zcx_mcp_server INTO DATA(ex).
        cl_abap_unit_assert=>assert_equals( exp = zcx_mcp_server=>required_params
                                            act = ex->if_t100_message~t100key
                                            msg = 'exception key' ).
    ENDTRY.
  ENDMETHOD.

  METHOD context_parsed.
    DATA(json) = prompt_json( ).
    json->set_string( iv_path = '/context/arguments/region'
                      iv_val  = 'eu' ).
    json->set_string( iv_path = '/context/arguments/stage'
                      iv_val  = 'prod' ).

    DATA(req) = NEW zcl_mcp_req_complete( json ).
    cl_abap_unit_assert=>assert_true( act = req->has_context( )
                                      msg = 'has_context' ).
    cl_abap_unit_assert=>assert_bound( act = req->get_context_json( )
                                       msg = 'context_json bound' ).
    cl_abap_unit_assert=>assert_equals( exp = 'eu'
                                        act = req->get_context_json( )->get_string( '/arguments/region' )
                                        msg = 'context region' ).
    cl_abap_unit_assert=>assert_equals( exp = 'prod'
                                        act = req->get_context_json( )->get_string( '/arguments/stage' )
                                        msg = 'context stage' ).
  ENDMETHOD.

  METHOD no_context.
    DATA(req) = NEW zcl_mcp_req_complete( prompt_json( ) ).
    cl_abap_unit_assert=>assert_false( act = req->has_context( )
                                       msg = 'has_context false' ).
    cl_abap_unit_assert=>assert_not_bound( act = req->get_context_json( )
                                           msg = 'context_json unbound' ).
  ENDMETHOD.

  METHOD meta_parsed.
    DATA(json) = prompt_json( ).
    json->set_string( iv_path = '/_meta/token'
                      iv_val  = 'abc123' ).

    DATA(req) = NEW zcl_mcp_req_complete( json ).
    cl_abap_unit_assert=>assert_bound( act = req->get_meta( )
                                       msg = 'meta bound' ).
    cl_abap_unit_assert=>assert_equals( exp = 'abc123'
                                        act = req->get_meta( )->get_string( '/token' )
                                        msg = 'meta token' ).
  ENDMETHOD.

  METHOD no_meta.
    DATA(req) = NEW zcl_mcp_req_complete( prompt_json( ) ).
    cl_abap_unit_assert=>assert_not_bound( act = req->get_meta( )
                                           msg = 'meta unbound' ).
  ENDMETHOD.

  METHOD missing_ref_type.
    DATA(json) = zcl_mcp_ajson=>create_empty( ).
    json->set_string( iv_path = '/argument/name'
                      iv_val  = 'x' ).
    TRY.
        NEW zcl_mcp_req_complete( json ).
        cl_abap_unit_assert=>fail( 'Exception expected for missing ref.type' ).
      CATCH zcx_mcp_server INTO DATA(ex).
        cl_abap_unit_assert=>assert_equals( exp = zcx_mcp_server=>required_params
                                            act = ex->if_t100_message~t100key
                                            msg = 'exception key' ).
    ENDTRY.
  ENDMETHOD.

  METHOD unknown_ref_type.
    DATA(json) = zcl_mcp_ajson=>create_empty( ).
    json->set_string( iv_path = '/ref/type'
                      iv_val  = 'ref/unknown' ).
    json->set_string( iv_path = '/argument/name'
                      iv_val  = 'x' ).
    TRY.
        NEW zcl_mcp_req_complete( json ).
        cl_abap_unit_assert=>fail( 'Exception expected for unknown ref type' ).
      CATCH zcx_mcp_server INTO DATA(ex).
        cl_abap_unit_assert=>assert_equals( exp = zcx_mcp_server=>invalid_arguments
                                            act = ex->if_t100_message~t100key
                                            msg = 'exception key' ).
    ENDTRY.
  ENDMETHOD.

  METHOD missing_ref_name.
    DATA(json) = zcl_mcp_ajson=>create_empty( ).
    json->set_string( iv_path = '/ref/type'
                      iv_val  = zcl_mcp_req_complete=>ref_type-prompt ).
    json->set_string( iv_path = '/argument/name'
                      iv_val  = 'x' ).
    " no /ref/name
    TRY.
        NEW zcl_mcp_req_complete( json ).
        cl_abap_unit_assert=>fail( 'Exception expected for missing ref.name' ).
      CATCH zcx_mcp_server INTO DATA(ex).
        cl_abap_unit_assert=>assert_equals( exp = zcx_mcp_server=>required_params
                                            act = ex->if_t100_message~t100key
                                            msg = 'exception key' ).
    ENDTRY.
  ENDMETHOD.

  METHOD missing_ref_uri.
    DATA(json) = zcl_mcp_ajson=>create_empty( ).
    json->set_string( iv_path = '/ref/type'
                      iv_val  = zcl_mcp_req_complete=>ref_type-resource ).
    json->set_string( iv_path = '/argument/name'
                      iv_val  = 'x' ).
    " no /ref/uri
    TRY.
        NEW zcl_mcp_req_complete( json ).
        cl_abap_unit_assert=>fail( 'Exception expected for missing ref.uri' ).
      CATCH zcx_mcp_server INTO DATA(ex).
        cl_abap_unit_assert=>assert_equals( exp = zcx_mcp_server=>required_params
                                            act = ex->if_t100_message~t100key
                                            msg = 'exception key' ).
    ENDTRY.
  ENDMETHOD.

  METHOD missing_argument_name.
    DATA(json) = zcl_mcp_ajson=>create_empty( ).
    json->set_string( iv_path = '/ref/type'
                      iv_val  = zcl_mcp_req_complete=>ref_type-prompt ).
    json->set_string( iv_path = '/ref/name'
                      iv_val  = 'p' ).
    " no /argument/name
    TRY.
        NEW zcl_mcp_req_complete( json ).
        cl_abap_unit_assert=>fail( 'Exception expected for missing argument.name' ).
      CATCH zcx_mcp_server INTO DATA(ex).
        cl_abap_unit_assert=>assert_equals( exp = zcx_mcp_server=>required_params
                                            act = ex->if_t100_message~t100key
                                            msg = 'exception key' ).
    ENDTRY.
  ENDMETHOD.

  METHOD empty_argument_value.
    DATA(json) = zcl_mcp_ajson=>create_empty( ).
    json->set_string( iv_path = '/ref/type'
                      iv_val  = zcl_mcp_req_complete=>ref_type-prompt ).
    json->set_string( iv_path = '/ref/name'
                      iv_val  = 'p' ).
    json->set_string( iv_path = '/argument/name'
                      iv_val  = 'x' ).
    json->set_string( iv_path = '/argument/value'
                      iv_val  = '' ).

    DATA(req) = NEW zcl_mcp_req_complete( json ).

    cl_abap_unit_assert=>assert_equals( exp = ''
                                        act = req->get_argument_value( ) ).
  ENDMETHOD.

  ENDCLASS.
