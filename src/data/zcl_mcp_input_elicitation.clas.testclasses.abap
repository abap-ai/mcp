CLASS ltcl_mcp_input_elicitation DEFINITION FINAL
  FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.

  PRIVATE SECTION.
    DATA cut TYPE REF TO zcl_mcp_input_elicitation.

    METHODS setup.
    METHODS form_mode_explicit FOR TESTING RAISING zcx_mcp_ajson_error.
    METHODS form_mode_implicit FOR TESTING RAISING zcx_mcp_ajson_error.
    METHODS url_mode           FOR TESTING RAISING zcx_mcp_ajson_error.
    METHODS full_request_json  FOR TESTING RAISING zcx_mcp_ajson_error.
  ENDCLASS.


CLASS ltcl_mcp_input_elicitation IMPLEMENTATION.
  METHOD setup.
    cut = NEW zcl_mcp_input_elicitation( ).
  ENDMETHOD.

  METHOD form_mode_explicit.
    DATA builder TYPE REF TO zcl_mcp_schema_builder.
    DATA params  TYPE REF TO zif_mcp_ajson.

    builder = NEW zcl_mcp_schema_builder( ).
    builder->add_boolean( name        = `approved`
                          description = `Whether the action is approved.`
                          required    = abap_true ).

    cut->set_form( message          = `Confirm the action.`
                   requested_schema = builder->to_json( ) ).

    params = cut->get_params( ).

    cl_abap_unit_assert=>assert_equals( exp = `form`
                                        act = params->get_string( `/mode` ) ).
    cl_abap_unit_assert=>assert_equals( exp = `Confirm the action.`
                                        act = params->get_string( `/message` ) ).
    cl_abap_unit_assert=>assert_equals( exp = `object`
                                        act = params->get_string( `/requestedSchema/type` ) ).
    cl_abap_unit_assert=>assert_equals( exp = `boolean`
                                        act = params->get_string( `/requestedSchema/properties/approved/type` ) ).
    cl_abap_unit_assert=>assert_equals( exp = `approved`
                                        act = params->get_string( `/requestedSchema/required/1` ) ).
  ENDMETHOD.

  METHOD form_mode_implicit.
    DATA builder TYPE REF TO zcl_mcp_schema_builder.
    DATA params  TYPE REF TO zif_mcp_ajson.

    builder = NEW zcl_mcp_schema_builder( ).
    builder->add_string( name     = `comment`
                         required = abap_false ).

    cut->set_form( message          = `Optional comment.`
                   requested_schema = builder->to_json( )
                   include_mode     = abap_false ).

    params = cut->get_params( ).

    cl_abap_unit_assert=>assert_false( act = params->exists( `/mode` ) ).
    cl_abap_unit_assert=>assert_equals( exp = `Optional comment.`
                                        act = params->get_string( `/message` ) ).
    cl_abap_unit_assert=>assert_equals( exp = `string`
                                        act = params->get_string( `/requestedSchema/properties/comment/type` ) ).
  ENDMETHOD.

  METHOD url_mode.
    DATA params TYPE REF TO zif_mcp_ajson.

    cut->set_url( message        = `Authorize access.`
                  url            = `https://example.com/connect?id=1`
                  elicitation_id = `550e8400-e29b-41d4-a716-446655440000` ).

    params = cut->get_params( ).

    cl_abap_unit_assert=>assert_equals( exp = `url`
                                        act = params->get_string( `/mode` ) ).
    cl_abap_unit_assert=>assert_equals( exp = `Authorize access.`
                                        act = params->get_string( `/message` ) ).
    cl_abap_unit_assert=>assert_equals( exp = `https://example.com/connect?id=1`
                                        act = params->get_string( `/url` ) ).
    cl_abap_unit_assert=>assert_equals( exp = `550e8400-e29b-41d4-a716-446655440000`
                                        act = params->get_string( `/elicitationId` ) ).
  ENDMETHOD.

  METHOD full_request_json.
    DATA builder TYPE REF TO zcl_mcp_schema_builder.
    DATA json    TYPE REF TO zif_mcp_ajson.

    builder = NEW zcl_mcp_schema_builder( ).
    builder->add_boolean( name     = `approved`
                          required = abap_true ).

    cut->set_form( message          = `Confirm.`
                   requested_schema = builder->to_json( ) ).

    json = cut->generate_json( ).

    cl_abap_unit_assert=>assert_equals( exp = `elicitation/create`
                                        act = json->get_string( `/method` ) ).
    cl_abap_unit_assert=>assert_equals( exp = `form`
                                        act = json->get_string( `/params/mode` ) ).
    cl_abap_unit_assert=>assert_equals( exp = `Confirm.`
                                        act = json->get_string( `/params/message` ) ).
  ENDMETHOD.
ENDCLASS.
