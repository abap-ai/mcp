CLASS ltcl_mcp_resp_initialize DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA cut   TYPE REF TO zcl_mcp_resp_initialize.
    DATA ajson TYPE REF TO zif_mcp_ajson.

    METHODS setup.
    METHODS test_empty_json        FOR TESTING RAISING zcx_mcp_ajson_error.
    METHODS test_all_capabilities  FOR TESTING RAISING zcx_mcp_ajson_error.
    METHODS test_some_capabilities FOR TESTING RAISING zcx_mcp_ajson_error.
    METHODS test_implementation    FOR TESTING RAISING zcx_mcp_ajson_error.
    METHODS test_instructions      FOR TESTING RAISING zcx_mcp_ajson_error.
    METHODS test_full_response     FOR TESTING RAISING zcx_mcp_ajson_error.
ENDCLASS.

CLASS ltcl_mcp_resp_initialize IMPLEMENTATION.

  METHOD setup.
    cut = NEW #( ).
    ajson = zcl_mcp_ajson=>create_empty( ).
  ENDMETHOD.

  METHOD test_empty_json.
    " When generating JSON without setting any data
    DATA(ajson) = cut->zif_mcp_internal~generate_json( ).

    " Then server info should be set but empty
    cl_abap_unit_assert=>assert_equals( exp = ''
                                        act = ajson->get_string( 'serverInfo/name' ) ).
    cl_abap_unit_assert=>assert_equals( exp = ''
                                        act = ajson->get_string( 'serverInfo/version' ) ).

    " And no capabilities should be set
    cl_abap_unit_assert=>assert_false( ajson->exists( 'capabilities/prompts/listChanged' ) ).
    cl_abap_unit_assert=>assert_false( ajson->exists( 'capabilities/resources/subscribe' ) ).
    cl_abap_unit_assert=>assert_false( ajson->exists( 'capabilities/resources/listChanged' ) ).
    cl_abap_unit_assert=>assert_false( ajson->exists( 'capabilities/tools/listChanged' ) ).

    " And no instructions should be set
    cl_abap_unit_assert=>assert_false( ajson->exists( 'instructions' ) ).
  ENDMETHOD.

  METHOD test_all_capabilities.
    " Given capabilities with all options enabled
    DATA capabilities TYPE zcl_mcp_resp_initialize=>capabilities.

    capabilities-prompts   = abap_true.
    capabilities-resources = abap_true.
    capabilities-tools     = abap_true.

    " When setting capabilities and generating JSON
    cut->set_capabilities( capabilities ).
    DATA(ajson) = cut->zif_mcp_internal~generate_json( ).

    " Then all capability flags should be present and set to false
    cl_abap_unit_assert=>assert_false( ajson->get_boolean( 'capabilities/prompts/listChanged' ) ).
    cl_abap_unit_assert=>assert_false( ajson->get_boolean( 'capabilities/resources/subscribe' ) ).
    cl_abap_unit_assert=>assert_false( ajson->get_boolean( 'capabilities/resources/listChanged' ) ).
    cl_abap_unit_assert=>assert_false( ajson->get_boolean( 'capabilities/tools/listChanged' ) ).
  ENDMETHOD.

  METHOD test_some_capabilities.
    " Given capabilities with only prompts enabled
    DATA capabilities TYPE zcl_mcp_resp_initialize=>capabilities.

    capabilities-prompts   = abap_true.
    capabilities-resources = abap_false.
    capabilities-tools     = abap_false.

    " When setting capabilities and generating JSON
    cut->set_capabilities( capabilities ).
    DATA(ajson) = cut->zif_mcp_internal~generate_json( ).

    " Then only the prompts capability should be present
    cl_abap_unit_assert=>assert_false( ajson->get_boolean( 'capabilities/prompts/listChanged' ) ).

    " And other capabilities should not exist
    cl_abap_unit_assert=>assert_false( ajson->exists( 'capabilities/resources/subscribe' ) ).
    cl_abap_unit_assert=>assert_false( ajson->exists( 'capabilities/resources/listChanged' ) ).
    cl_abap_unit_assert=>assert_false( ajson->exists( 'capabilities/tools/listChanged' ) ).
  ENDMETHOD.

  METHOD test_implementation.
    " Given implementation details
    DATA implementation TYPE zcl_mcp_resp_initialize=>implementation.

    implementation-name    = 'ABAP MCP Server'.
    implementation-version = '1.0.0'.

    " When setting implementation and generating JSON
    cut->set_implementation( implementation ).
    DATA(ajson) = cut->zif_mcp_internal~generate_json( ).

    " Then serverInfo should contain the implementation details
    cl_abap_unit_assert=>assert_equals( exp = 'ABAP MCP Server'
                                        act = ajson->get_string( 'serverInfo/name' ) ).
    cl_abap_unit_assert=>assert_equals( exp = '1.0.0'
                                        act = ajson->get_string( 'serverInfo/version' ) ).
  ENDMETHOD.

  METHOD test_instructions.
    " Given instructions text
    DATA instructions TYPE zcl_mcp_resp_initialize=>instructions.

    instructions = 'These are special server instructions for the client.'.

    " When setting instructions and generating JSON
    cut->set_instructions( instructions ).
    DATA(ajson) = cut->zif_mcp_internal~generate_json( ).

    " Then instructions should be included in the JSON
    cl_abap_unit_assert=>assert_equals( exp = 'These are special server instructions for the client.'
                                        act = ajson->get_string( 'instructions' ) ).
  ENDMETHOD.

  METHOD test_full_response.
    " Given all components set
    DATA capabilities   TYPE zcl_mcp_resp_initialize=>capabilities.
    DATA implementation TYPE zcl_mcp_resp_initialize=>implementation.
    DATA instructions   TYPE zcl_mcp_resp_initialize=>instructions.

    " Set all capabilities
    capabilities-prompts   = abap_true.
    capabilities-resources = abap_true.
    capabilities-tools     = abap_true.

    " Set implementation details
    implementation-name    = 'ABAP MCP Server'.
    implementation-version = '1.0.0'.

    " Set instructions
    instructions = 'These are special server instructions for the client.'.

    " When setting all and generating JSON
    cut->set_capabilities( capabilities ).
    cut->set_implementation( implementation ).
    cut->set_instructions( instructions ).
    DATA(ajson) = cut->zif_mcp_internal~generate_json( ).

    " Then all components should be present in the JSON
    " Capabilities
    cl_abap_unit_assert=>assert_false( ajson->get_boolean( 'capabilities/prompts/listChanged' ) ).
    cl_abap_unit_assert=>assert_false( ajson->get_boolean( 'capabilities/resources/subscribe' ) ).
    cl_abap_unit_assert=>assert_false( ajson->get_boolean( 'capabilities/resources/listChanged' ) ).
    cl_abap_unit_assert=>assert_false( ajson->get_boolean( 'capabilities/tools/listChanged' ) ).

    " Implementation
    cl_abap_unit_assert=>assert_equals( exp = 'ABAP MCP Server'
                                        act = ajson->get_string( 'serverInfo/name' ) ).
    cl_abap_unit_assert=>assert_equals( exp = '1.0.0'
                                        act = ajson->get_string( 'serverInfo/version' ) ).

    " Instructions
    cl_abap_unit_assert=>assert_equals( exp = 'These are special server instructions for the client.'
                                        act = ajson->get_string( 'instructions' ) ).
  ENDMETHOD.

ENDCLASS.
