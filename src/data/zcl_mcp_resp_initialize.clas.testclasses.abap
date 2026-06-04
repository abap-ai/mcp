CLASS ltcl_mcp_resp_initialize DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA cut TYPE REF TO zcl_mcp_resp_initialize.

    METHODS setup.
    METHODS test_empty_json            FOR TESTING RAISING zcx_mcp_ajson_error.
    METHODS test_capabilities_enabled  FOR TESTING RAISING zcx_mcp_ajson_error.
    METHODS test_capabilities_flags    FOR TESTING RAISING zcx_mcp_ajson_error.
    METHODS test_some_capabilities     FOR TESTING RAISING zcx_mcp_ajson_error.
    METHODS test_tasks_capability      FOR TESTING RAISING zcx_mcp_ajson_error.
    METHODS test_logging_completions   FOR TESTING RAISING zcx_mcp_ajson_error.
    METHODS test_implementation        FOR TESTING RAISING zcx_mcp_ajson_error.
    METHODS test_impl_optional_fields  FOR TESTING RAISING zcx_mcp_ajson_error.
    METHODS test_impl_icons            FOR TESTING RAISING zcx_mcp_ajson_error.
    METHODS test_instructions          FOR TESTING RAISING zcx_mcp_ajson_error.
    METHODS test_full_response         FOR TESTING RAISING zcx_mcp_ajson_error.
ENDCLASS.

CLASS ltcl_mcp_resp_initialize IMPLEMENTATION.

  METHOD setup.
    cut = NEW #( ).
  ENDMETHOD.

  METHOD test_empty_json.
    DATA(json) = cut->zif_mcp_internal~generate_json( ).

    " name/version are always emitted (mandatory fields)
    cl_abap_unit_assert=>assert_equals( exp = ''
                                        act = json->get_string( 'serverInfo/name' ) ).
    cl_abap_unit_assert=>assert_equals( exp = ''
                                        act = json->get_string( 'serverInfo/version' ) ).

    " No capability sub-objects when nothing is enabled
    cl_abap_unit_assert=>assert_false( act = json->exists( 'capabilities/prompts' )
                                       msg = 'prompts should be absent' ).
    cl_abap_unit_assert=>assert_false( act = json->exists( 'capabilities/resources' )
                                       msg = 'resources should be absent' ).
    cl_abap_unit_assert=>assert_false( act = json->exists( 'capabilities/tools' )
                                       msg = 'tools should be absent' ).
    cl_abap_unit_assert=>assert_false( act = json->exists( 'capabilities/tasks' )
                                       msg = 'tasks should be absent' ).
    cl_abap_unit_assert=>assert_false( act = json->exists( 'capabilities/logging' )
                                       msg = 'logging should be absent' ).
    cl_abap_unit_assert=>assert_false( act = json->exists( 'capabilities/completions' )
                                       msg = 'completions should be absent' ).
    cl_abap_unit_assert=>assert_false( act = json->exists( 'instructions' )
                                       msg = 'instructions should be absent' ).
  ENDMETHOD.

  METHOD test_capabilities_enabled.
    " Capabilities enabled but no sub-flags - each block
    " appears as an empty object {} in the JSON
    DATA capabilities TYPE zcl_mcp_resp_initialize=>capabilities.
    capabilities-prompts-enabled   = abap_true.
    capabilities-resources-enabled = abap_true.
    capabilities-tools-enabled     = abap_true.

    cut->set_capabilities( capabilities ).
    DATA(json) = cut->zif_mcp_internal~generate_json( ).

    cl_abap_unit_assert=>assert_true( act = json->exists( 'capabilities/prompts' )
                                      msg = 'prompts object should exist' ).
    cl_abap_unit_assert=>assert_true( act = json->exists( 'capabilities/resources' )
                                      msg = 'resources object should exist' ).
    cl_abap_unit_assert=>assert_true( act = json->exists( 'capabilities/tools' )
                                      msg = 'tools object should exist' ).

    " Sub-flags must be absent when not set
    cl_abap_unit_assert=>assert_false( act = json->exists( 'capabilities/prompts/listChanged' )
                                       msg = 'listChanged should be absent' ).
    cl_abap_unit_assert=>assert_false( act = json->exists( 'capabilities/resources/subscribe' )
                                       msg = 'subscribe should be absent' ).
    cl_abap_unit_assert=>assert_false( act = json->exists( 'capabilities/resources/listChanged' )
                                       msg = 'resources listChanged should be absent' ).
    cl_abap_unit_assert=>assert_false( act = json->exists( 'capabilities/tools/listChanged' )
                                       msg = 'tools listChanged should be absent' ).
  ENDMETHOD.

  METHOD test_capabilities_flags.
    " Notification-backed sub-flags must be suppressed in ABAP HTTP transport,
    " even when explicitly set, because SSE/server-to-client notifications are unsupported.
    DATA capabilities TYPE zcl_mcp_resp_initialize=>capabilities.

    capabilities-prompts-enabled      = abap_true.
    capabilities-prompts-list_changed = abap_true.
    capabilities-resources-enabled      = abap_true.
    capabilities-resources-subscribe    = abap_true.
    capabilities-resources-list_changed = abap_true.
    capabilities-tools-enabled      = abap_true.
    capabilities-tools-list_changed = abap_true.

    cut->set_capabilities( capabilities ).
    DATA(json) = cut->zif_mcp_internal~generate_json( ).

    cl_abap_unit_assert=>assert_true( act = json->exists( 'capabilities/prompts' )
                                      msg = 'prompts object should still exist' ).
    cl_abap_unit_assert=>assert_true( act = json->exists( 'capabilities/resources' )
                                      msg = 'resources object should still exist' ).
    cl_abap_unit_assert=>assert_true( act = json->exists( 'capabilities/tools' )
                                      msg = 'tools object should still exist' ).

    cl_abap_unit_assert=>assert_false( act = json->exists( 'capabilities/prompts/listChanged' )
                                       msg = 'prompts listChanged must be suppressed' ).
    cl_abap_unit_assert=>assert_false( act = json->exists( 'capabilities/resources/subscribe' )
                                       msg = 'resources subscribe must be suppressed' ).
    cl_abap_unit_assert=>assert_false( act = json->exists( 'capabilities/resources/listChanged' )
                                       msg = 'resources listChanged must be suppressed' ).
    cl_abap_unit_assert=>assert_false( act = json->exists( 'capabilities/tools/listChanged' )
                                       msg = 'tools listChanged must be suppressed' ).
  ENDMETHOD.

  METHOD test_some_capabilities.
    " Only prompts enabled - resources and tools must be absent
    DATA capabilities TYPE zcl_mcp_resp_initialize=>capabilities.
    capabilities-prompts-enabled = abap_true.

    cut->set_capabilities( capabilities ).
    DATA(json) = cut->zif_mcp_internal~generate_json( ).

    cl_abap_unit_assert=>assert_true( act = json->exists( 'capabilities/prompts' )
                                      msg = 'prompts should exist' ).
    cl_abap_unit_assert=>assert_false( act = json->exists( 'capabilities/resources' )
                                       msg = 'resources should be absent' ).
    cl_abap_unit_assert=>assert_false( act = json->exists( 'capabilities/tools' )
                                       msg = 'tools should be absent' ).
  ENDMETHOD.

  METHOD test_tasks_capability.
    " tasks is new in MCP 2025-11-25 - tools_call maps to
    " tasks/requests/tools/call: {} which tells clients this
    " server accepts task: {...} on tools/call requests
    DATA capabilities TYPE zcl_mcp_resp_initialize=>capabilities.
    capabilities-tasks-list       = abap_true.
    capabilities-tasks-cancel     = abap_true.
    capabilities-tasks-tools_call = abap_true.

    cut->set_capabilities( capabilities ).
    DATA(json) = cut->zif_mcp_internal~generate_json( ).

    cl_abap_unit_assert=>assert_true( act = json->exists( 'capabilities/tasks' )
                                      msg = 'tasks object should exist' ).
    cl_abap_unit_assert=>assert_true( act = json->exists( 'capabilities/tasks/list' )
                                      msg = 'tasks/list should exist' ).
    cl_abap_unit_assert=>assert_true( act = json->exists( 'capabilities/tasks/cancel' )
                                      msg = 'tasks/cancel should exist' ).
    cl_abap_unit_assert=>assert_true(
      act = json->exists( 'capabilities/tasks/requests/tools/call' )
      msg = 'tasks/requests/tools/call should exist' ).

    " tasks must be absent when no task flags are set
    DATA capabilities2 TYPE zcl_mcp_resp_initialize=>capabilities.
    capabilities2-tools-enabled = abap_true.
    cut->set_capabilities( capabilities2 ).
    DATA(json2) = cut->zif_mcp_internal~generate_json( ).
    cl_abap_unit_assert=>assert_false( act = json2->exists( 'capabilities/tasks' )
                                       msg = 'tasks should be absent when not set' ).
  ENDMETHOD.

  METHOD test_logging_completions.
    " logging and completions are suppressed:
    " - logging notifications cannot be delivered without SSE/server-to-client transport
    DATA capabilities TYPE zcl_mcp_resp_initialize=>capabilities.

    capabilities-logging     = abap_true.
    capabilities-completions = abap_true.

    cut->set_capabilities( capabilities ).
    DATA(json) = cut->zif_mcp_internal~generate_json( ).

    cl_abap_unit_assert=>assert_false( act = json->exists( 'capabilities/logging' )
                                       msg = 'logging must be suppressed' ).

    cl_abap_unit_assert=>assert_true( act = json->exists( 'capabilities/completions' )
                                      msg = 'completions must be suppressed' ).
  ENDMETHOD.

  METHOD test_implementation.
    DATA implementation TYPE zcl_mcp_resp_initialize=>implementation.
    implementation-name    = 'ABAP MCP Server'.
    implementation-version = '1.0.0'.

    cut->set_implementation( implementation ).
    DATA(json) = cut->zif_mcp_internal~generate_json( ).

    cl_abap_unit_assert=>assert_equals( exp = 'ABAP MCP Server'
                                        act = json->get_string( 'serverInfo/name' ) ).
    cl_abap_unit_assert=>assert_equals( exp = '1.0.0'
                                        act = json->get_string( 'serverInfo/version' ) ).

    " Optional fields absent when not set
    cl_abap_unit_assert=>assert_false( act = json->exists( 'serverInfo/title' )
                                       msg = 'title should be absent' ).
    cl_abap_unit_assert=>assert_false( act = json->exists( 'serverInfo/description' )
                                       msg = 'description should be absent' ).
    cl_abap_unit_assert=>assert_false( act = json->exists( 'serverInfo/websiteUrl' )
                                       msg = 'websiteUrl should be absent' ).
    cl_abap_unit_assert=>assert_false( act = json->exists( 'serverInfo/icons' )
                                       msg = 'icons should be absent' ).
  ENDMETHOD.

  METHOD test_impl_optional_fields.
    " title, description, websiteUrl are new in MCP 2025-11-25
    DATA implementation TYPE zcl_mcp_resp_initialize=>implementation.
    implementation-name        = 'ABAP MCP Server'.
    implementation-version     = '1.0.0'.
    implementation-title       = 'ABAP MCP Server for SAP'.
    implementation-description = 'Exposes SAP ABAP functionality via MCP'.
    implementation-website_url = 'https://example.com/abap-mcp'.

    cut->set_implementation( implementation ).
    DATA(json) = cut->zif_mcp_internal~generate_json( ).

    cl_abap_unit_assert=>assert_equals( exp = 'ABAP MCP Server for SAP'
                                        act = json->get_string( 'serverInfo/title' ) ).
    cl_abap_unit_assert=>assert_equals( exp = 'Exposes SAP ABAP functionality via MCP'
                                        act = json->get_string( 'serverInfo/description' ) ).
    cl_abap_unit_assert=>assert_equals( exp = 'https://example.com/abap-mcp'
                                        act = json->get_string( 'serverInfo/websiteUrl' ) ).
  ENDMETHOD.

  METHOD test_impl_icons.
    " icons is new in MCP 2025-11-25
    DATA implementation TYPE zcl_mcp_resp_initialize=>implementation.
    implementation-name    = 'ABAP MCP Server'.
    implementation-version = '1.0.0'.

    DATA icon1 TYPE zif_mcp_types=>icon.
    icon1-src       = 'https://example.com/icon-48.png'.
    icon1-mime_type = 'image/png'.
    icon1-theme     = 'light'.
    APPEND '48x48' TO icon1-sizes.
    APPEND icon1 TO implementation-icons.

    DATA icon2 TYPE zif_mcp_types=>icon.
    icon2-src = 'https://example.com/icon.svg'.
    APPEND 'any' TO icon2-sizes.
    APPEND icon2 TO implementation-icons.

    cut->set_implementation( implementation ).
    DATA(json) = cut->zif_mcp_internal~generate_json( ).

    cl_abap_unit_assert=>assert_true( act = json->exists( 'serverInfo/icons' )
                                      msg = 'icons array should exist' ).
    cl_abap_unit_assert=>assert_equals( exp = 'https://example.com/icon-48.png'
                                        act = json->get_string( 'serverInfo/icons/1/src' ) ).
    cl_abap_unit_assert=>assert_equals( exp = 'image/png'
                                        act = json->get_string( 'serverInfo/icons/1/mimeType' ) ).
    cl_abap_unit_assert=>assert_equals( exp = 'light'
                                        act = json->get_string( 'serverInfo/icons/1/theme' ) ).
    cl_abap_unit_assert=>assert_equals( exp = '48x48'
                                        act = json->get_string( 'serverInfo/icons/1/sizes/1' ) ).
    cl_abap_unit_assert=>assert_equals( exp = 'https://example.com/icon.svg'
                                        act = json->get_string( 'serverInfo/icons/2/src' ) ).

    " mimeType and theme absent on second icon
    cl_abap_unit_assert=>assert_false( act = json->exists( 'serverInfo/icons/2/mimeType' )
                                       msg = 'mimeType should be absent when not set' ).
    cl_abap_unit_assert=>assert_false( act = json->exists( 'serverInfo/icons/2/theme' )
                                       msg = 'theme should be absent when not set' ).
  ENDMETHOD.

  METHOD test_instructions.
    cut->set_instructions( 'These are special server instructions for the client.' ).
    DATA(json) = cut->zif_mcp_internal~generate_json( ).

    cl_abap_unit_assert=>assert_equals(
      exp = 'These are special server instructions for the client.'
      act = json->get_string( 'instructions' ) ).
  ENDMETHOD.

  METHOD test_full_response.
    DATA capabilities   TYPE zcl_mcp_resp_initialize=>capabilities.
    DATA implementation TYPE zcl_mcp_resp_initialize=>implementation.

    capabilities-prompts-enabled      = abap_true.
    capabilities-prompts-list_changed = abap_true.
    capabilities-resources-enabled    = abap_true.
    capabilities-tools-enabled        = abap_true.
    capabilities-tasks-tools_call     = abap_true.
    capabilities-logging              = abap_true.

    implementation-name    = 'ABAP MCP Server'.
    implementation-version = '1.0.0'.
    implementation-title   = 'ABAP MCP Server for SAP'.

    cut->set_capabilities( capabilities ).
    cut->set_implementation( implementation ).
    cut->set_instructions( 'Use this server to access SAP data.' ).

    DATA(json) = cut->zif_mcp_internal~generate_json( ).

    cl_abap_unit_assert=>assert_false( act = json->exists( 'capabilities/prompts/listChanged' )
                                       msg = 'prompts listChanged must be suppressed' ).
    cl_abap_unit_assert=>assert_true( json->exists( 'capabilities/resources' ) ).
    cl_abap_unit_assert=>assert_true( json->exists( 'capabilities/tools' ) ).
    cl_abap_unit_assert=>assert_true( json->exists( 'capabilities/tasks/requests/tools/call' ) ).
    cl_abap_unit_assert=>assert_false( act = json->exists( 'capabilities/logging' )
                                       msg = 'logging must be suppressed' ).
    cl_abap_unit_assert=>assert_equals( exp = 'ABAP MCP Server'
                                        act = json->get_string( 'serverInfo/name' ) ).
    cl_abap_unit_assert=>assert_equals( exp = 'ABAP MCP Server for SAP'
                                        act = json->get_string( 'serverInfo/title' ) ).
    cl_abap_unit_assert=>assert_equals( exp = 'Use this server to access SAP data.'
                                        act = json->get_string( 'instructions' ) ).
  ENDMETHOD.

ENDCLASS.
