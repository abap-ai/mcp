CLASS ltcl_mcp_resp_server_disc DEFINITION DEFERRED.
CLASS zcl_mcp_resp_server_disc DEFINITION LOCAL FRIENDS ltcl_mcp_resp_server_disc.

CLASS ltcl_mcp_resp_server_disc DEFINITION FINAL
FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.

  PRIVATE SECTION.
    DATA cut TYPE REF TO zcl_mcp_resp_server_disc.

    METHODS setup.

    METHODS generate_minimal           FOR TESTING RAISING zcx_mcp_ajson_error.
    METHODS generate_with_capabilities FOR TESTING RAISING zcx_mcp_ajson_error.
    METHODS generate_with_instructions FOR TESTING RAISING zcx_mcp_ajson_error.
    METHODS generate_with_cache        FOR TESTING RAISING zcx_mcp_ajson_error.
    METHODS generate_with_meta         FOR TESTING RAISING zcx_mcp_ajson_error.
    METHODS generate_with_icons        FOR TESTING RAISING zcx_mcp_ajson_error.
    METHODS set_cache_initial_scope    FOR TESTING RAISING zcx_mcp_ajson_error.
  ENDCLASS.

  CLASS ltcl_mcp_resp_server_disc IMPLEMENTATION.
  METHOD setup.
    cut = NEW zcl_mcp_resp_server_disc( ).
  ENDMETHOD.

  METHOD generate_minimal.
    DATA implementation TYPE zcl_mcp_resp_server_disc=>implementation.

    implementation-name    = 'ABAP MCP Server'.
    implementation-version = '1.0.0'.

    cut->set_implementation( implementation ).

    DATA(json) = cut->zif_mcp_modern_result~generate_json( ).

    cl_abap_unit_assert=>assert_equals( exp = zif_mcp_constants=>result_types-complete
                                        act = json->get_string( '/resultType' ) ).

    cl_abap_unit_assert=>assert_equals( exp = zif_mcp_constants=>latest_modern_protocol_version
                                        act = json->get_string( '/supportedVersions/1' ) ).

    cl_abap_unit_assert=>assert_equals( exp = 'ABAP MCP Server'
                                        act = json->get_string( '/serverInfo/name' ) ).

    cl_abap_unit_assert=>assert_equals( exp = '1.0.0'
                                        act = json->get_string( '/serverInfo/version' ) ).

    cl_abap_unit_assert=>assert_equals( exp = 0
                                        act = json->get_integer( '/ttlMs' ) ).

    cl_abap_unit_assert=>assert_equals( exp = zif_mcp_constants=>cache_scopes-private
                                        act = json->get_string( '/cacheScope' ) ).

    cl_abap_unit_assert=>assert_true( act = json->exists( '/capabilities' ) ).
  ENDMETHOD.

  METHOD generate_with_capabilities.
    DATA implementation TYPE zcl_mcp_resp_server_disc=>implementation.
    DATA capabilities   TYPE zcl_mcp_resp_server_disc=>capabilities.

    implementation-name    = 'ABAP MCP Server'.
    implementation-version = '1.0.0'.

    capabilities-prompts       = abap_true.
    capabilities-resources     = abap_true.
    capabilities-tools         = abap_true.
    capabilities-completions   = abap_true.
    capabilities-tasks         = abap_true.

    cut->set_implementation( implementation ).
    cut->set_capabilities( capabilities ).

    DATA(json) = cut->zif_mcp_modern_result~generate_json( ).

    cl_abap_unit_assert=>assert_true( act = json->exists( '/capabilities/prompts' ) ).

    cl_abap_unit_assert=>assert_true( act = json->exists( '/capabilities/resources' ) ).

    cl_abap_unit_assert=>assert_true( act = json->exists( '/capabilities/tools' ) ).

    cl_abap_unit_assert=>assert_true( act = json->exists( '/capabilities/completions' ) ).

    cl_abap_unit_assert=>assert_true( act = json->exists( '/capabilities/extensions/io.modelcontextprotocol~1tasks' ) ).
  ENDMETHOD.

  METHOD generate_with_instructions.
    DATA implementation TYPE zcl_mcp_resp_server_disc=>implementation.

    implementation-name    = 'ABAP MCP Server'.
    implementation-version = '1.0.0'.

    cut->set_implementation( implementation ).
    cut->set_instructions( 'Use this server for ABAP system metadata.' ).

    DATA(json) = cut->zif_mcp_modern_result~generate_json( ).

    cl_abap_unit_assert=>assert_equals( exp = 'Use this server for ABAP system metadata.'
                                        act = json->get_string( '/instructions' ) ).
  ENDMETHOD.

  METHOD generate_with_cache.
    DATA implementation TYPE zcl_mcp_resp_server_disc=>implementation.

    implementation-name    = 'ABAP MCP Server'.
    implementation-version = '1.0.0'.

    cut->set_implementation( implementation ).
    cut->zif_mcp_modern_result~set_cache( ttl_ms      = 3600000
                                          cache_scope = zif_mcp_constants=>cache_scopes-public ).

    DATA(json) = cut->zif_mcp_modern_result~generate_json( ).

    cl_abap_unit_assert=>assert_equals( exp = 3600000
                                        act = json->get_integer( '/ttlMs' ) ).

    cl_abap_unit_assert=>assert_equals( exp = zif_mcp_constants=>cache_scopes-public
                                        act = json->get_string( '/cacheScope' ) ).
  ENDMETHOD.

  METHOD generate_with_meta.
    DATA implementation TYPE zcl_mcp_resp_server_disc=>implementation.
    DATA meta           TYPE REF TO zif_mcp_ajson.

    implementation-name    = 'ABAP MCP Server'.
    implementation-version = '1.0.0'.

    meta = zcl_mcp_ajson=>create_empty( ).
    meta->set_string( iv_path = '/vendor'
                      iv_val  = 'sap' ).

    cut->set_implementation( implementation ).
    cut->zif_mcp_modern_result~set_meta( meta ).

    DATA(json) = cut->zif_mcp_modern_result~generate_json( ).

    cl_abap_unit_assert=>assert_equals( exp = 'sap'
                                        act = json->get_string( '/_meta/vendor' ) ).
  ENDMETHOD.

  METHOD generate_with_icons.
    DATA implementation TYPE zcl_mcp_resp_server_disc=>implementation.
    DATA icon           TYPE zif_mcp_types=>icon.

    implementation-name        = 'ABAP MCP Server'.
    implementation-version     = '1.0.0'.
    implementation-title       = 'ABAP MCP'.
    implementation-description = 'MCP server for ABAP systems'.
    implementation-website_url = 'https://example.invalid/mcp'.

    icon-src       = 'https://example.invalid/icon.png'.
    icon-mime_type = 'image/png'.
    icon-theme     = 'dark'.
    APPEND '64x64' TO icon-sizes.
    APPEND icon TO implementation-icons.

    cut->set_implementation( implementation ).

    DATA(json) = cut->zif_mcp_modern_result~generate_json( ).

    cl_abap_unit_assert=>assert_equals( exp = 'ABAP MCP'
                                        act = json->get_string( '/serverInfo/title' ) ).

    cl_abap_unit_assert=>assert_equals( exp = 'MCP server for ABAP systems'
                                        act = json->get_string( '/serverInfo/description' ) ).

    cl_abap_unit_assert=>assert_equals( exp = 'https://example.invalid/mcp'
                                        act = json->get_string( '/serverInfo/websiteUrl' ) ).

    cl_abap_unit_assert=>assert_equals( exp = 'https://example.invalid/icon.png'
                                        act = json->get_string( '/serverInfo/icons/1/src' ) ).

    cl_abap_unit_assert=>assert_equals( exp = 'image/png'
                                        act = json->get_string( '/serverInfo/icons/1/mimeType' ) ).

    cl_abap_unit_assert=>assert_equals( exp = '64x64'
                                        act = json->get_string( '/serverInfo/icons/1/sizes/1' ) ).

    cl_abap_unit_assert=>assert_equals( exp = 'dark'
                                        act = json->get_string( '/serverInfo/icons/1/theme' ) ).
  ENDMETHOD.

  METHOD set_cache_initial_scope.
    DATA implementation TYPE zcl_mcp_resp_server_disc=>implementation.

    implementation-name    = 'ABAP MCP Server'.
    implementation-version = '1.0.0'.

    cut->set_implementation( implementation ).
    cut->zif_mcp_modern_result~set_cache( ttl_ms      = 1000
                                          cache_scope = `` ).

    DATA(json) = cut->zif_mcp_modern_result~generate_json( ).

    cl_abap_unit_assert=>assert_equals( exp = 1000
                                        act = json->get_integer( '/ttlMs' ) ).

    cl_abap_unit_assert=>assert_equals( exp = zif_mcp_constants=>cache_scopes-private
                                        act = json->get_string( '/cacheScope' ) ).
  ENDMETHOD.
  ENDCLASS.
