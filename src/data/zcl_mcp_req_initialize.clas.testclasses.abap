CLASS ltcl_mcp_req_initialize DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    METHODS setup                              RAISING zcx_mcp_ajson_error.
    METHODS test_basic_initialization          FOR TESTING RAISING zcx_mcp_ajson_error.
    METHODS test_client_info_optional_flds   FOR TESTING RAISING zcx_mcp_ajson_error.
    METHODS test_client_info_minimal           FOR TESTING RAISING zcx_mcp_ajson_error.
    METHODS test_with_capabilities             FOR TESTING RAISING zcx_mcp_ajson_error.
    METHODS test_with_complex_capabilities     FOR TESTING RAISING zcx_mcp_ajson_error.
    METHODS test_with_elicitation_cap   FOR TESTING RAISING zcx_mcp_ajson_error.
    METHODS test_with_tasks_capability         FOR TESTING RAISING zcx_mcp_ajson_error.
    METHODS test_with_tasks_and_sampling       FOR TESTING RAISING zcx_mcp_ajson_error.

    DATA json TYPE REF TO zif_mcp_ajson.
ENDCLASS.

CLASS ltcl_mcp_req_initialize IMPLEMENTATION.
  METHOD setup.
    json = zcl_mcp_ajson=>create_empty( ).
    json->set_string( iv_path = '/protocolVersion'
                      iv_val  = '2025-11-25' ).
    json->set_string( iv_path = '/clientInfo/name'
                      iv_val  = 'TestClient' ).
    json->set_string( iv_path = '/clientInfo/title'
                      iv_val  = 'Just a test client' ).
    json->set_string( iv_path = '/clientInfo/version'
                      iv_val  = '1.5.0' ).
  ENDMETHOD.

  METHOD test_basic_initialization.
    DATA(req) = NEW zcl_mcp_req_initialize( json ).

    cl_abap_unit_assert=>assert_equals( exp = '2025-11-25'
                                        act = req->get_protocol_version( ) ).

    DATA(client_info) = req->get_client_info( ).
    cl_abap_unit_assert=>assert_equals( exp = 'TestClient'
                                        act = client_info-name ).
    cl_abap_unit_assert=>assert_equals( exp = '1.5.0'
                                        act = client_info-version ).
    cl_abap_unit_assert=>assert_equals( exp = 'Just a test client'
                                        act = client_info-title ).

    " Optional fields absent - should be initial
    cl_abap_unit_assert=>assert_initial( act = client_info-description
                                         msg = 'description should be initial when absent' ).
    cl_abap_unit_assert=>assert_initial( act = client_info-website_url
                                         msg = 'website_url should be initial when absent' ).

    " All capability flags off
    cl_abap_unit_assert=>assert_false( req->has_roots_capability( ) ).
    cl_abap_unit_assert=>assert_false( req->has_sampling_capability( ) ).
    cl_abap_unit_assert=>assert_false( req->has_elicitation_capability( ) ).
    cl_abap_unit_assert=>assert_false( req->has_tasks_capability( ) ).
    cl_abap_unit_assert=>assert_false( req->has_experimental_capability( ) ).
  ENDMETHOD.

  METHOD test_client_info_optional_flds.
    " description and websiteUrl are new optional fields in MCP 2025-11-25
    json->set_string( iv_path = '/clientInfo/description'
                      iv_val  = 'A test client for unit tests' ).
    json->set_string( iv_path = '/clientInfo/websiteUrl'
                      iv_val  = 'https://example.com/client' ).

    DATA(req) = NEW zcl_mcp_req_initialize( json ).
    DATA(client_info) = req->get_client_info( ).

    cl_abap_unit_assert=>assert_equals( exp = 'A test client for unit tests'
                                        act = client_info-description
                                        msg = 'description should be parsed' ).
    cl_abap_unit_assert=>assert_equals( exp = 'https://example.com/client'
                                        act = client_info-website_url
                                        msg = 'websiteUrl should be parsed' ).

    " Existing fields unaffected
    cl_abap_unit_assert=>assert_equals( exp = 'TestClient'
                                        act = client_info-name ).
    cl_abap_unit_assert=>assert_equals( exp = '1.5.0'
                                        act = client_info-version ).
  ENDMETHOD.

  METHOD test_client_info_minimal.
    " Only name and version are required by the spec; title is optional
    DATA(minimal_json) = zcl_mcp_ajson=>create_empty( ).
    minimal_json->set_string( iv_path = '/protocolVersion'
                              iv_val  = '2025-11-25' ).
    minimal_json->set_string( iv_path = '/clientInfo/name'
                              iv_val  = 'MinimalClient' ).
    minimal_json->set_string( iv_path = '/clientInfo/version'
                              iv_val  = '1.0.0' ).

    DATA(req) = NEW zcl_mcp_req_initialize( minimal_json ).
    DATA(client_info) = req->get_client_info( ).

    cl_abap_unit_assert=>assert_equals( exp = 'MinimalClient'
                                        act = client_info-name ).
    cl_abap_unit_assert=>assert_equals( exp = '1.0.0'
                                        act = client_info-version ).
    cl_abap_unit_assert=>assert_initial( client_info-title ).
    cl_abap_unit_assert=>assert_initial( client_info-description ).
    cl_abap_unit_assert=>assert_initial( client_info-website_url ).
  ENDMETHOD.

  METHOD test_with_capabilities.
    json->set_boolean( iv_path = '/capabilities/roots/listChanged'
                       iv_val  = abap_true ).

    DATA(req) = NEW zcl_mcp_req_initialize( json ).

    cl_abap_unit_assert=>assert_true( req->has_roots_capability( ) ).

    DATA(capabilities) = req->get_capabilities( ).
    cl_abap_unit_assert=>assert_true( capabilities-roots-list_changed ).
  ENDMETHOD.

  METHOD test_with_complex_capabilities.
    json->set_boolean( iv_path = '/capabilities/sampling/temperature'
                       iv_val  = abap_true ).
    json->set_string( iv_path = '/capabilities/experimental/featureX/status'
                      iv_val  = 'active' ).
    json->set_integer( iv_path = '/capabilities/experimental/featureX/priority'
                       iv_val  = '5' ).

    DATA(req) = NEW zcl_mcp_req_initialize( json ).

    cl_abap_unit_assert=>assert_true( req->has_sampling_capability( ) ).
    cl_abap_unit_assert=>assert_true( req->has_experimental_capability( ) ).

    DATA(sampling_json) = req->get_sampling_json( ).
    cl_abap_unit_assert=>assert_not_initial( sampling_json ).
    cl_abap_unit_assert=>assert_true( act = sampling_json->exists( iv_path = '/temperature' ) ).
    cl_abap_unit_assert=>assert_true( act = sampling_json->get_boolean( iv_path = '/temperature' ) ).

    DATA(experimental_json) = req->get_experimental_json( ).
    cl_abap_unit_assert=>assert_not_initial( experimental_json ).
    cl_abap_unit_assert=>assert_equals( exp = 'active'
                                        act = experimental_json->get_string( iv_path = '/featureX/status' ) ).
    cl_abap_unit_assert=>assert_equals( exp = 5
                                        act = experimental_json->get_integer( iv_path = '/featureX/priority' ) ).
  ENDMETHOD.

  METHOD test_with_elicitation_cap.
    " elicitation is new in MCP 2025-11-25; the server does not send
    " elicitation requests but must correctly parse what the client declares
    json->touch_object( '/capabilities/elicitation/form' ).
    json->touch_object( '/capabilities/elicitation/url' ).

    DATA(req) = NEW zcl_mcp_req_initialize( json ).

    cl_abap_unit_assert=>assert_true( act = req->has_elicitation_capability( )
                                      msg = 'has_elicitation_capability should be true' ).

    DATA(elicitation_json) = req->get_elicitation_json( ).
    cl_abap_unit_assert=>assert_not_initial( act = elicitation_json
                                             msg = 'elicitation JSON should not be initial' ).
    cl_abap_unit_assert=>assert_true( act = elicitation_json->exists( '/form' )
                                      msg = 'form sub-object should be present' ).
    cl_abap_unit_assert=>assert_true( act = elicitation_json->exists( '/url' )
                                      msg = 'url sub-object should be present' ).

    " Other capabilities unaffected
    cl_abap_unit_assert=>assert_false( req->has_tasks_capability( ) ).
    cl_abap_unit_assert=>assert_false( req->has_sampling_capability( ) ).
  ENDMETHOD.

  METHOD test_with_tasks_capability.
    " tasks is new in MCP 2025-11-25; needed for task-augmented tool execution
    json->touch_object( '/capabilities/tasks/list' ).
    json->touch_object( '/capabilities/tasks/cancel' ).
    json->touch_object( '/capabilities/tasks/requests/sampling/createMessage' ).

    DATA(req) = NEW zcl_mcp_req_initialize( json ).

    cl_abap_unit_assert=>assert_true( act = req->has_tasks_capability( )
                                      msg = 'has_tasks_capability should be true' ).

    DATA(tasks_json) = req->get_tasks_json( ).
    cl_abap_unit_assert=>assert_not_initial( act = tasks_json
                                             msg = 'tasks JSON should not be initial' ).
    cl_abap_unit_assert=>assert_true( act = tasks_json->exists( '/list' )
                                      msg = 'list sub-object should be present' ).
    cl_abap_unit_assert=>assert_true( act = tasks_json->exists( '/cancel' )
                                      msg = 'cancel sub-object should be present' ).
    cl_abap_unit_assert=>assert_true( act = tasks_json->exists( '/requests/sampling/createMessage' )
                                      msg = 'nested requests path should be present' ).

    " Other capabilities unaffected
    cl_abap_unit_assert=>assert_false( req->has_elicitation_capability( ) ).
    cl_abap_unit_assert=>assert_false( req->has_sampling_capability( ) ).
  ENDMETHOD.

  METHOD test_with_tasks_and_sampling.
    " Realistic client: declares both tasks and sampling together,
    " since task-augmented sampling/createMessage requires both
    json->touch_object( '/capabilities/tasks/list' ).
    json->touch_object( '/capabilities/tasks/requests/sampling/createMessage' ).
    json->touch_object( '/capabilities/sampling/context' ).
    json->touch_object( '/capabilities/sampling/tools' ).

    DATA(req) = NEW zcl_mcp_req_initialize( json ).

    cl_abap_unit_assert=>assert_true( req->has_tasks_capability( ) ).
    cl_abap_unit_assert=>assert_true( req->has_sampling_capability( ) ).

    " Verify tasks JSON carries the nested sampling request declaration
    DATA(tasks_json) = req->get_tasks_json( ).
    cl_abap_unit_assert=>assert_true(
      act = tasks_json->exists( '/requests/sampling/createMessage' )
      msg = 'tasks JSON should carry sampling createMessage declaration' ).

    " Verify sampling JSON carries new 2025-11-25 sub-objects
    DATA(sampling_json) = req->get_sampling_json( ).
    cl_abap_unit_assert=>assert_true( act = sampling_json->exists( '/context' )
                                      msg = 'sampling context sub-object should be present' ).
    cl_abap_unit_assert=>assert_true( act = sampling_json->exists( '/tools' )
                                      msg = 'sampling tools sub-object should be present' ).
  ENDMETHOD.

ENDCLASS.
