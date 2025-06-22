CLASS ltcl_mcp_req_initialize DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    METHODS setup                          RAISING zcx_mcp_ajson_error.
    METHODS test_basic_initialization      FOR TESTING RAISING zcx_mcp_ajson_error.
    METHODS test_with_capabilities         FOR TESTING RAISING zcx_mcp_ajson_error.
    METHODS test_with_complex_capabilities FOR TESTING RAISING zcx_mcp_ajson_error.

    DATA json TYPE REF TO zif_mcp_ajson.
ENDCLASS.

CLASS ltcl_mcp_req_initialize IMPLEMENTATION.
  METHOD setup.
    " Create base JSON structure - now starting at params level
    json = zcl_mcp_ajson=>create_empty( ).
    json->set_string( iv_path = '/protocolVersion'
                      iv_val  = '1.0' ).
    json->set_string( iv_path = '/clientInfo/name'
                      iv_val  = 'TestClient' ).
    json->set_string( iv_path = '/clientInfo/title'
                      iv_val  = 'Just a test client' ).
    json->set_string( iv_path = '/clientInfo/version'
                      iv_val  = '1.5.0' ).
  ENDMETHOD.

  METHOD test_basic_initialization.
    " When
    DATA req TYPE REF TO zcl_mcp_req_initialize.

    req = NEW #( json ).

    " Then
    cl_abap_unit_assert=>assert_equals( exp = '1.0'
                                        act = req->get_protocol_version( ) ).

    DATA client_info TYPE zcl_mcp_req_initialize=>implementation.
    client_info = req->get_client_info( ).

    cl_abap_unit_assert=>assert_equals( exp = 'TestClient'
                                        act = client_info-name ).

    cl_abap_unit_assert=>assert_equals( exp = '1.5.0'
                                        act = client_info-version ).
    cl_abap_unit_assert=>assert_equals( exp = 'Just a test client'
                                        act = client_info-title ).

    cl_abap_unit_assert=>assert_false( req->has_roots_capability( ) ).
    cl_abap_unit_assert=>assert_false( req->has_sampling_capability( ) ).
    cl_abap_unit_assert=>assert_false( req->has_experimental_capability( ) ).
  ENDMETHOD.

  METHOD test_with_capabilities.
    " Given
    json->set_boolean( iv_path = '/capabilities/roots/listChanged'
                       iv_val  = abap_true ).

    " When
    DATA req TYPE REF TO zcl_mcp_req_initialize.
    req = NEW #( json ).

    " Then
    cl_abap_unit_assert=>assert_true( req->has_roots_capability( ) ).

    DATA capabilities TYPE zcl_mcp_req_initialize=>client_capabilities.
    capabilities = req->get_capabilities( ).

    cl_abap_unit_assert=>assert_true( capabilities-roots-list_changed ).
  ENDMETHOD.

  METHOD test_with_complex_capabilities.
    " Given - Setup complex JSON structure
    json->set_boolean( iv_path = '/capabilities/sampling/temperature'
                       iv_val  = abap_true ).
    json->set_string( iv_path = '/capabilities/experimental/featureX/status'
                      iv_val  = 'active' ).
    json->set_integer( iv_path = '/capabilities/experimental/featureX/priority'
                       iv_val  = '5' ).

    " When
    DATA req TYPE REF TO zcl_mcp_req_initialize.
    req = NEW #( json ).

    " Then - Check if capabilities exist
    cl_abap_unit_assert=>assert_true( req->has_sampling_capability( ) ).
    cl_abap_unit_assert=>assert_true( req->has_experimental_capability( ) ).

    " Check sampling JSON content
    DATA sampling_json TYPE REF TO zif_mcp_ajson.
    sampling_json = req->get_sampling_json( ).

    cl_abap_unit_assert=>assert_not_initial( sampling_json ).
    cl_abap_unit_assert=>assert_true( sampling_json->exists( iv_path = '/temperature' ) ).
    cl_abap_unit_assert=>assert_true( sampling_json->get_boolean( iv_path = '/temperature' ) ).

    " Check experimental JSON content
    DATA experimental_json TYPE REF TO zif_mcp_ajson.
    experimental_json = req->get_experimental_json( ).

    cl_abap_unit_assert=>assert_not_initial( experimental_json ).
    cl_abap_unit_assert=>assert_true( experimental_json->exists( iv_path = '/featureX/status' ) ).
    cl_abap_unit_assert=>assert_equals( exp = 'active'
                                        act = experimental_json->get_string( iv_path = '/featureX/status' ) ).
    cl_abap_unit_assert=>assert_equals( exp = 5
                                        act = experimental_json->get_integer( iv_path = '/featureX/priority' ) ).
  ENDMETHOD.

ENDCLASS.
