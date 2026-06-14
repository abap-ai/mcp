CLASS ltcl_mcp_modern_context DEFINITION DEFERRED.
CLASS zcl_mcp_modern_context DEFINITION LOCAL FRIENDS ltcl_mcp_modern_context.

CLASS ltcl_mcp_modern_context DEFINITION FINAL
  FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.

  PRIVATE SECTION.
    METHODS create_request
      RETURNING VALUE(result) TYPE zcl_mcp_jsonrpc=>request
      RAISING   zcx_mcp_ajson_error.

    METHODS create_request_no_meta
      RETURNING VALUE(result) TYPE zcl_mcp_jsonrpc=>request.

    METHODS create_request_version
      IMPORTING protocol_ver  TYPE string
      RETURNING VALUE(result) TYPE zcl_mcp_jsonrpc=>request
      RAISING   zcx_mcp_ajson_error.

    METHODS is_modern_by_method       FOR TESTING.
    METHODS is_modern_by_meta         FOR TESTING RAISING zcx_mcp_ajson_error.
    METHODS not_modern_without_meta   FOR TESTING.
    METHODS supported_version         FOR TESTING.
    METHODS build_context_full_meta   FOR TESTING RAISING zcx_mcp_ajson_error zcx_mcp_server.
    METHODS build_context_no_meta     FOR TESTING.
    METHODS unsupported_version       FOR TESTING RAISING zcx_mcp_ajson_error.
    METHODS error_mapping_required    FOR TESTING.
    METHODS error_mapping_bad_version FOR TESTING RAISING zcx_mcp_ajson_error.
ENDCLASS.

CLASS ltcl_mcp_modern_context IMPLEMENTATION.
  METHOD create_request.
    result-jsonrpc    = zcl_mcp_jsonrpc=>jsonrpc_version.
    result-method     = 'tools/list'.
    result-id         = '1'.
    result-id_present = abap_true.
    result-params     = zcl_mcp_ajson=>create_empty( ).

    result-params->touch_object( '/_meta' ).

    result-params->set_string( iv_path = zif_mcp_constants=>meta_paths-protocol_version
                               iv_val  = zif_mcp_constants=>latest_modern_protocol_version ).

    result-params->touch_object( zif_mcp_constants=>meta_paths-client_info ).

    result-params->set_string( iv_path = |{ zif_mcp_constants=>meta_paths-client_info }/name|
                               iv_val  = 'UnitTestClient' ).

    result-params->set_string( iv_path = |{ zif_mcp_constants=>meta_paths-client_info }/version|
                               iv_val  = '1.0.0' ).

    result-params->touch_object( zif_mcp_constants=>meta_paths-client_capabilities ).

    result-params->touch_object( |{ zif_mcp_constants=>meta_paths-client_capabilities }/extensions| ).

    result-params->touch_object(
        |{ zif_mcp_constants=>meta_paths-client_capabilities }/extensions/io.modelcontextprotocol~1tasks| ).

    result-params->set_string( iv_path = zif_mcp_constants=>meta_paths-log_level
                               iv_val  = 'debug' ).

    result-params->set_string( iv_path = zif_mcp_constants=>meta_paths-traceparent
                               iv_val  = '00-4bf92f3577b34da6a3ce929d0e0e4736-00f067aa0ba902b7-00' ).

    result-params->set_string( iv_path = zif_mcp_constants=>meta_paths-tracestate
                               iv_val  = 'vendor=value' ).

    result-params->set_string( iv_path = zif_mcp_constants=>meta_paths-baggage
                               iv_val  = 'userId=abc' ).
  ENDMETHOD.

  METHOD create_request_no_meta.
    result-jsonrpc    = zcl_mcp_jsonrpc=>jsonrpc_version.
    result-method     = 'tools/list'.
    result-id         = '1'.
    result-id_present = abap_true.
    result-params     = zcl_mcp_ajson=>create_empty( ).
  ENDMETHOD.

  METHOD create_request_version.
    result = create_request( ).
    result-params->set_string( iv_path = zif_mcp_constants=>meta_paths-protocol_version
                               iv_val  = protocol_ver ).
  ENDMETHOD.

  METHOD is_modern_by_method.
    DATA http_request TYPE REF TO if_http_request.

    DATA(request) = create_request_no_meta( ).
    request-method = 'server/discover'.

    DATA(is_modern) = zcl_mcp_modern_context=>is_modern_request( request      = request
                                                                 http_request = http_request ).

    cl_abap_unit_assert=>assert_true( act = is_modern ).
  ENDMETHOD.

  METHOD is_modern_by_meta.
    DATA http_request TYPE REF TO if_http_request.

    DATA(request) = create_request( ).

    DATA(is_modern) = zcl_mcp_modern_context=>is_modern_request( request      = request
                                                                 http_request = http_request ).

    cl_abap_unit_assert=>assert_true( act = is_modern ).
  ENDMETHOD.

  METHOD not_modern_without_meta.
    DATA http_request TYPE REF TO if_http_request.

    DATA(request) = create_request_no_meta( ).

    DATA(is_modern) = zcl_mcp_modern_context=>is_modern_request( request      = request
                                                                 http_request = http_request ).

    cl_abap_unit_assert=>assert_false( act = is_modern ).
  ENDMETHOD.

  METHOD supported_version.
    cl_abap_unit_assert=>assert_true( act = zcl_mcp_modern_context=>is_supported_version(
                                                zif_mcp_constants=>latest_modern_protocol_version ) ).

    cl_abap_unit_assert=>assert_false( act = zcl_mcp_modern_context=>is_supported_version(
                                                 zif_mcp_constants=>protocol_version_2025_11_25 ) ).
  ENDMETHOD.

  METHOD build_context_full_meta.
    DATA http_request  TYPE REF TO if_http_request.
    DATA http_response TYPE REF TO if_http_response.
    DATA http_server   TYPE REF TO if_http_server.

    DATA(request) = create_request( ).

    DATA(context) = zcl_mcp_modern_context=>build_context( area          = 'AREA'
                                                           mcp_server    = 'SERVER'
                                                           request       = request
                                                           http_request  = http_request
                                                           http_response = http_response
                                                           http_server   = http_server
                                                           cors_mode     = zcl_mcp_configuration=>cors_mode_ignore ).

    cl_abap_unit_assert=>assert_equals( exp = 'AREA'
                                        act = context-area ).

    cl_abap_unit_assert=>assert_equals( exp = 'SERVER'
                                        act = context-server ).

    cl_abap_unit_assert=>assert_equals( exp = zif_mcp_constants=>latest_modern_protocol_version
                                        act = context-protocol_ver ).

    cl_abap_unit_assert=>assert_bound( act = context-meta ).
    cl_abap_unit_assert=>assert_bound( act = context-client_info ).
    cl_abap_unit_assert=>assert_bound( act = context-client_caps ).
    cl_abap_unit_assert=>assert_bound( act = context-extensions ).

    cl_abap_unit_assert=>assert_equals( exp = 'UnitTestClient'
                                        act = context-client_info->get_string( '/name' ) ).

    cl_abap_unit_assert=>assert_equals( exp = '1.0.0'
                                        act = context-client_info->get_string( '/version' ) ).

    cl_abap_unit_assert=>assert_true( act = context-extensions->exists( '/io.modelcontextprotocol~1tasks' ) ).

    cl_abap_unit_assert=>assert_equals( exp = 'debug'
                                        act = context-log_level ).

    cl_abap_unit_assert=>assert_equals( exp = '00-4bf92f3577b34da6a3ce929d0e0e4736-00f067aa0ba902b7-00'
                                        act = context-traceparent ).

    cl_abap_unit_assert=>assert_equals( exp = 'vendor=value'
                                        act = context-tracestate ).

    cl_abap_unit_assert=>assert_equals( exp = 'userId=abc'
                                        act = context-baggage ).
  ENDMETHOD.

  METHOD build_context_no_meta.
    DATA http_request  TYPE REF TO if_http_request.
    DATA http_response TYPE REF TO if_http_response.
    DATA http_server   TYPE REF TO if_http_server.

    DATA(request) = create_request_no_meta( ).

    TRY.
        zcl_mcp_modern_context=>build_context( area          = 'AREA'
                                               mcp_server    = 'SERVER'
                                               request       = request
                                               http_request  = http_request
                                               http_response = http_response
                                               http_server   = http_server
                                               cors_mode     = zcl_mcp_configuration=>cors_mode_ignore ).

        cl_abap_unit_assert=>fail( 'Expected zcx_mcp_server for missing protocol _meta' ).

      CATCH zcx_mcp_server INTO DATA(error).
        cl_abap_unit_assert=>assert_equals( exp = zcx_mcp_server=>required_params
                                            act = error->if_t100_message~t100key ).

        DATA(mapped) = zcl_mcp_modern_context=>error_from_exception( error ).

        cl_abap_unit_assert=>assert_equals( exp = zcl_mcp_jsonrpc=>error_codes-invalid_params
                                            act = mapped-code ).
    ENDTRY.
  ENDMETHOD.

  METHOD unsupported_version.
    DATA http_request  TYPE REF TO if_http_request.
    DATA http_response TYPE REF TO if_http_response.
    DATA http_server   TYPE REF TO if_http_server.

    DATA(request) = create_request_version( '2099-01-01' ).

    TRY.
        zcl_mcp_modern_context=>build_context( area          = 'AREA'
                                               mcp_server    = 'SERVER'
                                               request       = request
                                               http_request  = http_request
                                               http_response = http_response
                                               http_server   = http_server
                                               cors_mode     = zcl_mcp_configuration=>cors_mode_ignore ).

        cl_abap_unit_assert=>fail( 'Expected zcx_mcp_server for unsupported version' ).

      CATCH zcx_mcp_server INTO DATA(error).
        DATA(mapped) = zcl_mcp_modern_context=>error_from_exception( error ).

        cl_abap_unit_assert=>assert_equals( exp = zcl_mcp_jsonrpc=>error_codes-unsupported_protocol_version
                                            act = mapped-code ).
    ENDTRY.
  ENDMETHOD.

  METHOD error_mapping_required.
    DATA(error) = NEW zcx_mcp_server( textid = zcx_mcp_server=>required_params
                                      msgv1  = 'io.modelcontextprotocol/protocolVersion' ).

    DATA(mapped) = zcl_mcp_modern_context=>error_from_exception( error ).

    cl_abap_unit_assert=>assert_equals( exp = zcl_mcp_jsonrpc=>error_codes-invalid_params
                                        act = mapped-code ).
  ENDMETHOD.

  METHOD error_mapping_bad_version.
    DATA http_request  TYPE REF TO if_http_request.
    DATA http_response TYPE REF TO if_http_response.
    DATA http_server   TYPE REF TO if_http_server.

    DATA(request) = create_request_version( '2099-01-01' ).

    TRY.
        zcl_mcp_modern_context=>build_context( area          = 'AREA'
                                               mcp_server    = 'SERVER'
                                               request       = request
                                               http_request  = http_request
                                               http_response = http_response
                                               http_server   = http_server
                                               cors_mode     = zcl_mcp_configuration=>cors_mode_ignore ).

        cl_abap_unit_assert=>fail( 'Expected zcx_mcp_server for unsupported version' ).

      CATCH zcx_mcp_server INTO DATA(error).
        DATA(mapped) = zcl_mcp_modern_context=>error_from_exception( error ).

        cl_abap_unit_assert=>assert_equals( exp = zcl_mcp_jsonrpc=>error_codes-unsupported_protocol_version
                                            act = mapped-code ).
    ENDTRY.
  ENDMETHOD.
ENDCLASS.
