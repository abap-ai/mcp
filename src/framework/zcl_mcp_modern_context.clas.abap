"! <p class="shorttext synchronized">MCP draft request context parser</p>
"! Builds and validates the stateless draft request context from JSON-RPC
"! params, MCP _meta fields, and Streamable HTTP headers.
CLASS zcl_mcp_modern_context DEFINITION
  PUBLIC FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    "! <p class="shorttext synchronized">Check whether a request looks modern</p>
    "! Detects draft requests by method, protocol header, or protocol _meta.
    "!
    "! @parameter request      | <p class="shorttext synchronized">JSON-RPC request</p>
    "! @parameter http_request | <p class="shorttext synchronized">HTTP request</p>
    "! @parameter result       | <p class="shorttext synchronized">True if request should use v2 routing</p>
    CLASS-METHODS is_modern_request
      IMPORTING !request      TYPE zcl_mcp_jsonrpc=>request
                http_request  TYPE REF TO if_http_request
      RETURNING VALUE(result) TYPE abap_bool.

    "! <p class="shorttext synchronized">Build draft request context</p>
    "! Validates protocol metadata, header mirrors, client information,
    "! capabilities, extension data, log level, and tracing fields.
    "!
    "! @parameter area           | <p class="shorttext synchronized">MCP area</p>
    "! @parameter mcp_server     | <p class="shorttext synchronized">MCP server name</p>
    "! @parameter request        | <p class="shorttext synchronized">JSON-RPC request</p>
    "! @parameter http_request   | <p class="shorttext synchronized">HTTP request</p>
    "! @parameter http_response  | <p class="shorttext synchronized">HTTP response</p>
    "! @parameter http_server    | <p class="shorttext synchronized">HTTP server</p>
    "! @parameter cors_mode      | <p class="shorttext synchronized">CORS mode</p>
    "! @parameter result         | <p class="shorttext synchronized">Draft v2 context</p>
    "! @raising   zcx_mcp_server | <p class="shorttext synchronized">Invalid or unsupported draft metadata</p>
    CLASS-METHODS build_context
      IMPORTING !area         TYPE zmcp_area
                mcp_server    TYPE zmcp_server
                !request      TYPE zcl_mcp_jsonrpc=>request
                http_request  TYPE REF TO if_http_request
                http_response TYPE REF TO if_http_response
                http_server   TYPE REF TO if_http_server
                cors_mode     TYPE zmcp_conf_cors
      RETURNING VALUE(result) TYPE zif_mcp_server_v2=>v2_context
      RAISING   zcx_mcp_server.

    "! <p class="shorttext synchronized">Check modern protocol version</p>
    "! Returns true if the version is supported by the draft v2 path.
    "!
    "! @parameter protocol_ver | <p class="shorttext synchronized">Protocol version</p>
    "! @parameter result       | <p class="shorttext synchronized">True if supported</p>
    CLASS-METHODS is_supported_version
      IMPORTING protocol_ver  TYPE string
      RETURNING VALUE(result) TYPE abap_bool.

    "! <p class="shorttext synchronized">Create draft metadata error response</p>
    "! Converts a context parser exception into the draft JSON-RPC error code.
    "!
    "! @parameter error    | <p class="shorttext synchronized">Context parser exception</p>
    "! @parameter response | <p class="shorttext synchronized">JSON-RPC error data</p>
    CLASS-METHODS error_from_exception
      IMPORTING !error          TYPE REF TO zcx_mcp_server
      RETURNING VALUE(response) TYPE zcl_mcp_jsonrpc=>error.

  PRIVATE SECTION.
    CONSTANTS c_error_header_mismatch TYPE symsgv VALUE 'HEADER_MISMATCH'.
    CONSTANTS c_error_unsupported     TYPE symsgv VALUE 'UNSUPPORTED_PROTOCOL'.
    CONSTANTS c_error_invalid_meta    TYPE symsgv VALUE 'INVALID_META'.

    "! <p class="shorttext synchronized">Get request metadata</p>
    "! Returns the _meta object or an empty JSON object.
    "!
    "! @parameter request | <p class="shorttext synchronized">JSON-RPC request</p>
    "! @parameter result  | <p class="shorttext synchronized">Metadata object</p>
    CLASS-METHODS get_meta
      IMPORTING !request      TYPE zcl_mcp_jsonrpc=>request
      RETURNING VALUE(result) TYPE REF TO zif_mcp_ajson.

    "! <p class="shorttext synchronized">Validate mirrored transport headers</p>
    "! Checks Mcp-Method and, for named primitive calls, Mcp-Name against the JSON-RPC body.
    "!
    "! @parameter request        | <p class="shorttext synchronized">JSON-RPC request</p>
    "! @parameter http_request   | <p class="shorttext synchronized">HTTP request</p>
    "! @raising   zcx_mcp_server | <p class="shorttext synchronized">Missing or mismatched mirrored header</p>
    CLASS-METHODS validate_header_mirror
      IMPORTING !request     TYPE zcl_mcp_jsonrpc=>request
                http_request TYPE REF TO if_http_request
      RAISING   zcx_mcp_server.

    "! <p class="shorttext synchronized">Get required Mcp-Name value</p>
    "! Returns params.name or params.uri for methods that require Mcp-Name.
    "!
    "! @parameter request | <p class="shorttext synchronized">JSON-RPC request</p>
    "! @parameter result  | <p class="shorttext synchronized">Expected Mcp-Name value</p>
    CLASS-METHODS get_required_name
      IMPORTING !request      TYPE zcl_mcp_jsonrpc=>request
      RETURNING VALUE(result) TYPE string.

    CLASS-METHODS validate_client_info
      IMPORTING meta          TYPE REF TO zif_mcp_ajson
      RETURNING VALUE(result) TYPE REF TO zif_mcp_ajson
      RAISING   zcx_mcp_server.

    CLASS-METHODS validate_client_capabilities
      IMPORTING meta          TYPE REF TO zif_mcp_ajson
      RETURNING VALUE(result) TYPE REF TO zif_mcp_ajson
      RAISING   zcx_mcp_server.

    CLASS-METHODS raise_invalid_meta
      IMPORTING !message TYPE string
      RAISING   zcx_mcp_server.

ENDCLASS.


CLASS zcl_mcp_modern_context IMPLEMENTATION.
  METHOD is_modern_request.
    result = abap_false.

    IF request-method = 'server/discover'.
      result = abap_true.
      RETURN.
    ENDIF.

    DATA(meta) = get_meta( request ).
    DATA(protocol_meta) = meta->get_string( zif_mcp_constants=>meta_member_paths-protocol_version ).

    IF protocol_meta IS NOT INITIAL.
      result = abap_true.
      RETURN.
    ENDIF.

    IF http_request IS BOUND.
      DATA(protocol_header) = http_request->get_header_field( zif_mcp_constants=>header_names-protocol_version ).

      IF     protocol_header IS NOT INITIAL
         AND is_supported_version( protocol_header )  = abap_true.
        result = abap_true.
        RETURN.
      ENDIF.
    ENDIF.
  ENDMETHOD.

  METHOD build_context.
    DATA protocol_meta   TYPE string.
    DATA protocol_header TYPE string.

    result-area          = area.
    result-server        = mcp_server.
    result-mcp_request   = request.
    result-http_request  = http_request.
    result-http_response = http_response.
    result-http_server   = http_server.
    result-cors_mode     = cors_mode.

    result-meta          = get_meta( request ).

    protocol_meta = result-meta->get_string( zif_mcp_constants=>meta_member_paths-protocol_version ).

    IF http_request IS BOUND.
      protocol_header = http_request->get_header_field( zif_mcp_constants=>header_names-protocol_version ).
    ENDIF.

    IF protocol_meta IS INITIAL.
      RAISE EXCEPTION NEW zcx_mcp_server( textid = zcx_mcp_server=>required_params
                                          msgv1  = CONV #( zif_mcp_constants=>meta_keys-protocol_version ) ).
    ENDIF.

    IF http_request IS BOUND AND protocol_header IS INITIAL.
      RAISE EXCEPTION NEW zcx_mcp_server( textid = zcx_mcp_server=>invalid_arguments
                                          msgv1  = 'Missing Mcp-Protocol-Version header'
                                          msgv2  = c_error_header_mismatch ) ##NO_TEXT.
    ENDIF.

    IF http_request IS BOUND AND protocol_header <> protocol_meta.
      RAISE EXCEPTION NEW zcx_mcp_server( textid = zcx_mcp_server=>invalid_arguments
                                          msgv1  = 'Mcp-Protocol-Version mismatch'
                                          msgv2  = c_error_header_mismatch ) ##NO_TEXT.
    ENDIF.

    IF is_supported_version( protocol_meta ) = abap_false.
      RAISE EXCEPTION NEW zcx_mcp_server( textid = zcx_mcp_server=>invalid_arguments
                                          msgv1  = CONV #( protocol_meta )
                                          msgv2  = c_error_unsupported ) ##NO_TEXT.
    ENDIF.

    validate_header_mirror( request      = request
                            http_request = http_request ).

    result-protocol_ver = protocol_meta.

    result-client_info  = validate_client_info( result-meta ).
    result-client_caps  = validate_client_capabilities( result-meta ).

    IF result-client_caps->exists( '/extensions' ).
      IF result-client_caps->get_node_type( '/extensions' ) <> zif_mcp_ajson_types=>node_type-object.
        raise_invalid_meta( 'clientCapabilities.extensions must be an object' ).
      ENDIF.
      result-extensions = result-client_caps->slice( '/extensions' ).
    ELSE.
      result-extensions = zcl_mcp_ajson=>create_empty( ).
    ENDIF.

    result-log_level   = result-meta->get_string( zif_mcp_constants=>meta_member_paths-log_level ).
    result-traceparent = result-meta->get_string( zif_mcp_constants=>meta_member_paths-traceparent ).
    result-tracestate  = result-meta->get_string( zif_mcp_constants=>meta_member_paths-tracestate ).
    result-baggage     = result-meta->get_string( zif_mcp_constants=>meta_member_paths-baggage ).
  ENDMETHOD.

  METHOD is_supported_version.
    SPLIT zif_mcp_constants=>modern_protocol_versions AT `,` INTO TABLE DATA(versions).
    result = xsdbool( line_exists( versions[ table_line = protocol_ver ] ) ).
  ENDMETHOD.

  METHOD error_from_exception.
    DATA supported_versions TYPE string_table.
    DATA error_data         TYPE REF TO zif_mcp_ajson.

    response-code    = zcl_mcp_jsonrpc=>error_codes-invalid_request.
    response-message = error->get_text( ).

    CASE error->if_t100_message~t100key.
      WHEN zcx_mcp_server=>required_params.
        response-code = zcl_mcp_jsonrpc=>error_codes-invalid_params.

      WHEN zcx_mcp_server=>invalid_arguments.
        CASE error->msgv2.
          WHEN 'HEADER_MISMATCH'.
            response-code = zcl_mcp_jsonrpc=>error_codes-header_mismatch.

          WHEN 'UNSUPPORTED_PROTOCOL'.
            response-message = |Unsupported protocol version { error->msgv1 }|.
            response-code    = zcl_mcp_jsonrpc=>error_codes-unsupported_protocol_version.

            TRY.
                error_data = zcl_mcp_ajson=>create_empty( ).
                error_data->touch_array( `/supported` ).

                SPLIT zif_mcp_constants=>modern_protocol_versions AT `,` INTO TABLE supported_versions.

                LOOP AT supported_versions INTO DATA(supported_version).
                  error_data->set_string( iv_path = |/supported/{ sy-tabix }|
                                          iv_val  = supported_version ).
                ENDLOOP.

                error_data->set_string( iv_path = `/requested`
                                        iv_val  = CONV string( error->msgv1 ) ).

                response-data = error_data.

              CATCH zcx_mcp_ajson_error.
                CLEAR response-data.
            ENDTRY.

          WHEN OTHERS.
            response-code = zcl_mcp_jsonrpc=>error_codes-invalid_params.
        ENDCASE.

      WHEN OTHERS.
        response-code = zcl_mcp_jsonrpc=>error_codes-invalid_request.
    ENDCASE.
  ENDMETHOD.

  METHOD get_meta.
    IF request-params IS BOUND AND request-params->exists( '/_meta' ).
      result = request-params->slice( '/_meta' ).
    ELSE.
      result = zcl_mcp_ajson=>create_empty( ).
    ENDIF.
  ENDMETHOD.

  METHOD validate_header_mirror.
    DATA method_header TYPE string.
    DATA name_header   TYPE string.
    DATA required_name TYPE string.

    IF http_request IS NOT BOUND.
      RETURN.
    ENDIF.

    method_header = http_request->get_header_field( zif_mcp_constants=>header_names-method ).

    IF method_header IS INITIAL.
      RAISE EXCEPTION NEW zcx_mcp_server( textid = zcx_mcp_server=>invalid_arguments
                                          msgv1  = 'Missing Mcp-Method header'
                                          msgv2  = c_error_header_mismatch ) ##NO_TEXT.
    ENDIF.

    IF method_header <> request-method.
      RAISE EXCEPTION NEW zcx_mcp_server( textid = zcx_mcp_server=>invalid_arguments
                                          msgv1  = |Mcp-Method mismatch: { method_header } <> { request-method }|
                                          msgv2  = c_error_header_mismatch ) ##NO_TEXT.
    ENDIF.

    required_name = get_required_name( request ).

    IF required_name IS INITIAL.
      RETURN.
    ENDIF.

    name_header = http_request->get_header_field( zif_mcp_constants=>header_names-name ).

    IF name_header IS INITIAL.
      RAISE EXCEPTION NEW zcx_mcp_server( textid = zcx_mcp_server=>invalid_arguments
                                          msgv1  = 'Missing Mcp-Name header'
                                          msgv2  = c_error_header_mismatch ) ##NO_TEXT.
    ENDIF.

    IF name_header <> required_name.
      RAISE EXCEPTION NEW zcx_mcp_server( textid = zcx_mcp_server=>invalid_arguments
                                          msgv1  = |Mcp-Name mismatch: { name_header } <> { required_name }|
                                          msgv2  = c_error_header_mismatch ) ##NO_TEXT.
    ENDIF.
  ENDMETHOD.

  METHOD get_required_name.
    CLEAR result.

    IF request-params IS NOT BOUND.
      RETURN.
    ENDIF.

    CASE request-method.
      WHEN 'tools/call' OR 'prompts/get'.
        result = request-params->get_string( '/name' ).

      WHEN 'resources/read'.
        result = request-params->get_string( '/uri' ).
    ENDCASE.
  ENDMETHOD.

  METHOD validate_client_info.
    IF meta->exists( zif_mcp_constants=>meta_member_paths-client_info ) = abap_false.
      RAISE EXCEPTION NEW zcx_mcp_server( textid = zcx_mcp_server=>required_params
                                          msgv1  = CONV #( zif_mcp_constants=>meta_keys-client_info ) ).
    ENDIF.

    IF meta->get_node_type( zif_mcp_constants=>meta_member_paths-client_info ) <> zif_mcp_ajson_types=>node_type-object.
      raise_invalid_meta( 'clientInfo must be an object' ).
    ENDIF.

    result = meta->slice( zif_mcp_constants=>meta_member_paths-client_info ).

    IF result->get_string( '/name' ) IS INITIAL.
      raise_invalid_meta( 'clientInfo.name must be a non-empty string' ).
    ENDIF.

    IF result->get_string( '/version' ) IS INITIAL.
      raise_invalid_meta( 'clientInfo.version must be a non-empty string' ).
    ENDIF.
  ENDMETHOD.

  METHOD validate_client_capabilities.
    IF meta->exists( zif_mcp_constants=>meta_member_paths-client_capabilities ) = abap_false.
      RAISE EXCEPTION NEW zcx_mcp_server(
        textid = zcx_mcp_server=>required_params
        msgv1  = CONV #( zif_mcp_constants=>meta_keys-client_capabilities ) ).
    ENDIF.

    IF meta->get_node_type( zif_mcp_constants=>meta_member_paths-client_capabilities ) <> zif_mcp_ajson_types=>node_type-object.
      raise_invalid_meta( 'clientCapabilities must be an object' ).
    ENDIF.

    result = meta->slice( zif_mcp_constants=>meta_member_paths-client_capabilities ).
  ENDMETHOD.

  METHOD raise_invalid_meta.
    RAISE EXCEPTION NEW zcx_mcp_server( textid = zcx_mcp_server=>invalid_arguments
                                        msgv1  = CONV #( message )
                                        msgv2  = c_error_invalid_meta ).
  ENDMETHOD.
ENDCLASS.
