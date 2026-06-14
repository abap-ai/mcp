"! <p class="shorttext synchronized" lang="en">MCP HTTP Handler</p>
CLASS zcl_mcp_http_handler DEFINITION
  PUBLIC
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES if_http_extension.

  PROTECTED SECTION.
  PRIVATE SECTION.
    "! JSON-RPC parser instance
    DATA jsonrpc TYPE REF TO zcl_mcp_jsonrpc.
    DATA logger  TYPE REF TO zcl_mcp_logger.

    "! Parse the MCP endpoint path
    "! Extracts the area and server name from the URL path format /mcp/area/servername
    "!
    "! @parameter path   | HTTP request path
    "! @parameter area   | Extracted area name
    "! @parameter server | Extracted server name
    "! @parameter valid  | Flag indicating if path is valid
    METHODS parse_mcp_path
      IMPORTING !path  TYPE string
      EXPORTING !area  TYPE zmcp_area
                server TYPE zmcp_server
                !valid TYPE abap_bool.

    "! Handle HTTP POST request
    "! Processes client-to-server JSON-RPC messages
    "!
    "! @parameter request  | HTTP request object
    "! @parameter response | HTTP response object
    "! @parameter area     | MCP area name
    "! @parameter server   | MCP server name
    METHODS handle_post
      IMPORTING !request  TYPE REF TO if_http_request
                !response TYPE REF TO if_http_response
                !area     TYPE zmcp_area
                server    TYPE zmcp_server.

    "! Handle HTTP GET request
    "! Returns method not allowed as we don't support streaming
    "!
    "! @parameter request  | HTTP request object
    "! @parameter response | HTTP response object
    "! @parameter area     | MCP area name
    "! @parameter server   | MCP server name
    METHODS handle_get
      IMPORTING !request  TYPE REF TO if_http_request
                !response TYPE REF TO if_http_response
                !area     TYPE zmcp_area
                server    TYPE zmcp_server.

    "! Handle HTTP DELETE request
    "! Handles session termination requests
    "!
    "! @parameter request  | HTTP request object
    "! @parameter response | HTTP response object
    "! @parameter area     | MCP area name
    "! @parameter server   | MCP server name
    METHODS handle_delete
      IMPORTING !request  TYPE REF TO if_http_request
                !response TYPE REF TO if_http_response
                !area     TYPE zmcp_area
                server    TYPE zmcp_server.

    "! Check and process client message type
    "! Determines if client message contains requests, responses or notifications
    "!
    "! @parameter json          | JSON message from client
    "! @parameter has_requests  | True if message contains requests
    "! @parameter has_responses | True if message contains responses
    "! @parameter has_notifs    | True if message contains notifications
    METHODS classify_message
      IMPORTING !json         TYPE string
      EXPORTING has_requests  TYPE abap_bool
                has_responses TYPE abap_bool
                has_notifs    TYPE abap_bool.

    "! Process client request
    "! Routes the request to the appropriate MCP server implementation
    "!
    "! @parameter json                | JSON-RPC request
    "! @parameter result              | JSON-RPC response
    "! @raising   zcx_mcp_ajson_error | Error
    METHODS process_request
      IMPORTING !json         TYPE string
      RETURNING VALUE(result) TYPE string
      RAISING   zcx_mcp_ajson_error.

    "! Process draft MCP request
    "! Routes a parsed JSON-RPC request through the stateless v2 server path.
    "!
    "! @parameter request             | Parsed JSON-RPC request
    "! @parameter result              | Serialized JSON-RPC response
    "! @raising   zcx_mcp_ajson_error | JSON serialization error
    METHODS process_modern_request
      IMPORTING !request      TYPE zcl_mcp_jsonrpc=>request
      RETURNING VALUE(result) TYPE string
      RAISING   zcx_mcp_ajson_error.

    "! Handle HTTP OPTIONS request
    "! Handles cors requests
    "!
    "! @parameter request  | HTTP request object
    "! @parameter response | HTTP response object
    "! @parameter area     | MCP area name
    "! @parameter server   | MCP server name
    METHODS handle_options
      IMPORTING !request  TYPE REF TO if_http_request
                !response TYPE REF TO if_http_response
                !area     TYPE zmcp_area
                server    TYPE zmcp_server.

    METHODS origin_allowed IMPORTING origin        TYPE string
                                     !area         TYPE zmcp_area
                                     server        TYPE zmcp_server
                           RETURNING VALUE(result) TYPE abap_bool.

    METHODS set_cors_response_headers
      IMPORTING origin    TYPE string
                !response TYPE REF TO if_http_response.

    METHODS create_error_json
      IMPORTING !code         TYPE i
                !message      TYPE string
                !json         TYPE string OPTIONAL
      RETURNING VALUE(result) TYPE string.

    DATA mcp_server            TYPE REF TO zif_mcp_server.
    DATA mcp_server_v2         TYPE REF TO zif_mcp_server_v2.
    DATA config                TYPE REF TO zcl_mcp_configuration.

    DATA current_area          TYPE zmcp_area.
    DATA current_server        TYPE zmcp_server.
    DATA current_cors_mode     TYPE zmcp_conf_cors.
    DATA current_http_request  TYPE REF TO if_http_request.
    DATA current_http_response TYPE REF TO if_http_response.
    DATA current_http_server   TYPE REF TO if_http_server.

    METHODS validate_session_id
      IMPORTING session_id TYPE string
      RAISING   zcx_mcp_server.

    "! <p class="shorttext synchronized">Parse HTTP request target</p>
    "! Extracts method, area, and server name from the incoming HTTP request.
    "!
    "! @parameter http_server | <p class="shorttext synchronized">HTTP server object</p>
    "! @parameter method      | <p class="shorttext synchronized">HTTP method</p>
    "! @parameter area        | <p class="shorttext synchronized">MCP area</p>
    "! @parameter servername  | <p class="shorttext synchronized">MCP server name</p>
    "! @parameter valid       | <p class="shorttext synchronized">True if path is a valid MCP endpoint</p>
    METHODS parse_target
      IMPORTING http_server TYPE REF TO if_http_server
      EXPORTING !method     TYPE string
                !area       TYPE zmcp_area
                servername  TYPE zmcp_server
                !valid      TYPE abap_bool.

    "! <p class="shorttext synchronized">Authorize endpoint access</p>
    "! Checks ZMCP_SRV authorization and sets the HTTP response on failure.
    "!
    "! @parameter http_server | <p class="shorttext synchronized">HTTP server object</p>
    "! @parameter area        | <p class="shorttext synchronized">MCP area</p>
    "! @parameter servername  | <p class="shorttext synchronized">MCP server name</p>
    "! @parameter result      | <p class="shorttext synchronized">True if access is authorized</p>
    METHODS authorize_target
      IMPORTING http_server   TYPE REF TO if_http_server
                !area         TYPE zmcp_area
                servername    TYPE zmcp_server
      RETURNING VALUE(result) TYPE abap_bool.

    "! <p class="shorttext synchronized">Set current request context</p>
    "! Stores the HTTP and endpoint context used by downstream request processing.
    "!
    "! @parameter http_server | <p class="shorttext synchronized">HTTP server object</p>
    "! @parameter area        | <p class="shorttext synchronized">MCP area</p>
    "! @parameter servername  | <p class="shorttext synchronized">MCP server name</p>
    METHODS set_request_context
      IMPORTING http_server TYPE REF TO if_http_server
                !area       TYPE zmcp_area
                servername  TYPE zmcp_server.

    "! <p class="shorttext synchronized">Bind configured server instances</p>
    "! Loads legacy and draft server instances or restores an active stateful server.
    "!
    "! @parameter http_server | <p class="shorttext synchronized">HTTP server object</p>
    "! @parameter area        | <p class="shorttext synchronized">MCP area</p>
    "! @parameter servername  | <p class="shorttext synchronized">MCP server name</p>
    "! @parameter result      | <p class="shorttext synchronized">True if request processing can continue</p>
    METHODS bind_server_context
      IMPORTING http_server   TYPE REF TO if_http_server
                !area         TYPE zmcp_area
                servername    TYPE zmcp_server
      RETURNING VALUE(result) TYPE abap_bool.

    "! <p class="shorttext synchronized">Enforce CORS origin policy</p>
    "! Validates Origin for non-OPTIONS requests and writes CORS response headers.
    "!
    "! @parameter http_server | <p class="shorttext synchronized">HTTP server object</p>
    "! @parameter method      | <p class="shorttext synchronized">HTTP method</p>
    "! @parameter area        | <p class="shorttext synchronized">MCP area</p>
    "! @parameter servername  | <p class="shorttext synchronized">MCP server name</p>
    "! @parameter result      | <p class="shorttext synchronized">True if request processing can continue</p>
    METHODS enforce_origin
      IMPORTING http_server   TYPE REF TO if_http_server
                !method       TYPE string
                !area         TYPE zmcp_area
                servername    TYPE zmcp_server
      RETURNING VALUE(result) TYPE abap_bool.

    "! <p class="shorttext synchronized">Dispatch HTTP method</p>
    "! Routes the request to the POST, GET, DELETE, or OPTIONS handler.
    "!
    "! @parameter http_server | <p class="shorttext synchronized">HTTP server object</p>
    "! @parameter method      | <p class="shorttext synchronized">HTTP method</p>
    "! @parameter area        | <p class="shorttext synchronized">MCP area</p>
    "! @parameter servername  | <p class="shorttext synchronized">MCP server name</p>
    METHODS dispatch_method
      IMPORTING http_server TYPE REF TO if_http_server
                !method     TYPE string
                !area       TYPE zmcp_area
                servername  TYPE zmcp_server.

    "! <p class="shorttext synchronized">Save session after request</p>
    "! Persists MCP-managed legacy sessions after successful POST processing.
    "!
    "! @parameter method     | <p class="shorttext synchronized">HTTP method</p>
    "! @parameter area       | <p class="shorttext synchronized">MCP area</p>
    "! @parameter servername | <p class="shorttext synchronized">MCP server name</p>
    METHODS save_session
      IMPORTING !method    TYPE string
                !area      TYPE zmcp_area
                servername TYPE zmcp_server.

    "! <p class="shorttext synchronized">Set final protocol headers</p>
    "! Adds protocol and session response headers after request processing.
    "!
    "! @parameter http_server | <p class="shorttext synchronized">HTTP server object</p>
    METHODS set_response_headers
      IMPORTING http_server TYPE REF TO if_http_server.

    "! <p class="shorttext synchronized">Parse JSON-RPC request envelope</p>
    "! Parses and validates the JSON-RPC request, recovers usable error ids,
    "! and handles parse, invalid-request, and notification cases.
    "!
    "! @parameter json                | <p class="shorttext synchronized">Raw JSON-RPC request body</p>
    "! @parameter request             | <p class="shorttext synchronized">Parsed JSON-RPC request</p>
    "! @parameter response_json       | <p class="shorttext synchronized">Serialized response when processing should stop</p>
    "! @parameter result              | <p class="shorttext synchronized">True if request processing should continue</p>
    "! @raising   zcx_mcp_ajson_error | <p class="shorttext synchronized">JSON serialization error</p>
    METHODS parse_request_envelope
      IMPORTING !json         TYPE string
      EXPORTING !request      TYPE zcl_mcp_jsonrpc=>request
                response_json TYPE string
      RETURNING VALUE(result) TYPE abap_bool
      RAISING   zcx_mcp_ajson_error.

    "! <p class="shorttext synchronized">Recover JSON-RPC id</p>
    "! Extracts a usable string or numeric id from parsed JSON for error responses.
    "!
    "! @parameter json_obj   | <p class="shorttext synchronized">Parsed JSON object</p>
    "! @parameter id         | <p class="shorttext synchronized">Recovered id value</p>
    "! @parameter id_present | <p class="shorttext synchronized">True if a usable id was present</p>
    METHODS recover_jsonrpc_id
      IMPORTING json_obj   TYPE REF TO zif_mcp_ajson
      EXPORTING !id        TYPE string
                id_present TYPE abap_bool.

    "! <p class="shorttext synchronized">Create serialized JSON-RPC error</p>
    "! Builds a JSON-RPC error response and serializes it.
    "!
    "! @parameter id                  | <p class="shorttext synchronized">JSON-RPC id</p>
    "! @parameter id_present          | <p class="shorttext synchronized">True if id should be emitted</p>
    "! @parameter id_is_null          | <p class="shorttext synchronized">True if id should be JSON null</p>
    "! @parameter code                | <p class="shorttext synchronized">JSON-RPC error code</p>
    "! @parameter message             | <p class="shorttext synchronized">JSON-RPC error message</p>
    "! @parameter result              | <p class="shorttext synchronized">Serialized JSON-RPC error response</p>
    "! @raising   zcx_mcp_ajson_error | <p class="shorttext synchronized">JSON serialization error</p>
    METHODS create_rpc_error_json
      IMPORTING !id           TYPE string
                id_present    TYPE abap_bool
                id_is_null    TYPE abap_bool DEFAULT abap_false
                !code         TYPE i
                !message      TYPE string
      RETURNING VALUE(result) TYPE string
      RAISING   zcx_mcp_ajson_error.

    "! <p class="shorttext synchronized">Validate legacy session</p>
    "! Checks or loads the legacy session required for non-initialize requests.
    "!
    "! @parameter request  | <p class="shorttext synchronized">JSON-RPC request</p>
    "! @parameter response | <p class="shorttext synchronized">Error response when validation fails</p>
    "! @parameter result   | <p class="shorttext synchronized">True if request processing should continue</p>
    METHODS validate_legacy_session
      IMPORTING !request  TYPE zcl_mcp_jsonrpc=>request
      EXPORTING result    TYPE abap_bool
      CHANGING  !response TYPE zcl_mcp_jsonrpc=>response.
    .

    "! <p class="shorttext synchronized">Negotiate legacy protocol version</p>
    "! Reads, restores, defaults, or validates the legacy protocol version.
    "!
    "! @parameter request  | <p class="shorttext synchronized">JSON-RPC request</p>
    "! @parameter response | <p class="shorttext synchronized">Error response when negotiation fails</p>
    "! @parameter result   | <p class="shorttext synchronized">True if request processing should continue</p>
    METHODS negotiate_legacy_protocol
      IMPORTING !request  TYPE zcl_mcp_jsonrpc=>request
      EXPORTING result    TYPE abap_bool
      CHANGING  !response TYPE zcl_mcp_jsonrpc=>response.

    "! <p class="shorttext synchronized">Dispatch legacy MCP request</p>
    "! Routes a parsed legacy JSON-RPC request to the configured v1 server.
    "!
    "! @parameter request             | <p class="shorttext synchronized">JSON-RPC request</p>
    "! @parameter response            | <p class="shorttext synchronized">JSON-RPC response</p>
    "! @raising   zcx_mcp_ajson_error | <p class="shorttext synchronized">JSON result generation error</p>
    METHODS dispatch_legacy_request
      IMPORTING !request        TYPE zcl_mcp_jsonrpc=>request
      RETURNING VALUE(response) TYPE zcl_mcp_jsonrpc=>response
      RAISING   zcx_mcp_ajson_error.
ENDCLASS.

CLASS zcl_mcp_http_handler IMPLEMENTATION.
  METHOD classify_message.
    DATA first_char TYPE c LENGTH 1.
    DATA json_obj   TYPE REF TO zif_mcp_ajson.

    " Initialize export parameters
    CLEAR: has_requests,
           has_responses,
           has_notifs.

    TRY.
        " Keep current simple batch handling for now.
        " Full JSON-RPC batch semantics are tracked separately but also removed from the specification since a while.
        first_char = json(1).

        IF first_char = '['.
          " JSON-RPC batches are intentionally unsupported for MCP.
          " Leave all flags initial so handle_post routes to bad-request handling below.
          RETURN.
        ENDIF.

        json_obj = zcl_mcp_ajson=>parse( json ).

        " JSON-RPC request/notification objects have a method.
        " Presence of /id, not its ABAP value, distinguishes request from notification.
        " This matters because JSON-RPC id 0 is valid but initial in ABAP.
        IF json_obj->exists( '/method' ).
          IF json_obj->exists( '/id' ).
            has_requests = abap_true.
          ELSE.
            has_notifs = abap_true.
          ENDIF.
          RETURN.
        ENDIF.

        " JSON-RPC response objects contain either result or error.
        IF json_obj->exists( '/result' ) OR json_obj->exists( '/error' ).
          has_responses = abap_true.
          RETURN.
        ENDIF.

      CATCH zcx_mcp_ajson_error.
        " Invalid JSON is handled by the request processing/error path.
        RETURN.
      CATCH cx_root.                                  "#EC NEED_CX_ROOT
        " Defensive fallback: invalid input is handled by caller.
    ENDTRY.
  ENDMETHOD.

  METHOD handle_delete.
    IF mcp_server IS NOT BOUND.
      response->set_status( code   = 405
                            reason = 'Method Not Allowed' ) ##NO_TEXT.
      response->set_header_field( name  = 'Allow'
                                  value = 'POST, OPTIONS' ) ##NO_TEXT.
      RETURN.
    ENDIF.

    " Check if sessions are enabled
    IF mcp_server->server-session_mode = zcl_mcp_session=>session_mode_stateless.
      response->set_status( code   = 405
                            reason = 'Method Not Allowed' ) ##NO_TEXT.
      response->set_header_field( name  = 'Allow'
                                  value = 'POST, OPTIONS' ) ##NO_TEXT.
      RETURN.
    ENDIF.

    " Load the session
    DATA(session_id) = mcp_server->server-http_request->get_header_field( 'Mcp-Session-Id' ) ##NO_TEXT.
    CASE mcp_server->server-session_mode.
      WHEN zcl_mcp_session=>session_mode_icf.
        IF session_id <> mcp_server->server-session_id.
          mcp_server->server-http_response->set_status( code   = 404
                                                        reason = 'Not Found' ) ##NO_TEXT.
          RETURN.
        ENDIF.
      WHEN zcl_mcp_session=>session_mode_mcp.
        TRY.
            validate_session_id( session_id ).
            mcp_server->session = NEW zcl_mcp_session( session_id   = CONV sysuuid_c32( session_id )
                                                       session_mode = mcp_server->server-session_mode
                                                       create_new   = abap_false ).
            mcp_server->server-session_id = session_id.
          CATCH zcx_mcp_server INTO DATA(session_error).
            CASE session_error->if_t100_message~t100key.
              WHEN zcx_mcp_server=>session_unknown OR zcx_mcp_server=>session_expired.
                mcp_server->server-http_response->set_status( code   = 404
                                                              reason = 'Not Found' ) ##NO_TEXT.
                RETURN.

              WHEN zcx_mcp_server=>session_load_error.
                logger->error(
                    |Session { session_id } load error for { mcp_server->server-area } { mcp_server->server-server } details: { session_error->get_text( ) }| ) ##NO_TEXT.
                mcp_server->server-http_response->set_status( code   = 500
                                                              reason = 'Internal Error' ) ##NO_TEXT.
                RETURN.

              WHEN OTHERS.
                mcp_server->server-http_response->set_status( code   = 500
                                                              reason = 'Internal Error' ) ##NO_TEXT.
                RETURN.
            ENDCASE.
        ENDTRY.
    ENDCASE.

    IF mcp_server->server-session_mode = zcl_mcp_session=>session_mode_icf.
      mcp_server->server-http_server->set_session_stateful( stateful = if_http_server=>co_disabled ).
    ELSE.
      mcp_server->session->delete( ).
    ENDIF.
  ENDMETHOD.

  METHOD handle_get.
    response->set_status( code   = 405
                          reason = 'Method Not Allowed' ) ##NO_TEXT.

    IF mcp_server_v2 IS BOUND AND mcp_server IS NOT BOUND.
      response->set_header_field( name  = 'Allow'
                                  value = 'POST, OPTIONS' ) ##NO_TEXT.
    ELSE.
      response->set_header_field( name  = 'Allow'
                                  value = 'POST, DELETE, OPTIONS' ) ##NO_TEXT.
    ENDIF.
  ENDMETHOD.

  METHOD handle_post.
    DATA content_type  TYPE string.
    DATA accept        TYPE string.
    DATA json          TYPE string.
    DATA response_text TYPE string.
    DATA has_requests  TYPE abap_bool.
    DATA has_responses TYPE abap_bool.
    DATA has_notifs    TYPE abap_bool.

    " Get request content and headers
    content_type = request->get_header_field( 'Content-Type' ) ##NO_TEXT.
    accept       = request->get_header_field( 'Accept' ) ##NO_TEXT.
    json         = request->get_cdata( ).

    " Verify content type is application/json
    IF content_type NS 'application/json'.
      response->set_status( code   = 415
                            reason = 'Unsupported Media Type' ) ##NO_TEXT.
      RETURN.
    ENDIF.

    " Accept application/json, or any wildcard that covers it
    IF     accept NS 'application/json'
       AND accept NS 'application/*'
       AND accept NS '*/*'.
      response->set_status( code   = 406
                            reason = 'Not Acceptable' ) ##NO_TEXT.
      RETURN.
    ENDIF.

    " Validate JSON syntax before classification.
    " Unsupported JSON-RPC batches are valid JSON and are handled below as Invalid Request.
    TRY.
        zcl_mcp_ajson=>parse( json ).
      CATCH zcx_mcp_ajson_error.
        response->set_status( code   = 400
                              reason = 'Bad Request' ) ##NO_TEXT.
        response->set_header_field( name  = 'Content-Type'
                                    value = 'application/json' ) ##NO_TEXT.
        response->set_cdata( create_error_json( code    = zcl_mcp_jsonrpc=>error_codes-parse_error
                                                message = 'Invalid JSON' ) ) ##NO_TEXT.
        RETURN.
    ENDTRY.

    " Classify the message to determine content (requests, responses, notifications)
    classify_message( EXPORTING json          = json
                      IMPORTING has_requests  = has_requests
                                has_responses = has_responses
                                has_notifs    = has_notifs ).

    IF has_responses = abap_true.
      DATA(protocol_header) = request->get_header_field( zif_mcp_constants=>header_names-protocol_version ).

      IF zcl_mcp_modern_context=>is_supported_version( protocol_header ) = abap_true.
        response->set_status( code   = 400
                              reason = 'Bad Request' ) ##NO_TEXT.
        response->set_header_field( name  = 'Content-Type'
                                    value = 'application/json' ) ##NO_TEXT.
        response->set_cdata( create_error_json( code    = zcl_mcp_jsonrpc=>error_codes-invalid_request
                                                message = 'Invalid Request'
                                                json    = json ) ) ##NO_TEXT.
      ELSE.
        response->set_status( code   = 202
                              reason = 'Accepted' ) ##NO_TEXT.
      ENDIF.
      RETURN.
    ENDIF.

    IF has_notifs = abap_true.
      TRY.
          DATA(notification_request) = jsonrpc->parse_request( json ).

          IF zcl_mcp_modern_context=>is_modern_request( request      = notification_request
                                                        http_request = request ) = abap_true.

            IF mcp_server_v2 IS NOT BOUND.
              response->set_status( code   = 404
                                    reason = 'Not Found' ) ##NO_TEXT.
              response->set_header_field( name  = 'Content-Type'
                                          value = 'application/json' ) ##NO_TEXT.
              response->set_cdata( create_error_json( code    = zcl_mcp_jsonrpc=>error_codes-method_not_found
                                                      message = |Method { notification_request-method } not found.|
                                                      json    = json ) ) ##NO_TEXT.
              RETURN.
            ENDIF.

            DATA(v2_context) = zcl_mcp_modern_context=>build_context( area          = area
                                                                      mcp_server    = server
                                                                      request       = notification_request
                                                                      http_request  = request
                                                                      http_response = response
                                                                      http_server   = current_http_server
                                                                      cors_mode     = current_cors_mode ).

            mcp_server_v2->set_v2_context( v2_context ).
          ENDIF.

          response->set_status( code   = 202
                                reason = 'Accepted' ) ##NO_TEXT.

        CATCH zcx_mcp_server INTO DATA(modern_error).
          DATA(mapped_error) = zcl_mcp_modern_context=>error_from_exception( modern_error ).

          response->set_status( code   = 400
                                reason = 'Bad Request' ) ##NO_TEXT.
          response->set_header_field( name  = 'Content-Type'
                                      value = 'application/json' ) ##NO_TEXT.
          response->set_cdata( create_error_json( code    = mapped_error-code
                                                  message = mapped_error-message
                                                  json    = json ) ).

        CATCH zcx_mcp_ajson_error.
          response->set_status( code   = 400
                                reason = 'Bad Request' ) ##NO_TEXT.
          response->set_header_field( name  = 'Content-Type'
                                      value = 'application/json' ) ##NO_TEXT.
          response->set_cdata( create_error_json( code    = zcl_mcp_jsonrpc=>error_codes-invalid_request
                                                  message = 'Invalid Request'
                                                  json    = json ) ) ##NO_TEXT.
      ENDTRY.

      RETURN.
    ENDIF.

    IF     has_requests  = abap_false
       AND has_responses = abap_false
       AND has_notifs    = abap_false.

      response->set_status( code   = 400
                            reason = 'Bad Request' ) ##NO_TEXT.
      response->set_header_field( name  = 'Content-Type'
                                  value = 'application/json' ) ##NO_TEXT.
      response->set_cdata( create_error_json( code    = zcl_mcp_jsonrpc=>error_codes-invalid_request
                                              message = 'Invalid Request'
                                              json    = json ) ) ##NO_TEXT.
      RETURN.
    ENDIF.

    " If message contains only responses or notifications
    IF has_requests = abap_false AND ( has_responses = abap_true OR has_notifs = abap_true ).
      " Return 202 Accepted with no body and stop processing.
      " Without streaming notifications processing makes no sense.
      response->set_status( code   = 202
                            reason = 'Accepted' ) ##NO_TEXT.
      RETURN.
    ENDIF.

    " If message contains requests, process them
    IF has_requests = abap_true.
      TRY.
          " Process the request and get the response
          response_text = process_request( json ).
        CATCH zcx_mcp_ajson_error.
          " Handle JSON-RPC error
          response->set_status( code   = 400
                                reason = 'Bad Request' ) ##NO_TEXT.
          response->set_header_field( name  = 'Content-Type'
                                      value = 'application/json' ) ##NO_TEXT.
          response->set_cdata( create_error_json( code    = zcl_mcp_jsonrpc=>error_codes-parse_error
                                                  message = 'Invalid JSON' ) ) ##NO_TEXT.
          RETURN.
      ENDTRY.

      IF response_text IS INITIAL.
        response->set_status( code   = 202
                              reason = 'Accepted' ) ##NO_TEXT.
        RETURN.
      ENDIF.

      " Always JSON response
      response->set_header_field( name  = 'Content-Type'
                                  value = 'application/json' ) ##NO_TEXT.
      " Set default security headers
      response->set_header_field( name  = 'X-Content-Type-Options'
                                  value = 'nosniff' ) ##NO_TEXT.
      response->set_header_field( name  = 'Cache-Control'
                                  value = 'no-store' ) ##NO_TEXT.
      response->set_header_field( name  = 'Content-Security-Policy'
                                  value = 'frame-ancestors ''none''' ) ##NO_TEXT.
      response->set_header_field( name  = 'X-Frame-Options'
                                  value = 'DENY' ) ##NO_TEXT.
      response->set_cdata( response_text ).
    ENDIF.
  ENDMETHOD.


  METHOD if_http_extension~handle_request.
    DATA method     TYPE string.
    DATA area       TYPE zmcp_area.
    DATA servername TYPE zmcp_server.
    DATA valid      TYPE abap_bool.

    IF jsonrpc IS INITIAL.
      jsonrpc = NEW zcl_mcp_jsonrpc( ).
    ENDIF.

    parse_target( EXPORTING http_server = server
                  IMPORTING method      = method
                            area        = area
                            servername  = servername
                            valid       = valid ).

    IF valid = abap_false.
      server->response->set_status( code   = 404
                                    reason = 'Not Found' ) ##NO_TEXT.
      RETURN.
    ENDIF.

    IF authorize_target( http_server = server
                         area        = area
                         servername  = servername ) = abap_false.
      RETURN.
    ENDIF.

    set_request_context( http_server = server
                         area        = area
                         servername  = servername ).

    IF bind_server_context( http_server = server
                            area        = area
                            servername  = servername ) = abap_false.
      RETURN.
    ENDIF.

    logger = config->get_logger( ).
    logger->info( |HTTP { method } for { area } { servername } received| ) ##NO_TEXT.

    IF enforce_origin( http_server = server
                       method      = method
                       area        = area
                       servername  = servername ) = abap_false.
      logger->save( ).
      RETURN.
    ENDIF.

    dispatch_method( http_server = server
                     method      = method
                     area        = area
                     servername  = servername ).

    save_session( method     = method
                  area       = area
                  servername = servername ).

    set_response_headers( server ).

    logger->info( |HTTP { method } for { area } { servername } completed| ) ##NO_TEXT.
    logger->save( ).
  ENDMETHOD.


  METHOD parse_mcp_path.
    DATA path_parts TYPE TABLE OF string.
    DATA count      TYPE i.

    " Initialize export parameters
    CLEAR: area,
           server,
           valid.
    valid = abap_false.

    " Split path by '/' character
    SPLIT path AT '/' INTO TABLE path_parts.

    " Remove empty entries (leading/trailing slashes)
    DELETE path_parts WHERE table_line IS INITIAL.

    " Count path parts
    count = lines( path_parts ).

    " Valid path structure: /area/servername
    IF count = 2.
      area   = path_parts[ 1 ].
      server = path_parts[ 2 ].
      valid  = abap_true.
    ENDIF.
  ENDMETHOD.

  METHOD process_request.
    DATA request       TYPE zcl_mcp_jsonrpc=>request.
    DATA response      TYPE zcl_mcp_jsonrpc=>response.
    DATA response_json TYPE string.

    IF parse_request_envelope( EXPORTING json          = json
                               IMPORTING request       = request
                                         response_json = response_json ) = abap_false.
      result = response_json.
      RETURN.
    ENDIF.

    IF zcl_mcp_modern_context=>is_modern_request( request      = request
                                                  http_request = current_http_request ) = abap_true.
      result = process_modern_request( request ).
      RETURN.
    ENDIF.

    IF mcp_server IS NOT BOUND AND mcp_server_v2 IS BOUND.
      result = zcl_mcp_legacy_v2_adapter=>process_request( server        = mcp_server_v2
                                                           request       = request
                                                           area          = current_area
                                                           servername    = current_server
                                                           http_request  = current_http_request
                                                           http_response = current_http_response
                                                           http_server   = current_http_server
                                                           cors_mode     = current_cors_mode ).
      RETURN.
    ENDIF.

    IF mcp_server IS NOT BOUND.
      result = create_rpc_error_json( id         = request-id
                                      id_present = request-id_present
                                      code       = zcl_mcp_jsonrpc=>error_codes-method_not_found
                                      message    = |Method { request-method } not found.| ) ##NO_TEXT.
      RETURN.
    ENDIF.

    validate_legacy_session( EXPORTING request  = request
                             IMPORTING result   = DATA(legacy_check)
                             CHANGING  response = response ).

    IF legacy_check = abap_false.
      result = jsonrpc->serialize_response( response ).
      RETURN.
    ENDIF.

    negotiate_legacy_protocol( EXPORTING request  = request
                               IMPORTING result   = DATA(negotiate_check)
                               CHANGING  response = response ).

    IF negotiate_check = abap_false.
      result = jsonrpc->serialize_response( response ).
      RETURN.
    ENDIF.

    response = dispatch_legacy_request( request ).

    response-id         = request-id.
    response-id_present = request-id_present.
    response-jsonrpc    = request-jsonrpc.

    result = jsonrpc->serialize_response( response ).
  ENDMETHOD.

  METHOD handle_options.
    DATA origin            TYPE string.
    DATA allow_methods     TYPE string.
    DATA allow_headers     TYPE string.
    DATA requested_headers TYPE string.
    DATA requested_parts   TYPE string_table.
    DATA requested_header  TYPE string.
    DATA lower_header      TYPE string.

    origin = request->get_header_field( `Origin` ) ##NO_TEXT.

    IF mcp_server_v2 IS BOUND AND mcp_server IS NOT BOUND.
      allow_methods = `POST, OPTIONS`.
    ELSE.
      allow_methods = `POST, DELETE, OPTIONS`.
    ENDIF.

    response->set_status( code   = 200
                          reason = `OK` ) ##NO_TEXT.

    response->set_header_field( name  = `Allow`
                                value = allow_methods ) ##NO_TEXT.

    response->set_header_field( name  = `Access-Control-Allow-Methods`
                                value = allow_methods ) ##NO_TEXT.

    IF mcp_server_v2 IS BOUND AND mcp_server IS NOT BOUND.
      allow_headers = `Content-Type, Accept, Authorization, Mcp-Protocol-Version, Mcp-Method, Mcp-Name` ##NO_TEXT.
    ELSE.
      allow_headers = `Content-Type, Accept, Authorization, Mcp-Session-Id, Mcp-Protocol-Version` ##NO_TEXT.
    ENDIF.

    requested_headers = request->get_header_field( `Access-Control-Request-Headers` ) ##NO_TEXT.

    IF requested_headers IS NOT INITIAL.
      SPLIT requested_headers AT `,` INTO TABLE requested_parts.

      LOOP AT requested_parts INTO requested_header.
        requested_header = condense( requested_header ).
        lower_header = to_lower( requested_header ).

        IF     lower_header  CP `mcp-param-*`
           AND allow_headers NS requested_header.
          allow_headers = |{ allow_headers }, { requested_header }|.
        ENDIF.
      ENDLOOP.
    ENDIF.

    response->set_header_field( name  = `Access-Control-Allow-Headers`
                                value = allow_headers ) ##NO_TEXT.

    IF origin IS NOT INITIAL.
      IF    current_cors_mode                 = zcl_mcp_configuration=>cors_mode_ignore
         OR origin_allowed( area   = area
                            server = server
                            origin = origin ) = abap_true.
        response->set_header_field( name  = `Access-Control-Allow-Origin`
                                    value = origin ) ##NO_TEXT.
      ENDIF.
    ENDIF.
  ENDMETHOD.

  METHOD set_cors_response_headers.
    response->set_header_field( name  = 'Access-Control-Allow-Origin'
                                value = origin ) ##NO_TEXT.
    response->set_header_field( name  = 'Access-Control-Allow-Credentials'
                                value = 'true' ) ##NO_TEXT.
    response->set_header_field( name  = 'Access-Control-Expose-Headers'
                                value = 'Mcp-Session-Id, Mcp-Protocol-Version' ) ##NO_TEXT.
    response->set_header_field( name  = 'Vary'
                                value = 'Origin' ) ##NO_TEXT.
  ENDMETHOD.

  METHOD origin_allowed.
    DATA(origins) = config->get_allowed_origins( ).
    result = abap_false.
    LOOP AT origins ASSIGNING FIELD-SYMBOL(<origin>).
      IF origin CP <origin>.
        result = abap_true.
        EXIT.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD create_error_json.
    DATA response   TYPE zcl_mcp_jsonrpc=>response.
    DATA json_obj   TYPE REF TO zif_mcp_ajson.
    DATA error_id   TYPE string.
    DATA id_present TYPE abap_bool.

    IF json IS SUPPLIED AND json IS NOT INITIAL.
      TRY.
          json_obj = zcl_mcp_ajson=>parse( json ).

          IF json_obj->exists( '/id' ).
            CASE json_obj->get_node_type( '/id' ).
              WHEN 'str' OR 'num'.
                error_id   = json_obj->get_string( '/id' ).
                id_present = abap_true.
            ENDCASE.
          ENDIF.
        CATCH zcx_mcp_ajson_error.
          " Malformed JSON: no usable id can be recovered.
      ENDTRY.
    ENDIF.

    response = jsonrpc->create_error_response( id      = error_id
                                               code    = code
                                               message = message ).

    response-id_present = id_present.
    response-id_is_null = abap_false.

    TRY.
        result = jsonrpc->serialize_response( response ).
      CATCH zcx_mcp_ajson_error.
        result = |\{"jsonrpc":"2.0","error":\{"code":{ code },"message":"{ message }"\}\}|.
    ENDTRY.
  ENDMETHOD.

  METHOD validate_session_id.
    IF session_id IS INITIAL.
      RAISE EXCEPTION NEW zcx_mcp_server( textid = zcx_mcp_server=>session_unknown
                                          msgv1  = CONV #( session_id ) ).
    ENDIF.

    IF strlen( session_id ) <> 32.
      RAISE EXCEPTION NEW zcx_mcp_server( textid = zcx_mcp_server=>session_unknown
                                          msgv1  = CONV #( session_id ) ).
    ENDIF.

    FIND REGEX '^[0-9A-Fa-f]{32}$' IN session_id ##NO_TEXT.
    IF sy-subrc <> 0.
      RAISE EXCEPTION NEW zcx_mcp_server( textid = zcx_mcp_server=>session_unknown
                                          msgv1  = CONV #( session_id ) ).
    ENDIF.
  ENDMETHOD.

  METHOD process_modern_request.
    DATA context      TYPE zif_mcp_server_v2=>v2_context.
    DATA response     TYPE zcl_mcp_jsonrpc=>response.
    DATA mapped_error TYPE zcl_mcp_jsonrpc=>error.

    TRY.
        IF mcp_server_v2 IS NOT BOUND.
          response = jsonrpc->create_error_response( id      = request-id
                                                     code    = zcl_mcp_jsonrpc=>error_codes-method_not_found
                                                     message = |Method { request-method } not found.| ) ##NO_TEXT.
          response-id_present = request-id_present.
          response-jsonrpc    = request-jsonrpc.

          IF current_http_response IS BOUND.
            current_http_response->set_status( code   = 404
                                               reason = 'Not Found' ) ##NO_TEXT.
          ENDIF.

          result = jsonrpc->serialize_response( response ).
          RETURN.
        ENDIF.

        context = zcl_mcp_modern_context=>build_context( area          = current_area
                                                         mcp_server    = current_server
                                                         request       = request
                                                         http_request  = current_http_request
                                                         http_response = current_http_response
                                                         http_server   = current_http_server
                                                         cors_mode     = current_cors_mode ).

        mcp_server_v2->set_v2_context( context ).

        response = zcl_mcp_modern_router=>route_request( server  = mcp_server_v2
                                                         request = request ).

        IF     response-error-code    = zcl_mcp_jsonrpc=>error_codes-method_not_found
           AND current_http_response IS BOUND.
          current_http_response->set_status( code   = 404
                                             reason = 'Not Found' ) ##NO_TEXT.
        ELSEIF     response-error-code    = zcl_mcp_jsonrpc=>error_codes-header_mismatch
               AND current_http_response IS BOUND.
          current_http_response->set_status( code   = 400
                                             reason = 'Bad Request' ) ##NO_TEXT.
        ENDIF.

      CATCH zcx_mcp_server INTO DATA(context_error).
        mapped_error = zcl_mcp_modern_context=>error_from_exception( context_error ).

        IF current_http_response IS BOUND.
          current_http_response->set_status( code   = 400
                                             reason = 'Bad Request' ) ##NO_TEXT.
        ENDIF.

        response = jsonrpc->create_error_response( id      = request-id
                                                   code    = mapped_error-code
                                                   message = mapped_error-message
                                                   data    = mapped_error-data ).
        response-id_present = request-id_present.
        response-jsonrpc    = request-jsonrpc.
    ENDTRY.

    result = jsonrpc->serialize_response( response ).
  ENDMETHOD.

  METHOD parse_target.
    DATA path TYPE string.

    method = http_server->request->get_method( ).
    path   = http_server->request->get_header_field( '~path_info' ).

    parse_mcp_path( EXPORTING path   = path
                    IMPORTING area   = area
                              server = servername
                              valid  = valid ).
  ENDMETHOD.

  METHOD authorize_target.
    result = abap_true.

    AUTHORITY-CHECK OBJECT 'ZMCP_SRV'
                    ID 'ZMCP_AREA' FIELD area
                    ID 'ZMCP_SRV' FIELD servername.
    IF sy-subrc <> 0.
      http_server->response->set_status( code   = 401
                                         reason = 'Not Authorized' ) ##NO_TEXT.
      result = abap_false.
    ENDIF.
  ENDMETHOD.

  METHOD set_request_context.
    current_area          = area.
    current_server        = servername.
    current_http_request  = http_server->request.
    current_http_response = http_server->response.
    current_http_server   = http_server.
    CLEAR current_cors_mode.
  ENDMETHOD.

  METHOD bind_server_context.
    result = abap_true.
    CLEAR: mcp_server_v2,
           config.

    IF http_server->stateful = if_http_server=>co_enabled.
      IF mcp_server IS NOT BOUND.
        http_server->response->set_status( code   = 400
                                           reason = 'Bad Request' ) ##NO_TEXT.
        http_server->response->set_header_field( name  = 'Content-Type'
                                                 value = 'application/json' ) ##NO_TEXT.
        http_server->response->set_cdata( create_error_json( code    = zcl_mcp_jsonrpc=>error_codes-invalid_request
                                                             message = 'Invalid or expired MCP session'
                                                             json    = http_server->request->get_cdata( ) ) ) ##NO_TEXT.
        result = abap_false.
        RETURN.
      ENDIF.

      config = mcp_server->config.
      current_cors_mode = mcp_server->server-cors_mode.
      RETURN.
    ENDIF.

    CLEAR mcp_server.

    mcp_server = zcl_mcp_server_factory=>get_server( area   = area
                                                     server = servername ).

    mcp_server_v2 = zcl_mcp_server_factory=>get_server_v2( area   = area
                                                           server = servername ).

    IF mcp_server IS NOT BOUND AND mcp_server_v2 IS NOT BOUND.
      http_server->response->set_status( code   = 404
                                         reason = 'Not Found' ) ##NO_TEXT.
      result = abap_false.
      RETURN.
    ENDIF.

    IF mcp_server IS BOUND.
      mcp_server->server-area          = area.
      mcp_server->server-server        = servername.
      mcp_server->server-http_request  = http_server->request.
      mcp_server->server-http_response = http_server->response.
      mcp_server->server-http_server   = http_server.
      config = mcp_server->config.
      current_cors_mode = mcp_server->server-cors_mode.
    ELSEIF mcp_server_v2 IS BOUND.
      config = mcp_server_v2->config.
      current_cors_mode = config->get_cors_mode( ).
    ENDIF.
  ENDMETHOD.

  METHOD enforce_origin.
    DATA origin          TYPE string.
    DATA validate_origin TYPE abap_bool.

    result = abap_true.

    origin = http_server->request->get_header_field( 'Origin' ) ##NO_TEXT.

    validate_origin = xsdbool(    current_cors_mode <> zcl_mcp_configuration=>cors_mode_ignore
                               OR mcp_server_v2     IS BOUND ).

    IF origin IS INITIAL.
      IF current_cors_mode = zcl_mcp_configuration=>cors_mode_enforce.
        http_server->response->set_status( code   = 400
                                           reason = 'Origin Header Missing' ) ##NO_TEXT.
        logger->warning( |Origin header missing for { area } { servername }| ) ##NO_TEXT.
        result = abap_false.
      ENDIF.
      RETURN.
    ENDIF.

    IF     validate_origin = abap_true
       AND origin_allowed( area   = area
                           server = servername
                           origin = origin )   = abap_false.
      http_server->response->set_status( code   = 403
                                         reason = 'Forbidden' ) ##NO_TEXT.
      logger->warning( |Origin { origin } not allowed for { area } { servername }| ) ##NO_TEXT.
      result = abap_false.
      RETURN.
    ENDIF.

    set_cors_response_headers( origin   = origin
                               response = http_server->response ).
  ENDMETHOD.

  METHOD dispatch_method.
    CASE method.
      WHEN 'POST'.
        handle_post( request  = http_server->request
                     response = http_server->response
                     area     = area
                     server   = servername ).

      WHEN 'GET'.
        handle_get( request  = http_server->request
                    response = http_server->response
                    area     = area
                    server   = servername ).

      WHEN 'DELETE'.
        handle_delete( request  = http_server->request
                       response = http_server->response
                       area     = area
                       server   = servername ).

      WHEN 'OPTIONS'.
        handle_options( request  = http_server->request
                        response = http_server->response
                        area     = area
                        server   = servername ).

      WHEN OTHERS.
        http_server->response->set_status( code   = 405
                                           reason = 'Method Not Allowed' ) ##NO_TEXT.

        IF mcp_server_v2 IS BOUND AND mcp_server IS NOT BOUND.
          http_server->response->set_header_field( name  = 'Allow'
                                                   value = 'POST, OPTIONS' ) ##NO_TEXT.
        ELSE.
          http_server->response->set_header_field( name  = 'Allow'
                                                   value = 'POST, DELETE, OPTIONS' ) ##NO_TEXT.
        ENDIF.
    ENDCASE.
  ENDMETHOD.

  METHOD save_session.
    IF     mcp_server IS BOUND
       AND mcp_server->server-session_mode  = zcl_mcp_session=>session_mode_mcp
       AND method = 'POST'
       AND mcp_server->session             IS BOUND.
      TRY.
          mcp_server->session->save( ).
        CATCH zcx_mcp_server INTO DATA(session_error).
          logger->error(
              |Session { mcp_server->server-session_id } save error for { area } { servername } details: { session_error->get_text( ) }| ) ##NO_TEXT.
      ENDTRY.
    ENDIF.
  ENDMETHOD.

  METHOD set_response_headers.
    DATA v2_context       TYPE zif_mcp_server_v2=>v2_context.
    DATA protocol_version TYPE string.

    IF mcp_server_v2 IS BOUND.
      v2_context = mcp_server_v2->get_v2_context( ).
    ENDIF.

    IF v2_context-protocol_ver IS NOT INITIAL.
      protocol_version = v2_context-protocol_ver.

    ELSEIF mcp_server IS BOUND.
      protocol_version = mcp_server->server-protocol_version.

    ELSEIF mcp_server_v2 IS BOUND.
      protocol_version = http_server->request->get_header_field( zif_mcp_constants=>header_names-protocol_version ).

      IF protocol_version IS INITIAL.
        protocol_version = zif_mcp_constants=>protocol_version_2025_03_26.
      ENDIF.
    ENDIF.

    IF protocol_version IS NOT INITIAL.
      http_server->response->set_header_field( name  = zif_mcp_constants=>header_names-protocol_version
                                               value = protocol_version ).
    ENDIF.

    IF     mcp_server                    IS BOUND
       AND mcp_server->server-session_id IS NOT INITIAL.
      http_server->response->set_header_field( name  = zif_mcp_constants=>header_names-session_id
                                               value = CONV #( mcp_server->server-session_id ) ).
    ENDIF.
  ENDMETHOD.

  METHOD parse_request_envelope.
    DATA json_obj         TYPE REF TO zif_mcp_ajson.
    DATA error_id         TYPE string.
    DATA error_id_present TYPE abap_bool.

    result = abap_false.
    CLEAR: request,
           response_json.

    TRY.
        json_obj = zcl_mcp_ajson=>parse( json ).
      CATCH zcx_mcp_ajson_error.
        response_json = create_rpc_error_json( id         = ``
                                              id_present = abap_false
                                              id_is_null = abap_true
                                              code       = zcl_mcp_jsonrpc=>error_codes-parse_error
                                              message    = 'Invalid JSON' ) ##NO_TEXT.
        RETURN.
    ENDTRY.

    recover_jsonrpc_id( EXPORTING json_obj   = json_obj
                        IMPORTING id         = error_id
                                  id_present = error_id_present ).

    IF    json_obj->get_string( '/jsonrpc' ) <> zcl_mcp_jsonrpc=>jsonrpc_version
       OR json_obj->exists( '/method' )       = abap_false
       OR json_obj->get_string( '/method' )  IS INITIAL.

      response_json = create_rpc_error_json( id         = error_id
                                            id_present = error_id_present
                                            code       = zcl_mcp_jsonrpc=>error_codes-invalid_request
                                            message    = 'Invalid Request' ) ##NO_TEXT.
      RETURN.
    ENDIF.

    TRY.
        request = jsonrpc->parse_request( json ).
      CATCH zcx_mcp_ajson_error.
        response_json = create_rpc_error_json( id         = error_id
                                              id_present = error_id_present
                                              code       = zcl_mcp_jsonrpc=>error_codes-invalid_request
                                              message    = 'Invalid Request' ) ##NO_TEXT.
        RETURN.
    ENDTRY.

    IF request-id_present = abap_false.
      response_json = ``.
      RETURN.
    ENDIF.

    result = abap_true.
  ENDMETHOD.

  METHOD recover_jsonrpc_id.
    CLEAR: id,
           id_present.

    IF json_obj IS NOT BOUND OR json_obj->exists( '/id' ) = abap_false.
      RETURN.
    ENDIF.

    CASE json_obj->get_node_type( '/id' ).
      WHEN 'str' OR 'num'.
        id = json_obj->get_string( '/id' ).
        id_present = abap_true.
    ENDCASE.
  ENDMETHOD.

  METHOD create_rpc_error_json.
    DATA response TYPE zcl_mcp_jsonrpc=>response.

    response = jsonrpc->create_error_response( id      = id
                                               code    = code
                                               message = message ).

    response-id_present = id_present.
    response-id_is_null = id_is_null.

    result = jsonrpc->serialize_response( response ).
  ENDMETHOD.

  METHOD validate_legacy_session.
    result = abap_true.
    CLEAR response.

    IF request-method = 'initialize'.
      RETURN.
    ENDIF.

    DATA(session_id) = mcp_server->server-http_request->get_header_field( 'Mcp-Session-Id' ) ##NO_TEXT.

    IF mcp_server->server-session_mode <> zcl_mcp_session=>session_mode_stateless AND session_id IS INITIAL.
      mcp_server->server-http_response->set_status( code   = 400
                                                    reason = 'Bad Request' ) ##NO_TEXT.
      response = jsonrpc->create_error_response( id      = request-id
                                                 code    = zcl_mcp_jsonrpc=>error_codes-invalid_request
                                                 message = 'Missing Mcp-Session-Id' ) ##NO_TEXT.
      response-id_present = request-id_present.
      response-jsonrpc    = request-jsonrpc.
      result = abap_false.
      RETURN.
    ENDIF.

    CASE mcp_server->server-session_mode.
      WHEN zcl_mcp_session=>session_mode_icf.
        IF session_id <> mcp_server->server-session_id.
          mcp_server->server-http_response->set_status( code   = 404
                                                        reason = 'Not Found' ) ##NO_TEXT.
          response = jsonrpc->create_error_response( id      = request-id
                                                     code    = zcl_mcp_jsonrpc=>error_codes-invalid_request
                                                     message = 'Invalid or expired MCP session' ) ##NO_TEXT.
          response-id_present = request-id_present.
          response-jsonrpc    = request-jsonrpc.
          result = abap_false.
          RETURN.
        ENDIF.

      WHEN zcl_mcp_session=>session_mode_mcp.
        TRY.
            validate_session_id( session_id ).
            mcp_server->session = NEW zcl_mcp_session( session_id   = CONV sysuuid_c32( session_id )
                                                       session_mode = mcp_server->server-session_mode
                                                       create_new   = abap_false ).
            mcp_server->server-session_id = session_id.

          CATCH zcx_mcp_server INTO DATA(session_error).
            CASE session_error->if_t100_message~t100key.
              WHEN zcx_mcp_server=>session_unknown OR zcx_mcp_server=>session_expired.
                mcp_server->server-http_response->set_status( code   = 404
                                                              reason = 'Not Found' ) ##NO_TEXT.
                response = jsonrpc->create_error_response( id      = request-id
                                                           code    = zcl_mcp_jsonrpc=>error_codes-invalid_request
                                                           message = 'Invalid or expired MCP session' ) ##NO_TEXT.
              WHEN zcx_mcp_server=>session_load_error.
                logger->error(
                    |Session { session_id } load error for { mcp_server->server-area } { mcp_server->server-server } details: { session_error->get_text( ) }| ) ##NO_TEXT.
                mcp_server->server-http_response->set_status( code   = 500
                                                              reason = 'Internal Error' ) ##NO_TEXT.
                response = jsonrpc->create_error_response( id      = request-id
                                                           code    = zcl_mcp_jsonrpc=>error_codes-internal_error
                                                           message = session_error->get_text( ) ).
              WHEN OTHERS.
                mcp_server->server-http_response->set_status( code   = 500
                                                              reason = 'Internal Error' ) ##NO_TEXT.
                response = jsonrpc->create_error_response( id      = request-id
                                                           code    = zcl_mcp_jsonrpc=>error_codes-internal_error
                                                           message = session_error->get_text( ) ).
            ENDCASE.

            response-id_present = request-id_present.
            response-jsonrpc    = request-jsonrpc.
            result = abap_false.
            RETURN.
        ENDTRY.
    ENDCASE.
  ENDMETHOD.

  METHOD negotiate_legacy_protocol.
    result = abap_true.
    CLEAR response.

    IF request-method = 'initialize'.
      RETURN.
    ENDIF.

    DATA(protocol_version) = mcp_server->server-http_request->get_header_field( 'Mcp-Protocol-Version' ) ##NO_TEXT.

    IF protocol_version IS INITIAL.
      IF mcp_server->server-protocol_version IS NOT INITIAL.
        protocol_version = mcp_server->server-protocol_version.
      ELSEIF mcp_server->session IS BOUND.
        DATA(protocol_entry) = mcp_server->session->get( 'protocolVersion' ).
        protocol_version = protocol_entry-value.
      ENDIF.

      IF protocol_version IS INITIAL.
        protocol_version = zif_mcp_constants=>protocol_version_2025_03_26.
      ENDIF.

      mcp_server->server-protocol_version = protocol_version.
      RETURN.
    ENDIF.

    SPLIT zif_mcp_constants=>supported_protocol_versions AT `,` INTO TABLE DATA(supported_protocol_versions).

    IF line_exists( supported_protocol_versions[ table_line = protocol_version ] ).
      mcp_server->server-protocol_version = protocol_version.
      RETURN.
    ENDIF.

    mcp_server->server-http_response->set_status( code   = 400
                                                  reason = 'Bad Request' ) ##NO_TEXT.
    response = jsonrpc->create_error_response( id      = request-id
                                               code    = zcl_mcp_jsonrpc=>error_codes-invalid_request
                                               message = |Unsupported Mcp-Protocol-Version { protocol_version }| ) ##NO_TEXT.
    response-id_present = request-id_present.
    response-jsonrpc    = request-jsonrpc.
    result = abap_false.
  ENDMETHOD.

  METHOD dispatch_legacy_request.
    mcp_server->server-mcp_request = request.

    logger->info( |Processing request { request-method } for { mcp_server->server-area } { mcp_server->server-server }| ) ##NO_TEXT.

    TRY.
        CASE request-method.
          WHEN 'initialize'.
            DATA(initialize) = mcp_server->initialize( NEW zcl_mcp_req_initialize( request-params ) ).
            response-error  = initialize-error.
            response-result = initialize-result->zif_mcp_internal~generate_json( ).

          WHEN 'ping'.
            CLEAR response-error.
            response-result = zcl_mcp_ajson=>create_empty( ).
            response-result->touch_object( '' ).

          WHEN 'logging/setLevel'.
            response-error-code    = zcl_mcp_jsonrpc=>error_codes-method_not_found.
            response-error-message = 'logging/setLevel is not supported: server does not declare logging capability' ##NO_TEXT.

          WHEN 'prompts/list'.
            DATA(list_prompts) = mcp_server->prompts_list( NEW zcl_mcp_req_list_prompts( request-params ) ).
            response-error  = list_prompts-error.
            response-result = list_prompts-result->zif_mcp_internal~generate_json( ).

          WHEN 'prompts/get'.
            DATA(get_prompt) = mcp_server->prompts_get( NEW zcl_mcp_req_get_prompt( request-params ) ).
            response-error  = get_prompt-error.
            response-result = get_prompt-result->zif_mcp_internal~generate_json( ).

          WHEN 'resources/list'.
            DATA(list_resources) = mcp_server->resources_list( NEW zcl_mcp_req_list_resources( request-params ) ).
            response-error  = list_resources-error.
            response-result = list_resources-result->zif_mcp_internal~generate_json( ).

          WHEN 'resources/templates/list'.
            DATA(list_res_tmpl) = mcp_server->resources_templates_list( NEW zcl_mcp_req_list_res_tmpls( request-params ) ).
            response-error  = list_res_tmpl-error.
            response-result = list_res_tmpl-result->zif_mcp_internal~generate_json( ).

          WHEN 'resources/read'.
            DATA(read_resource) = mcp_server->resources_read( NEW zcl_mcp_req_read_resource( request-params ) ).
            response-error  = read_resource-error.
            response-result = read_resource-result->zif_mcp_internal~generate_json( ).

          WHEN 'tools/list'.
            DATA(list_tools) = mcp_server->tools_list( NEW zcl_mcp_req_list_tools( request-params ) ).
            response-error  = list_tools-error.
            response-result = list_tools-result->zif_mcp_internal~generate_json( ).

          WHEN 'tools/call'.
            DATA(call_tool) = mcp_server->tools_call( NEW zcl_mcp_req_call_tool( request-params ) ).
            response-error  = call_tool-error.
            response-result = call_tool-result->zif_mcp_internal~generate_json( ).

          WHEN 'tasks/list'.
            DATA(list_tasks) = mcp_server->tasks_list( NEW zcl_mcp_req_list_tasks( request-params ) ).
            response-error  = list_tasks-error.
            response-result = list_tasks-result->zif_mcp_internal~generate_json( ).

          WHEN 'tasks/get'.
            DATA(get_task) = mcp_server->tasks_get( NEW zcl_mcp_req_get_task( request-params ) ).
            response-error  = get_task-error.
            response-result = get_task-result->zif_mcp_internal~generate_json( ).

          WHEN 'tasks/result'.
            DATA(task_result) = mcp_server->tasks_result( NEW zcl_mcp_req_get_task_payload( request-params ) ).
            response-error  = task_result-error.
            response-result = task_result-result->zif_mcp_internal~generate_json( ).

          WHEN 'tasks/cancel'.
            DATA(cancel_task) = mcp_server->tasks_cancel( NEW zcl_mcp_req_cancel_task( request-params ) ).
            response-error  = cancel_task-error.
            response-result = cancel_task-result->zif_mcp_internal~generate_json( ).

          WHEN 'completion/complete'.
            DATA(complete) = mcp_server->completions_complete( NEW zcl_mcp_req_complete( request-params ) ).
            response-error  = complete-error.
            response-result = complete-result->zif_mcp_internal~generate_json( ).

          WHEN OTHERS.
            response-error-code    = zcl_mcp_jsonrpc=>error_codes-method_not_found.
            response-error-message = |Method { request-method } not found.| ##NO_TEXT.
        ENDCASE.

      CATCH zcx_mcp_server INTO DATA(mcp_error).
        IF     request-method = 'tools/call'
           AND mcp_error->if_t100_message~t100key = zcx_mcp_server=>invalid_arguments.
          DATA(err_result) = NEW zcl_mcp_resp_call_tool( ).
          err_result->set_error( abap_true ).
          err_result->add_text_content( mcp_error->get_text( ) ).
          response-result = err_result->zif_mcp_internal~generate_json( ).
        ELSE.
          CASE mcp_error->if_t100_message~t100key.
            WHEN zcx_mcp_server=>invalid_arguments
              OR zcx_mcp_server=>prompt_name_invalid
              OR zcx_mcp_server=>required_params.
              response-error-code = zcl_mcp_jsonrpc=>error_codes-invalid_params.
            WHEN zcx_mcp_server=>resource_not_found.
              response-error-code = zcl_mcp_jsonrpc=>error_codes-resource_not_found.
            WHEN zcx_mcp_server=>unknown_tool.
              response-error-code = zcl_mcp_jsonrpc=>error_codes-method_not_found.
            WHEN OTHERS.
              response-error-code = zcl_mcp_jsonrpc=>error_codes-internal_error.
          ENDCASE.
          response-error-message = mcp_error->get_text( ).
        ENDIF.

        logger->warning(
            |Error processing request { request-method } for { mcp_server->server-area } { mcp_server->server-server } details: { mcp_error->get_text( ) }| ) ##NO_TEXT.
    ENDTRY.
  ENDMETHOD.
ENDCLASS.
