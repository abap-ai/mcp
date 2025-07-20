"! <p class="shorttext synchronized" lang="en">MCP HTTP Handler</p>
CLASS zcl_mcp_http_handler DEFINITION
  PUBLIC
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES if_http_extension.

protected section.
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
      IMPORTING json          TYPE string
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
      IMPORTING json          TYPE string
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

    DATA mcp_server TYPE REF TO zif_mcp_server.
ENDCLASS.



CLASS ZCL_MCP_HTTP_HANDLER IMPLEMENTATION.


  METHOD classify_message.
    DATA first_char TYPE c LENGTH 1.
    DATA request    TYPE zcl_mcp_jsonrpc=>request.

    " Initialize export parameters
    CLEAR: has_requests,
           has_responses,
           has_notifs.

    " Check if JSON is valid
    TRY.
        " Get first character to check if it's an array or object
        first_char = json(1).

        IF first_char = '['.
          " It's a batch request - for simplicity we'll assume it contains at least one request
          has_requests = abap_true.
        ELSE.
          " Try to parse as a single request
          TRY.
              request = jsonrpc->parse_request( json ).

              " If it has an ID, it's a request
              IF request-id IS NOT INITIAL.
                has_requests = abap_true.
              ELSE.
                " No ID means it's a notification
                has_notifs = abap_true.
              ENDIF.
            CATCH zcx_mcp_ajson_error.
              " If not a valid request, assume it's a response
              has_responses = abap_true.
          ENDTRY.
        ENDIF.
      CATCH cx_root.   "#EC NEED_CX_ROOT
        " Invalid JSON - will be handled by caller
    ENDTRY.
  ENDMETHOD.


  METHOD handle_delete.
    " Check if sessions are enabled
    IF mcp_server->server-session_mode = zcl_mcp_session=>session_mode_stateless.
      response->set_status( code   = 405
                            reason = 'Method Not Allowed' ) ##NO_TEXT.
      response->set_header_field( name  = 'Allow'
                                  value = 'POST' ) ##NO_TEXT.
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
            mcp_server->session = NEW zcl_mcp_session( session_id   = CONV sysuuid_c32( session_id )
                                                       session_mode = mcp_server->server-session_mode
                                                       create_new   = abap_false ).
            mcp_server->server-session_id = session_id.
          CATCH zcx_mcp_server INTO DATA(session_error).
            CASE session_error->if_t100_message~t100key.
              WHEN zcx_mcp_server=>session_unknown OR zcx_mcp_server=>session_expired.
                mcp_server->server-http_response->set_status( code   = 404
                                              reason = 'Not Found' ) ##NO_TEXT.
              WHEN zcx_mcp_server=>session_load_error.
                logger->error( |Session { session_id } load error for { mcp_server->server-area } { mcp_server->server-server } details: { session_error->get_text( ) }| ) ##NO_TEXT.
                mcp_server->server-http_response->set_status( code   = 500
                                              reason = 'Internal Error' ) ##NO_TEXT.
            ENDCASE.
            mcp_server->server-http_response->set_status( code   = 500
                                          reason = 'Internal Error' ) ##NO_TEXT.
        ENDTRY.
    ENDCASE.

    IF mcp_server->server-session_mode = zcl_mcp_session=>session_mode_icf.
      mcp_server->server-http_server->set_session_stateful( stateful = if_http_server=>co_disabled ).
    ELSE.
      mcp_server->session->delete( ).
    ENDIF.
  ENDMETHOD.


  METHOD handle_get.
    " We don't support streaming - return Method Not Allowed
    response->set_status( code   = 405
                          reason = 'Method Not Allowed' ) ##NO_TEXT.
    response->set_header_field( name  = 'Allow'
                                value = 'POST' ) ##NO_TEXT.
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

    " Verify Accept header contains application/json
    IF accept NS 'application/json'.
      response->set_status( code   = 406
                            reason = 'Not Acceptable' ) ##NO_TEXT.
      RETURN.
    ENDIF.

    " Classify the message to determine content (requests, responses, notifications)
    classify_message( EXPORTING json          = json
                      IMPORTING has_requests  = has_requests
                                has_responses = has_responses
                                has_notifs    = has_notifs ).

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
        CATCH zcx_mcp_ajson_error INTO DATA(exception).
          " Handle JSON-RPC error
          response->set_status( code   = 400
                                reason = 'Bad Request' ) ##NO_TEXT.
          response->set_cdata(
              |\{"jsonrpc":"2.0","error":{ zcl_mcp_jsonrpc=>error_codes-parse_error } "code":\{,"message":"Invalid JSON"\},"id":null\}| ) ##NO_TEXT.
          RETURN.
      ENDTRY.

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
    DATA path       TYPE string.
    DATA method     TYPE string.
    DATA area       TYPE zmcp_area.
    DATA servername TYPE zmcp_server.
    DATA valid      TYPE abap_bool.
    DATA continue   TYPE abap_bool VALUE abap_true.

    " Create JSON-RPC parser instance if not exists
    IF jsonrpc IS INITIAL.
      jsonrpc = NEW zcl_mcp_jsonrpc( ).
    ENDIF.

    " Get HTTP method and path
    method = server->request->get_method( ).
    path   = server->request->get_header_field( '~path_info' ).

    " Parse the path to extract area and server name
    parse_mcp_path( EXPORTING path   = path
                    IMPORTING area   = area
                              server = servername
                              valid  = valid ).

    " If path is invalid, return 404 Not Found
    IF valid = abap_false.
      server->response->set_status( code   = 404
                                    reason = 'Not Found' ) ##NO_TEXT.
      RETURN.
    ENDIF.

    AUTHORITY-CHECK OBJECT 'ZMCP_SRV'
                    ID 'ZMCP_AREA' FIELD area
                    ID 'ZMCP_SRV' FIELD servername.
    IF sy-subrc <> 0.
      server->response->set_status( code   = 401
                                    reason = 'Not Authorized' ) ##NO_TEXT.
      RETURN.
    ENDIF.

    " Next steps depend on session management:
    " 1) mcp_server is bound ==> we have an active ICF handled session
    " 2) mcp_server is not bound but we have a valid session id ==> MCP internal session handling
    " 3) mcp_server is not bound and no session id ==> either not session management or we have a new session

    IF server->stateful = if_http_server=>co_enabled.
      IF mcp_server IS NOT BOUND.
        server->response->set_status( code   = 400
                                      reason = 'Bad Request' ) ##NO_TEXT.
        continue = abap_false.
      ENDIF.
    ELSE.
      mcp_server = zcl_mcp_server_factory=>get_server( area   = area
                                                       server = servername ).
      IF mcp_server IS NOT BOUND.
        server->response->set_status( code   = 404
                                      reason = 'Not Found' ) ##NO_TEXT.
        continue = abap_false.
      ELSE.
        mcp_server->server-area          = area.
        mcp_server->server-server        = servername.
        mcp_server->server-http_request  = server->request.
        mcp_server->server-http_response = server->response.
        mcp_server->server-http_server   = server.
      ENDIF.
    ENDIF.

    IF continue = abap_false.
      RETURN.
    ENDIF.

    logger = mcp_server->config->get_logger( ).
    logger->info( |HTTP { method } for { area } { servername } received| ) ##NO_TEXT.

    " Except options call check origin header before any further steps if present
    IF method <> 'OPTIONS' AND mcp_server->server-cors_mode <> zcl_mcp_configuration=>cors_mode_ignore.
      DATA(origin) = server->request->get_header_field( 'Origin' ) ##NO_TEXT.
      IF origin IS INITIAL AND mcp_server->server-cors_mode = zcl_mcp_configuration=>cors_mode_enforce.
        server->response->set_status( code   = 400
                                      reason = 'Origin Header Missing' ) ##NO_TEXT.
        logger->warning( |Origin header missing for { area } { servername }| ) ##NO_TEXT.
        continue = abap_false.
      ENDIF.
      IF origin IS NOT INITIAL AND origin_allowed( area   = area
                                                   server = servername
                                                   origin = origin ) = abap_false.
        server->response->set_status( code   = 403
                                      reason = 'Forbidden' ) ##NO_TEXT.
        logger->warning( |Origin { origin } not allowed for { area } { servername }| ) ##NO_TEXT.
        continue = abap_false.
      ENDIF.
    ENDIF.

    IF continue = abap_false.
      logger->save( ).
      RETURN.
    ENDIF.

    " Handle the request based on HTTP method
    CASE method.
      WHEN 'POST'.
        handle_post( request  = server->request
                     response = server->response
                     area     = area
                     server   = servername ).

      WHEN 'GET'.
        handle_get( request  = server->request
                    response = server->response
                    area     = area
                    server   = servername ).

      WHEN 'DELETE'.
        handle_delete( request  = server->request
                       response = server->response
                       area     = area
                       server   = servername ).
      WHEN 'OPTIONS'.
        handle_options( request  = server->request
                        response = server->response
                        area     = area
                        server   = servername ).
      WHEN OTHERS.
        " Method not allowed
        server->response->set_status( code   = 405
                                      reason = 'Method Not Allowed' ) ##NO_TEXT.
        server->response->set_header_field( name  = 'Allow'
                                            value = 'POST, DELETE' ) ##NO_TEXT.
    ENDCASE.

    IF mcp_server->server-session_mode = zcl_mcp_session=>session_mode_mcp AND method = 'POST' AND mcp_server->session IS BOUND.
      TRY.
          mcp_server->session->save( ).
        CATCH zcx_mcp_server INTO DATA(session_error).
          logger->error(
              |Session { mcp_server->server-session_id } save error for { area } { servername } details: { session_error->get_text( ) }| ) ##NO_TEXT.
      ENDTRY.
    ENDIF.

    IF mcp_server->server-session_id IS NOT INITIAL.
      server->response->set_header_field( name  = 'Mcp-Session-Id'
                                          value = CONV #( mcp_server->server-session_id ) ) ##NO_TEXT.
    ENDIF.
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
    DATA response TYPE zcl_mcp_jsonrpc=>response.
    DATA request  TYPE zcl_mcp_jsonrpc=>request.
    DATA error    TYPE zcl_mcp_jsonrpc=>error.

    " Parse the request(s)
    TRY.
        request = jsonrpc->parse_request( json ).
      CATCH zcx_mcp_ajson_error.
        " JSON parse error
        error-code    = jsonrpc->error_codes-parse_error.
        error-message = 'Invalid JSON' ##NO_TEXT.
        response = jsonrpc->create_error_response( id      = ''
                                                   code    = error-code
                                                   message = error-message ).
        result = jsonrpc->serialize_response( response ).
        logger->warning(
            |JSON parse error for { mcp_server->server-area } { mcp_server->server-server } details: { error-message }| ) ##NO_TEXT.
        RETURN.
    ENDTRY.

    " Get session id except for initialize
    IF request-method <> 'initialize'.
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
              ENDCASE.
              mcp_server->server-http_response->set_status( code   = 500
                                                            reason = 'Internal Error' ) ##NO_TEXT.
              RETURN.
          ENDTRY.
      ENDCASE.
    ENDIF.

    " Handle mcp-protocol-version header except for initialize
    IF request-method <> 'initialize'.
      " Determine protocol version from mcp-protocol-version header
      DATA(protocol_version) = mcp_server->server-http_request->get_header_field( 'Mcp-Protocol-Version' ) ##NO_TEXT.
      IF protocol_version IS INITIAL.
        mcp_server->server-protocol_version = zif_mcp_constants=>latest_protocol_version.
      ELSE.
        SPLIT zif_mcp_constants=>supported_protocol_versions AT `,` INTO TABLE DATA(supported_protocol_versions).
        IF line_exists( supported_protocol_versions[ table_line = protocol_version ] ).
          mcp_server->server-protocol_version = protocol_version.
        ELSE.
          " As per spec we must return a 400 error if we don't support the protocol version
          mcp_server->server-http_response->set_status( code   = 400
                                                        reason = 'Bad Request' ) ##NO_TEXT.
          RETURN.
        ENDIF.
      ENDIF.
    ENDIF.

    " Process all requests
    mcp_server->server-mcp_request = request.

    logger->info( |Processing request { request-method } for { mcp_server->server-area } { mcp_server->server-server }| ) ##NO_TEXT.

    TRY.
        CASE request-method.
          WHEN 'initialize'.
            DATA(initialize) = mcp_server->initialize( NEW zcl_mcp_req_initialize( request-params ) ).
            response-error  = initialize-error.
            response-result = initialize-result->zif_mcp_internal~generate_json( ).
          WHEN 'ping'.  " see: https://modelcontextprotocol.io/specification/2025-06-18/basic/utilities/ping
            CLEAR: response-error.
            response-result = zcl_mcp_ajson=>create_empty( ).
            response-result->touch_object( '' ).
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
          WHEN OTHERS.
            response-error-code    = -32601.
            response-error-message = |Method { request-method } not allowed.| ##NO_TEXT.
        ENDCASE.

      CATCH zcx_mcp_server INTO DATA(mcp_error).
        CASE mcp_error->if_t100_message~t100key.
          WHEN zcx_mcp_server=>invalid_arguments OR zcx_mcp_server=>prompt_name_invalid OR zcx_mcp_server=>required_params.
            response-error-code    = zcl_mcp_jsonrpc=>error_codes-invalid_params.
            response-error-message = mcp_error->get_text( ).
          WHEN OTHERS.
            response-error-code    = zcl_mcp_jsonrpc=>error_codes-internal_error.
            response-error-message = mcp_error->get_text( ).
        ENDCASE.
        logger->warning(
            |Error processing request { request-method } for { mcp_server->server-area } { mcp_server->server-server } details: { response-error-message }| ) ##NO_TEXT.
    ENDTRY.

    response-id      = request-id.
    response-jsonrpc = request-jsonrpc.

    result = jsonrpc->serialize_response( response ).
  ENDMETHOD.


  METHOD handle_options.
    DATA(origin) = request->get_header_field( 'Origin' ) ##NO_TEXT.
    DATA fields TYPE tihttpnvp.
    request->get_header_fields( CHANGING fields = fields ).
    IF origin IS INITIAL.
      response->set_status( code   = 400
                            reason = 'Origin Header Missing' ) ##NO_TEXT.
      RETURN.
    ENDIF.

    IF ( origin_allowed( origin = origin
                         area   = area
                         server = server ) ) = abap_false.
      response->set_status( code   = 403
                            reason = 'Forbidden' ) ##NO_TEXT.
      RETURN.
    ENDIF.

    response->set_header_field( name  = 'Access-Control-Allow-Origin'
                                value = origin ) ##NO_TEXT.
    response->set_status( code   = 200
                          reason = 'OK' ).

    DATA(request_methods) = request->get_header_field( 'Access-Control-Request-Method' ) ##NO_TEXT.
    IF request_methods IS NOT INITIAL.
      " As the current method is fine we just return all that we support
      response->set_header_field( name  = 'Access-Control-Allow-Methods'
                                  value = 'POST, DELETE, OPTIONS' ) ##NO_TEXT.
    ENDIF.

    response->set_header_field( name = 'Access-Control-Max-Age' value = '86400' ) ##NO_TEXT.
  ENDMETHOD.


  METHOD origin_allowed.
    DATA(origins) = mcp_server->config->get_allowed_origins( ).
    result = abap_false.
    LOOP AT origins ASSIGNING FIELD-SYMBOL(<origin>).
      IF origin CP <origin>.
        result = abap_true.
        EXIT.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.
