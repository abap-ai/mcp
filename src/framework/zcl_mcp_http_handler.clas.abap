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

    DATA mcp_server TYPE REF TO zif_mcp_server.

    METHODS validate_session_id
      IMPORTING session_id TYPE string
      RAISING   zcx_mcp_server.
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
        IF json_obj->exists( '/method' ) IS NOT INITIAL.
          IF json_obj->exists( '/id' ) IS NOT INITIAL.
            has_requests = abap_true.
          ELSE.
            has_notifs = abap_true.
          ENDIF.
          RETURN.
        ENDIF.

        " JSON-RPC response objects contain either result or error.
        IF json_obj->exists( '/result' ) IS NOT INITIAL OR json_obj->exists( '/error' ) IS NOT INITIAL.
          has_responses = abap_true.
          RETURN.
        ENDIF.

      CATCH zcx_mcp_ajson_error.
        " Invalid JSON is handled by the request processing/error path.
        RETURN.
      CATCH cx_root.                                  "#EC NEED_CX_ROOT
        " Defensive fallback: invalid input is handled by caller.
        RETURN.
    ENDTRY.
  ENDMETHOD.

  METHOD handle_delete.
    DATA session_id TYPE string.
            DATA temp1 TYPE sysuuid_c32.
            DATA session_error TYPE REF TO zcx_mcp_server.
    " Check if sessions are enabled
    IF mcp_server->server-session_mode = zcl_mcp_session=>session_mode_stateless.
      response->set_status( code   = 405
                            reason = 'Method Not Allowed' ) ##NO_TEXT.
      response->set_header_field( name  = 'Allow'
                                  value = 'POST, OPTIONS' ) ##NO_TEXT.
      RETURN.
    ENDIF.

    " Load the session
    
    session_id = mcp_server->server-http_request->get_header_field( 'Mcp-Session-Id' ) ##NO_TEXT.
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
            
            temp1 = session_id.
            CREATE OBJECT mcp_server->session TYPE zcl_mcp_session EXPORTING session_id = temp1 session_mode = mcp_server->server-session_mode create_new = abap_false.
            mcp_server->server-session_id = session_id.
            
          CATCH zcx_mcp_server INTO session_error.
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
    " We don't support streaming - return Method Not Allowed
    response->set_status( code   = 405
                          reason = 'Method Not Allowed' ) ##NO_TEXT.
    response->set_header_field( name  = 'Allow'
                                value = 'POST, DELETE, OPTIONS' ) ##NO_TEXT.
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
    DATA path       TYPE string.
    DATA method     TYPE string.
    DATA area       TYPE zmcp_area.
    DATA servername TYPE zmcp_server.
    DATA valid      TYPE abap_bool.
    DATA continue   TYPE abap_bool VALUE abap_true.
      DATA origin TYPE string.
          DATA session_error TYPE REF TO zcx_mcp_server.
      DATA temp2 TYPE string.

    " Create JSON-RPC parser instance if not exists
    IF jsonrpc IS INITIAL.
      CREATE OBJECT jsonrpc TYPE zcl_mcp_jsonrpc.
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
        server->response->set_header_field( name  = 'Content-Type'
                                            value = 'application/json' ) ##NO_TEXT.
        server->response->set_cdata( create_error_json( code    = zcl_mcp_jsonrpc=>error_codes-invalid_request
                                                        message = 'Invalid or expired MCP session'
                                                        json    = server->request->get_cdata( ) ) ) ##NO_TEXT.
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
      
      origin = server->request->get_header_field( 'Origin' ) ##NO_TEXT.
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

      IF continue = abap_true AND origin IS NOT INITIAL.
        set_cors_response_headers( origin   = origin
                                   response = server->response ).
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
                                            value = 'POST, DELETE, OPTIONS' ) ##NO_TEXT.
    ENDCASE.

    IF mcp_server->server-session_mode = zcl_mcp_session=>session_mode_mcp AND method = 'POST' AND mcp_server->session IS BOUND.
      TRY.
          mcp_server->session->save( ).
          
        CATCH zcx_mcp_server INTO session_error.
          logger->error(
              |Session { mcp_server->server-session_id } save error for { area } { servername } details: { session_error->get_text( ) }| ) ##NO_TEXT.
      ENDTRY.
    ENDIF.

    IF mcp_server->server-session_id IS NOT INITIAL.
      
      temp2 = mcp_server->server-session_id.
      server->response->set_header_field( name  = 'Mcp-Session-Id'
                                          value = temp2 ) ##NO_TEXT.
    ENDIF.
    IF mcp_server->server-protocol_version IS NOT INITIAL.
      server->response->set_header_field( name  = 'Mcp-Protocol-Version'
                                          value = mcp_server->server-protocol_version ) ##NO_TEXT.
    ENDIF.
    logger->info( |HTTP { method } for { area } { servername } completed| ) ##NO_TEXT.
    logger->save( ).
  ENDMETHOD.


  METHOD parse_mcp_path.
    DATA path_parts TYPE TABLE OF string.
    DATA count      TYPE i.
      DATA temp3 LIKE LINE OF path_parts.
      DATA temp4 LIKE sy-tabix.
      DATA temp5 LIKE LINE OF path_parts.
      DATA temp6 LIKE sy-tabix.

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
      
      
      temp4 = sy-tabix.
      READ TABLE path_parts INDEX 1 INTO temp3.
      sy-tabix = temp4.
      IF sy-subrc <> 0.
        RAISE EXCEPTION TYPE cx_sy_itab_line_not_found.
      ENDIF.
      area   = temp3.
      
      
      temp6 = sy-tabix.
      READ TABLE path_parts INDEX 2 INTO temp5.
      sy-tabix = temp6.
      IF sy-subrc <> 0.
        RAISE EXCEPTION TYPE cx_sy_itab_line_not_found.
      ENDIF.
      server = temp5.
      valid  = abap_true.
    ENDIF.
  ENDMETHOD.

  METHOD process_request.
    DATA response TYPE zcl_mcp_jsonrpc=>response.
    DATA request  TYPE zcl_mcp_jsonrpc=>request.
    DATA error    TYPE zcl_mcp_jsonrpc=>error.

    DATA json_obj TYPE REF TO zif_mcp_ajson.
    DATA error_id TYPE string.
    DATA error_id_present TYPE abap_bool.
      DATA session_id TYPE string.
              DATA temp7 TYPE sysuuid_c32.
              DATA session_error TYPE REF TO zcx_mcp_server.
      DATA protocol_version TYPE string.
          DATA protocol_entry TYPE zcl_mcp_session=>session_entry.
        DATA supported_protocol_versions TYPE STANDARD TABLE OF string WITH DEFAULT KEY.
        DATA temp8 LIKE sy-subrc.
            DATA initialize TYPE zif_mcp_server=>initialize_response.
            DATA temp1 TYPE REF TO zcl_mcp_req_initialize.
            DATA list_prompts TYPE zif_mcp_server=>list_prompts_response.
            DATA temp2 TYPE REF TO zcl_mcp_req_list_prompts.
            DATA get_prompt TYPE zif_mcp_server=>get_prompt_response.
            DATA temp3 TYPE REF TO zcl_mcp_req_get_prompt.
            DATA list_resources TYPE zif_mcp_server=>list_resources_response.
            DATA temp4 TYPE REF TO zcl_mcp_req_list_resources.
            DATA list_res_tmpl TYPE zif_mcp_server=>list_resources_tmpl_response.
            DATA temp5 TYPE REF TO zcl_mcp_req_list_res_tmpls.
            DATA read_resource TYPE zif_mcp_server=>resources_read_response.
            DATA temp6 TYPE REF TO zcl_mcp_req_read_resource.
            DATA list_tools TYPE zif_mcp_server=>list_tools_response.
            DATA temp9 TYPE REF TO zcl_mcp_req_list_tools.
            DATA call_tool TYPE zif_mcp_server=>call_tool_response.
            DATA temp10 TYPE REF TO zcl_mcp_req_call_tool.
            DATA list_tasks TYPE zif_mcp_server=>list_tasks_response.
            DATA temp11 TYPE REF TO zcl_mcp_req_list_tasks.
            DATA get_task TYPE zif_mcp_server=>get_task_response.
            DATA temp12 TYPE REF TO zcl_mcp_req_get_task.
            DATA task_result TYPE zif_mcp_server=>get_task_payload_response.
            DATA temp13 TYPE REF TO zcl_mcp_req_get_task_payload.
            DATA cancel_task TYPE zif_mcp_server=>cancel_task_response.
            DATA temp14 TYPE REF TO zcl_mcp_req_cancel_task.
            DATA complete TYPE zif_mcp_server=>complete_response.
            DATA temp15 TYPE REF TO zcl_mcp_req_complete.
        DATA mcp_error TYPE REF TO zcx_mcp_server.
          DATA err_result TYPE REF TO zcl_mcp_resp_call_tool.

    TRY.
        json_obj = zcl_mcp_ajson=>parse( json ).
      CATCH zcx_mcp_ajson_error.
        error-code    = zcl_mcp_jsonrpc=>error_codes-parse_error.
        error-message = 'Invalid JSON' ##NO_TEXT.
        response = jsonrpc->create_error_response( id      = ''
                                                   code    = error-code
                                                   message = error-message ).
        response-id_is_null = abap_true.
        result = jsonrpc->serialize_response( response ).
        RETURN.
    ENDTRY.

    

    IF json_obj->exists( '/id' ) IS NOT INITIAL.
      CASE json_obj->get_node_type( '/id' ).
        WHEN 'str' OR 'num'.
          error_id = json_obj->get_string( '/id' ).
          error_id_present = abap_true.
      ENDCASE.
    ENDIF.

    IF    json_obj->get_string( '/jsonrpc' ) <> zcl_mcp_jsonrpc=>jsonrpc_version
       OR json_obj->exists( '/method' )       = abap_false
       OR json_obj->get_string( '/method' )  IS INITIAL.

      error-code    = zcl_mcp_jsonrpc=>error_codes-invalid_request.
      error-message = 'Invalid Request' ##NO_TEXT.
      response = jsonrpc->create_error_response( id      = error_id
                                                 code    = error-code
                                                 message = error-message ).
      response-id_present = error_id_present.
      response-id_is_null = abap_false.
      result = jsonrpc->serialize_response( response ).
      RETURN.
    ENDIF.

    " Parse the request(s)
    TRY.
        request = jsonrpc->parse_request( json ).
        IF request-id_present = abap_false.
          " Notifications do not receive JSON-RPC responses.
          result = ``.
          RETURN.
        ENDIF.
      CATCH zcx_mcp_ajson_error.
        error-code    = zcl_mcp_jsonrpc=>error_codes-invalid_request.
        error-message = 'Invalid Request' ##NO_TEXT.
        response = jsonrpc->create_error_response( id      = error_id
                                                   code    = error-code
                                                   message = error-message ).
        response-id_present = error_id_present.
        response-id_is_null = abap_false.
        result = jsonrpc->serialize_response( response ).
        RETURN.
    ENDTRY.

    " Get session id except for initialize
    IF request-method <> 'initialize'.
      
      session_id = mcp_server->server-http_request->get_header_field( 'Mcp-Session-Id' ) ##NO_TEXT.
      IF mcp_server->server-session_mode <> zcl_mcp_session=>session_mode_stateless AND session_id IS INITIAL.
        mcp_server->server-http_response->set_status( code   = 400
                                                      reason = 'Bad Request' ) ##NO_TEXT.
        response = jsonrpc->create_error_response( id      = request-id
                                                   code    = zcl_mcp_jsonrpc=>error_codes-invalid_request
                                                   message = 'Missing Mcp-Session-Id' ) ##NO_TEXT.
        response-id_present = request-id_present.
        response-jsonrpc    = request-jsonrpc.
        result = jsonrpc->serialize_response( response ).
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
            result = jsonrpc->serialize_response( response ).
            RETURN.
          ENDIF.
        WHEN zcl_mcp_session=>session_mode_mcp.
          TRY.
              validate_session_id( session_id ).
              
              temp7 = session_id.
              CREATE OBJECT mcp_server->session TYPE zcl_mcp_session EXPORTING session_id = temp7 session_mode = mcp_server->server-session_mode create_new = abap_false.
              mcp_server->server-session_id = session_id.
              
            CATCH zcx_mcp_server INTO session_error.
              CASE session_error->if_t100_message~t100key.
                WHEN zcx_mcp_server=>session_unknown OR zcx_mcp_server=>session_expired.
                  mcp_server->server-http_response->set_status( code   = 404
                                                                reason = 'Not Found' ) ##NO_TEXT.
                  response = jsonrpc->create_error_response( id      = request-id
                                                             code    = zcl_mcp_jsonrpc=>error_codes-invalid_request
                                                             message = 'Invalid or expired MCP session' ) ##NO_TEXT.
                  response-id_present = request-id_present.
                  response-jsonrpc    = request-jsonrpc.
                  result = jsonrpc->serialize_response( response ).
                  RETURN.
                WHEN zcx_mcp_server=>session_load_error.
                  logger->error(
                      |Session { session_id } load error for { mcp_server->server-area } { mcp_server->server-server } details: { session_error->get_text( ) }| ) ##NO_TEXT.
                  mcp_server->server-http_response->set_status( code   = 500
                                                                reason = 'Internal Error' ) ##NO_TEXT.
                  response = jsonrpc->create_error_response( id      = request-id
                                                             code    = zcl_mcp_jsonrpc=>error_codes-internal_error
                                                             message = session_error->get_text( ) ).
                  response-id_present = request-id_present.
                  response-jsonrpc    = request-jsonrpc.
                  result = jsonrpc->serialize_response( response ).
                  RETURN.
              ENDCASE.
              mcp_server->server-http_response->set_status( code   = 500
                                                            reason = 'Internal Error' ) ##NO_TEXT.
              response = jsonrpc->create_error_response( id      = request-id
                                                         code    = zcl_mcp_jsonrpc=>error_codes-internal_error
                                                         message = session_error->get_text( ) ).
              response-id_present = request-id_present.
              response-jsonrpc    = request-jsonrpc.
              result = jsonrpc->serialize_response( response ).
              RETURN.
          ENDTRY.
      ENDCASE.
    ENDIF.

    " Handle mcp-protocol-version header except for initialize
    IF request-method <> 'initialize'.
      " Determine protocol version from mcp-protocol-version header
      
      protocol_version = mcp_server->server-http_request->get_header_field( 'Mcp-Protocol-Version' ) ##NO_TEXT.
      IF protocol_version IS INITIAL.
        IF mcp_server->server-protocol_version IS NOT INITIAL.
          " Stateful ICF mode can keep the negotiated version in the server instance.
          protocol_version = mcp_server->server-protocol_version.
        ELSEIF mcp_server->session IS BOUND.
          " MCP session mode can restore the negotiated version from persisted session data.
          
          protocol_entry = mcp_server->session->get( 'protocolVersion' ).
          protocol_version = protocol_entry-value.
        ENDIF.

        IF protocol_version IS INITIAL.
          " Streamable HTTP default when no negotiated version can be identified.
          protocol_version = zif_mcp_constants=>protocol_version_2025_03_26.
        ENDIF.

        mcp_server->server-protocol_version = protocol_version.
      ELSE.
        
        SPLIT zif_mcp_constants=>supported_protocol_versions AT `,` INTO TABLE supported_protocol_versions.
        
        READ TABLE supported_protocol_versions WITH KEY table_line = protocol_version TRANSPORTING NO FIELDS.
        temp8 = sy-subrc.
        IF temp8 = 0.
          mcp_server->server-protocol_version = protocol_version.
        ELSE.
          " As per spec we must return a 400 error if we don't support the protocol version
          mcp_server->server-http_response->set_status( code   = 400
                                                        reason = 'Bad Request' ) ##NO_TEXT.
          response = jsonrpc->create_error_response( id      = request-id
                                                     code    = zcl_mcp_jsonrpc=>error_codes-invalid_request
                                                     message = |Unsupported Mcp-Protocol-Version { protocol_version }| ) ##NO_TEXT.
          response-id_present = request-id_present.
          response-jsonrpc    = request-jsonrpc.
          result = jsonrpc->serialize_response( response ).
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
            
            
            CREATE OBJECT temp1 TYPE zcl_mcp_req_initialize EXPORTING JSON = request-params.
            initialize = mcp_server->initialize( temp1 ).
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
            
            
            CREATE OBJECT temp2 TYPE zcl_mcp_req_list_prompts EXPORTING JSON = request-params.
            list_prompts = mcp_server->prompts_list( temp2 ).
            response-error  = list_prompts-error.
            response-result = list_prompts-result->zif_mcp_internal~generate_json( ).
          WHEN 'prompts/get'.
            
            
            CREATE OBJECT temp3 TYPE zcl_mcp_req_get_prompt EXPORTING JSON = request-params.
            get_prompt = mcp_server->prompts_get( temp3 ).
            response-error  = get_prompt-error.
            response-result = get_prompt-result->zif_mcp_internal~generate_json( ).
          WHEN 'resources/list'.
            
            
            CREATE OBJECT temp4 TYPE zcl_mcp_req_list_resources EXPORTING JSON = request-params.
            list_resources = mcp_server->resources_list( temp4 ).
            response-error  = list_resources-error.
            response-result = list_resources-result->zif_mcp_internal~generate_json( ).
          WHEN 'resources/templates/list'.
            
            
            CREATE OBJECT temp5 TYPE zcl_mcp_req_list_res_tmpls EXPORTING JSON = request-params.
            list_res_tmpl = mcp_server->resources_templates_list( temp5 ).
            response-error  = list_res_tmpl-error.
            response-result = list_res_tmpl-result->zif_mcp_internal~generate_json( ).
          WHEN 'resources/read'.
            
            
            CREATE OBJECT temp6 TYPE zcl_mcp_req_read_resource EXPORTING JSON = request-params.
            read_resource = mcp_server->resources_read( temp6 ).
            response-error  = read_resource-error.
            response-result = read_resource-result->zif_mcp_internal~generate_json( ).
          WHEN 'tools/list'.
            
            
            CREATE OBJECT temp9 TYPE zcl_mcp_req_list_tools EXPORTING JSON = request-params.
            list_tools = mcp_server->tools_list( temp9 ).
            response-error  = list_tools-error.
            response-result = list_tools-result->zif_mcp_internal~generate_json( ).
          WHEN 'tools/call'.
            
            
            CREATE OBJECT temp10 TYPE zcl_mcp_req_call_tool EXPORTING JSON = request-params.
            call_tool = mcp_server->tools_call( temp10 ).
            response-error  = call_tool-error.
            response-result = call_tool-result->zif_mcp_internal~generate_json( ).
          WHEN 'tasks/list'.
            
            
            CREATE OBJECT temp11 TYPE zcl_mcp_req_list_tasks EXPORTING JSON = request-params.
            list_tasks = mcp_server->tasks_list( temp11 ).
            response-error  = list_tasks-error.
            response-result = list_tasks-result->zif_mcp_internal~generate_json( ).
          WHEN 'tasks/get'.
            
            
            CREATE OBJECT temp12 TYPE zcl_mcp_req_get_task EXPORTING JSON = request-params.
            get_task = mcp_server->tasks_get( temp12 ).
            response-error  = get_task-error.
            response-result = get_task-result->zif_mcp_internal~generate_json( ).
          WHEN 'tasks/result'.
            
            
            CREATE OBJECT temp13 TYPE zcl_mcp_req_get_task_payload EXPORTING JSON = request-params.
            task_result = mcp_server->tasks_result( temp13 ).
            response-error  = task_result-error.
            response-result = task_result-result->zif_mcp_internal~generate_json( ).
          WHEN 'tasks/cancel'.
            
            
            CREATE OBJECT temp14 TYPE zcl_mcp_req_cancel_task EXPORTING JSON = request-params.
            cancel_task = mcp_server->tasks_cancel( temp14 ).
            response-error  = cancel_task-error.
            response-result = cancel_task-result->zif_mcp_internal~generate_json( ).
          WHEN 'completion/complete'.
            
            
            CREATE OBJECT temp15 TYPE zcl_mcp_req_complete EXPORTING JSON = request-params.
            complete = mcp_server->completions_complete( temp15 ).
            response-error  = complete-error.
            response-result = complete-result->zif_mcp_internal~generate_json( ).
          WHEN OTHERS.
            response-error-code    = -32601.
            response-error-message = |Method { request-method } not found.| ##NO_TEXT.
        ENDCASE.

        
      CATCH zcx_mcp_server INTO mcp_error.
        IF     request-method = 'tools/call'
           AND mcp_error->if_t100_message~t100key = zcx_mcp_server=>invalid_arguments.
          
          CREATE OBJECT err_result TYPE zcl_mcp_resp_call_tool.
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

    response-id         = request-id.
    response-id_present = request-id_present.
    response-jsonrpc    = request-jsonrpc.

    result = jsonrpc->serialize_response( response ).
  ENDMETHOD.

  METHOD handle_options.
    DATA origin TYPE string.
    DATA fields TYPE tihttpnvp.
    DATA request_methods TYPE string.
      DATA request_headers TYPE string.
    origin = request->get_header_field( 'Origin' ) ##NO_TEXT.
    
    request->get_header_fields( CHANGING fields = fields ).
    IF origin IS INITIAL.
      response->set_status( code   = 400
                            reason = 'Origin Header Missing' ) ##NO_TEXT.
      RETURN.
    ENDIF.

    IF origin_allowed( origin = origin
                       area   = area
                       server = server ) = abap_false.
      response->set_status( code   = 403
                            reason = 'Forbidden' ) ##NO_TEXT.
      RETURN.
    ENDIF.

    set_cors_response_headers( origin   = origin
                               response = response ).
    response->set_status( code   = 200
                          reason = 'OK' ).

    
    request_methods = request->get_header_field( 'Access-Control-Request-Method' ) ##NO_TEXT.
    IF request_methods IS NOT INITIAL.
      " As the current method is fine we just return all that we support
      response->set_header_field( name  = 'Access-Control-Allow-Methods'
                                  value = 'POST, DELETE, OPTIONS' ) ##NO_TEXT.
      
      request_headers = request->get_header_field( 'Access-Control-Request-Headers' ) ##NO_TEXT.

      IF request_headers IS NOT INITIAL.
        response->set_header_field( name  = 'Access-Control-Allow-Headers'
                                    value = request_headers ) ##NO_TEXT.
      ELSE.
        response->set_header_field(
            name  = 'Access-Control-Allow-Headers'
            value = 'Content-Type, Accept, Authorization, Mcp-Session-Id, Mcp-Protocol-Version' ) ##NO_TEXT.
      ENDIF.
    ENDIF.

    response->set_header_field( name  = 'Access-Control-Max-Age'
                                value = '86400' ) ##NO_TEXT.
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
    DATA origins TYPE zcl_mcp_configuration=>origins.
    FIELD-SYMBOLS <origin> LIKE LINE OF origins.
    origins = mcp_server->config->get_allowed_origins( ).
    result = abap_false.
    
    LOOP AT origins ASSIGNING <origin>.
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

          IF json_obj->exists( '/id' ) IS NOT INITIAL.
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
      DATA temp9 TYPE symsgv.
      DATA temp16 TYPE REF TO zcx_mcp_server.
      DATA temp10 TYPE symsgv.
      DATA temp17 TYPE REF TO zcx_mcp_server.
      DATA temp11 TYPE symsgv.
      DATA temp18 TYPE REF TO zcx_mcp_server.
    IF session_id IS INITIAL.
      
      temp9 = session_id.
      
      CREATE OBJECT temp16 TYPE zcx_mcp_server EXPORTING textid = zcx_mcp_server=>session_unknown msgv1 = temp9.
      RAISE EXCEPTION temp16.
    ENDIF.

    IF strlen( session_id ) <> 32.
      
      temp10 = session_id.
      
      CREATE OBJECT temp17 TYPE zcx_mcp_server EXPORTING textid = zcx_mcp_server=>session_unknown msgv1 = temp10.
      RAISE EXCEPTION temp17.
    ENDIF.

    FIND REGEX '^[0-9A-Fa-f]{32}$' IN session_id.
    IF sy-subrc <> 0.
      
      temp11 = session_id.
      
      CREATE OBJECT temp18 TYPE zcx_mcp_server EXPORTING textid = zcx_mcp_server=>session_unknown msgv1 = temp11.
      RAISE EXCEPTION temp18.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
