"! <p class="shorttext synchronized">MCP draft JSON-RPC router</p>
"! Dispatches parsed JSON-RPC requests to ZIF_MCP_SERVER_V2 implementations.
CLASS zcl_mcp_modern_router DEFINITION
PUBLIC FINAL
CREATE PUBLIC.

  PUBLIC SECTION.
    "! <p class="shorttext synchronized">Route a draft MCP request</p>
    "! Calls the matching ZIF_MCP_SERVER_V2 method and wraps the result or
    "! error in a JSON-RPC response.
    "!
    "! @parameter server   | <p class="shorttext synchronized">Draft MCP server instance</p>
    "! @parameter request  | <p class="shorttext synchronized">Parsed JSON-RPC request</p>
    "! @parameter response | <p class="shorttext synchronized">JSON-RPC response</p>
    CLASS-METHODS route_request
      IMPORTING server          TYPE REF TO zif_mcp_server_v2
                !request        TYPE zcl_mcp_jsonrpc=>request
      RETURNING VALUE(response) TYPE zcl_mcp_jsonrpc=>response.

  PRIVATE SECTION.
    CONSTANTS c_error_param_header TYPE symsgv VALUE 'PARAM_HEADER'.

    CLASS-METHODS apply_v2_response
      IMPORTING server      TYPE REF TO zif_mcp_server_v2
                v2_response TYPE zif_mcp_server_v2=>v2_response
                !request    TYPE zcl_mcp_jsonrpc=>request
      CHANGING  !response   TYPE zcl_mcp_jsonrpc=>response.

    CLASS-METHODS error_response
      IMPORTING !request        TYPE zcl_mcp_jsonrpc=>request
                !code           TYPE i
                !message        TYPE string
      RETURNING VALUE(response) TYPE zcl_mcp_jsonrpc=>response.

    CLASS-METHODS exception_response
      IMPORTING !request        TYPE zcl_mcp_jsonrpc=>request
                !error          TYPE REF TO cx_root
      RETURNING VALUE(response) TYPE zcl_mcp_jsonrpc=>response.

    "! <p class="shorttext synchronized">Validate mirrored tool parameter headers</p>
    "! Checks Mcp-Param-* headers for tool input schema properties marked with x-mcp-header.
    "!
    "! @parameter server              | <p class="shorttext synchronized">Draft MCP server instance</p>
    "! @parameter call_request        | <p class="shorttext synchronized">Parsed tool call request</p>
    "! @raising   zcx_mcp_server      | <p class="shorttext synchronized">Header mismatch</p>
    "! @raising   zcx_mcp_ajson_error | <p class="shorttext synchronized">JSON access error</p>
    CLASS-METHODS validate_param_headers
      IMPORTING server       TYPE REF TO zif_mcp_server_v2
                call_request TYPE REF TO zcl_mcp_req_call_tool
      RAISING   zcx_mcp_server
                zcx_mcp_ajson_error.

    "! <p class="shorttext synchronized">Convert argument to header text</p>
    "! Converts a primitive JSON argument value to the corresponding header string.
    "!
    "! @parameter arguments | <p class="shorttext synchronized">Tool arguments JSON</p>
    "! @parameter path      | <p class="shorttext synchronized">Argument JSON path</p>
    "! @parameter result    | <p class="shorttext synchronized">Header comparison value</p>
    CLASS-METHODS argument_to_header_value
      IMPORTING arguments     TYPE REF TO zif_mcp_ajson
                !path         TYPE string
      RETURNING VALUE(result) TYPE string.

    TYPES: BEGIN OF param_header,
             property      TYPE string,
             property_type TYPE string,
             header_suffix TYPE string,
             header_name   TYPE string,
           END OF param_header,
           param_headers TYPE STANDARD TABLE OF param_header WITH EMPTY KEY.

    "! <p class="shorttext synchronized">Collect mirrored parameter headers</p>
    "! Validates x-mcp-header annotations and returns top-level header mappings.
    CLASS-METHODS collect_param_headers
      IMPORTING !schema       TYPE REF TO zif_mcp_ajson
      RETURNING VALUE(result) TYPE param_headers
      RAISING   zcx_mcp_server
                zcx_mcp_ajson_error.

    "! <p class="shorttext synchronized">Check HTTP field-name token</p>
    CLASS-METHODS is_valid_header_suffix
      IMPORTING !suffix       TYPE string
      RETURNING VALUE(result) TYPE abap_bool.

    "! <p class="shorttext synchronized">Find nested x-mcp-header annotations</p>
    CLASS-METHODS has_x_mcp_header_at_or_below
      IMPORTING !schema       TYPE REF TO zif_mcp_ajson
                !path         TYPE string
      RETURNING VALUE(result) TYPE abap_bool
      RAISING   zcx_mcp_ajson_error.

    CLASS-METHODS normalize_header_value
      IMPORTING header_name   TYPE string
                header_value  TYPE string
      RETURNING VALUE(result) TYPE string
      RAISING   zcx_mcp_server.

    CLASS-METHODS assert_header_value_safe
      IMPORTING header_name  TYPE string
                header_value TYPE string
      RAISING   zcx_mcp_server.

    CLASS-METHODS compare_param_header
      IMPORTING header_name   TYPE string
                header_value  TYPE string
                body_value    TYPE string
                property_type TYPE string
      RAISING   zcx_mcp_server.

    CLASS-METHODS assert_safe_integer_text
      IMPORTING header_name TYPE string
                !value      TYPE string
      RAISING   zcx_mcp_server.
ENDCLASS.


CLASS zcl_mcp_modern_router IMPLEMENTATION.
  METHOD route_request.
    DATA v2_response TYPE zif_mcp_server_v2=>v2_response.

    IF server IS NOT BOUND.
      response = error_response( request = request
                                 code    = zcl_mcp_jsonrpc=>error_codes-internal_error
                                 message = 'Draft MCP server is not available' ) ##NO_TEXT.
      RETURN.
    ENDIF.

    TRY.
        CASE request-method.
          WHEN 'server/discover'.
            v2_response = server->server_discover( ).

          WHEN 'prompts/list'.
            v2_response = server->prompts_list( NEW zcl_mcp_req_list_prompts( request-params ) ).

          WHEN 'prompts/get'.
            v2_response = server->prompts_get( NEW zcl_mcp_req_get_prompt( request-params ) ).

          WHEN 'resources/list'.
            v2_response = server->resources_list( NEW zcl_mcp_req_list_resources( request-params ) ).

          WHEN 'resources/read'.
            v2_response = server->resources_read( NEW zcl_mcp_req_read_resource( request-params ) ).

          WHEN 'resources/templates/list'.
            v2_response = server->resources_tmpls_list( NEW zcl_mcp_req_list_res_tmpls( request-params ) ).

          WHEN 'tools/list'.
            v2_response = server->tools_list( NEW zcl_mcp_req_list_tools( request-params ) ).

          WHEN 'tools/call'.
            DATA(call_request) = NEW zcl_mcp_req_call_tool( request-params ).

            TRY.
                validate_param_headers( server       = server
                                        call_request = call_request ).
              CATCH zcx_mcp_server INTO DATA(header_error).
                response = error_response( request = request
                                           code    = COND #( WHEN header_error->msgv2 = c_error_param_header
                                                             THEN zcl_mcp_jsonrpc=>error_codes-header_mismatch
                                                             ELSE zcl_mcp_jsonrpc=>error_codes-invalid_params )
                                           message = header_error->get_text( ) ).
                RETURN.
            ENDTRY.

            v2_response = server->tools_call( call_request ).

          WHEN 'completion/complete'.
            v2_response = server->completions_complete( NEW zcl_mcp_req_complete( request-params ) ).

          WHEN 'tasks/get'.
            v2_response = server->tasks_get( NEW zcl_mcp_req_get_task( request-params ) ).

          WHEN 'tasks/update'.
            v2_response = server->tasks_update( NEW zcl_mcp_req_update_task( request-params ) ).

          WHEN 'tasks/cancel'.
            v2_response = server->tasks_cancel( NEW zcl_mcp_req_cancel_task( request-params ) ).

          WHEN OTHERS.
            response = error_response( request = request
                                       code    = zcl_mcp_jsonrpc=>error_codes-method_not_found
                                       message = |Method { request-method } not found.| ).
            RETURN.
        ENDCASE.

        apply_v2_response( EXPORTING server      = server
                                     v2_response = v2_response
                                     request     = request
                           CHANGING  response    = response ).
      CATCH zcx_mcp_server INTO DATA(mcp_error).
        response = error_response( request = request
                                   code    = zcl_mcp_jsonrpc=>error_codes-invalid_params
                                   message = mcp_error->get_text( ) ).

      CATCH zcx_mcp_ajson_error INTO DATA(json_error).
        response = error_response( request = request
                                   code    = zcl_mcp_jsonrpc=>error_codes-invalid_params
                                   message = json_error->get_text( ) ).

      CATCH cx_root INTO DATA(root_error) ##CATCH_ALL.
        response = exception_response( request = request
                                       error   = root_error ).
    ENDTRY.
  ENDMETHOD.

  METHOD apply_v2_response.
    DATA context    TYPE zif_mcp_server_v2=>v2_context.
    DATA error_data TYPE REF TO zif_mcp_ajson.

    response-jsonrpc    = request-jsonrpc.
    response-id         = request-id.
    response-id_present = request-id_present.

    IF    v2_response-error-code    IS NOT INITIAL
       OR v2_response-error-message IS NOT INITIAL.
      response-error = v2_response-error.
      RETURN.
    ENDIF.

    IF v2_response-result IS NOT BOUND.
      response-error-code    = zcl_mcp_jsonrpc=>error_codes-internal_error.
      response-error-message = |Method { request-method } did not return a result.| ##NO_TEXT.
      RETURN.
    ENDIF.

    IF v2_response-result->get_string( '/resultType' ) = zif_mcp_constants=>result_types-task.
      context = server->get_v2_context( ).

      IF    context-extensions IS NOT BOUND
         OR context-extensions->exists( '/io.modelcontextprotocol~1tasks' )  = abap_false.

        response-error-code    = zcl_mcp_jsonrpc=>error_codes-missing_client_capability.
        response-error-message = `Client did not declare io.modelcontextprotocol/tasks.`.

        TRY.
            error_data = zcl_mcp_ajson=>create_empty( ).
            error_data->touch_array( '/requiredCapabilities' ).
            error_data->set_string( iv_path = '/requiredCapabilities/1'
                                    iv_val  = zif_mcp_constants=>extensions-tasks ).
            response-error-data = error_data.
          CATCH zcx_mcp_ajson_error.
            CLEAR response-error-data.
        ENDTRY.

        RETURN.
      ENDIF.
    ENDIF.

    response-result = v2_response-result.
  ENDMETHOD.

  METHOD error_response.
    response-jsonrpc    = request-jsonrpc.
    response-id         = request-id.
    response-id_present = request-id_present.
    response-error-code    = code.
    response-error-message = message.
  ENDMETHOD.

  METHOD exception_response.
    response = error_response( request = request
                               code    = zcl_mcp_jsonrpc=>error_codes-internal_error
                               message = error->get_text( ) ).
  ENDMETHOD.

  METHOD validate_param_headers.
    DATA context       TYPE zif_mcp_server_v2=>v2_context.
    DATA schema        TYPE REF TO zif_mcp_ajson.
    DATA arguments     TYPE REF TO zif_mcp_ajson.
    DATA headers       TYPE param_headers.
    DATA argument_path TYPE string.
    DATA header_value  TYPE string.
    DATA body_value    TYPE string.

    context = server->get_v2_context( ).

    IF context-http_request IS NOT BOUND.
      RETURN.
    ENDIF.

    schema = server->get_tool_input_schema( call_request->get_name( ) ).

    IF schema IS NOT BOUND OR schema->exists( '/properties' ) = abap_false.
      RETURN.
    ENDIF.

    arguments = call_request->get_arguments( ).

    TRY.
        headers = collect_param_headers( schema ).

        LOOP AT headers INTO DATA(header_param).
          argument_path = |/{ header_param-property }|.

          IF arguments IS NOT BOUND OR arguments->exists( argument_path ) = abap_false.
            header_value = context-http_request->get_header_field( header_param-header_name ) ##NO_TEXT.

            IF header_value IS NOT INITIAL.
              RAISE EXCEPTION NEW zcx_mcp_server(
                  textid = zcx_mcp_server=>invalid_arguments
                  msgv1  = CONV #( |{ header_param-header_name } supplied but argument { argument_path } is missing| ) ).
            ENDIF.

            CONTINUE.
          ENDIF.

          header_value = context-http_request->get_header_field( header_param-header_name ) ##NO_TEXT.

          IF header_value IS INITIAL.
            RAISE EXCEPTION NEW zcx_mcp_server( textid = zcx_mcp_server=>invalid_arguments
                                                msgv1  = CONV #( |Missing { header_param-header_name } header| ) ).
          ENDIF.

          body_value = argument_to_header_value( arguments = arguments
                                                 path      = argument_path ).

          compare_param_header( header_name   = header_param-header_name
                                header_value  = header_value
                                body_value    = body_value
                                property_type = header_param-property_type ).
        ENDLOOP.

      CATCH zcx_mcp_server INTO DATA(param_error).
        RAISE EXCEPTION NEW zcx_mcp_server( textid = zcx_mcp_server=>invalid_arguments
                                            msgv1  = param_error->msgv1
                                            msgv2  = c_error_param_header ).
    ENDTRY.
  ENDMETHOD.

  METHOD argument_to_header_value.
    CASE arguments->get_node_type( path ).
      WHEN 'bool'.
        IF arguments->get_boolean( path ) = abap_true.
          result = `true`.
        ELSE.
          result = `false`.
        ENDIF.

      WHEN 'num'.
        result = arguments->get_string( path ).

      WHEN OTHERS.
        result = arguments->get_string( path ).
    ENDCASE.
  ENDMETHOD.

  METHOD collect_param_headers.
    DATA members        TYPE string_table.
    DATA nested_members TYPE string_table.
    DATA seen_headers   TYPE string_table.
    DATA property_path  TYPE string.
    DATA nested_path    TYPE string.
    DATA property_type  TYPE string.
    DATA header_suffix  TYPE string.
    DATA header_key     TYPE string.
    DATA header_param   TYPE param_header.

    IF schema IS NOT BOUND OR schema->exists( '/properties' ) = abap_false.
      RETURN.
    ENDIF.

    members = schema->members( '/properties' ).

    LOOP AT members INTO DATA(property).
      property_path = |/properties/{ property }|.

      IF schema->exists( |{ property_path }/properties| ).
        nested_members = schema->members( |{ property_path }/properties| ).
        LOOP AT nested_members INTO DATA(nested_property).
          nested_path = |{ property_path }/properties/{ nested_property }|.
          IF has_x_mcp_header_at_or_below( schema = schema
                                           path   = nested_path ) = abap_true.
            RAISE EXCEPTION NEW zcx_mcp_server(
                textid = zcx_mcp_server=>invalid_arguments
                msgv1  = CONV #( |Nested x-mcp-header annotations are not supported: { nested_path }| ) ).
          ENDIF.
        ENDLOOP.
      ENDIF.

      IF     schema->exists( |{ property_path }/items| ) = abap_true
         AND has_x_mcp_header_at_or_below( schema = schema
                                           path   = |{ property_path }/items| ) = abap_true.
        RAISE EXCEPTION NEW zcx_mcp_server(
            textid = zcx_mcp_server=>invalid_arguments
            msgv1  = CONV #( |Nested x-mcp-header annotations are not supported: { property_path }/items| ) ).
      ENDIF.

      IF schema->exists( |{ property_path }/x-mcp-header| ) = abap_false.
        CONTINUE.
      ENDIF.

      header_suffix = schema->get_string( |{ property_path }/x-mcp-header| ).

      IF     header_suffix IS INITIAL
         AND schema->get_node_type( |{ property_path }/x-mcp-header| )  = 'bool'
         AND schema->get_boolean( |{ property_path }/x-mcp-header| )    = abap_true.
        header_suffix = property.
      ENDIF.

      property_type = schema->get_string( |{ property_path }/type| ).

      IF property_type <> 'string' AND property_type <> 'integer' AND property_type <> 'boolean'.
        RAISE EXCEPTION NEW zcx_mcp_server(
            textid = zcx_mcp_server=>invalid_arguments
            msgv1  = CONV #( |x-mcp-header is not allowed on { property_type } property { property }| ) ).
      ENDIF.

      IF is_valid_header_suffix( header_suffix ) = abap_false.
        RAISE EXCEPTION NEW zcx_mcp_server(
                                textid = zcx_mcp_server=>invalid_arguments
                                msgv1  = CONV #( |Invalid x-mcp-header value for { property }: { header_suffix }| ) ).
      ENDIF.

      header_key = header_suffix.
      header_key = to_lower( header_key ).

      IF line_exists( seen_headers[ table_line = header_key ] ).
        RAISE EXCEPTION NEW zcx_mcp_server( textid = zcx_mcp_server=>invalid_arguments
                                            msgv1  = CONV #( |Duplicate x-mcp-header value: { header_suffix }| ) ).
      ENDIF.

      APPEND header_key TO seen_headers.

      CLEAR header_param.
      header_param-property      = property.
      header_param-property_type = property_type.
      header_param-header_suffix = header_suffix.
      header_param-header_name   = |Mcp-Param-{ header_suffix }|.
      APPEND header_param TO result.
    ENDLOOP.
  ENDMETHOD.

  METHOD is_valid_header_suffix.
    DATA allowed TYPE string VALUE 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789!#$%&''*+-.^_`|~'.

    result = abap_false.

    IF suffix IS INITIAL.
      RETURN.
    ENDIF.

    IF suffix CO allowed.
      result = abap_true.
    ENDIF.
  ENDMETHOD.

  METHOD has_x_mcp_header_at_or_below.
    DATA members TYPE string_table.

    result = abap_false.

    IF schema IS NOT BOUND.
      RETURN.
    ENDIF.

    IF schema->exists( |{ path }/x-mcp-header| ) = abap_true.
      result = abap_true.
      RETURN.
    ENDIF.

    IF schema->exists( |{ path }/properties| ) = abap_true.
      members = schema->members( |{ path }/properties| ).
      LOOP AT members INTO DATA(member).
        IF has_x_mcp_header_at_or_below( schema = schema
                                         path   = |{ path }/properties/{ member }| ) = abap_true.
          result = abap_true.
          RETURN.
        ENDIF.
      ENDLOOP.
    ENDIF.

    IF     schema->exists( |{ path }/items| ) = abap_true
       AND has_x_mcp_header_at_or_below( schema = schema
                                         path   = |{ path }/items| ) = abap_true.
      result = abap_true.
    ENDIF.
  ENDMETHOD.

  METHOD normalize_header_value.
    DATA encoded     TYPE string.
    DATA decoded     TYPE xstring.
    DATA encoded_len TYPE i.
    DATA value_len   TYPE i.
    DATA end_offset  TYPE i.

    result = header_value.
    value_len = strlen( header_value ).

    IF value_len >= 11.
      end_offset = value_len - 2.

      IF     header_value(9)            = `=?base64?`
         AND header_value+end_offset(2) = `?=`.

        encoded_len = value_len - 11.
        encoded = header_value+9(encoded_len).

        IF    encoded IS INITIAL
           OR encoded CN `ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/=`.
          RAISE EXCEPTION NEW zcx_mcp_server( textid = zcx_mcp_server=>invalid_arguments
                                              msgv1  = CONV #( |Invalid { header_name } base64 value| ) ).
        ENDIF.

        CALL FUNCTION 'SCMS_BASE64_DECODE_STR'
          EXPORTING  input    = encoded
                     unescape = ``
          IMPORTING  output   = decoded
          EXCEPTIONS failed   = 1
                     OTHERS   = 2.

        IF sy-subrc <> 0.
          RAISE EXCEPTION NEW zcx_mcp_server( textid = zcx_mcp_server=>invalid_arguments
                                              msgv1  = CONV #( |Invalid { header_name } base64 value| ) ).
        ENDIF.

        TRY.
            result = cl_abap_codepage=>convert_from( source = decoded ).
          CATCH cx_root.
            RAISE EXCEPTION NEW zcx_mcp_server( textid = zcx_mcp_server=>invalid_arguments
                                                msgv1  = CONV #( |Invalid { header_name } base64 text| ) ).
        ENDTRY.
      ENDIF.
    ENDIF.

    assert_header_value_safe( header_name  = header_name
                              header_value = result ).
  ENDMETHOD.

  METHOD assert_header_value_safe.
    IF    header_value CS cl_abap_char_utilities=>newline
       OR header_value CS cl_abap_char_utilities=>cr_lf
       OR header_value CS cl_abap_char_utilities=>horizontal_tab.
      RAISE EXCEPTION NEW zcx_mcp_server( textid = zcx_mcp_server=>invalid_arguments
                                          msgv1  = CONV #( |Unsafe control character in { header_name }| ) ).
    ENDIF.
  ENDMETHOD.

  METHOD assert_safe_integer_text.
    DATA numeric_value TYPE decfloat34.
    CONSTANTS max_safe TYPE decfloat34 VALUE '9007199254740991'.
    CONSTANTS min_safe TYPE decfloat34 VALUE '-9007199254740991'.

    IF value IS INITIAL.
      RAISE EXCEPTION NEW zcx_mcp_server( textid = zcx_mcp_server=>invalid_arguments
                                          msgv1  = CONV #( |Invalid integer value in { header_name }| ) ).
    ENDIF.

    FIND REGEX `^-?[0-9]+$` IN value.
    IF sy-subrc <> 0.
      RAISE EXCEPTION NEW zcx_mcp_server( textid = zcx_mcp_server=>invalid_arguments
                                          msgv1  = CONV #( |Invalid integer value in { header_name }| ) ).
    ENDIF.

    TRY.
        numeric_value = CONV decfloat34( value ).
      CATCH cx_root.
        RAISE EXCEPTION NEW zcx_mcp_server( textid = zcx_mcp_server=>invalid_arguments
                                            msgv1  = CONV #( |Invalid integer value in { header_name }| ) ).
    ENDTRY.

    IF numeric_value > max_safe OR numeric_value < min_safe.
      RAISE EXCEPTION NEW zcx_mcp_server( textid = zcx_mcp_server=>invalid_arguments
                                          msgv1  = CONV #( |Unsafe integer value in { header_name }| ) ).
    ENDIF.
  ENDMETHOD.

  METHOD compare_param_header.
    DATA normalized_header TYPE string.
    DATA header_number     TYPE decfloat34.
    DATA body_number       TYPE decfloat34.

    normalized_header = normalize_header_value( header_name  = header_name
                                                header_value = header_value ).

    CASE property_type.
      WHEN `integer`.
        assert_safe_integer_text( header_name = header_name
                                  value       = normalized_header ).
        assert_safe_integer_text( header_name = header_name
                                  value       = body_value ).

        header_number = CONV decfloat34( normalized_header ).
        body_number   = CONV decfloat34( body_value ).

        IF header_number <> body_number.
          RAISE EXCEPTION NEW zcx_mcp_server(
              textid = zcx_mcp_server=>invalid_arguments
              msgv1  = CONV #( |{ header_name } mismatch: { normalized_header } <> { body_value }| ) ).
        ENDIF.

      WHEN `boolean`.
        IF normalized_header <> `true` AND normalized_header <> `false`.
          RAISE EXCEPTION NEW zcx_mcp_server( textid = zcx_mcp_server=>invalid_arguments
                                              msgv1  = CONV #( |Invalid boolean value in { header_name }| ) ).
        ENDIF.

        IF normalized_header <> body_value.
          RAISE EXCEPTION NEW zcx_mcp_server(
              textid = zcx_mcp_server=>invalid_arguments
              msgv1  = CONV #( |{ header_name } mismatch: { normalized_header } <> { body_value }| ) ).
        ENDIF.

      WHEN OTHERS.
        IF normalized_header <> body_value.
          RAISE EXCEPTION NEW zcx_mcp_server(
              textid = zcx_mcp_server=>invalid_arguments
              msgv1  = CONV #( |{ header_name } mismatch: { normalized_header } <> { body_value }| ) ).
        ENDIF.
    ENDCASE.
  ENDMETHOD.
ENDCLASS.
