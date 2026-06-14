"! <p class="shorttext synchronized">JSON-RPC 2.0 protocol implementation</p>
"! Class implementing the JSON-RPC 2.0 specification for remote procedure calls using JSON.
"! Supports request/response handling, error management.
"! Batch processing not supported due to removal in current spec and non-suppoort in clients.
"! see https://www.jsonrpc.org/specification
CLASS zcl_mcp_jsonrpc DEFINITION
  PUBLIC FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    " Constants for JSON-RPC protocol
    CONSTANTS jsonrpc_version TYPE string VALUE '2.0'.
    CONSTANTS: BEGIN OF error_codes,
                 parse_error                  TYPE i VALUE -32700,
                 invalid_request              TYPE i VALUE -32600,
                 method_not_found             TYPE i VALUE -32601,
                 invalid_params               TYPE i VALUE -32602,
                 internal_error               TYPE i VALUE -32603,
                 header_mismatch              TYPE i VALUE -32001,
                 resource_not_found           TYPE i VALUE -32002,
                 missing_client_capability    TYPE i VALUE -32003,
                 unsupported_protocol_version TYPE i VALUE -32004,
               END OF error_codes.

    " Core data types
    TYPES: BEGIN OF request,
             jsonrpc    TYPE string,
             method     TYPE string,
             params     TYPE REF TO zif_mcp_ajson,
             id         TYPE string,
             id_present TYPE abap_bool,
           END OF request.

    TYPES: BEGIN OF error,
             code    TYPE i,
             message TYPE string,
             data    TYPE REF TO zif_mcp_ajson,
           END OF error.

    TYPES: BEGIN OF response,
             jsonrpc    TYPE string,
             result     TYPE REF TO zif_mcp_ajson,
             error      TYPE error,
             id         TYPE string,
             id_present TYPE abap_bool,
             id_is_null TYPE abap_bool,
           END OF response.

    " Core JSON-RPC functionality
    METHODS create_request
      IMPORTING !method       TYPE string
                !id           TYPE string OPTIONAL
      RETURNING VALUE(result) TYPE request.

    METHODS create_success_response
      IMPORTING !id             TYPE string
                !result         TYPE REF TO zif_mcp_ajson OPTIONAL
      RETURNING VALUE(response) TYPE response.

    METHODS create_error_response
      IMPORTING !id             TYPE string
                !code           TYPE i
                !message        TYPE string
                !data           TYPE REF TO zif_mcp_ajson OPTIONAL
      RETURNING VALUE(response) TYPE response.

    " JSON parsing and serialization
    METHODS parse_request
      IMPORTING !json         TYPE string
      RETURNING VALUE(result) TYPE request
      RAISING   zcx_mcp_ajson_error.

    METHODS serialize_request
      IMPORTING !request      TYPE request
      RETURNING VALUE(result) TYPE string
      RAISING   zcx_mcp_ajson_error.

    METHODS serialize_response
      IMPORTING !response     TYPE response
      RETURNING VALUE(result) TYPE string
      RAISING   zcx_mcp_ajson_error.

    METHODS parse_response
      IMPORTING !json         TYPE string
      RETURNING VALUE(result) TYPE response
      RAISING   zcx_mcp_ajson_error.

  PRIVATE SECTION.

    METHODS extract_id
      IMPORTING json_obj      TYPE REF TO zif_mcp_ajson
      RETURNING VALUE(result) TYPE string.
ENDCLASS.



CLASS zcl_mcp_jsonrpc IMPLEMENTATION.
  METHOD create_error_response.
    response-jsonrpc    = jsonrpc_version.
    response-id         = id.
    response-id_present = abap_true.
    response-error-code    = code.
    response-error-message = message.

    " Set error data if provided
    IF data IS SUPPLIED AND data IS BOUND.
      response-error-data = data.
    ENDIF.
  ENDMETHOD.


  METHOD create_request.
    result-jsonrpc = jsonrpc_version.
    result-method  = method.

    IF id IS SUPPLIED.
      result-id_present = abap_true.
      result-id = id.
    ENDIF.
  ENDMETHOD.

  METHOD create_success_response.
    response-jsonrpc    = jsonrpc_version.
    response-id         = id.
    response-id_present = abap_true.

    " Set result if provided
    IF result IS SUPPLIED AND result IS BOUND.
      response-result = result.
    ENDIF.
  ENDMETHOD.


  METHOD extract_id.
    " Return empty if ID doesn't exist
    IF NOT json_obj->exists( '/id' ).
      RETURN.
    ENDIF.

    " Handle based on node type
    CASE json_obj->get_node_type( '/id' ).
      WHEN 'str' OR 'num'.
        " Either string or number ID - return as string
        result = json_obj->get_string( '/id' ).
      WHEN 'null'.
        " Null ID - leave empty
        CLEAR result.
      WHEN OTHERS.
        " Unsupported type - use string representation
        result = json_obj->get_string( '/id' ).
    ENDCASE.
  ENDMETHOD.

  METHOD parse_request.
    DATA json_obj TYPE REF TO zif_mcp_ajson.

    json_obj = zcl_mcp_ajson=>parse( json ).

    result-jsonrpc = json_obj->get_string( '/jsonrpc' ).
    IF result-jsonrpc <> jsonrpc_version.
      zcx_mcp_ajson_error=>raise( |Invalid JSON-RPC version| ) ##NO_TEXT.
    ENDIF.

    IF json_obj->exists( '/method' ) = abap_false.
      zcx_mcp_ajson_error=>raise( |Missing JSON-RPC method| ) ##NO_TEXT.
    ENDIF.

    result-method = json_obj->get_string( '/method' ).
    IF result-method IS INITIAL.
      zcx_mcp_ajson_error=>raise( |Empty JSON-RPC method| ) ##NO_TEXT.
    ENDIF.

    IF json_obj->exists( '/params' ).
      result-params = json_obj->slice( '/params' ).
    ELSE.
      result-params = zcl_mcp_ajson=>create_empty( ).
    ENDIF.

    result-id_present = xsdbool( json_obj->exists( '/id' ) ).
    result-id         = extract_id( json_obj ).
  ENDMETHOD.

  METHOD parse_response.
    DATA json_obj TYPE REF TO zif_mcp_ajson.

    " Parse JSON string to object
    json_obj = zcl_mcp_ajson=>parse( json ).

    " Extract standard fields
    result-jsonrpc = json_obj->get_string( '/jsonrpc' ).

    " Extract result if it exists
    IF json_obj->exists( '/result' ).
      result-result = json_obj->slice( '/result' ).
    ENDIF.

    " Extract error if it exists
    IF json_obj->exists( '/error' ).
      result-error-code    = json_obj->get_integer( '/error/code' ).
      result-error-message = json_obj->get_string( '/error/message' ).

      " Extract error data if it exists
      IF json_obj->exists( '/error/data' ).
        result-error-data = json_obj->slice( '/error/data' ).
      ENDIF.
    ENDIF.

    " Extract ID with special handling
    result-id_present = xsdbool( json_obj->exists( '/id' ) ).
    result-id_is_null = xsdbool(     json_obj->exists( '/id' )
                                 AND json_obj->get_node_type( '/id' ) = 'null' ).

    result-id         = extract_id( json_obj ).
  ENDMETHOD.

  METHOD serialize_request.
    DATA json_obj TYPE REF TO zif_mcp_ajson.

    " Create new JSON object
    json_obj = zcl_mcp_ajson=>create_empty( ).

    " Add standard fields
    json_obj->set_string( iv_path = '/jsonrpc'
                          iv_val  = request-jsonrpc ).
    json_obj->set_string( iv_path = '/method'
                          iv_val  = request-method ).

    " Add params if present
    IF request-params IS BOUND.
      " Use slice content at params path
      DATA(params_node_type) = request-params->get_node_type( '/' ).

      IF params_node_type = 'array'.
        json_obj->touch_array( '/params' ).
      ENDIF.

      " Copy params content to the request JSON
      DATA(params_json) = request-params->stringify( ).
      DATA(params_obj) = zcl_mcp_ajson=>parse( params_json ).
      json_obj->set( iv_path = '/params'
                     iv_val  = params_obj ).
    ENDIF.

    " Add ID with correct type if present
    IF request-id_present = abap_true.
      " Try to determine if it's numeric
      TRY.
          DATA(number) = CONV i( request-id ).
          " Only treat as number if exact string representation matches
          IF request-id = |{ number }|.
            json_obj->set_integer( iv_path = '/id'
                                   iv_val  = number ).
          ELSE.
            " Not an exact number, treat as string
            json_obj->set_string( iv_path = '/id'
                                  iv_val  = request-id ).
          ENDIF.
        CATCH cx_sy_conversion_no_number cx_sy_conversion_overflow.
          " Not a number, use as string
          json_obj->set_string( iv_path = '/id'
                                iv_val  = request-id ).
      ENDTRY.
    ENDIF.

    " Convert to string
    result = json_obj->stringify( ).
  ENDMETHOD.

  METHOD serialize_response.
    DATA json_obj TYPE REF TO zif_mcp_ajson.

    " Create new JSON object
    json_obj = zcl_mcp_ajson=>create_empty( ).

    " Add standard fields
    json_obj->set_string( iv_path = '/jsonrpc'
                          iv_val  = response-jsonrpc ).

    " Add result if success response
    IF     response-result        IS BOUND
       AND response-error-code     = 0
       AND response-error-message IS INITIAL.
      DATA(result_json) = response-result->stringify( ).
      DATA(result_obj)  = zcl_mcp_ajson=>parse( result_json ).
      json_obj->set( iv_path = '/result'
                     iv_val  = result_obj ).
    ENDIF.

    " Add error if error response
    IF response-error-code <> 0 OR response-error-message IS NOT INITIAL.
      json_obj->set_integer( iv_path = '/error/code'
                             iv_val  = response-error-code ).
      json_obj->set_string( iv_path = '/error/message'
                            iv_val  = response-error-message ).

      IF response-error-data IS BOUND.
        DATA(error_data_json) = response-error-data->stringify( ).
        DATA(error_data_obj)  = zcl_mcp_ajson=>parse( error_data_json ).
        json_obj->set( iv_path = '/error/data'
                       iv_val  = error_data_obj ).
      ENDIF.
    ENDIF.

    " MCP TS SDK JSONRPCErrorResponseSchema accepts absent id, string id, or number id,
    " but not null. Therefore unknown/null/unusable ids are omitted.
    IF response-id_present = abap_true AND response-id_is_null = abap_false.
      TRY.
          DATA(number) = CONV i( response-id ).
          IF response-id = |{ number }|.
            json_obj->set_integer( iv_path = '/id'
                                   iv_val  = number ).
          ELSE.
            json_obj->set_string( iv_path = '/id'
                                  iv_val  = response-id ).
          ENDIF.
        CATCH cx_sy_conversion_no_number
              cx_sy_conversion_overflow.
          json_obj->set_string( iv_path = '/id'
                                iv_val  = response-id ).
      ENDTRY.
    ENDIF.

    result = json_obj->stringify( ).
  ENDMETHOD.
ENDCLASS.
