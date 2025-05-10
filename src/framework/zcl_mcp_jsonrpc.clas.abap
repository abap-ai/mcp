"! <p class="shorttext synchronized">JSON-RPC 2.0 protocol implementation</p>
"! Class implementing the JSON-RPC 2.0 specification for remote procedure calls using JSON.
"! Supports request/response handling, error management, and batch processing according to the specification.
"! see https://www.jsonrpc.org/specification
CLASS zcl_mcp_jsonrpc DEFINITION
  PUBLIC FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    " Constants for JSON-RPC protocol
    CONSTANTS jsonrpc_version TYPE string VALUE '2.0'.
    CONSTANTS: BEGIN OF error_codes,
                 parse_error      TYPE i VALUE -32700,
                 invalid_request  TYPE i VALUE -32600,
                 method_not_found TYPE i VALUE -32601,
                 invalid_params   TYPE i VALUE -32602,
                 internal_error   TYPE i VALUE -32603,
               END OF error_codes.

    " Core data types
    TYPES: BEGIN OF request,
             jsonrpc TYPE string,
             method  TYPE string,
             params  TYPE REF TO zif_mcp_ajson,
             id      TYPE string,
           END OF request.

    TYPES: BEGIN OF error,
             code    TYPE i,
             message TYPE string,
             data    TYPE REF TO zif_mcp_ajson,
           END OF error.

    TYPES: BEGIN OF response,
             jsonrpc TYPE string,
             result  TYPE REF TO zif_mcp_ajson,
             error   TYPE error,
             id      TYPE string,
           END OF response.

    " Table types for batch operations
    TYPES requests  TYPE TABLE OF request WITH DEFAULT KEY.
    TYPES responses TYPE TABLE OF response WITH DEFAULT KEY.

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
      IMPORTING json          TYPE string
      RETURNING VALUE(result) TYPE request
      RAISING   zcx_mcp_ajson_error.

    METHODS parse_batch_request
      IMPORTING json          TYPE string
      RETURNING VALUE(result) TYPE requests
      RAISING   zcx_mcp_ajson_error.

    METHODS serialize_request
      IMPORTING !request      TYPE request
      RETURNING VALUE(result) TYPE string
      RAISING   zcx_mcp_ajson_error.

    METHODS serialize_response
      IMPORTING !response     TYPE response
      RETURNING VALUE(result) TYPE string
      RAISING   zcx_mcp_ajson_error.

    METHODS serialize_batch_response
      IMPORTING !responses    TYPE responses
      RETURNING VALUE(result) TYPE string
      RAISING   zcx_mcp_ajson_error.

    METHODS parse_response
      IMPORTING json          TYPE string
      RETURNING VALUE(result) TYPE response
      RAISING   zcx_mcp_ajson_error.

  PRIVATE SECTION.
    METHODS is_batch
      IMPORTING json          TYPE string
      RETURNING VALUE(result) TYPE abap_bool.

    METHODS extract_id
      IMPORTING json_obj      TYPE REF TO zif_mcp_ajson
      RETURNING VALUE(result) TYPE string.
ENDCLASS.



CLASS zcl_mcp_jsonrpc IMPLEMENTATION.


  METHOD create_error_response.
    response-jsonrpc = jsonrpc_version.
    response-id      = id.
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

    " Only set ID if provided (for notifications, it should be omitted)
    IF id IS SUPPLIED AND id IS NOT INITIAL.
      result-id = id.
    ENDIF.
  ENDMETHOD.


  METHOD create_success_response.
    response-jsonrpc = jsonrpc_version.
    response-id      = id.

    " Set result if provided
    IF result IS SUPPLIED AND result IS BOUND.
      response-result = result.
    ENDIF.
  ENDMETHOD.


  METHOD extract_id.
    " Return empty if ID doesn't exist
    IF NOT json_obj->exists( '/id' ) IS NOT INITIAL.
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


  METHOD is_batch.
    DATA length TYPE i.
    DATA temp1 TYPE xsdboolean.
    length = strlen( json ).
    
    temp1 = boolc( length >= 2 AND substring( val = json off = 0 len = 1 ) = '[' AND substring( val = json off = length - 1 len = 1 ) = ']' ).
    result = temp1.
  ENDMETHOD.


  METHOD parse_batch_request.
      DATA json_array TYPE REF TO zcl_mcp_ajson.
      DATA members TYPE string_table.
      DATA member LIKE LINE OF members.
        DATA single_json TYPE REF TO zif_mcp_ajson.
        DATA single_request TYPE zcl_mcp_jsonrpc=>request.
    " Check if JSON is batch (array) or single request
    IF is_batch( json ) IS NOT INITIAL.
      " Parse as array
      
      json_array = zcl_mcp_ajson=>parse( json ).

      " Process each array item
      
      members = json_array->members( '/' ).
      
      LOOP AT members INTO member.
        
        single_json = json_array->slice( |/{ member }| ).

        " Parse as request
        
        single_request = parse_request( single_json->stringify( ) ).
        APPEND single_request TO result.
      ENDLOOP.
    ELSE.
      " Single request
      APPEND parse_request( json ) TO result.
    ENDIF.
  ENDMETHOD.


  METHOD parse_request.
    DATA json_obj TYPE REF TO zif_mcp_ajson.

    " Parse JSON string to object
    json_obj = zcl_mcp_ajson=>parse( json ).

    " Extract standard fields
    result-jsonrpc = json_obj->get_string( '/jsonrpc' ).
    result-method  = json_obj->get_string( '/method' ).

    " Extract parameters if they exist
    IF json_obj->exists( '/params' ) IS NOT INITIAL.
      result-params = json_obj->slice( '/params' ).
    ELSE.
      " Avoid null object references
      result-params = zcl_mcp_ajson=>create_empty( ).
    ENDIF.

    " Extract ID with special handling
    result-id = extract_id( json_obj ).
  ENDMETHOD.


  METHOD parse_response.
    DATA json_obj TYPE REF TO zif_mcp_ajson.

    " Parse JSON string to object
    json_obj = zcl_mcp_ajson=>parse( json ).

    " Extract standard fields
    result-jsonrpc = json_obj->get_string( '/jsonrpc' ).

    " Extract result if it exists
    IF json_obj->exists( '/result' ) IS NOT INITIAL.
      result-result = json_obj->slice( '/result' ).
    ENDIF.

    " Extract error if it exists
    IF json_obj->exists( '/error' ) IS NOT INITIAL.
      result-error-code    = json_obj->get_integer( '/error/code' ).
      result-error-message = json_obj->get_string( '/error/message' ).

      " Extract error data if it exists
      IF json_obj->exists( '/error/data' ) IS NOT INITIAL.
        result-error-data = json_obj->slice( '/error/data' ).
      ENDIF.
    ENDIF.

    " Extract ID with special handling
    result-id = extract_id( json_obj ).
  ENDMETHOD.


  METHOD serialize_batch_response.
    DATA array_json TYPE REF TO zif_mcp_ajson.
    DATA response LIKE LINE OF responses.
      DATA response_json TYPE string.
      DATA response_obj TYPE REF TO zcl_mcp_ajson.

    " Create empty array
    array_json = zcl_mcp_ajson=>create_empty( ).
    array_json->touch_array( '/' ).

    " Add each response to the array
    
    LOOP AT responses INTO response.
      
      response_json = serialize_response( response ).
      
      response_obj = zcl_mcp_ajson=>parse( response_json ).
      array_json->push( iv_path = '/'
                        iv_val  = response_obj ).
    ENDLOOP.

    " Convert to string
    result = array_json->stringify( ).
  ENDMETHOD.

  METHOD serialize_request.
    DATA json_obj TYPE REF TO zif_mcp_ajson.
      DATA params_node_type TYPE zif_mcp_ajson_types=>ty_node_type.
      DATA params_json TYPE string.
      DATA params_obj TYPE REF TO zcl_mcp_ajson.
          DATA temp1 TYPE i.
          DATA number LIKE temp1.

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
      
      params_node_type = request-params->get_node_type( '/' ).

      IF params_node_type = 'array'.
        json_obj->touch_array( '/params' ).
      ENDIF.

      " Copy params content to the request JSON
      
      params_json = request-params->stringify( ).
      
      params_obj = zcl_mcp_ajson=>parse( params_json ).
      json_obj->set( iv_path = '/params'
                     iv_val  = params_obj ).
    ENDIF.

    " Add ID with correct type if present
    IF request-id IS NOT INITIAL.
      " Try to determine if it's numeric
      TRY.
          
          temp1 = request-id.
          
          number = temp1.
          " Only treat as number if exact string representation matches
          IF request-id = |{ number }|.
            json_obj->set_integer( iv_path = '/id'
                                   iv_val  = number ).
          ELSE.
            " Not an exact number, treat as string
            json_obj->set_string( iv_path = '/id'
                                  iv_val  = request-id ).
          ENDIF.
        CATCH cx_sy_conversion_no_number.
          " Not a number, use as string
          json_obj->set_string( iv_path = '/id'
                                iv_val  = request-id ).
        CATCH zcx_mcp_ajson_error.
          " Handle JSON operation error
      ENDTRY.
    ENDIF.

    " Convert to string
    result = json_obj->stringify( ).
  ENDMETHOD.

  METHOD serialize_response.
    DATA json_obj TYPE REF TO zif_mcp_ajson.
      DATA result_json TYPE string.
      DATA result_obj TYPE REF TO zcl_mcp_ajson.
        DATA error_data_json TYPE string.
        DATA error_data_obj TYPE REF TO zcl_mcp_ajson.
          DATA temp2 TYPE i.
          DATA number LIKE temp2.

    " Create new JSON object
    json_obj = zcl_mcp_ajson=>create_empty( ).

    " Add standard fields
    json_obj->set_string( iv_path = '/jsonrpc'
                          iv_val  = response-jsonrpc ).

    " Add result if success response
    IF response-result IS BOUND AND response-error-message IS INITIAL.
      " Copy result content to response JSON
      
      result_json = response-result->stringify( ).
      
      result_obj = zcl_mcp_ajson=>parse( result_json ).
      json_obj->set( iv_path = '/result'
                     iv_val  = result_obj ).
    ENDIF.

    " Add error if error response
    IF response-error-message IS NOT INITIAL.
      " Error code and message
      json_obj->set_integer( iv_path = '/error/code'
                             iv_val  = response-error-code ).
      json_obj->set_string( iv_path = '/error/message'
                            iv_val  = response-error-message ).

      " Error data if present
      IF response-error-data IS BOUND.
        
        error_data_json = response-error-data->stringify( ).
        
        error_data_obj = zcl_mcp_ajson=>parse( error_data_json ).
        json_obj->set( iv_path = '/error/data'
                       iv_val  = error_data_obj ).
      ENDIF.
    ENDIF.

    " Add ID with correct type
    IF response-id IS NOT INITIAL.
      " Add ID with correct type if present
      TRY.
          
          temp2 = response-id.
          
          number = temp2.
          " Only treat as number if exact string representation matches
          IF response-id = |{ number }|.
            json_obj->set_integer( iv_path = '/id'
                                   iv_val  = number ).
          ELSE.
            " Not an exact number, treat as string
            json_obj->set_string( iv_path = '/id'
                                  iv_val  = response-id ).
          ENDIF.
        CATCH cx_sy_conversion_no_number.
          " Not a number, use as string
          json_obj->set_string( iv_path = '/id'
                                iv_val  = response-id ).
        CATCH zcx_mcp_ajson_error.
          " Handle JSON operation error
      ENDTRY.
    ELSE.
      " Null ID
      json_obj->set_null( '/id' ).
    ENDIF.

    " Convert to string
    result = json_obj->stringify( ).
  ENDMETHOD.
ENDCLASS.
