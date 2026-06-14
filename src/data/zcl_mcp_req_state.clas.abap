"! <p class="shorttext synchronized">MCP protected requestState helper</p>
"! Creates and validates signed MRTR requestState tokens.
CLASS zcl_mcp_req_state DEFINITION
  PUBLIC FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    CONSTANTS version TYPE string VALUE `1`.

    TYPES: BEGIN OF state_data,
             area       TYPE string,
             server     TYPE string,
             method     TYPE string,
             uname      TYPE syuname,
             expires_at TYPE timestampl,
             nonce      TYPE string,
             data       TYPE string,
           END OF state_data.

    "! <p class="shorttext synchronized">Create protected requestState</p>
    "! Signs request state for one user, server, method, and expiry.
    "!
    "! @parameter area                | <p class="shorttext synchronized">MCP area</p>
    "! @parameter server              | <p class="shorttext synchronized">MCP server name</p>
    "! @parameter method              | <p class="shorttext synchronized">JSON-RPC method</p>
    "! @parameter data                | <p class="shorttext synchronized">Application state payload</p>
    "! @parameter ttl_seconds         | <p class="shorttext synchronized">Validity in seconds</p>
    "! @parameter uname               | <p class="shorttext synchronized">User name, defaults to current user</p>
    "! @parameter result              | <p class="shorttext synchronized">Signed requestState token</p>
    "! @raising   zcx_mcp_ajson_error | <p class="shorttext synchronized">JSON build error</p>
    "! @raising   zcx_mcp_server      | <p class="shorttext synchronized">Signing error</p>
    CLASS-METHODS create
      IMPORTING !area         TYPE string
                server        TYPE string
                !method       TYPE string
                !data         TYPE string
                ttl_seconds   TYPE i DEFAULT 300
                !uname        TYPE syuname DEFAULT sy-uname
      RETURNING VALUE(result) TYPE string
      RAISING   zcx_mcp_ajson_error
                zcx_mcp_server.

    "! <p class="shorttext synchronized">Validate protected requestState</p>
    "! Verifies signature, expiry, user, server, and method.
    "!
    "! @parameter request_state       | <p class="shorttext synchronized">Signed requestState token</p>
    "! @parameter area                | <p class="shorttext synchronized">Expected MCP area</p>
    "! @parameter server              | <p class="shorttext synchronized">Expected MCP server name</p>
    "! @parameter method              | <p class="shorttext synchronized">Expected JSON-RPC method</p>
    "! @parameter uname               | <p class="shorttext synchronized">Expected user name, defaults to current user</p>
    "! @parameter result              | <p class="shorttext synchronized">Validated state data</p>
    "! @raising   zcx_mcp_ajson_error | <p class="shorttext synchronized">JSON parse error</p>
    "! @raising   zcx_mcp_server      | <p class="shorttext synchronized">Invalid, expired, or mismatched state</p>
    CLASS-METHODS validate
      IMPORTING request_state TYPE string
                !area         TYPE string
                server        TYPE string
                !method       TYPE string
                !uname        TYPE syuname DEFAULT sy-uname
      RETURNING VALUE(result) TYPE state_data
      RAISING   zcx_mcp_ajson_error
                zcx_mcp_server.

    "! <p class="shorttext synchronized">Delete expired consumed requestState nonces</p>
    "!
    "! @parameter result | <p class="shorttext synchronized">Deleted row count</p>
    CLASS-METHODS delete_expired_nonces
      RETURNING VALUE(result) TYPE i.

  PRIVATE SECTION.
    CLASS-METHODS build_payload
      IMPORTING !state        TYPE state_data
      RETURNING VALUE(result) TYPE REF TO zif_mcp_ajson
      RAISING   zcx_mcp_ajson_error.

    CLASS-METHODS sign_payload
      IMPORTING payload       TYPE REF TO zif_mcp_ajson
      RETURNING VALUE(result) TYPE string
      RAISING   zcx_mcp_ajson_error
                zcx_mcp_server.

    CLASS-METHODS create_nonce
      RETURNING VALUE(result) TYPE string.

    CLASS-METHODS raise_invalid
      IMPORTING !message TYPE string
      RAISING   zcx_mcp_server.

    CLASS-METHODS validate_payload_shape
      IMPORTING payload TYPE REF TO zif_mcp_ajson
      RAISING   zcx_mcp_ajson_error
                zcx_mcp_server.

    CLASS-METHODS signatures_equal
      IMPORTING expected      TYPE string
                !actual       TYPE string
      RETURNING VALUE(result) TYPE abap_bool.

    CLASS-METHODS consume_nonce
      IMPORTING !state TYPE state_data
      RAISING   zcx_mcp_server.
ENDCLASS.


CLASS zcl_mcp_req_state IMPLEMENTATION.
  METHOD create.
    DATA state      TYPE state_data.
    DATA payload    TYPE REF TO zif_mcp_ajson.
    DATA token      TYPE REF TO zif_mcp_ajson.
    DATA now        TYPE timestampl.
    DATA expires_at TYPE timestampl.
    DATA signature  TYPE string.

    IF ttl_seconds <= 0.
      RAISE EXCEPTION NEW zcx_mcp_server( textid = zcx_mcp_server=>invalid_arguments
                                          msgv1  = `ttl_seconds must be positive` ).
    ENDIF.

    GET TIME STAMP FIELD now.
    expires_at = cl_abap_tstmp=>add( tstmp = now
                                     secs  = ttl_seconds ).

    state-area       = area.
    state-server     = server.
    state-method     = method.
    state-uname      = uname.
    state-expires_at = expires_at.
    state-nonce      = create_nonce( ).
    state-data       = data.

    payload = build_payload( state ).
    signature = sign_payload( payload ).

    token = zcl_mcp_ajson=>create_empty( ).
    token->set( iv_path = `/payload`
                iv_val  = payload ).
    token->set_string( iv_path = `/sig`
                       iv_val  = signature ).

    result = token->stringify( ).
  ENDMETHOD.

  METHOD validate.
    DATA token              TYPE REF TO zif_mcp_ajson.
    DATA payload            TYPE REF TO zif_mcp_ajson.
    DATA expected_signature TYPE string.
    DATA actual_signature   TYPE string.
    DATA now                TYPE timestampl.

    IF request_state IS INITIAL.
      raise_invalid( `Missing requestState` ).
    ENDIF.

    token = zcl_mcp_ajson=>parse( request_state ).

    IF token->exists( `/payload` ) = abap_false OR token->exists( `/sig` ) = abap_false.
      raise_invalid( `Invalid requestState format` ).
    ENDIF.

    payload = token->slice( `/payload` ).
    validate_payload_shape( payload ).

    actual_signature = token->get_string( `/sig` ).
    actual_signature = to_upper( actual_signature ).

    IF actual_signature IS INITIAL OR actual_signature CN `0123456789ABCDEF`.
      raise_invalid( `Invalid requestState signature format` ).
    ENDIF.

    expected_signature = sign_payload( payload ).

    IF signatures_equal( expected = expected_signature
                         actual   = actual_signature ) = abap_false.
      raise_invalid( `Invalid requestState signature` ).
    ENDIF.

    result-area       = payload->get_string( `/area` ).
    result-server     = payload->get_string( `/server` ).
    result-method     = payload->get_string( `/method` ).
    result-uname      = payload->get_string( `/uname` ).
    result-expires_at = payload->get_string( `/expiresAt` ).
    result-nonce      = payload->get_string( `/nonce` ).
    result-data       = payload->get_string( `/data` ).

    IF result-area <> area.
      raise_invalid( `requestState area mismatch` ).
    ENDIF.

    IF result-server <> server.
      raise_invalid( `requestState server mismatch` ).
    ENDIF.

    IF result-method <> method.
      raise_invalid( `requestState method mismatch` ).
    ENDIF.

    IF result-uname <> uname.
      raise_invalid( `requestState user mismatch` ).
    ENDIF.

    GET TIME STAMP FIELD now.
    IF result-expires_at < now.
      raise_invalid( `requestState expired` ).
    ENDIF.

    consume_nonce( result ).
  ENDMETHOD.

  METHOD build_payload.
    result = zcl_mcp_ajson=>create_empty( ).

    result->set_string( iv_path = `/v`
                        iv_val  = version ).
    result->set_string( iv_path = `/area`
                        iv_val  = state-area ).
    result->set_string( iv_path = `/server`
                        iv_val  = state-server ).
    result->set_string( iv_path = `/method`
                        iv_val  = state-method ).
    result->set_string( iv_path = `/uname`
                        iv_val  = state-uname ).
    result->set_string( iv_path = `/expiresAt`
                        iv_val  = CONV string( state-expires_at ) ).
    result->set_string( iv_path = `/nonce`
                        iv_val  = state-nonce ).
    result->set_string( iv_path = `/data`
                        iv_val  = state-data ).
  ENDMETHOD.

  METHOD sign_payload.
    DATA hmac TYPE hash512_hex.

    CALL FUNCTION 'CALCULATE_HMAC_FOR_CHAR'
      EXPORTING
        alg    = 'SHA2'
        data   = payload->stringify( )
      IMPORTING
        hmac   = hmac
      EXCEPTIONS
        OTHERS = 1.

    IF sy-subrc <> 0.
      RAISE EXCEPTION NEW zcx_mcp_server( textid = zcx_mcp_server=>internal_error
                                          msgv1  = `Could not sign requestState` ).
    ENDIF.

    result = hmac.
  ENDMETHOD.

  METHOD create_nonce.
    DATA uuid TYPE sysuuid_c32.

    TRY.
        uuid = cl_system_uuid=>create_uuid_c32_static( ).
        result = uuid.
      CATCH cx_uuid_error.
        GET TIME STAMP FIELD DATA(now).
        result = CONV string( now ).
    ENDTRY.
  ENDMETHOD.

  METHOD raise_invalid.
    RAISE EXCEPTION NEW zcx_mcp_server( textid = zcx_mcp_server=>invalid_arguments
                                        msgv1  = CONV #( message ) ).
  ENDMETHOD.

  METHOD validate_payload_shape.
    DATA expires_at TYPE timestampl.

    IF payload IS NOT BOUND.
      raise_invalid( `Invalid requestState payload` ).
    ENDIF.

    IF payload->get_string( `/v` ) <> version.
      raise_invalid( `Unsupported requestState version` ).
    ENDIF.

    IF payload->get_string( `/area` ) IS INITIAL.
      raise_invalid( `Missing requestState area` ).
    ENDIF.

    IF payload->get_string( `/server` ) IS INITIAL.
      raise_invalid( `Missing requestState server` ).
    ENDIF.

    IF payload->get_string( `/method` ) IS INITIAL.
      raise_invalid( `Missing requestState method` ).
    ENDIF.

    IF payload->get_string( `/uname` ) IS INITIAL.
      raise_invalid( `Missing requestState user` ).
    ENDIF.

    IF payload->get_string( `/expiresAt` ) IS INITIAL.
      raise_invalid( `Missing requestState expiry` ).
    ENDIF.

    IF payload->get_string( `/nonce` ) IS INITIAL.
      raise_invalid( `Missing requestState nonce` ).
    ENDIF.

    IF payload->exists( `/data` ) = abap_false.
      raise_invalid( `Missing requestState data` ).
    ENDIF.

    TRY.
        expires_at = payload->get_string( `/expiresAt` ).
      CATCH cx_sy_conversion_no_number cx_sy_conversion_overflow.
        raise_invalid( `Invalid requestState expiry` ).
    ENDTRY.
  ENDMETHOD.

  METHOD signatures_equal.
    DATA expected_len TYPE i.
    DATA actual_len   TYPE i.
    DATA offset       TYPE i.
    DATA mismatches   TYPE i.

    result = abap_false.

    expected_len = strlen( expected ).
    actual_len   = strlen( actual ).

    IF expected_len = 0 OR expected_len <> actual_len.
      RETURN.
    ENDIF.

    DO expected_len TIMES.
      offset = sy-index - 1.
      IF expected+offset(1) <> actual+offset(1).
        mismatches = mismatches + 1.
      ENDIF.
    ENDDO.

    result = xsdbool( mismatches = 0 ).
  ENDMETHOD.

  METHOD consume_nonce.
    DATA db_nonce    TYPE zmcp_req_nonces.
    DATA consumed_at TYPE timestamp.
    DATA expires_at  TYPE timestamp.
    DATA now         TYPE timestampl.

    IF state-nonce IS INITIAL OR strlen( state-nonce ) > 64.
      raise_invalid( `Invalid requestState nonce` ).
    ENDIF.

    GET TIME STAMP FIELD now.

    cl_abap_tstmp=>move( EXPORTING tstmp_src = now
                         IMPORTING tstmp_tgt = consumed_at ).

    cl_abap_tstmp=>move( EXPORTING tstmp_src = state-expires_at
                         IMPORTING tstmp_tgt = expires_at ).

    db_nonce-client      = sy-mandt.
    db_nonce-nonce       = state-nonce.
    db_nonce-area        = CONV #( state-area ).
    db_nonce-server      = CONV #( state-server ).
    db_nonce-rpc_method  = state-method.
    db_nonce-uname       = state-uname.
    db_nonce-expires_at  = expires_at.
    db_nonce-consumed_at = consumed_at.

    INSERT zmcp_req_nonces FROM @db_nonce.

    IF sy-subrc <> 0.
      raise_invalid( `requestState replayed` ).
    ENDIF.
  ENDMETHOD.

  METHOD delete_expired_nonces.
    DATA now_l TYPE timestampl.
    DATA now   TYPE timestamp.

    GET TIME STAMP FIELD now_l.

    cl_abap_tstmp=>move( EXPORTING tstmp_src = now_l
                         IMPORTING tstmp_tgt = now ).

    DELETE FROM zmcp_req_nonces
      WHERE expires_at < @now.

    result = sy-dbcnt.

    IF result > 0.
      COMMIT WORK AND WAIT.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
