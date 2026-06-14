# V2 MRTR and Elicitation

Multi Round-Trip Requests let a v2 server ask the client for additional input without opening a server-to-client JSON-RPC request stream.

The server returns `resultType = "input_required"`. The client gathers input and retries the original method with `inputResponses` and the returned `requestState`.

## Supported Methods

The ABAP SDK supports MRTR on:

- `tools/call`
- `prompts/get`
- `resources/read`

Do not return `input_required` from other methods.

## Input Required Result

Use `ZCL_MCP_RESP_V2_INPUT_REQ`.

`ZCL_MCP_DEMO_SERVER_V2_WF` builds the direct MRTR and task-input request with one helper:

```abap
METHOD build_input_required.
  DATA input_required TYPE REF TO zcl_mcp_resp_v2_input_req.
  DATA elicitation    TYPE REF TO zcl_mcp_input_elicitation.
  DATA builder        TYPE REF TO zcl_mcp_schema_builder.

  builder = NEW zcl_mcp_schema_builder( ).
  builder->add_boolean(
    name        = `approved`
    description = `Whether the demo workflow may continue.`
    required    = abap_true ).
  builder->add_string(
    name        = `comment`
    description = `Optional user comment.`
    required    = abap_false ).

  elicitation = NEW zcl_mcp_input_elicitation( ).
  elicitation->set_form(
    message          = message
    requested_schema = builder->to_json( ) ).

  input_required = NEW zcl_mcp_resp_v2_input_req( ).
  input_required->set_request_state(
    create_request_state( data        = state_data
                          ttl_seconds = 300 ) ).
  input_required->add_input_request(
    request_key = c_input_confirm
    method      = elicitation->get_method( )
    params      = elicitation->get_params( ) ).

  result = input_required->zif_mcp_modern_result~generate_json( ).
ENDMETHOD.
```

`request_key` values become object keys below `/inputRequests`. Keep them simple, stable, and unique within the result.

## Tool Retry Handling

Request classes expose retry helpers. For tools, use `ZCL_MCP_REQ_CALL_TOOL`.

```abap
METHOD handle_tools_call.
  CASE request->get_name( ).
    WHEN c_tool_approval.
      IF request->is_retry( ) = abap_true.
        DATA state           TYPE zcl_mcp_req_state=>state_data.
        DATA input_responses TYPE REF TO zif_mcp_ajson.
        DATA elicitation     TYPE REF TO zcl_mcp_elicit_result.

        TRY.
            state = validate_request_state( request->get_request_state( ) ).

            IF state-data <> c_state_approval.
              response-error-code    = zcl_mcp_jsonrpc=>error_codes-invalid_params.
              response-error-message = `Unexpected requestState payload.`.
              RETURN.
            ENDIF.

            input_responses = request->get_input_responses( ).
            elicitation = NEW zcl_mcp_elicit_result(
              input_responses->slice( |/{ c_input_confirm }| ) ).

            IF elicitation->is_accept( ) = abap_true.
              DATA(approved_text) = COND string(
                WHEN elicitation->get_boolean( `approved` ) = abap_true
                THEN `approved`
                ELSE `not approved` ).

              response-result = build_text_result(
                |Approval workflow completed: { approved_text }. Comment: { elicitation->get_string( `comment` ) }| ).
            ELSE.
              response-result = build_text_result(
                |Approval workflow ended with action { elicitation->get_action( ) }.| ).
            ENDIF.

          CATCH zcx_mcp_server INTO DATA(mcp_error).
            response-error-code    = zcl_mcp_jsonrpc=>error_codes-invalid_params.
            response-error-message = mcp_error->get_text( ).
          CATCH zcx_mcp_ajson_error INTO DATA(json_error).
            response-error-code    = zcl_mcp_jsonrpc=>error_codes-invalid_params.
            response-error-message = json_error->get_text( ).
        ENDTRY.

        RETURN.
      ENDIF.

      TRY.
          response-result = build_input_required(
            state_data = c_state_approval
            message    = `Approve the direct MRTR demo action.` ).
        CATCH zcx_mcp_server INTO DATA(mcp_error).
          response-error-code    = zcl_mcp_jsonrpc=>error_codes-internal_error.
          response-error-message = mcp_error->get_text( ).
        CATCH zcx_mcp_ajson_error INTO DATA(json_error).
          response-error-code    = zcl_mcp_jsonrpc=>error_codes-internal_error.
          response-error-message = json_error->get_text( ).
      ENDTRY.
  ENDCASE.
ENDMETHOD.
```

The same pattern applies to `prompts/get` and `resources/read`.

## Elicitation Modes

`ZCL_MCP_INPUT_ELICITATION` supports form and URL mode.

Form mode:

```abap
elicitation->set_form( message          = `Provide the missing filter.`
                       requested_schema = builder->to_json( ) ).
```

URL mode:

```abap
elicitation->set_url( message        = `Complete authorization in the browser.`
                      url            = `https://example.invalid/authorize`
                      elicitation_id = `auth-123` ).
```

Use form mode for non-sensitive structured data. Use URL mode for sensitive interactions such as external authorization flows. Do not request passwords, API keys, access tokens, or payment credentials through form mode.

## RequestState Rules

Use `create_request_state` and `validate_request_state`. Do not invent custom signing or parsing in application code.

Good request-state payloads:

- small identifiers
- action names
- non-secret continuation keys
- short-lived workflow hints

Bad request-state payloads:

- passwords
- access tokens
- API keys
- full business documents
- personal data that does not need to be exposed to the client

The token is integrity-protected and replay-protected, but it is not encrypted.

## Legacy Compatibility

Old MCP clients cannot complete MRTR retries through the v2 continuation model. The compatibility adapter therefore fails these cases explicitly instead of pretending success.

Compatibility behavior:

| V2 behavior | Legacy adapter behavior |
| ----------- | ----------------------- |
| normal complete result | translated where possible |
| `input_required` tool/prompt/resource result | JSON-RPC error |
| elicitation input request | JSON-RPC error through the `input_required` result |
| task state `input_required` | JSON-RPC error |
| `tasks/update` | method not found |

Design modern interactive flows for v2 clients. Provide a separate non-interactive fallback tool if old clients must use the same business capability.
