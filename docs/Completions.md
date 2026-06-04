# MCP Server SDK: Completions

This documentation explains how to implement argument autocomplete in the Model
Context Protocol (MCP) Server SDK. Completions let MCP clients request suggestions
as a user fills in a **prompt argument** or a **resource-template variable** —
similar to IDE autocomplete.

The feature implements the `completion/complete` endpoint and is part of the
protocol from 2025-06-18 onward.

## Table of Contents

- [Overview](#overview)
- [Declaring the Capability](#declaring-the-capability)
- [Completion Request Class](#completion-request-class)
- [Completion Response Class](#completion-response-class)
- [Implementing the Handler](#implementing-the-handler)
- [Examples](#examples)

## Overview

When a client offers autocomplete for a prompt or resource-template argument, it
sends a `completion/complete` request containing:

- a **reference** to a prompt (by name) or a resource template (by URI template),
- the **argument** being completed and the partial value typed so far,
- optionally a **context** object holding the values of arguments already filled in.

Your server returns a list of candidate strings. The framework routes the request
to the `handle_completions_complete` hook of `ZCL_MCP_SERVER_BASE`. The default
implementation returns an error (`This server does not implement completions`), so
you only get completions once you override the hook.

## Declaring the Capability

Advertise the capability in `handle_initialize` so clients know to call the
endpoint:

```abap
METHOD handle_initialize.
  response-result->set_capabilities( VALUE #(
    prompts     = VALUE #( enabled = abap_true )
    resources   = VALUE #( enabled = abap_true )
    completions = abap_true ) ).
  " ... set_implementation / set_instructions ...
ENDMETHOD.
```

## Completion Request Class

The `ZCL_MCP_REQ_COMPLETE` class parses the request.

Reference type constants:

- `zcl_mcp_req_complete=>ref_type-prompt` (`ref/prompt`)
- `zcl_mcp_req_complete=>ref_type-resource` (`ref/resource`)

Key methods:

- `get_ref_type()`: Returns `ref/prompt` or `ref/resource`
- `get_ref_name()`: Prompt name (only when the ref type is `ref/prompt`)
- `get_ref_uri()`: Resource URI template (only when the ref type is `ref/resource`)
- `get_argument_name()`: Name of the argument being completed
- `get_argument_value()`: Partial value typed by the client so far
- `has_context()`: Whether a `context` object was supplied
- `get_context_json()`: Raw context slice — read `context/arguments` for the values
  of already-resolved arguments
- `get_meta()`: Optional `_meta` object from the request

## Completion Response Class

The `ZCL_MCP_RESP_COMPLETE` class builds the response.

Key methods:

- `add_value()`: Adds a single completion candidate
- `set_values()`: Replaces the full candidate list (type
  `zcl_mcp_resp_complete=>completion_values`, a string table)
- `set_total()`: Optional total number of matches on the server (may exceed the
  values returned in this response)
- `set_has_more()`: Optional flag signalling that more candidates exist
- `set_meta()`: Optional metadata for the response

> The MCP spec limits a single completion response to **100 values**. Use
> `set_total`/`set_has_more` to indicate that more matches exist.

## Implementing the Handler

Override the hook in your server. Note that `handle_completions_complete` uses a
`CHANGING response` parameter (the underlying `ZIF_MCP_SERVER~completions_complete`
interface method is `RETURNING`, but you redefine the `CHANGING` base hook):

```abap
METHODS handle_completions_complete REDEFINITION.
```

## Examples

```abap
METHOD handle_completions_complete.
  CASE request->get_ref_type( ).

    WHEN zcl_mcp_req_complete=>ref_type-prompt.
      CASE request->get_ref_name( ).
        WHEN 'greeting'.
          IF request->get_argument_name( ) = 'name'.
            DATA(names) = VALUE zcl_mcp_resp_complete=>completion_values(
              ( `Alice` ) ( `Bob` ) ( `Charlie` ) ).
            response-result->set_values( names ).
          ENDIF.
      ENDCASE.

    WHEN zcl_mcp_req_complete=>ref_type-resource.
      IF request->get_ref_uri( ) = 'orders/{order_id}'.
        " Suggest order IDs, optionally filtered by the partial value
        DATA(prefix) = request->get_argument_value( ).
        DATA(orders) = VALUE zcl_mcp_resp_complete=>completion_values(
          ( `0000000010` ) ( `0000000011` ) ).
        response-result->set_values( orders ).
        response-result->set_has_more( abap_false ).
      ENDIF.

  ENDCASE.
ENDMETHOD.
```

See the `ZCL_MCP_DEMO_SERVER_STATELESS` demo for a working implementation covering
both a prompt argument and a resource-template variable.
