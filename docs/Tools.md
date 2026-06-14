# MCP Server SDK: Tools

This documentation explains how to implement and use tools in the Model Context Protocol (MCP) Server SDK. Tools are a powerful feature that allow AI assistants to execute functions in your ABAP systems, enabling dynamic interaction with your business data and processes.

## Table of Contents

- [Overview](#overview)
- [Tool Request Classes](#tool-request-classes)
- [Tool Response Classes](#tool-response-classes)
- [Implementing Tool Handlers](#implementing-tool-handlers)
- [Tool Input/Output Validation](#tool-inputoutput-validation)
- [Tool Response Types](#tool-response-types)
- [Tool Annotations](#tool-annotations)
- [Output Schemas](#output-schemas)
- [Background Task Support](#background-task-support)
- [V2 Tool Notes](#v2-tool-notes)
- [Examples](#examples)

## Overview

Tools in the MCP protocol are functions that AI assistants can call to:

- Retrieve information from your SAP system
- Perform calculations or data processing
- Execute business operations
- Access system features

Each tool is identified by a unique name and can accept structured input parameters defined by a JSON schema.

For draft `2026-07-28` v2 servers, implement tools through `ZCL_MCP_SERVER_BASE_V2` and return modern result envelopes. See [V2 Server Implementation](V2ServerImplementation.md), [V2 HTTP, Headers, and Security](V2HTTPAndSecurity.md), and [V2 MRTR and Elicitation](V2MRTR.md).

## V2 Tool Notes

V2 tools use the same core tool model but add modern result and transport behavior:

- successful modern results should include `resultType`
- `tools/call` may return `complete`, `input_required`, or a Tasks extension `task` result
- top-level string, integer, and boolean arguments can use `x_mcp_header` to require matching `Mcp-Param-*` headers
- `handle_tool_input_schema` should be redefined for efficient header validation
- old clients can call many v2 tools through the stateless legacy adapter, but MRTR/elicitation results cannot be down-converted

Detailed v2 tool examples are in [V2 Server Implementation](V2ServerImplementation.md).

## Tool Request Classes

### List Tools Request

The `ZCL_MCP_REQ_LIST_TOOLS` class handles requests to list available tools.

Key methods:
- `get_cursor()`: Retrieves the pagination cursor if provided
- `has_cursor()`: Checks if a pagination cursor was provided

### Call Tool Request

The `ZCL_MCP_REQ_CALL_TOOL` class handles requests to execute a specific tool.

Key methods:
- `get_name()`: Retrieves the requested tool name
- `has_arguments()`: Checks if arguments were provided
- `get_arguments()`: Retrieves the arguments as a JSON object
- `get_meta()`: Retrieves optional `_meta` data
- `has_task()`: Checks whether the client requested task-augmented execution
- `get_task_ttl()`: Retrieves the requested task TTL in milliseconds, or `0` if absent

Example:

```abap
METHOD handle_call_tool.
  CASE request->get_name( ).
    WHEN 'get_customer_info'.
      DATA(args) = request->get_arguments( ).
      DATA(customer_id) = args->get_string( 'customer_id' ).
      
      get_customer_info(
        EXPORTING customer_id = customer_id
        CHANGING response = response
      ).
      
    WHEN OTHERS.
      response-error-code = zcl_mcp_jsonrpc=>error_codes-invalid_params.
      response-error-message = |Tool { request->get_name( ) } not found|.
  ENDCASE.
ENDMETHOD.
```

## Tool Response Classes

### List Tools Response

The `ZCL_MCP_RESP_LIST_TOOLS` class builds the response for listing available tools.

Tool structure:

```abap
TYPES: BEGIN OF tool,
         name          TYPE string,       " Unique tool identifier
         description   TYPE string,       " Tool description
         title         TYPE string,       " Tool title (optional)
         input_schema  TYPE REF TO zif_mcp_ajson, " JSON Schema for inputs
         output_schema TYPE REF TO zif_mcp_ajson, " JSON Schema for outputs (optional)
         annotations   TYPE tool_annotations, " Additional properties
         execution     TYPE execution, " Task support metadata (optional)
         icons         TYPE zif_mcp_types=>icon_list, " Tool icons (optional)
         meta          TYPE REF TO zif_mcp_ajson, " Tool metadata (optional)
       END OF tool.

TYPES: BEGIN OF execution,
         task_support TYPE string, " forbidden / optional / required
       END OF execution.

TYPES: BEGIN OF tool_annotations,
         title           TYPE string,     " Annotation title
         readonlyhint    TYPE abap_bool, " Tool only reads data
         destructivehint TYPE abap_bool, " Tool modifies/deletes data
         destructivehint_set TYPE abap_bool, " Emit explicit destructiveHint value
         idempotenthint  TYPE abap_bool, " Tool can be called multiple times safely
         openworldhint   TYPE abap_bool, " Tool accepts additional parameters
         openworldhint_set TYPE abap_bool, " Emit explicit openWorldHint value
       END OF tool_annotations.
```

Key methods:
- `set_tools()`: Sets the list of available tools with their schemas
- `set_next_cursor()`: Sets pagination cursor for retrieving the next page
- `set_meta()`: Sets optional metadata for the response

Example:

```abap
METHOD handle_list_tools.
  DATA tools TYPE zcl_mcp_resp_list_tools=>tools.
  
  TRY.
      " Tool without parameters"
      APPEND VALUE #( 
          name = 'get_server_time'
          description = 'Get the current server date and time'
      ) TO tools.
      
      " Tool with input schema
      DATA(schema) = NEW zcl_mcp_schema_builder( ).
      schema->add_string( 
          name = 'customer_id'
          description = 'Customer ID to look up'
          required = abap_true 
      ).
      
      APPEND VALUE #( 
          name = 'get_customer_info'
          title = 'Customer Information Lookup'
          description = 'Retrieve customer information by ID'
          input_schema = schema->to_json( )
          annotations = VALUE #( readonlyhint = abap_true )
      ) TO tools.
      
      response-result->set_tools( tools ).
      
  CATCH zcx_mcp_ajson_error INTO DATA(error).
      response-error-code = zcl_mcp_jsonrpc=>error_codes-internal_error.
      response-error-message = error->get_text( ).
  ENDTRY.
ENDMETHOD.
```

### Call Tool Response

The `ZCL_MCP_RESP_CALL_TOOL` class builds the response for tool execution results.

Key methods:
- `add_text_content()`: Adds text output from the tool
- `add_image_content()`: Adds image output from the tool
- `add_audio_content()`: Adds audio output from the tool
- `add_text_resource()`: Adds text resource output from the tool
- `add_blob_resource()`: Adds binary resource output from the tool
- `add_resource_link()`: Adds a resource link output
- `set_structured_content()`: Adds machine-readable JSON output, optionally with an auto-generated text item
- `set_task_result()`: Returns a task object for task-augmented execution
- `set_error()`: Indicates that the tool execution resulted in an error

## Implementing Tool Handlers

Override these methods in your MCP server:

```abap
METHODS handle_list_tools REDEFINITION.
METHODS handle_call_tool REDEFINITION.
```

### Handling List Tools

```abap
METHOD handle_list_tools.
  DATA tools TYPE zcl_mcp_resp_list_tools=>tools.
  
  TRY.
      " Simple tool"
      APPEND VALUE #(
          name = 'get_server_time'
          description = 'Get the current server date and time'
      ) TO tools.
      
      " Complex tool with schema
      APPEND VALUE #(
          name = 'get_flight_details'
          description = 'Get details of flight connections'
          input_schema = get_flight_schema( )->to_json( )
      ) TO tools.
      
      response-result->set_tools( tools ).
      
  CATCH zcx_mcp_ajson_error INTO DATA(error).
      response-error-code = zcl_mcp_jsonrpc=>error_codes-internal_error.
      response-error-message = error->get_text( ).
  ENDTRY.
ENDMETHOD.
```

### Handling Tool Calls

```abap
METHOD handle_call_tool.
  CASE request->get_name( ).
    WHEN 'get_server_time'.
      get_server_time( CHANGING response = response ).
      
    WHEN 'get_flight_details'.
      get_flight_details( 
        EXPORTING request = request
        CHANGING response = response 
      ).
      
    WHEN OTHERS.
      response-error-code = zcl_mcp_jsonrpc=>error_codes-invalid_params.
      response-error-message = |Tool { request->get_name( ) } not found|.
  ENDCASE.
ENDMETHOD.
```

## Tool Input/Output Validation

Use `ZCL_MCP_SCHEMA_BUILDER` to define schemas and `ZCL_MCP_SCHEMA_VALIDATOR` to validate inputs:

```abap
" Define schema
METHOD get_flight_schema.
  DATA(schema) = NEW zcl_mcp_schema_builder( ).
  schema->add_string( 
      name = 'airline_code'
      description = 'Airline Code'
      required = abap_true
      enum = VALUE #( ( 'AA' ) ( 'LH' ) ( 'BA' ) )
  )->add_integer(
      name = 'flight_number'
      description = 'Flight Number'
      minimum = 0
      maximum = 9999
      required = abap_true
  ).
  result = schema.
ENDMETHOD.

" Validate input in tool implementation
METHOD get_flight_details.
  DATA(input) = request->get_arguments( ).
  
  TRY.
      " Validate input against schema
      DATA(schema) = get_flight_schema( ).
      DATA(validator) = NEW zcl_mcp_schema_validator( schema->to_json( ) ).
      
      IF validator->validate( input ) = abap_false.
        response-error-code = zcl_mcp_jsonrpc=>error_codes-invalid_params.
        response-error-message = concat_lines_of( validator->get_errors( ) ).
        RETURN.
      ENDIF.
      
      " Extract validated parameters
      DATA(airline_code) = input->get_string( 'airline_code' ).
      DATA(flight_number) = input->get_integer( 'flight_number' ).
      
      " Process tool logic...
      
  CATCH zcx_mcp_ajson_error INTO DATA(error).
      response-error-code = zcl_mcp_jsonrpc=>error_codes-internal_error.
      response-error-message = error->get_text( ).
  ENDTRY.
ENDMETHOD.
```

## Tool Response Types

Tools can return multiple content types:

1. **Text Content**:
   ```abap
   response-result->add_text_content( 'Operation completed successfully.' ).
   ```

2. **Markdown Content**:
   ```abap
   DATA(markdown) = |## Results\n\n* Item 1\n* Item 2|.
   response-result->add_text_content( markdown ).
   ```

3. **Image Content**:
   ```abap
   response-result->add_image_content(
     data = lv_base64_image
     mime_type = 'image/png'
   ).
   ```

4. **Resource Content**:
   ```abap
   response-result->add_text_resource(
     uri = 'reports://sales/2023'
     text = lv_report_content
     mime_type = 'text/csv'
   ).
   ```

## Tool Annotations

Annotations provide hints about tool behavior:

- `readonlyhint`: Tool only reads data
- `destructivehint`: Tool modifies or deletes data
- `idempotenthint`: Tool can be called multiple times safely
- `openworldhint`: Tool accepts additional parameters

The MCP defaults for `destructiveHint` and `openWorldHint` are `true`. Set `destructivehint_set = abap_true` or `openworldhint_set = abap_true` when you need to emit an explicit `abap_false` value.

```abap
APPEND VALUE #(
    name = 'update_customer'
    description = 'Update customer information'
    input_schema = schema->to_json( )
    annotations = VALUE #(
        destructivehint = abap_true
        title = 'Customer Update'
    )
) TO tools.
```

## Output Schemas

Tools can optionally declare an `output_schema` so clients know the structure of the JSON result. Use `ZCL_MCP_SCHEMA_BUILDER` to define it:

```abap
DATA(output) = NEW zcl_mcp_schema_builder( ).
output->add_string( name = 'status'  description = 'Result status' required = abap_true )
      ->add_string( name = 'message' description = 'Details'       required = abap_false ).

APPEND VALUE #(
    name          = 'process_order'
    description   = 'Process a sales order'
    input_schema  = input->to_json( )
    output_schema = output->to_json( )
) TO tools.
```

## Background Task Support

Tools can signal that they support (or require) asynchronous execution via `execution-task_support`. When set, `handle_call_tool` can create a task via `get_tasks( )->create_task`, start a background job, and return the task object with `response-result->set_task_result( task )` instead of returning normal content directly.

This section describes the legacy tool/task API. Draft v2 servers use `ZCL_MCP_SERVER_BASE_V2`, modern result helpers, and the `io.modelcontextprotocol/tasks` extension; see [V2 Support](V2.md) and [Tasks](Tasks.md) for the v2 flow.

```abap
APPEND VALUE #(
    name          = 'start_long_export'
    description   = 'Export large dataset — runs as a background task'
    input_schema  = input->to_json( )
    execution     = VALUE #( task_support = zcl_mcp_resp_list_tools=>task_support-optional )
) TO tools.
```

| `task_support` constant  | Meaning |
| ------------------------ | ------- |
| `task_support-forbidden` | Tool does not support task-augmented execution (the default when `execution` is omitted) |
| `task_support-optional`  | Client may poll for a task; a synchronous result is also acceptable |
| `task_support-required`  | Tool requires task-augmented execution and returns a task object |

The framework validates task support against the listed tools before dispatching `tools/call`: it rejects task requests for tools that advertise `task_support-forbidden`, and rejects synchronous calls for tools that advertise `task_support-required`.

See [Tasks](Tasks.md) for the full task lifecycle API.

## Examples

### Simple Tool Implementation

```abap
METHOD get_server_time.
  response-result->add_text_content( 
    |Current Server Date: { sy-datum } Time: { sy-uzeit } in internal format.|
  ).
ENDMETHOD.
```

### Database Query Tool with Validation

```abap
METHOD get_flight_details.
  DATA(input) = request->get_arguments( ).
  
  " Validate input
  TRY.
      DATA(validator) = NEW zcl_mcp_schema_validator( get_flight_schema( )->to_json( ) ).
      IF validator->validate( input ) = abap_false.
        response-error-code = zcl_mcp_jsonrpc=>error_codes-invalid_params.
        response-error-message = concat_lines_of( validator->get_errors( ) ).
        RETURN.
      ENDIF.
  CATCH zcx_mcp_ajson_error INTO DATA(error).
      response-error-code = zcl_mcp_jsonrpc=>error_codes-internal_error.
      response-error-message = error->get_text( ).
      RETURN.
  ENDTRY.
  
  " Extract parameters
  DATA(airline_code) = input->get_string( 'airline_code' ).
  DATA(flight_number) = input->get_integer( 'flight_number' ).
  
  " Query database
  SELECT carrid, connid, fldate, price, currency, planetype
    FROM sflight
    WHERE carrid = @airline_code AND connid = @flight_number
    ORDER BY fldate
    INTO TABLE @DATA(flights).
    
  IF sy-subrc = 0.
    " Format as markdown table
    DATA(markdown) = |## Flight Connection Details\n\n|.
    markdown = |{ markdown }| Airline | Connection | Flight Date | Price | Currency | Plane Type |\n|.
    markdown = |{ markdown }|---------|------------|-------------|-------|----------|------------|\n|.
    
    LOOP AT flights ASSIGNING FIELD-SYMBOL(<flight>).
      markdown = |{ markdown }| { <flight>-carrid } | { <flight>-connid } | |
                 |{ <flight>-fldate DATE = USER } | { <flight>-price } | |
                 |{ <flight>-currency } | { <flight>-planetype } |\n|.
    ENDLOOP.
    
    response-result->add_text_content( markdown ).
  ELSE.
    response-result->add_text_content( 
      |No flights found for airline { airline_code } and connection { flight_number }|
    ).
  ENDIF.
ENDMETHOD.
```
