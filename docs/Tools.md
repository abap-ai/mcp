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
- [Examples](#examples)

## Overview

Tools in the MCP protocol are functions that AI assistants can call to:

- Retrieve information from your SAP system
- Perform calculations or data processing
- Execute business operations
- Access system features

Each tool is identified by a unique name and can accept structured input parameters defined by a JSON schema.

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
         meta          TYPE REF TO zif_mcp_ajson, " Tool metadata (optional)
       END OF tool.

TYPES: BEGIN OF tool_annotations,
         title           TYPE string,     " Annotation title
         readonlyhint    TYPE abap_bool, " Tool only reads data
         destructivehint TYPE abap_bool, " Tool modifies/deletes data
         idempotenthint  TYPE abap_bool, " Tool can be called multiple times safely
         openworldhint   TYPE abap_bool, " Tool accepts additional parameters
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
- `add_text_resource()`: Adds text resource output from the tool
- `add_blob_resource()`: Adds binary resource output from the tool
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