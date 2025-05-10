# MCP Server SDK: Tools

This documentation explains how to implement and use tools in the Model Context Protocol (MCP) Server SDK. Tools are a powerful feature that allow AI assistants to execute functions in your ABAP systems, enabling dynamic interaction with your business data and processes.

## Table of Contents

- [MCP Server SDK: Tools](#mcp-server-sdk-tools)
  - [Table of Contents](#table-of-contents)
  - [Overview](#overview)
  - [Tool Request Classes](#tool-request-classes)
    - [List Tools Request](#list-tools-request)
    - [Call Tool Request](#call-tool-request)
  - [Tool Response Classes](#tool-response-classes)
    - [List Tools Response](#list-tools-response)
    - [Call Tool Response](#call-tool-response)
  - [Implementing Tool Handlers](#implementing-tool-handlers)
    - [Handling List Tools](#handling-list-tools)
    - [Handling Tool Calls](#handling-tool-calls)
  - [Tool Input Validation](#tool-input-validation)
  - [Tool Response Types](#tool-response-types)
  - [Tool Annotations](#tool-annotations)
  - [Examples](#examples)
    - [Simple Information Tools](#simple-information-tools)
    - [Database Query Tools](#database-query-tools)
    - [Stateful Tools](#stateful-tools)

## Overview

Tools in the MCP protocol are functions that AI assistants can call to:

- Retrieve information from your SAP system
- Perform calculations or data processing
- Execute business operations
- Access system features

Each tool is identified by a unique name and can accept structured input parameters defined by a JSON schema. Tool responses can include various content types, such as text, images, or resources.

## Tool Request Classes

### List Tools Request

The `ZCL_MCP_REQ_LIST_TOOLS` class handles requests to list available tools on the server.

Key methods:

- `get_cursor()`: Retrieves the pagination cursor if provided
- `has_cursor()`: Checks if a pagination cursor was provided

Example:

```abap
METHOD handle_list_tools.
  " Check if pagination is being used
  IF request->has_cursor( ).
    " Handle pagination with the cursor
    DATA(cursor) = request->get_cursor( ).
    " Use cursor to determine which page of tools to return
  ENDIF
  
  " Set up response...
ENDMETHOD.
```

### Call Tool Request

The `ZCL_MCP_REQ_CALL_TOOL` class handles requests to execute a specific tool with arguments.

Key methods:

- `get_name()`: Retrieves the requested tool name
- `has_arguments()`: Checks if arguments were provided
- `get_arguments()`: Retrieves the arguments as a JSON object

Example:

```abap
METHOD handle_call_tool.
  " Get the requested tool name
  DATA(tool_name) = request->get_name( ).
  
  CASE tool_name.
    WHEN 'get_customer_info'.
      " Access arguments as JSON
      DATA(args) = request->get_arguments( ).
      
      " Extract specific parameters with type checking
      DATA(customer_id) = args->get_string( 'customer_id' ).
      
      " Execute the tool functionality
      get_customer_info(
        EXPORTING customer_id = customer_id
        CHANGING response = response
      ).
      
    WHEN OTHERS.
      " Handle unknown tool
      response-error-code = zcl_mcp_jsonrpc=>error_codes-invalid_params.
      response-error-message = |Tool { tool_name } not found|.
  ENDCASE.
ENDMETHOD.
```

## Tool Response Classes

### List Tools Response

The `ZCL_MCP_RESP_LIST_TOOLS` class builds the response for listing available tools.

Key methods:

- `set_tools()`: Sets the list of available tools with their schemas
- `set_next_cursor()`: Sets pagination cursor for retrieving the next page
- `set_meta()`: Sets optional metadata for the response

Tool structure:

```abap
TYPES: BEGIN OF tool,
         name         TYPE string,       " Unique tool identifier
         description  TYPE string,       " Tool description
         input_schema TYPE REF TO zif_mcp_ajson, " JSON Schema for inputs
         annotations  TYPE tool_annotations, " Additional properties
       END OF tool.
```

Example:

```abap
METHOD handle_list_tools.
  " Create tools list
  DATA tools TYPE zcl_mcp_resp_list_tools=>tools.
  
  " Create schema for customer lookup tool
  TRY.
      DATA(schema) = NEW zcl_mcp_schema_builder( ).
      schema->add_string( 
          name = 'customer_id'
          description = 'Customer ID to look up'
          required = abap_true 
      ).
      
      " Add tool with schema
      APPEND VALUE #( 
          name = 'get_customer_info'
          description = 'Retrieve customer information by ID'
          input_schema = schema->to_json( )
      ) TO tools.
      
      " Set the tools in the response
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

Example:

```abap
METHOD get_customer_info.
  " Extract customer ID from arguments
  DATA(customer_id) = request->get_arguments( )->get_string( 'customer_id' ).
  
  " Perform database query
  SELECT single name, email, customer_type, created_on
    FROM customers
    WHERE id = @customer_id
    INTO @DATA(customer).
    
  IF sy-subrc = 0.
    " Format results as markdown
    DATA(output) = |## Customer Information\n\n|.
    output = |{ output }**ID:** { customer_id }\n|.
    output = |{ output }**Name:** { customer-name }\n|.
    output = |{ output }**Email:** { customer-email }\n|.
    output = |{ output }**Type:** { customer-customer_type }\n|.
    output = |{ output }**Created:** { customer-created_on DATE = USER }\n|.
    
    " Return as text content
    response-result->add_text_content( output ).
  ELSE.
    " Handle not found case
    response-result->set_error( abap_true ).
    response-result->add_text_content( |Customer with ID { customer_id } not found.| ).
  ENDIF.
ENDMETHOD.
```

## Implementing Tool Handlers

To implement tools in your MCP server, you need to override the following methods:

```abap
METHODS handle_list_tools REDEFINITION.
METHODS handle_call_tool REDEFINITION.
```

### Handling List Tools

The `handle_list_tools` method should return a list of available tools with their descriptions and input schemas:

```abap
METHOD handle_list_tools.
  DATA tools TYPE zcl_mcp_resp_list_tools=>tools.
  
  TRY.
      " Tool 1: Current time
      APPEND VALUE #(
          name = 'get_server_time'
          description = 'Get the current server date and time'
      ) TO tools.
      
      " Tool 2: Flight search with schema
      DATA(schema) = NEW zcl_mcp_schema_builder( ).
      schema->add_string( 
          name = 'airline' 
          description = 'Airline code (e.g., LH, AA)' 
          required = abap_true 
      ).
      schema->add_string( 
          name = 'destination' 
          description = 'Destination airport code'
      ).
      
      APPEND VALUE #(
          name = 'search_flights'
          description = 'Search for available flights'
          input_schema = schema->to_json( )
      ) TO tools.
      
      " Set the tools in the response
      response-result->set_tools( tools ).
      
  CATCH zcx_mcp_ajson_error INTO DATA(error).
      response-error-code = zcl_mcp_jsonrpc=>error_codes-internal_error.
      response-error-message = error->get_text( ).
  ENDTRY.
ENDMETHOD.
```

### Handling Tool Calls

The `handle_call_tool` method acts as a dispatcher for tool calls:

```abap
METHOD handle_call_tool.
  CASE request->get_name( ).
    WHEN 'get_server_time'.
      get_server_time( 
        EXPORTING request = request
        CHANGING response = response 
      ).
      
    WHEN 'search_flights'.
      search_flights( 
        EXPORTING request = request
        CHANGING response = response 
      ).
      
    WHEN OTHERS.
      " Unknown tool
      response-error-code = zcl_mcp_jsonrpc=>error_codes-invalid_params.
      response-error-message = |Tool { request->get_name( ) } not found|.
  ENDCASE.
ENDMETHOD.
```

## Tool Input Validation

Tool inputs are defined using JSON Schema. Use the `ZCL_MCP_SCHEMA_BUILDER` class to create schemas for your tools:

```abap
DATA(schema) = NEW zcl_mcp_schema_builder( ).

" Add basic parameters
schema->add_string( name = 'customer_id' required = abap_true )
      ->add_integer( name = 'max_results' )
      ->add_boolean( name = 'include_details' ).

" Add complex parameters
schema->begin_object( name = 'filter' )
      ->add_string( name = 'region' )
      ->add_string( name = 'category' )
      ->end_object( ).

" Use in tool definition
APPEND VALUE #(
    name = 'get_customer_orders'
    description = 'Get orders for a customer'
    input_schema = schema->to_json( )
) TO tools.
```

## Tool Response Types

The Call Tool response supports multiple content types:

1. **Text Content**: Simple text or markdown output

   ```abap
   response-result->add_text_content( 'Operation completed successfully.' ).
   ```

2. **Rich Text Content**: Formatted text (e.g., markdown)

   ```abap
   DATA(markdown) = |## Flight Results\n\n|.
   markdown = |{ markdown }| & |* Flight 1: LH123 - Frankfurt to New York\n|.
   markdown = |{ markdown }| & |* Flight 2: LH456 - Frankfurt to Boston\n|.
   response-result->add_text_content( markdown ).
   ```

3. **Image Content**: Base64-encoded images

   ```abap
   response-result->add_image_content(
     data = lv_base64_chart_image
     mime_type = 'image/png'
   ).
   ```

4. **Resource Content**: References to text or binary resources

   ```abap
   response-result->add_text_resource(
     uri = 'reports://sales/2023'
     text = lv_report_content
     mime_type = 'text/csv'
   ).
   ```

## Tool Annotations

Tools can include annotations that provide hints about their behavior:

```abap
APPEND VALUE #(
    name = 'get_customer_data'
    description = 'Retrieve customer data'
    input_schema = schema->to_json( )
    annotations = VALUE #(
        title = 'Customer Information'
        readonlyhint = abap_true  " Indicates this tool only reads data
    )
) TO tools.

APPEND VALUE #(
    name = 'update_customer'
    description = 'Update customer information'
    input_schema = update_schema->to_json( )
    annotations = VALUE #(
        title = 'Update Customer'
        destructivehint = abap_true  " Indicates this tool modifies data
    )
) TO tools.
```

## Examples

### Simple Information Tools

```abap
METHOD get_server_time.
  " Simple tool that returns the current server time
  DATA(date_str) = |{ sy-datum DATE = USER }|.
  DATA(time_str) = |{ sy-uzeit TIME = USER }|.
  
  response-result->add_text_content(
    |Current Server Date: { date_str }\nCurrent Server Time: { time_str }|
  ).
ENDMETHOD.
```

### Database Query Tools

```abap
METHOD search_flights.
  DATA(args) = request->get_arguments( ).
  DATA(airline) = args->get_string( 'airline' ).
  
  " Optional parameter
  DATA(destination) = ''.
  IF args->exists( 'destination' ).
    destination = args->get_string( 'destination' ).
  ENDIF.
  
  " Perform database query with parameters
  SELECT carrid, connid, cityfrom, cityto, fldate, price, currency
    FROM spfli
    INNER JOIN sflight ON spfli~carrid = sflight~carrid 
                       AND spfli~connid = sflight~connid
    WHERE spfli~carrid = @airline
    AND ( @destination IS INITIAL OR spfli~cityto = @destination )
    INTO TABLE @DATA(flights)
    UP TO 10 ROWS.
    
  IF sy-subrc = 0 AND lines( flights ) > 0.
    " Format results as markdown table
    DATA(output) = |## Available Flights\n\n|.
    output = |{ output }| & |Airline | Flight | From | To | Date | Price\n|.
    output = |{ output }| & |--------|--------|------|----|----- |------\n|.
    
    LOOP AT flights ASSIGNING FIELD-SYMBOL(<flight>).
      output = |{ output }| & 
        |{ <flight>-carrid } | { <flight>-connid } | { <flight>-cityfrom } | | 
        |{ <flight>-cityto } | { <flight>-fldate DATE = USER } | | 
        |{ <flight>-price } { <flight>-currency }\n|.
    ENDLOOP.
    
    response-result->add_text_content( output ).
  ELSE.
    " No results
    response-result->add_text_content( 
      |No flights found for airline { airline }| & 
      |{ COND #( WHEN destination IS NOT INITIAL THEN | to { destination }| ELSE '' ) }.|
    ).
  ENDIF.
ENDMETHOD.
```

### Stateful Tools

Tools can maintain state between calls when used with session support:

```abap
METHOD increment_counter.
  DATA(args) = request->get_arguments( ).
  DATA(increment_by) = args->get_integer( 'increment_by' ).
  
  " Get current counter value from session
  DATA(current_value) = 0.
  IF session IS BOUND.
    DATA(counter) = session->get( 'counter' ).
    IF counter IS NOT INITIAL.
      current_value = counter-value.
    ENDIF.
  ENDIF.
  
  " Increment counter
  current_value = current_value + increment_by.
  
  " Store updated value in session
  IF session IS BOUND.
    session->add( VALUE #(
      key = 'counter'
      value = current_value
    ) ).
  ENDIF.
  
  " Return result
  response-result->add_text_content( 
    |Counter incremented by { increment_by }. New value: { current_value }|
  ).
ENDMETHOD.
```
