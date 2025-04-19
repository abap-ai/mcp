# MCP Server SDK: Resources

This documentation explains how to work with resources in the Model Context Protocol (MCP) Server SDK. Resources provide a way for AI models to access files, data, and other content from your ABAP systems.

## Table of Contents

- [Overview](#overview)
- [Resource Request Classes](#resource-request-classes)
  - [List Resources Request](#list-resources-request)
  - [List Resource Templates Request](#list-resource-templates-request)
  - [Read Resource Request](#read-resource-request)
- [Resource Response Classes](#resource-response-classes)
  - [List Resources Response](#list-resources-response)
  - [List Resource Templates Response](#list-resource-templates-response)
  - [Read Resource Response](#read-resource-response)
- [Implementing Resource Handlers](#implementing-resource-handlers)
  - [Static Resources](#static-resources)
  - [Dynamic Resources](#dynamic-resources)
  - [Pagination](#pagination)
- [Examples](#examples)

## Overview

The MCP protocol provides a standardized way for AI assistants to access resources from your ABAP systems. These resources can be any type of content:

- Code snippets
- Documentation
- Database records
- Reports
- Binary files

There are two primary types of resources in MCP:

1. **Static Resources**: Fixed resources with defined URIs
2. **Dynamic Resources**: Templates that can generate resources based on parameters

## Resource Request Classes

### List Resources Request

The `ZCL_MCP_REQ_LIST_RESOURCES` class handles requests to list available resources on the server.

Key methods:

- `get_cursor()`: Retrieves the pagination cursor if provided
- `has_cursor()`: Checks if a pagination cursor was provided

Example:

```abap
METHOD handle_list_resources.
  " Check if pagination is being used
  IF request->has_cursor( ).
    " Handle pagination with the cursor
    DATA(cursor) = request->get_cursor( ).
    " Use cursor to determine which page of resources to return
  ENDIF
  
  " Set up response...
ENDMETHOD.
```

### List Resource Templates Request

The `ZCL_MCP_REQ_LIST_RES_TMPLS` class handles requests to list resource templates for dynamic resources.

Key methods:

- `get_cursor()`: Retrieves the pagination cursor if provided
- `has_cursor()`: Checks if a pagination cursor was provided

### Read Resource Request

The `ZCL_MCP_REQ_READ_RESOURCE` class handles requests to read specific resource content.

Key methods:

- `get_uri()`: Retrieves the URI of the requested resource

Example:

```abap
METHOD handle_resources_read.
  " Get the requested resource URI
  DATA(uri) = request->get_uri( ).
  
  " Load and return the requested resource
  CASE uri.
    WHEN 'abap://classes/zcl_my_class'.
      " Return class source code
    WHEN 'db://sales/orders/12345'.
      " Return order data
    ELSE.
      " Handle unknown resource
  ENDCASE
ENDMETHOD.
```

## Resource Response Classes

### List Resources Response

The `ZCL_MCP_RESP_LIST_RESOURCES` class builds the response for listing available resources.

Key methods:

- `set_resources()`: Sets the list of available resources
- `set_next_cursor()`: Sets pagination cursor for retrieving the next page
- `set_meta()`: Sets optional metadata for the response

Resource structure:

```abap
TYPES: BEGIN OF resource,
         uri         TYPE string,  " Unique identifier/path for the resource
         name        TYPE string,  " Display name for the resource
         description TYPE string,  " Description of the resource
         mime_type   TYPE string,  " MIME type (e.g., text/x-abap, application/pdf)
         annotations TYPE annotations, " Optional annotations (audience, priority)
         size        TYPE i,       " Optional size in bytes
       END OF resource.
```

Example:

```abap
METHOD handle_list_resources.
  " Create resources list
  DATA(resources) = VALUE zcl_mcp_resp_list_resources=>resources(
    ( uri         = 'abap://classes/zcl_my_utility'
      name        = 'My Utility Class'
      description = 'Utility class for XYZ operations'
      mime_type   = 'text/x-abap' )
    ( uri         = 'db://sales/top_customers'
      name        = 'Top Customers Report'
      description = 'List of top customers by revenue'
      mime_type   = 'text/csv' )
  ).
  
  " Set the resources in the response
  response-result->set_resources( resources ).
  
  " Optionally set pagination cursor if there are more resources
  IF more_resources_available = abap_true.
    response-result->set_next_cursor( 'page2' ).
  ENDIF.
ENDMETHOD.
```

### List Resource Templates Response

The `ZCL_MCP_RESP_LIST_RES_TMPL` class builds the response for listing available resource templates.

Key methods:

- `set_resource_templates()`: Sets the list of available resource templates
- `set_next_cursor()`: Sets pagination cursor for retrieving the next page
- `set_meta()`: Sets optional metadata for the response

Resource template structure:

```abap
TYPES: BEGIN OF resource_template,
         uritemplate TYPE string,  " Template with placeholders (e.g., 'orders/{order_id}')
         name        TYPE string,  " Display name for the template
         description TYPE string,  " Description of the template
         mime_type   TYPE string,  " MIME type of generated resources
         annotations TYPE annotations, " Optional annotations
       END OF resource_template.
```

Example:

```abap
METHOD handle_list_res_tmpls.
  " Create template list
  DATA(templates) = VALUE zcl_mcp_resp_list_res_tmpl=>resource_templates(
    ( uritemplate = 'orders/{order_id}'
      name        = 'Order Details'
      description = 'Details for a specific sales order'
      mime_type   = 'application/json' )
    ( uritemplate = 'users/{username}/profile'
      name        = 'User Profile'
      description = 'User profile information'
      mime_type   = 'application/json' )
  ).
  
  " Set the templates in the response
  response-result->set_resource_templates( templates ).
ENDMETHOD.
```

### Read Resource Response

The `ZCL_MCP_RESP_READ_RESOURCE` class builds the response for reading a specific resource's content.

Key methods:

- `add_text_resource()`: Adds a text-based resource to the response
- `add_blob_resource()`: Adds a binary resource (base64-encoded) to the response
- `set_contents()`: Sets the complete list of resource contents
- `set_meta()`: Sets optional metadata for the response

Example:

```abap
METHOD handle_resources_read.
  CASE request->get_uri( ).
    WHEN 'abap://classes/zcl_my_utility'.
      " Return ABAP class source code
      DATA(source_code) = get_class_source( 'ZCL_MY_UTILITY' ).
      response-result->add_text_resource(
        uri       = request->get_uri( )
        text      = source_code
        mime_type = 'text/x-abap'
      ).
      
    WHEN 'file://reports/sales_summary.pdf'.
      " Return binary PDF file as base64
      DATA(pdf_content) = get_pdf_as_base64( 'sales_summary.pdf' ).
      response-result->add_blob_resource(
        uri       = request->get_uri( )
        blob      = pdf_content
        mime_type = 'application/pdf'
      ).
      
    ELSE.
      " Resource not found
      response-error-code = zcl_mcp_jsonrpc=>error_codes-invalid_params.
      response-error-message = |Resource { request->get_uri( ) } not found|.
  ENDCASE.
ENDMETHOD.
```

## Implementing Resource Handlers

To implement resources in your MCP server, you need to override the following methods in your server class:

```abap
METHODS handle_list_resources REDEFINITION.
METHODS handle_list_res_tmpls REDEFINITION.
METHODS handle_resources_read REDEFINITION.
```

### Static Resources

Static resources have fixed URIs and content. Use a consistent URI scheme for your resources:

```
<protocol>://<category>/<path>
```

Examples:

- `abap://classes/zcl_my_class` - ABAP class source code
- `file://documentation/user_guide.md` - Markdown documentation file
- `db://customers/top_10` - Database query result

### Dynamic Resources

Dynamic resources are generated from templates with parameters. URI templates follow this pattern:

```
<protocol>://<category>/<path_with_{parameters}>
```

Examples:

- `orders/{order_id}` - Details for a specific order
- `users/{username}/profile` - A specific user's profile

### Pagination

If you have a large number of resources, implement pagination using cursors:

1. In your `handle_list_resources` method, check if a cursor is provided
2. Return a limited number of resources
3. If more resources are available, set the next cursor

## Examples

### Basic Resource Implementation

```abap
METHOD handle_list_resources.
  " Return a list of available resources
  DATA(resources) = VALUE zcl_mcp_resp_list_resources=>resources(
    ( uri         = 'abap://classes/zcl_demo'
      name        = 'Demo Class'
      description = 'Example ABAP class'
      mime_type   = 'text/x-abap' )
  ).
  
  response-result->set_resources( resources ).
ENDMETHOD.

METHOD handle_resources_read.
  " Handle the resource read request
  IF request->get_uri( ) = 'abap://classes/zcl_demo'.
    " Return the class source code
    DATA(source) = |CLASS zcl_demo DEFINITION...|.
    
    response-result->add_text_resource(
      uri       = request->get_uri( )
      text      = source
      mime_type = 'text/x-abap'
    ).
  ELSE.
    " Resource not found
    response-error-code = zcl_mcp_jsonrpc=>error_codes-invalid_params.
    response-error-message = |Resource { request->get_uri( ) } not found|.
  ENDIF.
ENDMETHOD.
```

### Dynamic Resource Templates

```abap
METHOD handle_list_res_tmpls.
  " Return a list of resource templates
  DATA(templates) = VALUE zcl_mcp_resp_list_res_tmpl=>resource_templates(
    ( uritemplate = 'sales_order/{sales_order_id}'
      name        = 'Sales Order Details'
      description = 'Details for a specific sales order'
      mime_type   = 'application/json' )
  ).
  
  response-result->set_resource_templates( templates ).
ENDMETHOD.

METHOD handle_resources_read.
  " Check if this is a dynamic resource
  DATA(uri) = request->get_uri( ).
  
  " Extract sales order ID from dynamic resource URI
  FIND REGEX '^sales_order/(\d+)$' IN uri SUBMATCHES DATA(sales_order_id).
  
  IF sy-subrc = 0.
    " Found a sales order ID, retrieve the data
    DATA(order_data) = get_sales_order_json( sales_order_id ).
    
    response-result->add_text_resource(
      uri       = uri
      text      = order_data
      mime_type = 'application/json'
    ).
  ELSE.
    " Not a valid dynamic resource
    response-error-code = zcl_mcp_jsonrpc=>error_codes-invalid_params.
    response-error-message = |Resource { uri } not found|.
  ENDIF.
ENDMETHOD.
```

---

By implementing these methods in your MCP server, you provide AI assistants with access to your ABAP system's data and resources in a standardized, secure way.
