# JSON Schema Builder for MCP Server SDK

The JSON Schema Builder (`ZCL_MCP_SCHEMA_BUILDER`) is a utility class that simplifies creating JSON Schema definitions for your Model Context Protocol tools, allowing you to define input validation rules with a chainable API.

## Table of Contents

- [Overview](#overview)
- [Basic Usage](#basic-usage)
- [Basic Property Types](#basic-property-types)
  - [String Properties](#string-properties)
  - [Number Properties](#number-properties)
  - [Integer Properties](#integer-properties)
  - [Boolean Properties](#boolean-properties)
- [Property Validation](#property-validation)
  - [String Validation](#string-validation)
  - [Number and Integer Validation](#number-and-integer-validation)
  - [Array Validation](#array-validation)
- [Defining Required Properties](#defining-required-properties)
- [Creating Objects](#creating-objects)
- [Creating Arrays](#creating-arrays)
- [Complex Schema Examples](#complex-schema-examples)
- [API Reference](#api-reference)

## Overview

JSON Schema is used in the MCP Server SDK to define the expected input format for tools that can be called by AI models. The Schema Builder provides a fluent interface that makes creating complex schemas simple and readable.

Key features:

- Easy definition of property types (string, number, integer, boolean)
- Rich validation options for property values
- Support for nested objects and arrays
- Fluent interface for chainable method calls

## Basic Usage

```abap
TRY.
    " Create a new schema builder
    DATA(schema_builder) = NEW zcl_mcp_schema_builder( ).
    
    " Add properties with method chaining
    schema_builder->add_string( name = 'name' required = abap_true )
                  ->add_integer( name = 'age' minimum = 18 )
                  ->add_boolean( name = 'active' ).
                  
    " Convert to JSON for use in your tool definition
    DATA(json_schema) = schema_builder->to_json( ).
    
    " Use the schema in your tool definition
    tools = VALUE #( ( name = 'my_tool'
                       description = 'Does something useful'
                       input_schema = json_schema ) ).
                       
CATCH zcx_mcp_ajson_error INTO DATA(error).
    " Handle schema creation errors
ENDTRY.
```

## Basic Property Types

### String Properties

```abap
schema_builder->add_string( 
    name = 'username'
    description = 'The user login name'
    required = abap_true
).
```

### Number Properties

```abap
schema_builder->add_number(
    name = 'price'
    description = 'Product price with decimal places'
).
```

### Integer Properties

```abap
schema_builder->add_integer(
    name = 'quantity'
    description = 'Number of items to order'
).
```

### Boolean Properties

```abap
schema_builder->add_boolean(
    name = 'is_active'
    description = 'Whether the account is active'
).
```

## Property Validation

### String Validation

You can apply length constraints and enumeration validation to strings:

```abap
" Length validation
schema_builder->add_string(
    name = 'password'
    description = 'User password'
    min_length = 8
    max_length = 30
).

" Enumeration validation
DATA(status_values) = VALUE string_table( ( 'pending' ) ( 'active' ) ( 'cancelled' ) ).
schema_builder->add_string(
    name = 'status'
    description = 'Order status'
    enum = status_values
).
```

### Number and Integer Validation

You can set minimum and maximum constraints for numeric values:

```abap
" Number range validation
schema_builder->add_number(
    name = 'weight'
    description = 'Product weight in kg'
    minimum = 0.1
    maximum = 1000.0
).

" Integer range validation
schema_builder->add_integer(
    name = 'age'
    description = 'User age'
    minimum = 18
    maximum = 120
).
```

### Array Validation

You can constrain the number of items in an array:

```abap
schema_builder->begin_array(
    name = 'tags'
    description = 'Product tags'
    min_items = 1
    max_items = 10
)->add_string( name = 'value' )->end_array( ).
```

## Defining Required Properties

Mark properties as required by setting the `required` parameter:

```abap
schema_builder->add_string( name = 'username' required = abap_true )
              ->add_string( name = 'password' required = abap_true )
              ->add_string( name = 'email' required = abap_false ).
```

## Creating Objects

Use `begin_object` and `end_object` to create nested structures:

```abap
schema_builder->add_string( name = 'name' )
              ->begin_object( name = 'address' required = abap_true )
                  ->add_string( name = 'street' required = abap_true )
                  ->add_string( name = 'city' required = abap_true )
                  ->add_string( name = 'country' )
              ->end_object( ).
```

## Creating Arrays

Create arrays of objects using `begin_array` and `end_array`:

```abap
schema_builder->begin_array( name = 'tags' min_items = 1 )
                  ->add_string( name = 'value' required = abap_true )
                  ->add_string( name = 'color' )
              ->end_array( ).
```

## Complex Schema Examples

Combine these features to create complex schemas:

```abap
schema_builder->add_string( name = 'title' required = abap_true max_length = 100 )
              ->add_string( name = 'description' max_length = 500 )
              ->begin_object( name = 'author' required = abap_true )
                  ->add_string( name = 'name' required = abap_true )
                  ->add_string( name = 'email' )
              ->end_object( )
              ->begin_array( name = 'chapters' min_items = 1 )
                  ->add_string( name = 'title' required = abap_true )
                  ->add_integer( name = 'pages' minimum = 1 )
              ->end_array( ).
```

## API Reference

### Property Methods

```abap
METHODS add_string
  IMPORTING
    name        TYPE string
    description TYPE string OPTIONAL
    enum        TYPE string_table OPTIONAL
    required    TYPE abap_bool DEFAULT abap_false
    min_length  TYPE i OPTIONAL
    max_length  TYPE i OPTIONAL
  RETURNING
    VALUE(self) TYPE REF TO zcl_mcp_schema_builder.

METHODS add_number
  IMPORTING
    name        TYPE string
    description TYPE string OPTIONAL
    required    TYPE abap_bool DEFAULT abap_false
    minimum     TYPE f OPTIONAL
    maximum     TYPE f OPTIONAL
  RETURNING
    VALUE(self) TYPE REF TO zcl_mcp_schema_builder.

METHODS add_integer
  IMPORTING
    name        TYPE string
    description TYPE string OPTIONAL
    required    TYPE abap_bool DEFAULT abap_false
    minimum     TYPE i OPTIONAL
    maximum     TYPE i OPTIONAL
  RETURNING
    VALUE(self) TYPE REF TO zcl_mcp_schema_builder.

METHODS add_boolean
  IMPORTING
    name        TYPE string
    description TYPE string OPTIONAL
    required    TYPE abap_bool DEFAULT abap_false
  RETURNING
    VALUE(self) TYPE REF TO zcl_mcp_schema_builder.
```

### Structure Methods

```abap
METHODS begin_object
  IMPORTING
    name        TYPE string
    description TYPE string OPTIONAL
    required    TYPE abap_bool DEFAULT abap_false
  RETURNING
    VALUE(self) TYPE REF TO zcl_mcp_schema_builder.

METHODS end_object
  RETURNING
    VALUE(self) TYPE REF TO zcl_mcp_schema_builder.

METHODS begin_array
  IMPORTING
    name        TYPE string
    description TYPE string OPTIONAL
    required    TYPE abap_bool DEFAULT abap_false
    min_items   TYPE i OPTIONAL
    max_items   TYPE i OPTIONAL
  RETURNING
    VALUE(self) TYPE REF TO zcl_mcp_schema_builder.

METHODS end_array
  RETURNING
    VALUE(self) TYPE REF TO zcl_mcp_schema_builder.
```

### Output Method

```abap
METHODS to_json
  RETURNING
    VALUE(result) TYPE REF TO zif_mcp_ajson
  RAISING
    zcx_mcp_ajson_error.
```
