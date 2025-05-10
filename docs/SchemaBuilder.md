# JSON Schema Builder for MCP Server SDK

The JSON Schema Builder (`ZCL_MCP_SCHEMA_BUILDER`) is a powerful utility class that simplifies the creation of JSON Schema definitions for your Model Context Protocol tools. This page provides detailed information on how to use the schema builder to define input validation rules for your tools.

## Table of Contents

- [JSON Schema Builder for MCP Server SDK](#json-schema-builder-for-mcp-server-sdk)
  - [Table of Contents](#table-of-contents)
  - [Overview](#overview)
  - [Basic Usage](#basic-usage)
  - [Basic Property Types](#basic-property-types)
    - [String Properties](#string-properties)
    - [Number Properties](#number-properties)
    - [Integer Properties](#integer-properties)
    - [Boolean Properties](#boolean-properties)
  - [Setting Properties as Required](#setting-properties-as-required)
  - [String Validation with Enumerations](#string-validation-with-enumerations)
  - [Creating Objects](#creating-objects)
  - [Creating Arrays](#creating-arrays)
  - [Complex Schema Examples](#complex-schema-examples)
  - [API Reference](#api-reference)
    - [Constructor](#constructor)
    - [Property Methods](#property-methods)
    - [Structure Methods](#structure-methods)
    - [Output Method](#output-method)

## Overview

JSON Schema is a standard for validating the structure of JSON data. In the MCP Server SDK, JSON schemas are used to define the expected input format for tools that can be called by AI models. The Schema Builder provides a fluent interface that makes creating complex schemas simple and readable.

Key features:

- Easy definition of property types (string, number, integer, boolean)
- Support for nested objects and arrays
- Marking properties as required
- Defining allowed values through enumerations
- Fluent interface for chainable method calls

## Basic Usage

To create a JSON schema with the Schema Builder:

```abap
TRY.
    " Create a new schema builder
    DATA(schema_builder) = NEW zcl_mcp_schema_builder( ).
    
    " Add properties with method chaining
    schema_builder->add_string( name = 'name' required = abap_true )
                  ->add_integer( name = 'age' )
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

The Schema Builder supports all standard JSON Schema property types:

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

## Setting Properties as Required

You can mark properties as required by setting the `required` parameter to `abap_true`:

```abap
schema_builder->add_string( name = 'username' required = abap_true )
              ->add_string( name = 'password' required = abap_true )
              ->add_string( name = 'email' required = abap_false ).
```

Required properties will be included in the schema's `required` array, and MCP clients will enforce these requirements before calling your tool.

## String Validation with Enumerations

You can restrict string values to a specific set of allowed values using enumerations:

```abap
DATA(enum_values) = VALUE string_table( 
    ( `red` ) 
    ( `green` ) 
    ( `blue` ) 
).

schema_builder->add_string(
    name = 'color'
    description = 'The color selection'
    enum = enum_values
).
```

This will ensure the input only accepts the values "red", "green", or "blue".

## Creating Objects

You can create nested object structures using the `begin_object` and `end_object` methods:

```abap
schema_builder->add_string( name = 'name' )
              ->begin_object( name = 'address' )
                  ->add_string( name = 'street' )
                  ->add_string( name = 'city' )
                  ->add_string( name = 'country' )
              ->end_object( ).
```

This creates a schema with an object property named "address" that contains string properties for "street", "city", and "country".

You can nest objects to any depth:

```abap
schema_builder->begin_object( name = 'company' )
                  ->add_string( name = 'name' )
                  ->begin_object( name = 'address' )
                      ->add_string( name = 'street' required = abap_true )
                      ->add_string( name = 'city' required = abap_true )
                  ->end_object( )
              ->end_object( ).
```

## Creating Arrays

Arrays of objects can be created using the `begin_array` and `end_array` methods:

```abap
schema_builder->begin_array( name = 'tags' )
                  ->add_string( name = 'value' required = abap_true )
                  ->add_string( name = 'color' )
              ->end_array( ).
```

This creates an array property named "tags" where each array item is an object with "value" and "color" properties.

## Complex Schema Examples

You can combine all these features to create complex schema structures:

```abap
schema_builder->add_string( name = 'title' required = abap_true )
              ->add_string( name = 'description' )
              ->begin_object( name = 'author' required = abap_true )
                  ->add_string( name = 'name' required = abap_true )
                  ->add_string( name = 'email' )
              ->end_object( )
              ->begin_array( name = 'chapters' )
                  ->add_string( name = 'title' required = abap_true )
                  ->add_integer( name = 'pages' )
                  ->begin_array( name = 'sections' )
                      ->add_string( name = 'heading' required = abap_true )
                      ->add_integer( name = 'pageStart' )
                  ->end_array( )
              ->end_array( ).
```

This creates a schema for a book structure with nested objects and arrays.

## API Reference

### Constructor

```abap
METHODS constructor RAISING zcx_mcp_ajson_error.
```

Creates a new schema builder instance.

### Property Methods

```abap
METHODS add_string
  IMPORTING
    name        TYPE string
    description TYPE string OPTIONAL
    enum        TYPE string_table OPTIONAL
    required    TYPE abap_bool DEFAULT abap_false
  RETURNING
    VALUE(self) TYPE REF TO zcl_mcp_schema_builder.

METHODS add_number
  IMPORTING
    name        TYPE string
    description TYPE string OPTIONAL
    required    TYPE abap_bool DEFAULT abap_false
  RETURNING
    VALUE(self) TYPE REF TO zcl_mcp_schema_builder.

METHODS add_integer
  IMPORTING
    name        TYPE string
    description TYPE string OPTIONAL
    required    TYPE abap_bool DEFAULT abap_false
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

Converts the built schema to a JSON object that can be used in tool definitions.
