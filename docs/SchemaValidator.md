# JSON Schema Validator for MCP Server SDK

The JSON Schema Validator (`ZCL_MCP_SCHEMA_VALIDATOR`) works alongside the Schema Builder to validate that JSON data conforms to your defined schema. This class allows you to verify input before processing it in your Model Context Protocol tools.

## Table of Contents

- [JSON Schema Validator for MCP Server SDK](#json-schema-validator-for-mcp-server-sdk)
  - [Table of Contents](#table-of-contents)
  - [Overview](#overview)
  - [Basic Usage](#basic-usage)
  - [Validation Features](#validation-features)
    - [Type Validation](#type-validation)
    - [Required Properties](#required-properties)
    - [String Validation](#string-validation)
    - [Numeric Validation](#numeric-validation)
    - [Array Validation](#array-validation)
  - [Error Handling](#error-handling)
  - [Working with Complex Schemas](#working-with-complex-schemas)
  - [API Reference](#api-reference)
    - [Constructor](#constructor)
    - [Validation Method](#validation-method)
    - [Error Retrieval Method](#error-retrieval-method)

## Overview

The Schema Validator ensures that JSON data conforms to a schema created with the Schema Builder. It validates:

- Presence of required properties
- Correct data types
- Value constraints (ranges, lengths, enums)
- Nested object structures
- Array contents and size limits

## Basic Usage

```abap
TRY.
    " 1. Create a schema with the builder
    DATA(schema_builder) = NEW zcl_mcp_schema_builder( ).
    schema_builder->add_string( name = 'name' required = abap_true )
                  ->add_integer( name = 'age' minimum = 18 ).
    
    DATA(schema) = schema_builder->to_json( ).
    
    " 2. Create the validator with the schema
    DATA(validator) = NEW zcl_mcp_schema_validator( schema = schema ).
    
    " 3. Parse input JSON data
    DATA(json) = zcl_mcp_ajson=>parse( '{"name":"John","age":25}' ).
    
    " 4. Validate the JSON against the schema
    DATA(is_valid) = validator->validate( json = json ).
    
    " 5. Handle validation result
    IF is_valid = abap_true.
        " Process valid data
    ELSE.
        " Check validation errors
        DATA(errors) = validator->get_errors( ).
        " Handle errors...
    ENDIF.
    
CATCH zcx_mcp_ajson_error INTO DATA(error).
    " Handle JSON parsing errors
ENDTRY.
```

## Validation Features

### Type Validation

The validator ensures that property values match their expected types:

- `string` properties must contain string values
- `number` properties must contain numeric values
- `integer` properties must contain integer values
- `boolean` properties must contain boolean values
- `object` properties must contain nested objects
- `array` properties must contain arrays

### Required Properties

Properties marked as required in your schema must be present in the input JSON:

```abap
" Schema definition
schema_builder->add_string( name = 'username' required = abap_true )
              ->add_string( name = 'email' required = abap_true )
              ->add_string( name = 'comment' required = abap_false ).

" JSON '{"username": "johndoe"}' would fail validation
" because the required "email" property is missing
```

### String Validation

For string properties, the validator checks:

- **Length constraints**: Minimum and maximum string lengths
- **Enumeration values**: String must be one of the allowed values

```abap
" Validation will check if values meet these constraints
schema_builder->add_string( 
    name = 'password' 
    min_length = 8
    max_length = 30
).

" Only these values would be accepted
DATA(status_values) = VALUE string_table( ( 'pending' ) ( 'active' ) ( 'cancelled' ) ).
schema_builder->add_string(
    name = 'status'
    enum = status_values
).
```

### Numeric Validation

For number and integer properties, the validator checks:

- **Minimum values**: Value must be greater than or equal to the minimum
- **Maximum values**: Value must be less than or equal to the maximum

```abap
schema_builder->add_number(
    name = 'price'
    minimum = 0.01
    maximum = 9999.99
).

schema_builder->add_integer(
    name = 'quantity'
    minimum = 1
    maximum = 100
).
```

### Array Validation

For array properties, the validator checks:

- **Item count**: Min and max items constraints
- **Item validation**: Each array item is validated against its schema

```abap
schema_builder->begin_array(
    name = 'tags'
    min_items = 1
    max_items = 5
)->add_string( name = 'value' )->end_array( ).
```

## Error Handling

When validation fails, you can retrieve detailed error messages:

```abap
IF validator->validate( json ) = abap_false.
    DATA(errors) = validator->get_errors( ).
    
    " Process errors
    LOOP AT errors ASSIGNING FIELD-SYMBOL(<error>).
        " Each error shows the path and the reason for failure
        WRITE: / <error>.
    ENDLOOP.
ENDIF.
```

Error messages include:

- JSON path where the error occurred
- Description of the validation failure
- Expected and actual values (when applicable)

## Working with Complex Schemas

For complex nested structures, the validator traverses the entire object graph:

```abap
" Schema for a more complex structure
schema_builder->add_string( name = 'title' required = abap_true )
              ->begin_object( name = 'author' required = abap_true )
                  ->add_string( name = 'name' required = abap_true )
                  ->add_string( name = 'email' )
              ->end_object( )
              ->begin_array( name = 'chapters' min_items = 1 )
                  ->add_string( name = 'title' required = abap_true )
                  ->add_integer( name = 'pages' minimum = 1 )
              ->end_array( ).

" The validator will check the entire structure recursively
```

## API Reference

### Constructor

```abap
METHODS constructor
  IMPORTING !schema TYPE REF TO zif_mcp_ajson
  RAISING   zcx_mcp_ajson_error.
```

Creates a new validator instance with the specified JSON schema.

### Validation Method

```abap
METHODS validate
  IMPORTING !json          TYPE REF TO zif_mcp_ajson
  RETURNING VALUE(result)  TYPE abap_bool
  RAISING   zcx_mcp_ajson_error.
```

Validates JSON data against the schema. Returns `abap_true` if valid, `abap_false` if invalid.

### Error Retrieval Method

```abap
METHODS get_errors
  RETURNING VALUE(errors) TYPE string_table.
```

Returns a table of error messages if validation failed. Each entry contains the path and error description.
