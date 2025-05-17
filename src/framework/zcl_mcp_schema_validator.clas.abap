CLASS zcl_mcp_schema_validator DEFINITION
  PUBLIC FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    "! <p class="shorttext synchronized">Create a new JSON schema validator</p>
    "!
    "! @parameter schema              | <p class="shorttext synchronized">JSON schema to validate against</p>
    "! @raising   zcx_mcp_ajson_error | <p class="shorttext synchronized">JSON error</p>
    METHODS constructor
      IMPORTING !schema TYPE REF TO zif_mcp_ajson
      RAISING   zcx_mcp_ajson_error.

    "! <p class="shorttext synchronized">Validate JSON data against schema</p>
    "!
    "!
    "! @parameter result              | <p class="shorttext synchronized">Validation result</p>
    "! @raising   zcx_mcp_ajson_error | <p class="shorttext synchronized">JSON error</p>
    METHODS validate
      IMPORTING !json         TYPE REF TO zif_mcp_ajson
      RETURNING VALUE(result) TYPE abap_bool
      RAISING   zcx_mcp_ajson_error.

    "! <p class="shorttext synchronized">Get validation errors</p>
    "!
    "! @parameter result | <p class="shorttext synchronized">Validation errors</p>
    METHODS get_errors
      RETURNING VALUE(result) TYPE string_table.

  PRIVATE SECTION.
    TYPES:
      "! Validation error structure
      BEGIN OF error,
        path    TYPE string,
        message TYPE string,
      END OF error,
      errors TYPE STANDARD TABLE OF error WITH EMPTY KEY.

    DATA schema          TYPE REF TO zif_mcp_ajson.
    DATA detailed_errors TYPE errors.
    DATA error_list      TYPE string_table.

    "! <p class="shorttext synchronized">Validate object properties</p>
    "!
    "! @parameter json_path           | <p class="shorttext synchronized">Current JSON path</p>
    "! @parameter schema_path         | <p class="shorttext synchronized">Current schema path</p>
    "! @parameter json                | <p class="shorttext synchronized">JSON to validate</p>
    "! @parameter result              | <p class="shorttext synchronized">Validation result</p>
    "! @raising   zcx_mcp_ajson_error | <p class="shorttext synchronized">JSON error</p>
    METHODS validate_object
      IMPORTING json_path     TYPE string
                schema_path   TYPE string
                json          TYPE REF TO zif_mcp_ajson
      RETURNING VALUE(result) TYPE abap_bool
      RAISING   zcx_mcp_ajson_error.

    "! <p class="shorttext synchronized">Validate array items</p>
    "!
    "! @parameter json_path           | <p class="shorttext synchronized">Current JSON path</p>
    "! @parameter schema_path         | <p class="shorttext synchronized">Current schema path</p>
    "! @parameter json                | <p class="shorttext synchronized">JSON to validate</p>
    "! @parameter result              | <p class="shorttext synchronized">Validation result</p>
    "! @raising   zcx_mcp_ajson_error | <p class="shorttext synchronized">JSON error</p>
    METHODS validate_array
      IMPORTING json_path     TYPE string
                schema_path   TYPE string
                json          TYPE REF TO zif_mcp_ajson
      RETURNING VALUE(result) TYPE abap_bool
      RAISING   zcx_mcp_ajson_error.

    "! <p class="shorttext synchronized">Validate property against schema</p>
    "!
    "! @parameter json_path           | <p class="shorttext synchronized">JSON path to property</p>
    "! @parameter schema_path         | <p class="shorttext synchronized">Schema path for property</p>
    "! @parameter json                | <p class="shorttext synchronized">JSON to validate</p>
    "! @parameter result              | <p class="shorttext synchronized">Validation result</p>
    "! @raising   zcx_mcp_ajson_error | <p class="shorttext synchronized">JSON error</p>
    METHODS validate_property
      IMPORTING json_path     TYPE string
                schema_path   TYPE string
                json          TYPE REF TO zif_mcp_ajson
      RETURNING VALUE(result) TYPE abap_bool
      RAISING   zcx_mcp_ajson_error.

    "! <p class="shorttext synchronized">Validate string property</p>
    "!
    "! @parameter json_path           | <p class="shorttext synchronized">JSON path to property</p>
    "! @parameter schema_path         | <p class="shorttext synchronized">Schema path for property</p>
    "! @parameter json                | <p class="shorttext synchronized">JSON to validate</p>
    "! @parameter result              | <p class="shorttext synchronized">Validation result</p>
    "! @raising   zcx_mcp_ajson_error | <p class="shorttext synchronized">JSON error</p>
    METHODS validate_string
      IMPORTING json_path     TYPE string
                schema_path   TYPE string
                json          TYPE REF TO zif_mcp_ajson
      RETURNING VALUE(result) TYPE abap_bool
      RAISING   zcx_mcp_ajson_error.

    "! <p class="shorttext synchronized">Validate number property</p>
    "!
    "! @parameter json_path           | <p class="shorttext synchronized">JSON path to property</p>
    "! @parameter schema_path         | <p class="shorttext synchronized">Schema path for property</p>
    "! @parameter json                | <p class="shorttext synchronized">JSON to validate</p>
    "! @parameter result              | <p class="shorttext synchronized">Validation result</p>
    "! @raising   zcx_mcp_ajson_error | <p class="shorttext synchronized">JSON error</p>
    METHODS validate_number
      IMPORTING json_path     TYPE string
                schema_path   TYPE string
                json          TYPE REF TO zif_mcp_ajson
      RETURNING VALUE(result) TYPE abap_bool
      RAISING   zcx_mcp_ajson_error.

    "! <p class="shorttext synchronized">Validate integer property</p>
    "!
    "! @parameter json_path           | <p class="shorttext synchronized">JSON path to property</p>
    "! @parameter schema_path         | <p class="shorttext synchronized">Schema path for property</p>
    "! @parameter json                | <p class="shorttext synchronized">JSON to validate</p>
    "! @parameter result              | <p class="shorttext synchronized">Validation result</p>
    "! @raising   zcx_mcp_ajson_error | <p class="shorttext synchronized">JSON error</p>
    METHODS validate_integer
      IMPORTING json_path     TYPE string
                schema_path   TYPE string
                json          TYPE REF TO zif_mcp_ajson
      RETURNING VALUE(result) TYPE abap_bool
      RAISING   zcx_mcp_ajson_error.

    "! <p class="shorttext synchronized">Check if property exists</p>
    "!
    "! @parameter json_path           | <p class="shorttext synchronized">JSON path to property</p>
    "! @parameter json                | <p class="shorttext synchronized">JSON to check</p>
    "! @parameter result              | <p class="shorttext synchronized">Whether property exists</p>
    "! @raising   zcx_mcp_ajson_error | <p class="shorttext synchronized">JSON error</p>
    METHODS property_exists
      IMPORTING json_path     TYPE string
                json          TYPE REF TO zif_mcp_ajson
      RETURNING VALUE(result) TYPE abap_bool
      RAISING   zcx_mcp_ajson_error.

    "! <p class="shorttext synchronized">Add validation error</p>
    "!
    "! @parameter path    | <p class="shorttext synchronized">Path where error occurred</p>
    "! @parameter message | <p class="shorttext synchronized">Error message</p>
    METHODS add_error
      IMPORTING !path    TYPE string
                !message TYPE string.

ENDCLASS.

CLASS zcl_mcp_schema_validator IMPLEMENTATION.
  METHOD constructor.
    me->schema = schema.
  ENDMETHOD.

  METHOD validate.
    " Clear previous errors
    CLEAR detailed_errors.
    CLEAR error_list.

    " The schema root should be an object type
    TRY.
        DATA(root_type) = schema->get_string( '/type' ).
        IF root_type <> 'object'.
          " Schema root must be of type "object", but found &1
          MESSAGE e100(zmcp) WITH root_type INTO DATA(msg).
          add_error( path    = '/'
                     message = msg ).
          result = abap_false.
          RETURN.
        ENDIF.
      CATCH zcx_mcp_ajson_error.
        " Schema error: missing root type
        MESSAGE e101(zmcp) INTO msg.
        add_error( path    = '/'
                   message = msg ).
        result = abap_false.
        RETURN.
    ENDTRY.

    " Validate the JSON against the schema
    result = validate_object( json_path   = '/'
                              schema_path = '/'
                              json        = json ).

    " Convert detailed errors to simple string table
    LOOP AT detailed_errors ASSIGNING FIELD-SYMBOL(<error>).
      APPEND |{ <error>-path }: { <error>-message }| TO error_list.
    ENDLOOP.
  ENDMETHOD.

  METHOD get_errors.
    result = me->error_list.
  ENDMETHOD.

  METHOD validate_object.
    DATA is_valid       TYPE abap_bool VALUE abap_true.

    " Check required properties
    DATA required_props TYPE string_table.

    TRY.
        " First check if 'required' property exists in schema
        IF schema->exists( |{ schema_path }required| ).
          " Get required properties from schema
          DATA required_json TYPE REF TO zif_mcp_ajson.
          required_json = schema->slice( |{ schema_path }required| ).

          " Convert to string table
          required_json->to_abap( IMPORTING ev_container = required_props ).

          " Check each required property
          LOOP AT required_props ASSIGNING FIELD-SYMBOL(<prop>).
            IF NOT property_exists( json_path = |{ json_path }{ <prop> }|
                                    json      = json ).
              " Required property "&1" is missing
              MESSAGE e102(zmcp) WITH <prop> INTO DATA(msg).
              add_error( path    = json_path
                         message = msg ).
              is_valid = abap_false.
            ENDIF.
          ENDLOOP.
        ENDIF.
      CATCH zcx_mcp_ajson_error.
        " Error accessing required properties, log and continue
        MESSAGE e103(zmcp) INTO msg.
        add_error( path    = schema_path
                   message = msg ).
    ENDTRY.

    " Get schema properties
    DATA props_json TYPE REF TO zif_mcp_ajson.
    props_json = schema->slice( |{ schema_path }properties| ).

    " Get property names from schema
    DATA props_names TYPE string_table.
    props_names = props_json->members( '' ).

    " Validate each property
    LOOP AT props_names ASSIGNING FIELD-SYMBOL(<prop_name>).
      DATA prop_schema_path TYPE string.
      prop_schema_path = |{ schema_path }properties/{ <prop_name> }|.

      DATA prop_json_path TYPE string.
      prop_json_path = |{ json_path }{ <prop_name> }|.

      " Skip validation if property doesn't exist (required check is done separately)
      IF property_exists( json_path = prop_json_path
                          json      = json ).
        DATA(prop_valid) = validate_property( json_path   = prop_json_path
                                              schema_path = prop_schema_path
                                              json        = json ).
        IF prop_valid = abap_false.
          is_valid = abap_false.
        ENDIF.
      ENDIF.
    ENDLOOP.
    result = is_valid.
  ENDMETHOD.

  METHOD validate_array.
    DATA is_valid TYPE abap_bool VALUE abap_true.

    " Make sure schema path has correct format
    DATA(schema_items_path) = |{ schema_path }items|.

    " Get array items type from schema
    DATA items_type TYPE string.
    TRY.
        items_type = schema->get_string( |{ schema_items_path }/type| ).
      CATCH zcx_mcp_ajson_error.
        MESSAGE e104(zmcp) INTO DATA(msg).
        add_error( path    = json_path
                   message = msg ).
        result = abap_false.
        RETURN.
    ENDTRY.

    " Get array items - use array_to_string_table for index detection
    DATA item_indices TYPE string_table.
    TRY.
        " Remove trailing slash if present for members call
        DATA(array_path) = json_path.
        DATA(path_length) = strlen( array_path ).

        " Check if the last character is a slash and remove it if needed
        IF path_length > 0.
          DATA(offset) = path_length - 1.
          DATA(last_char) = array_path+offset(1).
          IF last_char = '/'.
            array_path = substring( val = array_path
                                    off = 0
                                    len = offset ).
          ENDIF.
        ENDIF.

        " Get array members
        item_indices = json->members( array_path ).

        " Filter item_indices to keep only numeric entries
        DATA(item_count) = 0.
        LOOP AT item_indices ASSIGNING FIELD-SYMBOL(<idx>).
          " If the entry is a numeric array index, count it
          IF <idx> CO '0123456789'.
            item_count = item_count + 1.
          ENDIF.
        ENDLOOP.

      CATCH zcx_mcp_ajson_error.
        " Likely empty array
        item_count = 0.
    ENDTRY.

    " Check minItems - only if it exists in schema
    IF schema->exists( |{ schema_path }minItems| ).
      DATA(min_items) = schema->get_integer( |{ schema_path }minItems| ).
      IF item_count < min_items.
        " Array must have at least &1 items, but has &2
        MESSAGE e105(zmcp) WITH min_items item_count INTO msg.
        add_error( path    = array_path
                   message = msg ).
        is_valid = abap_false.
      ENDIF.
    ENDIF.

    " Check maxItems - only if it exists in schema
    IF schema->exists( |{ schema_path }maxItems| ).
      DATA(max_items) = schema->get_integer( |{ schema_path }maxItems| ).
      IF item_count > max_items.
        " Array must have at most &1 items, but has &2
        MESSAGE e106(zmcp) WITH max_items item_count INTO msg.
        add_error( path    = array_path
                   message = msg ).
        is_valid = abap_false.
      ENDIF.
    ENDIF.

    " Validate each array item
    LOOP AT item_indices ASSIGNING <idx>.
      " Skip non-numeric entries
      IF <idx> CN '0123456789'.
        CONTINUE.
      ENDIF.

      " Form correct path to array item
      DATA(item_path) = |{ array_path }/{ <idx> }|.

      " Check item type and validate accordingly
      CASE items_type.
        WHEN 'object'.
          DATA(item_valid) = validate_object( json_path   = |{ item_path }/|
                                              schema_path = |{ schema_items_path }/|
                                              json        = json ).
          IF item_valid = abap_false.
            is_valid = abap_false.
          ENDIF.
        WHEN 'string'.
          item_valid = validate_string( json_path   = item_path
                                        schema_path = schema_items_path
                                        json        = json ).
          IF item_valid = abap_false.
            is_valid = abap_false.
          ENDIF.
        WHEN 'number'.
          item_valid = validate_number( json_path   = item_path
                                        schema_path = schema_items_path
                                        json        = json ).
          IF item_valid = abap_false.
            is_valid = abap_false.
          ENDIF.
        WHEN 'integer'.
          item_valid = validate_integer( json_path   = item_path
                                         schema_path = schema_items_path
                                         json        = json ).
          IF item_valid = abap_false.
            is_valid = abap_false.
          ENDIF.
        WHEN 'boolean'.
          " For boolean, just check type
          TRY.
              DATA(item_type) = json->get_node_type( item_path ).
              IF item_type <> 'bool'.
                " Array item must be of type "boolean", but is "&1"
                MESSAGE e107(zmcp) WITH item_type INTO msg.
                add_error( path    = item_path
                           message = msg ).
                is_valid = abap_false.
              ENDIF.
            CATCH zcx_mcp_ajson_error.
              " Error accessing array item
              MESSAGE e108(zmcp) INTO msg.
              add_error( path    = item_path
                         message = msg ).
              is_valid = abap_false.
          ENDTRY.
        WHEN 'array'.
          " Nested array validation
          item_valid = validate_array( json_path   = |{ item_path }/|
                                       schema_path = |{ schema_items_path }/|
                                       json        = json ).
          IF item_valid = abap_false.
            is_valid = abap_false.
          ENDIF.
        WHEN OTHERS.
          " Unsupported array item type "&1"
          MESSAGE e109(zmcp) WITH items_type INTO msg.
          add_error( path    = item_path
                     message = msg ).
          is_valid = abap_false.
      ENDCASE.
    ENDLOOP.

    result = is_valid.
  ENDMETHOD.

  METHOD validate_property.
    " Get the property type from the schema
    DATA prop_type TYPE string.

    TRY.
        prop_type = schema->get_string( |{ schema_path }/type| ).
      CATCH zcx_mcp_ajson_error.
        " Schema error: missing type for property
        MESSAGE e110(zmcp) INTO DATA(msg).
        add_error( path    = json_path
                   message = msg ).
        result = abap_false.
        RETURN.
    ENDTRY.

    " Get the actual property value type
    DATA value_type TYPE string.
    TRY.
        value_type = json->get_node_type( json_path ).
      CATCH zcx_mcp_ajson_error.
        " Error accessing property
        MESSAGE e111(zmcp) INTO msg.
        add_error( path    = json_path
                   message = msg ).
        result = abap_false.
        RETURN.
    ENDTRY.

    " Validate based on property type
    CASE prop_type.
      WHEN 'string'.
        IF value_type <> 'str'.
          " Property must be of type "string", but is "&1"
          MESSAGE e112(zmcp) WITH value_type INTO msg.
          add_error( path    = json_path
                     message = msg ).
          result = abap_false.
        ELSE.
          result = validate_string( json_path   = json_path
                                    schema_path = schema_path
                                    json        = json ).
        ENDIF.

      WHEN 'number'.
        IF value_type <> 'num'.
          " Property must be of type "number", but is "&1"
          MESSAGE e113(zmcp) WITH value_type INTO msg.
          add_error( path    = json_path
                     message = msg ).
          result = abap_false.
        ELSE.
          result = validate_number( json_path   = json_path
                                    schema_path = schema_path
                                    json        = json ).
        ENDIF.

      WHEN 'integer'.
        IF value_type <> 'num'.
          " Property must be of type "integer", but is "&1"
          MESSAGE e114(zmcp) WITH value_type INTO msg.
          add_error( path    = json_path
                     message = msg ).
          result = abap_false.
        ELSE.
          result = validate_integer( json_path   = json_path
                                     schema_path = schema_path
                                     json        = json ).
        ENDIF.

      WHEN 'boolean'.
        " For boolean, just check type
        IF value_type <> 'bool'.
          " Property must be of type "boolean", but is "&1"
          MESSAGE e115(zmcp) WITH value_type INTO msg.
          add_error( path    = json_path
                     message = msg ).
          result = abap_false.
        ELSE.
          result = abap_true.
        ENDIF.

      WHEN 'object'.
        " Check type and validate nested object
        IF value_type <> 'object'.
          " Property must be of type "object", but is "&1"
          MESSAGE e116(zmcp) WITH value_type INTO msg.
          add_error( path    = json_path
                     message = msg ).
          result = abap_false.
        ELSE.
          result = validate_object( json_path   = |{ json_path }/|
                                    schema_path = |{ schema_path }/|
                                    json        = json ).
        ENDIF.

      WHEN 'array'.
        " Check type and validate array
        IF value_type <> 'array'.
          " Property must be of type "array", but is "&1"
          MESSAGE e117(zmcp) WITH value_type INTO msg.
          add_error( path    = json_path
                     message = msg ).
          result = abap_false.
        ELSE.
          result = validate_array( json_path   = |{ json_path }/|
                                   schema_path = |{ schema_path }/|
                                   json        = json ).
        ENDIF.

      WHEN OTHERS.
        " Unsupported property type "&1"
        MESSAGE e118(zmcp) WITH value_type INTO msg.
        add_error( path    = json_path
                   message = msg ).
        result = abap_false.
    ENDCASE.
  ENDMETHOD.

  METHOD validate_string.
    TRY.
        DATA(node_type) = json->get_node_type( json_path ).
        IF node_type <> 'str'.
          " Expected type "string" but got "&1"
          MESSAGE e119(zmcp) WITH node_type INTO DATA(msg).
          add_error( path    = json_path
                     message = msg ).
          result = abap_false.
          RETURN.
        ENDIF.
      CATCH zcx_mcp_ajson_error.
        MESSAGE e111(zmcp) INTO msg.
        add_error( path    = json_path
                   message = msg ).
        result = abap_false.
        RETURN.
    ENDTRY.

    " Proceed with string validation
    DATA is_valid TYPE abap_bool VALUE abap_true.

    " Get string value
    DATA(string_value) = json->get_string( json_path ).

    " Check minLength - only if it exists in schema
    DATA strlength TYPE i.
    IF schema->exists( |{ schema_path }/minLength| ).
      DATA(min_length) = schema->get_integer( |{ schema_path }/minLength| ).
      strlength = strlen( string_value ).
      IF strlength < min_length.
        " String must have minimum length &1, but has &2
        MESSAGE e120(zmcp) WITH min_length strlength INTO msg.
        add_error( path    = json_path
                   message = msg ).
        is_valid = abap_false.
      ENDIF.
    ENDIF.

    " Check maxLength - only if it exists in schema
    IF schema->exists( |{ schema_path }/maxLength| ).
      DATA(max_length) = schema->get_integer( |{ schema_path }/maxLength| ).
      strlength = strlen( string_value ).
      IF strlength > max_length.
        " String must have maximum length &1, but has &2
        MESSAGE e121(zmcp) WITH min_length strlength INTO msg.
        add_error( path    = json_path
                   message = msg ).
        is_valid = abap_false.
      ENDIF.
    ENDIF.

    " Check enum - only if it exists in schema
    IF schema->exists( |{ schema_path }/enum| ).
      DATA enum_values TYPE string_table.
      enum_values = schema->array_to_string_table( |{ schema_path }/enum| ).

      " Check if value is in enum
      IF string_value IS NOT INITIAL AND NOT line_exists( enum_values[ table_line = string_value ] ).
        " Build readable enum list for error message
        DATA enum_list TYPE string.
        LOOP AT enum_values ASSIGNING FIELD-SYMBOL(<enum>).
          IF enum_list IS INITIAL.
            enum_list = |"{ <enum> }"|.
          ELSE.
            enum_list = |{ enum_list }, "{ <enum> }"|.
          ENDIF.
        ENDLOOP.

        " String must be one of &1, but is "&2"
        MESSAGE e122(zmcp) WITH enum_list string_value INTO msg.
        add_error( path    = json_path
                   message = msg ).
        is_valid = abap_false.
      ENDIF.
    ENDIF.

    result = is_valid.
  ENDMETHOD.

  METHOD validate_number.
    " First check if the node type is number
    TRY.
        DATA(node_type) = json->get_node_type( json_path ).
        IF node_type <> 'num'.
          " Expected type "number" but got "&1"
          MESSAGE e123(zmcp) WITH node_type INTO DATA(msg).
          add_error( path    = json_path
                     message = msg ).
          result = abap_false.
          RETURN.
        ENDIF.
      CATCH zcx_mcp_ajson_error.
        MESSAGE e111(zmcp) INTO msg.
        add_error( path    = json_path
                   message = msg ).
        result = abap_false.
        RETURN.
    ENDTRY.

    " Proceed with number validation
    DATA is_valid TYPE abap_bool VALUE abap_true.

    " Get number value
    DATA(number_value) = json->get_number( json_path ).

    " Check minimum - only if it exists in schema
    IF schema->exists( |{ schema_path }/minimum| ).
      DATA(minimum) = schema->get_number( |{ schema_path }/minimum| ).
      IF number_value < minimum.
        " Number must be at least &1, but is &2
        MESSAGE e124(zmcp) WITH minimum number_value INTO msg.
        add_error( path    = json_path
                   message = msg ).
        is_valid = abap_false.
      ENDIF.
    ENDIF.

    " Check maximum - only if it exists in schema
    IF schema->exists( |{ schema_path }/maximum| ).
      DATA(maximum) = schema->get_number( |{ schema_path }/maximum| ).
      IF number_value > maximum.
        " Number must be at most &1, but is &2
        MESSAGE e125(zmcp) WITH maximum number_value INTO msg.
        add_error( path    = json_path
                   message = msg ).
        is_valid = abap_false.
      ENDIF.
    ENDIF.

    result = is_valid.
  ENDMETHOD.

  METHOD validate_integer.
    " First check if the node type is number (JSON doesn't have a specific integer type)
    TRY.
        DATA(node_type) = json->get_node_type( json_path ).
        IF node_type <> 'num'.
          " Expected type "integer" but got "&1"
          MESSAGE e126(zmcp) WITH node_type INTO DATA(msg).
          add_error( path    = json_path
                     message = msg ).
          result = abap_false.
          RETURN.
        ENDIF.
      CATCH zcx_mcp_ajson_error.
        MESSAGE e111(zmcp) INTO msg.
        add_error( path    = json_path
                   message = msg ).
        result = abap_false.
        RETURN.
    ENDTRY.

    " Proceed with integer validation
    DATA is_valid TYPE abap_bool VALUE abap_true.

    " Get integer value
    DATA(integer_value) = json->get_integer( json_path ).

    " Check minimum - only if it exists in schema
    IF schema->exists( |{ schema_path }/minimum| ).
      DATA(minimum) = schema->get_integer( |{ schema_path }/minimum| ).
      IF integer_value < minimum.
        " Integer must be at least &1, but is &2
        MESSAGE e127(zmcp) WITH minimum integer_value INTO msg.
        add_error( path    = json_path
                   message = msg ).
        is_valid = abap_false.
      ENDIF.
    ENDIF.

    " Check maximum - only if it exists in schema
    IF schema->exists( |{ schema_path }/maximum| ).
      DATA(maximum) = schema->get_integer( |{ schema_path }/maximum| ).
      IF integer_value > maximum.
        " Integer must be at most &1, but is &2
        MESSAGE e128(zmcp) WITH maximum integer_value INTO msg.
        add_error( path    = json_path
                   message = msg ).
        is_valid = abap_false.
      ENDIF.
    ENDIF.

    result = is_valid.
  ENDMETHOD.

  METHOD property_exists.
    TRY.
        result = json->exists( json_path ).
      CATCH zcx_mcp_ajson_error.
        result = abap_false.
    ENDTRY.
  ENDMETHOD.

  METHOD add_error.
    " Add error to detailed errors table
    APPEND VALUE #( path = path
                   message = message ) TO detailed_errors.
  ENDMETHOD.
ENDCLASS.
