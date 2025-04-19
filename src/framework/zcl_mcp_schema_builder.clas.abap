CLASS zcl_mcp_schema_builder DEFINITION
  PUBLIC
  CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS constructor rAISING zcx_mcp_ajson_error.

    METHODS add_string
      IMPORTING
        name        TYPE string
        description TYPE string OPTIONAL
        enum        TYPE string_table OPTIONAL
        required    TYPE abap_bool DEFAULT abap_false
      RETURNING
        VALUE(self) TYPE REF TO zcl_mcp_schema_builder
        RAISING zcx_mcp_ajson_error.

    METHODS add_number
      IMPORTING
        name        TYPE string
        description TYPE string OPTIONAL
        required    TYPE abap_bool DEFAULT abap_false
      RETURNING
        VALUE(self) TYPE REF TO zcl_mcp_schema_builder
        rAISING zcx_mcp_ajson_error.

    METHODS add_integer
      IMPORTING
        name        TYPE string
        description TYPE string OPTIONAL
        required    TYPE abap_bool DEFAULT abap_false
      RETURNING
        VALUE(self) TYPE REF TO zcl_mcp_schema_builder
        rAISING zcx_mcp_ajson_error.

    METHODS add_boolean
      IMPORTING
        name        TYPE string
        description TYPE string OPTIONAL
        required    TYPE abap_bool DEFAULT abap_false
      RETURNING
        VALUE(self) TYPE REF TO zcl_mcp_schema_builder
        rAISING zcx_mcp_ajson_error.

    METHODS begin_object
      IMPORTING
        name        TYPE string
        description TYPE string OPTIONAL
        required    TYPE abap_bool DEFAULT abap_false
      RETURNING
        VALUE(self) TYPE REF TO zcl_mcp_schema_builder
        rAISING zcx_mcp_ajson_error.

    METHODS end_object
      RETURNING
        VALUE(self) TYPE REF TO zcl_mcp_schema_builder
        rAISING zcx_mcp_ajson_error.

    METHODS begin_array
      IMPORTING
        name        TYPE string
        description TYPE string OPTIONAL
        required    TYPE abap_bool DEFAULT abap_false
      RETURNING
        VALUE(self) TYPE REF TO zcl_mcp_schema_builder
        rAISING zcx_mcp_ajson_error.

    METHODS end_array
      RETURNING
        VALUE(self) TYPE REF TO zcl_mcp_schema_builder
        rAISING zcx_mcp_ajson_error.

    METHODS to_json
      RETURNING
        VALUE(result) TYPE REF TO zif_mcp_ajson
      RAISING
        zcx_mcp_ajson_error.

  PRIVATE SECTION.
    DATA schema TYPE REF TO zif_mcp_ajson.
    DATA current_path TYPE string.
    DATA parent_builder TYPE REF TO zcl_mcp_schema_builder.
    DATA node_type TYPE string.
    DATA required_properties TYPE STANDARD TABLE OF string WITH DEFAULT KEY.

    METHODS add_property
      IMPORTING
        name        TYPE string
        type        TYPE string
        description TYPE string OPTIONAL
        required    TYPE abap_bool DEFAULT abap_false
        rAISING zcx_mcp_ajson_error.
ENDCLASS.


CLASS zcl_mcp_schema_builder IMPLEMENTATION.
  METHOD constructor.
    schema = zcl_mcp_ajson=>create_empty( ).
    node_type = 'object'.
    schema->set( iv_path = '/type'
                 iv_val  = 'object' ).
  ENDMETHOD.

  METHOD add_property.
    " Add property to schema
    DATA path TYPE string.

    IF current_path IS INITIAL.
      path = |/properties/{ name }|.
    ELSE.
      path = |{ current_path }/properties/{ name }|.
    ENDIF.

    " Set type
    schema->set( iv_path = |{ path }/type|
                 iv_val  = type ).

    " Add description if provided
    IF description IS NOT INITIAL.
      schema->set( iv_path = |{ path }/description|
                   iv_val  = description ).
    ENDIF.

    " Mark as required if needed
    IF required = abap_true.
      APPEND name TO required_properties.
    ENDIF.
  ENDMETHOD.

  METHOD add_string.
    DATA path TYPE string.
      FIELD-SYMBOLS <enum_value> LIKE LINE OF enum.
    add_property( name        = name
                  type        = 'string'
                  description = description
                  required    = required ).

    
    IF current_path IS INITIAL.
      path = |/properties/{ name }|.
    ELSE.
      path = |{ current_path }/properties/{ name }|.
    ENDIF.

    " Add enum if provided
    IF enum IS NOT INITIAL.
      schema->touch_array( |{ path }/enum| ).
      
      LOOP AT enum ASSIGNING <enum_value>.
        schema->set( iv_path = |{ path }/enum/{ sy-tabix }|
                     iv_val  = <enum_value> ).
      ENDLOOP.
    ENDIF.

    self = me.
  ENDMETHOD.

  METHOD add_number.
    DATA path TYPE string.
    add_property( name        = name
                  type        = 'number'
                  description = description
                  required    = required ).

    
    IF current_path IS INITIAL.
      path = |/properties/{ name }|.
    ELSE.
      path = |{ current_path }/properties/{ name }|.
    ENDIF.

    self = me.
  ENDMETHOD.

  METHOD add_integer.
    DATA path TYPE string.
    add_property( name        = name
                  type        = 'integer'
                  description = description
                  required    = required ).

    
    IF current_path IS INITIAL.
      path = |/properties/{ name }|.
    ELSE.
      path = |{ current_path }/properties/{ name }|.
    ENDIF.

    self = me.
  ENDMETHOD.

  METHOD add_boolean.
    add_property( name        = name
                  type        = 'boolean'
                  description = description
                  required    = required ).

    self = me.
  ENDMETHOD.

  METHOD begin_object.
    DATA path TYPE string.
    DATA new_builder TYPE REF TO zcl_mcp_schema_builder.
    " Create a new object property
    add_property( name        = name
                  type        = 'object'
                  description = description
                  required    = required ).

    " Create path for this object
    
    IF current_path IS INITIAL.
      path = |/properties/{ name }|.
    ELSE.
      path = |{ current_path }/properties/{ name }|.
    ENDIF.

    " Create new builder for object properties
    
    CREATE OBJECT new_builder TYPE zcl_mcp_schema_builder.
    new_builder->schema         = schema.
    new_builder->parent_builder = me.
    new_builder->current_path   = path.
    new_builder->node_type      = 'object'.

    self = new_builder.
  ENDMETHOD.

  METHOD end_object.
      FIELD-SYMBOLS <required> LIKE LINE OF required_properties.
    " Add required properties if any
    IF required_properties IS NOT INITIAL AND node_type = 'object'.
      schema->touch_array( |{ current_path }/required| ).

      
      LOOP AT required_properties ASSIGNING <required>.
        schema->set( iv_path = |{ current_path }/required/{ sy-tabix }| iv_val = <required> ).
      ENDLOOP.
    ENDIF.

    " Return to parent
    self = parent_builder.
  ENDMETHOD.

  METHOD begin_array.
    DATA path TYPE string.
    DATA new_builder TYPE REF TO zcl_mcp_schema_builder.
    " Create a new array property
    add_property( name        = name
                  type        = 'array'
                  description = description
                  required    = required ).

    " Create path for this array
    
    IF current_path IS INITIAL.
      path = |/properties/{ name }|.
    ELSE.
      path = |{ current_path }/properties/{ name }|.
    ENDIF.

    " Set items type to object
    schema->set( iv_path = |{ path }/items/type|
                 iv_val  = 'object' ).

    " Create new builder for array items
    
    CREATE OBJECT new_builder TYPE zcl_mcp_schema_builder.
    new_builder->schema         = schema.
    new_builder->parent_builder = me.
    new_builder->current_path   = |{ path }/items|.
    new_builder->node_type      = 'array'.

    self = new_builder.
  ENDMETHOD.

  METHOD end_array.
      FIELD-SYMBOLS <required> LIKE LINE OF required_properties.
    " Add required properties if any
    IF required_properties IS NOT INITIAL AND node_type = 'array'.
      schema->touch_array( |{ current_path }/required| ).

      
      LOOP AT required_properties ASSIGNING <required>.
        schema->set( iv_path = |{ current_path }/required/{ sy-tabix }|
                     iv_val  = <required> ).
      ENDLOOP.
    ENDIF.

    " Return to parent
    self = parent_builder.
  ENDMETHOD.

  METHOD to_json.
      FIELD-SYMBOLS <required> LIKE LINE OF required_properties.
    " Add required properties to the root object if any
    IF required_properties IS NOT INITIAL AND current_path IS INITIAL.
      schema->touch_array( '/required' ).

      
      LOOP AT required_properties ASSIGNING <required>.
        schema->set( iv_path = |/required/{ sy-tabix }|
                     iv_val  = <required> ).
      ENDLOOP.
    ENDIF.

    result = schema.
  ENDMETHOD.
ENDCLASS.


