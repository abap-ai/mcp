CLASS zcl_mcp_schema_builder DEFINITION
  PUBLIC FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    "! <p class="shorttext synchronized">Create a new JSON schema builder</p>
    "!
    "! @raising zcx_mcp_ajson_error | <p class="shorttext synchronized">JSON error</p>
    METHODS constructor RAISING zcx_mcp_ajson_error.

    "! <p class="shorttext synchronized">Add string property to schema</p>
    "!
    "! @parameter name                | <p class="shorttext synchronized">Property name</p>
    "! @parameter description         | <p class="shorttext synchronized">Property description</p>
    "! @parameter enum                | <p class="shorttext synchronized">Enumeration values</p>
    "! @parameter required            | <p class="shorttext synchronized">Whether property is required</p>
    "! @parameter self                | <p class="shorttext synchronized">Builder instance for chaining</p>
    "! @raising   zcx_mcp_ajson_error | <p class="shorttext synchronized">JSON error</p>
    METHODS add_string
      IMPORTING !name        TYPE string
                !description TYPE string       OPTIONAL
                !enum        TYPE string_table OPTIONAL
                !required    TYPE abap_bool    DEFAULT abap_false
      RETURNING VALUE(self)  TYPE REF TO zcl_mcp_schema_builder
      RAISING   zcx_mcp_ajson_error.

    "! <p class="shorttext synchronized">Add number property to schema</p>
    "!
    "! @parameter name                | <p class="shorttext synchronized">Property name</p>
    "! @parameter description         | <p class="shorttext synchronized">Property description</p>
    "! @parameter required            | <p class="shorttext synchronized">Whether property is required</p>
    "! @parameter self                | <p class="shorttext synchronized">Builder instance for chaining</p>
    "! @raising   zcx_mcp_ajson_error | <p class="shorttext synchronized">JSON error</p>
    METHODS add_number
      IMPORTING !name        TYPE string
                !description TYPE string    OPTIONAL
                !required    TYPE abap_bool DEFAULT abap_false
      RETURNING VALUE(self)  TYPE REF TO zcl_mcp_schema_builder
      RAISING   zcx_mcp_ajson_error.

    "! <p class="shorttext synchronized">Add integer property to schema</p>
    "!
    "! @parameter name                | <p class="shorttext synchronized">Property name</p>
    "! @parameter description         | <p class="shorttext synchronized">Property description</p>
    "! @parameter required            | <p class="shorttext synchronized">Whether property is required</p>
    "! @parameter self                | <p class="shorttext synchronized">Builder instance for chaining</p>
    "! @raising   zcx_mcp_ajson_error | <p class="shorttext synchronized">JSON error</p>
    METHODS add_integer
      IMPORTING !name        TYPE string
                !description TYPE string    OPTIONAL
                !required    TYPE abap_bool DEFAULT abap_false
      RETURNING VALUE(self)  TYPE REF TO zcl_mcp_schema_builder
      RAISING   zcx_mcp_ajson_error.

    "! <p class="shorttext synchronized">Add boolean property to schema</p>
    "!
    "! @parameter name                | <p class="shorttext synchronized">Property name</p>
    "! @parameter description         | <p class="shorttext synchronized">Property description</p>
    "! @parameter required            | <p class="shorttext synchronized">Whether property is required</p>
    "! @parameter self                | <p class="shorttext synchronized">Builder instance for chaining</p>
    "! @raising   zcx_mcp_ajson_error | <p class="shorttext synchronized">JSON error</p>
    METHODS add_boolean
      IMPORTING !name        TYPE string
                !description TYPE string    OPTIONAL
                !required    TYPE abap_bool DEFAULT abap_false
      RETURNING VALUE(self)  TYPE REF TO zcl_mcp_schema_builder
      RAISING   zcx_mcp_ajson_error.

    "! <p class="shorttext synchronized">Begin object property</p>
    "!
    "! @parameter name                | <p class="shorttext synchronized">Object name</p>
    "! @parameter description         | <p class="shorttext synchronized">Object description</p>
    "! @parameter required            | <p class="shorttext synchronized">Whether object is required</p>
    "! @parameter self                | <p class="shorttext synchronized">Builder instance for the new object</p>
    "! @raising   zcx_mcp_ajson_error | <p class="shorttext synchronized">JSON error</p>
    METHODS begin_object
      IMPORTING !name        TYPE string
                !description TYPE string    OPTIONAL
                !required    TYPE abap_bool DEFAULT abap_false
      RETURNING VALUE(self)  TYPE REF TO zcl_mcp_schema_builder
      RAISING   zcx_mcp_ajson_error.

    "! <p class="shorttext synchronized">End current object</p>
    "!
    "! @parameter self                | <p class="shorttext synchronized">Builder instance of the parent object</p>
    "! @raising   zcx_mcp_ajson_error | <p class="shorttext synchronized">JSON error</p>
    METHODS end_object
      RETURNING VALUE(self) TYPE REF TO zcl_mcp_schema_builder
      RAISING   zcx_mcp_ajson_error.

    "! <p class="shorttext synchronized">Begin array property</p>
    "!
    "! @parameter name                | <p class="shorttext synchronized">Array name</p>
    "! @parameter description         | <p class="shorttext synchronized">Array description</p>
    "! @parameter required            | <p class="shorttext synchronized">Whether array is required</p>
    "! @parameter self                | <p class="shorttext synchronized">Builder instance for the array items</p>
    "! @raising   zcx_mcp_ajson_error | <p class="shorttext synchronized">JSON error</p>
    METHODS begin_array
      IMPORTING !name        TYPE string
                !description TYPE string    OPTIONAL
                !required    TYPE abap_bool DEFAULT abap_false
      RETURNING VALUE(self)  TYPE REF TO zcl_mcp_schema_builder
      RAISING   zcx_mcp_ajson_error.

    "! <p class="shorttext synchronized">End current array</p>
    "!
    "! @parameter self                | <p class="shorttext synchronized">Builder instance of the parent object</p>
    "! @raising   zcx_mcp_ajson_error | <p class="shorttext synchronized">JSON error</p>
    METHODS end_array
      RETURNING VALUE(self) TYPE REF TO zcl_mcp_schema_builder
      RAISING   zcx_mcp_ajson_error.

    "! <p class="shorttext synchronized">Convert schema to JSON</p>
    "!
    "! @parameter result              | <p class="shorttext synchronized">JSON representation of the schema</p>
    "! @raising   zcx_mcp_ajson_error | <p class="shorttext synchronized">JSON error</p>
    METHODS to_json
      RETURNING VALUE(result) TYPE REF TO zif_mcp_ajson
      RAISING   zcx_mcp_ajson_error.

  PRIVATE SECTION.
    DATA schema              TYPE REF TO zif_mcp_ajson.
    DATA current_path        TYPE string.
    DATA parent_builder      TYPE REF TO zcl_mcp_schema_builder.
    DATA node_type           TYPE string.
    DATA required_properties TYPE STANDARD TABLE OF string WITH DEFAULT KEY.
    DATA root_builder        TYPE REF TO zcl_mcp_schema_builder.
    DATA active_builder      TYPE REF TO zcl_mcp_schema_builder.

    "! <p class="shorttext synchronized">Get currently active builder</p>
    "!
    "! @parameter result | <p class="shorttext synchronized">The active builder</p>
    METHODS get_active_builder
      RETURNING VALUE(result) TYPE REF TO zcl_mcp_schema_builder.

    "! <p class="shorttext synchronized">Add property to schema</p>
    "!
    "! @parameter name                | <p class="shorttext synchronized">Property name</p>
    "! @parameter type                | <p class="shorttext synchronized">Property type</p>
    "! @parameter description         | <p class="shorttext synchronized">Property description</p>
    "! @parameter required            | <p class="shorttext synchronized">Whether property is required</p>
    "! @raising   zcx_mcp_ajson_error | <p class="shorttext synchronized">JSON error</p>
    METHODS add_property
      IMPORTING !name        TYPE string
                !type        TYPE string
                !description TYPE string    OPTIONAL
                !required    TYPE abap_bool DEFAULT abap_false
      RAISING   zcx_mcp_ajson_error.
ENDCLASS.

CLASS zcl_mcp_schema_builder IMPLEMENTATION.
  METHOD constructor.
    schema = zcl_mcp_ajson=>create_empty( ).
    node_type = 'object'.
    schema->set( iv_path = '/type'
                 iv_val  = 'object' ).

    " Initialize tracking
    root_builder = me.
    active_builder = me.
  ENDMETHOD.

  METHOD get_active_builder.
    " Return active builder if this is the root
    IF me = root_builder AND active_builder IS BOUND AND active_builder <> me.
      result = active_builder.
    ELSE.
      result = me.
    ENDIF.
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
    " Forward to active builder if needed
    DATA builder TYPE REF TO zcl_mcp_schema_builder.
    DATA path TYPE string.
      FIELD-SYMBOLS <enum_value> LIKE LINE OF enum.
    builder = get_active_builder( ).
    IF builder <> me.
      self = builder->add_string( name        = name
                                  description = description
                                  enum        = enum
                                  required    = required ).
      RETURN.
    ENDIF.

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
    " Forward to active builder if needed
    DATA builder TYPE REF TO zcl_mcp_schema_builder.
    DATA path TYPE string.
    builder = get_active_builder( ).
    IF builder <> me.
      self = builder->add_number( name        = name
                                  description = description
                                  required    = required ).
      RETURN.
    ENDIF.

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
    " Forward to active builder if needed
    DATA builder TYPE REF TO zcl_mcp_schema_builder.
    DATA path TYPE string.
    builder = get_active_builder( ).
    IF builder <> me.
      self = builder->add_integer( name        = name
                                   description = description
                                   required    = required ).
      RETURN.
    ENDIF.

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
    " Forward to active builder if needed
    DATA builder TYPE REF TO zcl_mcp_schema_builder.
    builder = get_active_builder( ).
    IF builder <> me.
      self = builder->add_boolean( name        = name
                                   description = description
                                   required    = required ).
      RETURN.
    ENDIF.

    add_property( name        = name
                  type        = 'boolean'
                  description = description
                  required    = required ).

    self = me.
  ENDMETHOD.

  METHOD begin_object.
    " Forward to active builder if needed
    DATA builder TYPE REF TO zcl_mcp_schema_builder.
    DATA path TYPE string.
    DATA new_builder TYPE REF TO zcl_mcp_schema_builder.
    builder = get_active_builder( ).
    IF builder <> me.
      self = builder->begin_object( name        = name
                                    description = description
                                    required    = required ).
      RETURN.
    ENDIF.

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
    new_builder->root_builder   = root_builder.

    " Update active builder in root
    root_builder->active_builder = new_builder.

    self = new_builder.
  ENDMETHOD.

  METHOD end_object.
    " Forward to active builder if needed
    DATA builder TYPE REF TO zcl_mcp_schema_builder.
      FIELD-SYMBOLS <required> LIKE LINE OF required_properties.
    builder = get_active_builder( ).
    IF builder <> me.
      self = builder->end_object( ).
      RETURN.
    ENDIF.

    " Add required properties if any
    IF required_properties IS NOT INITIAL AND node_type = 'object'.
      schema->touch_array( |{ current_path }/required| ).

      
      LOOP AT required_properties ASSIGNING <required>.
        schema->set( iv_path = |{ current_path }/required/{ sy-tabix }|
                     iv_val  = <required> ).
      ENDLOOP.
    ENDIF.

    " Update active builder in root before returning to parent
    root_builder->active_builder = parent_builder.

    " Return to parent
    self = parent_builder.
  ENDMETHOD.

  METHOD begin_array.
    " Forward to active builder if needed
    DATA builder TYPE REF TO zcl_mcp_schema_builder.
    DATA path TYPE string.
    DATA new_builder TYPE REF TO zcl_mcp_schema_builder.
    builder = get_active_builder( ).
    IF builder <> me.
      self = builder->begin_array( name        = name
                                   description = description
                                   required    = required ).
      RETURN.
    ENDIF.

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
    new_builder->root_builder   = root_builder.

    " Update active builder in root
    root_builder->active_builder = new_builder.

    self = new_builder.
  ENDMETHOD.

  METHOD end_array.
    " Forward to active builder if needed
    DATA builder TYPE REF TO zcl_mcp_schema_builder.
      FIELD-SYMBOLS <required> LIKE LINE OF required_properties.
    builder = get_active_builder( ).
    IF builder <> me.
      self = builder->end_array( ).
      RETURN.
    ENDIF.

    " Add required properties if any
    IF required_properties IS NOT INITIAL AND node_type = 'array'.
      schema->touch_array( |{ current_path }/required| ).

      
      LOOP AT required_properties ASSIGNING <required>.
        schema->set( iv_path = |{ current_path }/required/{ sy-tabix }|
                     iv_val  = <required> ).
      ENDLOOP.
    ENDIF.

    " Update active builder in root before returning to parent
    root_builder->active_builder = parent_builder.

    " Return to parent
    self = parent_builder.
  ENDMETHOD.

  METHOD to_json.
    " Forward to active builder if needed
    DATA builder TYPE REF TO zcl_mcp_schema_builder.
      FIELD-SYMBOLS <required> LIKE LINE OF required_properties.
    builder = get_active_builder( ).
    IF builder <> me.
      result = builder->to_json( ).
      RETURN.
    ENDIF.

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
