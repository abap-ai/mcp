CLASS zcl_mcp_schema_builder_ddic DEFINITION
  PUBLIC FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    TYPES: BEGIN OF def_field_override,
             field_path  TYPE string,
             name        TYPE string,
             description TYPE string,
             required    TYPE abap_bool,
           END OF def_field_override.

    "! <p class="shorttext synchronized">Table of field overrides</p>
    TYPES def_field_overrides TYPE STANDARD TABLE OF def_field_override
          WITH NON-UNIQUE KEY field_path.

    "! <p class="shorttext synchronized">Create schema builder from DDIC structure</p>
    "!
    "! @parameter structure_name      | <p class="shorttext synchronized">Name of DDIC structure/table</p>
    "! @parameter field_overrides     | <p class="shorttext synchronized">Field override configurations</p>
    "! @raising   zcx_mcp_schema_ddic_error | <p class="shorttext synchronized">DDIC structure error</p>
    "! @raising   zcx_mcp_ajson_error | <p class="shorttext synchronized">JSON error</p>
    METHODS constructor
      IMPORTING structure_name  TYPE string
                field_overrides TYPE def_field_overrides OPTIONAL
      RAISING   zcx_mcp_schema_ddic_error
                zcx_mcp_ajson_error.

    "! <p class="shorttext synchronized">Convert schema to JSON</p>
    "!
    "! @parameter result              | <p class="shorttext synchronized">JSON representation of the schema</p>
    "! @raising   zcx_mcp_ajson_error | <p class="shorttext synchronized">JSON error</p>
    METHODS to_json
      RETURNING VALUE(result) TYPE REF TO zif_mcp_ajson
      RAISING   zcx_mcp_ajson_error.

  PRIVATE SECTION.
    DATA structure_name  TYPE string.
    DATA field_overrides TYPE def_field_overrides.
    DATA builder         TYPE REF TO zcl_mcp_schema_builder.

    "! <p class="shorttext synchronized">Process DDIC structure</p>
    "!
    "! @parameter structure_name            | <p class="shorttext synchronized">Structure name to process</p>
    "! @parameter current_builder           | <p class="shorttext synchronized">Current schema builder</p>
    "! @parameter field_path                | <p class="shorttext synchronized">Current field path</p>
    "! @raising   zcx_mcp_schema_ddic_error | <p class="shorttext synchronized">DDIC structure error</p>
    "! @raising   zcx_mcp_ajson_error       | <p class="shorttext synchronized">JSON error</p>
    METHODS process_structure
      IMPORTING structure_name  TYPE string
                current_builder TYPE REF TO zcl_mcp_schema_builder
                field_path      TYPE string DEFAULT ''
      RAISING   zcx_mcp_schema_ddic_error
                zcx_mcp_ajson_error.

    "! <p class="shorttext synchronized">Process single field</p>
    "!
    "! @parameter field_info                | <p class="shorttext synchronized">Field information</p>
    "! @parameter current_builder           | <p class="shorttext synchronized">Current schema builder</p>
    "! @parameter field_path                | <p class="shorttext synchronized">Current field path</p>
    "! @raising   zcx_mcp_schema_ddic_error | <p class="shorttext synchronized">DDIC structure error</p>
    "! @raising   zcx_mcp_ajson_error       | <p class="shorttext synchronized">JSON error</p>
    METHODS process_field
      IMPORTING field_info      TYPE dfies
                current_builder TYPE REF TO zcl_mcp_schema_builder
                field_path      TYPE string
      RAISING   zcx_mcp_schema_ddic_error
                zcx_mcp_ajson_error.

    "! <p class="shorttext synchronized">Get field name</p>
    "!
    "! @parameter field_info | <p class="shorttext synchronized">Field information</p>
    "! @parameter field_path | <p class="shorttext synchronized">Field path for overrides</p>
    "! @parameter result     | <p class="shorttext synchronized">Field name to use</p>
    METHODS get_field_name
      IMPORTING field_info    TYPE dfies
                field_path    TYPE string
      RETURNING VALUE(result) TYPE string.

    "! <p class="shorttext synchronized">Get field description</p>
    "!
    "! @parameter field_info | <p class="shorttext synchronized">Field information</p>
    "! @parameter field_path | <p class="shorttext synchronized">Field path for overrides</p>
    "! @parameter result     | <p class="shorttext synchronized">Field description</p>
    METHODS get_field_description
      IMPORTING field_info    TYPE dfies
                field_path    TYPE string
      RETURNING VALUE(result) TYPE string.

    "! <p class="shorttext synchronized">Check if field is required</p>
    "!
    "! @parameter field_info | <p class="shorttext synchronized">Field information</p>
    "! @parameter field_path | <p class="shorttext synchronized">Field path for overrides</p>
    "! @parameter result     | <p class="shorttext synchronized">Whether field is required</p>
    METHODS is_field_required
      IMPORTING field_info    TYPE dfies
                field_path    TYPE string
      RETURNING VALUE(result) TYPE abap_bool.

    "! <p class="shorttext synchronized">Get domain values for enum</p>
    "!
    "! @parameter domain_name | <p class="shorttext synchronized">Domain name</p>
    "! @parameter result      | <p class="shorttext synchronized">Domain values</p>
    METHODS get_domain_values
      IMPORTING domain_name   TYPE domname
      RETURNING VALUE(result) TYPE string_table.

    "! <p class="shorttext synchronized">Map ABAP type to JSON schema type</p>
    "!
    "! @parameter field_info | <p class="shorttext synchronized">Field information</p>
    "! @parameter result     | <p class="shorttext synchronized">JSON schema type</p>
    METHODS map_abap_type
      IMPORTING field_info    TYPE dfies
      RETURNING VALUE(result) TYPE string.

    "! <p class="shorttext synchronized">Get field override by path</p>
    "!
    "! @parameter field_path | <p class="shorttext synchronized">Field path</p>
    "! @parameter result     | <p class="shorttext synchronized">Field override if found</p>
    METHODS get_field_override
      IMPORTING field_path    TYPE string
      RETURNING VALUE(result) TYPE def_field_override.

    "! <p class="shorttext synchronized">Build full field path</p>
    "!
    "! @parameter base_path  | <p class="shorttext synchronized">Base path</p>
    "! @parameter field_name | <p class="shorttext synchronized">Field name</p>
    "! @parameter result     | <p class="shorttext synchronized">Full field path</p>
    METHODS build_field_path
      IMPORTING base_path     TYPE string
                field_name    TYPE string
      RETURNING VALUE(result) TYPE string.

    "! <p class="shorttext synchronized">Get field length</p>
    "!
    "! @parameter field_info | <p class="shorttext synchronized">Field information</p>
    "! @parameter result     | <p class="shorttext synchronized">Field length</p>
    METHODS get_field_length
      IMPORTING field_info    TYPE dfies
      RETURNING VALUE(result) TYPE i.
ENDCLASS.

CLASS zcl_mcp_schema_builder_ddic IMPLEMENTATION.
  METHOD constructor.
    me->structure_name  = structure_name.
    me->field_overrides = field_overrides.

    " Validate overrides
    LOOP AT field_overrides INTO DATA(override).
      IF override-field_path IS INITIAL.
        zcx_mcp_schema_ddic_error=>raise_invalid_override( field_path = 'empty'
                                                           reason     = 'Field path cannot be empty' ) ##NO_TEXT.
      ENDIF.
    ENDLOOP.

    " Create the main schema builder
    builder = NEW zcl_mcp_schema_builder( ).

    " Process the main structure
    process_structure( structure_name  = structure_name
                       current_builder = builder ).
  ENDMETHOD.

  METHOD to_json.
    result = builder->to_json( ).
  ENDMETHOD.

  METHOD process_structure.
    " Always use DDIF_FIELDINFO_GET - it works for all DDIC objects
    DATA field_list TYPE ddfields.
    DATA tabname    TYPE ddobjname.

    tabname = structure_name.

    CALL FUNCTION 'DDIF_FIELDINFO_GET'
      EXPORTING
        tabname        = tabname
        langu          = sy-langu
      TABLES
        dfies_tab      = field_list
      EXCEPTIONS
        not_found      = 1
        internal_error = 2
        OTHERS         = 3.

    IF sy-subrc <> 0.
      zcx_mcp_schema_ddic_error=>raise_structure_not_found( structure_name ).
    ENDIF.
    " Process each field
    LOOP AT field_list INTO DATA(field_info).
      " For field path, we need just the field name when at root level
      DATA(field_name_lower) = to_lower( field_info-fieldname ).
      DATA(current_field_path) = COND #(
        WHEN field_path IS INITIAL
        THEN field_name_lower
        ELSE build_field_path( base_path  = field_path
                               field_name = field_name_lower ) ).
      process_field( field_info      = field_info
                     current_builder = current_builder
                     field_path      = current_field_path ).
    ENDLOOP.
  ENDMETHOD.

  METHOD process_field.
    " Build the nested field path correctly
    " For nested processing, pass the current field's name as part of the path
    DATA(nested_field_path) = COND #(
      WHEN field_path IS INITIAL
      THEN to_lower( field_info-fieldname )
      ELSE field_path ).

    DATA(field_name) = get_field_name( field_info = field_info
                                       field_path = nested_field_path ).
    DATA(description) = get_field_description( field_info = field_info
                                               field_path = nested_field_path ).
    DATA(required) = is_field_required( field_info = field_info
                                         field_path = nested_field_path ).

    " Handle different field types
    CASE field_info-datatype.
      WHEN 'STRU'.
        " Nested structure
        DATA(nested_builder) = current_builder->begin_object( name        = field_name
                                                              description = description
                                                              required    = required ).

        " Use the referenced structure name
        DATA struct_name TYPE string.
        IF field_info-rollname IS NOT INITIAL.
          struct_name = field_info-rollname.
        ELSEIF field_info-comptype = 'S'.
          " Component type S indicates structure, use the type from checktable or domname
          IF field_info-checktable IS NOT INITIAL.
            struct_name = field_info-checktable.
          ENDIF.
        ENDIF.

        IF struct_name IS NOT INITIAL.
          " Build the nested field path for recursive processing
          process_structure( structure_name  = struct_name
                             current_builder = nested_builder
                             field_path      = nested_field_path ).
        ENDIF.

        nested_builder->end_object( ).

      WHEN 'TTYP'.
        " Table type
        DATA(array_builder) = current_builder->begin_array( name        = field_name
                                                            description = description
                                                            required    = required ).

        " For table types, we need to analyze the line type
        " If rollname contains the table type, we could potentially analyze it further
        " For now, add a generic item
        array_builder->add_string( name        = 'item'
                                   description = 'Table line item' ) ##NO_TEXT.

        array_builder->end_array( ).

      WHEN OTHERS.
        " Simple field
        DATA(json_type) = map_abap_type( field_info ).

        CASE json_type.
          WHEN 'string'.
            DATA(enum_values) = get_domain_values( field_info-domname ).

            " Get field length
            DATA(max_length) = get_field_length( field_info ).

            " Add format hint for special datatypes
            DATA description_with_format TYPE string.
            description_with_format = description.
            CASE field_info-datatype.
              WHEN 'DATS'.
                description_with_format = |{ description_with_format } (Date: YYYYMMDD)| ##NO_TEXT.
              WHEN 'TIMS'.
                description_with_format = |{ description_with_format } (Time: HHMMSS)| ##NO_TEXT.
              WHEN 'ACCP'.
                description_with_format = |{ description_with_format } (Period: YYYYMM)| ##NO_TEXT.
              WHEN 'NUMC'.
                description_with_format = |{ description_with_format } (Numeric text)| ##NO_TEXT.
            ENDCASE.

            current_builder->add_string( name        = field_name
                                         description = description_with_format
                                         enum        = enum_values
                                         required    = required
                                         min_length  = COND #( WHEN max_length > 0 THEN 0 )
                                         max_length  = COND #( WHEN max_length > 0 THEN max_length ) ).

          WHEN 'integer'.
            current_builder->add_integer( name        = field_name
                                          description = description
                                          required    = required ).

          WHEN 'number'.
            current_builder->add_number( name        = field_name
                                         description = description
                                         required    = required ).

          WHEN 'boolean'.
            current_builder->add_boolean( name        = field_name
                                          description = description
                                          required    = required ).
        ENDCASE.
    ENDCASE.
  ENDMETHOD.

  METHOD get_field_name.
    " Check for override first
    DATA(override) = get_field_override( field_path ).
    IF override-name IS NOT INITIAL.
      result = override-name.
      RETURN.
    ENDIF.

    " Use field name from DDIC info
    result = field_info-fieldname.

    " Clean up the name (remove special characters, etc.)
    REPLACE ALL OCCURRENCES OF '/' IN result WITH '_'.
    REPLACE ALL OCCURRENCES OF '-' IN result WITH '_'.
    result = to_lower( result ).
  ENDMETHOD.

  METHOD get_field_description.
    " Check for override first
    DATA(override) = get_field_override( field_path ).
    IF override-description IS NOT INITIAL.
      result = override-description.
      RETURN.
    ENDIF.

    " Use description from DDIC info
    IF field_info-scrtext_l IS NOT INITIAL.
      result = field_info-scrtext_l.
    ELSEIF field_info-scrtext_m IS NOT INITIAL.
      result = field_info-scrtext_m.
    ELSEIF field_info-scrtext_s IS NOT INITIAL.
      result = field_info-scrtext_s.
    ELSEIF field_info-fieldtext IS NOT INITIAL.
      result = field_info-fieldtext.
    ELSE.
      result = |Field { field_info-fieldname }| ##NO_TEXT.
    ENDIF.
  ENDMETHOD.

  METHOD is_field_required.
    " Check for override first
    DATA(override) = get_field_override( field_path ).
    IF override-name IS NOT INITIAL OR override-description IS NOT INITIAL OR override-field_path IS NOT INITIAL.
      " Override found - use its required flag
      result = override-required.
      RETURN.
    ENDIF.

    IF field_info-keyflag = abap_true.
      result = abap_true.
    ELSEIF field_info-datatype = 'CLNT'.
      result = abap_true.
    ELSE.
      result = abap_false.
    ENDIF.
  ENDMETHOD.

  METHOD get_field_length.
    " Determine actual field length from DDIC info
    IF field_info-leng > 0.
      result = field_info-leng.
    ELSEIF field_info-outputlen > 0.
      result = field_info-outputlen.
    ELSE.
      result = 0.
    ENDIF.
  ENDMETHOD.

  METHOD get_domain_values.
    CLEAR result.

    IF domain_name IS INITIAL.
      RETURN.
    ENDIF.

    " Get fixed domain values
    DATA domain_values TYPE TABLE OF dd07v.
    CALL FUNCTION 'DD_DOMVALUES_GET'
      EXPORTING
        domname        = domain_name
        text           = abap_true
        langu          = sy-langu
      TABLES
        dd07v_tab      = domain_values
      EXCEPTIONS
        wrong_textflag = 1
        OTHERS         = 2.

    IF sy-subrc = 0.
      LOOP AT domain_values INTO DATA(domain_value).
        " Only include active values
        IF domain_value-valpos IS NOT INITIAL.
          APPEND domain_value-domvalue_l TO result.
        ENDIF.
      ENDLOOP.
    ENDIF.
  ENDMETHOD.

  METHOD map_abap_type.
    " Map ABAP datatype to JSON schema type
    CASE field_info-datatype.
      WHEN 'CHAR' OR 'NUMC' OR 'CUKY' OR 'UNIT' OR 'LANG' OR 'CLNT' OR 'LCHR' OR 'LRAW' OR 'STRING' OR 'SSTRING'.
        result = 'string'.
      WHEN 'DATS' OR 'TIMS' OR 'ACCP'.
        result = 'string'.
      WHEN 'INT1' OR 'INT2' OR 'INT4' OR 'INT8'.
        result = 'integer'.
      WHEN 'DEC' OR 'CURR' OR 'QUAN' OR 'FLTP' OR 'D16D' OR 'D34D' OR 'D16R' OR 'D34R' OR 'DECFLOAT16' OR 'DECFLOAT34'.
        result = 'number'.
      WHEN 'RAW' OR 'RAWSTRING'.
        result = 'string'.
      WHEN 'PREC'.
        result = 'integer'.
      WHEN OTHERS.
        " Check internal type if datatype not recognized
        CASE field_info-inttype.
          WHEN 'C' OR 'N' OR 'D' OR 'T' OR 'g' OR 'y'.
            result = 'string'.
          WHEN 'X'.
            result = 'string'.
          WHEN 'I' OR 'b' OR 's' OR '8'.
            result = 'integer'.
          WHEN 'P' OR 'F' OR 'a' OR 'e' OR 'k'.
            result = 'number'.
          WHEN OTHERS.
            result = 'string'.
        ENDCASE.
    ENDCASE.
  ENDMETHOD.

  METHOD get_field_override.
    CLEAR result.

    " The field_path passed in is the full path, we need to check against it directly
    READ TABLE field_overrides INTO result WITH KEY field_path = field_path.
    IF sy-subrc = 0.
      RETURN.
    ENDIF.

    " Also check for just the field name (last part of the path)
    DATA(last_dot) = find( val = field_path
                           sub = '.'
                           occ = -1 ).
    IF last_dot >= 0.
      DATA(field_name_only) = substring( val = field_path
                                         off = last_dot + 1 ).
      READ TABLE field_overrides INTO result WITH KEY field_path = field_name_only.
      IF sy-subrc <> 0.
        CLEAR result.
      ENDIF.
    ELSE.
      " No dot found, field_path is already just the field name
      CLEAR result.
    ENDIF.
  ENDMETHOD.

  METHOD build_field_path.
    IF base_path IS INITIAL.
      result = field_name.
    ELSE.
      result = |{ base_path }.{ field_name }|.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
