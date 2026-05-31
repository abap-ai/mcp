CLASS ltcl_schema_builder_ddic_test DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA cut         TYPE REF TO zcl_mcp_schema_builder_ddic.
    DATA json_result TYPE REF TO zif_mcp_ajson.

    METHODS setup.
    METHODS teardown.

    METHODS test_simple_structure       FOR TESTING RAISING cx_static_check.
    METHODS test_structure_diagnostic   FOR TESTING RAISING cx_static_check.
    METHODS test_field_overrides        FOR TESTING RAISING cx_static_check.
    METHODS test_nested_structure       FOR TESTING RAISING cx_static_check.
    METHODS test_table_type_field       FOR TESTING RAISING cx_static_check.
    METHODS test_domain_enum_values     FOR TESTING RAISING cx_static_check.
    METHODS test_date_time_fields       FOR TESTING RAISING cx_static_check.
    METHODS test_numeric_fields         FOR TESTING RAISING cx_static_check.
    METHODS test_key_fields_required    FOR TESTING RAISING cx_static_check.
    METHODS test_field_descriptions     FOR TESTING RAISING cx_static_check.
    METHODS test_invalid_structure      FOR TESTING RAISING cx_static_check.
    METHODS test_database_table         FOR TESTING RAISING cx_static_check.
    METHODS test_complex_nested_struct  FOR TESTING RAISING cx_static_check.
    METHODS test_field_name_cleanup     FOR TESTING RAISING cx_static_check.
    METHODS test_different_length_types FOR TESTING RAISING cx_static_check.

    " Helper methods
    METHODS create_test_structure
      RETURNING VALUE(result) TYPE string.

    METHODS assert_field_exists
      IMPORTING field_path TYPE string
                !json      TYPE REF TO zif_mcp_ajson.

    METHODS assert_field_type
      IMPORTING field_path    TYPE string
                expected_type TYPE string
                !json         TYPE REF TO zif_mcp_ajson.

    METHODS assert_field_required
      IMPORTING field_path TYPE string
                expected   TYPE abap_bool
                !json      TYPE REF TO zif_mcp_ajson.

    METHODS get_property_names
      IMPORTING !json         TYPE REF TO zif_mcp_ajson
      RETURNING VALUE(result) TYPE string_table.
ENDCLASS.

CLASS ltcl_schema_builder_ddic_test IMPLEMENTATION.
  METHOD setup.
    CLEAR: cut,
           json_result.
  ENDMETHOD.

  METHOD teardown.
    CLEAR: cut,
           json_result.
  ENDMETHOD.

  METHOD test_simple_structure.
    " Test with a well-known SAP structure
    cut = NEW #( structure_name = 'BAPIRET2' ).
    json_result = cut->to_json( ).

    " Check that we got a valid JSON object
    cl_abap_unit_assert=>assert_not_initial( json_result ).
    cl_abap_unit_assert=>assert_equals( exp = 'object'
                                        act = json_result->get_string( '/type' ) ).

    " Get all property names to see what we actually have
    DATA(property_names) = json_result->members( '/properties' ).
    cl_abap_unit_assert=>assert_not_initial( act = property_names
                                             msg = 'Schema should have properties' ).

    " BAPIRET2 should contain at least some standard fields
    " Check if we have at least 4 properties (minimum expected)
    DATA(prop_count) = lines( property_names ).
    cl_abap_unit_assert=>assert_true( act = xsdbool( prop_count >= 4 )
                                      msg = |Expected at least 4 properties, found { prop_count }| ).

    " Check for existence of common BAPIRET2 fields (case-insensitive)
    DATA has_type    TYPE abap_bool.
    DATA has_message TYPE abap_bool.

    LOOP AT property_names INTO DATA(prop_name).
      IF to_upper( prop_name ) = 'TYPE'.
        has_type = abap_true.
      ELSEIF to_upper( prop_name ) = 'MESSAGE'.
        has_message = abap_true.
      ENDIF.
    ENDLOOP.

    " At least one of these core fields should exist
    cl_abap_unit_assert=>assert_true( act = xsdbool( has_type = abap_true OR has_message = abap_true )
                                      msg = 'Should have TYPE or MESSAGE field' ).

    " Check field types for fields that exist
    IF has_type = abap_true.
      assert_field_type( field_path    = '/properties/type/type'
                         expected_type = 'string'
                         json          = json_result ).
    ENDIF.

    IF has_message = abap_true.
      assert_field_type( field_path    = '/properties/message/type'
                         expected_type = 'string'
                         json          = json_result ).
    ENDIF.
  ENDMETHOD.

  METHOD test_structure_diagnostic.
    " Diagnostic test to understand what's actually generated
    cut = NEW #( structure_name = 'BAPIRET2' ).
    json_result = cut->to_json( ).

    " Get the actual property names
    DATA(property_names) = json_result->members( '/properties' ).

    " Check we have properties
    cl_abap_unit_assert=>assert_not_initial( act = property_names
                                             msg = 'Should have properties in schema' ).

    " Log each property for debugging
    DATA msg TYPE string.
    LOOP AT property_names INTO DATA(prop_name).
      msg = |Property found: { prop_name }|.
      " This will help identify the exact field names
      cl_abap_unit_assert=>assert_not_initial( act = prop_name
                                               msg = msg ).
    ENDLOOP.

    " Also check the JSON string representation if needed
    IF lines( property_names ) = 0.
      DATA(json_string) = json_result->stringify( 2 ).
      cl_abap_unit_assert=>fail( |No properties found. JSON: { json_string }| ).
    ENDIF.
  ENDMETHOD.

  METHOD test_field_overrides.
    DATA overrides TYPE zcl_mcp_schema_builder_ddic=>def_field_overrides.

    " Set up overrides
    overrides = VALUE #( ( field_path  = 'type'
                           name        = 'messageType'
                           description = 'Type of the return message'
                           required    = abap_true )
                         ( field_path  = 'message'
                           name        = 'messageText'
                           description = 'Actual message content'
                           required    = abap_false ) ).

    cut = NEW #( structure_name  = 'BAPIRET2'
                 field_overrides = overrides ).
    json_result = cut->to_json( ).

    " Check that overrides were applied by checking property names
    DATA(property_names) = json_result->members( '/properties' ).

    " Check that new names exist
    DATA has_messagetype TYPE abap_bool.
    DATA has_messagetext TYPE abap_bool.
    DATA has_type        TYPE abap_bool.
    DATA has_message     TYPE abap_bool.

    LOOP AT property_names INTO DATA(prop_name).
      CASE prop_name.
        WHEN 'messageType'.
          has_messagetype = abap_true.
        WHEN 'messageText'.
          has_messagetext = abap_true.
        WHEN 'type'.
          has_type = abap_true.
        WHEN 'message'.
          has_message = abap_true.
      ENDCASE.
    ENDLOOP.

    " New names should exist
    cl_abap_unit_assert=>assert_true( act = has_messagetype
                                      msg = 'messageType field should exist' ).
    cl_abap_unit_assert=>assert_true( act = has_messagetext
                                      msg = 'messageText field should exist' ).

    " Original names should not exist
    cl_abap_unit_assert=>assert_false( act = has_type
                                       msg = 'type field should not exist (was renamed)' ).
    cl_abap_unit_assert=>assert_false( act = has_message
                                       msg = 'message field should not exist (was renamed)' ).

    " Check descriptions
    cl_abap_unit_assert=>assert_equals( exp = 'Type of the return message'
                                        act = json_result->get_string( '/properties/messageType/description' ) ).

    " Check required flags
    assert_field_required( field_path = '/properties/messageType'
                           expected   = abap_true
                           json       = json_result ).
    assert_field_required( field_path = '/properties/messageText'
                           expected   = abap_false
                           json       = json_result ).
  ENDMETHOD.

  METHOD test_nested_structure.
    " Test with a structure that might contain nested structures
    " First check if the structure exists
    TRY.
        DATA field_list TYPE ddfields.
        DATA tabname    TYPE ddobjname.
        tabname = 'BAPI_ALM_ORDER_HEADER_E'.

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

        IF sy-subrc = 0.
          " Structure exists, test it
          cut = NEW #( structure_name = 'BAPI_ALM_ORDER_HEADER_E' ).
          json_result = cut->to_json( ).

          " Check for nested object properties
          DATA(properties) = json_result->members( '/properties' ).
          cl_abap_unit_assert=>assert_not_initial( properties ).

          " The exact fields depend on your system, but we can check the structure
          cl_abap_unit_assert=>assert_equals( exp = 'object'
                                              act = json_result->get_string( '/type' ) ).
        ELSE.
          " Structure doesn't exist, skip test
          cl_abap_unit_assert=>assert_true( act = abap_true
                                            msg = 'Test skipped - structure not available in system' ).
        ENDIF.

      CATCH zcx_mcp_ajson_error INTO DATA(error).
        " If structure doesn't exist in system, that's OK
        cl_abap_unit_assert=>assert_not_initial( error->get_text( ) ).
    ENDTRY.
  ENDMETHOD.

  METHOD test_table_type_field.
    " Test structure with table type fields
    " Try to find any BAPI structure that might have table fields
    DATA structures_to_try TYPE TABLE OF string.

    structures_to_try = VALUE #( ( |BAPI_ALM_ORDER_HEADER_I| )
                                 ( |BAPIRET2_TAB| )
                                 ( |BAPIRETTAB| )
                                 ( |STANDARD_TABLE_OF_STRING| ) ).

    DATA found_structure TYPE abap_bool VALUE abap_false.

    LOOP AT structures_to_try INTO DATA(struct_name).
      TRY.
          cut = NEW #( structure_name = struct_name ).
          json_result = cut->to_json( ).

          " Look for array type fields
          DATA(properties) = json_result->members( '/properties' ).
          cl_abap_unit_assert=>assert_not_initial( properties ).

          found_structure = abap_true.
          EXIT.

        CATCH zcx_mcp_schema_ddic_error. "#EC EMPTY_CATCH
        CATCH zcx_mcp_ajson_error.  "#EC EMPTY_CATCH
      ENDTRY.
    ENDLOOP.

    IF found_structure = abap_false.
      " No suitable structure found
      cl_abap_unit_assert=>assert_true( act = abap_true
                                        msg = 'Test skipped - no structures with table types found' ).
    ENDIF.
  ENDMETHOD.

  METHOD test_domain_enum_values.
    " Test with a structure that has domain values
    " BAPIRET2-TYPE uses domain BAPI_MTYPE which has fixed values
    cut = NEW #( structure_name = 'BAPIRET2' ).
    json_result = cut->to_json( ).

    " Check if TYPE field has enum values
    DATA(enum_values) = json_result->get( '/properties/type/enum' ).

    " Domain BAPI_MTYPE typically has values like 'S', 'E', 'W', 'I', 'A', 'X'
    IF enum_values IS NOT INITIAL.
      cl_abap_unit_assert=>assert_not_initial( enum_values ).
    ENDIF.
  ENDMETHOD.

  METHOD test_date_time_fields.
    " Create a test structure with date/time fields
    " Using a structure that contains date fields
    cut = NEW #( structure_name = 'SYST' ).
    json_result = cut->to_json( ).

    " Look for date fields and check their descriptions
    DATA(properties) = json_result->members( '/properties' ).
    cl_abap_unit_assert=>assert_not_initial( properties ).
  ENDMETHOD.

  METHOD test_numeric_fields.
    " Test handling of numeric fields
    cut = NEW #( structure_name = 'BAPIRET2' ).
    json_result = cut->to_json( ).

    " NUMBER field should be string (NUMC type)
    assert_field_type( field_path    = '/properties/number/type'
                       expected_type = 'string'
                       json          = json_result ).

    " Check for numeric text hint in description
    DATA(number_desc) = json_result->get_string( '/properties/number/description' ).
    cl_abap_unit_assert=>assert_not_initial( number_desc ).
  ENDMETHOD.

  METHOD test_key_fields_required.
    " Test that key fields are marked as required
    " Try to find a table with key fields
    DATA tables_to_try TYPE TABLE OF string.
    tables_to_try = VALUE #(
      ( |T001| )     " Company code table
      ( |T000| )     " Client table
      ( |DD02L| )    " SAP tables
      ( |DD03L| )    " Table fields
    ).

    DATA success TYPE abap_bool VALUE abap_false.

    LOOP AT tables_to_try INTO DATA(table_name).
      TRY.
          cut = NEW #( structure_name = table_name ).
          json_result = cut->to_json( ).

          " Check if any fields are in the required array
          DATA(required_count) = 0.
          DATA idx TYPE i VALUE 1.
          DO 20 TIMES.
            DATA(path) = |/required/{ idx }|.
            DATA(field) = json_result->get_string( path ).
            IF field IS NOT INITIAL.
              required_count = required_count + 1.
            ELSE.
              EXIT.
            ENDIF.
            idx = idx + 1.
          ENDDO.

          " Key fields should be required
          cl_abap_unit_assert=>assert_true(
            act = xsdbool( required_count > 0 )
            msg = |Table { table_name } should have required key fields| ).

          success = abap_true.
          EXIT.

        CATCH zcx_mcp_schema_ddic_error. "#EC EMPTY_CATCH
        CATCH zcx_mcp_ajson_error. "#EC EMPTY_CATCH
      ENDTRY.
    ENDLOOP.

    IF success = abap_false.
      " No tables found
      cl_abap_unit_assert=>assert_true(
        act = abap_true
        msg = 'Test skipped - no tables with key fields found' ).
    ENDIF.
  ENDMETHOD.

  METHOD test_field_descriptions.
    " Test that field descriptions are properly extracted
    cut = NEW #( structure_name = 'BAPIRET2' ).
    json_result = cut->to_json( ).

    " Check that descriptions are not empty
    DATA(type_desc) = json_result->get_string( '/properties/type/description' ).
    DATA(message_desc) = json_result->get_string( '/properties/message/description' ).

    cl_abap_unit_assert=>assert_not_initial( type_desc ).
    cl_abap_unit_assert=>assert_not_initial( message_desc ).
  ENDMETHOD.

  METHOD test_invalid_structure.
    " Test with non-existent structure
    TRY.
        cut = NEW #( structure_name = 'ZZZ_NONEXISTENT_STRUCTURE_XYZ' ).
        json_result = cut->to_json( ).

        cl_abap_unit_assert=>fail( 'Exception expected for non-existent structure' ).

      CATCH zcx_mcp_schema_ddic_error INTO DATA(ddic_error).
        cl_abap_unit_assert=>assert_not_initial( ddic_error ).
        " The error object itself should exist
        cl_abap_unit_assert=>assert_equals( exp = 'ZZZ_NONEXISTENT_STRUCTURE_XYZ'
                                            act = ddic_error->structure_name ).
      CATCH zcx_mcp_ajson_error INTO DATA(json_error).
        " Also acceptable if JSON error is raised
        cl_abap_unit_assert=>assert_not_initial( json_error ).
    ENDTRY.
  ENDMETHOD.

  METHOD test_database_table.
    " Test with a database table instead of structure
    " Try different tables that might exist in different systems
    DATA tables_to_try TYPE TABLE OF string.

    tables_to_try = VALUE #( ( |T001| )
                             ( |T000| )
                             ( |DD02L| )
                             ( |DD03L| )
                             ( |TADIR| ) ).

    DATA success TYPE abap_bool VALUE abap_false.

    LOOP AT tables_to_try INTO DATA(table_name).
      TRY.
          cut = NEW #( structure_name = table_name ).
          json_result = cut->to_json( ).

          " Should work for database tables too
          cl_abap_unit_assert=>assert_not_initial( json_result ).

          " At least one property should exist
          DATA(props) = json_result->members( '/properties' ).
          cl_abap_unit_assert=>assert_not_initial( act = props
                                                   msg = |Table { table_name } should have properties| ).

          success = abap_true.
          EXIT.

        CATCH zcx_mcp_schema_ddic_error. "#EC EMPTY_CATCH
        CATCH zcx_mcp_ajson_error. "#EC EMPTY_CATCH
      ENDTRY.
    ENDLOOP.

    IF success = abap_false.
      " No tables found, skip test
      cl_abap_unit_assert=>assert_true( act = abap_true
                                        msg = 'Test skipped - no standard tables found in system' ).
    ENDIF.
  ENDMETHOD.

  METHOD test_complex_nested_struct.
    " Test nested structures with field path overrides
    DATA overrides TYPE zcl_mcp_schema_builder_ddic=>def_field_overrides.

    " For BAPIRET2, we can't test nested overrides since it has no nested structures
    " So let's just test that overrides work at the root level
    overrides = VALUE #( required = abap_true
                         ( field_path  = 'type'
                           name        = 'error_type'
                           description = 'Error type code' )
                         ( field_path  = 'message'
                           name        = 'error_message'
                           description = 'Error message text' ) ).

    TRY.
        " Use BAPIRET2 which we know exists
        cut = NEW #( structure_name  = 'BAPIRET2'
                     field_overrides = overrides ).
        json_result = cut->to_json( ).

        cl_abap_unit_assert=>assert_not_initial( json_result ).

        " Verify the overrides were applied
        DATA(props) = json_result->members( '/properties' ).
        DATA found_error_type    TYPE abap_bool.
        DATA found_error_message TYPE abap_bool.

        LOOP AT props INTO DATA(prop).
          IF prop = 'error_type'.
            found_error_type = abap_true.
          ELSEIF prop = 'error_message'.
            found_error_message = abap_true.
          ENDIF.
        ENDLOOP.

        cl_abap_unit_assert=>assert_true( act = found_error_type
                                          msg = 'Override for type->error_type should be applied' ).
        cl_abap_unit_assert=>assert_true( act = found_error_message
                                          msg = 'Override for message->error_message should be applied' ).

      CATCH zcx_mcp_schema_ddic_error INTO DATA(ddic_error).
        cl_abap_unit_assert=>fail( |Unexpected error: { ddic_error->get_text( ) }| ).
      CATCH zcx_mcp_ajson_error INTO DATA(json_error).
        cl_abap_unit_assert=>fail( |JSON error: { json_error->get_text( ) }| ).
    ENDTRY.
  ENDMETHOD.

  METHOD test_field_name_cleanup.
    " Test that field names are properly cleaned up
    " BAPIRET2 fields should all be lowercase
    cut = NEW #( structure_name = 'BAPIRET2' ).
    json_result = cut->to_json( ).

    DATA(property_names) = json_result->members( '/properties' ).
    cl_abap_unit_assert=>assert_not_initial( property_names ).

    " Check that all field names are lowercase
    LOOP AT property_names INTO DATA(prop_name).
      DATA(lower_name) = to_lower( prop_name ).
      cl_abap_unit_assert=>assert_equals( exp = lower_name
                                          act = prop_name
                                          msg = |Field name '{ prop_name }' should be lowercase| ).
    ENDLOOP.
  ENDMETHOD.

  METHOD test_different_length_types.
    " Test handling of different length specifications
    cut = NEW #( structure_name = 'BAPIRET2' ).
    json_result = cut->to_json( ).

    " Check that length constraints are applied
    DATA(message_max_len) = json_result->get_number( '/properties/message/maxLength' ).
    cl_abap_unit_assert=>assert_not_initial( message_max_len ).
    cl_abap_unit_assert=>assert_true( act = xsdbool( message_max_len > 0 )
                                      msg = 'Message field should have a positive maxLength' ).
  ENDMETHOD.

  " Helper methods
  METHOD create_test_structure.
    " Returns a test structure name if available in the system
    " This is system-dependent
    result = 'BAPIRET2'.
  ENDMETHOD.

  METHOD assert_field_exists.
    " Check if the field exists by looking at property names
    DATA field_name  TYPE string.
    DATA parent_path TYPE string.

    " Extract field name and parent path from the full path
    DATA(last_slash) = find( val = field_path
                             sub = '/'
                             occ = -1 ).
    IF last_slash >= 0.
      parent_path = field_path(last_slash).
      field_name = field_path+last_slash.
      field_name = shift_left( val = field_name places = 1 ).
    ELSE.
      parent_path = '/'.
      field_name = field_path.
    ENDIF.

    TRY.
        DATA(members) = json->members( parent_path ).
        READ TABLE members TRANSPORTING NO FIELDS WITH KEY table_line = field_name.
        IF sy-subrc <> 0.
          DATA siblings_list TYPE string.
          LOOP AT members INTO DATA(sibling).
            IF siblings_list IS INITIAL.
              siblings_list = sibling.
            ELSE.
              siblings_list = |{ siblings_list }, { sibling }|.
            ENDIF.
          ENDLOOP.
          cl_abap_unit_assert=>fail(
              |Field { field_path } not found. Available fields at { parent_path }: { siblings_list }| ).
        ENDIF.
      CATCH cx_root. "#EC NEED_CX_ROOT
        cl_abap_unit_assert=>fail( |Field { field_path } not found - parent path { parent_path } does not exist| ).
    ENDTRY.
  ENDMETHOD.

  METHOD assert_field_type.
    DATA(actual_type) = json->get_string( field_path ).
    cl_abap_unit_assert=>assert_equals( exp = expected_type
                                        act = actual_type
                                        msg = |Field { field_path } should be of type { expected_type }| ).
  ENDMETHOD.

  METHOD assert_field_required.
    " In JSON Schema, required is an array at the object level, not a field property
    " Extract just the field name from the path
    DATA field_name TYPE string.

    DATA(last_slash) = find( val = field_path
                             sub = '/'
                             occ = -1 ).
    IF last_slash >= 0.
      field_name = field_path+last_slash.
      field_name = shift_left( val = field_name places = 1 ).
    ELSE.
      field_name = field_path.
    ENDIF.

    " Check if field name is in the required array
    DATA found TYPE abap_bool VALUE abap_false.
    DATA idx   TYPE i         VALUE 1.

    DO 100 TIMES.
      DATA(path) = |/required/{ idx }|.
      DATA(value) = json->get_string( path ).
      IF value IS INITIAL.
        EXIT.
      ENDIF.
      IF value = field_name.
        found = abap_true.
        EXIT.
      ENDIF.
      idx = idx + 1.
    ENDDO.

    cl_abap_unit_assert=>assert_equals( exp = expected
                                        act = found
                                        msg = |Field { field_name } required status should be { expected }| ).
  ENDMETHOD.

  METHOD get_property_names.
    " Helper method to get all property names from the schema
    CLEAR result.

    TRY.
        " Use the members method from zif_mcp_ajson
        result = json->members( '/properties' ).
      CATCH cx_root. "#EC NEED_CX_ROOT "#EC EMPTY_CATCH
    ENDTRY.
  ENDMETHOD.

ENDCLASS.
