CLASS ltcl_mcp_schema_validator_test DEFINITION
  FINAL
  FOR TESTING
  RISK LEVEL HARMLESS
  DURATION SHORT.

  PRIVATE SECTION.
    DATA schema    TYPE REF TO zif_mcp_ajson.
    DATA validator TYPE REF TO zcl_mcp_schema_validator.

    METHODS teardown.
    METHODS test_constructor         FOR TESTING RAISING cx_static_check.
    METHODS test_basic_validation    FOR TESTING RAISING cx_static_check.
    METHODS test_required_properties FOR TESTING RAISING cx_static_check.
    METHODS test_string_validation   FOR TESTING RAISING cx_static_check.
    METHODS test_number_validation   FOR TESTING RAISING cx_static_check.
    METHODS test_integer_validation  FOR TESTING RAISING cx_static_check.
    METHODS test_boolean_validation  FOR TESTING RAISING cx_static_check.
    METHODS test_object_validation   FOR TESTING RAISING cx_static_check.
    METHODS test_array_validation    FOR TESTING RAISING cx_static_check.
    METHODS test_nested_array        FOR TESTING RAISING cx_static_check.
    METHODS test_enum_validation     FOR TESTING RAISING cx_static_check.
    METHODS test_error_messages      FOR TESTING RAISING cx_static_check.

ENDCLASS.

CLASS ltcl_mcp_schema_validator_test IMPLEMENTATION.

  METHOD teardown.
    " Clean up after each test
    CLEAR schema.
    CLEAR validator.
  ENDMETHOD.

  METHOD test_constructor.
    " Test schema creation
    schema = zcl_mcp_ajson=>parse( `{"type":"object","properties":{"name":{"type":"string"}}}` ).

    " Create validator instance
    validator = NEW zcl_mcp_schema_validator( schema ).

    " Verify constructor worked
    DATA errors TYPE string_table.
    errors = validator->get_errors( ).
    cl_abap_unit_assert=>assert_initial( errors ).
  ENDMETHOD.

  METHOD test_basic_validation.
    " Create schema
    schema = zcl_mcp_ajson=>parse( `{"type":"object","properties":{"name":{"type":"string"}}}` ).

    validator = NEW zcl_mcp_schema_validator( schema ).

    " Valid data
    DATA(json) = zcl_mcp_ajson=>parse( `{"name":"John"}` ).
    DATA(is_valid) = validator->validate( json ).
    cl_abap_unit_assert=>assert_true( is_valid ).

    " Invalid data (wrong type)
    json = zcl_mcp_ajson=>parse( `{"name":123}` ).
    is_valid = validator->validate( json ).
    cl_abap_unit_assert=>assert_false( is_valid ).
  ENDMETHOD.

  METHOD test_required_properties.
    " Create schema with required property
    schema = zcl_mcp_ajson=>parse( |\{"type":"object","properties":\{"name":\{"type":"string"\},| &&
                                   |"age":\{"type":"integer"\}\},"required":["name"]\}| ).

    validator = NEW zcl_mcp_schema_validator( schema ).

    " Valid data with required property
    DATA(json) = zcl_mcp_ajson=>parse( `{"name":"John","age":30}` ).
    DATA(is_valid) = validator->validate( json ).
    cl_abap_unit_assert=>assert_true( is_valid ).

    " Invalid data missing required property
    json = zcl_mcp_ajson=>parse( `{"age":30}` ).
    is_valid = validator->validate( json ).
    cl_abap_unit_assert=>assert_false( is_valid ).

    " Check error message
    DATA(errors) = validator->get_errors( ).
    cl_abap_unit_assert=>assert_not_initial( errors ).

    DATA found TYPE abap_bool.
    LOOP AT errors ASSIGNING FIELD-SYMBOL(<error>).
      IF <error> CS 'Required property "name" is missing'.
        found = abap_true.
        EXIT.
      ENDIF.
    ENDLOOP.

    cl_abap_unit_assert=>assert_true( found ).
  ENDMETHOD.

  METHOD test_string_validation.
    " Create schema with string constraints
    schema = zcl_mcp_ajson=>parse( |\{"type":"object","properties":\{"text":| &&
                                   |\{"type":"string","minLength":2,"maxLength":10\}\}\}| ).

    validator = NEW zcl_mcp_schema_validator( schema ).

    " Valid data
    DATA(json) = zcl_mcp_ajson=>parse( `{"text":"Hello"}` ).
    DATA(is_valid) = validator->validate( json ).
    cl_abap_unit_assert=>assert_true( is_valid ).

    " Invalid data - too short
    json = zcl_mcp_ajson=>parse( `{"text":"A"}` ).
    is_valid = validator->validate( json ).
    cl_abap_unit_assert=>assert_false( is_valid ).

    " Invalid data - too long
    json = zcl_mcp_ajson=>parse( `{"text":"This text is too long"}` ).
    is_valid = validator->validate( json ).
    cl_abap_unit_assert=>assert_false( is_valid ).
  ENDMETHOD.

  METHOD test_number_validation.
    " Create schema with number constraints
    schema = zcl_mcp_ajson=>parse( |\{"type":"object","properties":\{"score":| &&
                                   |\{"type":"number","minimum":0,"maximum":100\}\}\}| ).

    validator = NEW zcl_mcp_schema_validator( schema ).

    " Valid data
    DATA(json) = zcl_mcp_ajson=>parse( `{"score":75.5}` ).
    DATA(is_valid) = validator->validate( json ).
    cl_abap_unit_assert=>assert_true( is_valid ).

    " Invalid data - too low
    json = zcl_mcp_ajson=>parse( `{"score":-10}` ).
    is_valid = validator->validate( json ).
    cl_abap_unit_assert=>assert_false( is_valid ).

    " Invalid data - too high
    json = zcl_mcp_ajson=>parse( `{"score":200}` ).
    is_valid = validator->validate( json ).
    cl_abap_unit_assert=>assert_false( is_valid ).
  ENDMETHOD.

  METHOD test_integer_validation.
    " Create schema with integer constraints
    schema = zcl_mcp_ajson=>parse( |\{"type":"object","properties":\{"age":| &&
                                   |\{"type":"integer","minimum":18,"maximum":120\}\}\}| ).

    validator = NEW zcl_mcp_schema_validator( schema ).

    " Valid data
    DATA(json) = zcl_mcp_ajson=>parse( `{"age":30}` ).
    DATA(is_valid) = validator->validate( json ).
    cl_abap_unit_assert=>assert_true( is_valid ).

    " Invalid data - too low
    json = zcl_mcp_ajson=>parse( `{"age":15}` ).
    is_valid = validator->validate( json ).
    cl_abap_unit_assert=>assert_false( is_valid ).

    " Invalid data - too high
    json = zcl_mcp_ajson=>parse( `{"age":130}` ).
    is_valid = validator->validate( json ).
    cl_abap_unit_assert=>assert_false( is_valid ).
  ENDMETHOD.

  METHOD test_boolean_validation.
    " Create schema with boolean property
    schema = zcl_mcp_ajson=>parse( `{"type":"object","properties":{"active":{"type":"boolean"}}}` ).

    validator = NEW zcl_mcp_schema_validator( schema ).

    " Valid data
    DATA(json) = zcl_mcp_ajson=>parse( `{"active":true}` ).
    DATA(is_valid) = validator->validate( json ).
    cl_abap_unit_assert=>assert_true( is_valid ).

    " Invalid data - wrong type
    json = zcl_mcp_ajson=>parse( `{"active":"yes"}` ).
    is_valid = validator->validate( json ).
    cl_abap_unit_assert=>assert_false( is_valid ).
  ENDMETHOD.

  METHOD test_object_validation.
    " Create schema with nested object
    schema = zcl_mcp_ajson=>parse( |\{"type":"object","properties":\{"person":\{"type":"object",| &&
                                   |"properties":\{"name":\{"type":"string"\},"age":\{"type":"integer"\}\},| &&
                                   |"required":["name"]\}\}\}| ).

    validator = NEW zcl_mcp_schema_validator( schema ).

    " Valid data
    DATA(json) = zcl_mcp_ajson=>parse( `{"person":{"name":"John","age":30}}` ).
    DATA(is_valid) = validator->validate( json ).
    cl_abap_unit_assert=>assert_true( is_valid ).

    " Invalid data - missing required property in nested object
    json = zcl_mcp_ajson=>parse( `{"person":{"age":30}}` ).
    is_valid = validator->validate( json ).
    cl_abap_unit_assert=>assert_false( is_valid ).
  ENDMETHOD.

  METHOD test_array_validation.
    " Create schema with array
    schema = zcl_mcp_ajson=>parse( |\{"type":"object","properties":\{"tags":\{"type":"array",| &&
                                   |"items":\{"type":"string"\},"minItems":1,"maxItems":5\}\}\}| ).

    validator = NEW zcl_mcp_schema_validator( schema ).

    " Valid data
    DATA(json) = zcl_mcp_ajson=>parse( `{"tags":["red","green","blue"]}` ).
    DATA(is_valid) = validator->validate( json ).
    cl_abap_unit_assert=>assert_true( is_valid ).

    " Invalid data - empty array
    json = zcl_mcp_ajson=>parse( `{"tags":[]}` ).
    is_valid = validator->validate( json ).
    cl_abap_unit_assert=>assert_false( is_valid ).

    " Invalid data - wrong type in array
    json = zcl_mcp_ajson=>parse( `{"tags":["red",123,"blue"]}` ).
    is_valid = validator->validate( json ).
    cl_abap_unit_assert=>assert_false( is_valid ).

    " Invalid data - too many items
    json = zcl_mcp_ajson=>parse( `{"tags":["red","green","blue","yellow","black","white"]}` ).
    is_valid = validator->validate( json ).
    cl_abap_unit_assert=>assert_false( is_valid ).
  ENDMETHOD.

  METHOD test_nested_array.
    " Create schema with nested array
    schema = zcl_mcp_ajson=>parse( |\{"type":"object","properties":\{"matrix":\{"type":"array",| &&
                                   |"items":\{"type":"array","items":\{"type":"integer"\}\}\}\}\}| ).

    validator = NEW zcl_mcp_schema_validator( schema ).

    " Valid data - 2D array of integers
    DATA(json) = zcl_mcp_ajson=>parse( `{"matrix":[[1,2,3],[4,5,6],[7,8,9]]}` ).
    DATA(is_valid) = validator->validate( json ).
    cl_abap_unit_assert=>assert_true( is_valid ).

    " Invalid data - nested array with wrong type
    json = zcl_mcp_ajson=>parse( `{"matrix":[[1,2,3],[4,"five",6],[7,8,9]]}` ).
    is_valid = validator->validate( json ).
    cl_abap_unit_assert=>assert_false( is_valid ).
  ENDMETHOD.

  METHOD test_enum_validation.
    " Create schema with enum
    schema = zcl_mcp_ajson=>parse( |\{"type":"object","properties":\{"status":| &&
                                   |\{"type":"string","enum":["active","inactive","pending"]\}\}\}| ).

    validator = NEW zcl_mcp_schema_validator( schema ).

    " Valid data
    DATA(json) = zcl_mcp_ajson=>parse( `{"status":"active"}` ).
    DATA(is_valid) = validator->validate( json ).
    cl_abap_unit_assert=>assert_true( is_valid ).

    " Invalid data - value not in enum
    json = zcl_mcp_ajson=>parse( `{"status":"deleted"}` ).
    is_valid = validator->validate( json ).
    cl_abap_unit_assert=>assert_false( is_valid ).

    " Check error message contains enum values
    DATA(errors) = validator->get_errors( ).
    cl_abap_unit_assert=>assert_not_initial( errors ).

    DATA found TYPE abap_bool.
    LOOP AT errors ASSIGNING FIELD-SYMBOL(<error>).
      IF <error> CS '"active"' AND <error> CS '"inactive"' AND <error> CS '"pending"'.
        found = abap_true.
        EXIT.
      ENDIF.
    ENDLOOP.

    cl_abap_unit_assert=>assert_true( found ).
  ENDMETHOD.

  METHOD test_error_messages.
    " Create schema with multiple constraints
    schema = zcl_mcp_ajson=>parse( |\{"type":"object","properties":\{"username":\{"type":"string",| &&
                                   |"minLength":3,"maxLength":20\},"age":\{"type":"integer",| &&
                                   |"minimum":18\}\},"required":["username","age"]\}| ).

    validator = NEW zcl_mcp_schema_validator( schema ).

    " Invalid data with multiple validation errors
    DATA(json) = zcl_mcp_ajson=>parse( `{"username":"a","age":15}` ).
    DATA(is_valid) = validator->validate( json ).
    cl_abap_unit_assert=>assert_false( is_valid ).

    " Verify error messages
    DATA(errors) = validator->get_errors( ).
    cl_abap_unit_assert=>assert_equals( exp = 2
                                        act = lines( errors ) ).

    DATA found_minlength TYPE abap_bool.
    DATA found_minimum   TYPE abap_bool.

    LOOP AT errors ASSIGNING FIELD-SYMBOL(<error>).
      IF <error> CS 'minimum length'.
        found_minlength = abap_true.
      ENDIF.
      IF <error> CS 'must be at least 18'.
        found_minimum = abap_true.
      ENDIF.
    ENDLOOP.

    cl_abap_unit_assert=>assert_true( found_minlength ).
    cl_abap_unit_assert=>assert_true( found_minimum ).
  ENDMETHOD.

ENDCLASS.
