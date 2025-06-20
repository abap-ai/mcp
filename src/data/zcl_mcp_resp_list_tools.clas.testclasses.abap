CLASS ltcl_tools_response DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    METHODS setup.
    METHODS test_complex_schema FOR TESTING RAISING cx_static_check.
    METHODS test_nested_schema  FOR TESTING RAISING cx_static_check.

    DATA cut TYPE REF TO zcl_mcp_resp_list_tools.
ENDCLASS.

CLASS ltcl_tools_response IMPLEMENTATION.

  METHOD setup.
    cut = NEW #( ).
  ENDMETHOD.

  METHOD test_complex_schema.
    " Given
    TRY.
        " Create schema for a weather tool
        DATA(schema_builder) = NEW zcl_mcp_schema_builder( ).
        schema_builder->add_string( name        = 'location'
                                    description = 'City name or zip code'
                                    required    = abap_true )->add_integer(
                                                                name        = 'days'
                                                                description = 'Number of days forecast' )->add_boolean(
                                                                    name        = 'detailed'
                                                                    description = 'Get detailed forecast' ).

        DATA(tools) = VALUE zcl_mcp_resp_list_tools=>tools( ( name         = 'weather_tool'
                                                              description  = 'Get weather information'
                                                              title        = 'Weather Tool Title'
                                                              input_schema = schema_builder->to_json( )
                                                              annotations  = VALUE #( title        = 'Weather Tool'
                                                                                      readonlyhint = abap_true ) ) ).

        cut->set_tools( tools ).

        " When
        DATA(ajson) = cut->zif_mcp_internal~generate_json( ).

        " Then
        cl_abap_unit_assert=>assert_equals( exp = 'weather_tool'
                                            act = ajson->get_string( '/tools/1/name' ) ).

        cl_abap_unit_assert=>assert_equals( exp = 'Weather Tool Title'
                                            act = ajson->get_string( '/tools/1/title' ) ).

        cl_abap_unit_assert=>assert_equals(
            exp = 'string'
            act = ajson->get_string( '/tools/1/inputSchema/properties/location/type' ) ).

        cl_abap_unit_assert=>assert_equals( exp = 'location'
                                            act = ajson->get_string( '/tools/1/inputSchema/required/1' ) ).

        cl_abap_unit_assert=>assert_equals( exp = 'Weather Tool'
                                            act = ajson->get_string( '/tools/1/annotations/title' ) ).

      CATCH zcx_mcp_ajson_error INTO DATA(error).
        cl_abap_unit_assert=>fail( error->get_text( ) ).
    ENDTRY.
  ENDMETHOD.

  METHOD test_nested_schema.
    " Given
    TRY.
        " Create schema with nested objects and arrays
        DATA(schema_builder) = NEW zcl_mcp_schema_builder( ).
        schema_builder->add_string( name        = 'name'
                                    description = 'Customer name'
                                    required    = abap_true )->begin_object( name        = 'address'
                                                                             description = 'Customer address'
                                                                             required    = abap_true )->add_string(
                                                                                 name     = 'street'
                                                                                 required = abap_true )->add_string(
                                                                                     name     = 'city'
                                                                                     required = abap_true )->add_string(
                                                                                         name = 'country' )->end_object( )->begin_array(
                                                                                                    name        = 'orders'
                                                                                                    description = 'Previous orders' )->add_string(
                                                                                                        name     = 'id'
                                                                                                        required = abap_true )->add_number(
                                                                                                            name     = 'amount'
                                                                                                            required = abap_true )->add_string(
                                                                                                                name = 'currency' )->end_array( ).

        DATA(tools) = VALUE zcl_mcp_resp_list_tools=>tools( ( name         = 'customer_tool'
                                                              description  = 'Customer management'
                                                              input_schema = schema_builder->to_json( ) ) ).

        cut->set_tools( tools ).

        " When
        DATA(ajson) = cut->zif_mcp_internal~generate_json( ).

        " Then - check nested structures
        cl_abap_unit_assert=>assert_equals( exp = 'object'
                                            act = ajson->get_string( '/tools/1/inputSchema/properties/address/type' ) ).

        cl_abap_unit_assert=>assert_equals(
            exp = 'string'
            act = ajson->get_string( '/tools/1/inputSchema/properties/address/properties/street/type' ) ).

        cl_abap_unit_assert=>assert_equals( exp = 'array'
                                            act = ajson->get_string( '/tools/1/inputSchema/properties/orders/type' ) ).

        cl_abap_unit_assert=>assert_equals(
            exp = 'string'
            act = ajson->get_string( '/tools/1/inputSchema/properties/orders/items/properties/id/type' ) ).

      CATCH zcx_mcp_ajson_error INTO DATA(error).
        cl_abap_unit_assert=>fail( error->get_text( ) ).
    ENDTRY.
  ENDMETHOD.
ENDCLASS.
