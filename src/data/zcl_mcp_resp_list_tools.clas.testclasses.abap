CLASS ltcl_tools_response DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    METHODS setup.
    METHODS test_complex_schema FOR TESTING RAISING cx_static_check.
    METHODS test_nested_schema  FOR TESTING RAISING cx_static_check.
    METHODS test_execution      FOR TESTING RAISING cx_static_check.
    METHODS test_output_schema  FOR TESTING RAISING cx_static_check.
    METHODS test_tool_icons     FOR TESTING RAISING cx_static_check.

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

  METHOD test_execution.
    TRY.
        " optional task support - execution block emitted
        DATA(tools) = VALUE zcl_mcp_resp_list_tools=>tools(
                        ( name      = 'async_tool'
                          execution = VALUE #( task_support = zcl_mcp_resp_list_tools=>task_support-optional ) ) ).
        cut->set_tools( tools ).
        DATA(ajson) = cut->zif_mcp_internal~generate_json( ).

        cl_abap_unit_assert=>assert_equals(
          exp = zcl_mcp_resp_list_tools=>task_support-optional
          act = ajson->get_string( '/tools/1/execution/taskSupport' )
          msg = 'taskSupport should be optional' ).

        " required task support
        cut = NEW #( ).
        DATA(tools2) = VALUE zcl_mcp_resp_list_tools=>tools(
                         ( name      = 'required_tool'
                           execution = VALUE #( task_support = zcl_mcp_resp_list_tools=>task_support-required ) ) ).
        cut->set_tools( tools2 ).
        DATA(ajson2) = cut->zif_mcp_internal~generate_json( ).

        cl_abap_unit_assert=>assert_equals(
          exp = zcl_mcp_resp_list_tools=>task_support-required
          act = ajson2->get_string( '/tools/1/execution/taskSupport' )
          msg = 'taskSupport should be required' ).

        " no execution set - block must be absent (forbidden is the default when omitted)
        cut = NEW #( ).
        DATA(tools3) = VALUE zcl_mcp_resp_list_tools=>tools(
                         ( name = 'sync_tool' ) ).
        cut->set_tools( tools3 ).
        DATA(ajson3) = cut->zif_mcp_internal~generate_json( ).

        cl_abap_unit_assert=>assert_false(
          act = ajson3->exists( '/tools/1/execution' )
          msg = 'execution block should be absent when task_support not set' ).

      CATCH zcx_mcp_ajson_error INTO DATA(error).
        cl_abap_unit_assert=>fail( error->get_text( ) ).
    ENDTRY.
  ENDMETHOD.

  METHOD test_output_schema.
    TRY.
        DATA(output_schema) = zcl_mcp_ajson=>create_empty( ).
        output_schema->set( iv_path = '/type'       iv_val = 'object' ).
        output_schema->set( iv_path = '/properties/status/type' iv_val = 'string' ).
        output_schema->set( iv_path = '/properties/count/type'  iv_val = 'integer' ).

        DATA(tools) = VALUE zcl_mcp_resp_list_tools=>tools(
                        ( name          = 'structured_tool'
                          description   = 'Returns structured output'
                          output_schema = output_schema ) ).
        cut->set_tools( tools ).

        DATA(ajson) = cut->zif_mcp_internal~generate_json( ).

        cl_abap_unit_assert=>assert_true(
          act = ajson->exists( '/tools/1/outputSchema' )
          msg = 'outputSchema should exist' ).
        cl_abap_unit_assert=>assert_equals(
          exp = 'object'
          act = ajson->get_string( '/tools/1/outputSchema/type' ) ).
        cl_abap_unit_assert=>assert_equals(
          exp = 'string'
          act = ajson->get_string( '/tools/1/outputSchema/properties/status/type' ) ).
        cl_abap_unit_assert=>assert_equals(
          exp = 'integer'
          act = ajson->get_string( '/tools/1/outputSchema/properties/count/type' ) ).

        " absent when not set
        cut = NEW #( ).
        DATA(tools2) = VALUE zcl_mcp_resp_list_tools=>tools(
                         ( name = 'plain_tool' ) ).
        cut->set_tools( tools2 ).
        DATA(ajson2) = cut->zif_mcp_internal~generate_json( ).

        cl_abap_unit_assert=>assert_false(
          act = ajson2->exists( '/tools/1/outputSchema' )
          msg = 'outputSchema should be absent when not set' ).

      CATCH zcx_mcp_ajson_error INTO DATA(error).
        cl_abap_unit_assert=>fail( error->get_text( ) ).
    ENDTRY.
  ENDMETHOD.

  METHOD test_tool_icons.
    " Build icons explicitly - VALUE constructor fails for types
    " with nested internal tables (sizes field)
    DATA icon1 TYPE zif_mcp_types=>icon.
    icon1-src       = 'https://example.com/tool-icon.png'.
    icon1-mime_type = 'image/png'.
    icon1-theme     = 'light'.
    APPEND '48x48' TO icon1-sizes.
    APPEND '96x96' TO icon1-sizes.

    DATA icon2 TYPE zif_mcp_types=>icon.
    icon2-src = 'https://example.com/tool-icon.svg'.
    APPEND 'any' TO icon2-sizes.

    DATA tool TYPE zcl_mcp_resp_list_tools=>tool.
    tool-name        = 'iconic_tool'.
    tool-description = 'A tool with icons'.
    APPEND icon1 TO tool-icons.
    APPEND icon2 TO tool-icons.

    DATA tools TYPE zcl_mcp_resp_list_tools=>tools.
    APPEND tool TO tools.
    cut->set_tools( tools ).

    TRY.
        DATA(ajson) = cut->zif_mcp_internal~generate_json( ).

        cl_abap_unit_assert=>assert_true(
          act = ajson->exists( '/tools/1/icons' )
          msg = 'icons array should exist' ).
        cl_abap_unit_assert=>assert_equals(
          exp = 'https://example.com/tool-icon.png'
          act = ajson->get_string( '/tools/1/icons/1/src' ) ).
        cl_abap_unit_assert=>assert_equals(
          exp = 'image/png'
          act = ajson->get_string( '/tools/1/icons/1/mimeType' ) ).
        cl_abap_unit_assert=>assert_equals(
          exp = 'light'
          act = ajson->get_string( '/tools/1/icons/1/theme' ) ).
        cl_abap_unit_assert=>assert_equals(
          exp = '48x48'
          act = ajson->get_string( '/tools/1/icons/1/sizes/1' ) ).
        cl_abap_unit_assert=>assert_equals(
          exp = '96x96'
          act = ajson->get_string( '/tools/1/icons/1/sizes/2' ) ).
        cl_abap_unit_assert=>assert_equals(
          exp = 'https://example.com/tool-icon.svg'
          act = ajson->get_string( '/tools/1/icons/2/src' ) ).
        cl_abap_unit_assert=>assert_false(
          act = ajson->exists( '/tools/1/icons/2/mimeType' )
          msg = 'mimeType absent when not set' ).
        cl_abap_unit_assert=>assert_false(
          act = ajson->exists( '/tools/1/icons/2/theme' )
          msg = 'theme absent when not set' ).

      CATCH zcx_mcp_ajson_error INTO DATA(error).
        cl_abap_unit_assert=>fail( error->get_text( ) ).
    ENDTRY.
  ENDMETHOD.
ENDCLASS.
