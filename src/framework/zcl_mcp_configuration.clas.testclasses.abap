CLASS ltcl_mcp_configuration DEFINITION FINAL
  FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.

  PRIVATE SECTION.
    CLASS-DATA sql_environment TYPE REF TO if_osql_test_environment. " Make this class-wide

    DATA cut         TYPE REF TO zcl_mcp_configuration.
    DATA test_area   TYPE zmcp_area                    VALUE 'TEST_AREA'.
    DATA test_server TYPE zmcp_server                  VALUE 'TEST_SERVER'.

    CLASS-METHODS class_setup.    " Runs once before all tests
    CLASS-METHODS class_teardown. " Runs once after all tests

    METHODS setup.
    METHODS clear_test_data. " Helper method to clear test data

    " Tests for get_allowed_origins
    METHODS test_get_origins_specific      FOR TESTING.
    METHODS test_get_origins_area_wildcard FOR TESTING.
    METHODS test_get_origins_full_wildcard FOR TESTING.
    METHODS test_get_origins_no_entries    FOR TESTING.

    " Tests for get_cors_mode
    METHODS test_get_cors_mode             FOR TESTING.
    METHODS test_get_cors_mode_default     FOR TESTING.

    " Tests for get_logger
    METHODS test_get_logger                FOR TESTING.
    METHODS test_get_logger_caching        FOR TESTING.
ENDCLASS.


CLASS ltcl_mcp_configuration IMPLEMENTATION.
  METHOD class_setup.
    " Initialize test double environment once for the entire test class
    sql_environment = cl_osql_test_environment=>create( VALUE #( ( 'ZMCP_ORIGINS' ) ( 'ZMCP_CONFIG' ) ) ).
  ENDMETHOD.

  METHOD class_teardown.
    " Clean up once after all tests
    IF sql_environment IS BOUND.
      sql_environment->destroy( ).
    ENDIF.
  ENDMETHOD.

  METHOD setup.
    " This runs before each test method
    " We only clear test data here, not creating a new environment
    clear_test_data( ).
  ENDMETHOD.

  METHOD clear_test_data.
    " Clear any previously inserted test data
    IF sql_environment IS BOUND.
      sql_environment->clear_doubles( ).
    ENDIF.
  ENDMETHOD.

  METHOD test_get_origins_specific.
    " Arrange: Exact match for area and server
    DATA test_origins TYPE TABLE OF zmcp_origins.

    APPEND VALUE #( area   = test_area
                    server = test_server
                    origin = 'https://example.com'
                    id     = 1 ) TO test_origins.
    APPEND VALUE #( area   = test_area
                    server = test_server
                    origin = 'https://test.example.com'
                    id     = 2 ) TO test_origins.
    sql_environment->insert_test_data( test_origins ).

    DATA test_config TYPE TABLE OF zmcp_config.
    APPEND VALUE #( cors_mode = 'C' ) TO test_config.
    sql_environment->insert_test_data( test_config ).

    cut = NEW #( area   = test_area
                 server = test_server ).

    " Act
    DATA(origins) = cut->get_allowed_origins( ).

    " Assert
    cl_abap_unit_assert=>assert_equals( exp = 2
                                        act = lines( origins )
                                        msg = 'Should return exactly 2 origins' ).

    " Fix type compatibility issues by checking if the value exists in the table
    " rather than direct table contains assertion
    DATA(found1) = abap_false.
    DATA(found2) = abap_false.

    LOOP AT origins ASSIGNING FIELD-SYMBOL(<origin>).
      IF <origin> = 'https://example.com'.
        found1 = abap_true.
      ENDIF.
      IF <origin> = 'https://test.example.com'.
        found2 = abap_true.
      ENDIF.
    ENDLOOP.

    cl_abap_unit_assert=>assert_true( act = found1
                                      msg = 'Origins should contain https://example.com' ).

    cl_abap_unit_assert=>assert_true( act = found2
                                      msg = 'Origins should contain https://test.example.com' ).
  ENDMETHOD.

  METHOD test_get_origins_area_wildcard.
    " Arrange: Wildcard area, specific server
    DATA test_origins TYPE TABLE OF zmcp_origins.

    APPEND VALUE #( area   = '*'
                    server = test_server
                    origin = 'https://area-wildcard.com'
                    id     = 1 ) TO test_origins.
    APPEND VALUE #( area   = '*'
                    server = test_server
                    origin = 'https://another.com'
                    id     = 2 ) TO test_origins.
    sql_environment->insert_test_data( test_origins ).

    DATA test_config TYPE TABLE OF zmcp_config.
    APPEND VALUE #( cors_mode = 'C' ) TO test_config.
    sql_environment->insert_test_data( test_config ).

    cut = NEW #( area   = test_area
                 server = test_server ).

    " Act
    DATA(origins) = cut->get_allowed_origins( ).

    " Assert
    cl_abap_unit_assert=>assert_equals( exp = 2
                                        act = lines( origins )
                                        msg = 'Should return exactly 2 origins' ).

    " Fix type compatibility by checking for value in loop
    DATA(found) = abap_false.
    LOOP AT origins ASSIGNING FIELD-SYMBOL(<origin>).
      IF <origin> = 'https://area-wildcard.com'.
        found = abap_true.
        EXIT.
      ENDIF.
    ENDLOOP.

    cl_abap_unit_assert=>assert_true( act = found
                                      msg = 'Origins should contain https://area-wildcard.com' ).
  ENDMETHOD.

  METHOD test_get_origins_full_wildcard.
    " Arrange: Wildcard area, wildcard server
    DATA test_origins TYPE TABLE OF zmcp_origins.

    APPEND VALUE #( area   = '*'
                    server = '*'
                    origin = 'https://full-wildcard.com'
                    id     = 1  ) TO test_origins.
    APPEND VALUE #( area   = '*'
                    server = '*'
                    origin = 'https://another-full.com'
                    id     = 2 ) TO test_origins.
    sql_environment->insert_test_data( test_origins ).

    DATA test_config TYPE TABLE OF zmcp_config.
    APPEND VALUE #( cors_mode = 'C' ) TO test_config.
    sql_environment->insert_test_data( test_config ).

    cut = NEW #( area   = test_area
                 server = test_server ).

    " Act
    DATA(origins) = cut->get_allowed_origins( ).

    " Assert
    cl_abap_unit_assert=>assert_equals( exp = 2
                                        act = lines( origins )
                                        msg = 'Should return exactly 2 origins' ).

    " Fix type compatibility by checking for value in loop
    DATA(found) = abap_false.
    LOOP AT origins ASSIGNING FIELD-SYMBOL(<origin>).
      IF <origin> = 'https://full-wildcard.com'.
        found = abap_true.
        EXIT.
      ENDIF.
    ENDLOOP.

    cl_abap_unit_assert=>assert_true( act = found
                                      msg = 'Origins should contain https://full-wildcard.com' ).
  ENDMETHOD.

  METHOD test_get_origins_no_entries.
    " Arrange: No entries in origins table
    DATA test_config TYPE TABLE OF zmcp_config.

    APPEND VALUE #( cors_mode = 'C' ) TO test_config.
    sql_environment->insert_test_data( test_config ).

    cut = NEW #( area   = test_area
                 server = test_server ).

    " Act
    DATA(origins) = cut->get_allowed_origins( ).

    " Assert: Should default to wildcard
    cl_abap_unit_assert=>assert_equals( exp = 1
                                        act = lines( origins )
                                        msg = 'Should return default wildcard' ).

    " Fix type compatibility by checking for value in loop
    DATA(found) = abap_false.
    LOOP AT origins ASSIGNING FIELD-SYMBOL(<origin>).
      IF <origin> = '*'.
        found = abap_true.
        EXIT.
      ENDIF.
    ENDLOOP.

    cl_abap_unit_assert=>assert_true( act = found
                                      msg = 'Origins should contain wildcard "*"' ).
  ENDMETHOD.

  METHOD test_get_cors_mode.
    " Arrange: Explicit ENFORCE mode
    DATA test_config TYPE TABLE OF zmcp_config.

    APPEND VALUE #( cors_mode = 'E' ) TO test_config.
    sql_environment->insert_test_data( test_config ).

    cut = NEW #( area   = test_area
                 server = test_server ).

    " Act
    DATA(cors_mode) = cut->get_cors_mode( ).

    " Assert
    cl_abap_unit_assert=>assert_equals( exp = 'E'
                                        act = cors_mode
                                        msg = 'CORS mode should be ENFORCE (E)' ).
  ENDMETHOD.

  METHOD test_get_cors_mode_default.
    " Arrange: Empty CORS mode (will default to IGNORE)
    DATA test_config TYPE TABLE OF zmcp_config.

    APPEND VALUE #( cors_mode = '' ) TO test_config.
    sql_environment->insert_test_data( test_config ).

    cut = NEW #( area   = test_area
                 server = test_server ).

    " Act
    DATA(cors_mode) = cut->get_cors_mode( ).

    " Assert
    cl_abap_unit_assert=>assert_equals( exp = 'I'
                                        act = cors_mode
                                        msg = 'Empty CORS mode should default to IGNORE (I)' ).
  ENDMETHOD.

  METHOD test_get_logger.
    " Arrange
    DATA test_config TYPE TABLE OF zmcp_config.

    APPEND VALUE #( log_level = zcl_mcp_logger=>log_levels-info
                    object    = 'TEST_OBJ'
                    subobject = 'TEST_SUB' )
           TO test_config.
    sql_environment->insert_test_data( test_config ).

    cut = NEW #( area   = test_area
                 server = test_server ).

    " Act
    DATA(logger) = cut->get_logger( ).

    " Assert
    cl_abap_unit_assert=>assert_bound( act = logger
                                       msg = 'Logger should be bound' ).
  ENDMETHOD.

  METHOD test_get_logger_caching.
    " Arrange
    DATA test_config TYPE TABLE OF zmcp_config.

    APPEND VALUE #( log_level = zcl_mcp_logger=>log_levels-info
                    object    = 'TEST_OBJ'
                    subobject = 'TEST_SUB' )
           TO test_config.
    sql_environment->insert_test_data( test_config ).

    cut = NEW #( area   = test_area
                 server = test_server ).

    " Act: Get logger twice
    DATA(logger1) = cut->get_logger( ).
    DATA(logger2) = cut->get_logger( ).

    " Assert: Should return the same instance
    cl_abap_unit_assert=>assert_equals( exp = logger1
                                        act = logger2
                                        msg = 'Repeated calls to get_logger should return the same instance' ).
  ENDMETHOD.
ENDCLASS.
