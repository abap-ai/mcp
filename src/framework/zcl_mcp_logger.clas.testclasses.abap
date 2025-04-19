CLASS ltcl_mcp_logger DEFINITION FINAL
  FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.

  PRIVATE SECTION.
    CONSTANTS test_object    TYPE balobj_d  VALUE 'ZMCPTEST'.
    CONSTANTS test_subobject TYPE balsubobj VALUE 'LOGGER'.
    CONSTANTS test_ext_id    TYPE balnrext  VALUE 'TEST_ID_12345'.

    DATA logger TYPE REF TO zcl_mcp_logger.

    METHODS setup.
    METHODS teardown.
    METHODS test_info_message         FOR TESTING.
    METHODS test_log_level_filtering  FOR TESTING.
    METHODS test_external_id          FOR TESTING.
    METHODS test_off_logs_nothing     FOR TESTING.
ENDCLASS.

CLASS ltcl_mcp_logger IMPLEMENTATION.
  METHOD setup.
    " Create a new logger instance before each test
    logger = NEW zcl_mcp_logger( object    = test_object
                                 subobject = test_subobject
                                 log_level = zcl_mcp_logger=>log_levels-info ).
  ENDMETHOD.

  METHOD teardown.
    " Clean up
    FREE logger.
  ENDMETHOD.

  METHOD test_info_message.
    " Test basic info logging
    logger->info( 'Test information message' ).

    " Verify log was created
    DATA(log_number) = logger->save( ).
    cl_abap_unit_assert=>assert_not_initial( act = log_number
                                             msg = 'Log number should be returned after saving' ).
  ENDMETHOD.

  METHOD test_log_level_filtering.
    " Set logger to warning level
    DATA(warning_logger) = NEW zcl_mcp_logger( object    = test_object
                                               subobject = test_subobject
                                               log_level = zcl_mcp_logger=>log_levels-warning ).

    " This should not be logged (below warning level)
    warning_logger->info( 'Info message' ).

    " These should be logged (at or above warning level)
    warning_logger->warning( 'Warning message' ).
    warning_logger->error( 'Error message' ).

    " Save and validate
    DATA(log_number) = warning_logger->save( ).
    cl_abap_unit_assert=>assert_not_initial( log_number ).
  ENDMETHOD.

  METHOD test_external_id.
    " Create logger with external ID
    DATA(ext_logger) = NEW zcl_mcp_logger( object      = test_object
                                           subobject   = test_subobject
                                           external_id = test_ext_id ).

    " Log a message
    ext_logger->info( 'Testing external ID' ).

    " Save and validate
    DATA(log_number) = ext_logger->save( ).
    cl_abap_unit_assert=>assert_not_initial( log_number ).
  ENDMETHOD.

  METHOD test_off_logs_nothing.
    " Set logger to OFF level
    DATA(silent_logger) = NEW zcl_mcp_logger( object    = test_object
                                              subobject = test_subobject
                                              log_level = zcl_mcp_logger=>log_levels-off ).

    " None of these should be logged
    silent_logger->info( 'No log' ).
    silent_logger->warning( 'No log' ).
    silent_logger->error( 'No log' ).

    " Since nothing was logged, we shouldn't get a log number
    DATA(log_number) = silent_logger->save( ).
    cl_abap_unit_assert=>assert_initial( act = log_number
                                         msg = 'Log number should be empty when logging is disabled' ).
  ENDMETHOD.
ENDCLASS.
