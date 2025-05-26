CLASS ltcl_mcp_session DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    CONSTANTS:
      session_id_valid   TYPE sysuuid_c32 VALUE '100000000000000000000000000000AA',
      session_id_expired TYPE sysuuid_c32 VALUE '100000000000000000000000000000BB',
      session_id_unknown TYPE sysuuid_c32 VALUE '100000000000000000000000000000CC'.


    CLASS-DATA:
      sql_doubles TYPE REF TO if_osql_test_environment.

    DATA:
      session TYPE REF TO zcl_mcp_session.

    CLASS-METHODS:
      class_setup,
      class_teardown.

    METHODS:
      setup,
      teardown,

      " Constructor tests
      test_constructor_stateless FOR TESTING RAISING zcx_mcp_server,
      test_constructor_icf FOR TESTING RAISING zcx_mcp_server,
      test_constructor_mcp FOR TESTING RAISING zcx_mcp_server,
      test_constructor_unknown FOR TESTING RAISING zcx_mcp_server,
      test_constructor_expired FOR TESTING RAISING zcx_mcp_server,
      test_constructor_create_new FOR TESTING RAISING zcx_mcp_server,

      " Session data manipulation tests
      test_add_entry FOR TESTING RAISING zcx_mcp_server,
      test_add_existing_entry FOR TESTING RAISING zcx_mcp_server,
      test_get_entry FOR TESTING RAISING zcx_mcp_server,
      test_get_nonexistent_entry FOR TESTING RAISING zcx_mcp_server,
      test_get_all FOR TESTING RAISING zcx_mcp_server,
      test_remove_entry FOR TESTING RAISING zcx_mcp_server,
      test_remove_nonexistent_entry FOR TESTING RAISING zcx_mcp_server,
      test_clear FOR TESTING RAISING zcx_mcp_server,

      " Session management tests
      test_save_mcp_mode FOR TESTING RAISING zcx_mcp_server,
      test_save_non_mcp_mode FOR TESTING RAISING zcx_mcp_server,
      test_delete FOR TESTING RAISING zcx_mcp_server,
      test_delete_nonexistent FOR TESTING RAISING zcx_mcp_server,
      test_delete_outdated_sessions FOR TESTING RAISING zcx_mcp_server.
ENDCLASS.

CLASS ltcl_mcp_session IMPLEMENTATION.
  METHOD class_setup.
    " Set up the SQL test environment once for all tests
    sql_doubles = cl_osql_test_environment=>create( VALUE #( ( 'ZMCP_SESSIONS' ) ) ).
  ENDMETHOD.

  METHOD class_teardown.
    " Clean up the test environment
    sql_doubles->destroy( ).
  ENDMETHOD.

  METHOD setup.
    " Clear any existing test data before each test
    sql_doubles->clear_doubles( ).

    " Prepare test data for valid session
    DATA valid_session TYPE zmcp_sessions.
    valid_session-session_id = session_id_valid.
    valid_session-data = '[{"key":"KEY1","value":"VALUE1"},{"key":"KEY2","value":"VALUE2"}]'.
    GET TIME STAMP FIELD valid_session-updated.

    " Prepare test data for expired session
    DATA expired_session TYPE zmcp_sessions.
    expired_session-session_id = session_id_expired.
    expired_session-data = '[{"key":"KEY1","value":"VALUE1"},{"key":"KEY2","value":"VALUE2"}]'.
    " Set timestamp to be older than session_validity (3600 seconds)
    DATA outdated_timestamp TYPE timestamp.
    GET TIME STAMP FIELD outdated_timestamp.
    outdated_timestamp = cl_abap_tstmp=>subtractsecs_to_short(
      tstmp = outdated_timestamp
      secs  = 4300  " Session validity (3600) + some extra time
    ).
    expired_session-updated = outdated_timestamp.

    " Create a table of test data
    DATA test_data TYPE TABLE OF zmcp_sessions.
    APPEND valid_session TO test_data.
    APPEND expired_session TO test_data.

    " Insert test data into mock table
    sql_doubles->insert_test_data( test_data ).
  ENDMETHOD.


  METHOD teardown.
    " No need to destroy sql_doubles here as it's handled in class_teardown
  ENDMETHOD.

  METHOD test_constructor_stateless.
    session = NEW #(
      session_id   = session_id_valid
      session_mode = zcl_mcp_session=>session_mode_stateless
    ).

    " In stateless mode, no data should be loaded
    cl_abap_unit_assert=>assert_equals(
      act = lines( session->get_all( ) )
      exp = 0
      msg = 'Stateless session should not have data loaded'
    ).
  ENDMETHOD.

  METHOD test_constructor_icf.
    session = NEW #(
      session_id   = session_id_valid
      session_mode = zcl_mcp_session=>session_mode_icf
    ).

    " In ICF mode, no data should be loaded
    cl_abap_unit_assert=>assert_equals(
      act = lines( session->get_all( ) )
      exp = 0
      msg = 'ICF session should not have data loaded'
    ).
  ENDMETHOD.

  METHOD test_constructor_mcp.
    " Test MCP mode with data loading
    session = NEW #(
      session_id   = session_id_valid
      session_mode = zcl_mcp_session=>session_mode_mcp
    ).

    " Session data should be loaded
    DATA entries TYPE zcl_mcp_session=>session_entries.
    entries = session->get_all( ).

    cl_abap_unit_assert=>assert_equals(
      act = lines( entries )
      exp = 2
      msg = 'MCP session should have 2 entries loaded'
    ).

    cl_abap_unit_assert=>assert_equals(
      act = entries[ key = 'KEY1' ]-value
      exp = 'VALUE1'
      msg = 'First entry should have correct value'
    ).

    cl_abap_unit_assert=>assert_equals(
      act = entries[ key = 'KEY2' ]-value
      exp = 'VALUE2'
      msg = 'Second entry should have correct value'
    ).
  ENDMETHOD.

  METHOD test_constructor_unknown.
    " Test with unknown session ID
    TRY.
        session = NEW #(
          session_id   = session_id_unknown
          session_mode = zcl_mcp_session=>session_mode_mcp
        ).
        cl_abap_unit_assert=>fail( 'Exception for unknown session expected' ).
      CATCH zcx_mcp_server INTO DATA(exception).
        " Just check that we got the exception - we know it's the right one
        " because we're catching a specific exception class
        cl_abap_unit_assert=>assert_bound(
          act = exception
          msg = 'Exception for unknown session should be raised'
        ).
    ENDTRY.
  ENDMETHOD.

  METHOD test_constructor_expired.
    " Test with expired session
    TRY.
        session = NEW #(
          session_id   = session_id_expired
          session_mode = zcl_mcp_session=>session_mode_mcp
        ).
        cl_abap_unit_assert=>fail( 'Exception for expired session expected' ).
      CATCH zcx_mcp_server INTO DATA(exception).
        " Just check that we got the exception - we know it's the right one
        " because we're catching a specific exception class
        cl_abap_unit_assert=>assert_bound(
          act = exception
          msg = 'Exception for expired session should be raised'
        ).
    ENDTRY.
  ENDMETHOD.

  METHOD test_constructor_create_new.
    " Test MCP mode with create_new flag
    session = NEW #(
      session_id   = session_id_valid
      session_mode = zcl_mcp_session=>session_mode_mcp
      create_new   = abap_true
    ).

    " No data should be loaded when create_new is true
    cl_abap_unit_assert=>assert_equals(
      act = lines( session->get_all( ) )
      exp = 0
      msg = 'Creating new session should not load existing data'
    ).
  ENDMETHOD.

  METHOD test_add_entry.
    " Create a session in stateless mode
    session = NEW #(
      session_id   = session_id_valid
      session_mode = zcl_mcp_session=>session_mode_stateless
    ).

    " Add a new entry
    session->add( VALUE #( key = 'NEW_KEY' value = 'NEW_VALUE' ) ).

    " Check if the entry was added
    DATA entry TYPE zcl_mcp_session=>session_entry.
    entry = session->get( 'NEW_KEY' ).

    cl_abap_unit_assert=>assert_equals(
      act = entry-value
      exp = 'NEW_VALUE'
      msg = 'Entry should be added correctly'
    ).
  ENDMETHOD.

  METHOD test_add_existing_entry.
    " Create a session in stateless mode
    session = NEW #(
      session_id   = session_id_valid
      session_mode = zcl_mcp_session=>session_mode_stateless
    ).

    " Add an entry
    session->add( VALUE #( key = 'TEST_KEY' value = 'INITIAL_VALUE' ) ).

    " Update the same entry
    session->add( VALUE #( key = 'TEST_KEY' value = 'UPDATED_VALUE' ) ).

    " Check if the entry was updated
    DATA entry TYPE zcl_mcp_session=>session_entry.
    entry = session->get( 'TEST_KEY' ).

    cl_abap_unit_assert=>assert_equals(
      act = entry-value
      exp = 'UPDATED_VALUE'
      msg = 'Entry should be updated correctly'
    ).
  ENDMETHOD.

  METHOD test_get_entry.
    " Create a session in MCP mode to load data
    session = NEW #(
      session_id   = session_id_valid
      session_mode = zcl_mcp_session=>session_mode_mcp
    ).

    " Get an existing entry
    DATA entry TYPE zcl_mcp_session=>session_entry.
    entry = session->get( 'KEY1' ).

    cl_abap_unit_assert=>assert_equals(
      act = entry-value
      exp = 'VALUE1'
      msg = 'Should return correct entry value'
    ).
  ENDMETHOD.

  METHOD test_get_nonexistent_entry.
    " Create a session
    session = NEW #(
      session_id   = session_id_valid
      session_mode = zcl_mcp_session=>session_mode_mcp
    ).

    " Get a non-existing entry
    DATA entry TYPE zcl_mcp_session=>session_entry.
    entry = session->get( 'NON_EXISTING' ).

    cl_abap_unit_assert=>assert_initial(
      act = entry
      msg = 'Should return empty entry for non-existing key'
    ).
  ENDMETHOD.

  METHOD test_get_all.
    " Create a session in MCP mode to load data
    session = NEW #(
      session_id   = session_id_valid
      session_mode = zcl_mcp_session=>session_mode_mcp
    ).

    " Get all entries
    DATA entries TYPE zcl_mcp_session=>session_entries.
    entries = session->get_all( ).

    cl_abap_unit_assert=>assert_equals(
      act = lines( entries )
      exp = 2
      msg = 'Should return all 2 entries'
    ).
  ENDMETHOD.

  METHOD test_remove_entry.
    " Create a session in MCP mode to load data
    session = NEW #(
      session_id   = session_id_valid
      session_mode = zcl_mcp_session=>session_mode_mcp
    ).

    " Remove an entry
    session->remove( 'KEY1' ).

    " Check if the entry was removed
    DATA entries TYPE zcl_mcp_session=>session_entries.
    entries = session->get_all( ).

    cl_abap_unit_assert=>assert_equals(
      act = lines( entries )
      exp = 1
      msg = 'One entry should be removed'
    ).

    cl_abap_unit_assert=>assert_equals(
      act = entries[ key = 'KEY2' ]-value
      exp = 'VALUE2'
      msg = 'Remaining entry should be correct'
    ).
  ENDMETHOD.

  METHOD test_remove_nonexistent_entry.
    " Create a session
    session = NEW #(
      session_id   = session_id_valid
      session_mode = zcl_mcp_session=>session_mode_mcp
    ).

    " Get initial count
    DATA count TYPE i.
    count = lines( session->get_all( ) ).

    " Try to remove non-existing entry (should not cause errors)
    session->remove( 'NON_EXISTING' ).

    " Verify count didn't change
    cl_abap_unit_assert=>assert_equals(
      act = lines( session->get_all( ) )
      exp = count
      msg = 'Number of entries should remain unchanged'
    ).
  ENDMETHOD.

  METHOD test_clear.
    " Create a session in MCP mode to load data
    session = NEW #(
      session_id   = session_id_valid
      session_mode = zcl_mcp_session=>session_mode_mcp
    ).

    " Verify data is loaded
    cl_abap_unit_assert=>assert_equals(
      act = lines( session->get_all( ) )
      exp = 2
      msg = 'Session should have 2 entries initially'
    ).

    " Clear the session
    session->clear( ).

    " Check if all entries were removed
    cl_abap_unit_assert=>assert_equals(
      act = lines( session->get_all( ) )
      exp = 0
      msg = 'Session should be empty after clear'
    ).
  ENDMETHOD.

  METHOD test_save_mcp_mode.
    " Create a new session
    session = NEW #(
      session_id   = session_id_valid
      session_mode = zcl_mcp_session=>session_mode_mcp
      create_new   = abap_true
    ).

    " Add some data
    session->add( VALUE #( key = 'SAVE_KEY1' value = 'SAVE_VALUE1' ) ).
    session->add( VALUE #( key = 'SAVE_KEY2' value = 'SAVE_VALUE2' ) ).

    " Save the session
    session->save( ).

    " Verify data was saved to the database
    SELECT SINGLE * FROM zmcp_sessions
      WHERE session_id = @session_id_valid
      INTO @DATA(db_session).

    cl_abap_unit_assert=>assert_subrc(
      act = sy-subrc
      exp = 0
      msg = 'Session should be saved in the database'
    ).

    " Check that the JSON contains our data (simple string check)
    cl_abap_unit_assert=>assert_char_cp(
      act = db_session-data
      exp = '*SAVE_KEY1*SAVE_VALUE1*'
      msg = 'JSON should contain first key-value pair'
    ).

    cl_abap_unit_assert=>assert_char_cp(
      act = db_session-data
      exp = '*SAVE_KEY2*SAVE_VALUE2*'
      msg = 'JSON should contain second key-value pair'
    ).
  ENDMETHOD.

  METHOD test_save_non_mcp_mode.
    " Create session in non-MCP mode
    session = NEW #(
      session_id   = 'TEST_STATELESS'
      session_mode = zcl_mcp_session=>session_mode_stateless
    ).

    session->add( VALUE #( key = 'TEST_KEY' value = 'TEST_VALUE' ) ).

    " Save should do nothing in non-MCP mode
    session->save( ).

    " Verify nothing was saved to the database
    SELECT SINGLE client FROM zmcp_sessions
      WHERE session_id = 'TEST_STATELESS'
      INTO @DATA(db_session).

    cl_abap_unit_assert=>assert_subrc(
      act = sy-subrc
      exp = 4
      msg = 'Session should not be saved in non-MCP mode'
    ).
  ENDMETHOD.

  METHOD test_delete.
    " Create a session
    session = NEW #(
      session_id   = session_id_valid
      session_mode = zcl_mcp_session=>session_mode_mcp
    ).

    " Delete the session
    session->delete( ).

    " Check if the session was deleted from the database
    SELECT SINGLE client FROM zmcp_sessions
      WHERE session_id = @session_id_valid
      INTO @DATA(db_session).

    cl_abap_unit_assert=>assert_subrc(
      act = sy-subrc
      exp = 4
      msg = 'Session should be deleted from the database'
    ).
  ENDMETHOD.

  METHOD test_delete_nonexistent.
    " Create session with unknown ID
    session = NEW #(
      session_id   = 'NONEXISTENT_SESSION'
      session_mode = zcl_mcp_session=>session_mode_stateless
    ).

    " Delete should not cause errors for non-existent sessions
    session->delete( ).

    " No assertion needed - test passes if no exception is raised
  ENDMETHOD.

  METHOD test_delete_outdated_sessions.
    " Insert additional outdated sessions for this test
    DATA current_time TYPE timestampl.
    GET TIME STAMP FIELD current_time.
    DATA(old_time) = cl_abap_tstmp=>subtractsecs(
      tstmp = current_time
      secs  = 5000  " More than session_validity + 10 min buffer
    ).

    " Collect all outdated sessions in a single table
    DATA outdated_sessions TYPE TABLE OF zmcp_sessions.

    DO 3 TIMES.
      DATA outdated_session TYPE zmcp_sessions.
      outdated_session-session_id = |OUTDATED{ sy-index }|.
      outdated_session-data = '[{"key":"TEST","value":"TEST"}]'.
      outdated_session-updated = cl_abap_tstmp=>move_to_short( old_time ).

      APPEND outdated_session TO outdated_sessions.
    ENDDO.

    " Insert all test data at once
    sql_doubles->insert_test_data( outdated_sessions ).

    " Call the method to delete outdated sessions
    DATA(deleted_count) = zcl_mcp_session=>delete_outdated_sessions( ).

    " Verify correct number of sessions deleted (3 outdated + 1 expired = 4)
    cl_abap_unit_assert=>assert_equals(
      act = deleted_count
      exp = 4
      msg = 'Should delete 4 outdated sessions'
    ).

    " Verify only valid session remains

    SELECT COUNT(*) FROM zmcp_sessions INTO @DATA(remaining_count). "#EC CI_NOWHERE

    cl_abap_unit_assert=>assert_equals(
      act = remaining_count
      exp = 1
      msg = 'Only 1 valid session should remain'
    ).
  ENDMETHOD.


ENDCLASS.
