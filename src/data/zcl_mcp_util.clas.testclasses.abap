CLASS ltcl_mcp_util DEFINITION FINAL
FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.

  PRIVATE SECTION.
    METHODS timestamp_full_value FOR TESTING.
    METHODS timestamp_midnight FOR TESTING.
    METHODS timestamp_initial    FOR TESTING.
ENDCLASS.

CLASS ltcl_mcp_util IMPLEMENTATION.
  METHOD timestamp_full_value.
    DATA ts TYPE timestamp.

    ts = '20240115143045'.

    cl_abap_unit_assert=>assert_equals( exp = '2024-01-15T14:30:45Z'
                                        act = zcl_mcp_util=>timestamp_to_iso8601( ts )
                                        msg = 'Timestamp should be formatted as ISO-8601 UTC' ).
  ENDMETHOD.

  METHOD timestamp_midnight.
    DATA ts TYPE timestamp.

    ts = '20240601000000'.

    cl_abap_unit_assert=>assert_equals( exp = '2024-06-01T00:00:00Z'
                                        act = zcl_mcp_util=>timestamp_to_iso8601( ts )
                                        msg = 'Midnight timestamp should preserve zero time components' ).
  ENDMETHOD.

  METHOD timestamp_initial.
    DATA ts TYPE timestamp.

    cl_abap_unit_assert=>assert_equals( exp = '0000-00-00T00:00:00Z'
                                        act = zcl_mcp_util=>timestamp_to_iso8601( ts )
                                        msg = 'Initial timestamp should be left-padded before formatting' ).
  ENDMETHOD.

ENDCLASS.
