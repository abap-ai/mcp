  CLASS zcl_mcp_util DEFINITION
    PUBLIC FINAL
    CREATE PUBLIC.

    PUBLIC SECTION.
      "! <p class="shorttext synchronized" lang="en">Convert timestamp to iso format</p>
      "!
      "! @parameter timestamp | <p class="shorttext synchronized" lang="en">Timestamp</p>
      "! @parameter result | <p class="shorttext synchronized" lang="en">Timestamp in Iso format</p>
      CLASS-METHODS timestamp_to_iso8601
        IMPORTING !timestamp    TYPE timestamp
        RETURNING VALUE(result) TYPE string.
  ENDCLASS.

  CLASS zcl_mcp_util IMPLEMENTATION.
    METHOD timestamp_to_iso8601.
      DATA local_date       TYPE sy-datum.
      DATA local_time       TYPE sy-uzeit.
      DATA utc_timestamp    TYPE timestamp.
      DATA timestamp_string TYPE string.

      timestamp_string = |{ timestamp }|.
      WHILE strlen( timestamp_string ) < 14.
        timestamp_string = |0{ timestamp_string }|.
      ENDWHILE.

      local_date = timestamp_string+0(8).
      local_time = timestamp_string+8(6).

      CONVERT DATE local_date TIME local_time
              INTO TIME STAMP utc_timestamp
              TIME ZONE sy-zonlo.

      timestamp_string = |{ utc_timestamp }|.
      WHILE strlen( timestamp_string ) < 14.
        timestamp_string = |0{ timestamp_string }|.
      ENDWHILE.

      result = |{ timestamp_string+0(4) }-{ timestamp_string+4(2) }-{ timestamp_string+6(2) }T{ timestamp_string+8(2) }:{ timestamp_string+10(2) }:{ timestamp_string+12(2) }Z|.
    ENDMETHOD.
  ENDCLASS.
