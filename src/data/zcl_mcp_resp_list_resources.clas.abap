"! <p class="shorttext synchronized">MCP List Resources Result</p>
"! Implementation for the ListResourcesResult interface
CLASS zcl_mcp_resp_list_resources DEFINITION
  PUBLIC FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES zif_mcp_internal.

    TYPES: BEGIN OF annotations,
             audience      TYPE STANDARD TABLE OF string WITH EMPTY KEY,
             priority      TYPE decfloat16,
             last_modified TYPE timestamp,
           END OF annotations.

    TYPES: BEGIN OF resource,
             uri         TYPE string,
             name        TYPE string,
             title       TYPE string,
             description TYPE string,
             mime_type   TYPE string,
             annotations TYPE annotations,
             size        TYPE i,
             meta        TYPE REF TO zif_mcp_ajson,
           END OF resource.

    TYPES resources   TYPE STANDARD TABLE OF resource WITH KEY uri.
    TYPES next_cursor TYPE string.

    "! <p class="shorttext synchronized">Set Resources</p>
    "!
    "! @parameter resources | <p class="shorttext synchronized">List of Resources</p>
    METHODS set_resources
      IMPORTING resources TYPE resources.

    "! <p class="shorttext synchronized">Set Next Cursor</p>
    "! This is an optional parameter for pagination
    "!
    "! @parameter next_cursor | <p class="shorttext synchronized">Pagination cursor</p>
    METHODS set_next_cursor
      IMPORTING next_cursor TYPE next_cursor OPTIONAL.

    "! <p class="shorttext synchronized">Set Meta Data</p>
    "! Optional metadata to attach to response
    "!
    "! @parameter meta | <p class="shorttext synchronized">Meta data</p>
    METHODS set_meta
      IMPORTING meta TYPE REF TO zif_mcp_ajson.

  PRIVATE SECTION.
    DATA int_resources   TYPE resources.
    DATA int_next_cursor TYPE string.
    DATA int_meta        TYPE REF TO zif_mcp_ajson.

    METHODS convert_timestamp_to_iso8601
      IMPORTING timestamp     TYPE timestamp
      RETURNING VALUE(result) TYPE string.
ENDCLASS.

CLASS zcl_mcp_resp_list_resources IMPLEMENTATION.
  METHOD zif_mcp_internal~generate_json.
    result = zcl_mcp_ajson=>create_empty( ).
    " Create resources array
    result->touch_array( '/resources' ).

    " Add all resources
    LOOP AT int_resources ASSIGNING FIELD-SYMBOL(<resource>).
      DATA(resource_index) = sy-tabix.

      " Add uri (required)
      result->set( iv_path         = |/resources/{ resource_index }/uri|
                   iv_val          = <resource>-uri
                   iv_ignore_empty = abap_false ).

      " Add name (required)
      result->set( iv_path         = |/resources/{ resource_index }/name|
                   iv_val          = <resource>-name
                   iv_ignore_empty = abap_false ).

      " Add title (optional)
      IF <resource>-title IS NOT INITIAL.
        result->set( iv_path = |/resources/{ resource_index }/title|
                     iv_val  = <resource>-title ).
      ENDIF.

      " Add description (optional)
      IF <resource>-description IS NOT INITIAL.
        result->set( iv_path = |/resources/{ resource_index }/description|
                     iv_val  = <resource>-description ).
      ENDIF.

      " Add mimeType (optional)
      IF <resource>-mime_type IS NOT INITIAL.
        result->set( iv_path = |/resources/{ resource_index }/mimeType|
                     iv_val  = <resource>-mime_type ).
      ENDIF.

      " Add size (optional)
      IF <resource>-size IS NOT INITIAL.
        result->set( iv_path = |/resources/{ resource_index }/size|
                     iv_val  = <resource>-size ).
      ENDIF.

      " Add meta (optional)
      IF <resource>-meta IS BOUND.
        " Create the '_meta' node in the resulting JSON
        result->set( iv_path = |/resources/{ resource_index }/_meta|
                     iv_val  = <resource>-meta ).
      ENDIF.

      " Add annotations (optional)
      IF    <resource>-annotations-audience      IS NOT INITIAL
         OR <resource>-annotations-priority      IS NOT INITIAL
         OR <resource>-annotations-last_modified IS NOT INITIAL.

        " Add audience array if not empty
        IF <resource>-annotations-audience IS NOT INITIAL.
          result->touch_array( |/resources/{ resource_index }/annotations/audience| ).

          LOOP AT <resource>-annotations-audience ASSIGNING FIELD-SYMBOL(<audience>).
            result->set( iv_path = |/resources/{ resource_index }/annotations/audience/{ sy-tabix }|
                         iv_val  = <audience> ).
          ENDLOOP.
        ENDIF.

        " Add priority if not empty
        IF <resource>-annotations-priority IS NOT INITIAL.
          result->set( iv_path = |/resources/{ resource_index }/annotations/priority|
                       iv_val  = <resource>-annotations-priority ).
        ENDIF.

        " Add lastModified if not empty
        IF <resource>-annotations-last_modified IS NOT INITIAL.
          DATA(iso_timestamp) = convert_timestamp_to_iso8601( <resource>-annotations-last_modified ).
          result->set( iv_path = |/resources/{ resource_index }/annotations/lastModified|
                       iv_val  = iso_timestamp ).
        ENDIF.
      ENDIF.
    ENDLOOP.

    " Add nextCursor (optional)
    IF int_next_cursor IS NOT INITIAL.
      result->set( iv_path = '/nextCursor'
                   iv_val  = int_next_cursor ).
    ENDIF.

    " Add metadata (optional)
    IF int_meta IS BOUND.
      " Create the '_meta' node in the resulting JSON
      result->set( iv_path = '/_meta'
                   iv_val  = int_meta ).
    ENDIF.
  ENDMETHOD.

  METHOD convert_timestamp_to_iso8601.
    " Convert session timestamp to UTC and format as ISO 8601
    DATA local_date       TYPE sy-datum.
    DATA local_time       TYPE sy-uzeit.
    DATA utc_timestamp    TYPE timestamp.
    DATA timestamp_string TYPE string.

    " Convert timestamp to string first
    timestamp_string = |{ timestamp }|.

    " Pad with leading zeros if needed
    WHILE strlen( timestamp_string ) < 14.
      timestamp_string = |0{ timestamp_string }|.
    ENDWHILE.

    " Extract date and time from timestamp string
    local_date = timestamp_string+0(8).
    local_time = timestamp_string+8(6).

    " Convert local date/time to UTC timestamp
    CONVERT DATE local_date TIME local_time INTO TIME STAMP utc_timestamp TIME ZONE sy-zonlo.

    " Convert UTC timestamp back to string for formatting
    timestamp_string = |{ utc_timestamp }|.
    WHILE strlen( timestamp_string ) < 14.
      timestamp_string = |0{ timestamp_string }|.
    ENDWHILE.

    " Format: YYYYMMDDHHMMSS -> YYYY-MM-DDTHH:MM:SSZ
    result = |{ timestamp_string+0(4) }-{ timestamp_string+4(2) }-{ timestamp_string+6(2) }T{ timestamp_string+8(2) }:{ timestamp_string+10(2) }:{ timestamp_string+12(2) }Z|.
  ENDMETHOD.

  METHOD set_resources.
    int_resources = resources.
  ENDMETHOD.

  METHOD set_next_cursor.
    int_next_cursor = next_cursor.
  ENDMETHOD.

  METHOD set_meta.
    int_meta = meta.
  ENDMETHOD.
ENDCLASS.
