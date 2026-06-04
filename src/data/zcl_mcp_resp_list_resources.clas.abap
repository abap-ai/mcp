"! <p class="shorttext synchronized">MCP List Resources Result</p>
"! Implementation for the ListResourcesResult interface
CLASS zcl_mcp_resp_list_resources DEFINITION
  PUBLIC FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES zif_mcp_internal.

    TYPES: BEGIN OF resource,
             uri         TYPE string,
             name        TYPE string,
             title       TYPE string,
             description TYPE string,
             mime_type   TYPE string,
             annotations TYPE zif_mcp_types=>annotations,
             size        TYPE i,
             icons       TYPE zif_mcp_types=>icon_list,
             meta        TYPE REF TO zif_mcp_ajson,
           END OF resource.

    TYPES resources   TYPE STANDARD TABLE OF resource WITH KEY uri.
    TYPES next_cursor TYPE zif_mcp_types=>page_cursor.

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
    DATA int_next_cursor TYPE zif_mcp_types=>page_cursor.
    DATA int_meta        TYPE REF TO zif_mcp_ajson.

ENDCLASS.

CLASS zcl_mcp_resp_list_resources IMPLEMENTATION.
  METHOD zif_mcp_internal~generate_json.
    FIELD-SYMBOLS <resource> LIKE LINE OF int_resources.
      DATA resource_index LIKE sy-tabix.
        FIELD-SYMBOLS <icon> LIKE LINE OF <resource>-icons.
          DATA icon_path TYPE string.
            FIELD-SYMBOLS <size> LIKE LINE OF <icon>-sizes.
          FIELD-SYMBOLS <audience> LIKE LINE OF <resource>-annotations-audience.
          DATA iso_timestamp TYPE string.
    result = zcl_mcp_ajson=>create_empty( ).
    " Create resources array
    result->touch_array( '/resources' ).

    " Add all resources
    
    LOOP AT int_resources ASSIGNING <resource>.
      
      resource_index = sy-tabix.

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

      " Add icons (optional, new in MCP 2025-11-25)
      IF <resource>-icons IS NOT INITIAL.
        result->touch_array( |/resources/{ resource_index }/icons| ).
        
        LOOP AT <resource>-icons ASSIGNING <icon>.
          
          icon_path = |/resources/{ resource_index }/icons/{ sy-tabix }|.
          result->set( iv_path = |{ icon_path }/src|
                       iv_val  = <icon>-src ).
          IF <icon>-mime_type IS NOT INITIAL.
            result->set( iv_path = |{ icon_path }/mimeType|
                         iv_val  = <icon>-mime_type ).
          ENDIF.
          IF <icon>-sizes IS NOT INITIAL.
            result->touch_array( |{ icon_path }/sizes| ).
            
            LOOP AT <icon>-sizes ASSIGNING <size>.
              result->set( iv_path = |{ icon_path }/sizes/{ sy-tabix }|
                           iv_val  = <size> ).
            ENDLOOP.
          ENDIF.
          IF <icon>-theme IS NOT INITIAL.
            result->set( iv_path = |{ icon_path }/theme|
                         iv_val  = <icon>-theme ).
          ENDIF.
        ENDLOOP.
      ENDIF.

      " Add annotations (optional)
      IF    <resource>-annotations-audience      IS NOT INITIAL
         OR <resource>-annotations-priority      IS NOT INITIAL
         OR <resource>-annotations-last_modified IS NOT INITIAL.

        " Add audience array if not empty
        IF <resource>-annotations-audience IS NOT INITIAL.
          result->touch_array( |/resources/{ resource_index }/annotations/audience| ).

          
          LOOP AT <resource>-annotations-audience ASSIGNING <audience>.
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
          
          iso_timestamp = zcl_mcp_util=>timestamp_to_iso8601( <resource>-annotations-last_modified ).
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
