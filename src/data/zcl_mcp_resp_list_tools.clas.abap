"! <p class="shorttext synchronized">MCP List Tools Response</p>
"! Implementation for the ListToolsResult interface
CLASS zcl_mcp_resp_list_tools DEFINITION
  PUBLIC FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES zif_mcp_internal.

    CONSTANTS: BEGIN OF task_support,
                 forbidden TYPE string VALUE 'forbidden',
                 optional  TYPE string VALUE 'optional',
                 required  TYPE string VALUE 'required',
               END OF task_support.

    TYPES: BEGIN OF execution,
             task_support TYPE string,
           END OF execution.

    TYPES: BEGIN OF tool_annotations,
             title               TYPE string,
             readonlyhint        TYPE abap_bool,
             destructivehint     TYPE abap_bool,
             destructivehint_set TYPE abap_bool,
             idempotenthint      TYPE abap_bool,
             openworldhint       TYPE abap_bool,
             openworldhint_set   TYPE abap_bool,
           END OF tool_annotations.

    TYPES: BEGIN OF tool,
             name          TYPE string,
             description   TYPE string,
             title         TYPE string,
             input_schema  TYPE REF TO zif_mcp_ajson,
             output_schema TYPE REF TO zif_mcp_ajson,
             annotations   TYPE tool_annotations,
             execution     TYPE execution,
             icons         TYPE zif_mcp_types=>icon_list,
             meta          TYPE REF TO zif_mcp_ajson,
           END OF tool.

    "! Table of tools
    TYPES tools       TYPE STANDARD TABLE OF tool WITH KEY name.
    TYPES next_cursor TYPE zif_mcp_types=>page_cursor.

    "! <p class="shorttext synchronized">Set Tools</p>
    "!
    "! @parameter tools | <p class="shorttext synchronized">List of Tools</p>
    METHODS set_tools
      IMPORTING tools TYPE tools.

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

    "! <p class="shorttext synchronized">Return tools</p>
    "!
    "! @parameter result | <p class="shorttext synchronized">Tools</p>
    METHODS get_tools
      RETURNING VALUE(result) TYPE tools.

    "! <p class="shorttext synchronized" lang="en">Get next cursor</p>
    "!
    "! @parameter result | <p class="shorttext synchronized" lang="en">Cursor</p>
    METHODS get_next_cursor
      RETURNING VALUE(result) TYPE next_cursor.

  PRIVATE SECTION.
    DATA int_tools       TYPE tools.
    DATA int_next_cursor TYPE zif_mcp_types=>page_cursor.
    DATA int_meta        TYPE REF TO zif_mcp_ajson.
ENDCLASS.

CLASS zcl_mcp_resp_list_tools IMPLEMENTATION.
  METHOD zif_mcp_internal~generate_json.
    FIELD-SYMBOLS <tool> LIKE LINE OF int_tools.
      DATA tool_index LIKE sy-tabix.
        FIELD-SYMBOLS <icon> LIKE LINE OF <tool>-icons.
          DATA icon_path TYPE string.
            FIELD-SYMBOLS <size> LIKE LINE OF <icon>-sizes.
    result = zcl_mcp_ajson=>create_empty( ).

    " Create tools array
    result->touch_array( '/tools' ).

    " Add all tools
    
    LOOP AT int_tools ASSIGNING <tool>.
      
      tool_index = sy-tabix.

      " Add name (required)
      result->set( iv_path         = |/tools/{ tool_index }/name|
                   iv_val          = <tool>-name
                   iv_ignore_empty = abap_false ).

      " Add description (optional)
      IF <tool>-description IS NOT INITIAL.
        result->set( iv_path = |/tools/{ tool_index }/description|
                     iv_val  = <tool>-description ).
      ENDIF.

      " Add title (optional)
      IF <tool>-title IS NOT INITIAL.
        result->set( iv_path = |/tools/{ tool_index }/title|
                     iv_val  = <tool>-title ).
      ENDIF.

      " Add input schema (required)
      IF <tool>-input_schema IS BOUND.
        result->set( iv_path = |/tools/{ tool_index }/inputSchema|
                     iv_val  = <tool>-input_schema ).
      ELSE.
        " Create a minimal schema if none provided
        result->set( iv_path         = |/tools/{ tool_index }/inputSchema/type|
                     iv_val          = 'object'
                     iv_ignore_empty = abap_false ).
      ENDIF.

      " Add meta (optional)
      IF <tool>-meta IS BOUND.
        result->set( iv_path = |/tools/{ tool_index }/_meta|
                     iv_val  = <tool>-meta ).
      ENDIF.

      " Add output schema (optional)
      IF <tool>-output_schema IS BOUND.
        result->set( iv_path = |/tools/{ tool_index }/outputSchema|
                     iv_val  = <tool>-output_schema ).
      ENDIF.

      " Add annotations (optional)
      IF    <tool>-annotations-title               IS NOT INITIAL
         OR <tool>-annotations-readonlyhint         = abap_true
         OR <tool>-annotations-destructivehint      = abap_true
         OR <tool>-annotations-destructivehint_set  = abap_true
         OR <tool>-annotations-idempotenthint       = abap_true
         OR <tool>-annotations-openworldhint        = abap_true
         OR <tool>-annotations-openworldhint_set    = abap_true.

        " Add title if not empty
        IF <tool>-annotations-title IS NOT INITIAL.
          result->set( iv_path = |/tools/{ tool_index }/annotations/title|
                       iv_val  = <tool>-annotations-title ).
        ENDIF.

        " readOnlyHint - spec default false, emit only when true
        IF <tool>-annotations-readonlyhint = abap_true.
          result->set( iv_path = |/tools/{ tool_index }/annotations/readOnlyHint|
                       iv_val  = abap_true ).
        ENDIF.

        " destructiveHint - spec default true; emit explicit override or legacy abap_true
        IF <tool>-annotations-destructivehint_set = abap_true.
          result->set( iv_path = |/tools/{ tool_index }/annotations/destructiveHint|
                       iv_val  = <tool>-annotations-destructivehint ).
        ELSEIF <tool>-annotations-destructivehint = abap_true.
          result->set( iv_path = |/tools/{ tool_index }/annotations/destructiveHint|
                       iv_val  = abap_true ).
        ENDIF.

        " idempotentHint - spec default false, emit only when true
        IF <tool>-annotations-idempotenthint = abap_true.
          result->set( iv_path = |/tools/{ tool_index }/annotations/idempotentHint|
                       iv_val  = abap_true ).
        ENDIF.

        " openWorldHint - spec default true; emit explicit override or legacy abap_true
        IF <tool>-annotations-openworldhint_set = abap_true.
          result->set( iv_path = |/tools/{ tool_index }/annotations/openWorldHint|
                       iv_val  = <tool>-annotations-openworldhint ).
        ELSEIF <tool>-annotations-openworldhint = abap_true.
          result->set( iv_path = |/tools/{ tool_index }/annotations/openWorldHint|
                       iv_val  = abap_true ).
        ENDIF.
      ENDIF.

      " Add execution (optional, new in MCP 2025-11-25)
      " Omit entirely when initial - absent execution means forbidden by default.
      IF <tool>-execution-task_support IS NOT INITIAL.
        result->set( iv_path = |/tools/{ tool_index }/execution/taskSupport|
                     iv_val  = <tool>-execution-task_support ).
      ENDIF.

      " Add icons (optional, new in MCP 2025-11-25)
      IF <tool>-icons IS NOT INITIAL.
        result->touch_array( |/tools/{ tool_index }/icons| ).
        
        LOOP AT <tool>-icons ASSIGNING <icon>.
          
          icon_path = |/tools/{ tool_index }/icons/{ sy-tabix }|.
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

  METHOD set_tools.
    int_tools = tools.
  ENDMETHOD.

  METHOD set_next_cursor.
    int_next_cursor = next_cursor.
  ENDMETHOD.

  METHOD set_meta.
    int_meta = meta.
  ENDMETHOD.

  METHOD get_tools.
    result = int_tools.
  ENDMETHOD.

  METHOD get_next_cursor.
    result = int_next_cursor.
  ENDMETHOD.
ENDCLASS.
