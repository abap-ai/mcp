"! <p class="shorttext synchronized">MCP List Tools Response</p>
"! Implementation for the ListToolsResult interface
CLASS zcl_mcp_resp_list_tools DEFINITION
  PUBLIC FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES zif_mcp_internal.

    TYPES: BEGIN OF tool_annotations,
             title           TYPE string,
             readonlyhint    TYPE abap_bool,
             destructivehint TYPE abap_bool,
             idempotenthint  TYPE abap_bool,
             openworldhint   TYPE abap_bool,
           END OF tool_annotations.

    TYPES: BEGIN OF tool,
             name         TYPE string,
             description  TYPE string,
             input_schema TYPE REF TO zif_mcp_ajson,
             annotations  TYPE tool_annotations,
           END OF tool.

    "! Table of tools
    TYPES tools       TYPE STANDARD TABLE OF tool WITH KEY name.
    TYPES next_cursor TYPE string.

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

  PRIVATE SECTION.
    DATA int_tools      TYPE tools.
    DATA int_next_cursor TYPE string.
    DATA int_meta        TYPE REF TO zif_mcp_ajson.
ENDCLASS.

CLASS zcl_mcp_resp_list_tools IMPLEMENTATION.
  METHOD zif_mcp_internal~generate_json.
    FIELD-SYMBOLS <tool> LIKE LINE OF int_tools.
      DATA tool_index LIKE sy-tabix.
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

      " Add annotations (optional)
      IF    <tool>-annotations-title           IS NOT INITIAL
         OR <tool>-annotations-readonlyhint     = abap_true
         OR <tool>-annotations-destructivehint  = abap_true
         OR <tool>-annotations-idempotenthint   = abap_true
         OR <tool>-annotations-openworldhint    = abap_true.

        " Add title if not empty
        IF <tool>-annotations-title IS NOT INITIAL.
          result->set( iv_path = |/tools/{ tool_index }/annotations/title|
                      iv_val  = <tool>-annotations-title ).
        ENDIF.

        " Add readOnlyHint if true
        IF <tool>-annotations-readonlyhint = abap_true.
          result->set( iv_path = |/tools/{ tool_index }/annotations/readOnlyHint|
                      iv_val  = <tool>-annotations-readonlyhint ).
        ENDIF.

        " Add destructiveHint if true
        IF <tool>-annotations-destructivehint = abap_true.
          result->set( iv_path = |/tools/{ tool_index }/annotations/destructiveHint|
                      iv_val  = <tool>-annotations-destructivehint ).
        ENDIF.

        " Add idempotentHint if true
        IF <tool>-annotations-idempotenthint = abap_true.
          result->set( iv_path = |/tools/{ tool_index }/annotations/idempotentHint|
                      iv_val  = <tool>-annotations-idempotenthint ).
        ENDIF.

        " Add openWorldHint if true
        IF <tool>-annotations-openworldhint = abap_true.
          result->set( iv_path = |/tools/{ tool_index }/annotations/openWorldHint|
                      iv_val  = <tool>-annotations-openworldhint ).
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

  METHOD set_tools.
    int_tools = tools.
  ENDMETHOD.

  METHOD set_next_cursor.
    int_next_cursor = next_cursor.
  ENDMETHOD.

  METHOD set_meta.
    int_meta = meta.
  ENDMETHOD.
ENDCLASS.
