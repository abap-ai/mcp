"! <p class="shorttext synchronized">MCP List Prompts Response</p>
"! Implementation for the ListPromptsResult interface
CLASS zcl_mcp_resp_list_prompts DEFINITION
  PUBLIC FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES zif_mcp_internal.

    TYPES: BEGIN OF prompt_argument,
             name        TYPE string,
             description TYPE string,
             title       TYPE string,
             required    TYPE abap_bool,
           END OF prompt_argument.

    TYPES prompt_arguments TYPE STANDARD TABLE OF prompt_argument WITH KEY name.

    TYPES: BEGIN OF prompt,
             name        TYPE string,
             description TYPE string,
             arguments   TYPE prompt_arguments,
             title       TYPE string,
             meta        TYPE REF TO zif_mcp_ajson,
           END OF prompt.

    TYPES prompts     TYPE STANDARD TABLE OF prompt WITH KEY name.

    TYPES next_cursor TYPE string.

    "! <p class="shorttext synchronized">Set Prompts</p>
    "!
    "! @parameter prompts | <p class="shorttext synchronized">List of Prompts</p>
    METHODS set_prompts
      IMPORTING prompts TYPE prompts.

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
    DATA int_prompts     TYPE prompts.
    DATA int_next_cursor TYPE string.
    DATA int_meta        TYPE REF TO zif_mcp_ajson.
ENDCLASS.

CLASS zcl_mcp_resp_list_prompts IMPLEMENTATION.
  METHOD zif_mcp_internal~generate_json.
    result = zcl_mcp_ajson=>create_empty( ).
    " Create prompts array
    result->touch_array( '/prompts' ).

    " Add all prompts
    LOOP AT int_prompts ASSIGNING FIELD-SYMBOL(<prompt>).
      " Create a new prompt object
      DATA(prompt_index) = sy-tabix.

      " Add name (required)
      result->set( iv_path         = |/prompts/{ prompt_index }/name|
                   iv_val          = <prompt>-name
                   iv_ignore_empty = abap_false ).

      " Add description (optional)
      IF <prompt>-description IS NOT INITIAL.
        result->set( iv_path = |/prompts/{ prompt_index }/description|
                     iv_val  = <prompt>-description ).
      ENDIF.

      " Add title (optional)
      IF <prompt>-title IS NOT INITIAL.
        result->set( iv_path = |/prompts/{ prompt_index }/title|
                     iv_val  = <prompt>-title ).
      ENDIF.

      " Add meta data if available
      IF <prompt>-meta IS BOUND.
        result->set( iv_path = |/prompts/{ prompt_index }/_meta|
                     iv_val  = <prompt>-meta ).
      ENDIF.

      " Create arguments array if there are arguments
      IF <prompt>-arguments IS INITIAL.
        CONTINUE.
      ENDIF.

      result->touch_array( |/prompts/{ prompt_index }/arguments| ).

      " Add each argument
      LOOP AT <prompt>-arguments ASSIGNING FIELD-SYMBOL(<argument>).
        DATA(arg_index) = sy-tabix.

        " Add name (required)
        result->set( iv_path         = |/prompts/{ prompt_index }/arguments/{ arg_index }/name|
                     iv_val          = <argument>-name
                     iv_ignore_empty = abap_false ).

        " Add description (optional)
        IF <argument>-description IS NOT INITIAL.
          result->set( iv_path = |/prompts/{ prompt_index }/arguments/{ arg_index }/description|
                       iv_val  = <argument>-description ).
        ENDIF.

        " Add title (optional)
        IF <argument>-title IS NOT INITIAL.
          result->set( iv_path = |/prompts/{ prompt_index }/arguments/{ arg_index }/title|
                       iv_val  = <argument>-title ).
        ENDIF.

        " Add required flag (optional)
        IF <argument>-required = abap_true.
          result->set( iv_path = |/prompts/{ prompt_index }/arguments/{ arg_index }/required|
                       iv_val  = <argument>-required ).
        ENDIF.
      ENDLOOP.
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

  METHOD set_prompts.
    int_prompts = prompts.
  ENDMETHOD.

  METHOD set_next_cursor.
    int_next_cursor = next_cursor.
  ENDMETHOD.

  METHOD set_meta.
    int_meta = meta.
  ENDMETHOD.
ENDCLASS.
