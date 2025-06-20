"! <p class="shorttext synchronized">MCP List Resource Templates Result</p>
"! Implementation for the ListResourceTemplatesResult interface
CLASS zcl_mcp_resp_list_res_tmpl DEFINITION
  PUBLIC FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES zif_mcp_internal.

    TYPES: BEGIN OF annotations,
             audience TYPE STANDARD TABLE OF string WITH EMPTY KEY,
             priority TYPE decfloat16,
           END OF annotations.

    TYPES: BEGIN OF resource_template,
             uritemplate TYPE string,
             name        TYPE string,
             title       TYPE string,
             description TYPE string,
             mime_type   TYPE string,
             annotations TYPE annotations,
             meta        TYPE REF TO zif_mcp_ajson,
           END OF resource_template.

    TYPES resource_templates TYPE STANDARD TABLE OF resource_template WITH KEY uritemplate.
    TYPES next_cursor        TYPE string.

    "! <p class="shorttext synchronized">Set Resource Templates</p>
    "!
    "! @parameter resource_templates | <p class="shorttext synchronized">List of Resource Templates</p>
    METHODS set_resource_templates
      IMPORTING resource_templates TYPE resource_templates.

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
    DATA int_resource_templates TYPE resource_templates.
    DATA int_next_cursor        TYPE string.
    DATA int_meta               TYPE REF TO zif_mcp_ajson.
ENDCLASS.



CLASS ZCL_MCP_RESP_LIST_RES_TMPL IMPLEMENTATION.


  METHOD set_meta.
    int_meta = meta.
  ENDMETHOD.


  METHOD set_next_cursor.
    int_next_cursor = next_cursor.
  ENDMETHOD.


  METHOD set_resource_templates.
    int_resource_templates = resource_templates.
  ENDMETHOD.

  METHOD zif_mcp_internal~generate_json.
    result = zcl_mcp_ajson=>create_empty( ).
    " Create resourceTemplates array

    result->touch_array( '/resourceTemplates' ).

    " Add all resource templates
    LOOP AT int_resource_templates ASSIGNING FIELD-SYMBOL(<template>).
      DATA(template_index) = sy-tabix.

      " Add uriTemplate (required)
      result->set( iv_path         = |/resourceTemplates/{ template_index }/uriTemplate|
                   iv_val          = <template>-uritemplate
                   iv_ignore_empty = abap_false ).

      " Add name (required)
      result->set( iv_path         = |/resourceTemplates/{ template_index }/name|
                   iv_val          = <template>-name
                   iv_ignore_empty = abap_false ).

      " Add description (optional)
      IF <template>-description IS NOT INITIAL.
        result->set( iv_path = |/resourceTemplates/{ template_index }/description|
                     iv_val  = <template>-description ).
      ENDIF.

      " Add title (optional)
      IF <template>-title IS NOT INITIAL.
        result->set( iv_path = |/resourceTemplates/{ template_index }/title|
                     iv_val  = <template>-title ).
      ENDIF.

      " Add mimeType (optional)
      IF <template>-mime_type IS NOT INITIAL.
        result->set( iv_path = |/resourceTemplates/{ template_index }/mimeType|
                     iv_val  = <template>-mime_type ).
      ENDIF.

      " Add meta data (optional)
      IF <template>-meta IS BOUND.
        " Create the '_meta' node in the resulting JSON
        result->set( iv_path = |/resourceTemplates/{ template_index }/_meta|
                     iv_val  = <template>-meta ).
      ENDIF.

      " Add annotations (optional)
      IF <template>-annotations-audience IS NOT INITIAL OR <template>-annotations-priority IS NOT INITIAL.
        " Add audience array if not empty
        IF <template>-annotations-audience IS NOT INITIAL.
          result->touch_array( |/resourceTemplates/{ template_index }/annotations/audience| ).

          LOOP AT <template>-annotations-audience ASSIGNING FIELD-SYMBOL(<audience>).
            result->set( iv_path = |/resourceTemplates/{ template_index }/annotations/audience/{ sy-tabix }|
                         iv_val  = <audience> ).
          ENDLOOP.
        ENDIF.

        " Add priority if not empty
        IF <template>-annotations-priority IS NOT INITIAL.
          result->set( iv_path = |/resourceTemplates/{ template_index }/annotations/priority|
                       iv_val  = <template>-annotations-priority ).
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
ENDCLASS.
