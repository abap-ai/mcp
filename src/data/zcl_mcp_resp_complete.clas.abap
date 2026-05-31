"! <p class="shorttext synchronized">MCP Completion Result</p>
  "! Implementation for the CompleteResult interface (MCP 2025-11-25)
  CLASS zcl_mcp_resp_complete DEFINITION
    PUBLIC FINAL
    CREATE PUBLIC.

    PUBLIC SECTION.
      INTERFACES zif_mcp_internal.

      TYPES completion_values TYPE STANDARD TABLE OF string WITH EMPTY KEY.

      "! <p class="shorttext synchronized">Add a single completion candidate</p>
      "!
      "! @parameter value | <p class="shorttext synchronized">Completion candidate string</p>
      METHODS add_value
        IMPORTING !value TYPE string.

      "! <p class="shorttext synchronized">Replace the full candidate list</p>
      "!
      "! @parameter values | <p class="shorttext synchronized">List of completion candidates</p>
      METHODS set_values
        IMPORTING values TYPE completion_values.

      "! <p class="shorttext synchronized">Set total candidate count</p>
      "! Optional. Total number of matches on the server, may exceed this page.
      "!
      "! @parameter total | <p class="shorttext synchronized">Total candidate count</p>
      METHODS set_total
        IMPORTING total TYPE i.

      "! <p class="shorttext synchronized">Signal additional candidates exist</p>
      "! Optional. Set to true when more results are available beyond this page.
      "!
      "! @parameter has_more | <p class="shorttext synchronized">True if more candidates exist</p>
      METHODS set_has_more
        IMPORTING has_more TYPE abap_bool.

      "! <p class="shorttext synchronized">Set meta data</p>
      "! Optional metadata to attach to the response.
      "!
      "! @parameter meta | <p class="shorttext synchronized">Meta data JSON object</p>
      METHODS set_meta
        IMPORTING meta TYPE REF TO zif_mcp_ajson.

    PRIVATE SECTION.
      DATA int_values   TYPE completion_values.
      DATA int_total    TYPE i.
      DATA int_has_more TYPE abap_bool.
      DATA int_meta     TYPE REF TO zif_mcp_ajson.
  ENDCLASS.

  CLASS zcl_mcp_resp_complete IMPLEMENTATION.
    METHOD zif_mcp_internal~generate_json.
      result = zcl_mcp_ajson=>create_empty( ).
      result->touch_array( '/completion/values' ).
      LOOP AT int_values INTO DATA(val).
        result->set( iv_path = |/completion/values/{ sy-tabix }|
                     iv_val  = val ).
      ENDLOOP.
      IF int_total > 0.
        result->set_integer( iv_path = '/completion/total'
                             iv_val  = int_total ).
      ENDIF.
      IF int_has_more = abap_true.
        result->set( iv_path = '/completion/hasMore'
                     iv_val  = abap_true ).
      ENDIF.
      IF int_meta IS BOUND.
        result->set( iv_path = '/_meta'
                     iv_val  = int_meta ).
      ENDIF.
    ENDMETHOD.

    METHOD add_value.    APPEND value TO int_values.  ENDMETHOD.
    METHOD set_values.   int_values   = values.        ENDMETHOD.
    METHOD set_total.    int_total    = total.          ENDMETHOD.
    METHOD set_has_more. int_has_more = has_more.       ENDMETHOD.
    METHOD set_meta.     int_meta     = meta.           ENDMETHOD.
  ENDCLASS.
