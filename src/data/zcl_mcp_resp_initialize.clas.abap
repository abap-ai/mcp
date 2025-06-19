"! <p class="shorttext synchronized">MCP Initialize Response</p>
"! Note that we do not implement protocol version here as this
"! is implemented in the main MCP server class.
CLASS zcl_mcp_resp_initialize DEFINITION
  PUBLIC FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES zif_mcp_internal.

    TYPES: BEGIN OF capabilities,
             prompts   TYPE abap_bool,
             resources TYPE abap_bool,
             tools     TYPE abap_bool,
           END OF capabilities.

    TYPES: BEGIN OF implementation,
             name    TYPE string,
             version TYPE string,
           END OF implementation.

    TYPES instructions TYPE string.

    "! <p class="shorttext synchronized">Set Server Capabilities</p>
    "!
    "! @parameter capabilities | <p class="shorttext synchronized">Capabilities</p>
    METHODS set_capabilities
      IMPORTING capabilities TYPE capabilities.

    "! <p class="shorttext synchronized">Set Server Implementation Details/p>
    "!
    "! @parameter implementation | <p class="shorttext synchronized">Implementation</p>
    METHODS set_implementation
      IMPORTING !implementation TYPE implementation.

    "! <p class="shorttext synchronized">Set Server Instructions</p>
    "! This is an optional parameter.
    "!
    "! @parameter instructions | <p class="shorttext synchronized">Instructions</p>
    METHODS set_instructions
      IMPORTING instructions TYPE instructions.

    METHODS set_protocol_version
      IMPORTING protocol_version TYPE string.

  PRIVATE SECTION.
    DATA int_capabilities   TYPE capabilities.
    DATA int_implementation TYPE implementation.
    DATA int_instructions   TYPE instructions.
    DATA int_protocol_version TYPE string.
ENDCLASS.

CLASS zcl_mcp_resp_initialize IMPLEMENTATION.
  METHOD zif_mcp_internal~generate_json.
    result = zcl_mcp_ajson=>create_empty( ).

    " The base server logic will handle this but
    " as fallback we set the latest version as per specification.
    IF int_protocol_version IS NOT INITIAL.
      result->set( iv_path = 'protocolVersion'
                   iv_val  = int_protocol_version ).
    ELSE.
      result->set( iv_path = 'protocolVersion'
                   iv_val  = zif_mcp_constants=>latest_protocol_version ).
    ENDIF.

    result->touch_object( 'capabilities' ).

    IF int_capabilities-prompts = abap_true.
      result->touch_object( 'capabilities/prompts' ).
    ENDIF.

    IF int_capabilities-resources = abap_true.
      result->touch_object( 'capabilities/resources' ).
    ENDIF.

    IF int_capabilities-tools = abap_true.
      result->touch_object( 'capabilities/tools' ).
    ENDIF.

    result->set( iv_path         = 'serverInfo/name'
                 iv_val          = int_implementation-name
                 iv_ignore_empty = abap_false
                 iv_node_type    = zif_mcp_ajson_types=>node_type-string ).
    result->set( iv_path         = 'serverInfo/version'
                 iv_val          = int_implementation-version
                 iv_ignore_empty = abap_false
                 iv_node_type    = zif_mcp_ajson_types=>node_type-string ).

    IF int_instructions IS NOT INITIAL.
      result->set( iv_path         = 'instructions'
                   iv_val          = int_instructions
                   iv_ignore_empty = abap_false
                   iv_node_type    = zif_mcp_ajson_types=>node_type-string ).
    ENDIF.
  ENDMETHOD.

  METHOD set_capabilities.
    int_capabilities = capabilities.
  ENDMETHOD.

  METHOD set_implementation.
    int_implementation = implementation.
  ENDMETHOD.

  METHOD set_instructions.
    int_instructions = instructions.
  ENDMETHOD.

  METHOD set_protocol_version.
    int_protocol_version = protocol_version.
  ENDMETHOD.

ENDCLASS.
