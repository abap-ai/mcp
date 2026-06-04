"! <p class="shorttext synchronized">MCP Initialize Response</p>
"! Note that we do not implement protocol version here as this
"! is implemented in the main MCP server class.
CLASS zcl_mcp_resp_initialize DEFINITION
  PUBLIC FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES zif_mcp_internal.

    TYPES: BEGIN OF prompts_capability,
             enabled      TYPE abap_bool,
             list_changed TYPE abap_bool,
           END OF prompts_capability.

    TYPES: BEGIN OF resources_capability,
             enabled      TYPE abap_bool,
             subscribe    TYPE abap_bool,
             list_changed TYPE abap_bool,
           END OF resources_capability.

    TYPES: BEGIN OF tools_capability,
             enabled      TYPE abap_bool,
             list_changed TYPE abap_bool,
           END OF tools_capability.

    " tools_call maps to tasks/requests/tools/call: {}
    " which signals clients that tools/call accepts task: {...}
    TYPES: BEGIN OF tasks_capability,
             list       TYPE abap_bool,
             cancel     TYPE abap_bool,
             tools_call TYPE abap_bool,
           END OF tasks_capability.

    TYPES: BEGIN OF capabilities,
             prompts     TYPE prompts_capability,
             resources   TYPE resources_capability,
             tools       TYPE tools_capability,
             tasks       TYPE tasks_capability,
             logging     TYPE abap_bool,
             completions TYPE abap_bool,
           END OF capabilities.

    TYPES: BEGIN OF implementation,
             name        TYPE string,
             version     TYPE string,
             title       TYPE string,
             description TYPE string,
             website_url TYPE string,
             icons       TYPE zif_mcp_types=>icon_list,
           END OF implementation.

    TYPES instructions TYPE string.

    "! <p class="shorttext synchronized">Set Server Capabilities</p>
    "!
    "! @parameter capabilities | <p class="shorttext synchronized">Capabilities</p>
    METHODS set_capabilities
      IMPORTING capabilities TYPE capabilities.

    "! <p class="shorttext synchronized">Set Server Implementation Details</p>
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

    "! <p class="shorttext synchronized">Set Meta Data</p>
    "! Optional metadata to attach to response
    "!
    "! @parameter meta | <p class="shorttext synchronized">Meta data</p>
    METHODS set_meta
      IMPORTING meta TYPE REF TO zif_mcp_ajson.

    METHODS get_capabilities
      RETURNING VALUE(result) TYPE capabilities.

    METHODS set_protocol_version
      IMPORTING protocol_version TYPE string.

  PRIVATE SECTION.
    DATA int_capabilities     TYPE capabilities.
    DATA int_implementation   TYPE implementation.
    DATA int_instructions     TYPE instructions.
    DATA int_protocol_version TYPE string.
    DATA meta                 TYPE REF TO zif_mcp_ajson.
ENDCLASS.

CLASS zcl_mcp_resp_initialize IMPLEMENTATION.
  METHOD zif_mcp_internal~generate_json.
    result = zcl_mcp_ajson=>create_empty( ).

    " Protocol version - base server logic sets this normally;
    DATA(effective_protocol_version) = COND string(
      WHEN int_protocol_version IS NOT INITIAL
      THEN int_protocol_version
      ELSE zif_mcp_constants=>latest_protocol_version ).

    result->set( iv_path = 'protocolVersion'
                 iv_val  = effective_protocol_version ).

    result->touch_object( 'capabilities' ).

    " Prompts capability
    IF int_capabilities-prompts-enabled = abap_true.
      result->touch_object( 'capabilities/prompts' ).
      " listChanged requires server-to-client notifications, unsupported in ABAP HTTP transport.
    ENDIF.

    " Resources capability
    IF int_capabilities-resources-enabled = abap_true.
      result->touch_object( 'capabilities/resources' ).
      " subscribe and listChanged require server-to-client notifications,
      " unsupported in ABAP HTTP transport
    ENDIF.

    " Tools capability
    IF int_capabilities-tools-enabled = abap_true.
      result->touch_object( 'capabilities/tools' ).
      " listChanged requires server-to-client notifications, unsupported in ABAP HTTP transport..
    ENDIF.

    " Tasks capability - new in MCP 2025-11-25
    IF     effective_protocol_version >= zif_mcp_constants=>protocol_version_2025_11_25
       AND (    int_capabilities-tasks-list       = abap_true
             OR int_capabilities-tasks-cancel     = abap_true
             OR int_capabilities-tasks-tools_call = abap_true ).
      result->touch_object( 'capabilities/tasks' ).
      IF int_capabilities-tasks-list = abap_true.
        result->touch_object( 'capabilities/tasks/list' ).
      ENDIF.
      IF int_capabilities-tasks-cancel = abap_true.
        result->touch_object( 'capabilities/tasks/cancel' ).
      ENDIF.
      IF int_capabilities-tasks-tools_call = abap_true.
        result->touch_object( 'capabilities/tasks/requests/tools/call' ).
      ENDIF.
    ENDIF.

    " Logging capability
    " logging notifications are unsupported without SSE/server-to-client delivery.

    " Completions capability
    IF int_capabilities-completions = abap_true.
      result->touch_object( 'capabilities/completions' ).
    ENDIF.

    " Server info - name and version are mandatory
    result->set( iv_path         = 'serverInfo/name'
                 iv_val          = int_implementation-name
                 iv_ignore_empty = abap_false
                 iv_node_type    = zif_mcp_ajson_types=>node_type-string ).
    result->set( iv_path         = 'serverInfo/version'
                 iv_val          = int_implementation-version
                 iv_ignore_empty = abap_false
                 iv_node_type    = zif_mcp_ajson_types=>node_type-string ).

    " Optional server info fields - new in MCP 2025-11-25
    IF int_implementation-title IS NOT INITIAL.
      result->set( iv_path      = 'serverInfo/title'
                   iv_val       = int_implementation-title
                   iv_node_type = zif_mcp_ajson_types=>node_type-string ).
    ENDIF.
    IF int_implementation-description IS NOT INITIAL.
      result->set( iv_path      = 'serverInfo/description'
                   iv_val       = int_implementation-description
                   iv_node_type = zif_mcp_ajson_types=>node_type-string ).
    ENDIF.
    IF int_implementation-website_url IS NOT INITIAL.
      result->set( iv_path      = 'serverInfo/websiteUrl'
                   iv_val       = int_implementation-website_url
                   iv_node_type = zif_mcp_ajson_types=>node_type-string ).
    ENDIF.

    " Icons - new in MCP 2025-11-25
    IF int_implementation-icons IS NOT INITIAL.
      result->touch_array( 'serverInfo/icons' ).
      LOOP AT int_implementation-icons ASSIGNING FIELD-SYMBOL(<icon>).
        DATA(icon_path) = |serverInfo/icons/{ sy-tabix }|.
        result->set( iv_path      = |{ icon_path }/src|
                     iv_val       = <icon>-src
                     iv_node_type = zif_mcp_ajson_types=>node_type-string ).
        IF <icon>-mime_type IS NOT INITIAL.
          result->set( iv_path = |{ icon_path }/mimeType|
                       iv_val  = <icon>-mime_type ).
        ENDIF.
        IF <icon>-sizes IS NOT INITIAL.
          result->touch_array( |{ icon_path }/sizes| ).
          LOOP AT <icon>-sizes ASSIGNING FIELD-SYMBOL(<size>).
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

    IF int_instructions IS NOT INITIAL.
      result->set( iv_path         = 'instructions'
                   iv_val          = int_instructions
                   iv_ignore_empty = abap_false
                   iv_node_type    = zif_mcp_ajson_types=>node_type-string ).
    ENDIF.

    IF meta IS BOUND.
      result->set( iv_path = '/_meta'
                   iv_val  = meta ).
    ENDIF.
  ENDMETHOD.

  METHOD get_capabilities.
    result = int_capabilities.
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

  METHOD set_meta.
    me->meta = meta.
  ENDMETHOD.

ENDCLASS.
