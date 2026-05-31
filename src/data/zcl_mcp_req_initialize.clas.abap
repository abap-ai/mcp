CLASS zcl_mcp_req_initialize DEFINITION
  PUBLIC FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    TYPES: BEGIN OF implementation,
             name        TYPE string,
             version     TYPE string,
             title       TYPE string,
             description TYPE string,
             website_url TYPE string,
           END OF implementation.

    TYPES: BEGIN OF roots_capability,
             list_changed TYPE abap_bool,
           END OF roots_capability.

    TYPES: BEGIN OF client_capabilities,
             roots TYPE roots_capability,
           END OF client_capabilities.

    "! <p class="shorttext synchronized">Constructor</p>
    "! Parses the Initialize Request JSON (MCP 2025-11-25)
    "!
    "! @parameter json | <p class="shorttext synchronized">JSON Request</p>
    METHODS constructor
      IMPORTING json TYPE REF TO zif_mcp_ajson
      RAISING   zcx_mcp_ajson_error.

    "! <p class="shorttext synchronized">Get Protocol Version</p>
    "!
    "! @parameter result | <p class="shorttext synchronized">Protocol Version</p>
    METHODS get_protocol_version
      RETURNING VALUE(result) TYPE string.

    "! <p class="shorttext synchronized">Get Client Capabilities</p>
    "!
    "! @parameter result | <p class="shorttext synchronized">Client Capabilities</p>
    METHODS get_capabilities
      RETURNING VALUE(result) TYPE client_capabilities.

    "! <p class="shorttext synchronized">Get Client Info</p>
    "! Now includes optional description and websiteUrl (MCP 2025-11-25).
    "!
    "! @parameter result | <p class="shorttext synchronized">Client Info</p>
    METHODS get_client_info
      RETURNING VALUE(result) TYPE implementation.

    "! <p class="shorttext synchronized">Check if roots capability exists</p>
    "!
    "! @parameter result | <p class="shorttext synchronized">True if exists</p>
    METHODS has_roots_capability
      RETURNING VALUE(result) TYPE abap_bool.

    "! <p class="shorttext synchronized">Check if sampling capability exists</p>
    "!
    "! @parameter result | <p class="shorttext synchronized">True if exists</p>
    METHODS has_sampling_capability
      RETURNING VALUE(result) TYPE abap_bool.

    "! <p class="shorttext synchronized">Check if elicitation capability exists</p>
    "! New in MCP 2025-11-25. The server does not send elicitation requests,
    "! but this tells you what the client has declared.
    "!
    "! @parameter result | <p class="shorttext synchronized">True if exists</p>
    METHODS has_elicitation_capability
      RETURNING VALUE(result) TYPE abap_bool.

    "! <p class="shorttext synchronized">Check if tasks capability exists</p>
    "! New in MCP 2025-11-25. Required for task-augmented tool execution.
    "!
    "! @parameter result | <p class="shorttext synchronized">True if exists</p>
    METHODS has_tasks_capability
      RETURNING VALUE(result) TYPE abap_bool.

    "! <p class="shorttext synchronized">Check if experimental capability exists</p>
    "!
    "! @parameter result | <p class="shorttext synchronized">True if exists</p>
    METHODS has_experimental_capability
      RETURNING VALUE(result) TYPE abap_bool.

    "! <p class="shorttext synchronized">Get sampling capability JSON</p>
    "! Contains optional context and tools sub-objects (MCP 2025-11-25).
    "!
    "! @parameter result | <p class="shorttext synchronized">JSON object</p>
    METHODS get_sampling_json
      RETURNING VALUE(result) TYPE REF TO zif_mcp_ajson.

    "! <p class="shorttext synchronized">Get elicitation capability JSON</p>
    "! New in MCP 2025-11-25. Contains optional form and url sub-objects.
    "!
    "! @parameter result | <p class="shorttext synchronized">JSON object</p>
    METHODS get_elicitation_json
      RETURNING VALUE(result) TYPE REF TO zif_mcp_ajson.

    "! <p class="shorttext synchronized">Get tasks capability JSON</p>
    "! New in MCP 2025-11-25. Contains list, cancel, and requests sub-objects.
    "! Use to inspect fine-grained task support (e.g. tasks/list, tasks/cancel).
    "!
    "! @parameter result | <p class="shorttext synchronized">JSON object</p>
    METHODS get_tasks_json
      RETURNING VALUE(result) TYPE REF TO zif_mcp_ajson.

    "! <p class="shorttext synchronized">Get experimental capability JSON</p>
    "!
    "! @parameter result | <p class="shorttext synchronized">JSON object</p>
    METHODS get_experimental_json
      RETURNING VALUE(result) TYPE REF TO zif_mcp_ajson.

    "! <p class="shorttext synchronized">Get _meta fields</p>
    "!
    "! @parameter result | <p class="shorttext synchronized">_meta JSON</p>
    METHODS get_meta
      RETURNING VALUE(result) TYPE REF TO zif_mcp_ajson.

  PRIVATE SECTION.
    DATA int_protocol_version   TYPE string.
    DATA int_capabilities       TYPE client_capabilities.
    DATA int_client_info        TYPE implementation.
    DATA int_has_roots          TYPE abap_bool.
    DATA int_has_sampling       TYPE abap_bool.
    DATA int_has_elicitation    TYPE abap_bool.
    DATA int_has_tasks          TYPE abap_bool.
    DATA int_has_experimental   TYPE abap_bool.
    DATA int_sampling_json      TYPE REF TO zif_mcp_ajson.
    DATA int_elicitation_json   TYPE REF TO zif_mcp_ajson.
    DATA int_tasks_json         TYPE REF TO zif_mcp_ajson.
    DATA int_experimental_json  TYPE REF TO zif_mcp_ajson.
    DATA int_meta               TYPE REF TO zif_mcp_ajson.
ENDCLASS.

CLASS zcl_mcp_req_initialize IMPLEMENTATION.
  METHOD constructor.
    " Parse protocol version
    int_protocol_version = json->get_string( '/protocolVersion' ).

    " Parse client info - description and websiteUrl are new optional fields
    " in MCP 2025-11-25 (Implementation now extends BaseMetadata + Icons).
    " icons is intentionally skipped: it is an array of complex objects
    " that the server has no use for during initialization.
    int_client_info-name    = json->get_string( '/clientInfo/name' ).
    int_client_info-version = json->get_string( '/clientInfo/version' ).
    IF json->exists( '/clientInfo/title' ).
      int_client_info-title = json->get_string( '/clientInfo/title' ).
    ENDIF.
    IF json->exists( '/clientInfo/description' ).
      int_client_info-description = json->get_string( '/clientInfo/description' ).
    ENDIF.
    IF json->exists( '/clientInfo/websiteUrl' ).
      int_client_info-website_url = json->get_string( '/clientInfo/websiteUrl' ).
    ENDIF.

    " Parse capabilities - roots
    IF json->exists( '/capabilities/roots' ).
      int_has_roots = abap_true.
      IF json->exists( '/capabilities/roots/listChanged' ).
        int_capabilities-roots-list_changed = json->get_boolean( '/capabilities/roots/listChanged' ).
      ENDIF.
    ENDIF.

    " sampling: stored as JSON slice; in 2025-11-25 it may contain
    " context and tools sub-objects but their internal structure is
    " opaque to the server (just presence matters for capability checks).
    IF json->exists( '/capabilities/sampling' ).
      int_has_sampling  = abap_true.
      int_sampling_json = json->slice( '/capabilities/sampling' ).
    ENDIF.

    " elicitation: new in MCP 2025-11-25.
    " The server does not send elicitation requests, but we parse this
    " so callers can inspect what form/url modes the client declared.
    IF json->exists( '/capabilities/elicitation' ).
      int_has_elicitation  = abap_true.
      int_elicitation_json = json->slice( '/capabilities/elicitation' ).
    ENDIF.

    " tasks: new in MCP 2025-11-25.
    " Stored as JSON slice - the nested structure (list, cancel, requests)
    " is best inspected by callers via get_tasks_json rather than
    " pre-flattening into a fixed struct, keeping options open as the
    " tasks feature evolves.
    IF json->exists( '/capabilities/tasks' ).
      int_has_tasks  = abap_true.
      int_tasks_json = json->slice( '/capabilities/tasks' ).
    ENDIF.

    IF json->exists( '/capabilities/experimental' ).
      int_has_experimental  = abap_true.
      int_experimental_json = json->slice( '/capabilities/experimental' ).
    ENDIF.

    " _meta
    IF json->exists( '/_meta' ).
      int_meta = json->slice( '/_meta' ).
    ELSE.
      int_meta = zcl_mcp_ajson=>create_empty( ).
    ENDIF.
  ENDMETHOD.

  METHOD get_protocol_version.
    result = int_protocol_version.
  ENDMETHOD.

  METHOD get_capabilities.
    result = int_capabilities.
  ENDMETHOD.

  METHOD get_client_info.
    result = int_client_info.
  ENDMETHOD.

  METHOD has_roots_capability.
    result = int_has_roots.
  ENDMETHOD.

  METHOD has_sampling_capability.
    result = int_has_sampling.
  ENDMETHOD.

  METHOD has_elicitation_capability.
    result = int_has_elicitation.
  ENDMETHOD.

  METHOD has_tasks_capability.
    result = int_has_tasks.
  ENDMETHOD.

  METHOD has_experimental_capability.
    result = int_has_experimental.
  ENDMETHOD.

  METHOD get_sampling_json.
    result = int_sampling_json.
  ENDMETHOD.

  METHOD get_elicitation_json.
    result = int_elicitation_json.
  ENDMETHOD.

  METHOD get_tasks_json.
    result = int_tasks_json.
  ENDMETHOD.

  METHOD get_experimental_json.
    result = int_experimental_json.
  ENDMETHOD.

  METHOD get_meta.
    result = int_meta.
  ENDMETHOD.
ENDCLASS.
