CLASS zcl_mcp_req_initialize DEFINITION
  PUBLIC FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    TYPES: BEGIN OF implementation,
             name    TYPE string,
             version TYPE string,
           END OF implementation.

    TYPES: BEGIN OF roots_capability,
             list_changed TYPE abap_bool,
           END OF roots_capability.

    TYPES: BEGIN OF client_capabilities,
             roots TYPE roots_capability,
           END OF client_capabilities.

    "! <p class="shorttext synchronized">Constructor</p>
    "! Parses the Initialize Request JSON
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

    "! <p class="shorttext synchronized">Check if experimental capability exists</p>
    "!
    "! @parameter result | <p class="shorttext synchronized">True if exists</p>
    METHODS has_experimental_capability
      RETURNING VALUE(result) TYPE abap_bool.

    "! <p class="shorttext synchronized">Get sampling capability JSON</p>
    "!
    "! @parameter result | <p class="shorttext synchronized">JSON object</p>
    METHODS get_sampling_json
      RETURNING VALUE(result) TYPE REF TO zif_mcp_ajson.

    "! <p class="shorttext synchronized">Get experimental capability JSON</p>
    "!
    "! @parameter result | <p class="shorttext synchronized">JSON object</p>
    METHODS get_experimental_json
      RETURNING VALUE(result) TYPE REF TO zif_mcp_ajson.

  PRIVATE SECTION.
    DATA int_protocol_version  TYPE string.
    DATA int_capabilities      TYPE client_capabilities.
    DATA int_client_info       TYPE implementation.
    DATA int_has_roots         TYPE abap_bool.
    DATA int_has_sampling      TYPE abap_bool.
    DATA int_has_experimental  TYPE abap_bool.
    DATA int_sampling_json     TYPE REF TO zif_mcp_ajson.
    DATA int_experimental_json TYPE REF TO zif_mcp_ajson.
ENDCLASS.

CLASS zcl_mcp_req_initialize IMPLEMENTATION.
  METHOD constructor.
    " Parse protocol version
    int_protocol_version = json->get_string( '/protocolVersion' ).

    " Parse client info
    int_client_info-name    = json->get_string( '/clientInfo/name' ).
    int_client_info-version = json->get_string( '/clientInfo/version' ).

    " Parse capabilities
    IF json->exists( '/capabilities/roots' ) IS NOT INITIAL.
      int_has_roots = abap_true.
      IF json->exists( '/capabilities/roots/listChanged' ) IS NOT INITIAL.
        int_capabilities-roots-list_changed = json->get_boolean( '/capabilities/roots/listChanged' ).
      ENDIF.
    ENDIF.

    " For complex structures, we'll just store the JSON subtree
    IF json->exists( '/capabilities/sampling' ) IS NOT INITIAL.
      int_has_sampling = abap_true.
      int_sampling_json = json->slice( '/capabilities/sampling' ).
    ENDIF.

    IF json->exists( '/capabilities/experimental' ) IS NOT INITIAL.
      int_has_experimental = abap_true.
      int_experimental_json = json->slice( '/capabilities/experimental' ).
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

  METHOD has_experimental_capability.
    result = int_has_experimental.
  ENDMETHOD.

  METHOD get_sampling_json.
    result = int_sampling_json.
  ENDMETHOD.

  METHOD get_experimental_json.
    result = int_experimental_json.
  ENDMETHOD.
ENDCLASS.
