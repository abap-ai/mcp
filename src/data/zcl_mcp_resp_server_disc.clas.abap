"! <p class="shorttext synchronized">MCP draft server/discover response</p>
"! Builds the result object for server/discover in the stateless draft protocol.
CLASS zcl_mcp_resp_server_disc DEFINITION
PUBLIC FINAL
CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES zif_mcp_modern_result.

    " Server implementation metadata.
    TYPES: BEGIN OF implementation,
             name        TYPE string,
             version     TYPE string,
             title       TYPE string,
             description TYPE string,
             website_url TYPE string,
             icons       TYPE zif_mcp_types=>icon_list,
           END OF implementation.

    " Server capabilities advertised by server/discover.
    TYPES: BEGIN OF capabilities,
             prompts     TYPE abap_bool,
             resources   TYPE abap_bool,
             tools       TYPE abap_bool,
             completions TYPE abap_bool,
             tasks       TYPE abap_bool,
           END OF capabilities.

    "! <p class="shorttext synchronized">Set supported protocol versions</p>
    "! Sets the protocol versions advertised by server/discover.
    "!
    "! @parameter versions | <p class="shorttext synchronized">Supported protocol versions</p>
    METHODS set_supported_versions
      IMPORTING versions TYPE string_table.

    "! <p class="shorttext synchronized">Set server capabilities</p>
    "! Sets the draft MCP capabilities advertised by server/discover.
    "!
    "! @parameter capabilities | <p class="shorttext synchronized">Server capabilities</p>
    METHODS set_capabilities
      IMPORTING capabilities TYPE capabilities.

    "! <p class="shorttext synchronized">Set server implementation</p>
    "! Sets server identity metadata for server/discover.
    "!
    "! @parameter implementation | <p class="shorttext synchronized">Server implementation metadata</p>
    METHODS set_implementation
      IMPORTING !implementation TYPE implementation.

    "! <p class="shorttext synchronized">Set server instructions</p>
    "! Sets optional LLM-facing usage instructions for the server.
    "!
    "! @parameter instructions | <p class="shorttext synchronized">Server instructions</p>
    METHODS set_instructions
      IMPORTING instructions TYPE string.

  PRIVATE SECTION.
    DATA int_versions     TYPE string_table.
    DATA int_capabilities TYPE capabilities.
    DATA int_impl         TYPE implementation.
    DATA int_instructions TYPE string.
    DATA int_meta         TYPE REF TO zif_mcp_ajson.
    DATA int_ttl_ms       TYPE i.
    DATA int_cache_scope  TYPE string VALUE 'private'.

ENDCLASS.

CLASS zcl_mcp_resp_server_disc IMPLEMENTATION.
  METHOD set_supported_versions.
    int_versions = versions.
  ENDMETHOD.

  METHOD set_capabilities.
    int_capabilities = capabilities.
  ENDMETHOD.

  METHOD set_implementation.
    int_impl = implementation.
  ENDMETHOD.

  METHOD set_instructions.
    int_instructions = instructions.
  ENDMETHOD.

  METHOD zif_mcp_modern_result~set_meta.
    int_meta = meta.
  ENDMETHOD.

  METHOD zif_mcp_modern_result~set_cache.
    int_ttl_ms = ttl_ms.
    IF cache_scope IS INITIAL.
      int_cache_scope = zif_mcp_constants=>cache_scopes-private.
    ELSE.
      int_cache_scope = cache_scope.
    ENDIF.
  ENDMETHOD.

  METHOD zif_mcp_modern_result~generate_json.
    DATA versions TYPE string_table.

    result = zcl_mcp_ajson=>create_empty( ).

    result->set( iv_path = '/resultType'
                 iv_val  = zif_mcp_constants=>result_types-complete ).

    versions = int_versions.
    IF versions IS INITIAL.
      APPEND zif_mcp_constants=>latest_modern_protocol_version TO versions.
    ENDIF.

    result->touch_array( '/supportedVersions' ).
    LOOP AT versions INTO DATA(version).
      result->set( iv_path = |/supportedVersions/{ sy-tabix }|
                   iv_val  = version ).
    ENDLOOP.

    result->touch_object( '/capabilities' ).

    IF int_capabilities-prompts = abap_true.
      result->touch_object( '/capabilities/prompts' ).
    ENDIF.

    IF int_capabilities-resources = abap_true.
      result->touch_object( '/capabilities/resources' ).
    ENDIF.

    IF int_capabilities-tools = abap_true.
      result->touch_object( '/capabilities/tools' ).
    ENDIF.

    IF int_capabilities-completions = abap_true.
      result->touch_object( '/capabilities/completions' ).
    ENDIF.

    IF int_capabilities-tasks = abap_true.
      result->touch_object( '/capabilities/extensions' ).
      result->touch_object( '/capabilities/extensions/io.modelcontextprotocol~1tasks' ).
    ENDIF.

    result->set( iv_path         = '/serverInfo/name'
                 iv_val          = int_impl-name
                 iv_ignore_empty = abap_false
                 iv_node_type    = zif_mcp_ajson_types=>node_type-string ).

    result->set( iv_path         = '/serverInfo/version'
                 iv_val          = int_impl-version
                 iv_ignore_empty = abap_false
                 iv_node_type    = zif_mcp_ajson_types=>node_type-string ).

    IF int_impl-title IS NOT INITIAL.
      result->set( iv_path      = '/serverInfo/title'
                   iv_val       = int_impl-title
                   iv_node_type = zif_mcp_ajson_types=>node_type-string ).
    ENDIF.

    IF int_impl-description IS NOT INITIAL.
      result->set( iv_path      = '/serverInfo/description'
                   iv_val       = int_impl-description
                   iv_node_type = zif_mcp_ajson_types=>node_type-string ).
    ENDIF.

    IF int_impl-website_url IS NOT INITIAL.
      result->set( iv_path      = '/serverInfo/websiteUrl'
                   iv_val       = int_impl-website_url
                   iv_node_type = zif_mcp_ajson_types=>node_type-string ).
    ENDIF.

    IF int_impl-icons IS NOT INITIAL.
      result->touch_array( '/serverInfo/icons' ).
      LOOP AT int_impl-icons ASSIGNING FIELD-SYMBOL(<icon>).
        DATA(icon_path) = |/serverInfo/icons/{ sy-tabix }|.

        result->set( iv_path = |{ icon_path }/src|
                     iv_val  = <icon>-src ).

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
      result->set( iv_path = '/instructions'
                   iv_val  = int_instructions ).
    ENDIF.

    result->set_integer( iv_path = '/ttlMs'
                         iv_val  = int_ttl_ms ).

    result->set( iv_path = '/cacheScope'
                 iv_val  = int_cache_scope ).

    IF int_meta IS BOUND.
      result->set( iv_path = '/_meta'
                   iv_val  = int_meta ).
    ENDIF.
  ENDMETHOD.
ENDCLASS.
