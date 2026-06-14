"! <p class="shorttext synchronized">Basic MCP V2 demo server</p>
"! Demonstrates the normal stateless MCP draft v2 server shape:
"! discovery, tools, prompts, resources, templates, completions, cache hints,
"! result metadata, and x-mcp-header parameter mirroring.
CLASS zcl_mcp_demo_server_v2_basic DEFINITION
PUBLIC
INHERITING FROM zcl_mcp_server_base_v2 FINAL
CREATE PUBLIC.

  PROTECTED SECTION.
    METHODS get_implementation       REDEFINITION.
    METHODS get_capabilities         REDEFINITION.
    METHODS get_instructions         REDEFINITION.
    METHODS handle_prompts_list      REDEFINITION.
    METHODS handle_prompts_get       REDEFINITION.
    METHODS handle_resources_list    REDEFINITION.
    METHODS handle_resources_read    REDEFINITION.
    METHODS handle_res_tmpls_list    REDEFINITION.
    METHODS handle_tools_list        REDEFINITION.
    METHODS handle_tools_call        REDEFINITION.
    METHODS handle_completion        REDEFINITION.
    METHODS handle_tool_input_schema REDEFINITION.

  PRIVATE SECTION.
    CONSTANTS c_tool_echo        TYPE string VALUE `echo`.
    CONSTANTS c_tool_server_time TYPE string VALUE `server_time`.
    CONSTANTS c_tool_cache_meta  TYPE string VALUE `cache_meta`.

    CONSTANTS c_prompt_summary   TYPE string VALUE `summary_prompt`.
    CONSTANTS c_resource_about   TYPE string VALUE `demo://v2/about`.
    CONSTANTS c_resource_notes   TYPE string VALUE `demo://v2/notes/{name}`.

    "! <p class="shorttext synchronized">Convert exception into v2 error</p>
    "!
    "! @parameter error    | <p class="shorttext synchronized">Caught exception</p>
    "! @parameter response | <p class="shorttext synchronized">V2 response</p>
    METHODS json_error
      IMPORTING !error          TYPE REF TO cx_root
      RETURNING VALUE(response) TYPE zif_mcp_server_v2=>v2_response.

    "! <p class="shorttext synchronized">Add complete result metadata</p>
    "!
    "! @parameter result_json         | <p class="shorttext synchronized">Result JSON object</p>
    "! @raising   zcx_mcp_ajson_error | <p class="shorttext synchronized">JSON error</p>
    METHODS set_complete_fields
      IMPORTING result_json TYPE REF TO zif_mcp_ajson
      RAISING   zcx_mcp_ajson_error.

    "! <p class="shorttext synchronized">Build schema for a tool</p>
    "!
    "! @parameter tool_name           | <p class="shorttext synchronized">Tool name</p>
    "! @parameter result              | <p class="shorttext synchronized">Input schema</p>
    "! @raising   zcx_mcp_ajson_error | <p class="shorttext synchronized">JSON error</p>
    METHODS build_tool_schema
      IMPORTING tool_name     TYPE string
      RETURNING VALUE(result) TYPE REF TO zif_mcp_ajson
      RAISING   zcx_mcp_ajson_error.

    "! <p class="shorttext synchronized">Build tools/list result</p>
    "!
    "! @parameter result              | <p class="shorttext synchronized">Tools list result</p>
    "! @raising   zcx_mcp_ajson_error | <p class="shorttext synchronized">JSON error</p>
    METHODS build_tools_result
      RETURNING VALUE(result) TYPE REF TO zif_mcp_ajson
      RAISING   zcx_mcp_ajson_error.

    "! <p class="shorttext synchronized">Build text tool result</p>
    "!
    "! @parameter text                | <p class="shorttext synchronized">Tool response text</p>
    "! @parameter result              | <p class="shorttext synchronized">Tool result</p>
    "! @raising   zcx_mcp_ajson_error | <p class="shorttext synchronized">JSON error</p>
    METHODS build_text_tool_result
      IMPORTING !text         TYPE string
      RETURNING VALUE(result) TYPE REF TO zif_mcp_ajson
      RAISING   zcx_mcp_ajson_error.

    "! <p class="shorttext synchronized">Build cache and metadata tool result</p>
    "!
    "! @parameter result              | <p class="shorttext synchronized">Tool result</p>
    "! @raising   zcx_mcp_ajson_error | <p class="shorttext synchronized">JSON error</p>
    METHODS build_cache_meta_result
      RETURNING VALUE(result) TYPE REF TO zif_mcp_ajson
      RAISING   zcx_mcp_ajson_error.

    "! <p class="shorttext synchronized">Build prompts/list result</p>
    "!
    "! @parameter result              | <p class="shorttext synchronized">Prompts list result</p>
    "! @raising   zcx_mcp_ajson_error | <p class="shorttext synchronized">JSON error</p>
    METHODS build_prompts_result
      RETURNING VALUE(result) TYPE REF TO zif_mcp_ajson
      RAISING   zcx_mcp_ajson_error.

    "! <p class="shorttext synchronized">Build prompts/get result</p>
    "!
    "! @parameter topic               | <p class="shorttext synchronized">Prompt topic</p>
    "! @parameter result              | <p class="shorttext synchronized">Prompt result</p>
    "! @raising   zcx_mcp_ajson_error | <p class="shorttext synchronized">JSON error</p>
    METHODS build_prompt_result
      IMPORTING topic         TYPE string
      RETURNING VALUE(result) TYPE REF TO zif_mcp_ajson
      RAISING   zcx_mcp_ajson_error.

    "! <p class="shorttext synchronized">Build resources/list result</p>
    "!
    "! @parameter result              | <p class="shorttext synchronized">Resources list result</p>
    "! @raising   zcx_mcp_ajson_error | <p class="shorttext synchronized">JSON error</p>
    METHODS build_resources_result
      RETURNING VALUE(result) TYPE REF TO zif_mcp_ajson
      RAISING   zcx_mcp_ajson_error.

    "! <p class="shorttext synchronized">Build resources/templates/list result</p>
    "!
    "! @parameter result              | <p class="shorttext synchronized">Resource templates result</p>
    "! @raising   zcx_mcp_ajson_error | <p class="shorttext synchronized">JSON error</p>
    METHODS build_templates_result
      RETURNING VALUE(result) TYPE REF TO zif_mcp_ajson
      RAISING   zcx_mcp_ajson_error.

    "! <p class="shorttext synchronized">Build resources/read result</p>
    "!
    "! @parameter uri                 | <p class="shorttext synchronized">Resource URI</p>
    "! @parameter text                | <p class="shorttext synchronized">Resource text</p>
    "! @parameter result              | <p class="shorttext synchronized">Resource result</p>
    "! @raising   zcx_mcp_ajson_error | <p class="shorttext synchronized">JSON error</p>
    METHODS build_resource_read_result
      IMPORTING uri           TYPE string
                !text         TYPE string
      RETURNING VALUE(result) TYPE REF TO zif_mcp_ajson
      RAISING   zcx_mcp_ajson_error.

    "! <p class="shorttext synchronized">Build completion/complete result</p>
    "!
    "! @parameter result              | <p class="shorttext synchronized">Completion result</p>
    "! @raising   zcx_mcp_ajson_error | <p class="shorttext synchronized">JSON error</p>
    METHODS build_completion_result
      RETURNING VALUE(result) TYPE REF TO zif_mcp_ajson
      RAISING   zcx_mcp_ajson_error.
ENDCLASS.


CLASS zcl_mcp_demo_server_v2_basic IMPLEMENTATION.
  METHOD get_implementation.
    result-name        = `ABAP MCP V2 Basic Demo`.
    result-version     = `1.0.0`.
    result-title       = `ABAP MCP V2 Basic Demo`.
    result-description = `Basic stateless MCP draft v2 demo server.`.
    result-website_url = `https://github.com/b-tocs/abap_mcp`.
  ENDMETHOD.

  METHOD get_capabilities.
    result-prompts     = abap_true.
    result-resources   = abap_true.
    result-tools       = abap_true.
    result-completions = abap_true.
    result-tasks       = abap_false.
  ENDMETHOD.

  METHOD get_instructions.
    result = `Use this server to explore normal stateless MCP v2 tools, prompts, resources, templates, and completions.`.
  ENDMETHOD.

  METHOD handle_tools_list.
    TRY.
        response-result = build_tools_result( ).
      CATCH zcx_mcp_ajson_error INTO DATA(error).
        response = json_error( error ).
    ENDTRY.
  ENDMETHOD.

  METHOD handle_tool_input_schema.
    result = build_tool_schema( tool_name ).
  ENDMETHOD.

  METHOD handle_tools_call.
    TRY.
        CASE request->get_name( ).
          WHEN c_tool_echo.
            DATA(arguments) = request->get_arguments( ).
            DATA(message) = arguments->get_string( `/message` ).

            IF message IS INITIAL.
              message = `Hello from the ABAP MCP V2 basic demo.`.
            ENDIF.

            response-result = build_text_tool_result( |Echo: { message }| ).

          WHEN c_tool_server_time.
            response-result = build_text_tool_result( |ABAP server date { sy-datum }, time { sy-uzeit }.| ).

          WHEN c_tool_cache_meta.
            response-result = build_cache_meta_result( ).

          WHEN OTHERS.
            response = method_not_found( request->get_name( ) ).
        ENDCASE.

      CATCH zcx_mcp_ajson_error INTO DATA(error).
        response = json_error( error ).
    ENDTRY.
  ENDMETHOD.

  METHOD handle_prompts_list.
    TRY.
        response-result = build_prompts_result( ).
      CATCH zcx_mcp_ajson_error INTO DATA(error).
        response = json_error( error ).
    ENDTRY.
  ENDMETHOD.

  METHOD handle_prompts_get.
    DATA topic TYPE string VALUE `ABAP MCP`.

    TRY.
        IF request->get_name( ) <> c_prompt_summary.
          response-error-code    = zcl_mcp_jsonrpc=>error_codes-invalid_params.
          response-error-message = |Prompt { request->get_name( ) } unknown.|.
          RETURN.
        ENDIF.

        IF request->has_arguments( ) = abap_true.
          DATA(arguments) = request->get_arguments( ).
          READ TABLE arguments INTO DATA(argument) WITH KEY key = `topic`.
          IF sy-subrc = 0 AND argument-value IS NOT INITIAL.
            topic = argument-value.
          ENDIF.
        ENDIF.

        response-result = build_prompt_result( topic ).

      CATCH zcx_mcp_ajson_error INTO DATA(error).
        response = json_error( error ).
    ENDTRY.
  ENDMETHOD.

  METHOD handle_resources_list.
    TRY.
        response-result = build_resources_result( ).
      CATCH zcx_mcp_ajson_error INTO DATA(error).
        response = json_error( error ).
    ENDTRY.
  ENDMETHOD.

  METHOD handle_res_tmpls_list.
    TRY.
        response-result = build_templates_result( ).
      CATCH zcx_mcp_ajson_error INTO DATA(error).
        response = json_error( error ).
    ENDTRY.
  ENDMETHOD.

  METHOD handle_resources_read.
    DATA uri  TYPE string.
    DATA text TYPE string.

    uri = request->get_uri( ).

    TRY.
        IF uri = c_resource_about.
          text = `This resource is served by the ABAP MCP V2 basic demo server.`.

        ELSEIF uri CP `demo://v2/notes/*`.
          text = |Generated demo note for URI { uri }.|.

        ELSE.
          response-error-code    = zcl_mcp_jsonrpc=>error_codes-resource_not_found.
          response-error-message = |Resource { uri } not found.|.
          RETURN.
        ENDIF.

        response-result = build_resource_read_result( uri  = uri
                                                      text = text ).

      CATCH zcx_mcp_ajson_error INTO DATA(error).
        response = json_error( error ).
    ENDTRY.
  ENDMETHOD.

  METHOD handle_completion.
    TRY.
        response-result = build_completion_result( ).
      CATCH zcx_mcp_ajson_error INTO DATA(error).
        response = json_error( error ).
    ENDTRY.
  ENDMETHOD.

  METHOD json_error.
    response-error-code    = zcl_mcp_jsonrpc=>error_codes-internal_error.
    response-error-message = error->get_text( ).
  ENDMETHOD.

  METHOD set_complete_fields.
    result_json->set_string( iv_path = `/resultType`
                             iv_val  = zif_mcp_constants=>result_types-complete ).

    result_json->set_integer( iv_path = `/ttlMs`
                              iv_val  = 0 ).

    result_json->set_string( iv_path = `/cacheScope`
                             iv_val  = zif_mcp_constants=>cache_scopes-private ).
  ENDMETHOD.

  METHOD build_tool_schema.
    DATA builder TYPE REF TO zcl_mcp_schema_builder.

    builder = NEW zcl_mcp_schema_builder( ).

    CASE tool_name.
      WHEN c_tool_echo.
        builder->add_string( name         = `message`
                             description  = `Message to return from the demo server.`
                             required     = abap_false
                             x_mcp_header = `Message` ).

      WHEN c_tool_server_time.
        " Empty object schema.

      WHEN c_tool_cache_meta.
        " Empty object schema.

      WHEN OTHERS.
        " Empty object schema lets tools/list remain robust; tools/call still rejects unknown tools.
    ENDCASE.

    result = builder->to_json( ).
  ENDMETHOD.

  METHOD build_tools_result.
    DATA list_tools TYPE REF TO zcl_mcp_resp_list_tools.
    DATA tools      TYPE zcl_mcp_resp_list_tools=>tools.
    DATA tool       TYPE zcl_mcp_resp_list_tools=>tool.

    list_tools = NEW zcl_mcp_resp_list_tools( ).

    CLEAR tool.
    tool-name         = c_tool_echo.
    tool-title        = `Echo`.
    tool-description  = `Returns a text message. The message can also be mirrored through Mcp-Param-Message.`.
    tool-input_schema = build_tool_schema( c_tool_echo ).
    tool-annotations-readonlyhint = abap_true.
    APPEND tool TO tools.

    CLEAR tool.
    tool-name         = c_tool_server_time.
    tool-title        = `Server Time`.
    tool-description  = `Returns the current ABAP server date and time.`.
    tool-input_schema = build_tool_schema( c_tool_server_time ).
    tool-annotations-readonlyhint = abap_true.
    APPEND tool TO tools.

    CLEAR tool.
    tool-name         = c_tool_cache_meta.
    tool-title        = `Cache And Metadata`.
    tool-description  = `Returns a normal v2 complete result with ttlMs, cacheScope, and _meta.`.
    tool-input_schema = build_tool_schema( c_tool_cache_meta ).
    tool-annotations-readonlyhint = abap_true.
    APPEND tool TO tools.

    list_tools->set_tools( tools ).

    result = list_tools->zif_mcp_internal~generate_json( ).
    set_complete_fields( result ).
  ENDMETHOD.

  METHOD build_text_tool_result.
    DATA tool_result TYPE REF TO zcl_mcp_resp_v2_tool.

    tool_result = NEW zcl_mcp_resp_v2_tool( ).
    tool_result->set_complete( ).
    tool_result->set_error( abap_false ).
    tool_result->add_text_content( text ).

    result = tool_result->generate_json( ).
  ENDMETHOD.

  METHOD build_cache_meta_result.
    DATA tool_result TYPE REF TO zcl_mcp_resp_v2_tool.
    DATA meta        TYPE REF TO zif_mcp_ajson.

    tool_result = NEW zcl_mcp_resp_v2_tool( ).
    meta = zcl_mcp_ajson=>create_empty( ).

    meta->set_string( iv_path = `/abap.demo~1kind`
                      iv_val  = `basic-v2-demo` ).

    meta->set_string( iv_path = `/abap.demo~1feature`
                      iv_val  = `cache-meta` ).

    tool_result->set_complete( ).
    tool_result->set_error( abap_false ).
    tool_result->add_text_content( `This result demonstrates v2 cache hints and result metadata.` ).
    tool_result->set_cache( ttl_ms      = 30000
                            cache_scope = zif_mcp_constants=>cache_scopes-private ).
    tool_result->set_meta( meta ).

    result = tool_result->generate_json( ).
  ENDMETHOD.

  METHOD build_prompts_result.
    DATA list_prompts TYPE REF TO zcl_mcp_resp_list_prompts.
    DATA prompts      TYPE zcl_mcp_resp_list_prompts=>prompts.
    DATA prompt       TYPE zcl_mcp_resp_list_prompts=>prompt.
    DATA argument     TYPE zcl_mcp_resp_list_prompts=>prompt_argument.

    list_prompts = NEW zcl_mcp_resp_list_prompts( ).

    argument-name         = `topic`.
    argument-title        = `Topic`.
    argument-description  = `Topic to summarize.`.
    argument-required     = abap_false.
    argument-required_set = abap_true.

    prompt-name        = c_prompt_summary.
    prompt-title       = `Summary Prompt`.
    prompt-description = `Creates a short summary prompt for a topic.`.
    APPEND argument TO prompt-arguments.
    APPEND prompt TO prompts.

    list_prompts->set_prompts( prompts ).

    result = list_prompts->zif_mcp_internal~generate_json( ).
    set_complete_fields( result ).
  ENDMETHOD.

  METHOD build_prompt_result.
    DATA get_prompt TYPE REF TO zcl_mcp_resp_get_prompt.

    get_prompt = NEW zcl_mcp_resp_get_prompt( ).
    get_prompt->set_description( `Basic v2 demo prompt.` ).
    get_prompt->add_text_message( role = zif_mcp_types=>role_user
                                  text = |Write a concise technical summary about { topic }.| ).

    result = get_prompt->zif_mcp_internal~generate_json( ).
    set_complete_fields( result ).
  ENDMETHOD.

  METHOD build_resources_result.
    DATA list_resources TYPE REF TO zcl_mcp_resp_list_resources.
    DATA resources      TYPE zcl_mcp_resp_list_resources=>resources.
    DATA resource       TYPE zcl_mcp_resp_list_resources=>resource.

    list_resources = NEW zcl_mcp_resp_list_resources( ).

    resource-uri         = c_resource_about.
    resource-name        = `about-v2-basic-demo`.
    resource-title       = `About V2 Basic Demo`.
    resource-description = `Static text resource from the ABAP MCP V2 basic demo.`.
    resource-mime_type   = `text/plain`.
    APPEND resource TO resources.

    list_resources->set_resources( resources ).

    result = list_resources->zif_mcp_internal~generate_json( ).
    set_complete_fields( result ).
  ENDMETHOD.

  METHOD build_templates_result.
    DATA list_templates TYPE REF TO zcl_mcp_resp_list_res_tmpl.
    DATA templates      TYPE zcl_mcp_resp_list_res_tmpl=>resource_templates.
    DATA template       TYPE zcl_mcp_resp_list_res_tmpl=>resource_template.

    list_templates = NEW zcl_mcp_resp_list_res_tmpl( ).

    template-uritemplate = c_resource_notes.
    template-name        = `demo-note`.
    template-title       = `Demo Note`.
    template-description = `Generated text resource for demo://v2/notes/{name}.`.
    template-mime_type   = `text/plain`.
    APPEND template TO templates.

    list_templates->set_resource_templates( templates ).

    result = list_templates->zif_mcp_internal~generate_json( ).
    set_complete_fields( result ).
  ENDMETHOD.

  METHOD build_resource_read_result.
    DATA read_resource TYPE REF TO zcl_mcp_resp_read_resource.

    read_resource = NEW zcl_mcp_resp_read_resource( ).
    read_resource->add_text_resource( uri       = uri
                                      text      = text
                                      mime_type = `text/plain` ).

    result = read_resource->zif_mcp_internal~generate_json( ).
    set_complete_fields( result ).
  ENDMETHOD.

  METHOD build_completion_result.
    DATA complete TYPE REF TO zcl_mcp_resp_complete.

    complete = NEW zcl_mcp_resp_complete( ).
    complete->add_value( `ABAP` ).
    complete->add_value( `MCP` ).
    complete->add_value( `V2` ).
    complete->add_value( `Stateless HTTP` ).
    complete->set_total( 4 ).
    complete->set_has_more( abap_false ).

    result = complete->zif_mcp_internal~generate_json( ).
    set_complete_fields( result ).
  ENDMETHOD.
ENDCLASS.
