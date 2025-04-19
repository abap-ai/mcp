CLASS ltcl_mcp_resp_list_prompts DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA cut TYPE REF TO zcl_mcp_resp_list_prompts.

    METHODS setup.
    METHODS test_empty_response   FOR TESTING RAISING zcx_mcp_ajson_error.
    METHODS test_with_prompts     FOR TESTING RAISING zcx_mcp_ajson_error.
    METHODS test_with_next_cursor FOR TESTING RAISING zcx_mcp_ajson_error.
    METHODS test_with_meta        FOR TESTING RAISING zcx_mcp_ajson_error.
    METHODS test_with_all_fields  FOR TESTING RAISING zcx_mcp_ajson_error.

ENDCLASS.

CLASS ltcl_mcp_resp_list_prompts IMPLEMENTATION.

  METHOD setup.
    cut = NEW zcl_mcp_resp_list_prompts( ).
  ENDMETHOD.

  METHOD test_empty_response.
    " Given
    " No data set

    " When
    DATA(ajson) = cut->zif_mcp_internal~generate_json( ).

    " Then
    DATA json_str TYPE string.
    json_str = ajson->stringify( ).

    cl_abap_unit_assert=>assert_equals( exp = '{"prompts":[]}'
                                        act = json_str ).
  ENDMETHOD.

  METHOD test_with_prompts.
    " Given
    DATA prompts  TYPE zcl_mcp_resp_list_prompts=>prompts.
    DATA prompt   TYPE zcl_mcp_resp_list_prompts=>prompt.
    DATA argument TYPE zcl_mcp_resp_list_prompts=>prompt_argument.

    " Create a prompt with arguments
    prompt-name        = 'Test Prompt'.
    prompt-description = 'A test prompt description'.

    " Add an argument
    argument-name        = 'arg1'.
    argument-description = 'First argument'.
    argument-required    = abap_true.
    APPEND argument TO prompt-arguments.

    " Add another argument
    CLEAR argument.
    argument-name        = 'arg2'.
    argument-description = 'Second argument'.
    argument-required    = abap_false.
    APPEND argument TO prompt-arguments.

    APPEND prompt TO prompts.

    " Add another prompt without arguments
    CLEAR prompt.
    prompt-name        = 'Simple Prompt'.
    prompt-description = 'Simple prompt without arguments'.
    APPEND prompt TO prompts.

    " When
    cut->set_prompts( prompts ).

    DATA(ajson) = cut->zif_mcp_internal~generate_json( ).

    " Then
    DATA json_str TYPE string.
    json_str = ajson->stringify( ).

    " Check for prompt with name "Test Prompt"
    DATA json_test_prompt TYPE REF TO zif_mcp_ajson.
    json_test_prompt = zcl_mcp_ajson=>parse( json_str ).

    DATA prompt_name TYPE string.
    prompt_name = json_test_prompt->get_string( '/prompts/1/name' ).
    cl_abap_unit_assert=>assert_equals( exp = 'Test Prompt'
                                        act = prompt_name ).

    DATA prompt_desc TYPE string.
    prompt_desc = json_test_prompt->get_string( '/prompts/1/description' ).
    cl_abap_unit_assert=>assert_equals( exp = 'A test prompt description'
                                        act = prompt_desc ).

    " Check arguments
    DATA arg1_name TYPE string.
    arg1_name = json_test_prompt->get_string( '/prompts/1/arguments/1/name' ).
    cl_abap_unit_assert=>assert_equals( exp = 'arg1'
                                        act = arg1_name ).

    DATA arg1_desc TYPE string.
    arg1_desc = json_test_prompt->get_string( '/prompts/1/arguments/1/description' ).
    cl_abap_unit_assert=>assert_equals( exp = 'First argument'
                                        act = arg1_desc ).

    DATA arg1_required TYPE abap_bool.
    arg1_required = json_test_prompt->get_boolean( '/prompts/1/arguments/1/required' ).
    cl_abap_unit_assert=>assert_true( arg1_required ).

    DATA arg2_name TYPE string.
    arg2_name = json_test_prompt->get_string( '/prompts/1/arguments/2/name' ).
    cl_abap_unit_assert=>assert_equals( exp = 'arg2'
                                        act = arg2_name ).

    DATA arg2_desc TYPE string.
    arg2_desc = json_test_prompt->get_string( '/prompts/1/arguments/2/description' ).
    cl_abap_unit_assert=>assert_equals( exp = 'Second argument'
                                        act = arg2_desc ).

    " Check simple prompt
    DATA simple_name TYPE string.
    simple_name = json_test_prompt->get_string( '/prompts/2/name' ).
    cl_abap_unit_assert=>assert_equals( exp = 'Simple Prompt'
                                        act = simple_name ).

    DATA simple_desc TYPE string.
    simple_desc = json_test_prompt->get_string( '/prompts/2/description' ).
    cl_abap_unit_assert=>assert_equals( exp = 'Simple prompt without arguments'
                                        act = simple_desc ).
  ENDMETHOD.

  METHOD test_with_next_cursor.
    " Given
    DATA next_cursor TYPE zcl_mcp_resp_list_prompts=>next_cursor.

    next_cursor = 'next_page_token_123'.

    " When
    cut->set_next_cursor( next_cursor ).

    DATA(ajson) = cut->zif_mcp_internal~generate_json( ).

    " Then
    DATA json_str TYPE string.
    json_str = ajson->stringify( ).

    DATA json_result TYPE REF TO zif_mcp_ajson.
    json_result = zcl_mcp_ajson=>parse( json_str ).

    DATA cursor_value TYPE string.
    cursor_value = json_result->get_string( '/nextCursor' ).
    cl_abap_unit_assert=>assert_equals( exp = 'next_page_token_123'
                                        act = cursor_value ).
  ENDMETHOD.

  METHOD test_with_meta.
    " Given
    DATA meta TYPE REF TO zif_mcp_ajson.

    meta = zcl_mcp_ajson=>create_empty( ).
    meta->set( iv_path = '/version'
               iv_val  = '1.0' ).
    meta->set( iv_path = '/timestamp'
               iv_val  = '2023-10-15T14:30:00Z' ).

    " When
    cut->set_meta( meta ).

    DATA(ajson) = cut->zif_mcp_internal~generate_json( ).

    " Then
    DATA json_str TYPE string.
    json_str = ajson->stringify( ).

    DATA json_result TYPE REF TO zif_mcp_ajson.
    json_result = zcl_mcp_ajson=>parse( json_str ).

    DATA version TYPE string.
    version = json_result->get_string( '/_meta/version' ).
    cl_abap_unit_assert=>assert_equals( exp = '1.0'
                                        act = version ).

    DATA timestamp TYPE string.
    timestamp = json_result->get_string( '/_meta/timestamp' ).
    cl_abap_unit_assert=>assert_equals( exp = '2023-10-15T14:30:00Z'
                                        act = timestamp ).
  ENDMETHOD.

  METHOD test_with_all_fields.
    " Given
    " 1. Set prompts
    DATA prompts  TYPE zcl_mcp_resp_list_prompts=>prompts.
    DATA prompt   TYPE zcl_mcp_resp_list_prompts=>prompt.
    DATA argument TYPE zcl_mcp_resp_list_prompts=>prompt_argument.

    prompt-name        = 'Complete Prompt'.
    prompt-description = 'A complete prompt example'.

    argument-name        = 'arg1'.
    argument-description = 'Required argument'.
    argument-required    = abap_true.
    APPEND argument TO prompt-arguments.

    APPEND prompt TO prompts.

    cut->set_prompts( prompts ).

    " 2. Set next cursor
    cut->set_next_cursor( 'cursor_token_xyz' ).

    " 3. Set meta
    DATA meta TYPE REF TO zif_mcp_ajson.
    meta = zcl_mcp_ajson=>create_empty( ).
    meta->set( iv_path = '/apiVersion'
               iv_val  = '2.0' ).

    cut->set_meta( meta ).

    " When
    DATA(ajson) = cut->zif_mcp_internal~generate_json( ).

    " Then
    DATA json_str TYPE string.
    json_str = ajson->stringify( ).

    DATA json_result TYPE REF TO zif_mcp_ajson.
    json_result = zcl_mcp_ajson=>parse( json_str ).

    " Check prompt fields
    DATA prompt_name TYPE string.
    prompt_name = json_result->get_string( '/prompts/1/name' ).
    cl_abap_unit_assert=>assert_equals( exp = 'Complete Prompt'
                                        act = prompt_name ).

    DATA prompt_desc TYPE string.
    prompt_desc = json_result->get_string( '/prompts/1/description' ).
    cl_abap_unit_assert=>assert_equals( exp = 'A complete prompt example'
                                        act = prompt_desc ).

    " Check argument
    DATA arg_name TYPE string.
    arg_name = json_result->get_string( '/prompts/1/arguments/1/name' ).
    cl_abap_unit_assert=>assert_equals( exp = 'arg1'
                                        act = arg_name ).

    DATA arg_desc TYPE string.
    arg_desc = json_result->get_string( '/prompts/1/arguments/1/description' ).
    cl_abap_unit_assert=>assert_equals( exp = 'Required argument'
                                        act = arg_desc ).

    DATA arg_required TYPE abap_bool.
    arg_required = json_result->get_boolean( '/prompts/1/arguments/1/required' ).
    cl_abap_unit_assert=>assert_true( arg_required ).

    " Check cursor
    DATA cursor TYPE string.
    cursor = json_result->get_string( '/nextCursor' ).
    cl_abap_unit_assert=>assert_equals( exp = 'cursor_token_xyz'
                                        act = cursor ).

    " Check meta
    DATA api_version TYPE string.
    api_version = json_result->get_string( '/_meta/apiVersion' ).
    cl_abap_unit_assert=>assert_equals( exp = '2.0'
                                        act = api_version ).
  ENDMETHOD.

ENDCLASS.
