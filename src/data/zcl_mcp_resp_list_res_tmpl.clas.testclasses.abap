CLASS ltcl_list_resource_templates DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA cut  TYPE REF TO zcl_mcp_resp_list_res_tmpl.
    DATA json TYPE REF TO zif_mcp_ajson.

    METHODS setup.
    METHODS test_simple_template          FOR TESTING RAISING zcx_mcp_ajson_error.
    METHODS test_template_with_all_fields FOR TESTING RAISING zcx_mcp_ajson_error.
    METHODS test_with_next_cursor         FOR TESTING RAISING zcx_mcp_ajson_error.
    METHODS test_with_meta                FOR TESTING RAISING zcx_mcp_ajson_error.
    METHODS test_empty_templates          FOR TESTING RAISING zcx_mcp_ajson_error.

    METHODS assert_path_exists
      IMPORTING !path TYPE string.

    METHODS assert_path_equals
      IMPORTING !path  TYPE string
                !value TYPE any.
ENDCLASS.

CLASS ltcl_list_resource_templates IMPLEMENTATION.
  METHOD setup.
    cut = NEW zcl_mcp_resp_list_res_tmpl( ).
    json = zcl_mcp_ajson=>create_empty( ).
  ENDMETHOD.

  METHOD test_simple_template.
    " Arrange
    DATA templates TYPE zcl_mcp_resp_list_res_tmpl=>resource_templates.
    DATA template  TYPE zcl_mcp_resp_list_res_tmpl=>resource_template.

    template-uritemplate = '/api/resources/{id}'.
    template-name        = 'Simple Resource'.
    APPEND template TO templates.

    cut->set_resource_templates( templates ).

    " Act
    json = cut->zif_mcp_internal~generate_json( ).

    " Assert
    assert_path_exists( '/resourceTemplates' ).
    assert_path_equals( path  = '/resourceTemplates/1/uriTemplate'
                        value = '/api/resources/{id}' ).
    assert_path_equals( path  = '/resourceTemplates/1/name'
                        value = 'Simple Resource' ).
  ENDMETHOD.

  METHOD test_template_with_all_fields.
    " Arrange
    DATA templates TYPE zcl_mcp_resp_list_res_tmpl=>resource_templates.
    DATA template  TYPE zcl_mcp_resp_list_res_tmpl=>resource_template.
    DATA audiences TYPE STANDARD TABLE OF string.

    APPEND 'user' TO audiences.
    APPEND 'assistant' TO audiences.

    template-uritemplate = '/api/documents/{type}/{id}'.
    template-name        = 'Document Template'.
    template-title       = 'Document Title'.
    template-description = 'Templates for various document types'.
    template-mime_type   = 'application/pdf'.
    template-annotations-audience = audiences.
    template-annotations-priority = '0.8'.

    APPEND template TO templates.
    cut->set_resource_templates( templates ).

    " Act
    json = cut->zif_mcp_internal~generate_json( ).

    " Assert
    assert_path_exists( '/resourceTemplates' ).
    assert_path_equals( path  = '/resourceTemplates/1/uriTemplate'
                        value = '/api/documents/{type}/{id}' ).
    assert_path_equals( path  = '/resourceTemplates/1/name'
                        value = 'Document Template' ).
    assert_path_equals( path  = '/resourceTemplates/1/description'
                        value = 'Templates for various document types' ).
    assert_path_equals( path  = '/resourceTemplates/1/mimeType'
                        value = 'application/pdf' ).
    assert_path_exists( '/resourceTemplates/1/annotations/audience' ).
    assert_path_equals( path  = '/resourceTemplates/1/annotations/audience/1'
                        value = 'user' ).
    assert_path_equals( path  = '/resourceTemplates/1/annotations/audience/2'
                        value = 'assistant' ).
    assert_path_equals( path  = '/resourceTemplates/1/annotations/priority'
                        value = '0.8' ).
    assert_path_equals( path  = '/resourceTemplates/1/title'
                        value = 'Document Title' ).
  ENDMETHOD.

  METHOD test_with_next_cursor.
    " Arrange
    DATA templates TYPE zcl_mcp_resp_list_res_tmpl=>resource_templates.
    DATA template  TYPE zcl_mcp_resp_list_res_tmpl=>resource_template.

    template-uritemplate = '/api/resources/{id}'.
    template-name        = 'Resource Template'.
    APPEND template TO templates.

    cut->set_resource_templates( templates ).
    cut->set_next_cursor( 'eyJsYXN0SWQiOiAiMTAwIn0=' ).

    " Act
    json = cut->zif_mcp_internal~generate_json( ).

    " Assert
    assert_path_exists( '/resourceTemplates' ).
    assert_path_equals( path  = '/nextCursor'
                        value = 'eyJsYXN0SWQiOiAiMTAwIn0=' ).
  ENDMETHOD.

  METHOD test_with_meta.
    " Arrange
    DATA templates TYPE zcl_mcp_resp_list_res_tmpl=>resource_templates.
    DATA template  TYPE zcl_mcp_resp_list_res_tmpl=>resource_template.
    DATA meta      TYPE REF TO zif_mcp_ajson.

    template-uritemplate = '/api/resources/{id}'.
    template-name        = 'Resource Template'.
    APPEND template TO templates.

    cut->set_resource_templates( templates ).

    " Create meta data
    meta = zcl_mcp_ajson=>create_empty( ).
    meta->set( iv_path = '/version'
               iv_val  = '1.0' ).
    meta->set( iv_path = '/server'
               iv_val  = 'test-server' ).

    cut->set_meta( meta ).

    " Act
    json = cut->zif_mcp_internal~generate_json( ).

    " Assert
    assert_path_exists( '/resourceTemplates' ).
    assert_path_exists( '/_meta' ).
    assert_path_equals( path  = '/_meta/version'
                        value = '1.0' ).
    assert_path_equals( path  = '/_meta/server'
                        value = 'test-server' ).
  ENDMETHOD.

  METHOD test_empty_templates.
    " Arrange
    DATA templates TYPE zcl_mcp_resp_list_res_tmpl=>resource_templates.

    cut->set_resource_templates( templates ).

    " Act
    json = cut->zif_mcp_internal~generate_json( ).

    " Assert
    assert_path_exists( '/resourceTemplates' ).

    " Verify array exists and has the correct type
    cl_abap_unit_assert=>assert_equals( exp = 'array'
                                        act = json->get_node_type( '/resourceTemplates' )
                                        msg = 'Templates should be an array' ).

    " Check if the array is empty using members count
    DATA(members) = json->members( '/resourceTemplates' ).
    DATA(count) = lines( members ).

    cl_abap_unit_assert=>assert_equals( exp = 0
                                        act = count
                                        msg = 'Templates array should be empty' ).
  ENDMETHOD.

  METHOD assert_path_exists.
    cl_abap_unit_assert=>assert_true( act = json->exists( path )
                                      msg = |Path { path } should exist| ).
  ENDMETHOD.

  METHOD assert_path_equals.
    cl_abap_unit_assert=>assert_equals(
      act = json->get( path )
      exp = value
      msg = |Path { path } should equal expected value| ).
  ENDMETHOD.
ENDCLASS.
