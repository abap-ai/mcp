CLASS ltcl_resources_response DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    METHODS setup.
    METHODS test_empty_resources FOR TESTING RAISING cx_static_check.
    METHODS test_basic_resources FOR TESTING RAISING cx_static_check.
    METHODS test_full_resources  FOR TESTING RAISING cx_static_check.
    METHODS test_next_cursor     FOR TESTING RAISING cx_static_check.
    METHODS test_meta_data       FOR TESTING RAISING cx_static_check.
    METHODS test_resource_icons  FOR TESTING RAISING cx_static_check.

    DATA cut TYPE REF TO zcl_mcp_resp_list_resources.
ENDCLASS.

CLASS ltcl_resources_response IMPLEMENTATION.

  METHOD setup.
    cut = NEW #( ).
  ENDMETHOD.

  METHOD test_empty_resources.
    " Given
    DATA(resources) = VALUE zcl_mcp_resp_list_resources=>resources( ).
    cut->set_resources( resources ).

    " When

    TRY.
        DATA(ajson) = cut->zif_mcp_internal~generate_json( ).

        " Then
        DATA(json_string) = ajson->stringify( ).
        cl_abap_unit_assert=>assert_not_initial( json_string ).
        cl_abap_unit_assert=>assert_char_cp( exp = '*"resources":[]* '
                                             act = json_string ).

      CATCH zcx_mcp_ajson_error INTO DATA(error).
        cl_abap_unit_assert=>fail( error->get_text( ) ).
    ENDTRY.
  ENDMETHOD.

  METHOD test_basic_resources.
    " Given
    DATA(resources) = VALUE zcl_mcp_resp_list_resources=>resources( ( uri   = 'resource:file1'
                                                                      name  = 'File 1' )
                                                                    ( uri   = 'resource:file2'
                                                                      name  = 'File 2' ) ).

    cut->set_resources( resources ).

    " When

    TRY.
        DATA(ajson) = cut->zif_mcp_internal~generate_json( ).

        " Then - ignore field order, just check for existence
        cl_abap_unit_assert=>assert_equals( exp = 'File 1'
                                            act = ajson->get_string( '/resources/1/name' ) ).
        cl_abap_unit_assert=>assert_equals( exp = 'resource:file1'
                                            act = ajson->get_string( '/resources/1/uri' ) ).
        cl_abap_unit_assert=>assert_equals( exp = 'File 2'
                                            act = ajson->get_string( '/resources/2/name' ) ).
        cl_abap_unit_assert=>assert_equals( exp = 'resource:file2'
                                            act = ajson->get_string( '/resources/2/uri' ) ).

      CATCH zcx_mcp_ajson_error INTO DATA(error).
        cl_abap_unit_assert=>fail( error->get_text( ) ).
    ENDTRY.
  ENDMETHOD.

  METHOD test_full_resources.
    " Given
    DATA(meta) = zcl_mcp_ajson=>create_empty( ).
    meta->set( iv_path = '/blubb~1wuff'
               iv_val  = '1.0.0' ).

    DATA(resources) = VALUE zcl_mcp_resp_list_resources=>resources(
                                ( uri         = 'resource:file1'
                                  name        = 'File 1'
                                  description = 'Description of file 1'
                                  mime_type   = 'application/json'
                                  title       = 'Title of File 1'
                                  size        = 1024
                                  meta        = meta
                                  annotations = VALUE #( audience = VALUE #( ( `user` ) ( `assistant` ) )
                                                         priority = '0.8' ) ) ).

    cut->set_resources( resources ).

    " When

    TRY.
        DATA(ajson) = cut->zif_mcp_internal~generate_json( ).
        DATA(test) = ajson->stringify( ).

        " Then - check individual fields
        cl_abap_unit_assert=>assert_equals( exp = 'resource:file1'
                                            act = ajson->get_string( '/resources/1/uri' ) ).
        cl_abap_unit_assert=>assert_equals( exp = 'File 1'
                                            act = ajson->get_string( '/resources/1/name' ) ).
        cl_abap_unit_assert=>assert_equals( exp = 'Title of File 1'
                                            act = ajson->get_string( '/resources/1/title' ) ).
        cl_abap_unit_assert=>assert_equals( exp = 'Description of file 1'
                                            act = ajson->get_string( '/resources/1/description' ) ).
        cl_abap_unit_assert=>assert_equals( exp = 'application/json'
                                            act = ajson->get_string( '/resources/1/mimeType' ) ).
        cl_abap_unit_assert=>assert_equals( exp = 1024
                                            act = ajson->get_integer( '/resources/1/size' ) ).
        cl_abap_unit_assert=>assert_equals( exp = 'user'
                                            act = ajson->get_string( '/resources/1/annotations/audience/1' ) ).
        cl_abap_unit_assert=>assert_equals( exp = 'assistant'
                                            act = ajson->get_string( '/resources/1/annotations/audience/2' ) ).
        cl_abap_unit_assert=>assert_equals( exp = '0.8'
                                            act = ajson->get_number( '/resources/1/annotations/priority' ) ).
        cl_abap_unit_assert=>assert_equals( exp = '1.0.0'
                                            act = ajson->get_string( '/resources/1/_meta/blubb~1wuff' ) ).

      CATCH zcx_mcp_ajson_error INTO DATA(error).
        cl_abap_unit_assert=>fail( error->get_text( ) ).
    ENDTRY.
  ENDMETHOD.

  METHOD test_next_cursor.
    " Given
    DATA(resources) = VALUE zcl_mcp_resp_list_resources=>resources( ( uri  = 'resource:file1'
                                                                      name = 'File 1' ) ).
    cut->set_resources( resources ).
    cut->set_next_cursor( 'next-page-token-123' ).

    " When

    TRY.
        DATA(ajson) = cut->zif_mcp_internal~generate_json( ).

        " Then
        cl_abap_unit_assert=>assert_equals( exp = 'next-page-token-123'
                                            act = ajson->get_string( '/nextCursor' ) ).

      CATCH zcx_mcp_ajson_error INTO DATA(error).
        cl_abap_unit_assert=>fail( error->get_text( ) ).
    ENDTRY.
  ENDMETHOD.

  METHOD test_meta_data.
    " Given
    DATA(resources) = VALUE zcl_mcp_resp_list_resources=>resources( ( uri  = 'resource:file1'
                                                                      name = 'File 1' ) ).
    cut->set_resources( resources ).

    " Create metadata
    DATA(meta) = zcl_mcp_ajson=>create_empty( ).

    TRY.
        meta->set( iv_path = '/version'
                   iv_val  = '1.0.0' ).
        meta->set( iv_path = '/provider'
                   iv_val  = 'ABAP' ).

        cut->set_meta( meta ).

        " When
        DATA(ajson) = cut->zif_mcp_internal~generate_json( ).

        " Then - check meta fields individually
        cl_abap_unit_assert=>assert_equals( exp = '1.0.0'
                                            act = ajson->get_string( '/_meta/version' ) ).
        cl_abap_unit_assert=>assert_equals( exp = 'ABAP'
                                            act = ajson->get_string( '/_meta/provider' ) ).

      CATCH zcx_mcp_ajson_error INTO DATA(error).
        cl_abap_unit_assert=>fail( error->get_text( ) ).
    ENDTRY.
  ENDMETHOD.

  METHOD test_resource_icons.
    DATA icon1 TYPE zif_mcp_types=>icon.
    icon1-src       = 'https://example.com/res-icon.png'.
    icon1-mime_type = 'image/png'.
    icon1-theme     = 'dark'.
    APPEND '96x96' TO icon1-sizes.

    DATA icon2 TYPE zif_mcp_types=>icon.
    icon2-src = 'https://example.com/res-icon.svg'.
    APPEND 'any' TO icon2-sizes.

    DATA(resources) = VALUE zcl_mcp_resp_list_resources=>resources(
                        ( uri   = 'resource:file1'
                          name  = 'File 1'
                          icons = VALUE #( ( icon1 ) ( icon2 ) ) ) ).

    cut->set_resources( resources ).

    TRY.
        DATA(ajson) = cut->zif_mcp_internal~generate_json( ).

        cl_abap_unit_assert=>assert_true( act = ajson->exists( '/resources/1/icons' )
                                          msg = 'icons array should exist' ).
        cl_abap_unit_assert=>assert_equals( exp = 'https://example.com/res-icon.png'
                                            act = ajson->get_string( '/resources/1/icons/1/src' ) ).
        cl_abap_unit_assert=>assert_equals( exp = 'image/png'
                                            act = ajson->get_string( '/resources/1/icons/1/mimeType' ) ).
        cl_abap_unit_assert=>assert_equals( exp = 'dark'
                                            act = ajson->get_string( '/resources/1/icons/1/theme' ) ).
        cl_abap_unit_assert=>assert_equals( exp = '96x96'
                                            act = ajson->get_string( '/resources/1/icons/1/sizes/1' ) ).
        cl_abap_unit_assert=>assert_equals( exp = 'https://example.com/res-icon.svg'
                                            act = ajson->get_string( '/resources/1/icons/2/src' ) ).
        cl_abap_unit_assert=>assert_false( act = ajson->exists( '/resources/1/icons/2/mimeType' )
                                           msg = 'mimeType absent when not set' ).
        cl_abap_unit_assert=>assert_false( act = ajson->exists( '/resources/1/icons/2/theme' )
                                           msg = 'theme absent when not set' ).

      CATCH zcx_mcp_ajson_error INTO DATA(error).
        cl_abap_unit_assert=>fail( error->get_text( ) ).
    ENDTRY.
  ENDMETHOD.

ENDCLASS.
