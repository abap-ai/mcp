CLASS zcl_mcp_req_get_task DEFINITION
  PUBLIC FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    "! <p class="shorttext synchronized">Constructor</p>
    "! Parses the tasks/get request params
    "!
    "! @parameter json | <p class="shorttext synchronized">JSON params</p>
    METHODS constructor
      IMPORTING json TYPE REF TO zif_mcp_ajson
      RAISING   zcx_mcp_ajson_error
                zcx_mcp_server.

    "! <p class="shorttext synchronized">Get task ID</p>
    "!
    "! @parameter result | <p class="shorttext synchronized">Task ID</p>
    METHODS get_task_id
      RETURNING VALUE(result) TYPE string.

    "! <p class="shorttext synchronized">Get _meta fields</p>
    "!
    "! @parameter result | <p class="shorttext synchronized">_meta JSON</p>
    METHODS get_meta
      RETURNING VALUE(result) TYPE REF TO zif_mcp_ajson.

  PRIVATE SECTION.
    DATA int_task_id TYPE string.
    DATA int_meta    TYPE REF TO zif_mcp_ajson.
ENDCLASS.

CLASS zcl_mcp_req_get_task IMPLEMENTATION.
  METHOD constructor.
      DATA temp1 TYPE REF TO zcx_mcp_server.
      DATA temp2 TYPE symsgv.
      DATA temp4 TYPE REF TO zcx_mcp_server.
      DATA temp3 TYPE symsgv.
      DATA temp5 TYPE REF TO zcx_mcp_server.
    IF json->exists( '/taskId' ) IS NOT INITIAL.
      int_task_id = json->get_string( '/taskId' ).
    ENDIF.

    IF int_task_id IS INITIAL.
      
      CREATE OBJECT temp1 TYPE zcx_mcp_server EXPORTING textid = zcx_mcp_server=>required_params msgv1 = 'taskId'.
      RAISE EXCEPTION temp1.
    ENDIF.

    IF strlen( int_task_id ) <> 32.
      
      temp2 = |Invalid taskId: { int_task_id }|.
      
      CREATE OBJECT temp4 TYPE zcx_mcp_server EXPORTING textid = zcx_mcp_server=>invalid_arguments msgv1 = temp2.
      RAISE EXCEPTION temp4 ##NO_TEXT.
    ENDIF.

    FIND REGEX '^[0-9A-Fa-f]{32}$' IN int_task_id.
    IF sy-subrc <> 0.
      
      temp3 = |Invalid taskId: { int_task_id }|.
      
      CREATE OBJECT temp5 TYPE zcx_mcp_server EXPORTING textid = zcx_mcp_server=>invalid_arguments msgv1 = temp3.
      RAISE EXCEPTION temp5 ##NO_TEXT.
    ENDIF.

    TRANSLATE int_task_id TO UPPER CASE.

    IF json->exists( '/_meta' ) IS NOT INITIAL.
      int_meta = json->slice( '/_meta' ).
    ELSE.
      int_meta = zcl_mcp_ajson=>create_empty( ).
    ENDIF.
  ENDMETHOD.


  METHOD get_task_id.
    result = int_task_id.
  ENDMETHOD.

  METHOD get_meta.
    result = int_meta.
  ENDMETHOD.
ENDCLASS.
