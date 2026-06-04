CLASS zcx_mcp_schema_ddic_error DEFINITION
  PUBLIC
  INHERITING FROM cx_static_check FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES if_t100_message.

    CONSTANTS:
      BEGIN OF structure_not_found,
        msgid TYPE symsgid      VALUE 'ZMCP',
        msgno TYPE symsgno      VALUE '200',
        attr1 TYPE scx_attrname VALUE 'STRUCTURE_NAME',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF structure_not_found.
    CONSTANTS:
      BEGIN OF invalid_override,
        msgid TYPE symsgid      VALUE 'ZMCP',
        msgno TYPE symsgno      VALUE '201',
        attr1 TYPE scx_attrname VALUE 'FIELD_PATH',
        attr2 TYPE scx_attrname VALUE 'REASON',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF invalid_override.
    CONSTANTS:
      BEGIN OF processing_error,
        msgid TYPE symsgid      VALUE 'ZMCP',
        msgno TYPE symsgno      VALUE '202',
        attr1 TYPE scx_attrname VALUE 'STRUCTURE_NAME',
        attr2 TYPE scx_attrname VALUE 'FIELD_NAME',
        attr3 TYPE scx_attrname VALUE 'REASON',
        attr4 TYPE scx_attrname VALUE '',
      END OF processing_error.

    DATA structure_name TYPE string READ-ONLY.
    DATA field_name     TYPE string READ-ONLY.
    DATA field_path     TYPE string READ-ONLY.
    DATA reason         TYPE string READ-ONLY.

    "! <p class="shorttext synchronized">Raise exception for structure not found</p>
    CLASS-METHODS raise_structure_not_found
      IMPORTING structure_name TYPE string
      RAISING   zcx_mcp_schema_ddic_error.

    "! <p class="shorttext synchronized">Raise exception for invalid override</p>
    CLASS-METHODS raise_invalid_override
      IMPORTING field_path TYPE string
                reason     TYPE string
      RAISING   zcx_mcp_schema_ddic_error.

    "! <p class="shorttext synchronized">Raise exception for processing error</p>
    CLASS-METHODS raise_processing_error
      IMPORTING structure_name TYPE string
                field_name     TYPE string OPTIONAL
                reason         TYPE string
      RAISING   zcx_mcp_schema_ddic_error.

    "! <p class="shorttext synchronized">Constructor</p>
    METHODS constructor
      IMPORTING textid         LIKE if_t100_message=>t100key OPTIONAL
                !previous      LIKE previous                 OPTIONAL
                structure_name TYPE string                   OPTIONAL
                field_name     TYPE string                   OPTIONAL
                field_path     TYPE string                   OPTIONAL
                reason         TYPE string                   OPTIONAL.
ENDCLASS.



CLASS ZCX_MCP_SCHEMA_DDIC_ERROR IMPLEMENTATION.
  METHOD constructor ##ADT_SUPPRESS_GENERATION.
    super->constructor( previous = previous ).

    me->structure_name = structure_name.
    me->field_name     = field_name.
    me->field_path     = field_path.
    me->reason         = reason.

    CLEAR me->textid.
    IF textid IS INITIAL.
      if_t100_message~t100key = if_t100_message=>default_textid.
    ELSE.
      if_t100_message~t100key = textid.
    ENDIF.
  ENDMETHOD.

  METHOD raise_invalid_override.
    RAISE EXCEPTION NEW zcx_mcp_schema_ddic_error( textid     = invalid_override
                                                   field_path = field_path
                                                   reason     = reason ).
  ENDMETHOD.

  METHOD raise_processing_error.
    RAISE EXCEPTION NEW zcx_mcp_schema_ddic_error( textid         = processing_error
                                                   structure_name = structure_name
                                                   field_name     = field_name
                                                   reason         = reason ).
  ENDMETHOD.

  METHOD raise_structure_not_found.
    RAISE EXCEPTION NEW zcx_mcp_schema_ddic_error( textid         = structure_not_found
                                                   structure_name = structure_name ).
  ENDMETHOD.
ENDCLASS.
