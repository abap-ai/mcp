class ZCX_MCP_SCHEMA_DDIC_ERROR definition
  public
  inheriting from CX_STATIC_CHECK
  final
  create public .

public section.

  interfaces IF_T100_MESSAGE .

  constants:
    begin of STRUCTURE_NOT_FOUND,
      msgid type symsgid value 'ZMCP',
      msgno type symsgno value '200',
      attr1 type scx_attrname value 'STRUCTURE_NAME',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of STRUCTURE_NOT_FOUND .
  constants:
    begin of INVALID_OVERRIDE,
      msgid type symsgid value 'ZMCP',
      msgno type symsgno value '201',
      attr1 type scx_attrname value 'FIELD_PATH',
      attr2 type scx_attrname value 'REASON',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of INVALID_OVERRIDE .
  constants:
    begin of PROCESSING_ERROR,
      msgid type symsgid value 'ZMCP',
      msgno type symsgno value '202',
      attr1 type scx_attrname value 'STRUCTURE_NAME',
      attr2 type scx_attrname value 'FIELD_NAME',
      attr3 type scx_attrname value 'REASON',
      attr4 type scx_attrname value '',
    end of PROCESSING_ERROR .
  data STRUCTURE_NAME type STRING read-only .
  data FIELD_NAME type STRING read-only .
  data FIELD_PATH type STRING read-only .
  data REASON type STRING read-only .

    "! <p class="shorttext synchronized">Raise exception for structure not found</p>
  class-methods RAISE_STRUCTURE_NOT_FOUND
    importing
      !STRUCTURE_NAME type STRING
    raising
      ZCX_MCP_SCHEMA_DDIC_ERROR .
    "! <p class="shorttext synchronized">Raise exception for invalid override</p>
  class-methods RAISE_INVALID_OVERRIDE
    importing
      !FIELD_PATH type STRING
      !REASON type STRING
    raising
      ZCX_MCP_SCHEMA_DDIC_ERROR .
    "! <p class="shorttext synchronized">Raise exception for processing error</p>
  class-methods RAISE_PROCESSING_ERROR
    importing
      !STRUCTURE_NAME type STRING
      !FIELD_NAME type STRING optional
      !REASON type STRING
    raising
      ZCX_MCP_SCHEMA_DDIC_ERROR .
    "! <p class="shorttext synchronized">Constructor</p>
  methods CONSTRUCTOR
    importing
      !TEXTID like IF_T100_MESSAGE=>T100KEY optional
      !PREVIOUS like PREVIOUS optional
      !STRUCTURE_NAME type STRING optional
      !FIELD_NAME type STRING optional
      !FIELD_PATH type STRING optional
      !REASON type STRING optional .
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
    RAISE EXCEPTION TYPE zcx_mcp_schema_ddic_error
      EXPORTING textid     = invalid_override
                field_path = field_path
                reason     = reason.
  ENDMETHOD.


  METHOD raise_processing_error.
    RAISE EXCEPTION TYPE zcx_mcp_schema_ddic_error
      EXPORTING textid         = processing_error
                structure_name = structure_name
                field_name     = field_name
                reason         = reason.
  ENDMETHOD.


  METHOD raise_structure_not_found.
    RAISE EXCEPTION TYPE zcx_mcp_schema_ddic_error
      EXPORTING textid         = structure_not_found
                structure_name = structure_name.
  ENDMETHOD.
ENDCLASS.
