"! <p class="shorttext synchronized" lang="en">Exceptions during MCP processing</p>
class ZCX_MCP_SERVER definition
  public
  inheriting from CX_STATIC_CHECK
  final
  create public .

public section.

  interfaces IF_T100_DYN_MSG .
  interfaces IF_T100_MESSAGE .

  constants:
    BEGIN OF prompt_name_invalid,
        msgid TYPE symsgid      VALUE 'ZMCP',
        msgno TYPE symsgno      VALUE '001',
        attr1 TYPE scx_attrname VALUE 'MSGV1',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF prompt_name_invalid .
  constants:
    BEGIN OF internal_error,
        msgid TYPE symsgid      VALUE 'ZMCP',
        msgno TYPE symsgno      VALUE '003',
        attr1 TYPE scx_attrname VALUE 'MSGV1',
        attr2 TYPE scx_attrname VALUE 'MSGV2',
        attr3 TYPE scx_attrname VALUE 'MSGV3',
        attr4 TYPE scx_attrname VALUE 'MSGV4',
      END OF internal_error .
  constants:
    BEGIN OF required_params,
        msgid TYPE symsgid      VALUE 'ZMCP',
        msgno TYPE symsgno      VALUE '002',
        attr1 TYPE scx_attrname VALUE 'MSGV1',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF required_params .
  constants:
    begin of RESOURCE_NOT_FOUND,
      msgid type symsgid value 'ZMCP',
      msgno type symsgno value '004',
      attr1 type scx_attrname value 'MSGV1',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of RESOURCE_NOT_FOUND .
  constants:
    begin of JS_UNSUPPORTED_TYPE,
      msgid type symsgid value 'ZMCP',
      msgno type symsgno value '005',
      attr1 type scx_attrname value 'MSGV1',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of JS_UNSUPPORTED_TYPE .
  constants:
    begin of JS_GENERATION_ERROR,
      msgid type symsgid value 'ZMCP',
      msgno type symsgno value '006',
      attr1 type scx_attrname value 'MSGV1',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of JS_GENERATION_ERROR .
  constants:
    begin of JS_INVALID_STRUCTURE,
      msgid type symsgid value 'ZMCP',
      msgno type symsgno value '007',
      attr1 type scx_attrname value 'MSGV1',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of JS_INVALID_STRUCTURE .
  constants:
    begin of JS_UNMAPPED_DESCRIPTIONS,
      msgid type symsgid value 'ZMCP',
      msgno type symsgno value '008',
      attr1 type scx_attrname value 'MSGV1',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of JS_UNMAPPED_DESCRIPTIONS .
  constants:
    begin of UNKNOWN_TOOL,
      msgid type symsgid value 'ZMCP',
      msgno type symsgno value '009',
      attr1 type scx_attrname value 'MSGV1',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of UNKNOWN_TOOL .
  constants:
    begin of INVALID_ARGUMENTS,
      msgid type symsgid value 'ZLLM_MCP',
      msgno type symsgno value '010',
      attr1 type scx_attrname value 'MSGV1',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of INVALID_ARGUMENTS .
  constants:
    begin of SESSION_EXPIRED,
      msgid type symsgid value 'ZLLM_MCP',
      msgno type symsgno value '011',
      attr1 type scx_attrname value 'MSGV1',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of SESSION_EXPIRED .
  constants:
    begin of SESSION_UNKNOWN,
      msgid type symsgid value 'ZLLM_MCP',
      msgno type symsgno value '012',
      attr1 type scx_attrname value 'MSGV1',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of SESSION_UNKNOWN .
  constants:
    begin of SESSION_LOAD_ERROR,
      msgid type symsgid value 'ZLLM_MCP',
      msgno type symsgno value '013',
      attr1 type scx_attrname value 'MSGV1',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of SESSION_LOAD_ERROR .
  constants:
    begin of SESSION_SAVE_ERROR,
      msgid type symsgid value 'ZLLM_MCP',
      msgno type symsgno value '014',
      attr1 type scx_attrname value 'MSGV1',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of SESSION_SAVE_ERROR .
  data MSGV1 type SYMSGV .
  data MSGV2 type SYMSGV .
  data MSGV3 type SYMSGV .
  data MSGV4 type SYMSGV .

  methods CONSTRUCTOR
    importing
      !TEXTID like IF_T100_MESSAGE=>T100KEY optional
      !PREVIOUS like PREVIOUS optional
      !MSGV1 type SYMSGV optional
      !MSGV2 type SYMSGV optional
      !MSGV3 type SYMSGV optional
      !MSGV4 type SYMSGV optional .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCX_MCP_SERVER IMPLEMENTATION.


  METHOD constructor ##ADT_SUPPRESS_GENERATION.
    super->constructor( previous = previous ).
    CLEAR me->textid.
    IF textid IS INITIAL.
      if_t100_message~t100key = if_t100_message=>default_textid.
    ELSE.
      if_t100_message~t100key = textid.
    ENDIF.
    me->msgv1 = msgv1.
    me->msgv2 = msgv2.
    me->msgv3 = msgv3.
    me->msgv4 = msgv4.
  ENDMETHOD.
ENDCLASS.
