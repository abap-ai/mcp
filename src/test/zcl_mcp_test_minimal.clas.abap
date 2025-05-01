CLASS zcl_mcp_test_minimal DEFINITION
  PUBLIC
  INHERITING FROM zcl_mcp_server_base FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.

  PROTECTED SECTION.
    METHODS: handle_initialize REDEFINITION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_mcp_test_minimal IMPLEMENTATION.
  METHOD handle_initialize.
    DATA temp1 TYPE zcl_mcp_resp_initialize=>implementation.
    CLEAR temp1.
    temp1-name = `Simple Test Server with no features`.
    temp1-version = `1.0`.
    response-result->set_implementation( temp1 ) ##NO_TEXT.
  ENDMETHOD.

ENDCLASS.
