*&---------------------------------------------------------------------*
*& Report zmcp_clear_mcp_sessions
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zmcp_clear_mcp_sessions.

START-OF-SELECTION.
  DATA deleted_sessions TYPE i.
  deleted_sessions = zcl_mcp_session=>delete_outdated_sessions( ).
  WRITE: / 'Deleted sessions:'(001), deleted_sessions.
