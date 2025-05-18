*&---------------------------------------------------------------------*
*& Report zmcp_clear_mcp_sessions
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zmcp_clear_mcp_sessions.

DATA(deleted_sessions) = zcl_mcp_session=>delete_outdated_sessions( ).

START-OF-SELECTION.
  WRITE: / 'Deleted sessions:', deleted_sessions.
