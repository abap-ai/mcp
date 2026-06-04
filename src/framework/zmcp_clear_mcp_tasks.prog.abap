*&---------------------------------------------------------------------*
*& Report zmcp_clear_mcp_tasks
*&---------------------------------------------------------------------*
*& Cleans up outdated MCP tasks. Schedule as a periodic background job.
*& Removes:
*&   - Terminal tasks (completed/failed/cancelled) whose TTL has elapsed
*&   - Terminal tasks with no TTL older than 7 days
*&   - Stuck working tasks older than 24 hours
*&---------------------------------------------------------------------*
REPORT zmcp_clear_mcp_tasks.

START-OF-SELECTION.
  DATA deleted_tasks TYPE i.
  deleted_tasks = zcl_mcp_tasks=>delete_outdated_tasks( ).
  WRITE: / 'Deleted tasks:'(001), deleted_tasks.
