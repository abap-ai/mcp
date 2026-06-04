INTERFACE zif_mcp_types
  PUBLIC.

  " Conversation role - "user" | "assistant"
  TYPES message_role TYPE string.

  CONSTANTS role_user      TYPE message_role VALUE 'user'.
  CONSTANTS role_assistant TYPE message_role VALUE 'assistant'.

  " Optional annotations attached to content blocks, resources,
  " resource templates, and resource links.
  " last_modified is stored as ABAP timestamp and serialised to
  " ISO 8601 by the response classes.
  TYPES: BEGIN OF annotations,
           audience      TYPE STANDARD TABLE OF message_role WITH DEFAULT KEY,
           priority      TYPE decfloat16,
           last_modified TYPE timestamp,
         END OF annotations.

  " Icon entry (MCP 2025-11-25).
  " Used in Implementation, Tool, Resource, ResourceTemplate, Prompt.
  " theme: 'light' | 'dark' | initial means suitable for any theme.
  TYPES: BEGIN OF icon,
           src       TYPE string,
           mime_type TYPE string,
           sizes     TYPE STANDARD TABLE OF string WITH DEFAULT KEY,
           theme     TYPE string,
         END OF icon.

  TYPES icon_list TYPE STANDARD TABLE OF icon WITH DEFAULT KEY.

  " Opaque pagination cursor - used in all paginated
  " request and response classes.
  TYPES page_cursor TYPE string.

  " Task execution state (MCP 2025-11-25).
  TYPES task_state TYPE string.

  CONSTANTS: BEGIN OF task_states,
               working        TYPE task_state VALUE 'working',
               completed      TYPE task_state VALUE 'completed',
               failed         TYPE task_state VALUE 'failed',
               cancelled      TYPE task_state VALUE 'cancelled',
             END OF task_states.

  " Task data (MCP 2025-11-25).
  TYPES: BEGIN OF task,
           task_id        TYPE string,
           status         TYPE task_state,
           status_message TYPE string,
           created_at     TYPE timestamp,
           last_updated   TYPE timestamp,
           ttl            TYPE i,
           ttl_is_null    TYPE abap_bool,
           poll_interval  TYPE i,
         END OF task.

  TYPES: task_list TYPE STANDARD TABLE OF task WITH DEFAULT KEY.

  TYPES: BEGIN OF task_list_result,
           tasks       TYPE task_list,
           next_cursor TYPE page_cursor,
         END OF task_list_result.

ENDINTERFACE.
