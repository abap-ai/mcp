  REPORT zmcp_clear_req_nonces.

  START-OF-SELECTION.
    DATA(deleted_nonces) = zcl_mcp_req_state=>delete_expired_nonces( ).
    WRITE: / 'Deleted requestState nonces:', deleted_nonces.
