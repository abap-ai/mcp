interface zif_mcp_ajson_filter
  public.

  types ty_filter_tab type standard table of ref to zif_mcp_ajson_filter with key table_line.
  types ty_visit_type type i.

  constants:
    begin of visit_type,
      value type ty_visit_type value 0,
      open  type ty_visit_type value 1,
      close type ty_visit_type value 2,
    end of visit_type.

  methods keep_node
    importing
      is_node type zif_mcp_ajson_types=>ty_node
      iv_visit type ty_visit_type default visit_type-value
    returning
      value(rv_keep) type abap_bool
    raising
      zcx_mcp_ajson_error.

endinterface.
