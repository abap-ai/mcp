# Model Context Protocol Server - ABAP SDK

This documentation provides a comprehensive guide to the Model Context Protocol (MCP) Server SDK for ABAP. It explains how to install, configure, and implement custom MCP servers.

## Table of Contents

- [Overview](#overview)
- [Installation](#installation)
- [Configuration](#configuration)
- [Implementing Custom MCP Servers](#implementing-custom-mcp-servers)
- [Session Management](#session-management)
- [Core Components](#core-components)
- [Demo Implementations](#demo-implementations)
- [API Reference](#api-reference)

## Overview

The Model Context Protocol (MCP) Server SDK for ABAP allows you to create custom AI integration servers that can be accessed by MCP clients. The SDK provides:

- A standardized interface for MCP communication
- Base classes for quick implementation
- Session handling capabilities
- Tools for schema definition
- JSON-RPC handling

With the MCP Server SDK, you can implement servers that provide:

- Prompts for AI models
- Access to resources (like files, database records, etc.)
- Custom tools that AI models can call

## Authorizations

The authorization object `ZMCP_SRV` is used to check if you area allowed to call a specific server. The fields `ZMCP_AREA` and `ZMCP_SRV` match the area and server in `ZMCP_SERVERS` table.

## Installation

To install the MCP Server SDK, follow these steps:

1. Import the SDK objects into your SAP system via abapGit
2. Set up the ICF node for your server endpoint \
    Create a new node e.g. zmcp with handler class zcl_mcp_http_handler
3. Setup server configuration, e.g. create an entry for the stateless demo server zcl_mcp_demo_server_stateless

### Prerequisites

- SAP NetWeaver 7.52 or higher for the main branch
- SAP Netweaver 7.02 or higher for the downport branch (702) - **not tested** as I have no system below 7.50 available, open issues if required
- Authorization to create and modify ABAP classes
- Authorization to create ICF service nodes

## Configuration

### Server Configuration Table

Configure your MCP servers in the `ZMCP_SERVERS` table:

| Field        | Description                                                                            |
| ------------ | -------------------------------------------------------------------------------------- |
| AREA         | Logical grouping for servers                                                           |
| SERVER       | Server identifier                                                                      |
| CLASS        | Implementation class (must implement `ZIF_MCP_SERVER` or extend `ZCL_MCP_SERVER_BASE`) |
| SESSION_MODE | Session handling mode (`No Session`, `MCP`, or `ICF`)                                  |

Use the area to group your servers based on a well known structure in your company. This can e.g. be modules or end to end processes. For the session mode No Session is recommended except you really require session management.

### CORS Configuration

Configure CORS settings in the `ZMCP_CONFIG` table:

| Field     | Description                                          |
| --------- | ---------------------------------------------------- |
| CORS_MODE | CORS handling mode (`Ignore`, `Check`, or `Enforce`) |

Ignore = no validation, Check = if Origin header is present it is validated against maintained origins, Enforce ensures that the Origin header is present and checks against maintained origins. For whitelisted origins, maintain entries in the `ZMCP_ORIGINS` table. Use * in area and/or server for generic whitelists.

### Logging Configuration

Configure logging in the `ZMCP_CONFIG` table:

| Field     | Description               |
| --------- | ------------------------- |
| LOG_LEVEL | Logging detail level      |
| OBJECT    | Application log object    |
| SUBOBJECT | Application log subobject |

Make sure to create the Object/Subobject in SLG0. By default no logging happens. Note in case of incorrect object and subobject loggin will fail silently and not dump the server.

## Implementing Custom MCP Servers

To implement a custom MCP server, we strongly recommend extending `ZCL_MCP_SERVER_BASE` rather than directly implementing the `ZIF_MCP_SERVER` interface. This approach provides you with pre-implemented session handling and error management.

### Basic Implementation Pattern

```abap
CLASS zcl_my_custom_server DEFINITION
  PUBLIC
  INHERITING FROM zcl_mcp_server_base
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
  PROTECTED SECTION.
    METHODS handle_initialize REDEFINITION.
    " Implement only the handlers you need
    METHODS handle_list_tools REDEFINITION.
    METHODS handle_call_tool REDEFINITION.
    " Other handlers...
  
  PRIVATE SECTION.
    " Your helper methods
ENDCLASS.
```

### Minimal Implementation

At minimum, you must implement the `HANDLE_INITIALIZE` method to define your server's capabilities:

```abap
METHOD handle_initialize.
  response-result->set_capabilities( VALUE #( 
    prompts   = abap_false
    resources = abap_false
    tools     = abap_true 
  ) ).
  response-result->set_implementation( VALUE #( 
    name    = `My Custom MCP Server`
    version = `1.0.0` 
  ) ).
  response-result->set_instructions(
    `Instructions for the AI model on when to use this server...` 
  ).
ENDMETHOD.
```

## Session Management

The MCP Server SDK offers three session management modes:

| Mode        | Description                                                 |
| ----------- | ----------------------------------------------------------- |
| No Session  | No session management (stateless)                           |
| MCP Session | Uses the custom MCP session handling mechanism via database |
| ICF Session | Uses the standard ICF session management                    |

Note that ICF session management leads to potentially high number of sessions if the clients do not properly close them. Also your MCP client must support handling the session cookies. MCP Sessions are an alternative lightweight mode allowing you to store certain values in the DB between calls. In general use No Session --> Stateless where feasible.

## Core Components

### ZCL_MCP_SERVER_BASE

Abstract base class for implementing MCP servers. It handles:

- Session management
- Error handling
- Protocol conformance

### ZIF_MCP_SERVER

Interface defining all required MCP server methods. The main methods include:

- `initialize` - Server initialization and capability declaration
- `prompts_list` - List available prompts
- `prompts_get` - Get prompt details
- `resources_list` - List available resources
- `resources_read` - Read resource content
- `tools_list` - List available tools
- `tools_call` - Execute tool function

### ZCL_MCP_SCHEMA_BUILDER

Helper class to build JSON schemas for tool input validation. You can call it multiple times like below or fully chain it.

```abap
DATA(schema) = NEW zcl_mcp_schema_builder( ).
schema->add_string( name = 'parameter' required = abap_true ).
schema->add_integer( name = 'count' ).
schema->begin_object( name = 'options' ).
schema->add_boolean( name = 'flag' ).
schema->end_object( ).
```

For details see [Schema Builder](SchemaBuilder.md).

## Demo Implementations

The SDK includes three demo implementations:

### ZCL_MCP_DEMO_SERVER_STATELESS

A stateless MCP server demonstrating:

- Simple prompt handling
- Resource access
- Tool implementation (server time and flight connection details)

### ZCL_MCP_DEMO_SERVER_MCPSESSION

Demonstrates MCP session handling with:

- Session information tool
- Incremental counter tool that persists state between calls

### ZCL_MCP_DEMO_SERVER_ICFSESSION

Demonstrates ICF session handling with:

- Session information tool
- Instance variables that persist state between calls

### Demo Configuration

| Area | Service             | Class                             | SessionMode   |
|------|---------------------|-----------------------------------|---------------|
| demo | demo_session_icf    | ZCL_MCP_DEMO_SERVER_ICFSESSION    | ICF Stateful  |
| demo | demo_session_mcp    | ZCL_MCP_DEMO_SERVER_MCPSESSION    | MCP Session   |
| demo | demo_standard       | ZCL_MCP_DEMO_SERVER_STATELESS     | No Session    |

## Usage/Clients

At the time of writing this clients supporting HTTP Streamable protocol are still extremely rare and most official MCP SDKs do not yet fully support it. The [MCP Inspector](https://github.com/modelcontextprotocol/inspector) is currently a recommended testing tool.

## API Reference

### Handler Methods

| Method                  | Description                          |
| ----------------------- | ------------------------------------ |
| `handle_initialize`     | Required: Set up server capabilities |
| `handle_list_prompts`   | List available prompts               |
| `handle_get_prompt`     | Retrieve specific prompt details     |
| `handle_list_resources` | List available resources             |
| `handle_resources_read` | Read resource content                |
| `handle_list_res_tmpls` | List resource templates              |
| `handle_list_tools`     | List available tools                 |
| `handle_call_tool`      | Execute a tool                       |

### Key Data Types

| Type                      | Description                         |
| ------------------------- | ----------------------------------- |
| `initialize_response`     | Server capabilities and information |
| `list_prompts_response`   | Collection of available prompts     |
| `get_prompt_response`     | Details of a specific prompt        |
| `list_resources_response` | Collection of available resources   |
| `resources_read_response` | Content of a specific resource      |
| `list_tools_response`     | Collection of available tools       |
| `call_tool_response`      | Result of tool execution            |

### Request/Response Classes

See the relevant subpages:

- [Resources](Resources.md)
- [Prompts](Prompts.md)
- [Tools](Tools.md)

### Server Properties

| Property  | Description                     |
| --------- | ------------------------------- |
| `server`  | Server context information      |
| `config`  | Server configuration            |
| `session` | Session handling (when enabled) |

---

For more detailed information, examples, and advanced usage, refer to the individual component documentation pages.
