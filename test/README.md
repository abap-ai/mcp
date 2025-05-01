# Tests

This folder contains tests for the MCP SDK developed in TypeScript. It uses the servers from the TEST package.
The tests require a specific setup:

| Area | Server | Class | Session      |
|------|--------|------------------------|--------------|
| test | test_full        | ZCL_MCP_TEST_FULL         | No Session   |
| test | test_icf_session | ZCL_MCP_TEST_ICF_SESSION  | ICF Stateful |
| test | test_mcp_session | ZCL_MCP_TEST_MCP_SESSION  | MCP Session  |
| test | test_minimal     | ZCL_MCP_TEST_MINIMAL      | No Session   |

They also expect a specific path for now. We can make this dynamic to better support different environments in case a larger collaborator base needs this.
