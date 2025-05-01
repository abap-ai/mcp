"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
const index_js_1 = require("@modelcontextprotocol/sdk/client/index.js");
const streamableHttp_js_1 = require("@modelcontextprotocol/sdk/client/streamableHttp.js");
describe('MCP Server Session Tests', () => {
    const baseUrl = new URL("http://localhost:8000/zmcp");
    let client;
    let transport;
    beforeEach(async () => {
        client = new index_js_1.Client({
            name: 'test-client',
            version: '1.0.0'
        });
        const sessionServer = "/test/test_mcp_session";
        transport = new streamableHttp_js_1.StreamableHTTPClientTransport(new URL(baseUrl + sessionServer));
        await client.connect(transport);
    });
    afterEach(async () => {
        if (client) {
            await client.close();
        }
    });
    test('List tools should return increment tool', async () => {
        const tools = (await client.listTools()).tools;
        expect(tools).toHaveLength(1);
        const incrementTool = tools[0];
        expect(incrementTool.name).toBe("Test MCP Session");
        expect(incrementTool.description).toBe("Using MCP sessions we increment by the given number");
        expect(incrementTool.inputSchema).toBeDefined();
    });
    test('Should increment value in session', async () => {
        // First call should start from 0
        const result1 = await client.callTool({
            name: "Test MCP Session",
            arguments: {
                increment: 5
            }
        });
        const content1 = result1.content;
        expect(content1[0].text).toBe("Incremented value: 5");
        // Second call should use previous value
        const result2 = await client.callTool({
            name: "Test MCP Session",
            arguments: {
                increment: 3
            }
        });
        const content2 = result2.content;
        expect(content2[0].text).toBe("Incremented value: 8");
    });
    test('Should fail when increment value is missing', async () => {
        await expect(client.callTool({
            name: "Test MCP Session",
            arguments: {}
        })).rejects.toThrow("Increment value is required.");
    });
    test('Different client should start with fresh session', async () => {
        // First client increments to 5
        const result1 = await client.callTool({
            name: "Test MCP Session",
            arguments: {
                increment: 5
            }
        });
        const content1 = result1.content;
        expect(content1[0].text).toBe("Incremented value: 5");
        // Create new transport and client
        const newTransport = new streamableHttp_js_1.StreamableHTTPClientTransport(new URL(baseUrl + "/test/test_mcp_session"));
        const newClient = new index_js_1.Client({
            name: 'test-client-2',
            version: '1.0.0'
        });
        await newClient.connect(newTransport);
        // New client should start from 0
        const result2 = await newClient.callTool({
            name: "Test MCP Session",
            arguments: {
                increment: 3
            }
        });
        const content2 = result2.content;
        expect(content2[0].text).toBe("Incremented value: 3");
        await newClient.close();
    });
});
//# sourceMappingURL=session_mcp.test.js.map