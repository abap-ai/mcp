"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
const index_js_1 = require("@modelcontextprotocol/sdk/client/index.js");
const streamableHttp_js_1 = require("@modelcontextprotocol/sdk/client/streamableHttp.js");
describe('MCP Server Capabilities Tests', () => {
    const baseUrl = new URL("http://localhost:8000/zmcp");
    let client;
    let transport;
    beforeEach(() => {
        client = new index_js_1.Client({
            name: 'test-client',
            version: '1.0.0'
        });
    });
    afterEach(async () => {
        if (client) {
            await client.close();
        }
    });
    test('minimal server should have no capabilities but valid name and version', async () => {
        const demoServer = "/test/test_minimal";
        transport = new streamableHttp_js_1.StreamableHTTPClientTransport(new URL(baseUrl + demoServer));
        await client.connect(transport);
        expect(client.getServerCapabilities()?.prompts).toBeUndefined();
        expect(client.getServerCapabilities()?.resources).toBeUndefined();
        expect(client.getServerCapabilities()?.tools).toBeUndefined();
    });
    test('full server should have all capabilities', async () => {
        const demoServer = "/test/test_full";
        transport = new streamableHttp_js_1.StreamableHTTPClientTransport(new URL(baseUrl + demoServer));
        await client.connect(transport);
        expect(client.getServerCapabilities()?.prompts).toBeDefined();
        expect(client.getServerCapabilities()?.resources).toBeDefined();
        expect(client.getServerCapabilities()?.tools).toBeDefined();
    });
    test('full server should have instructions', async () => {
        const demoServer = "/test/test_full";
        transport = new streamableHttp_js_1.StreamableHTTPClientTransport(new URL(baseUrl + demoServer));
        await client.connect(transport);
        expect(client.getInstructions()).toEqual("Use this server to test the implementation");
    });
});
//# sourceMappingURL=capabilities.test.js.map