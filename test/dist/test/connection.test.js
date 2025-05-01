"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
const index_js_1 = require("@modelcontextprotocol/sdk/client/index.js");
const streamableHttp_js_1 = require("@modelcontextprotocol/sdk/client/streamableHttp.js");
describe('MCP Connection Tests', () => {
    const baseUrl = new URL("http://localhost:8000/zmcp");
    const demoServer = "/test/test_minimal";
    test('should successfully connect to server', async () => {
        const client = new index_js_1.Client({
            name: 'test-client',
            version: '1.0.0'
        });
        const transport = new streamableHttp_js_1.StreamableHTTPClientTransport(new URL(baseUrl + demoServer));
        await expect(client.connect(transport)).resolves.not.toThrow();
        await client.close();
    });
});
//# sourceMappingURL=connection.test.js.map