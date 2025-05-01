import { Client } from "@modelcontextprotocol/sdk/client/index.js";
import { StreamableHTTPClientTransport } from "@modelcontextprotocol/sdk/client/streamableHttp.js";

describe('MCP Connection Tests', () => {
    const baseUrl = new URL("http://localhost:8000/zmcp");
    const demoServer = "/test/test_minimal";

    test('should successfully connect to server', async () => {
        const client = new Client({
            name: 'test-client',
            version: '1.0.0'
        });
        const transport = new StreamableHTTPClientTransport(
            new URL(baseUrl + demoServer),
        );
        
        await expect(client.connect(transport)).resolves.not.toThrow();
        await client.close();
    });
});