import { Client } from "@modelcontextprotocol/sdk/client/index.js";
import { StreamableHTTPClientTransport } from "@modelcontextprotocol/sdk/client/streamableHttp.js";

describe('MCP Server Capabilities Tests', () => {
    const baseUrl = new URL("http://localhost:8000/zmcp");
    let client: Client;
    let transport: StreamableHTTPClientTransport;

    beforeEach(() => {
        client = new Client({
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
        transport = new StreamableHTTPClientTransport(
            new URL(baseUrl + demoServer),
        );
        await client.connect(transport);

        expect(client.getServerCapabilities()?.prompts).toBeUndefined();
        expect(client.getServerCapabilities()?.resources).toBeUndefined();
        expect(client.getServerCapabilities()?.tools).toBeUndefined();
    });

    test('full server should have all capabilities', async () => {
        const demoServer = "/test/test_full";
        transport = new StreamableHTTPClientTransport(
            new URL(baseUrl + demoServer),
        );
        await client.connect(transport);

        expect(client.getServerCapabilities()?.prompts).toBeDefined();
        expect(client.getServerCapabilities()?.resources).toBeDefined();
        expect(client.getServerCapabilities()?.tools).toBeDefined();
    });

    test('full server should have instructions', async () => {
        const demoServer = "/test/test_full";
        transport = new StreamableHTTPClientTransport(
            new URL(baseUrl + demoServer),
        );
        await client.connect(transport);

        expect(client.getInstructions()).toEqual("Use this server to test the implementation");
    });
});