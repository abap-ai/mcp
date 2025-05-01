import { Client } from "@modelcontextprotocol/sdk/client/index.js";
import { StreamableHTTPClientTransport } from "@modelcontextprotocol/sdk/client/streamableHttp.js";
import { TextResourceContents } from "@modelcontextprotocol/sdk/types.js";

describe('MCP Server Resources Tests', () => {
    const baseUrl = new URL("http://localhost:8000/zmcp");
    let client: Client;
    let transport: StreamableHTTPClientTransport;

    beforeEach(async () => {
        client = new Client({
            name: 'test-client',
            version: '1.0.0'
        });
        const demoServer = "/test/test_full";
        transport = new StreamableHTTPClientTransport(
            new URL(baseUrl + demoServer),
        );
        await client.connect(transport);
    });

    afterEach(async () => {
        if (client) {
            await client.close();
        }
    });

    test('List resources should return three resources', async () => {
        const resources = (await client.listResources()).resources;
        expect(resources).toHaveLength(3);
    });

    test('List resources should include specific resources', async () => {
        const resources = (await client.listResources()).resources;
        
        const okResource = resources.find(r => r.name === "OK");
        expect(okResource).toBeDefined();
        expect(okResource?.uri).toBe("file://sap/okay.gif");
        expect(okResource?.mimeType).toBe("image/gif");
        expect(okResource?.description).toBe("Okay Gif");

        const textResource = resources.find(r => r.name === "TextFile");
        expect(textResource).toBeDefined();
        expect(textResource?.uri).toBe("file://sap/text.json");
        expect(textResource?.mimeType).toBe("text/json");
        expect(textResource?.description).toBe("Same Json Text");
    });

    test('List resource templates should return gif template', async () => {
        const templates = (await client.listResourceTemplates()).resourceTemplates;
        expect(templates).toHaveLength(1);
        expect(templates[0].name).toBe("Gif");
        expect(templates[0].mimeType).toBe("image/gif");
        expect(templates[0].uriTemplate).toBe("file://{path}");
        expect(templates[0].description).toBe("Gifs ...");
    });

    test('Should read gif resource as blob', async () => {
        const resource = (await client.readResource({ uri: "file://sap/okay.gif" }));
        expect(resource.contents).toHaveLength(1);
        expect(resource.contents[0].mimeType).toBe("image/gif");
        expect(resource.contents[0].blob).toBeDefined();
        expect(resource.contents[0].text).toBeUndefined();
    });

    test('Should read json resource as text', async () => {
        const resource = await client.readResource({ uri: "file://sap/text.json" });
        expect(resource.contents).toHaveLength(1);
        expect(resource.contents[0].mimeType).toBe("text/json");
        expect(resource.contents[0].text).toBe('{ "key": "value" }');
        expect(resource.contents[0].blob).toBeUndefined();
    });

    test('Should fail when requesting non-existent resource', async () => {
        await expect(client.readResource({ uri: "file://sap/nonexistent.txt" }))
            .rejects
            .toThrow("Resource file://sap/nonexistent.txt not found.");
    });

});
