"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
const index_js_1 = require("@modelcontextprotocol/sdk/client/index.js");
const CookieAwareTransport_js_1 = require("./CookieAwareTransport.js");
describe('ICF Server Session Tests', () => {
    const baseUrl = new URL("http://localhost:8000/zmcp");
    let client;
    let transport;
    beforeEach(async () => {
        client = new index_js_1.Client({
            name: 'test-client',
            version: '1.0.0'
        });
        const sessionServer = "/test/test_icf_session";
        transport = new CookieAwareTransport_js_1.CookieAwareTransport(new URL(baseUrl + sessionServer));
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
        expect(incrementTool.name).toBe("Test ICF Session");
        expect(incrementTool.description).toBe("Using ICF sessions we increment by the given number");
        expect(incrementTool.inputSchema).toBeDefined();
    });
    test('Should increment value in session', async () => {
        // First call should start from 0
        const result1 = await client.callTool({
            name: "Test ICF Session",
            arguments: {
                increment: 5
            }
        });
        const content1 = result1.content;
        expect(content1[0].text).toBe("Incremented value: 5");
        // Second call should use previous value
        const result2 = await client.callTool({
            name: "Test ICF Session",
            arguments: {
                increment: 3
            }
        });
        const content2 = result2.content;
        expect(content2[0].text).toBe("Incremented value: 8");
    });
    test('Should fail when increment value is missing', async () => {
        await expect(client.callTool({
            name: "Test ICF Session",
            arguments: {}
        })).rejects.toThrow("Increment value is required.");
    });
    test('Different client should start with fresh session', async () => {
        try {
            // First client increments to 5
            const result1 = await client.callTool({
                name: "Test ICF Session",
                arguments: {
                    increment: 5
                }
            });
            const content1 = result1.content;
            expect(content1[0].text).toBe("Incremented value: 5");
            // Ensure first client is fully cleaned up
            await client.close();
            await transport.close();
            // Create new transport and client with clean session
            const sessionServer = "/test/test_icf_session";
            const newTransport = new CookieAwareTransport_js_1.CookieAwareTransport(new URL(baseUrl + sessionServer));
            const newClient = new index_js_1.Client({
                name: 'test-client-2',
                version: '1.0.0'
            });
            await newClient.connect(newTransport);
            // New client should start from 0
            const result2 = await newClient.callTool({
                name: "Test ICF Session",
                arguments: {
                    increment: 3
                }
            });
            const content2 = result2.content;
            expect(content2[0].text).toBe("Incremented value: 3");
            // Clean up new client
            await newClient.close();
            await newTransport.close();
        }
        finally {
            // Restore the original client for other tests
            client = new index_js_1.Client({
                name: 'test-client',
                version: '1.0.0'
            });
            transport = new CookieAwareTransport_js_1.CookieAwareTransport(new URL(baseUrl + "/test/test_icf_session"));
            await client.connect(transport);
        }
    });
});
//# sourceMappingURL=session_icf.test.js.map