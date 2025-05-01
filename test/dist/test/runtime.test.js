"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
const index_js_1 = require("@modelcontextprotocol/sdk/client/index.js");
const streamableHttp_js_1 = require("@modelcontextprotocol/sdk/client/streamableHttp.js");
const CookieAwareTransport_js_1 = require("./CookieAwareTransport.js");
async function measureExecutionTime(fn) {
    const startTime = performance.now();
    const result = await fn();
    const endTime = performance.now();
    return [result, endTime - startTime];
}
describe('MCP Server Runtime Performance Tests', () => {
    const baseUrl = new URL("http://localhost:8000/zmcp");
    let client;
    let transport;
    beforeEach(async () => {
        client = new index_js_1.Client({
            name: 'test-client',
            version: '1.0.0'
        });
        const demoServer = "/test/test_full";
        transport = new streamableHttp_js_1.StreamableHTTPClientTransport(new URL(baseUrl + demoServer));
        await client.connect(transport);
    });
    afterEach(async () => {
        if (client) {
            await client.close();
        }
    });
    test('should measure execution time for listing all prompts', async () => {
        const [result, executionTime] = await measureExecutionTime(() => client.listPrompts());
        expect(result.prompts).toHaveLength(4);
        console.log(`List prompts execution time: ${executionTime}ms`);
        expect(executionTime).toBeLessThan(500); // Should complete pretty fast
    });
    test('should measure execution time for getting all_content_types prompt', async () => {
        const [prompt, executionTime] = await measureExecutionTime(() => client.getPrompt({ name: "all_content_types" }));
        // Verify the content types are present
        expect(prompt.messages.find(m => m.content.type === "text")).toBeDefined();
        expect(prompt.messages.find(m => m.content.type === "image")).toBeDefined();
        expect(prompt.messages.find(m => m.content.type === "audio")).toBeDefined();
        console.log(`All content types prompt execution time: ${executionTime}ms`);
        expect(executionTime).toBeLessThan(500); // Should complete pretty fast
    });
    test('should measure execution time for client initialization', async () => {
        const [_, executionTime] = await measureExecutionTime(async () => {
            const testClient = new index_js_1.Client({
                name: 'test-client',
                version: '1.0.0'
            });
            const testTransport = new streamableHttp_js_1.StreamableHTTPClientTransport(new URL(baseUrl + "/test/test_full"));
            await testClient.connect(testTransport);
            await testClient.close();
            return true;
        });
        console.log(`Client initialization time: ${executionTime}ms`);
        expect(executionTime).toBeLessThan(1000); // Allow 1s for initial connection
    });
    test('should measure execution time for listing resources', async () => {
        const [result, executionTime] = await measureExecutionTime(() => client.listResources());
        expect(result.resources).toHaveLength(3);
        console.log(`List resources execution time: ${executionTime}ms`);
        expect(executionTime).toBeLessThan(500); // Should complete pretty fast
    });
    test('should measure execution time for getting a gif resource', async () => {
        const [resource, executionTime] = await measureExecutionTime(() => client.readResource({ uri: "file://sap/okay.gif" }));
        expect(resource.contents[0].mimeType).toBe("image/gif");
        expect(resource.contents[0].blob).toBeDefined();
        console.log(`Get gif resource execution time: ${executionTime}ms`);
        expect(executionTime).toBeLessThan(500); // Should complete pretty fast
    });
    test('should measure execution time for listing tools', async () => {
        const [result, executionTime] = await measureExecutionTime(() => client.listTools());
        expect(result.tools).toHaveLength(3);
        console.log(`List tools execution time: ${executionTime}ms`);
        expect(executionTime).toBeLessThan(500); // Should complete pretty fast
    });
    test('should measure execution time for All Content Types tool', async () => {
        const [result, executionTime] = await measureExecutionTime(() => client.callTool({
            name: "All Content Types"
        }));
        expect(result.content).toHaveLength(5);
        console.log(`All Content Types tool execution time: ${executionTime}ms`);
        expect(executionTime).toBeLessThan(1000); // Allow 1s for content generation
    });
    test('should measure execution time for Input Test tool', async () => {
        const [result, executionTime] = await measureExecutionTime(() => client.callTool({
            name: "Input Test",
            arguments: {
                TextInput: "Test input",
                TestInputArray: [
                    { Line: 1, Text: "First line" },
                    { Line: 2, Text: "Second line" }
                ]
            }
        }));
        expect(result.content).toHaveLength(2);
        console.log(`Input Test tool execution time: ${executionTime}ms`);
        expect(executionTime).toBeLessThan(500); // Should complete pretty fast
    });
    test('should measure execution time for Error Test tool', async () => {
        const [result, executionTime] = await measureExecutionTime(() => client.callTool({
            name: "Error Test"
        }));
        expect(result.isError).toBe(true);
        console.log(`Error Test tool execution time: ${executionTime}ms`);
        expect(executionTime).toBeLessThan(500); // Should complete pretty fast
    });
    test('should measure execution time for MCP Session operations', async () => {
        const sessionServer = "/test/test_mcp_session";
        const sessionTransport = new streamableHttp_js_1.StreamableHTTPClientTransport(new URL(baseUrl + sessionServer));
        const sessionClient = new index_js_1.Client({
            name: 'test-client-session',
            version: '1.0.0'
        });
        await sessionClient.connect(sessionTransport);
        try {
            const [result1, executionTime1] = await measureExecutionTime(() => sessionClient.callTool({
                name: "Test MCP Session",
                arguments: {
                    increment: 5
                }
            }));
            expect(result1.content[0].text).toBe("Incremented value: 5");
            console.log(`MCP Session first increment execution time: ${executionTime1}ms`);
            expect(executionTime1).toBeLessThan(500);
            const [result2, executionTime2] = await measureExecutionTime(() => sessionClient.callTool({
                name: "Test MCP Session",
                arguments: {
                    increment: 3
                }
            }));
            expect(result2.content[0].text).toBe("Incremented value: 8");
            console.log(`MCP Session second increment execution time: ${executionTime2}ms`);
            expect(executionTime2).toBeLessThan(500);
        }
        finally {
            await sessionClient.close();
        }
    });
    test('should measure execution time for ICF Session operations', async () => {
        const sessionServer = "/test/test_icf_session";
        const sessionTransport = new CookieAwareTransport_js_1.CookieAwareTransport(new URL(baseUrl + sessionServer));
        const sessionClient = new index_js_1.Client({
            name: 'test-client-session',
            version: '1.0.0'
        });
        await sessionClient.connect(sessionTransport);
        try {
            const [result1, executionTime1] = await measureExecutionTime(() => sessionClient.callTool({
                name: "Test ICF Session",
                arguments: {
                    increment: 5
                }
            }));
            expect(result1.content[0].text).toBe("Incremented value: 5");
            console.log(`ICF Session first increment execution time: ${executionTime1}ms`);
            expect(executionTime1).toBeLessThan(500);
            const [result2, executionTime2] = await measureExecutionTime(() => sessionClient.callTool({
                name: "Test ICF Session",
                arguments: {
                    increment: 3
                }
            }));
            expect(result2.content[0].text).toBe("Incremented value: 8");
            console.log(`ICF Session second increment execution time: ${executionTime2}ms`);
            expect(executionTime2).toBeLessThan(500);
        }
        finally {
            await sessionClient.close();
            await sessionTransport.close();
        }
    });
});
//# sourceMappingURL=runtime.test.js.map