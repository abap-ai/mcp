"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
const index_js_1 = require("@modelcontextprotocol/sdk/client/index.js");
const streamableHttp_js_1 = require("@modelcontextprotocol/sdk/client/streamableHttp.js");
let client = undefined;
//const baseUrl = new URL("https://vhcalnplci.dummy.nodomain:44300/zmcp");
const baseUrl = new URL("http://localhost:8000/zmcp");
const demoServer = "/demo/demo_standard";
async function initializeClient() {
    try {
        client = new index_js_1.Client({
            name: 'test-client',
            version: '1.0.0'
        });
        const transport = new streamableHttp_js_1.StreamableHTTPClientTransport(new URL(baseUrl + demoServer));
        await client.connect(transport);
        console.log("Connected using Streamable HTTP transport");
        runTests();
    }
    catch (error) {
        // If that fails with a 4xx error, try the older SSE transport
        console.log("Streamable HTTP connection failed");
        console.error(error);
    }
}
initializeClient();
async function runTests() {
    if (!client) {
        console.error("Client is not initialized. Cannot run tests.");
        return;
    }
    const prompts = await client.listPrompts();
    const resource = await client.listResources();
    const resourceTemplates = await client.listResourceTemplates();
    const tools = await client.listTools();
    console.log("Prompts:", prompts);
    console.log("Resources:", resource);
    console.log("Resource Templates:", resourceTemplates);
    console.log("Tools:", tools);
}
//# sourceMappingURL=tests.js.map