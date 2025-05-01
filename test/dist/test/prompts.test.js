"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
const index_js_1 = require("@modelcontextprotocol/sdk/client/index.js");
const streamableHttp_js_1 = require("@modelcontextprotocol/sdk/client/streamableHttp.js");
describe('MCP Server Prompts Tests', () => {
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
    test('List prompts should return two prompts', async () => {
        const prompts = (await client.listPrompts()).prompts;
        expect(prompts).toHaveLength(4);
    });
    test('List prompt should have one prompt with name "simple" and no arguments', async () => {
        const prompts = (await client.listPrompts()).prompts;
        const prompt = prompts.find(p => p.name === "simple");
        expect(prompt).toBeDefined();
        expect(prompt?.arguments).toBeUndefined();
    });
    test('List prompt should have one prompt with name "complex" and two arguments', async () => {
        const prompts = (await client.listPrompts()).prompts;
        const prompt = prompts.find(p => p.name === "complex");
        expect(prompt).toBeDefined();
        expect(prompt?.description).toEqual("A more complex test prompt with two arguments");
        expect(prompt?.arguments).toHaveLength(2);
        expect(prompt?.arguments?.[0]?.name).toEqual("optional");
        expect(prompt?.arguments?.[0]?.description).toEqual("Optional argument");
        expect(prompt?.arguments?.[0]?.required).not.toBeTruthy();
        expect(prompt?.arguments?.[1]?.name).toEqual("required");
        expect(prompt?.arguments?.[1]?.description).toEqual("Required argument");
        expect(prompt?.arguments?.[1]?.required).toEqual(true);
    });
    test('should get simple prompt by name', async () => {
        const prompt = await client.getPrompt({ name: "simple" });
        expect(prompt.description).toBe("Simple test prompt");
        expect(prompt.messages[0].content.text).toBe("This is a simple test prompt");
        expect(prompt.messages[0].role).toBe("user");
    });
    test('should get complex prompt by name with all arguments', async () => {
        const prompt = await client.getPrompt({ name: "complex", arguments: { required: "req", optional: "opt" } });
        expect(prompt.description).toBe("A more complex test prompt with two arguments");
        expect(prompt.messages[0].role).toBe("user");
        expect(prompt.messages[0].content.text).toBe("Execute a complex test with required parameter 'req' with optional parameter 'opt'");
    });
    test('Complex prompt with missing required argument should throw error', async () => {
        await expect(client.getPrompt({ name: "complex", arguments: { optional: "opt" } }))
            .rejects
            .toThrow("MCP error -32602: Prompt complex requires parameter 'required'");
    });
    test('should throw error when getting non-existent prompt', async () => {
        await expect(client.getPrompt({ name: "nonexistent" }))
            .rejects
            .toThrow();
    });
    test('prompt all_content_types should return all content types (except audio))', async () => {
        const prompt = await client.getPrompt({ name: "all_content_types" });
        expect(prompt.messages.find(m => m.content.type === "text")).toBeDefined();
        expect(prompt.messages.find(m => m.content.type === "image")).toBeDefined();
        expect(prompt.messages.find(m => m.content.type === "audio")).toBeDefined();
        expect(prompt.messages.find(m => m.content.type === "resource" &&
            m.content.resource.mimeType === 'image/gif' && m.content.resource.text != "")).toBeDefined();
        expect(prompt.messages.find(m => m.content.type === "resource" &&
            m.content.resource.mimeType === 'text/markdown' && m.content.resource.blob != "")).toBeDefined();
    });
    test('Ensure order of messages is preserved', async () => {
        const prompt = await client.getPrompt({ name: "ordered" });
        expect(prompt.messages[0].content.text).toBe("This is the first message");
        expect(prompt.messages[1].content.type).toBe("resource");
        const resourceMessage = prompt.messages[1].content.resource;
        expect(resourceMessage.text).toBe("This is the second message");
        expect(prompt.messages[2].content.text).toBe("This is the third message");
    });
});
//# sourceMappingURL=prompts.test.js.map