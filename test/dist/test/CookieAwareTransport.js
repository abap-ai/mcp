"use strict";
var __importDefault = (this && this.__importDefault) || function (mod) {
    return (mod && mod.__esModule) ? mod : { "default": mod };
};
Object.defineProperty(exports, "__esModule", { value: true });
exports.CookieAwareTransport = void 0;
const streamableHttp_js_1 = require("@modelcontextprotocol/sdk/client/streamableHttp.js");
const node_fetch_1 = __importDefault(require("node-fetch"));
class CookieAwareTransport extends streamableHttp_js_1.StreamableHTTPClientTransport {
    cookies = [];
    originalFetch;
    mcpSessionId = null;
    constructor(url, opts) {
        super(url, opts);
        // Store original fetch implementation
        this.originalFetch = global.fetch || node_fetch_1.default;
        // Override fetch
        global.fetch = async (url, init) => {
            // Add stored cookies and session ID to request
            init = init || {};
            const headers = {
                'Content-Type': 'application/json',
                'Accept': 'application/json',
                ...(init.headers || {})
            };
            if (this.cookies.length > 0) {
                headers['cookie'] = this.cookies.join('; ');
            }
            if (this.mcpSessionId) {
                headers['mcp-session-id'] = this.mcpSessionId;
            }
            init.headers = headers;
            const response = await this.originalFetch(url, init);
            // Store cookies from response
            const setCookieHeader = response.headers.get('set-cookie');
            if (setCookieHeader) {
                const newCookies = setCookieHeader.split(',').map(cookie => cookie.trim());
                this.cookies = [...this.cookies, ...newCookies];
            }
            // Store MCP session ID from response
            const mcpSessionId = response.headers.get('mcp-session-id');
            if (mcpSessionId) {
                this.mcpSessionId = mcpSessionId;
            }
            return response;
        };
    }
    async close() {
        // Reset all state
        this.cookies = [];
        this.mcpSessionId = null;
        // Restore original fetch
        global.fetch = this.originalFetch;
        await super.close();
    }
}
exports.CookieAwareTransport = CookieAwareTransport;
//# sourceMappingURL=CookieAwareTransport.js.map