import { StreamableHTTPClientTransport, StreamableHTTPClientTransportOptions } from "@modelcontextprotocol/sdk/client/streamableHttp.js";
import fetch, { Response, RequestInfo, RequestInit } from "node-fetch";

// Create a type for the fetch function
type FetchFn = (url: RequestInfo, init?: RequestInit) => Promise<Response>;

export class CookieAwareTransport extends StreamableHTTPClientTransport {
    private cookies: string[] = [];
    private originalFetch: FetchFn;
    private mcpSessionId: string | null = null;

    constructor(url: URL, opts?: StreamableHTTPClientTransportOptions) {
        super(url, opts);

        // Store original fetch implementation
        this.originalFetch = (global as any).fetch || fetch;

        // Override fetch
        (global as any).fetch = async (url: RequestInfo, init?: RequestInit): Promise<Response> => {
            // Add stored cookies and session ID to request
            init = init || {};
            const headers: Record<string, string> = {
                'Content-Type': 'application/json',
                'Accept': 'application/json',
                ...(init.headers as Record<string, string> || {})
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

    async close(): Promise<void> {
        // Reset all state
        this.cookies = [];
        this.mcpSessionId = null;
        
        // Restore original fetch
        (global as any).fetch = this.originalFetch;
        
        await super.close();
    }
}