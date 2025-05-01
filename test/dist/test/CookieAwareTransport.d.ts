import { StreamableHTTPClientTransport, StreamableHTTPClientTransportOptions } from "@modelcontextprotocol/sdk/client/streamableHttp.js";
export declare class CookieAwareTransport extends StreamableHTTPClientTransport {
    private cookies;
    private originalFetch;
    private mcpSessionId;
    constructor(url: URL, opts?: StreamableHTTPClientTransportOptions);
    close(): Promise<void>;
}
