# ABAP Model Context Protocol Server SDK

[Discord](https://discord.gg/jf2FhnFvSn) - for all ABAP-AI repos.

## Overview

ABAP implementation of the [Model Context Protocol](https://modelcontextprotocol.io/introduction), supporting up to version [2025-11-25](https://modelcontextprotocol.io/specification/2025-11-25). \
**Note** that this is only a server implementation as I currently see no realistic way to support SSE streaming which is mandatory for the client.

Supported protocol versions: 2025-03-26, 2025-06-18, 2025-11-25

**Important**: This SDK version will only support up to 2025-11-25 and is frozen except for error corrections. 2026-07-28 and further are implemented in the new [ABAP MCP SDK V2](https://github.com/abap-ai/mcp2). The vast number of breaking changes and different approaches made it challenging to properly support all versions. As especially the complex session support is gone I decided to make a clear cut and create a V2 SDK. The new SDK will support on best effort basis old MCP versions down to 2025-03-26 as long as no sessions are required.

## Documentation

See [Overview](docs/Overview.md) in docs folder.

## Used ABAP OpenSource projects

Special thanks to:

- [ajson](https://github.com/sbcgua/ajson) integrated as zmcp_ajson
- [abaplint](https://github.com/abaplint/abaplint) for review and more important 7.02 downport
- [abapGit](https://github.com/abapGit/abapGit) for source code control

Find more awesome projects at [dotabap](https://dotabap.org/).

## Features & Limitations

Currently implemented:

- Prompts
- Resources
- Tools (incl. optional output schemas and task-augmented execution)
- Tasks - long-running background work with client polling
- Completions - autocomplete for prompt and resource-template arguments
- DDIC-based schema generation plus a JSON schema builder and validator

No SSE support --> no notifications and therefore no support of listChanged, subscribe and logging.\

Not implemented:

- Sampling - considered for the future, open an issue if you have a use case that would benefit from it
- Elicitation - without SSE I see no proper way to implement this
- Roots - likely irrelevant for ABAP
- Further notifications not relevant without SSE: Cancellation, Progress, Logging

## Authorization

Implement the standard [Authorization](https://modelcontextprotocol.io/specification/2025-06-18/basic/authorization) in ABAP via custom code is not feasible, we have to rely on ABAP features. \
An overview of likely options is outlined in [Authentication Documentation](./docs/Authentication.md).

## Authentication

OAuth is suggested but outside of newer S/4HANA releases challenging, for options see [Authentication](./docs/Authentication.md). Further suggestions are welcome.

## Roadmap

See currently open issues for the next planned steps. Longer term I intend to add features to create modular MCP servers based on reusable implementations.

## FAQ

See [FAQ](./docs/FAQ.md).

## Contributing

Feel free to:

- Open issues for bug reports
- Use [Discord](https://discord.gg/jf2FhnFvSn) for discussions
- Suggest improvements
- Pull requests are welcome but ensure they are tested and follow the overall style, if in doubt open an issue first

## GenAI Notice

Part of the coding was generated using AI, mostly with Sonnet 3.7/4 and GitHub Copilot. Detailed documentation is largely AI generated - only high-level reviewed.
