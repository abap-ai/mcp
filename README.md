# MCP ABAP Server SDK Downport

This is an automated donwport of ABAP MCP SDK. This is a best effort donwport using the awesome downport features of abaplint. Unfortunately I have no system with a release below 7.50 available, therefore bugs and syntax issues are expected. Downport target is 7.02 which I assume should work.

If you face issues feel free to open an issue.

## ABAP Model Context Protocol Server SDK

[Discord](https://discord.gg/jf2FhnFvSn) - for all ABAP-AI repos.

## Overview

ABAP implementation of the [Model Context Protocol](https://modelcontextprotocol.io/introduction) based on version [2025-03-26](https://modelcontextprotocol.io/specification/2025-03-26). \
**Note** that this is only a server implementation as I currently see no realistic way to support SSE streaming which is mandatory for the client.

## Documentation

See [Overview](docs/Overview.md) in docs folder.

## Used OpenSource projects

- [ajson](https://github.com/sbcgua/ajson) integrated as zmcp_ajson
- [abaplint](https://github.com/abaplint/abaplint) for review and more important 7.02 downport

## Features & Limitations

Currently implemented:

- Prompts
- Resources
- Tools

No SSE support --> no notifications and therefore no support of listChanged, subscribe and logging.\

Not implmented:

- Sampling - considered for the future, open an issue if you have a use case that would benefit from it
- Completions - questionable from a performance point of view with ABAP
- Roots - likely irrelevant for ABAP
- Further notifications not relevant without SSE: Cancellation, Ping, Progress, Logging

**Non-compliance** to the spec:

- [Authorization](https://modelcontextprotocol.io/specification/2025-03-26/basic/authorization) standard cannot be implemented in ABAP --> we rely on ABAP capabilties instead

## Roadmap

See currently open issues for details. In general the following topics are prioritized:

- Full test suite especially covering session handling and error scenarios
- Demonstrate ways to use OAuth reducing challenges with only standard compliant clients
- Cleanup batch job / report to delete outdated sessions
- Improve logging by increasing the logged scope
- Add options to record requests & responses for debugging purposes
- Alternatve composable server - instead of writing a full server just configure a combination of standalone implementations for better reuse
- Examples, documentation, etc.

## Contributing

Feel free to:

- Open issues for bug reports
- Use [Discord](https://discord.gg/jf2FhnFvSn) for discussions
- Suggest improvements
- Pull requests are welcome but ensure they are tested and follow the overall style, if in doubt open an issue first

## GenAI Notice

Part of the coding was generated using AI, mostly with Sonnet 3.7 and GitHub Copilot. Detailed documentation is largely AI generated - only high-level reviewed.
