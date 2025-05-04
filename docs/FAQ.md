# FAQ

This document holds some tipps and tricks as well as solutions to common issues.

## ICF sessions do not work

Make sure that the MCP client used supports cookies. The MCP-Session header is not enough for ICF based sessions!

## Syntax error in ZCL_MCP_HTTP_OAUTH_HANDLER

This class is not fully downwards compatible, exclude the sub-package OAuth or delete the class. This class is only relevant for OAuth and if the interface does not exist your system is unlikely to support it.

## Syntax error after pull from branch 702

Somehow abapgit sometimes does not recognize changes, probably due to the unusual coding structure after downport. Make sure to update all code and not just changed if this issue happens. If this does not solve it and you confirmed there is a real error in the code by checking GitHub sources (if possible) feel free to open an issue with details. Due to me not having a system < 7.50 it might be challenging to fix but I will have a look and do my best.
