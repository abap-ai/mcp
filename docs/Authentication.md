# Authentication

## S/4HANA

If you use a relatively current S/4HANA Release OpenID Connect is supported by the ABAP Platform. For details see the [SAP Documentation](https://help.sap.com/docs/ABAP_PLATFORM_NEW/e815bb97839a4d83be6c4fca48ee5777/5523b9e2c63240be80f664cd18bec460.html?locale=en-US). \
Besides that all features supported by the ABAP Platform are available however using OIDC is highly recommended and fits well to MCP specification expecting OAuth.

## Others

On other (older) releases this topic is more complex and you have to rely on the even more limited features of the ABAP Netweaver stack. Following a few suggestions to consider.

### OAuth

ABAP 7.40 has a bit unusual but basic OAuth support, see the [SAP Documentation](https://help.sap.com/docs/SAP_NETWEAVER_AS_ABAP_FOR_SOH_740/e815bb97839a4d83be6c4fca48ee5777/4bd73e2050e84ce99b2d781687e0c62d.html?locale=en-US). This is more relevant when you use e.g. an API Gateway to map OAuth tokens from your main identitiy provider via OAuth SAML Bearer to an ABAP OAuth token (this is not a common feature but usually possible with some custom development).

Unfortunately this requires a special implementation which is not supported out of the box and requires a modification (or implicit enhancement). If you want to go this way:

Modify/Enhance Class CL_OAUTH2_S_SCOPE_CONTROLLER Method CLASS_CONSTRUCTOR:

```abap
 CLEAR ls_scope_properties.
 ls_scope_properties-object                 = 'CLAS'.
 ls_scope_properties-name_derivation_method = c_derive_name_default.
 ls_scope_properties-authority_check_method = c_authority_check_classic.
 ls_scope_properties-runtime_handler        = 'ZCL_MCP_HTTP_OAUTH_HANDLER'.
 CLEAR ls_scope_properties-rbam_strategy.
 INSERT ls_scope_properties INTO TABLE mt_scope_properties.
 INSERT ls_scope_properties-runtime_handler INTO TABLE mt_whitelist.
 create_hta ls_scope_properties-runtime_handler c_pgmid 'CLAS' 'ZCL_MCP_HTTP_OAUTH_HANDLER'
 ```

**Note:** Depending on your release the code might look a bit different - especially the macro might be missing - adjust it logically matching to the other entries in the class constructor.

Execute the class CL_OAUTH2_S_SCOPE_MANAGER method CREATE_SCOPE_FROM_OBJECT via the test feature in SE24: \
is_object --> R3TR CLAS ZCL_MCP_HTTP_OAUTH_HANDLER \
Description = any description you want \
Devclass = the package to be used

Change the SICF handler (or add an additional one for OAuth connections) to the class ZCL_MCP_HTTP_OAUTH_HANDLER

Setup everything as per SAP's documentation

### X509

Similar to how the SAP Cloud Connector works an option is to use a custom solution to extract user information from your Identity Providers OAuth token and create a short-lived (cached) X509 to login to the ABAP server. While this can be used without any modifications on ABAP side it is overall complex to setup as this is not common.
