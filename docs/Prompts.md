# MCP Server SDK: Prompts

This documentation explains how to implement and use prompts in the Model Context Protocol (MCP) Server SDK. Prompts enable AI assistants to retrieve pre-defined conversation starters or templates from your ABAP systems.

## Table of Contents

- [Overview](#overview)
- [Prompt Request Classes](#prompt-request-classes)
- [Prompt Response Classes](#prompt-response-classes)
- [Implementing Prompt Handlers](#implementing-prompt-handlers)
- [Examples](#examples)

## Overview

Prompts in the MCP protocol are pre-defined conversation templates that can be retrieved and used by AI assistants. They provide:

- Standardized ways to invoke specific capabilities
- Parameterized templates (with arguments)
- Multi-modal content (text, images, audio, resources)
- Consistent conversation patterns

Prompts are identified by a unique name and can accept arguments to customize their behavior.

## Prompt Request Classes

### List Prompts Request

The `ZCL_MCP_REQ_LIST_PROMPTS` class handles requests to list available prompts on the server.

Key methods:
- `get_cursor()`: Retrieves the pagination cursor if provided
- `has_cursor()`: Checks if a pagination cursor was provided

### Get Prompt Request

The `ZCL_MCP_REQ_GET_PROMPT` class handles requests to retrieve a specific prompt with arguments.

Key methods:
- `get_name()`: Retrieves the requested prompt name
- `has_arguments()`: Checks if arguments were provided
- `get_arguments()`: Retrieves the arguments as a table of key-value pairs

## Prompt Response Classes

### Get Prompt Response

The `ZCL_MCP_RESP_GET_PROMPT` class builds the response for a prompt request, supporting various content types and annotations.

**Key methods:**

- `set_description()`: Sets the prompt description
- `set_messages()`: Sets complete message list
- `set_meta()`: Sets metadata for the prompt

**Content methods:**

- `add_text_message()`: Adds a simple text message
- `add_image_message()`: Adds an image message (Base64-encoded)
- `add_audio_message()`: Adds an audio message (Base64-encoded)
- `add_text_resource_message()`: Adds embedded text resource
- `add_blob_resource_message()`: Adds embedded binary resource
- `add_resource_link_message()`: Adds a resource link reference

**Content Types:**

1. **Text Content**: Simple text messages
2. **Image Content**: Base64-encoded images with MIME type
3. **Audio Content**: Base64-encoded audio with MIME type
4. **Text Resource**: Embedded text resources with URI
5. **Blob Resource**: Embedded binary resources with URI
6. **Resource Links**: References to external resources

**Annotations Support:**

All content types support optional annotations:
- `audience`: Target audience list
- `priority`: Priority level (decimal)
- `last_modified`: Timestamp of last modification

## Implementing Prompt Handlers

Override these methods in your MCP server:

```abap
METHODS handle_list_prompts REDEFINITION.
METHODS handle_get_prompt REDEFINITION.
```

### Handling List Prompts

```abap
METHOD handle_list_prompts.
  response-result->set_prompts(
    VALUE #(
      ( name = 'greeting'
        description = 'Greet a person by name'
        arguments = VALUE #(
          ( name = 'name'
            description = 'Name of person to greet'
            required = abap_true )
        )
      )
    )
  ).
ENDMETHOD.
```

### Handling Get Prompt

```abap
METHOD handle_get_prompt.
  DATA(prompt_name) = request->get_name( ).
  
  CASE prompt_name.
    WHEN 'greeting'.
      IF request->has_arguments( ).
        DATA(arguments) = request->get_arguments( ).
        READ TABLE arguments INTO DATA(name_arg) WITH KEY key = 'name'.
        IF sy-subrc = 0.
          response-result->set_description( 'Greeting prompt' ).
          response-result->add_text_message(
            role = zif_mcp_server=>role_user
            text = |Hello { name_arg-value }, nice to meet you!|
          ).
        ENDIF.
      ENDIF.
      
    WHEN OTHERS.
      response-error-code = zcl_mcp_jsonrpc=>error_codes-invalid_params.
      response-error-message = |Prompt { prompt_name } not found|.
  ENDCASE.
ENDMETHOD.
```

## Examples

### Multi-Modal Prompt

```abap
METHOD handle_get_prompt.
  CASE request->get_name( ).
    WHEN 'analyze_media'.
      response-result->set_description( 'Analyze multiple media types' ).
      
      " Text instruction
      response-result->add_text_message(
        role = zif_mcp_server=>role_user
        text = 'Please analyze the following media files:'
      ).
      
      " Image content
      response-result->add_image_message(
        role = zif_mcp_server=>role_user
        data = get_base64_image( )
        mime_type = 'image/png'
        annotations = VALUE #( priority = '1.0' )
      ).
      
      " Audio content
      response-result->add_audio_message(
        role = zif_mcp_server=>role_user
        data = get_base64_audio( )
        mime_type = 'audio/mp3'
      ).
      
      " Resource link
      response-result->add_resource_link_message(
        role = zif_mcp_server=>role_user
        uri = 'https://example.com/document.pdf'
        name = 'Analysis Document'
        description = 'Supporting documentation'
        mime_type = 'application/pdf'
      ).
  ENDCASE.
ENDMETHOD.
```

### Resource-Based Prompt

```abap
METHOD handle_get_prompt.
  CASE request->get_name( ).
    WHEN 'review_code'.
      DATA(arguments) = request->get_arguments( ).
      READ TABLE arguments INTO DATA(class_arg) WITH KEY key = 'class_name'.
      
      IF sy-subrc = 0.
        DATA(source_code) = get_class_source( class_arg-value ).
        
        response-result->set_description( 'ABAP code review prompt' ).
        
        " Add embedded text resource
        response-result->add_text_resource_message(
          role = zif_mcp_server=>role_user
          uri = |abap://classes/{ class_arg-value }|
          text = source_code
          mime_type = 'text/x-abap'
          annotations = VALUE #( 
            audience = VALUE #( ( 'developers' ) ( 'architects' ) )
            priority = '0.8'
          )
        ).
      ENDIF.
  ENDCASE.
ENDMETHOD.
```