# MCP Server SDK: Prompts

This documentation explains how to implement and use prompts in the Model Context Protocol (MCP) Server SDK. Prompts enable AI assistants to retrieve pre-defined conversation starters or templates from your ABAP systems.

## Table of Contents

- [Overview](#overview)
- [Prompt Request Classes](#prompt-request-classes)
  - [List Prompts Request](#list-prompts-request)
  - [Get Prompt Request](#get-prompt-request)
- [Prompt Response Classes](#prompt-response-classes)
  - [Get Prompt Response](#get-prompt-response)
  - [Message Types](#message-types)
- [Implementing Prompt Handlers](#implementing-prompt-handlers)
  - [Handling List Prompts](#handling-list-prompts)
  - [Handling Get Prompt](#handling-get-prompt)
- [Examples](#examples)
  - [Simple Text Prompts](#simple-text-prompts)
  - [Prompts with Images](#prompts-with-images)
  - [Resource-based Prompts](#resource-based-prompts)

## Overview

Prompts in the MCP protocol are pre-defined conversation templates that can be retrieved and used by AI assistants. They provide:

- Standardized ways to invoke specific capabilities
- Parameterized templates (with arguments)
- Multi-modal content (text, images, resources)
- Consistent conversation patterns

Prompts are identified by a unique name and can accept arguments to customize their behavior.

## Prompt Request Classes

### List Prompts Request

The `ZCL_MCP_REQ_LIST_PROMPTS` class handles requests to list available prompts on the server.

Key methods:

- `get_cursor()`: Retrieves the pagination cursor if provided
- `has_cursor()`: Checks if a pagination cursor was provided

Example:

```abap
METHOD handle_list_prompts.
  " Check if pagination is being used
  IF request->has_cursor( ).
    " Handle pagination with the cursor
    DATA(cursor) = request->get_cursor( ).
    " Use cursor to determine which page of prompts to return
  ENDIF
  
  " Set up response...
ENDMETHOD.
```

### Get Prompt Request

The `ZCL_MCP_REQ_GET_PROMPT` class handles requests to retrieve a specific prompt with arguments.

Key methods:

- `get_name()`: Retrieves the requested prompt name
- `has_arguments()`: Checks if arguments were provided
- `get_arguments()`: Retrieves the arguments as a table of key-value pairs

Example:

```abap
METHOD handle_get_prompt.
  " Get the requested prompt name
  DATA(prompt_name) = request->get_name( ).
  
  " Check for arguments
  IF request->has_arguments( ).
    " Access the provided arguments
    DATA(arguments) = request->get_arguments( ).
    
    " Loop through arguments
    LOOP AT arguments ASSIGNING FIELD-SYMBOL(<arg>).
      " Use <arg>-key and <arg>-value
    ENDLOOP.
  ENDIF
  
  " Generate the prompt response...
ENDMETHOD.
```

## Prompt Response Classes

### Get Prompt Response

The `ZCL_MCP_RESP_GET_PROMPT` class builds the response for a prompt request, allowing you to:

- Set a description for the prompt
- Add conversation messages with different content types
- Assign roles to messages (user/assistant)

Key methods:

- `set_description()`: Sets the prompt description
- `add_text_message()`: Adds a simple text message
- `add_image_message()`: Adds an image message
- `add_text_resource_message()`: Adds a text resource message
- `add_blob_resource_message()`: Adds a binary resource message

### Message Types

The class supports several content types for prompt messages:

1. **Text Content**: Simple text messages

   ```abap
   add_text_message(
     role = zif_mcp_server=>role_user
     text = 'Please analyze this data'
   )
   ```

2. **Image Content**: Base64-encoded images

   ```abap
   add_image_message(
     role = zif_mcp_server=>role_user
     data = lv_base64_image
     mime_type = 'image/png'
   )
   ```

3. **Resource Content**: References to text or binary resources

   ```abap
   add_text_resource_message(
     role = zif_mcp_server=>role_user
     uri = 'file://documents/report.txt'
     text = lv_document_text
     mime_type = 'text/plain'
   )
   ```

## Implementing Prompt Handlers

To implement prompts in your MCP server, you need to override the following methods:

```abap
METHODS handle_list_prompts REDEFINITION.
METHODS handle_get_prompt REDEFINITION.
```

### Handling List Prompts

The `handle_list_prompts` method should return a list of available prompts with their descriptions and arguments:

```abap
METHOD handle_list_prompts.
  " Create list of available prompts
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
      ( name = 'analyze_code'
        description = 'Analyze provided ABAP code'
        arguments = VALUE #(
          ( name = 'code'
            description = 'ABAP code to analyze'
            required = abap_true )
        )
      )
    )
  ).
ENDMETHOD.
```

### Handling Get Prompt

The `handle_get_prompt` method should generate the actual prompt content based on the name and arguments:

```abap
METHOD handle_get_prompt.
  " Get the prompt name
  DATA(prompt_name) = request->get_name( ).
  
  " Handle different prompts
  CASE prompt_name.
    WHEN 'greeting'.
      " Handle greeting prompt
      IF request->has_arguments( ).
        DATA(arguments) = request->get_arguments( ).
        READ TABLE arguments INTO DATA(name_arg) WITH KEY key = 'name'.
        IF sy-subrc = 0.
          response-result->set_description( 'Greeting prompt' ).
          response-result->add_text_message(
            role = zif_mcp_server=>role_user
            text = |Hello { name_arg-value }, nice to meet you!|
          ).
        ELSE.
          " Missing required argument
          response-error-code = zcl_mcp_jsonrpc=>error_codes-invalid_params.
          response-error-message = 'Missing required argument: name'.
        ENDIF.
      ENDIF.
      
    WHEN 'analyze_code'.
      " Handle code analysis prompt
      " Similar implementation for other prompts
      
    WHEN OTHERS.
      " Unknown prompt
      response-error-code = zcl_mcp_jsonrpc=>error_codes-invalid_params.
      response-error-message = |Prompt { prompt_name } not found|.
  ENDCASE.
ENDMETHOD.
```

## Examples

### Simple Text Prompts

```abap
METHOD handle_get_prompt.
  CASE request->get_name( ).
    WHEN 'introduce_yourself'.
      response-result->set_description( 'Introduction prompt' ).
      
      " First message (user)
      response-result->add_text_message(
        role = zif_mcp_server=>role_user
        text = 'Please introduce yourself briefly and tell me how you can help with ABAP development.'
      ).
      
      " Second message with example response (assistant)
      response-result->add_text_message(
        role = zif_mcp_server=>role_assistant
        text = 'Hello! I''m an AI assistant specialized in ABAP development...'
      ).
      
    WHEN OTHERS.
      " Handle unknown prompt
  ENDCASE.
ENDMETHOD.
```

### Prompts with Images

```abap
METHOD handle_get_prompt.
  CASE request->get_name( ).
    WHEN 'analyze_image'.
      " Get image data from arguments
      DATA(arguments) = request->get_arguments( ).
      READ TABLE arguments INTO DATA(image_arg) WITH KEY key = 'image_id'.
      IF sy-subrc = 0.
        " Get the image data from your system
        DATA(image_data) = get_image_as_base64( image_arg-value ).
        
        response-result->set_description( 'Image analysis prompt' ).
        
        " Add text instruction
        response-result->add_text_message(
          role = zif_mcp_server=>role_user
          text = 'Please analyze the following image and describe what you see:'
        ).
        
        " Add the image
        response-result->add_image_message(
          role = zif_mcp_server=>role_user
          data = image_data
          mime_type = 'image/jpeg'
        ).
      ENDIF.
      
    WHEN OTHERS.
      " Handle unknown prompt
  ENDCASE.
ENDMETHOD.
```

### Resource-based Prompts

```abap
METHOD handle_get_prompt.
  CASE request->get_name( ).
    WHEN 'review_code'.
      " Get code ID from arguments
      DATA(arguments) = request->get_arguments( ).
      READ TABLE arguments INTO DATA(code_arg) WITH KEY key = 'class_name'.
      IF sy-subrc = 0.
        " Get the source code
        DATA(source_code) = get_class_source( code_arg-value ).
        
        response-result->set_description( 'Code review prompt' ).
        
        " Add instruction
        response-result->add_text_message(
          role = zif_mcp_server=>role_user
          text = |Please review the following ABAP class ({ code_arg-value }) and suggest improvements:|
        ).
        
        " Add the source code as a resource
        response-result->add_text_resource_message(
          role = zif_mcp_server=>role_user
          uri = |abap://classes/{ code_arg-value }|
          text = source_code
          mime_type = 'text/x-abap'
        ).
      ENDIF.
      
    WHEN OTHERS.
      " Handle unknown prompt
  ENDCASE.
ENDMETHOD.
```

---

By implementing prompts in your MCP server, you provide AI assistants with pre-defined conversation templates that ensure consistent, high-quality interactions with specific features of your ABAP systems.
