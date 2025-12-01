# clprompt Quickstart Guide

This guide will help you get started with `clprompt`, a Common Lisp library for working with generative AI models using a simple, powerful prompt file format.

## Installation

`clprompt` is designed to be used with a modern Common Lisp environment. The recommended way to install and manage dependencies is using [Quicklisp](https://www.quicklisp.org/beta/).

1.  **Clone the repository:**

    ```bash
    git clone https://github.com/vlebedev/clprompt.git
    ```

2.  **Load the system:**

    You can load `clprompt` into your Lisp image using Quicklisp. Make sure the cloned repository is in a location where Quicklisp can find it (e.g., `~/quicklisp/local-projects/`).

    ```lisp
    (ql:quickload :clprompt)
    ```

## Basic Usage

The core of `clprompt` is the `.prompt` file, which defines the model, its configuration, and the prompt template.

1.  **Create a prompt file:**

    Create a file named `summarize.prompt` with the following content:

    ```prompt
    ---
    model: googleai/gemini-1.5-pro
    temperature: 0.5
    maxOutputTokens: 500
    input:
      schema:
        content: string, the content to summarize
        style?: string, the summarization style (brief, detailed, bullet-points)
    ---
    {{#if style}}
    Summarize the following content in a {{style}} style:
    {{else}}
    Summarize the following content:
    {{/if}}

    {{content}}
    ```

2.  **Load and execute the prompt:**

    First, load the prompt file into a `dotprompt` object using `load-prompt`. Then, you can execute it with your input using `execute-prompt`.

    ```lisp
    (let ((prompt (clprompt:load-prompt #p"/path/to/your/summarize.prompt")))
      (clprompt:execute-prompt prompt
                               :input '(:content "Common Lisp is a powerful, dynamic, and expressive programming language.")))
    ```

    You can also provide the optional `style` parameter:

    ```lisp
    (let ((prompt (clprompt:load-prompt #p"/path/to/your/summarize.prompt")))
      (clprompt:execute-prompt prompt
                               :input '(:content "Common Lisp is a powerful, dynamic, and expressive programming language."
                                        :style "bullet-points")))
    ```

## API Overview

`clprompt` provides a set of high-level functions for working with prompts:

*   `load-prompt (pathname)`: Parses a `.prompt` file and returns a `dotprompt` object.
*   `execute-prompt (prompt &key input)`: Executes a `dotprompt` object with the given input and returns the response from the language model.
*   `render-prompt (prompt &key input)`: Renders the template of a `dotprompt` object with the given input, returning the filled-in prompt string.
*   `validate-input (prompt &key input)`: Validates the given input against the input schema of a `dotprompt` object.
*   `define-prompt (name &body body)`: A macro for defining prompts directly in your Lisp code.
