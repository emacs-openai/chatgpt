[![License: GPL v3](https://img.shields.io/badge/License-GPL%20v3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)
[![JCS-ELPA](https://raw.githubusercontent.com/jcs-emacs/badges/master/elpa/v/chatgpt.svg)](https://jcs-emacs.github.io/jcs-elpa/#/chatgpt)

# chatgpt
> Use ChatGPT inside Emacs

[![CI](https://github.com/emacs-openai/chatgpt/actions/workflows/test.yml/badge.svg)](https://github.com/emacs-openai/chatgpt/actions/workflows/test.yml)

This Emacs Code extension allows you to use the official OpenAI API to generate
code or natural language responses from OpenAI's [ChatGPT](https://openai.com/blog/chatgpt)
to your questions, right within the editor.

*P.S. This plugin focuses on experience with making conversations with ChatGPT!*

<p align="center">
<img alt="explain" src="./etc/demo.gif"/>
</p>

## 💾 Installation

#### package.el

This package is available from [JCS-ELPA](https://jcs-emacs.github.io/jcs-elpa/).
Install from these repositories then you should be good to go!

Normally, you don't need to add `(require 'chatgpt)` to your configuration since
most `'chatgpt` commands are autoload and can be called without loading the module!

#### use-package

If you are using [use-package](https://www.emacswiki.org/emacs/UsePackage),
add the following to your `init.el` file:

```elisp
(use-package chatgpt :ensure t)
```

or with `straight.el`:

```elisp
(use-package chatgpt
  :straight (chatgpt :type git :host github :repo "emacs-openai/chatgpt"))
```

#### Manual installation

Copy all `.el` files in this repository to `~/.emacs.d/lisp` and add the following:

```elisp
(add-to-list 'load-path "~/.emacs.d/lisp/")
(require 'chatgpt)
```

## 🔑 Obtaining API key

To use this extension, you will need an API key from OpenAI. To obtain one,
follow these steps:

1. Go to [OpenAI's website](https://beta.openai.com/account/api-keys). If you
don't have an account, you will need to create one or sign up using your Google
or Microsoft account.
2. Click on the `Create new secret key` button.
3. Copy the key and paste into the 'API Key' field under the 'openai' custom group settings.

When you create a new account, you receive $18 in free credits for the API which
you must use in the first 90 days. You can see pricing information
[here](https://openai.com/api/pricing/). 1000 tokens are about 700 words, and
you can see the token count for each request at the end of the response in the
sidebar.

## 🔨 Usage

To start this package:

```
M-x chatgpt
```

You will then be asked to insert your response; in this window, you press
<kbd>return</kbd> to send the message, and <kbd>Shift</kbd>+<kbd>return</kbd>
to insert a newline like a normal browser!

## 📝 Customization

#### 🧪 Variables

- `chatgpt-model` - ID of the model to use. (Default: `"gpt-3.5-turbo"`)
- `chatgpt-max-tokens` - The maximum number of tokens to generate in the completion. (Default: `2000`)
- `chatgpt-temperature` - What sampling temperature to use. (Default: `1.0`)
- `chatgpt-input-method` - Method to receive input. (Default: `'window`)
- `chatgpt-spinner-type` - Type of the spinner. (Default: `'moon`)
- `chatgpt-display-tokens-info` - Non-nil we display tokens information for each request. (Default: `t`)
- `chatgpt-animate-text` - Display text gradually instead of output it all at once. (Default: `t`)
- `chatgpt-animate-fps` - Frame per seconds to display text animation. (Default: `5`)

## 🔗 References

- [ChatGPT.el](https://github.com/joshcho/ChatGPT.el)
- [gptel](https://github.com/karthink/gptel)

## Contribute

[![PRs Welcome](https://img.shields.io/badge/PRs-welcome-brightgreen.svg)](http://makeapullrequest.com)
[![Elisp styleguide](https://img.shields.io/badge/elisp-style%20guide-purple)](https://github.com/bbatsov/emacs-lisp-style-guide)
[![Donate on paypal](https://img.shields.io/badge/paypal-donate-1?logo=paypal&color=blue)](https://www.paypal.me/jcs090218)
[![Become a patron](https://img.shields.io/badge/patreon-become%20a%20patron-orange.svg?logo=patreon)](https://www.patreon.com/jcs090218)

If you would like to contribute to this project, you may either
clone and make pull requests to this repository. Or you can
clone the project and establish your own branch of this tool.
Any methods are welcome!
