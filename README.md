# Semantic highlighting for Emacs Lisp

## Installation

Install from Git with your package manager of choice.

## Usage

```elisp
(add-hook 'emacs-lisp-mode-hook #'semel-mode)
(add-hook 'emacs-lisp-mode-hook #'cursor-sensor-mode)  ; Optional.
```

`semel-mode` analyzes Emacs Lisp code, highlighting each symbol it
encounters according to its meaning (semantic highlighting).

It also adds some help text to symbols via the `help-echo` text
property, so you can hover over a symbol with the mouse to learn
exactly what that symbol does/means.

If you additionally enable `cursor-sensor-mode`, then `semel-mode`
also highlights all occurrences of the local variable at point with a
bold face.

## Screenshot

![screenshot](screenshot.png)
