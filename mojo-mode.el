;;; mojo-mode.el --- Major mode for the Mojo programming language -*- lexical-binding: t; -*-

;; Copyright (C) 2026 mojo-mode contributors

;; Author: mojo-mode contributors
;; Maintainer: mojo-mode contributors
;; Version: 0.1.0
;; Package-Requires: ((emacs "30.1"))
;; Keywords: languages, mojo
;; URL: https://github.com/mojo-mode/mojo-mode
;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this file.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Major mode for editing Mojo source.
;;
;; Provides file association, a syntax table, font-lock, imenu, an
;; Eglot registration for `mojo-lsp-server', and block indentation.
;; The indenter lives in `mojo-indent.el'.  A REPL and compile or
;; format commands are not implemented yet.
;;
;; Keyword lists follow the Mojo 1.1 language reference
;; <https://mojolang.org/docs/reference/keywords/>.  Removed spellings
;; (`fn', `let', `alias', `owned', ...) are not highlighted as keywords.

;;; Code:

(require 'eglot)
(require 'mojo-indent)

(defgroup mojo nil
  "Major mode for the Mojo programming language."
  :group 'languages
  :prefix "mojo-"
  :link '(url-link "https://mojolang.org/docs/"))

(defcustom mojo-lsp-server-command '("mojo-lsp-server")
  "Command and arguments used to start the Mojo language server.
The executable is the `mojo-lsp-server' shipped with the Mojo SDK."
  :type '(repeat string)
  :group 'mojo)

;;;; Syntax

(defvar mojo-mode-syntax-table
  (let ((table (make-syntax-table)))
    ;; Identifiers.  Mojo identifiers are ASCII letters, digits, and
    ;; underscore; backtick-escaped names are handled as punctuation.
    (modify-syntax-entry ?_ "_" table)
    ;; Comments and the single hash used by `#` line comments.
    (modify-syntax-entry ?# "<" table)
    (modify-syntax-entry ?\n ">" table)
    ;; Expression delimiters.
    (modify-syntax-entry ?\( "()" table)
    (modify-syntax-entry ?\) ")(" table)
    (modify-syntax-entry ?\[ "(]" table)
    (modify-syntax-entry ?\] ")[" table)
    (modify-syntax-entry ?\{ "(}" table)
    (modify-syntax-entry ?\} "){" table)
    ;; Strings.  Triple quotes are recognized by font-lock below;
    ;; the syntax table treats a quote as a string fence.
    (modify-syntax-entry ?\" "\"" table)
    (modify-syntax-entry ?\' "\"" table)
    ;; Escape character inside strings.
    (modify-syntax-entry ?\\ "\\" table)
    ;; Operators and separators that should not stick to symbols.
    (dolist (char '(?+ ?- ?* ?/ ?% ?& ?| ?^ ?~ ?< ?> ?= ?! ?? ?: ?, ?\; ?. ?@))
      (modify-syntax-entry char "." table))
    ;; Backticks quote an escaped identifier.  Treat the contents as a
    ;; string so a keyword used as a name is not font-locked as one.
    (modify-syntax-entry ?` "\"" table)
    table)
  "Syntax table used in `mojo-mode'.")

;;;; Font-lock

(defconst mojo-keywords
  '("and" "as" "assert" "break" "comptime" "continue" "def" "elif" "else"
    "except" "finally" "for" "from" "if" "import" "in" "is" "lambda" "not"
    "or" "pass" "raise" "ref" "return" "struct" "trait" "try" "var" "where"
    "while" "with")
  "Hard keywords from the Mojo language reference.

`raises' is a declaration modifier rather than a statement keyword, and
is font-locked separately.  Removed spellings (`fn', `let', `alias',
`owned', `borrowed', `inout') are intentionally absent.")

(defconst mojo-constants
  '("True" "False" "None" "Self")
  "Literal keywords and the enclosing-type keyword `Self'.")

(defconst mojo-declaration-modifiers
  '("raises" "capturing")
  "Words with fixed meaning on a declaration that are not hard keywords.")

(defconst mojo-argument-conventions
  '("imm" "mut" "out" "deinit" "read" "owned")
  "Argument conventions.

`var' and `ref' are hard keywords and live in `mojo-keywords'.  `imm' is
the default and is rarely written.  `read' and `owned' are removed
conventions; they are still highlighted so leftover code stands out.")

(defconst mojo-decorators
  '("always_inline" "fieldwise_init" "implicit" "parameter" "staticmethod"
    "value")
  "Decorator names commonly recognized by the compiler.

The list is not exhaustive.  Unknown decorators are still highlighted
as decorator names by the `@name' matcher.")

(defconst mojo-builtin-types
  '("AnyType" "Bool" "DType" "Dict" "Error" "Float16" "Float32" "Float64"
    "ImmutableAnyOrigin" "Int" "Int8" "Int16" "Int32" "Int64" "Int128"
    "List" "NoneType" "Optional" "SIMD" "Span" "String" "StringSlice"
    "UInt" "UInt8" "UInt16" "UInt32" "UInt64" "UInt128")
  "Prelude type names highlighted as types.

This is a small, stable subset of the prelude, not the whole standard
library.  User types are highlighted by the declaration matchers.")

(defconst mojo-builtin-functions
  '("abs" "alloc" "len" "max" "min" "ord" "print" "range" "rebind" "repr"
    "round")
  "Prelude functions highlighted as builtins.")

(defconst mojo-font-lock-keywords
  `(
    ;; Decorators: @name, including dotted names.
    ("^\\s-*\\(@\\)\\(\\(?:\\sw\\|\\s_\\)+\\(?:\\.\\(?:\\sw\\|\\s_\\)+\\)*\\)"
     (1 font-lock-preprocessor-face)
     (2 font-lock-preprocessor-face))

    ;; def name.  The name is only highlighted when a parameter list
    ;; follows, so `mut self` inside the list is not taken for one.
    ("\\_<def\\_>\\s-+\\(\\(?:\\sw\\|\\s_\\)+\\)\\s-*[\\[(]"
     (1 font-lock-function-name-face))

    ;; struct / trait name.
    ("\\_<\\(?:struct\\|trait\\)\\_>\\s-+\\(\\(?:\\sw\\|\\s_\\)+\\)"
     (1 font-lock-type-face))

    ;; `var name` and `ref name` bindings.
    ("\\_<\\(?:var\\|ref\\)\\_>\\s-+\\(\\(?:\\sw\\|\\s_\\)+\\)"
     (1 font-lock-variable-name-face))

    ;; Hard keywords.
    (,(regexp-opt mojo-keywords 'symbols) . font-lock-keyword-face)

    ;; Declaration modifiers (`raises`, `capturing`).
    (,(regexp-opt mojo-declaration-modifiers 'symbols) . font-lock-keyword-face)

    ;; Argument conventions.  Matched only when they introduce a name,
    ;; so a local variable called `out` is left alone.
    (,(concat "\\(" (regexp-opt mojo-argument-conventions 'symbols) "\\)"
              "\\s-+\\(?:\\sw\\|\\s_\\)+\\_>")
     (1 font-lock-keyword-face))

    ;; Literals and `Self`.
    (,(regexp-opt mojo-constants 'symbols) . font-lock-constant-face)

    ;; `self` is a convention, not a keyword.
    ("\\_<self\\_>" . font-lock-variable-name-face)

    ;; Dunder methods and attributes.
    ("\\_<__\\(?:\\sw\\|\\s_\\)+__\\_>" . font-lock-builtin-face)

    ;; Known decorators written without being caught above, and builtins.
    (,(regexp-opt mojo-decorators 'symbols) . font-lock-builtin-face)
    (,(regexp-opt mojo-builtin-types 'symbols) . font-lock-type-face)
    (,(regexp-opt mojo-builtin-functions 'symbols) . font-lock-builtin-face)

    ;; Transfer sigil.  `^` is also bitwise XOR; only highlight the
    ;; postfix transfer form (`value^`).
    ("\\(?:\\sw\\|\\s_\\)\\(\\^\\)" (1 font-lock-keyword-face)))
  "Font-lock rules for `mojo-mode'.")

;;;; Imenu

(defconst mojo-imenu-generic-expression
  `(("Traits" "^\\s-*trait\\s-+\\(\\(?:\\sw\\|\\s_\\)+\\)" 1)
    ("Structs" "^\\s-*\\(?:@\\(?:\\sw\\|\\s_\\)+\\s-*\\)*struct\\s-+\\(\\(?:\\sw\\|\\s_\\)+\\)" 1)
    ("Functions" "^\\s-*\\(?:@\\(?:\\sw\\|\\s_\\)+\\s-*\\)*def\\s-+\\(\\(?:\\sw\\|\\s_\\)+\\)" 1)
    ("Comptime" "^\\s-*comptime\\s-+\\(\\(?:\\sw\\|\\s_\\)+\\)" 1))
  "Imenu index for top-level Mojo declarations.")

;;;; Eglot

(defun mojo-eglot-contact (_interactive)
  "Return the Eglot contact for the current Mojo buffer.
Honors `mojo-lsp-server-command'."
  mojo-lsp-server-command)

(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
               '(mojo-mode . mojo-eglot-contact)))

;;;; Mode

(defvar mojo-mode-map
  (let ((map (make-sparse-keymap)))
    map)
  "Keymap for `mojo-mode'.
Command bindings are added as run, build, and format support lands.")

;;;###autoload
(define-derived-mode mojo-mode prog-mode "Mojo"
  "Major mode for editing Mojo source.

\\{mojo-mode-map}"
  :syntax-table mojo-mode-syntax-table
  :group 'mojo
  (setq-local font-lock-defaults '(mojo-font-lock-keywords nil nil nil))
  (setq-local indent-line-function #'mojo-indent-line-function)
  (setq-local indent-region-function #'mojo-indent-region)
  (setq-local indent-tabs-mode nil)
  (setq-local tab-width mojo-indent-offset)
  (add-hook 'post-self-insert-hook
            #'mojo-indent-post-self-insert-function nil 'local)
  (setq-local comment-start "# ")
  (setq-local comment-start-skip "#+[ \t]*")
  (setq-local comment-end "")
  (setq-local comment-use-syntax t)
  (setq-local parse-sexp-ignore-comments t)
  (setq-local imenu-generic-expression mojo-imenu-generic-expression)
  (setq-local electric-indent-chars (append '(?:) electric-indent-chars)))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.mojo\\'" . mojo-mode))
;;;###autoload
(add-to-list 'auto-mode-alist '("\\.🔥\\'" . mojo-mode))

;;;###autoload
(add-to-list 'interpreter-mode-alist '("mojo" . mojo-mode))

(provide 'mojo-mode)

;;; mojo-mode.el ends here
