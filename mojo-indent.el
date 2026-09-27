;;; mojo-indent.el --- Indentation engine for mojo-mode -*- lexical-binding: t; -*-

;; Copyright (C) 2013-2026 Free Software Foundation, Inc.
;; Copyright (C) 2026 mojo-mode contributors

;; Author: Fabián Ezequiel Gallina <fgallina@gnu.org>
;; Maintainer: mojo-mode contributors
;; Version: 0.1.0
;; Package-Requires: ((emacs "30.1"))
;; Keywords: languages, mojo
;; URL: https://github.com/mojo-mode/mojo-mode
;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is part of mojo-mode.

;; mojo-mode is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; mojo-mode is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with mojo-mode.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Indentation for `mojo-mode'.
;;
;; The engine is the indentation, navigation, and syntax-context
;; support from GNU Emacs's python.el, adapted for Mojo.
;;
;; python.el is Copyright (C) 2013-2026 Free Software Foundation, Inc.,
;; written by Fabián Ezequiel Gallina.  This file was ported from the
;; python.el shipped with GNU Emacs 30.2.  Mojo's block openers are
;; `def', `struct', `trait', `comptime', `if', `elif', `else', `try',
;; `except', `finally', `for', `while', and `with'.  Python's `class',
;; `async', `match', and `case' forms were removed.  A split block
;; header indents one level, not two.

;;; Code:

(require 'cl-lib)

(defcustom mojo-indent-offset 4
  "Number of columns to indent each nested Mojo block."
  :type 'integer
  :safe #'integerp
  :group 'mojo)

(defcustom mojo-indent-trigger-commands
  '(indent-for-tab-command yas-expand yas/expand)
  "Commands that cycle indentation levels when repeated."
  :type '(repeat symbol)
  :group 'mojo)

(defcustom mojo-indent-block-paren-deeper nil
  "When non-nil, indent bracketed block headers deeper than the body.

With this non-nil:

    if (some_expression
            and another_expression):
        do_something()

With this nil:

    if (some_expression
        and another_expression):
        do_something()

Only applies when the opening bracket is followed by something other
than whitespace on the same line."
  :type 'boolean
  :safe #'booleanp
  :group 'mojo)


(defmacro mojo-rx (&rest regexps)
  "Mojo mode specialized rx macro.
This variant of `rx' supports the Mojo forms the indent engine matches."
  `(rx-let ((sp-bsnl (or space (and ?\\ ?\n)))
            (block-start       (seq symbol-start
                                    (or "def" "struct" "trait" "comptime"
                                        "if" "elif" "else" "try"
                                        "except" "finally" "for" "while" "with")
                                    symbol-end))
            (dedenter          (seq symbol-start
                                    (or "elif" "else" "except" "finally")
                                    symbol-end))
            (block-ender       (seq symbol-start
                                    (or
                                     "break" "continue" "pass" "raise" "return")
                                    symbol-end))
            (decorator         (seq line-start (* space) ?@ (any letter ?_)
                                    (* (any word ?_))))
            (defun             (seq symbol-start
                                    (or "def" "struct" "trait")
                                    symbol-end))
            (if-name-main      (seq line-start "if" (+ space) "__name__"
                                    (+ space) "==" (+ space)
                                    (any ?' ?\") "__main__" (any ?' ?\")
                                    (* space) ?:))
            (symbol-name       (seq (any letter ?_) (* (any word ?_))))
            (assignment-target (seq (? ?*)
                                    (* symbol-name ?.) symbol-name
                                    (? ?\[ (+ (not ?\])) ?\])))
            (grouped-assignment-target (seq (? ?*)
                                            (* symbol-name ?.) (group symbol-name)
                                            (? ?\[ (+ (not ?\])) ?\])))
            (open-paren        (or "{" "[" "("))
            (close-paren       (or "}" "]" ")"))
            (simple-operator   (any ?+ ?- ?/ ?& ?^ ?~ ?| ?* ?< ?> ?= ?%))
            (not-simple-operator (not (or simple-operator ?\n)))
            (operator          (or "==" ">="
                                   "**" "//" "<<" ">>" "<=" "!="
                                   "+" "-" "/" "&" "^" "~" "|" "*" "<" ">"
                                   "=" "%"))
            (assignment-operator (or "+=" "-=" "*=" "/=" "//=" "%=" "**="
                                     ">>=" "<<=" "&=" "^=" "|="
                                     "="))
            (string-delimiter  (seq
                                ;; Match even number of backslashes.
                                (or (not (any ?\\ ?\' ?\")) point
                                    ;; Quotes might be preceded by an
                                    ;; escaped quote.
                                    (and (or (not (any ?\\)) point) ?\\
                                         (* ?\\ ?\\) (any ?\' ?\")))
                                (* ?\\ ?\\)
                                ;; Match single or triple quotes of any kind.
                                (group (or  "\"\"\"" "\"" "'''" "'"))))
            (coding-cookie (seq line-start ?# (* space)
                                (or
                                 ;; # coding=<encoding name>
                                 (: "coding" (or ?: ?=) (* space)
                                    (group-n 1 (+ (or word ?-))))
                                 ;; # -*- coding: <encoding name> -*-
                                 (: "-*-" (* space) "coding:" (* space)
                                    (group-n 1 (+ (or word ?-)))
                                    (* space) "-*-")
                                 ;; # vim: set fileencoding=<encoding name> :
                                 (: "vim:" (* space) "set" (+ space)
                                    "fileencoding" (* space) ?= (* space)
                                    (group-n 1 (+ (or word ?-)))
                                    (* space) ":"))))
            (bytes-escape-sequence
             (seq (not "\\")
                  (group (or "\\\\" "\\'" "\\a" "\\b" "\\f"
                             "\\n" "\\r" "\\t" "\\v"
                             (seq "\\" (** 1 3 (in "0-7")))
                             (seq "\\x" hex hex)))))
            (string-escape-sequence
             (or bytes-escape-sequence
                 (seq (not "\\")
                      (or (group-n 1 "\\u" (= 4 hex))
                          (group-n 1 "\\U" (= 8 hex))
                          (group-n 1 "\\N{" (*? anychar) "}"))))))
     (rx ,@regexps)))




(eval-and-compile
  (defun mojo-syntax--context-compiler-macro (form type &optional syntax-ppss)
    (pcase type
      (''comment
       `(let ((ppss (or ,syntax-ppss (syntax-ppss))))
          (and (nth 4 ppss) (nth 8 ppss))))
      (''string
       `(let ((ppss (or ,syntax-ppss (syntax-ppss))))
          (and (nth 3 ppss) (nth 8 ppss))))
      (''single-quoted-string
       `(let ((ppss (or ,syntax-ppss (syntax-ppss))))
          (and (characterp (nth 3 ppss)) (nth 8 ppss))))
      (''triple-quoted-string
       `(let ((ppss (or ,syntax-ppss (syntax-ppss))))
          (and (eq t (nth 3 ppss)) (nth 8 ppss))))
      (''paren
       `(nth 1 (or ,syntax-ppss (syntax-ppss))))
      (_ form))))

(defun mojo-syntax-context (type &optional syntax-ppss)
  "Return non-nil if point is on TYPE using SYNTAX-PPSS.
TYPE can be `comment', `string', `single-quoted-string',
`triple-quoted-string' or `paren'.  It returns the start
character address of the specified TYPE."
  (declare (compiler-macro mojo-syntax--context-compiler-macro))
  (let ((ppss (or syntax-ppss (syntax-ppss))))
    (pcase type
      ('comment (and (nth 4 ppss) (nth 8 ppss)))
      ('string (and (nth 3 ppss) (nth 8 ppss)))
      ('single-quoted-string (and (characterp (nth 3 ppss)) (nth 8 ppss)))
      ('triple-quoted-string (and (eq t (nth 3 ppss)) (nth 8 ppss)))
      ('paren (nth 1 ppss))
      (_ nil))))

(defun mojo-syntax-context-type (&optional syntax-ppss)
  "Return the context type using SYNTAX-PPSS.
The type returned can be `comment', `string' or `paren'."
  (let ((ppss (or syntax-ppss (syntax-ppss))))
    (cond
     ((nth 8 ppss) (if (nth 4 ppss) 'comment 'string))
     ((nth 1 ppss) 'paren))))

(defsubst mojo-syntax-comment-or-string-p (&optional ppss)
  "Return non-nil if PPSS is inside comment or string."
  (nth 8 (or ppss (syntax-ppss))))

(defsubst mojo-syntax-closing-paren-p ()
  "Return non-nil if char after point is a closing paren."
  (eql (syntax-class (syntax-after (point)))
       (syntax-class (string-to-syntax ")"))))


(defsubst mojo-syntax-count-quotes (quote-char &optional point limit)
  "Count number of quotes around point (max is 3).
QUOTE-CHAR is the quote char to count.  Optional argument POINT is
the point where scan starts (defaults to current point), and LIMIT
is used to limit the scan."
  (let ((i 0))
    (while (and (< i 3)
                (or (not limit) (< (+ point i) limit))
                (eq (char-after (+ point i)) quote-char))
      (setq i (1+ i)))
    i))

(defun mojo-indent-context ()
  "Get information about the current indentation context.
Context is returned in a cons with the form (STATUS . START).

STATUS can be one of the following:

keyword
-------

:after-comment
 - Point is after a comment line.
 - START is the position of the \"#\" character.
:inside-string
 - Point is inside string.
 - START is the position of the first quote that starts it.
:no-indent
 - No possible indentation case matches.
 - START is always zero.

:inside-paren
 - Fallback case when point is inside paren.
 - START is the first non space char position *after* the open paren.
:inside-paren-at-closing-nested-paren
 - Point is on a line that contains a nested paren closer.
 - START is the position of the open paren it closes.
:inside-paren-at-closing-paren
 - Point is on a line that contains a paren closer.
 - START is the position of the open paren.
:inside-paren-newline-start
 - Point is inside a paren with items starting in their own line.
 - START is the position of the open paren.
:inside-paren-newline-start-from-block
 - Point is inside a paren with items starting in their own line
   from a block start.
 - START is the position of the open paren.
:inside-paren-from-block
 - Point is inside a paren from a block start followed by some
   items on the same line.
 - START is the first non space char position *after* the open paren.
:inside-paren-continuation-line
 - Point is on a continuation line inside a paren.
 - START is the position where the previous line (excluding lines
   for inner parens) starts.

:after-backslash
 - Fallback case when point is after backslash.
 - START is the char after the position of the backslash.
:after-backslash-assignment-continuation
 - Point is after a backslashed assignment.
 - START is the char after the position of the backslash.
:after-backslash-block-continuation
 - Point is after a backslashed block continuation.
 - START is the char after the position of the backslash.
:after-backslash-dotted-continuation
 - Point is after a backslashed dotted continuation.  Previous
   line must contain a dot to align with.
 - START is the char after the position of the backslash.
:after-backslash-first-line
 - First line following a backslashed continuation.
 - START is the char after the position of the backslash.

:after-block-end
 - Point is after a line containing a block ender.
 - START is the position where the ender starts.
:after-block-start
 - Point is after a line starting a block.
 - START is the position where the block starts.
:after-line
 - Point is after a simple line.
 - START is the position where the previous line starts.
:at-dedenter-block-start
 - Point is on a line starting a dedenter block.
 - START is the position where the dedenter block starts."
    (let ((ppss (save-excursion
                  (beginning-of-line)
                  (syntax-ppss))))
      (cond
       ;; Beginning of buffer.
       ((= (line-number-at-pos) 1)
        (cons :no-indent 0))
       ;; Inside a string.
       ((let ((start (mojo-syntax-context 'string ppss)))
          (when start
            (cons (if (mojo-info-docstring-p)
                      :inside-docstring
                    :inside-string) start))))
       ;; Inside a paren.
       ((let* ((start (mojo-syntax-context 'paren ppss))
               (starts-in-newline
                (when start
                  (save-excursion
                    (goto-char start)
                    (forward-char)
                    (not
                     (= (line-number-at-pos)
                        (progn
                          (mojo-util-forward-comment)
                          (line-number-at-pos)))))))
               (continuation-start
                (when start
                  (save-excursion
                    (forward-line -1)
                    (back-to-indentation)
                    ;; Skip inner parens.
                    (cl-loop with prev-start = (mojo-syntax-context 'paren)
                             while (and prev-start (>= prev-start start))
                             if (= prev-start start)
                             return (point)
                             else do (goto-char prev-start)
                                     (back-to-indentation)
                                     (setq prev-start
                                           (mojo-syntax-context 'paren)))))))
          (when start
            (cond
             ;; Current line only holds the closing paren.
             ((save-excursion
                (skip-syntax-forward " ")
                (when (and (mojo-syntax-closing-paren-p)
                           (progn
                             (forward-char 1)
                             (not (mojo-syntax-context 'paren))))
                  (cons :inside-paren-at-closing-paren start))))
             ;; Current line only holds a closing paren for nested.
             ((save-excursion
                (back-to-indentation)
                (mojo-syntax-closing-paren-p))
              (cons :inside-paren-at-closing-nested-paren start))
             ;; This line is a continuation of the previous line.
             (continuation-start
              (cons :inside-paren-continuation-line continuation-start))
             ;; This line starts from an opening block in its own line.
             ((save-excursion
                (goto-char start)
                (when (and
                       starts-in-newline
                       (save-excursion
                         (back-to-indentation)
                         (looking-at (mojo-rx block-start))))
                  (cons
                   :inside-paren-newline-start-from-block start))))
             (starts-in-newline
              (cons :inside-paren-newline-start start))
             ;; General case.
             (t (let ((after-start (save-excursion
                               (goto-char (1+ start))
                               (skip-syntax-forward "(" 1)
                               (skip-syntax-forward " ")
                               (point))))
                  (if (save-excursion
                        (mojo-nav-beginning-of-statement)
                        (mojo-info-looking-at-beginning-of-block))
                      (cons :inside-paren-from-block after-start)
                    (cons :inside-paren after-start))))))))
       ;; After backslash.
       ((let ((start (when (not (mojo-syntax-comment-or-string-p ppss))
                       (mojo-info-line-ends-backslash-p
                        (1- (line-number-at-pos))))))
          (when start
            (cond
             ;; Continuation of dotted expression.
             ((save-excursion
                (back-to-indentation)
                (when (eq (char-after) ?\.)
                  ;; Move point back until it's not inside a paren.
                  (while (prog2
                             (forward-line -1)
                             (and (not (bobp))
                                  (mojo-syntax-context 'paren))))
                  (goto-char (line-end-position))
                  (while (and (search-backward
                               "." (line-beginning-position) t)
                              (mojo-syntax-context-type)))
                  ;; Ensure previous statement has dot to align with.
                  (when (and (eq (char-after) ?\.)
                             (not (mojo-syntax-context-type)))
                    (cons :after-backslash-dotted-continuation (point))))))
             ;; Continuation of block definition.
             ((let ((block-continuation-start
                     (mojo-info-block-continuation-line-p)))
                (when block-continuation-start
                  (save-excursion
                    (goto-char block-continuation-start)
                    (re-search-forward
                     (mojo-rx block-start (* space))
                     (line-end-position) t)
                    (cons :after-backslash-block-continuation (point))))))
             ;; Continuation of assignment.
             ((let ((assignment-continuation-start
                     (mojo-info-assignment-continuation-line-p)))
                (when assignment-continuation-start
                  (save-excursion
                    (goto-char assignment-continuation-start)
                    (cons :after-backslash-assignment-continuation (point))))))
             ;; First line after backslash continuation start.
             ((save-excursion
                (goto-char start)
                (when (or (= (line-number-at-pos) 1)
                          (not (mojo-info-beginning-of-backslash
                                (1- (line-number-at-pos)))))
                  (cons :after-backslash-first-line start))))
             ;; General case.
             (t (cons :after-backslash start))))))
       ;; After beginning of block.
       ((let ((start (save-excursion
                       (back-to-indentation)
                       (mojo-util-forward-comment -1)
                       (when (equal (char-before) ?:)
                         (mojo-nav-beginning-of-block)))))
          (when start
            (cons :after-block-start start))))
       ;; At dedenter statement.
       ((let ((start (mojo-info-dedenter-statement-p)))
          (when start
            (cons :at-dedenter-block-start start))))
       ;; After normal line, comment or ender (default case).
       ((save-excursion
          (back-to-indentation)
          (skip-chars-backward " \t\n")
          (if (bobp)
              (cons :no-indent 0)
            (mojo-nav-beginning-of-statement)
            (cons
             (cond ((mojo-info-current-line-comment-p)
                    :after-comment)
                   ((save-excursion
                      (goto-char (line-end-position))
                      (mojo-util-forward-comment -1)
                      (mojo-nav-beginning-of-statement)
                      (looking-at (mojo-rx block-ender)))
                    :after-block-end)
                   (t :after-line))
             (point))))))))

(defun mojo-indent--calculate-indentation ()
  "Internal implementation of `mojo-indent-calculate-indentation'.
May return an integer for the maximum possible indentation at
current context or a list of integers.  The latter case is only
happening for :at-dedenter-block-start context since the
possibilities can be narrowed to specific indentation points."
    (save-excursion
      (pcase (mojo-indent-context)
        (`(:no-indent . ,_) (prog-first-column)) ; usually 0
        (`(,(or :after-line
                :after-comment
                :inside-string
                :after-backslash
                :inside-paren-continuation-line) . ,start)
         ;; Copy previous indentation.
         (goto-char start)
         (current-indentation))
        (`(,(or :inside-paren-at-closing-paren
                :inside-paren-at-closing-nested-paren) . ,start)
         (goto-char (+ 1 start))
         (if (looking-at "[ \t]*\\(?:#\\|$\\)")
             ;; Copy previous indentation.
             (current-indentation)
           ;; Align with opening paren.
           (current-column)))
        (`(:inside-docstring . ,start)
         (let* ((line-indentation (current-indentation))
                (base-indent (progn
                               (goto-char start)
                               (current-indentation))))
           (max line-indentation base-indent)))
        (`(,(or :after-block-start
                :after-backslash-first-line
                :after-backslash-assignment-continuation
                :inside-paren-newline-start) . ,start)
         ;; Add one indentation level.
         (goto-char start)
         (+ (current-indentation) mojo-indent-offset))
        (`(:after-backslash-block-continuation . ,start)
         (goto-char start)
         (let ((column (current-column)))
           (if (= column (+ (current-indentation) mojo-indent-offset))
               ;; Add one level to avoid same indent as next logical line.
               (+ column mojo-indent-offset)
             column)))
        (`(,(or :inside-paren
                :after-backslash-dotted-continuation) . ,start)
         ;; Use the column given by the context.
         (goto-char start)
         (current-column))
        (`(:after-block-end . ,start)
         ;; Subtract one indentation level.
         (goto-char start)
         (max 0 (- (current-indentation) mojo-indent-offset)))
        (`(:at-dedenter-block-start . ,_)
         ;; List all possible indentation levels from opening blocks.
         (let ((opening-block-start-points
                (mojo-info-dedenter-opening-block-positions)))
           (if (not opening-block-start-points)
               (prog-first-column) ; if not found default to first column
             (mapcar (lambda (pos)
                       (save-excursion
                         (goto-char pos)
                         (current-indentation)))
                     opening-block-start-points))))
        (`(,(or :inside-paren-newline-start-from-block) . ,start)
         ;; Mojo indents a split block header by one level.  Python's
         ;; engine multiplied by `python-indent-def-block-scale' here,
         ;; which defaults to two.
         (goto-char start)
         (+ (current-indentation) mojo-indent-offset))
        (`(,:inside-paren-from-block . ,start)
         (goto-char start)
         (let ((column (current-column)))
           (if (and mojo-indent-block-paren-deeper
                    (= column (+ (save-excursion
                                   (mojo-nav-beginning-of-statement)
                                   (current-indentation))
                                 mojo-indent-offset)))
               (+ column mojo-indent-offset)
             column))))))

(defun mojo-indent--calculate-levels (indentation)
  "Calculate levels list given INDENTATION.
Argument INDENTATION can either be an integer or a list of
integers.  Levels are returned in ascending order, and in the
case INDENTATION is a list, this order is enforced."
  (if (listp indentation)
      (sort (copy-sequence indentation) #'<)
    (nconc (number-sequence (prog-first-column) (1- indentation)
                            mojo-indent-offset)
           (list indentation))))

(defun mojo-indent--previous-level (levels indentation)
  "Return previous level from LEVELS relative to INDENTATION."
  (let* ((levels (sort (copy-sequence levels) #'>))
         (default (car levels)))
    (catch 'return
      (dolist (level levels)
        (when (funcall #'< level indentation)
          (throw 'return level)))
      default)))

(defun mojo-indent-calculate-indentation (&optional previous)
  "Calculate indentation.
Get indentation of PREVIOUS level when argument is non-nil.
Return the max level of the cycle when indentation reaches the
minimum."
  (let* ((indentation (mojo-indent--calculate-indentation))
         (levels (mojo-indent--calculate-levels indentation)))
    (if previous
        (mojo-indent--previous-level levels (current-indentation))
      (if levels
          (apply #'max levels)
        (prog-first-column)))))

(defun mojo-indent-line (&optional previous)
  "Internal implementation of `mojo-indent-line-function'.
Use the PREVIOUS level when argument is non-nil, otherwise indent
to the maximum available level.  When indentation is the minimum
possible and PREVIOUS is non-nil, cycle back to the maximum
level.  Return `noindent' when the indentation does not change, so
`indent-for-tab-command' can fall through to completion."
  (let ((follow-indentation-p
         ;; Check if point is within indentation.
         (and (<= (line-beginning-position) (point))
              (>= (+ (line-beginning-position)
                     (current-indentation))
                  (point))))
        (indentation (mojo-indent-calculate-indentation previous)))
    (if (= (current-indentation) indentation)
        'noindent
      (save-excursion
        (indent-line-to indentation)
        (mojo-info-dedenter-opening-block-message))
      (when follow-indentation-p
        (back-to-indentation)))))

(defun mojo-indent-calculate-levels ()
  "Return possible indentation levels."
  (mojo-indent--calculate-levels
   (mojo-indent--calculate-indentation)))

(defun mojo-indent-line-function ()
  "`indent-line-function' for `mojo-mode`.
When the variable `last-command' is equal to one of the symbols
inside `mojo-indent-trigger-commands' it cycles possible
indentation levels from right to left."
  (mojo-indent-line
   (and (memq this-command mojo-indent-trigger-commands)
        (eq last-command this-command))))

(defun mojo-indent-dedent-line ()
  "De-indent current line."
  (interactive "*")
  (when (and (not (bolp))
           (not (mojo-syntax-comment-or-string-p))
           (= (current-indentation) (current-column)))
      (mojo-indent-line t)
      t))

(defun mojo-indent-dedent-line-backspace (arg)
  "De-indent current line.
Argument ARG is passed to `backward-delete-char-untabify' when
point is not in between the indentation."
  (interactive "*p")
  (unless (mojo-indent-dedent-line)
    (backward-delete-char-untabify arg)))

(put 'mojo-indent-dedent-line-backspace 'delete-selection 'supersede)

(defun mojo-indent-region (start end)
  "Indent a Mojo region.

Called from a program, START and END specify the region to indent."
  (let ((deactivate-mark nil))
    (save-excursion
      (goto-char end)
      (setq end (point-marker))
      (goto-char start)
      (or (bolp) (forward-line 1))
      (while (< (point) end)
        (let ((indent
               (and (not (and (bolp) (eolp)))
                    ;; Skip an empty line.  python.el also skips a
                    ;; comment and the line after one, which leaves both
                    ;; the comment and the statement under it at column
                    ;; zero.  A comment takes the indentation of the
                    ;; line it describes.
                    (not (mojo-info-current-line-empty-p))
                    ;; Don't mess with strings, unless it's the
                    ;; enclosing set of quotes or a docstring.
                    (or (not (mojo-syntax-context 'string))
                        (equal
                         (syntax-after
                          (+ (1- (point))
                             (current-indentation)
                             (mojo-syntax-count-quotes (char-after) (point))))
                         (string-to-syntax "|"))
                        (mojo-info-docstring-p))
                    ;; A dedenter (`else', `elif', `except',
                    ;; `finally') that is already at or past the column
                    ;; of the block it closes is left where the user put
                    ;; it.  python.el skips dedenters and block enders
                    ;; unconditionally, which leaves an `else' or a
                    ;; `pass' typed at column zero where it is, and
                    ;; skips block openers too, which leaves a nested
                    ;; `if' at column zero and every line under it wrong.
                    (save-excursion
                      (back-to-indentation)
                      (let ((indentation (current-indentation))
                            (calc (mojo-indent-calculate-indentation)))
                        (cond
                         ((and (looking-at (mojo-rx dedenter))
                               (<= calc indentation))
                          nil)
                         (t calc)))))))
          (when indent
            (indent-line-to indent)))
        (forward-line 1))
      (move-marker end nil))))

(defun mojo-indent-shift-left (start end &optional count)
  "Shift lines contained in region START END by COUNT columns to the left.
COUNT defaults to `mojo-indent-offset'.  If region isn't
active, the current line is shifted.  The shifted region includes
the lines in which START and END lie.  An error is signaled if
any lines in the region are indented less than COUNT columns."
  (interactive
   (if mark-active
       (list (region-beginning) (region-end) current-prefix-arg)
     (list (line-beginning-position) (line-end-position) current-prefix-arg)))
  (if count
      (setq count (prefix-numeric-value count))
    (setq count mojo-indent-offset))
  (when (> count 0)
    (let ((deactivate-mark nil))
      (save-excursion
        (goto-char start)
        (while (< (point) end)
          (if (and (< (current-indentation) count)
                   (not (looking-at "[ \t]*$")))
              (user-error "Can't shift all lines enough"))
          (forward-line))
        (indent-rigidly start end (- count))))))

(defun mojo-indent-shift-right (start end &optional count)
  "Shift lines contained in region START END by COUNT columns to the right.
COUNT defaults to `mojo-indent-offset'.  If region isn't
active, the current line is shifted.  The shifted region includes
the lines in which START and END lie."
  (interactive
   (if mark-active
       (list (region-beginning) (region-end) current-prefix-arg)
     (list (line-beginning-position) (line-end-position) current-prefix-arg)))
  (let ((deactivate-mark nil))
    (setq count (if count (prefix-numeric-value count)
                  mojo-indent-offset))
    (indent-rigidly start end count)))

(defun mojo-indent-post-self-insert-function ()
  "Adjust indentation after insertion of some characters.
This function is intended to be added to `post-self-insert-hook.'
If a line renders a paren alone, after adding a char before it,
the line will be re-indented automatically if needed."
  (when (and electric-indent-mode
             (eq (char-before) last-command-event)
             (not (mojo-syntax-context 'string))
             (save-excursion
               (beginning-of-line)
               (not (mojo-syntax-context 'string (syntax-ppss)))))
    (cond
     ;; Electric indent inside parens
     ((and
       (not (bolp))
       (let ((paren-start (mojo-syntax-context 'paren)))
         ;; Check that point is inside parens.
         (when paren-start
           (not
            ;; Filter the case where input is happening in the same
            ;; line where the open paren is.
            (= (line-number-at-pos)
               (line-number-at-pos paren-start)))))
       ;; When content has been added before the closing paren or a
       ;; comma has been inserted, it's ok to do the trick.
       (or
        (memq (char-after) '(?\) ?\] ?\}))
        (eq (char-before) ?,)))
      (save-excursion
        (goto-char (line-beginning-position))
        (let ((indentation (mojo-indent-calculate-indentation)))
          (when (and (numberp indentation) (< (current-indentation) indentation))
            (indent-line-to indentation)))))
     ;; Electric colon
     ((and (eq ?: last-command-event)
           (memq ?: electric-indent-chars)
           (not current-prefix-arg)
           ;; Trigger electric colon only at end of line
           (eolp)
           ;; Avoid re-indenting on extra colon
           (not (equal ?: (char-before (1- (point)))))
           (not (mojo-syntax-comment-or-string-p)))
      ;; Just re-indent dedenters
      (let ((dedenter-pos (mojo-info-dedenter-statement-p)))
        (when dedenter-pos
          (let ((start (copy-marker dedenter-pos))
                (end (point-marker)))
            (save-excursion
              (goto-char start)
              (mojo-indent-line)
              (unless (= (line-number-at-pos start)
                         (line-number-at-pos end))
                ;; Reindent region if this is a multiline statement
                (mojo-indent-region start end))))))))))


(defun mojo-nav--syntactically (fn poscompfn &optional contextfn)
  "Move point using FN avoiding places with specific context.
FN must take no arguments.  POSCOMPFN is a two arguments function
used to compare current and previous point after it is moved
using FN, this is normally a less-than or greater-than
comparison.  Optional argument CONTEXTFN defaults to
`mojo-syntax-context-type' and is used for checking current
point context, it must return a non-nil value if this point must
be skipped."
  (let ((contextfn (or contextfn 'mojo-syntax-context-type))
        (start-pos (point-marker))
        (prev-pos))
    (catch 'found
      (while t
        (let* ((newpos
                (and (funcall fn) (point-marker)))
               (context (funcall contextfn)))
          (cond ((and (not context) newpos
                      (or (and (not prev-pos) newpos)
                          (and prev-pos newpos
                               (funcall poscompfn newpos prev-pos))))
                 (throw 'found (point-marker)))
                ((and newpos context)
                 (setq prev-pos (point)))
                (t (when (not newpos) (goto-char start-pos))
                   (throw 'found nil))))))))


(defun mojo-nav-beginning-of-statement ()
  "Move to start of current statement."
  (interactive "^")
  (forward-line 0)
  (let* ((ppss (syntax-ppss))
         (context-point
          (or
           (mojo-syntax-context 'paren ppss)
           (mojo-syntax-context 'string ppss))))
    (cond ((bobp))
          (context-point
           (goto-char context-point)
           (mojo-nav-beginning-of-statement))
          ((save-excursion
             (forward-line -1)
             (mojo-info-line-ends-backslash-p))
           (forward-line -1)
           (mojo-nav-beginning-of-statement))))
  (back-to-indentation)
  (point-marker))

(defun mojo-nav-end-of-statement (&optional noend)
  "Move to end of current statement.
Optional argument NOEND is internal and makes the logic to not
jump to the end of line when moving forward searching for the end
of the statement."
  (interactive "^")
  (let (string-start bs-pos (last-string-end 0))
    (while (and (or noend (goto-char (line-end-position)))
                (not (eobp))
                (cond ((setq string-start (mojo-syntax-context 'string))
                       ;; The condition can be nil if syntax table
                       ;; text properties and the `syntax-ppss' cache
                       ;; are somehow out of whack.  This has been
                       ;; observed when using `syntax-ppss' during
                       ;; narrowing.
                       (when (>= string-start last-string-end)
                         (goto-char string-start)
                         (if (mojo-syntax-context 'paren)
                             ;; Ended up inside a paren, roll again.
                             (mojo-nav-end-of-statement t)
                           ;; This is not inside a paren, move to the
                           ;; end of this string.
                           (goto-char (+ (point)
                                         (mojo-syntax-count-quotes
                                          (char-after (point)) (point))))
                           (setq
                            last-string-end
                            (or (if (eq t (nth 3 (syntax-ppss)))
                                    (re-search-forward
                                     (rx (syntax string-delimiter)) nil t)
                                  (ignore-error scan-error
                                    (goto-char string-start)
                                    (mojo-nav--lisp-forward-sexp)
                                    (point)))
                                (goto-char (point-max)))))))
                      ((mojo-syntax-context 'paren)
                       ;; The statement won't end before we've escaped
                       ;; at least one level of parenthesis.
                       (condition-case err
                           (goto-char (scan-lists (point) 1 -1))
                         (scan-error (goto-char (nth 3 err)))))
                      ((setq bs-pos (mojo-info-line-ends-backslash-p))
                       (goto-char bs-pos)
                       (forward-line 1))))))
  (point-marker))


(defun mojo-nav-beginning-of-block ()
  "Move to start of current block."
  (interactive "^")
  (let ((starting-pos (point)))
    ;; Go to first line beginning a statement
    (while (and (not (bobp))
                (or (and (mojo-nav-beginning-of-statement) nil)
                    (mojo-info-current-line-comment-p)
                    (mojo-info-current-line-empty-p)))
      (forward-line -1))
    (if (progn
          (mojo-nav-beginning-of-statement)
          (looking-at (mojo-rx block-start)))
        (point-marker)
      (let ((block-matching-indent
             (- (current-indentation) mojo-indent-offset)))
        (while
            (and (mojo-nav-backward-block)
                 (> (current-indentation) block-matching-indent)))
        (if (and (looking-at (mojo-rx block-start))
                 (= (current-indentation) block-matching-indent))
            (point-marker)
          (and (goto-char starting-pos) nil))))))

(defun mojo-nav-end-of-block ()
  "Move to end of current block."
  (interactive "^")
  (when (mojo-nav-beginning-of-block)
    (let ((block-indentation (current-indentation)))
      (mojo-nav-end-of-statement)
      (while (and (forward-line 1)
                  (not (eobp))
                  (or (and (> (current-indentation) block-indentation)
                           (or (mojo-nav-end-of-statement) t))
                      (mojo-info-current-line-comment-p)
                      (mojo-info-current-line-empty-p))))
      (mojo-util-forward-comment -1)
      (point-marker))))

(defun mojo-nav-backward-block (&optional arg)
  "Move backward to previous block of code.
With ARG, repeat.  See `mojo-nav-forward-block'."
  (interactive "^p")
  (or arg (setq arg 1))
  (mojo-nav-forward-block (- arg)))

(defun mojo-nav-forward-block (&optional arg)
  "Move forward to next block of code.
With ARG, repeat.  With negative argument, move ARG times
backward to previous block."
  (interactive "^p")
  (or arg (setq arg 1))
  (let ((block-start-regexp
         (mojo-rx line-start (* whitespace) block-start))
        (starting-pos (point))
        (orig-arg arg))
    (while (> arg 0)
      (mojo-nav-end-of-statement)
      (while (and
              (re-search-forward block-start-regexp nil t)
              (mojo-syntax-context-type)))
      (setq arg (1- arg)))
    (while (< arg 0)
      (mojo-nav-beginning-of-statement)
      (while (and
              (re-search-backward block-start-regexp nil t)
              (mojo-syntax-context-type)))
      (setq arg (1+ arg)))
    (mojo-nav-beginning-of-statement)
    (if (or (and (> orig-arg 0) (< (point) starting-pos))
            (not (looking-at (mojo-rx block-start))))
        (and (goto-char starting-pos) nil)
      (and (not (= (point) starting-pos)) (point-marker)))))

(defun mojo-nav--lisp-forward-sexp (&optional arg)
  "Standard version `forward-sexp'.
It ignores completely the value of `forward-sexp-function' by
setting it to nil before calling `forward-sexp'.  With positive
ARG move forward only one sexp, else move backwards."
  (let ((forward-sexp-function)
        (arg (if (or (not arg) (> arg 0)) 1 -1)))
    (forward-sexp arg)))

(defun mojo-nav--lisp-forward-sexp-safe (&optional arg)
  "Safe version of standard `forward-sexp'.
When at end of sexp (i.e. looking at an opening/closing paren)
skips it instead of throwing an error.  With positive ARG move
forward only one sexp, else move backwards."
  (let* ((arg (if (or (not arg) (> arg 0)) 1 -1))
         (paren-regexp
          (if (> arg 0) (mojo-rx close-paren) (mojo-rx open-paren)))
         (search-fn
          (if (> arg 0) #'re-search-forward #'re-search-backward)))
    (condition-case nil
        (mojo-nav--lisp-forward-sexp arg)
      (error
       (while (and (funcall search-fn paren-regexp nil t)
                   (mojo-syntax-context 'paren)))))))

(defun mojo-nav--forward-sexp (&optional dir safe skip-parens-p)
  "Move to forward sexp.
With positive optional argument DIR direction move forward, else
backwards.  When optional argument SAFE is non-nil do not throw
errors when at end of sexp, skip it instead.  With optional
argument SKIP-PARENS-P force sexp motion to ignore parenthesized
expressions when looking at them in either direction."
  (setq dir (or dir 1))
  (unless (= dir 0)
    (let* ((forward-p (if (> dir 0)
                          (and (setq dir 1) t)
                        (and (setq dir -1) nil)))
           (context-type (mojo-syntax-context-type)))
      (cond
       ((memq context-type '(string comment))
        ;; Inside of a string, get out of it.
        (let ((forward-sexp-function))
          (forward-sexp dir)))
       ((and (not skip-parens-p)
             (or (eq context-type 'paren)
                 (if forward-p
                     (eq (syntax-class (syntax-after (point)))
                         (car (string-to-syntax "(")))
                   (eq (syntax-class (syntax-after (1- (point))))
                       (car (string-to-syntax ")"))))))
        ;; Inside a paren or looking at it, lisp knows what to do.
        (if safe
            (mojo-nav--lisp-forward-sexp-safe dir)
          (mojo-nav--lisp-forward-sexp dir)))
       (t
        ;; This part handles the lispy feel of
        ;; `mojo-nav-forward-sexp'.  Knowing everything about the
        ;; current context and the context of the next sexp tries to
        ;; follow the lisp sexp motion commands in a symmetric manner.
        (let* ((context
                (cond
                 ((mojo-info-beginning-of-block-p) 'block-start)
                 ((mojo-info-end-of-block-p) 'block-end)
                 ((mojo-info-beginning-of-statement-p) 'statement-start)
                 ((mojo-info-end-of-statement-p) 'statement-end)))
               (next-sexp-pos
                (save-excursion
                  (if safe
                      (mojo-nav--lisp-forward-sexp-safe dir)
                    (mojo-nav--lisp-forward-sexp dir))
                  (point)))
               (next-sexp-context
                (save-excursion
                  (goto-char next-sexp-pos)
                  (cond
                   ((mojo-info-beginning-of-block-p) 'block-start)
                   ((mojo-info-end-of-block-p) 'block-end)
                   ((mojo-info-beginning-of-statement-p) 'statement-start)
                   ((mojo-info-end-of-statement-p) 'statement-end)
                   ((mojo-info-statement-starts-block-p) 'starts-block)
                   ((mojo-info-statement-ends-block-p) 'ends-block)))))
          (if forward-p
              (cond ((and (not (eobp))
                          (mojo-info-current-line-empty-p))
                     (mojo-util-forward-comment dir)
                     (mojo-nav--forward-sexp dir safe skip-parens-p))
                    ((eq context 'block-start)
                     (mojo-nav-end-of-block))
                    ((eq context 'statement-start)
                     (mojo-nav-end-of-statement))
                    ((and (memq context '(statement-end block-end))
                          (eq next-sexp-context 'ends-block))
                     (goto-char next-sexp-pos)
                     (mojo-nav-end-of-block))
                    ((and (memq context '(statement-end block-end))
                          (eq next-sexp-context 'starts-block))
                     (goto-char next-sexp-pos)
                     (mojo-nav-end-of-block))
                    ((memq context '(statement-end block-end))
                     (goto-char next-sexp-pos)
                     (mojo-nav-end-of-statement))
                    (t (goto-char next-sexp-pos)))
            (cond ((and (not (bobp))
                        (mojo-info-current-line-empty-p))
                   (mojo-util-forward-comment dir)
                   (mojo-nav--forward-sexp dir safe skip-parens-p))
                  ((eq context 'block-end)
                   (mojo-nav-beginning-of-block))
                  ((eq context 'statement-end)
                   (mojo-nav-beginning-of-statement))
                  ((and (memq context '(statement-start block-start))
                        (eq next-sexp-context 'starts-block))
                   (goto-char next-sexp-pos)
                   (mojo-nav-beginning-of-block))
                  ((and (memq context '(statement-start block-start))
                        (eq next-sexp-context 'ends-block))
                   (goto-char next-sexp-pos)
                   (mojo-nav-beginning-of-block))
                  ((memq context '(statement-start block-start))
                   (goto-char next-sexp-pos)
                   (mojo-nav-beginning-of-statement))
                  (t (goto-char next-sexp-pos))))))))))

(defun mojo-nav-forward-sexp (&optional arg safe skip-parens-p)
  "Move forward across expressions.
With ARG, do it that many times.  Negative arg -N means move
backward N times.  When optional argument SAFE is non-nil do not
throw errors when at end of sexp, skip it instead.  With optional
argument SKIP-PARENS-P force sexp motion to ignore parenthesized
expressions when looking at them in either direction (forced to t
in interactive calls)."
  (interactive "^p")
  (or arg (setq arg 1))
  ;; Do not follow parens on interactive calls.  This hack to detect
  ;; if the function was called interactively copes with the way
  ;; `forward-sexp' works by calling `forward-sexp-function', losing
  ;; interactive detection by checking `current-prefix-arg'.  The
  ;; reason to make this distinction is that lisp functions like
  ;; `blink-matching-open' get confused causing issues like the one in
  ;; Bug#16191.  With this approach the user gets a symmetric behavior
  ;; when working interactively while called functions expecting
  ;; paren-based sexp motion work just fine.
  (or
   skip-parens-p
   (setq skip-parens-p
         (memq real-this-command
               (list
                #'forward-sexp #'backward-sexp
                #'mojo-nav-forward-sexp #'mojo-nav-backward-sexp
                #'mojo-nav-forward-sexp-safe #'mojo-nav-backward-sexp))))
  (while (> arg 0)
    (mojo-nav--forward-sexp 1 safe skip-parens-p)
    (setq arg (1- arg)))
  (while (< arg 0)
    (mojo-nav--forward-sexp -1 safe skip-parens-p)
    (setq arg (1+ arg))))

(defun mojo-nav-backward-sexp (&optional arg safe skip-parens-p)
  "Move backward across expressions.
With ARG, do it that many times.  Negative arg -N means move
forward N times.  When optional argument SAFE is non-nil do not
throw errors when at end of sexp, skip it instead.  With optional
argument SKIP-PARENS-P force sexp motion to ignore parenthesized
expressions when looking at them in either direction (forced to t
in interactive calls)."
  (interactive "^p")
  (or arg (setq arg 1))
  (mojo-nav-forward-sexp (- arg) safe skip-parens-p))

(defun mojo-nav-forward-sexp-safe (&optional arg skip-parens-p)
  "Move forward safely across expressions.
With ARG, do it that many times.  Negative arg -N means move
backward N times.  With optional argument SKIP-PARENS-P force
sexp motion to ignore parenthesized expressions when looking at
them in either direction (forced to t in interactive calls)."
  (interactive "^p")
  (mojo-nav-forward-sexp arg t skip-parens-p))


(defun mojo-info-statement-starts-block-p ()
  "Return non-nil if current statement opens a block."
  (save-excursion
    (mojo-nav-beginning-of-statement)
    (looking-at (mojo-rx block-start))))

(defun mojo-info-statement-ends-block-p ()
  "Return non-nil if point is at end of block."
  (let ((end-of-block-pos (save-excursion
                            (mojo-nav-end-of-block)))
        (end-of-statement-pos (save-excursion
                                (mojo-nav-end-of-statement))))
    (and end-of-block-pos end-of-statement-pos
         (= end-of-block-pos end-of-statement-pos))))

(defun mojo-info-beginning-of-statement-p ()
  "Return non-nil if point is at beginning of statement."
  (= (point) (save-excursion
               (mojo-nav-beginning-of-statement)
               (point))))

(defun mojo-info-end-of-statement-p ()
  "Return non-nil if point is at end of statement."
  (= (point) (save-excursion
               (mojo-nav-end-of-statement)
               (point))))

(defun mojo-info-beginning-of-block-p ()
  "Return non-nil if point is at beginning of block."
  (and (mojo-info-beginning-of-statement-p)
       (mojo-info-statement-starts-block-p)))

(defun mojo-info-end-of-block-p ()
  "Return non-nil if point is at end of block."
  (and (mojo-info-end-of-statement-p)
       (mojo-info-statement-ends-block-p)))

(defun mojo-info-dedenter-opening-block-position ()
  "Return the point of the closest block the current line closes.
Returns nil if point is not on a dedenter statement or no opening
block can be detected.  The latter case meaning current file is
likely an invalid Mojo file."
  (let ((positions (mojo-info-dedenter-opening-block-positions))
        (indentation (current-indentation))
        (position))
    (while (and (not position)
                positions)
      (save-excursion
        (goto-char (car positions))
        (if (<= (current-indentation) indentation)
            (setq position (car positions))
          (setq positions (cdr positions)))))
    position))

(defun mojo-info-dedenter-opening-block-positions ()
  "Return points of blocks the current line may close sorted by closer.
Returns nil if point is not on a dedenter statement or no opening
block can be detected.  The latter case meaning current file is
likely an invalid Mojo file."
  (save-excursion
    (let ((dedenter-pos (mojo-info-dedenter-statement-p)))
      (when dedenter-pos
        (goto-char dedenter-pos)
        (let* ((cur-line (line-beginning-position))
               ;; Mojo has no `case'.  `else' also closes a `with`
               ;; block, which Python's list did not include.
               (pairs '(("elif" "elif" "if")
                        ("else" "if" "elif" "except" "for" "while" "with")
                        ("except" "except" "try")
                        ("finally" "else" "except" "try")))
               (dedenter (match-string-no-properties 0))
               (possible-opening-blocks (cdr (assoc-string dedenter pairs)))
               (collected-indentations)
               (opening-blocks))
          (catch 'exit
            (while (mojo-nav--syntactically
                    (lambda ()
                      (cl-loop while (re-search-backward (mojo-rx block-start) nil t)
                               if (save-match-data
                                    (looking-back (rx line-start (* whitespace))
                                                  (line-beginning-position)))
                               return t))
                    #'<)
              (let ((indentation (current-indentation)))
                (when (and (not (memq indentation collected-indentations))
                           (or (not collected-indentations)
                               (< indentation
                                  (apply #'min collected-indentations)))
                           ;; There must be no line with indentation
                           ;; smaller than `indentation' (except for
                           ;; blank lines) between the found opening
                           ;; block and the current line, otherwise it
                           ;; is not an opening block.
                           (save-excursion
                             (mojo-nav-end-of-statement)
                             (forward-line)
                             (let ((no-back-indent t))
                               (save-match-data
                                 (while (and (< (point) cur-line)
                                             (setq no-back-indent
                                                   (or (> (current-indentation) indentation)
                                                       (mojo-info-current-line-empty-p)
                                                       (mojo-info-current-line-comment-p))))
                                   (forward-line)))
                               no-back-indent)))
                  (setq collected-indentations
                        (cons indentation collected-indentations))
                  (when (member (match-string-no-properties 0)
                                possible-opening-blocks)
                    (setq opening-blocks (cons (point) opening-blocks))))
                (when (zerop indentation)
                  (throw 'exit nil)))))
          ;; sort by closer
          (nreverse opening-blocks))))))

(defun mojo-info-dedenter-opening-block-message  ()
  "Message the first line of the block the current statement closes."
  (let ((point (mojo-info-dedenter-opening-block-position)))
    (when point
        (message "Closes %s" (save-excursion
                               (goto-char point)
                               (buffer-substring
                                (point) (line-end-position)))))))

(defun mojo-info-dedenter-statement-p ()
  "Return point if current statement is a dedenter.
Sets `match-data' to the keyword that starts the dedenter
statement."
  (save-excursion
    (mojo-nav-beginning-of-statement)
    (when (and (not (mojo-syntax-context-type))
               (looking-at (mojo-rx dedenter))
               ;; Exclude the first "case" in the block.
               (not (and (string= (match-string-no-properties 0)
                                  "case")
                         (save-excursion
                           (back-to-indentation)
                           (mojo-util-forward-comment -1)
                           (equal (char-before) ?:)))))
      (point))))

(defun mojo-info-line-ends-backslash-p (&optional line-number)
  "Return non-nil if current line ends with backslash.
With optional argument LINE-NUMBER, check that line instead."
  (save-excursion
      (when line-number
        (mojo-util-goto-line line-number))
      (while (and (not (eobp))
                  (goto-char (line-end-position))
                  (mojo-syntax-context 'paren)
                  (not (equal (char-before (point)) ?\\)))
        (forward-line 1))
      (when (equal (char-before) ?\\)
        (point-marker))))

(defun mojo-info-beginning-of-backslash (&optional line-number)
  "Return the point where the backslashed line starts.
Optional argument LINE-NUMBER forces the line number to check against."
  (save-excursion
      (when line-number
        (mojo-util-goto-line line-number))
      (when (mojo-info-line-ends-backslash-p)
        (while (save-excursion
                 (goto-char (line-beginning-position))
                 (mojo-syntax-context 'paren))
          (forward-line -1))
        (back-to-indentation)
        (point-marker))))

(defun mojo-info-continuation-line-p ()
  "Check if current line is continuation of another.
When current line is continuation of another return the point
where the continued line ends."
  (save-excursion
      (let* ((context-type (progn
                             (back-to-indentation)
                             (mojo-syntax-context-type)))
             (line-start (line-number-at-pos))
             (context-start (when context-type
                              (mojo-syntax-context context-type))))
        (cond ((equal context-type 'paren)
               ;; Lines inside a paren are always a continuation line
               ;; (except the first one).
               (mojo-util-forward-comment -1)
               (point-marker))
              ((member context-type '(string comment))
               ;; move forward an roll again
               (goto-char context-start)
               (mojo-util-forward-comment)
               (mojo-info-continuation-line-p))
              (t
               ;; Not within a paren, string or comment, the only way
               ;; we are dealing with a continuation line is that
               ;; previous line contains a backslash, and this can
               ;; only be the previous line from current
               (back-to-indentation)
               (mojo-util-forward-comment -1)
               (when (and (equal (1- line-start) (line-number-at-pos))
                          (mojo-info-line-ends-backslash-p))
                 (point-marker)))))))

(defun mojo-info-block-continuation-line-p ()
  "Return non-nil if current line is a continuation of a block."
  (save-excursion
    (when (mojo-info-continuation-line-p)
      (forward-line -1)
      (back-to-indentation)
      (when (looking-at (mojo-rx block-start))
        (point-marker)))))

(defun mojo-info-assignment-statement-p (&optional current-line-only)
  "Check if current line is an assignment.
With argument CURRENT-LINE-ONLY is non-nil, don't follow any
continuations, just check the if current line is an assignment."
  (save-excursion
    (let ((found nil))
      (if current-line-only
          (back-to-indentation)
        (mojo-nav-beginning-of-statement))
      (while (and
              (re-search-forward (mojo-rx not-simple-operator
                                            assignment-operator
                                            (group not-simple-operator))
                                 (line-end-position) t)
              (not found))
        (save-excursion
          ;; The assignment operator should not be inside a string.
          (backward-char (length (match-string-no-properties 1)))
          (setq found (not (mojo-syntax-context-type)))))
      (when found
        (skip-syntax-forward " ")
        (point-marker)))))

;; TODO: rename to clarify this is only for the first continuation
;; line or remove it and move its body to `mojo-indent-context'.
(defun mojo-info-assignment-continuation-line-p ()
  "Check if current line is the first continuation of an assignment.
When current line is continuation of another with an assignment
return the point of the first non-blank character after the
operator."
  (save-excursion
    (when (mojo-info-continuation-line-p)
      (forward-line -1)
      (mojo-info-assignment-statement-p t))))

(defvar mojo-nav-beginning-of-defun-regexp
  (mojo-rx line-start (* space) defun (+ sp-bsnl) (group symbol-name))
  "Regexp matching a struct, trait, or function definition.
The name of the defun is grouped so it can be retrieved via `match-string'.")

(defvar mojo-nav-beginning-of-block-regexp
  (mojo-rx line-start (* space) block-start)
  "Regexp matching the start of a block.")

(defun mojo-info-looking-at-beginning-of-defun (&optional syntax-ppss
                                                            check-statement)
  "Check if point is at `beginning-of-defun' using SYNTAX-PPSS.
When CHECK-STATEMENT is non-nil, the current statement is checked
instead of the current physical line."
  (save-excursion
    (when check-statement
      (mojo-nav-beginning-of-statement))
    (beginning-of-line 1)
    (and (not (mojo-syntax-context-type (or syntax-ppss (syntax-ppss))))
         (looking-at mojo-nav-beginning-of-defun-regexp))))

(defun mojo-info-looking-at-beginning-of-block ()
  "Check if point is at the beginning of block."
  (let ((pos (point)))
    (save-excursion
      (mojo-nav-beginning-of-statement)
      (beginning-of-line)
      (and
       (<= (point) pos (+ (point) (current-indentation)))
       (looking-at mojo-nav-beginning-of-block-regexp)))))

(defun mojo-info-current-line-comment-p ()
  "Return non-nil if current line is a comment line."
  (char-equal
   (or (char-after (+ (line-beginning-position) (current-indentation))) ?_)
   ?#))

(defun mojo-info-current-line-empty-p ()
  "Return non-nil if current line is empty, ignoring whitespace."
  (save-excursion
    (beginning-of-line 1)
    (looking-at
     (mojo-rx line-start (* whitespace)
                (group (* not-newline))
                (* whitespace) line-end))
    (string-equal "" (match-string-no-properties 1))))

(defun mojo-info-docstring-p (&optional syntax-ppss)
  "Return non-nil if point is in a docstring.
When optional argument SYNTAX-PPSS is given, use that instead of
point's current `syntax-ppss'."
  (save-excursion
    (when (and syntax-ppss (mojo-syntax-context 'string syntax-ppss))
      (goto-char (nth 8 syntax-ppss)))
    (mojo-nav-beginning-of-statement)
    (let ((counter 1)
          (indentation (current-indentation))
          (backward-sexp-point)
          (re "[uU]?[rR]?[\"']"))
      (when (and
             (not (mojo-info-assignment-statement-p))
             (looking-at-p re)
             ;; Allow up to two consecutive docstrings only.
             (>=
              2
              (let (last-backward-sexp-point)
                (while (and (<= counter 2)
                            (save-excursion
                              (mojo-nav-backward-sexp)
                              (setq backward-sexp-point (point))
                              (and (= indentation (current-indentation))
                                   ;; Make sure we're always moving point.
                                   ;; If we get stuck in the same position
                                   ;; on consecutive loop iterations,
                                   ;; bail out.
                                   (prog1 (not (eql last-backward-sexp-point
                                                    backward-sexp-point))
                                     (setq last-backward-sexp-point
                                           backward-sexp-point))
                                   (looking-at-p re))))
                  ;; Previous sexp was a string, restore point.
                  (goto-char backward-sexp-point)
                  (cl-incf counter))
                counter)))
        (mojo-util-forward-comment -1)
        (mojo-nav-beginning-of-statement)
        (cond ((and (bobp) (save-excursion
                             (mojo-util-forward-comment)
                             (looking-at-p re))))
              ((mojo-info-assignment-statement-p) t)
              ((mojo-info-looking-at-beginning-of-defun))
              (t nil))))))


(defun mojo-util-goto-line (line-number)
  "Move point to LINE-NUMBER."
  (goto-char (point-min))
  (forward-line (1- line-number)))

(defun mojo-util-forward-comment (&optional direction)
  "Mojo mode specific version of `forward-comment'.
Optional argument DIRECTION defines the direction to move to."
  (let ((comment-start (mojo-syntax-context 'comment))
        (factor (if (< (or direction 0) 0)
                    -99999
                  99999)))
    (when comment-start
      (goto-char comment-start))
    (forward-comment factor)))

(provide 'mojo-indent)

;;; mojo-indent.el ends here
