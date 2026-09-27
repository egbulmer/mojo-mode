;;; mojo-mode-test.el --- Tests for mojo-mode -*- lexical-binding: t; -*-

;;; Commentary:

;; Smoke tests for the initial scaffold.  They check that the mode
;; loads, associates the Mojo file extensions, and font-locks the
;; current keyword set.  They do not require a Mojo SDK.

;;; Code:

(require 'buttercup)
(require 'mojo-mode)

(defun mojo-mode-test--faces (source)
  "Font-lock SOURCE and return a list of (FACE . TEXT) spans."
  (with-temp-buffer
    (mojo-mode)
    (insert source)
    (font-lock-ensure)
    (let ((pos (point-min))
          (spans nil))
      (while (< pos (point-max))
        (let* ((next (or (next-single-property-change pos 'face) (point-max)))
               (face (get-text-property pos 'face))
               (text (buffer-substring-no-properties pos next)))
          (when face
            (push (cons face text) spans))
          (setq pos next)))
      (nreverse spans))))

(defun mojo-mode-test--faced (source face)
  "Return the concatenated text in SOURCE font-locked with FACE."
  (mapconcat #'cdr
             (seq-filter (lambda (span) (eq (car span) face))
                         (mojo-mode-test--faces source))
             ""))

(defun mojo-mode-test--indent (source)
  "Insert SOURCE, indent the whole buffer, and return its text."
  (with-temp-buffer
    (mojo-mode)
    (insert source)
    (indent-region (point-min) (point-max))
    (buffer-string)))

(describe "mojo-mode"
  (it "is derived from prog-mode"
    (expect (provided-mode-derived-p 'mojo-mode 'prog-mode)))

  (it "associates .mojo and the fire-emoji extension"
    (expect (cdr (assoc "\\.mojo\\'" auto-mode-alist)) :to-be 'mojo-mode)
    (expect (cdr (assoc "\\.🔥\\'" auto-mode-alist)) :to-be 'mojo-mode))

  (it "registers a shebang interpreter"
    (expect (cdr (assoc "mojo" interpreter-mode-alist)) :to-be 'mojo-mode))

  (it "uses spaces and a 4-column indent"
    (with-temp-buffer
      (mojo-mode)
      (expect indent-tabs-mode :to-be nil)
      (expect mojo-indent-offset :to-be 4)
      (expect indent-line-function :to-be #'mojo-indent-line-function)
      (expect comment-start :to-equal "# ")))

  (it "registers the Mojo language server with eglot"
    (expect (assoc 'mojo-mode eglot-server-programs) :to-be-truthy)))

(describe "indentation"
  (it "indents the body of a struct"
    (expect (mojo-mode-test--indent "struct Point:\nvar x: Int\n")
            :to-equal "struct Point:\n    var x: Int\n"))

  (it "indents the body of a function"
    (expect (mojo-mode-test--indent "def greet():\nprint(\"hi\")\n")
            :to-equal "def greet():\n    print(\"hi\")\n"))

  (it "dedents an else clause"
    (expect (mojo-mode-test--indent
             "def greet(ready: Bool):\nif ready:\nprint(\"hi\")\nelse:\nprint(\"bye\")\n")
            :to-equal
            "def greet(ready: Bool):\n    if ready:\n        print(\"hi\")\n    else:\n        print(\"bye\")\n"))

  (it "indents one level inside a split signature"
    (expect (mojo-mode-test--indent "def build(\nself,\nvalue: Int):\npass\n")
            :to-equal "def build(\n    self,\n    value: Int):\n    pass\n"))

  (it "reports noindent when the line is already indented"
    (with-temp-buffer
      (mojo-mode)
      (insert "def greet():\n    print(\"hi\")")
      (goto-char (point-max))
      (expect (mojo-indent-line-function) :to-be 'noindent)))

  (it "lets the first tab fall through to completion"
    (with-temp-buffer
      (mojo-mode)
      (setq-local tab-always-indent 'complete)
      (let (completed)
        (add-hook 'completion-at-point-functions
                  (lambda () (setq completed t) nil)
                  nil t)
        (insert "def greet():\n    print(\"hi\")")
        (goto-char (point-max))
        (indent-for-tab-command)
        (expect completed :to-be t))))

  (it "dedents one level on backspace"
    (with-temp-buffer
      (mojo-mode)
      (insert "def greet():\n    print(\"hi\")")
      (goto-char (point-max))
      (beginning-of-line)
      (skip-chars-forward " ")
      (mojo-indent-dedent-line-backspace 1)
      (expect (buffer-string) :to-equal "def greet():\nprint(\"hi\")"))))

(describe "font-lock"
  (it "highlights current declaration keywords"
    (let ((faced (mojo-mode-test--faced
                  "def greet():\n    pass\nstruct Point:\n    var x: Int\n"
                  'font-lock-keyword-face)))
      (expect faced :to-match "def")
      (expect faced :to-match "pass")
      (expect faced :to-match "struct")
      (expect faced :to-match "var")))

  (it "highlights struct and function names"
    (let ((faces (mojo-mode-test--faces "struct Point:\n    def length(self) -> Int:\n        return 0\n")))
      (expect (alist-get 'font-lock-type-face faces) :to-equal "Point")
      (expect (alist-get 'font-lock-function-name-face faces) :to-equal "length")))

  (it "does not treat removed spellings as keywords"
    (dolist (word '("fn" "let" "alias" "borrowed" "inout"))
      (let ((faced (mojo-mode-test--faced (format "%s name\n" word)
                                          'font-lock-keyword-face)))
        (expect faced :not :to-match (regexp-quote word)))))

  (it "highlights argument conventions only in a signature"
    (let ((signature (mojo-mode-test--faced
                      "def increment(mut self):\n    pass\n"
                      'font-lock-keyword-face))
          (local (mojo-mode-test--faced
                  "def f():\n    var mut = 1\n"
                  'font-lock-keyword-face)))
      (expect signature :to-match "mut")
      (expect local :not :to-match "mut")))

  (it "highlights literals and Self"
    (let ((faced (mojo-mode-test--faced
                  "var ready: Bool = True\ncomptime T = Self\n"
                  'font-lock-constant-face)))
      (expect faced :to-match "True")
      (expect faced :to-match "Self"))))

(provide 'mojo-mode-test)

;;; mojo-mode-test.el ends here
