;;; treesit-polyfill-core.el --- treesit.el implementation on top of tree-sitter.el -*- lexical-binding: t -*-

;; Copyright (C) 2024 Maciej Katafiasz <mathrick@disroot.org>
;; Parts of this file duplicate portions of treesit.el for the purpose
;; of providing API compatibility

;; Maintainer: Maciej Katafiasz <mathrick@disroot.org>
;; Keywords: treesit, tree-sitter, languages
;; Package: treesit-polyfill

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;; This package provides a polyfill implementing the Emacs 29+
;; built-in treesit.el package on top of the tree-sitter.el package,
;; thus providing compatibility with older versions of Emacs

;; This file contains the core API implementations that interact
;; directly with the underlying tree-sitter.el API

;; Currently tracking git tag emacs-29.2 (ef01b634d219bcceda17dcd61024c7a12173b88c)

;;; Code:

(require 'tree-sitter)

(defalias 'treesit-parser-p 'tsc-parser-p)
(defalias 'treesit-node-p 'tsc-node-p)
(defalias 'treesit-compiled-query-p 'tsc-query-p)
(defalias 'treesit-query-p 'tsc-query-p)

(defvar tsp--buffer-parser-map
  (make-hash-table)
  "Hash table providing a mapping from buffers to their parsers.")

(defun treesit-parser-list (&optional buffer)
  "Return BUFFER’s parser list.

BUFFER defaults to the current buffer.  If that buffer is an indirect
buffer, its base buffer is used instead.  That is, indirect buffers
use their base buffer’s parsers."
  (gethash (tsp--get-target-buffer buffer) tsp--buffer-parser-map))

(defun tsp--wrap-parser (parser buffer)
  "Return (cons PARSER BUFFER). This is needed because tree-sit.el
does not provide API to retrieve a parser's buffer, but treesit.el
does."
  (cons parser buffer))

(cl-defmacro tsp--unwrap-parser ((parser-var buffer-var) parser &body body)
  "Helper to unwrap the cons of PARSER BUFFER."
  (declare (indent defun))
  `(pcase ,parser
     ((and `(,,parser-var . ,,buffer-var)
           (guard (tsc-parser-p ,parser-var)))
      ,@body)
     ((pred tsc-parser-p)
      (let ((,parser-var ,parser)
            (,buffer-var nil))
         ,@body))
     (_
      (error "%s is not a valid treesit-polyfill parser, should be (cons PARSER BUFFER)" ,parser))))

(defun tsp--get-target-buffer (buffer)
  "Get the target buffer for treesit operations. This means if
  BUFFER is nil, use `current-buffer'. For indirect buffers, look
  up their base buffer and return that instead."
  (let ((buffer (or buffer (current-buffer))))
    (or (buffer-base-buffer buffer) buffer)))

(defun tsp--lang (lang)
  "Given LANG, return a value that satisfies
  `tsc-language-p'. This might load a parser for LANG if it
  hasn't been loaded before."
  (pcase lang
    ((pred tsc-language-p) lang)
    ((pred symbolp) (tree-sitter-require lang))
    (_ (error "%s is not a valid tree-sitter language" lang))))

(defun tsp--lang-name (lang)
  "Given LANG, return a value that satisfies `symbolp' and names the language."
  (pcase lang
    ((pred tsc-language-p) (tsc--lang-symbol lang))
    ((pred symbolp) lang)
    (_
     (error "%s is not a valid tree-sitter language" lang))))

(cl-defun treesit-parser-create (language &optional buffer no-reuse)
  "Create and return a parser in BUFFER for LANGUAGE.

The parser is automatically added to BUFFER’s parser list, as returned
by ‘treesit-parser-list’.  LANGUAGE is a language symbol.  If BUFFER
is nil or omitted, it defaults to the current buffer.  If BUFFER
already has a parser for LANGUAGE, return that parser, but if NO-REUSE
is non-nil, always create a new parser.

If that buffer is an indirect buffer, its base buffer is used instead.
That is, indirect buffers use their base buffer’s parsers.  Lisp
programs should widen as necessary should they want to use a parser in
an indirect buffer."
  (let* ((buffer (tsp--get-target-buffer buffer))
         (language (tsp--lang language))
         (parsers (treesit-parser-list buffer))
         (existing (and (not no-reuse)
                        (first (seq-filter (lambda (parser)
                                             (eq (treesit-parser-language parser) language))
                                           parsers))))
         (parser (or existing (tsc-make-parser))))
    (unless existing
      (tsc-set-language parser language)
      (setf (gethash buffer tsp--buffer-parser-map)
            (append parsers (list parser))))
    (tsp--wrap-parser parser buffer)))

(defun treesit-parser-language (parser)
  "Return PARSER’s language symbol.
This symbol is the one used to create the parser."
  (tsp--unwrap-parser (parser buffer) parser
    (tsp--lang-name (tsc-parser-language parser))))

(defun treesit-parser-buffer (parser)
  "Return the buffer of PARSER."
  (tsp--unwrap-parser (parser buffer) buffer
    buffer))

(defun tsp--wrap-node (node parser)
  "Return (cons NODE PARSER). This is needed because tree-sit.el
does not provide API to retrieve a node's parser, but treesit.el
does."
  (cons node parser))

(cl-defmacro tsp--unwrap-node ((node-var parser-var) node &body body)
  "Helper to unwrap the cons of NODE PARSER."
  `(pcase ,node
     ((and `(,,node-var . ,,parser-var)
           (guard (tsc-node-p ,node-var)))
      ,@body)
     ((pred tsc-node-p)
      (let ((,node-var ,node)
            (,parser-var nil))
         ,@body))
     (_
      (error "%s is not a valid treesit-polyfill node, should be (cons NODE PARSER)" ,node))))

(defun treesit-parse-string (string language)
  "Parse STRING using a parser for LANGUAGE.
Return the root node of the syntax tree."
  (let ((parser (tsc-make-parser)))
    (tsc-set-language parser (tree-sitter-require language))
    (tsp--wrap-node (tsc-root-node (tsc-parse-string parser string))
                    parser)))

(provide 'treesit-polyfill-core)

;;; treesit-polyfill-core.el ends here

;; Local Variables:
;; nameless-current-name: "treesit"
;; End:
