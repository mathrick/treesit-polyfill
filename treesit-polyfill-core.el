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
  "Hash table providing a mapping from buffers to their parsers.
See also `tsp--parser-buffer-map'.

This is a part of the treesit-polyfill API.")

;; Since parsers are longer-lived and less numerous than nodes, we can
;; probably afford to keep two mappings (buffers→parsers and parsers→buffers)
;; and keep them in sync. This allows us to skip wrapping parsers the
;; way we have to wrap nodes to keep track of their parsers
(defvar tsp--parser-buffer-map
  (make-hash-table)
  "Hash table providing a mapping from parsers to their buffers.
This table is not meant to be used directly and is NOT guaranteed
to be up-to-date unless `tsp--sync-parser-buffer-map' has been
called. Use low-level `tsp-*' or high-leve `treesit-*' functions
instead of poking this table directly.
See also `tsp--buffer-parser-map'.

This is a part of the treesit-polyfill API.")

(defun treesit-parser-list (&optional buffer)
  "Return BUFFER’s parser list.

BUFFER defaults to the current buffer.  If that buffer is an indirect
buffer, its base buffer is used instead.  That is, indirect buffers
use their base buffer’s parsers."
  (gethash (tsp--get-target-buffer buffer) tsp--buffer-parser-map))

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

(defun treesit-parser-create (language &optional buffer no-reuse)
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
         (language (tsp--lang-name language))
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
    parser))

(defun treesit-parser-delete (parser)
  "Delete PARSER from its buffer’s parser list.
See ‘treesit-parser-list’ for the buffer’s parser list."
  (let* ((buffer (treesit-parser-buffer parser))
         (parsers (gethash buffer tsp--buffer-parser-map)))
    (setf (gethash buffer tsp--buffer-parser-map)
          (remove parser parsers))
    ;; FIXME: this might be slow, will need to see in practice
    (tsp--sync-parser-buffer-map)))

(defun tsp--sync-parser-buffer-map ()
  "Reflect the contents of `tsp--buffer-parser-map' into `tsp--parser-buffer-map'."
  (cl-loop for buf being the hash-keys of tsp--buffer-parser-map
               using (hash-value parsers)
           ;; cl-loop generates buggy code if we do "for parser in
           ;; parsers" in the outer loop, have to use nested
           do (cl-loop for parser in parsers
                       do (setf (gethash parser tsp--parser-buffer-map) buf))))

(defun treesit-parser-language (parser)
  "Return PARSER’s language symbol.
This symbol is the one used to create the parser."
  (tsp--lang-name (tsc-parser-language parser)))

(defun treesit-parser-buffer (parser)
  "Return the buffer of PARSER."
  (unless (gethash parser tsp--parser-buffer-map)
    ;; Try syncing the hash tables to see if we can find the buffer
    (tsp--sync-parser-buffer-map))
  (or (gethash parser tsp--parser-buffer-map)
      (error "Parser %s has no known buffer association. It might have been created outside of treesit.el API"
             parser)))

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
