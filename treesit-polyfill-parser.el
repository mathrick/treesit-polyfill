;;; treesit-polyfill-parser.el --- treesit.el implementation on top of tree-sitter.el -*- lexical-binding: t -*-

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

;; This file contains the parser-related parts of the core API
;; implemented directly through the underlying tree-sitter.el functions

;; Currently tracking git tag emacs-29.2 (ef01b634d219bcceda17dcd61024c7a12173b88c)

;;; Code:

(eval-when-compile
  (require 'treesit-polyfill-core))

(defalias 'treesit-parser-p 'tsc-parser-p)

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

(defun tsp--get-target-buffer (buffer)
  "Get the target buffer for treesit operations. This means if
  BUFFER is nil, use `current-buffer'. For indirect buffers, look
  up their base buffer and return that instead."
  (let ((buffer (or buffer (current-buffer))))
    (or (buffer-base-buffer buffer) buffer)))

(defun tsp--sync-parser-buffer-map ()
  "Reflect the contents of `tsp--buffer-parser-map' into `tsp--parser-buffer-map'."
  (cl-loop for buf being the hash-keys of tsp--buffer-parser-map
               using (hash-value parsers)
           ;; cl-loop generates buggy code if we do "for parser in
           ;; parsers" in the outer loop, have to use nested
           do (cl-loop for parser in parsers
                       do (setf (gethash parser tsp--parser-buffer-map) buf))))

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
         (language (tree-sitter-require (tsp--lang-name language)))
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

(defun treesit-parser-list (&optional buffer)
  "Return BUFFER’s parser list.

BUFFER defaults to the current buffer.  If that buffer is an indirect
buffer, its base buffer is used instead.  That is, indirect buffers
use their base buffer’s parsers."
  (gethash (tsp--get-target-buffer buffer) tsp--buffer-parser-map))

(defun treesit-parser-buffer (parser)
  "Return the buffer of PARSER."
  (unless (gethash parser tsp--parser-buffer-map)
    ;; Try syncing the hash tables to see if we can find the buffer
    (tsp--sync-parser-buffer-map))
  (or (gethash parser tsp--parser-buffer-map)
      (error "Parser %s has no known buffer association. It might have been created outside of treesit.el API"
             parser)))

(defun treesit-parser-language (parser)
  "Return PARSER’s language symbol.
This symbol is the one used to create the parser."
  (tsp--lang-name (tsc-parser-language parser)))

(defun treesit-parser-root-node (parser)
  "Return the root node of PARSER."
  (tsp--wrap-node (tsc-root-node parser) parser))

(defun treesit-parser-set-included-ranges (parser ranges)
  "Limit PARSER to RANGES.

RANGES is a list of (BEG . END), each (BEG . END) defines a region in
which the parser should operate.  Regions must not overlap, and the
regions should come in order in the list.  Signal
‘treesit-set-range-error’ if the argument is invalid, or something
else went wrong.  If RANGES is nil, the PARSER is to parse the whole
buffer."
  (error "FIXME: Not yet implemented"))

(defun treesit-parser-included-ranges (parser)
  "Return the ranges set for PARSER.
If no ranges are set for PARSER, return nil.
See also ‘treesit-parser-set-included-ranges’."
  (error "FIXME: Not yet implemented"))

(defun treesit-parser-add-notifier (parser function)
  "FUNCTION must be a function symbol, rather than a lambda form.
FUNCTION should take 2 arguments, RANGES and PARSER.  RANGES is a list
of cons cells of the form (START . END), where START and END are buffer
positions.  PARSER is the parser issuing the notification."
  (error "FIXME: Not yet implemented"))

(provide 'treesit-polyfill-parser)

;;; treesit-polyfill-parser.el ends here

;; Local Variables:
;; nameless-current-name: "treesit"
;; End:
