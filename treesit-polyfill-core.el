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

(defun tsp--sym-name (symbol)
  "Like `symbol-name', but skips initial : in keywords"
  (let ((name (symbol-name symbol)))
    (if (keywordp symbol)
        (substring name 1)
      name)))

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

(cl-defmacro tsp--node-defun (name (node-and-parser &rest args)
                                   &body body)
  "Helper macro which automatically unwraps argument called NODE-VAR"
  (declare (indent defun))
  (destructuring-bind (node-var &optional parser-var
                                &key (allow-nil t) rewrap)
      (if (listp node-and-parser)
          node-and-parser
        (list node-and-parser))
    (let* ((parser-var (if (or (not parser-var)
                               (eq parser-var '_))
                           ;; Can't use _ because we expand to `pcase', and _ is special there
                           (gensym "parser")
                         parser-var))
           (args (cons node-var args))
           (doc (when (and (stringp (first body))
                           (rest body))
                  (first body)))
           (body (if doc (rest body)
                   body))
           (body (if rewrap
                     `((tsp--wrap-node (progn ,@body) ,parser-var))
                   body)))
      `(cl-defun ,name (,@args)
         ,doc
         (if ,node-var
             (tsp--unwrap-node (,node-var ,parser-var) ,node-var
               ,@body)
           ,(unless allow-nil
              '(error "NODE cannot be nil")))))))

(defmacro tsp--node-alias (symbol args definition &optional doc)
  "Helper for defining translations from `treesit-node-*' to `tsc-*' functions.
Useful when there is a 1:1 correspondence. We can't just use
`defalias' because of the wrapping business we use to keep track
of nodes' parsers.

SYMBOL is the `treesit-node-*' function to implement, DEFINITION
is the target `tsc-*' function to use. ARGS is an option list of
extra arguments (other than NODE, which is implicit) to take and
pass to the target. DOC is the docstring."
  `(tsp--node-defun ,symbol (node ,@args)
     ,(when doc (list doc))
     (,definition node ,@args)))

;; These correspond to the built-in C functions of treesit.el, and are declared in the same order
(tsp--node-defun treesit-node-type (node)
  "Return the NODE’s type as a string.
If NODE is nil, return nil."
  (symbol-name (tsc-node-type node)))

(tsp--node-alias treesit-node-start () tsc-node-start-position
  "Return the NODE’s start position in its buffer.
If NODE is nil, return nil.")

(tsp--node-alias treesit-node-end () tsc-node-end-position
  "Return the NODE’s end position in its buffer.
If NODE is nil, return nil.")

(tsp--node-alias treesit-node-string () tsc-node-to-sexp
  "Return the string representation of NODE.
If NODE is nil, return nil.")

(tsp--node-alias treesit-node-parent () tsc-get-parent
  "Return the immediate parent of NODE.
Return nil if NODE has no parent.  If NODE is nil, return nil.")

(tsp--node-defun treesit-node-child ((node _ :rewrap t) n &optional named)
  "Return the Nth child of NODE.

Return nil if there is no Nth child.  If NAMED is non-nil, look for
named child only.  NAMED defaults to nil.  If NODE is nil, return
nil.

N could be negative, e.g., -1 represents the last child."
  (let* ((n (if (< n 0)
                ;; Count from the end if N is negative
                (+ (treesit-node-child-count node named) n)
              n))
         (func (if named #'tsc-get-nth-named-child #'tsc-get-nth-child)))
    (funcall func node n)))

(tsp--node-defun treesit-node-check ((node parser) property)
  "Return non-nil if NODE has PROPERTY, nil otherwise.

PROPERTY could be ‘named’, ‘missing’, ‘extra’, ‘outdated’,
‘has-error’, or ‘live’.

Named nodes correspond to named rules in the language definition,
whereas \"anonymous\" nodes correspond to string literals in the
language definition.

Missing nodes are inserted by the parser in order to recover from
certain kinds of syntax errors, i.e., should be there but not there.

Extra nodes represent things like comments, which are not required the
language definition, but can appear anywhere.

A node is \"outdated\" if the parser has reparsed at least once after
the node was created.

A node \"has error\" if itself is a syntax error or contains any syntax
errors.

A node is \"live\" if its parser is not deleted and its buffer is
live."
  (pcase property
    ('named (tsc-node-named-p node))
    ('missing (tsc-node-missing-p node))
    ('extra (tsc-node-extra-p node))
    ('has-error (tsc-node-has-error-p node))
    ;; `nil' is a valid input to `buffer-live-p', so this is safe to do
    ('live (buffer-live-p (treesit-parser-buffer parser)))
    ('outdated (error "`outdated' property is not currently implemented"))))

(tsp--node-defun treesit-node-field-name-for-child (node n)
  "Return the field name of the Nth child of NODE.

Return nil if there’s no Nth child, or if it has no field.
If NODE is nil, return nil.

N counts all children, i.e., named ones and anonymous ones.

N could be negative, e.g., -1 represents the last child."
  (let* ((n (if (< n 0)
                (+ (tsc-count-children node) n)
              n))
         (cursor (tsc-make-cursor node)))
    (tsc-goto-first-child cursor)
    (cl-loop for i from 1 to n do (tsc-goto-next-sibling cursor))
    (tsp--sym-name (tsc-current-field cursor))))

(tsp--node-defun treesit-node-child-count (node &optional named)
  "Return the number of children of NODE.

If NAMED is non-nil, count named children only.  NAMED defaults to
nil.  If NODE is nil, return nil."
  (if named
      (tsc-count-named-children node)
    (tsc-count-children node)))

(tsp--node-defun treesit-node-child-by-field-name ((node _ :rewrap t) field-name)
  "Return the child of NODE with FIELD-NAME.
Return nil if there is no such child.  If NODE is nil, return nil."
  (tsc--get-child-by-field-name node field-name))

(tsp--node-defun treesit-node-next-sibling (node &optional named)
  "Return the next sibling of NODE.

Return nil if there is no next sibling.  If NAMED is non-nil, look for named
siblings only. NAMED defaults to nil.  If NODE is nil, return nil."
  (if named
      (tsc-get-next-named-sibling node)
    (tsc-get-next-sibling node)))

(tsp--node-defun treesit-node-prev-sibling (node &optional named)
  "Return the previous sibling of NODE.

Return nil if there is no previous sibling.  If NAMED is non-nil, look
for named siblings only.  NAMED defaults to nil.  If NODE is nil,
return nil."
  (if named
      (tsc-get-prev-named-sibling node)
    (tsc-get-prev-sibling node)))

;; FIXME: Currently this disagrees with treesit.el on the last position.
;; Ie. if parssing a python string with 68 positions, our implementation
;; returns a block spanning (1 . 68) for pos = 68, but treesit.el
;; returns nil. I don't know who's right.
(tsp--node-defun treesit-node-first-child-for-pos ((node _ :rewrap t) pos &optional named)
  "Return the first child of NODE for buffer position POS.

Specifically, return the first child that extends beyond POS.
Return nil if there is no such child.
If NAMED is non-nil, look for named children only.  NAMED defaults to nil.
Note that this function returns an immediate child, not the smallest
(grand)child.  If NODE is nil, return nil."
  (let* ((cursor (tsc-make-cursor node))
         (candidate (progn
                      (tsc-goto-first-child-for-position cursor pos)
                      (tsc-current-node cursor))))
    (if (or (not named)
            (tsc-node-named-p candidate))
        candidate
      (tsc-get-next-named-sibling candidate))))

(tsp--node-defun treesit-node-descendant-for-range (node beg end &optional named)
  "Return the smallest node that covers buffer positions BEG to END.

The returned node is a descendant of NODE.
Return nil if there is no such node.
If NAMED is non-nil, look for named child only.  NAMED defaults to nil.
If NODE is nil, return nil."
  (if named
      (tsc-get-named-descendant-for-position-range node)
    (tsc-get-descendant-for-position-range node)))

(tsp--node-alias treesit-node-eq () tsc-node-eq
  "Return non-nil if NODE1 and NODE2 refer to the same node.
If any one of NODE1 and NODE2 is nil, return nil.
This function uses the same equivalence metric as ‘equal’, and returns
non-nil if NODE1 and NODE2 refer to the same node in a syntax tree
produced by tree-sitter.")

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
