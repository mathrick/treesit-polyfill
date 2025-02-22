;;; treesit-polyfill-node.el --- treesit.el implementation on top of tree-sitter.el -*- lexical-binding: t -*-

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

;; This file contains the node-related parts of the core API
;; implemented directly through the underlying tree-sitter.el functions

;; Currently tracking git tag emacs-29.2 (ef01b634d219bcceda17dcd61024c7a12173b88c)

;;; Code:

(eval-when-compile
  (require 'treesit-polyfill-core))

(defun tsp--wrap-node (node parser)
  "Return (cons NODE PARSER). This is needed because tree-sit.el
does not provide API to retrieve a node's parser, but treesit.el
does."
  (cons node parser))

(cl-defmacro tsp--unwrap-node ((node-var parser-var) node &body body)
  "Helper to unwrap the cons of (NODE . PARSER) inside BODY.
The unwrapped values will be bound to NODE-VAR and PARSER-VAR.

NODE must satisfy `tsc-node-p'. It is also permissible to pass in
a naked NODE (ie. not a cons), in which case the PARSER-VAR will
be bound to nil."
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
  "Unwrapping defun variant for nodes, See `tsp--unwrapping-defun' for details"
  (declare (indent defun))
  (cl-destructuring-bind (node-var &optional parser-var &key (allow-nil t) rewrap)
      (if (listp node-and-parser)
          node-and-parser
        (list node-and-parser))
    (let ((vars-and-options `((,node-var ,parser-var)
                              :allow-nil ,allow-nil :rewrap ,rewrap)))
      `(tsp--unwrapping-defun :node ,name (,vars-and-options ,@args)
        ,@body))))

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
     ,@(when doc (list doc))
     (,definition node ,@args)))

(tsp--node-alias treesit-node-p () tsc-node-p)

(tsp--node-defun treesit-node-parser ((node parser))
  "Return the parser to which NODE belongs.

NOTE: This is a polyfill which depends on a wrapper around
`tree-sitter' nodes. A bare node object will be accepted, but the
result of this function will be `nil'."
  parser)

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
      (tsc-get-named-descendant-for-position-range node beg end)
    (tsc-get-descendant-for-position-range node beg end)))

(tsp--node-alias treesit-node-eq () tsc-node-eq
  "Return non-nil if NODE1 and NODE2 refer to the same node.
If any one of NODE1 and NODE2 is nil, return nil.
This function uses the same equivalence metric as ‘equal’, and returns
non-nil if NODE1 and NODE2 refer to the same node in a syntax tree
produced by tree-sitter.")

(provide 'treesit-polyfill-node)

;;; treesit-polyfill-node.el ends here

;; Local Variables:
;; nameless-current-name: "treesit"
;; End:
