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

(eval-when-compile
  (require 'cl-lib))

(require 'tree-sitter)

;; FIXME: review which ones of these we can even reasonably signal, and see what maps to
;; tsc errors
(define-error 'treesit-error "Generic tree-sitter error")
(define-error 'treesit-query-error "Query pattern is malformed" 'treesit-error)
(define-error 'treesit-parse-error "Parse failed" 'treesit-error)
(define-error 'treesit-range-invalid
  "RANGES are invalid: they have to be ordered and should not overlap"
  'treesit-error)
(define-error 'treesit-unparsed-edits "There are unparsed edits in the buffer" 'treesit-error)
(define-error 'treesit-buffer-too-large "Buffer too large (> 4GiB)" 'treesit-error)
(define-error 'treesit-load-language-error "Cannot load language definition" 'treesit-error)
(define-error 'treesit-node-outdated "This node is outdated, please retrieve a new one" 'treesit-error)
(define-error 'treesit-parser-deleted "This parser is deleted and cannot be used" 'treesit-error)
(define-error 'treesit-invalid-predicate
  "Invalid predicate, see `treesit-thing-settings' for valid forms for a predicate"
  'treesit-error)

(defun tsp--sym-name (symbol)
  "Like `symbol-name', but skips initial : in keywords"
  (let ((name (symbol-name symbol)))
    (if (keywordp symbol)
        (substring name 1)
      name)))

(cl-defmacro tsp--unwrapping-defun (type name (vars-and-options &rest args)
                                         &body body)
  "Helper macro around `defun' which automatically unwraps a
passed-in TYPE (e.g. node or query) object.  VARS-AND-OPTIONS can
either be a list of the form \((VAR &rest OTHER-VARS) &OPTIONAL
PARSER-VAR &KEY (ALLOW-NIL T) REWRAP) or a single name, which is
equivalent to ((VAR)).

BODY will be automatically wrapped in `tsp--unwrap-TYPE'.

VAR and OTHER-VARS name symbols which will be bound to the
unwrapped value of object and it and parser respectively inside
BODY. OTHER-VARS can be omitted or '_, in which case they will
not be bound. VAR will be included as the first argument of the
generated `defun'.

ALLOW-NIL controls whether the object passed in is allowed to be
nil (t by default). If so, the whole function will return nil in
that case.

REWRAP means the return value will be automatically wrapped via
`tsp--wrap-TYPE', using the same auxiliary values as were passed in."
  (declare (indent defun))
  (cl-destructuring-bind (vars &key (allow-nil t) rewrap)
      (if (listp vars-and-options)
          vars-and-options
        (list vars-and-options))
    (let* ((type (tsp--sym-name type))
           (vars (if (listp vars) vars (list vars)))
           (var (car vars))
           (other-vars (mapcar (lambda (v)
                                 (if (or (not v)
                                         (eq v '_))
                                     ;; Can't use or nil _ because we expand to `pcase', and _ is special there
                                     (gensym (format "%s-var" type))
                                   v))
                               (cdr vars)))
           (unwrapper (intern (format "tsp--unwrap-%s" type)))
           (wrapper (intern (format "tsp--wrap-%s" type)))
           (args (cons var args))
           (doc (when (and (stringp (first body))
                           (rest body))
                  (first body)))
           (body (if doc (rest body)
                   body))
           (body (if rewrap
                     `((,wrapper (progn ,@body) ,@other-vars))
                   body)))
      `(cl-defun ,name (,@args)
         ,doc
         (if ,var
             (,unwrapper (,var ,@other-vars) ,var
               ,@body)
           ,(unless allow-nil
              `(error "%s cannot be nil" ,(upcase (symbol-name var)))))))))

(defun treesit-parse-string (string language)
  "Parse STRING using a parser for LANGUAGE.
Return the root node of the syntax tree."
  ;; Note: since we do this in a temp buffer, we no longer have the
  ;; text accessible if the user asks a query using `:pred', which
  ;; means this will error out. However, Emacs 29 simply crashes in
  ;; this situation, so we're still in a better shape, and also don't
  ;; have to worry about any code out in the wild expecting that to
  ;; work
  (with-temp-buffer
    (let ((parser (treesit-parser-create language)))
      (insert string)
      (treesit-parser-root-node parser)))

  (defun treesit-search-subtree (node predicate &optional backward all depth)
    "Traverse the parse tree of NODE depth-first using PREDICATE.

Traverse the subtree of NODE, and match PREDICATE with each node along
the way.  PREDICATE is a regexp string that matches against each
node’s type, or a function that takes a node and returns nil/non-nil.

By default, only traverse named nodes, but if ALL is non-nil, traverse
all nodes.  If BACKWARD is non-nil, traverse backwards.  If DEPTH is
non-nil, only traverse nodes up to that number of levels down in the
tree.  If DEPTH is nil, default to 1000.

Return the first matched node, or nil if none matches."
    (error "FIXME: Not yet implemented")))

(defun treesit-search-forward (start predicate &optional backward all)
  "Search for node matching PREDICATE in the parse tree of START.

Start traversing the tree from node START, and match PREDICATE with
each node (except START itself) along the way.  PREDICATE is a regexp
string that matches against each node’s type, or a function that takes
a node and returns non-nil if it matches.

By default, only search for named nodes, but if ALL is non-nil, search
for all nodes.  If BACKWARD is non-nil, search backwards.

Return the first matched node, or nil if none matches.

For a tree like below, where START is marked by S, traverse as
numbered from 1 to 12:

                12
                |
       S--------3----------11
       |        |          |
  o--o-+--o  1--+--2    6--+-----10
  |  |                  |        |
  o  o                +-+-+   +--+--+
                      |   |   |  |  |
                      4   5   7  8  9

Note that this function doesn’t traverse the subtree of START, and it
always traverse leaf nodes first, then upwards."
  (error "FIXME: Not yet implemented"))

(defun treesit-induce-sparse-tree (root predicate &optional process-fn depth)
  "Create a sparse tree of ROOT’s subtree.

This takes the subtree under ROOT, and combs it so only the nodes
that match PREDICATE are left, like picking out grapes on the vine.
PREDICATE is a regexp string that matches against each node’s type.

For a subtree on the left that consist of both numbers and letters, if
PREDICATE is \"is letter\", the returned tree is the one on the right.

	a                 a              a
	|                 |              |
    +---+---+         +---+---+      +---+---+
    |   |   |         |   |   |      |   |   |
    b   1   2         b   |   |      b   c   d
	|   |     =>      |   |  =>      |
	c   +--+          c   +          e
	|   |  |          |   |
     +--+   d  4       +--+   d
     |  |              |
     e  5              e

If PROCESS-FN is non-nil, it should be a function of one argument.  In
that case, instead of returning the matched nodes, pass each node to
PROCESS-FN, and use its return value instead.

If non-nil, DEPTH is the number of levels to go down the tree from
ROOT.  If DEPTH is nil or omitted, it defaults to 1000.

Each node in the returned tree looks like (NODE . (CHILD ...)).  The
root of this tree might be nil, if ROOT doesn’t match PREDICATE.

If no node matches PREDICATE, return nil.

PREDICATE can also be a function that takes a node and returns
nil/non-nil, but it is slower and more memory consuming than using
a regexp."
  (error "FIXME: Not yet implemented"))

(defun treesit-subtree-stat (node)
  "Return information about the subtree of NODE.

Return a list (MAX-DEPTH MAX-WIDTH COUNT), where MAX-DEPTH is the
maximum depth of the subtree, MAX-WIDTH is the maximum number of
direct children of nodes in the subtree, and COUNT is the number of
nodes in the subtree, including NODE."
  (error "FIXME: Not yet implemented"))


(provide 'treesit-polyfill-core)

;;; treesit-polyfill-core.el ends here

;; Local Variables:
;; nameless-current-name: "treesit"
;; End:
