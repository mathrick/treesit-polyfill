;;; treesit-polyfill.el --- treesit.el implementation on top of tree-sitter.el -*- lexical-binding: t -*-

;; Copyright (C) 2024 Maciej Katafiasz <mathrick@disroot.org>
;; Parts of this file duplicate portions of treesit.el for the purpose
;; of providing API compatibility

;; Maintainer: Maciej Katafiasz <mathrick@disroot.org>
;; Keywords: treesit, tree-sitter, languages
;; Package: treesit-polyfill

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;; This file provides a polyfill implementing the Emacs 29+ built-in
;; treesit.el package on top of the tree-sitter.el package, thus
;; providing compatibility with older versions of Emacs

;; Currently tracking git tag emacs-29.2 (ef01b634d219bcceda17dcd61024c7a12173b88c)

;;; Code:

(defalias 'treesit-parser-p 'tsc-parser-p)
(defalias 'treesit-node-p 'tsc-node-p)
(defalias 'treesit-compiled-query-p 'tsc-query-p)
(defalias 'treesit-query-p 'tsc-query-p)

(defun tsp--wrap-node (node parser)
  "Return (cons NODE PARSER). This is needed because tree-sit.el
does not provide API to retrieve a node's parser, but treesit.el
does"
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
      (error "%s is not a valid treesit-polyfill node, should be (cons TSC-NODE TSC-PARSER)" ,node))))

(cl-defmacro tsp--node-defun (name (node-arg parser-arg &rest args) &body body)
  "Like `defun', but wraps body in `tsp--unwrap-node'."
  (declare (indent defun))
  `(defun ,name (,node-arg ,@args)
     (tsp--unwrap-node (,node-arg ,parser-arg) ,node-arg
      ,@body)))

(tsp--node-defun treesit-node-language (node parser)
  (tsc--lang-symbol (tsc-parser-language parser)))

(defun treesit-parse-string (string language)
  "Parse STRING using a parser for LANGUAGE.
Return the root node of the syntax tree."
  (let ((parser (tsc-make-parser)))
    (tsc-set-language parser (tree-sitter-require language))
    (tsp--wrap-node (tsc-root-node (tsc-parse-string parser string))
                    parser)))

(provide 'treesit-polyfill)

;;; treesit-polyfill.el ends here

;; Local Variables:
;; nameless-current-name: "treesit"
;; End:
