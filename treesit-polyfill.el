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


(defun treesit--parse-string-to-tree (string language)
  "Like `treesit-parse-string', but return a tree, rather than its root node.
Internal implementation detail."
  (let ((parser (tsc-make-parser)))
    (tsc-set-language parser (tree-sitter-require language))
    (tsc-parse-string parser string)))

(defun treesit-parse-string (string language)
  "Parse STRING using a parser for LANGUAGE.
Return the root node of the syntax tree."
  (tsc-root-node
   (treesit--parse-string-to-tree string language)))

(provide 'treesit-polyfill)

;;; treesit-polyfill.el ends here
