;;; treesit-polyfill.el --- treesit.el implementation on top of tree-sitter.el -*- lexical-binding: t -*-

;; Copyright (C) 2024 Maciej Katafiasz <mathrick@disroot.org>
;; Parts of this file duplicate portions of treesit.el for the purpose
;; of providing API compatibility

;; Maintainer: Maciej Katafiasz <mathrick@disroot.org>
;; Keywords: treesit, tree-sitter, languages
;; Package: treesit-polyfill
;; Version: 0.0.1
;; Package-Requires: (dash)

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;; This package provides a polyfill implementing the Emacs 29+
;; built-in treesit.el package on top of the tree-sitter.el package,
;; thus providing compatibility with older versions of Emacs

;; This file contains the high-level API implemented in terms of core
;; APIs, for the purpose of completeness. These definitions are copied
;; directly from treesit.el and don't interact with the low-level
;; tree-sitter.el primitives.

;; Currently tracking git tag emacs-29.2 (ef01b634d219bcceda17dcd61024c7a12173b88c)

;;; Code:

(require 'treesit-polyfill-core)
(require 'treesit-polyfill-parser)
(require 'treesit-polyfill-node)
(require 'treesit-polyfill-query)

(defun treesit-available-p ()
  "Return non-nil if tree-sitter support is built-in and available."
  'polyfill)

(defun treesit-node-buffer (node)
  "Return the buffer in which NODE belongs."
  (treesit-parser-buffer
   (treesit-node-parser node)))

(defun treesit-node-language (node)
  "Return the language symbol that NODE's parser uses."
  (treesit-parser-language
   (treesit-node-parser node)))

(defun treesit-node-text (node &optional no-property)
  "Return the buffer (or string) content corresponding to NODE.
If optional argument NO-PROPERTY is non-nil, remove text
properties."
  (when node
    (with-current-buffer (treesit-node-buffer node)
      (if no-property
          (buffer-substring-no-properties
           (treesit-node-start node)
           (treesit-node-end node))
        (buffer-substring
         (treesit-node-start node)
         (treesit-node-end node))))))

(defvar-local treesit-language-at-point-function nil
  "A function that returns the language at point.
This is used by `treesit-language-at', which is used by various
functions to determine which parser to use at point.

The function is called with one argument, the position of point.

In general, this function should call `treesit-node-at' with an
explicit language (usually the host language), and determine the
language at point using the type of the returned node.

DO NOT derive the language at point from parser ranges.  It's
cumbersome and can't deal with some edge cases.")

(defun treesit-language-at (position)
  "Return the language at POSITION.
This function assumes that parser ranges are up-to-date.  It
returns the return value of `treesit-language-at-point-function'
if it's non-nil, otherwise it returns the language of the first
parser in `treesit-parser-list', or nil if there is no parser."
  (if treesit-language-at-point-function
      (funcall treesit-language-at-point-function position)
    (when-let ((parser (car (treesit-parser-list))))
      (treesit-parser-language parser))))

(provide 'treesit-polyfill)

;;; treesit-polyfill.el ends here

;; Local Variables:
;; nameless-current-name: "treesit"
;; End:
