;;; treesit-polyfill-util.el --- treesit.el implementation on top of tree-sitter.el -*- lexical-binding: t -*-

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

;; This file contains utility code

;; Currently tracking git tag emacs-29.2 (ef01b634d219bcceda17dcd61024c7a12173b88c)

;;; Code:


;; We need to be able to parse string form of TS queries back into sexps, because we need
;; to rewrite them in order to implement treesit.el's `:pred' functionality. `read' can't
;; be used because it trips up on the #predicate syntax, and rolling out a one-off parser
;; for sexps seems easier than figuring out how to use `parse-partial-sexp'
(defun tsp--read-sexp (string)
  "Quick and dirty reader to parse a tree-sitter query sexp in STRING.
Can't use `read' because of the #predicate syntax.

Returns a list of one or more sexps, compatible with `treesit-query-capture'."
  (cl-flet ((finalise-token (string state last-char where &aux (where (or where "??")))
                         (case state
                           (:number (string-to-number string))
                           (:anchor :anchor)
                           (:string string)
                           ((:symbol :capture :predicate) (intern string))
                           ((:field)
                            (unless (memql last-char '(?: ?\ ))
                              (error "Syntax error at %s, ':' is only valid at the end of field names" where))
                            (intern string))
                           (:quantifier (intern (concat ":" token)))
                           ;; If last-state is nil, this means we've only seen whitespace, so don't return anything
                           ((nil) nil)
                           (t
                            (error "Could not parse token '%s' at %s (parser state %s)"
                                   string where state)))))
      (cl-loop with result = ()
               with stack = ()
               with state = nil
               with escape = nil
               with token = nil
               with where = 0
               for i from 0
               ;; t if the previous char was whitespace, important for parsing quantifiers
               for whitespace = nil then (memql char '(?\  ?\n))
               for char being the elements of string do
               (block nil
                 (cl-flet ((next-char (&optional (new-state nil new-state-p))
                                      (push char token)
                                      (when new-state-p
                                        (setf state new-state
                                              where (or where i)))
                                      (setf escape nil)
                                      (return))
                           (standard-next-state ()
                                                (case state
                                                  ((nil :negation :number :symbol :anchor) :symbol)
                                                  ((:string :field :capture :predicate) state)
                                                  (t
                                                   (error "No standard next state for %s" state))))
                           (finalise-token ()
                                           (prog1
                                               (finalise-token (apply #'string (nreverse token))
                                                               state char where)
                                             (setf token nil
                                                   state nil
                                                   where nil)))
                           (push-stack (type)
                                       (push (cons () type) stack))
                           (char-to-type (char)
                                         (ecase char
                                           ((?\( ?\))
                                            :list)
                                           ((?\[ ?\])
                                            :vector)))
                           (pop-stack (type)
                                      (destructuring-bind (vals . saved-type)
                                          (pop stack)
                                        (unless (eq type saved-type)
                                          (error "Mismatched closing delimiter '%c' at %s" char i))
                                        (funcall (ecase type
                                                   (:list #'identity)
                                                   (:vector #'vconcat))
                                                 (nreverse vals))))
                           (collect (parsed)
                                    (when parsed
                                      (if stack
                                          (push parsed (first (first stack)))
                                        (push parsed result)))))
                   ;; Note: although the official TS grammar for TS queries talks about
                   ;; comments (';'), they are not documented (and thus probably not used
                   ;; much) and I don't want to bother suppporting them, so this parser
                   ;; doesn't.
                   ;;
                   ;; Note: it is also more lenient in some places, and possibly a bit
                   ;; more strict in some others; generally in places where the docs don't
                   ;; really bother to explain things so it's hard to tell what the
                   ;; official syntax is
                   (pcase char
                     ((or ?\( ?\[)
                      (when (or escape (eq state :string))
                        (next-char))
                      (when state
                        (collect (finalise-token)))
                      (setf where i)
                      (push-stack (char-to-type char)))
                     ((or ?\) ?\])
                      (when (or escape (eq state :string))
                        (next-char))
                      (unless stack
                        (error "Unmatched closing delimiter '%c' at %s" char i))
                      (when state
                        (collect (finalise-token)))
                      (collect (pop-stack (char-to-type char))))
                     ((pred cl-digit-char-p)
                      ;; A digit means we're probably at the start of a number, unless we're in a
                      ;; known state already
                      (next-char (or state :number)))
                     ;; Note: '?' is valid in predicate names, but we have to handle it
                     ;; separately because it's also a quantifier
                     ;; Note: this will misparse negative numbers as symbols, but I'm not sure
                     ;; numbers are even a thing in tree-sitter queries, so it shouldn't matter
                     ((guard (string-match-p "[-[:alpha:]_]" (string char)))
                      (next-char (standard-next-state)))
                     (?.
                      (when escape
                        (next-char (standard-next-state)))
                      (ecase state
                        ((nil) (next-char :anchor))
                        ((:negation :string :field :number :symbol :anchor :capture :predicate)
                         (next-char (standard-next-state)))
                        (:quantifier
                         (error "Syntax error at %s, '%c' cannot follow quantifier" i char))))
                     (?!
                      (when escape
                        (next-char (standard-next-state)))
                      (case state
                        ((nil) (next-char :negation))
                        ((:string :predicate) (next-char))
                        (t
                         (error "Syntax error at  %s, '!' is only valid as negation and in predicate names" i))))
                     ((or ?\  ?\n)   ; space or newline
                      (if escape
                          (next-char)
                        (collect (finalise-token))))
                     (?\\
                      (if (not escape)
                          (setf escape t)
                        ;; An escaped backslash outside of a string might as well be
                        ;; classified as symbol
                        (next-char (standard-next-state))))
                     (?\"
                      (if escape
                          (next-char)
                        (case state
                          (:string
                           (collect (finalise-token)))
                          ((nil) (setf state :string
                                     where i))
                          (t (collect (finalise-token))
                             (setf state :string
                                   where i)))))
                     (?@
                      (when escape
                        (next-char (standard-next-state)))
                      (ecase state
                        ((:negation :number :symbol :field :capture :predicate :quantifier)
                         (error "Syntax error at %s, '@' is only valid at the start of token" i))
                        (:string (next-char))
                        ((nil) (next-char :capture))))
                     (?#
                      (when escape
                        (next-char (standard-next-state)))
                      (ecase state
                        ((:negation :number :symbol :field :capture :predicate :quantifier)
                         (error "Syntax error at %s, '#' is only valid at the start of token" i))
                        (:string (next-char))
                        ((nil)
                         (setf char ?:)
                         (next-char :predicate))))
                     (?:
                      (when escape
                        (next-char (standard-next-state)))
                      (ecase state
                        (:string (next-char))
                        ((:number :symbol)
                         (next-char :field))
                        ((nil :negation :field :capture :predicate :quantifier)
                         (error "Syntax error at %s, ':' is only valid at the end of field names"))))
                     ((or ?+ ?* ??)
                      (when escape
                        (next-char (standard-next-state)))
                      (when (and whitespace
                                 (not (eq state :string)))
                        (error "Syntax error at %s, quantifier '%c' not valid after whitespace"
                               i char))
                      ;; '?' is special because it can be either a quantifier, or a part of
                      ;; a predicate name like #eq?
                      (if (eql char ??)
                          (ecase state
                            ((:string :predicate) (next-char))
                            ;; It's not clear whether ? would be a part of a symbol or a
                            ;; quantifier after the symbol, the docs are not very thorough
                            ((nil :number :symbol)
                             (next-char :quantifier))
                            ((:negation :field :capture :quantifier)
                             (error "Syntax error at %s, quantifier '%c' is not valid in this context (%s)"
                                    i char state)))
                        (ecase state
                          (:string (next-char))
                          ((nil :number :symbol)
                           (next-char :quantifier))
                          ((:negation :field :capture :predicate :quantifier)
                           (error "Syntax error at %s, quantifier '%c' is not valid in this context (%s)"
                                  i char state)))))

                     (_
                      (error "Syntax error at %s, unexpected character `%c'" i char)))))
            finally do
            (when stack
              (error "Input not terminated properly, unbalanced parens or quotes?"))
            (when token
              (push (finalise-token (apply #'string (nreverse token))
                                    state char where)
                    result))
            finally return (nreverse result))))

(defun test-tsp--do-run-tests (spec)
  "Quick and dirty test runner"
  (cl-loop for (func orig-form expected . data) in spec
           for signalled = nil
           do (if (eq expected :error)
                  (destructuring-bind (&optional (error-class 'error) &rest extra) data
                    (condition-case err
                        (funcall func)
                      (error
                       (unless (eq (car err) error-class)
                         (error "Test form '%s' should have signalled `%s', but got %s"
                                orig-form error-class err))
                       (setf signalled t)))
                    (unless signalled
                      (error "Test form '%s' was expected to signal %s, but no error was raised"
                             orig-form error-class)))
                (let ((result (condition-case err
                                  (funcall func)
                                (error "Test form '%s' signalled %s" orig-form err))))
                  (unless (equal result expected)
                    (error "Test form '%s' returned '%s', but '%s' was expected"
                           orig-form result expected))))))

(cl-defmacro test-tsp-run-tests (&body specs)
  "Quick and dirty test runner"
  (declare (indent defun))
  (cl-loop for spec in specs
           for (form expected . data) = spec
           collect `(list (lambda () ,form) ',form ,expected ,@data) into test-forms
           finally return `(test-tsp--do-run-tests (list ,@test-forms))))

(defun test-tsp--read-sexp ()
  "Quick and dirty test suite for `tsp--read-sexp'"
  (test-tsp-run-tests
    ((tsp--read-sexp "foo") '(foo))
    ((tsp--read-sexp "foo bar baz") '(foo bar baz))

    ((tsp--read-sexp "(function_definition name: (identifier) @name)")
     '((function_definition name: (identifier) @name)))
    ((tsp--read-sexp "(function_definition name: ([identifier \"none\"]+) @name)?")
     '((function_definition name: ([identifier "none"] :+) @name) :?))
    ((tsp--read-sexp "[(function_definition name: ([identifier \"none\"]*) @name) @func.named
                     (function_definition !name) @func.anonymous] @func")
     '([(function_definition name: ([identifier "none"] :*) @name) @func.named
        (function_definition !name) @func.anonymous] @func))

    ((tsp--read-sexp "(call (_) @call.inner)") '((call (_) @call.inner)))
    ((tsp--read-sexp "(call _)") '((call _)))

    ((tsp--read-sexp "(array . (identifier) @the-element)")
     '((array :anchor (identifier) @the-element)))
    ((tsp--read-sexp "(block (_) @last-expression .)")
     '((block (_) @last-expression :anchor)))

    ((tsp--read-sexp "((identifier) @variable.builtin (#eq? @variable.builtin \"self\") (#arbitrary! @variable.builtin))")
     '(((identifier) @variable.builtin (:eq? @variable.builtin "self") (:arbitrary! @variable.builtin))))

    ((tsp--read-sexp "(call [)") :error)
    ((tsp--read-sexp "(call [])))") :error)))

;;; treesit-polyfill-util.el ends here

;; Local Variables:
;; nameless-current-name: "treesit"
;; End:
