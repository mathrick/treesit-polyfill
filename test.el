;;; test.el --- tests for treesit-polyfill.el -*- lexical-binding: t -*-

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

(defun test-tsp--unexpand-query ()
  "Quick and dirty test suite for `tsp--unexpand-query'"
  (test-tsp-run-tests
    ((tsp--unexpand-query "foo") '(foo))
    ((tsp--unexpand-query "foo bar baz") '(foo bar baz))

    ((tsp--unexpand-query "(function_definition name: (identifier) @name)")
     '((function_definition name: (identifier) @name)))
    ((tsp--unexpand-query "(function_definition name: ([identifier \"none\"]+) @name)?")
     '((function_definition name: ([identifier "none"] :+) @name) :?))
    ((tsp--unexpand-query "[(function_definition name: ([identifier \"none\"]*) @name) @func.named
                     (function_definition !name) @func.anonymous] @func")
     '([(function_definition name: ([identifier "none"] :*) @name) @func.named
        (function_definition !name) @func.anonymous] @func))

    ((tsp--unexpand-query "(call (_) @call.inner)") '((call (_) @call.inner)))
    ((tsp--unexpand-query "(call _)") '((call _)))

    ((tsp--unexpand-query "(array . (identifier) @the-element)")
     '((array :anchor (identifier) @the-element)))
    ((tsp--unexpand-query "(block (_) @last-expression .)")
     '((block (_) @last-expression :anchor)))

    ((tsp--unexpand-query "((identifier) @variable.builtin (#eq? @variable.builtin \"self\") (#arbitrary! @variable.builtin))")
     '(((identifier) @variable.builtin (:eq? @variable.builtin "self") (:arbitrary! @variable.builtin))))

    ((tsp--unexpand-query "(call [)") :error)
    ((tsp--unexpand-query "(call [])))") :error)))

(defun test-tsp-run-all ()
  (test-tsp--unexpand-query)
  t)
