;;; test.el --- tests for treesit-polyfill.el -*- lexical-binding: t -*-

(require 'cl-lib)
(require 's)
(require 'dash)

(defun dedent (string)
  (let* ((lines (s-lines string))
         (prefix (cl-loop for line in lines
                          for prefix = (if (s-blank-str? line)
                                           prefix
                                         (s-shared-start (or prefix line)
                                                         (or (car (s-match "^\s+" line)) "")))
                          finally return prefix))
         (trimmed (-drop-while #'s-blank?
                               (cl-loop for line in lines
                                        collect (if (s-blank-str? line)
                                                    ""
                                                  (s-chop-prefix prefix line))))))
    (s-join "\n" trimmed)))

;; FIXME: This doesn't actually work with built-in treesit.el. Apparently it has to ba an
;; *interned* symbol with a value in function cell for it to get accepted. *SIGH*
(defun reify-func (function)
  "Return a symbol whose function cell is FUNCTION.
Works around treesit's annoying inability to take lambdas as a valid function for :PRED"
  (pcase function
    ((and (pred functionp)
          (pred symbolp))
     function)
    ((and (pred functionp)
          `(function ,symbol))
     symbol)
    ((pred functionp)
     (let ((symbol (gensym "reified-function")))
       (setf (symbol-function symbol) function)
       symbol))))

(defun repr-captures (captures)
  (cl-loop for (name . capture) in captures
           collect (cons name (dedent (treesit-node-text capture)))))

(cl-defun test-tsp--do-run-tests (spec &key (catch-errors nil))
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
                (let ((result (if catch-errors
                                  (condition-case err
                                      (funcall func)
                                    (error "Test form '%s' signalled %s" orig-form err))
                                (funcall func))))
                  (unless (equal result expected)
                    (error "Test form '%S' returned '%S', but '%S' was expected"
                           orig-form result expected))))))

(cl-defmacro test-tsp-run-tests (args &body specs)
  "Quick and dirty test runner"
  (declare (indent defun))
  (cl-loop for spec in specs
           for (form expected . data) = spec
           collect `(list (lambda () ,form) ',form ,expected ,@data) into test-forms
           finally return `(apply #'test-tsp--do-run-tests (list ,@test-forms) ,args)))

(defun test-tsp--unexpand-query (&rest args)
  "Quick and dirty test suite for `tsp--unexpand-query'"
  (test-tsp-run-tests args
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

(defun test-treesit-query-capture (&rest args)
  (let ((src-python-1 (dedent "
                       class MyClass:
                         def hello():
                           print(\"hello from MyClass\")

                       MyClass.hello()

                       def main():
                         print(\"Hello, world!\")

                       if __name__ == \"__main__\":
                         main()

                       if \"foo\" == \"bar\":
                         raise RuntimeError(\"I have questions\")")))
    (cl-macrolet ((with-capture ((var query &optional repr-var) source &body body)
                                (let ((root (gensym "root")))
                                  `(with-temp-buffer
                                     (insert ,source)
                                     (let* ((,root (treesit-parser-root-node (treesit-parser-create 'python)))
                                            (,var (treesit-query-capture ,root ,query))
                                            ,(when repr-var
                                               `(,repr-var (repr-captures ,var))))
                                       ,@body)))))
      (with-capture (capture `(((call
                                 function: [
                                            (identifier) @name
                                            (attribute
                                             attribute: (identifier) @name)
                                            ]
                                 ) @reference.call
                                   (:pred ,(reify-func (lambda (node)
                                                         (eql (length (treesit-node-text node)) 27)))
                                          @reference.call)))
                             repr)
                    src-python-1
                    repr)
      (test-tsp-run-tests args
        ((with-capture (capture '((class_definition
                                   name: (identifier) @name) @definition.class)
                                repr)
                       src-python-1
                       repr)
         `((definition\.class . ,(dedent "
                                  class MyClass:
                                    def hello():
                                      print(\"hello from MyClass\")"))
           (name . "MyClass")))
        ((with-capture (capture '((call
                                   function: [
                                              (identifier) @name
                                              (attribute
                                               attribute: (identifier) @name)
                                              ]) @reference.call)
                                repr)
                       src-python-1
                       repr)
         '((reference\.call . "print(\"hello from MyClass\")")
           (name . "print")
           (reference\.call . "MyClass.hello()")
           (name . "hello")
           (reference\.call . "print(\"Hello, world!\")")
           (name . "print")
           (reference\.call . "main()")
           (name . "main")
           (reference\.call . "RuntimeError(\"I have questions\")")
           (name . "RuntimeError")))
        ((with-capture (capture '((call
                                   function: [
                                              (identifier) @name
                                              (attribute
                                               attribute: (identifier) @name)
                                              ]
                                   (:equal @name "print")
                                   ) @reference.call)
                                repr)
                       src-python-1
                       repr)
         '((reference\.call . "print(\"hello from MyClass\")")
           (name . "print")
           (reference\.call . "print(\"Hello, world!\")")
           (name . "print")))
        ((with-capture (capture '(((call
                                    function: [
                                               (identifier) @name
                                               (attribute
                                                attribute: (identifier) @name)
                                               ]
                                    ) @reference.call
                                      (:match "questions" @reference.call)))
                                repr)
                       src-python-1
                       repr)
         '((reference\.call . "RuntimeError(\"I have questions\")")
           (name . "RuntimeError")))
        ((with-capture (capture `(((call
                                    function: [
                                               (identifier) @name
                                               (attribute
                                                attribute: (identifier) @name)
                                               ]
                                    ) @reference.call
                                      (:pred ,(reify-func (lambda (node)
                                                            (eql (length (treesit-node-text node)) 32)))
                                             @reference.call)))
                                repr)
                       src-python-1
                       repr)
         '((reference\.call . "RuntimeError(\"I have questions\")")
           (name . "RuntimeError"))))
      )))

(defun test-tsp-run-all ()
  (test-tsp--unexpand-query)
  (test-treesit-query-capture)
  t)
