;;; treesit-polyfill-query.el --- treesit.el implementation on top of tree-sitter.el -*- lexical-binding: t -*-

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

;; This file contains all the query machinery, including the query
;; rewriting engine to smooth over the differences in predicates
;; supported by tree-siter.el and treesit.el

;; Currently tracking git tag emacs-29.2 (ef01b634d219bcceda17dcd61024c7a12173b88c)

;;; Code:

(require 'treesit-polyfill-node)

(defalias 'treesit-compiled-query-p 'tsc-query-p)
(defalias 'treesit-query-p 'tsc-query-p)

(defun treesit-pattern-expand (pattern)
  "Expand PATTERN to its string form.

PATTERN can be

    :anchor
    :?
    :*
    :+
    :equal
    :match
    (TYPE PATTERN...)
    [PATTERN...]
    FIELD-NAME:
    @CAPTURE-NAME
    (_)
    _
    \"TYPE\"

See Info node ‘(elisp)Pattern Matching’ for detailed explanation."
  (pcase pattern
    (:anchor ".")
    ((or :? :* :+)
     (tsp--sym-name pattern))
    ((or :match :equal :pred)
     (concat "#" (tsp--sym-name pattern)))
    ((pred stringp) (format "\"%s\"" pattern))
    (_ (format "%s" pattern))))

(defun treesit-query-expand (query)
  "Expand sexp QUERY to its string form.

A PATTERN in QUERY can be

    :anchor
    :?
    :*
    :+
    :equal
    :match
    (TYPE PATTERN...)
    [PATTERN...]
    FIELD-NAME:
    @CAPTURE-NAME
    (_)
    _
    \"TYPE\"

See Info node ‘(elisp)Pattern Matching’ for detailed explanation."
  (cond
   ((listp query)
    (format "(%s)" (mapconcat #'treesit-query-expand query " ")))
   ((vectorp query)
    (format "[%s]" (mapconcat #'treesit-query-expand query " ")))
   (t (treesit-pattern-expand query))))

(defun tsp--wrap-query (query predicates)
  "Return (cons QUERY PREDICATES). This is needed because
tree-sit.el does not support the same set of predicates as
treesit.el, so we need to emulate some of them."
  (cons query predicates))

(cl-defmacro tsp--unwrap-query ((query-var predicates-var &key language) query &body body)
  "Helper to unwrap the cons of (QUERY . PREDICATES) inside BODY.
The unwrapped values will be bound to QUERY-VAR and PREDICATES-VAR.

QUERY must satisfy `tsc-query-p', or be a valid input to
`treesit-query-compile' It is also permissible to pass in a naked
QUERY (ie. not a cons), in which case the PREDICATES-VAR will be
bound to nil."
  (declare (indent 2))
  (let ((query-and-predicates (gensym "query-and-predicates")))
    `(let* ((,query-and-predicates
             (pcase ,query
               ((and `(,,query-var . ,,predicates-var)
                     (guard (tsc-query-p ,query-var)))
                (cons ,query-var ,predicates-var))
               ((pred tsc-query-p)
                (cons ,query-var nil))
               ((pred listp)
                (let* ((lang ,language)
                       (compiled (treesit-query-compile ,language ,query)))
                  compiled))
               (_
                (error "%S is not a valid treesit-polyfill query, should be (cons QUERY PREDICATES)" ,query))))
            (,query-var (car ,query-and-predicates))
            (,predicates-var (cdr ,query-and-predicates)))
       ,@body)))

(cl-defmacro tsp--query-defun (name (query-and-predicates &rest args)
                                   &body body)
  "Unwrapping defun variant for queries, See `tsp--unwrapping-defun' for details"
  (declare (indent defun))
  (cl-destructuring-bind (query-var &optional predicate-var &key (allow-nil t) rewrap language)
      (if (listp query-and-predicates)
          query-and-predicates
        (list query-and-predicates))
    (let ((vars-and-options `((,query-var ,predicate-var)
                              :allow-nil ,allow-nil :rewrap ,rewrap)))
      `(tsp--unwrapping-defun :query ,name (,vars-and-options ,@args)
        ,@body))))

(defun treesit-query-compile (language query &optional eager)
  "Compile QUERY to a compiled query.

Querying with a compiled query is much faster than an uncompiled one.
LANGUAGE is the language this query is for.

If EAGER is non-nil, immediately load LANGUAGE and compile the query.
Otherwise defer the compilation until the query is first used.

Signal ‘treesit-query-error’ if QUERY is malformed or something else
goes wrong.  (This only happens if EAGER is non-nil.)
You can use ‘treesit-query-validate’ to validate and debug a query."
  ;; We need to preprocess the query prior to compiling it, since treesit-* API supports
  ;; arbitrary functions as predicates with :pred, but TSC does not.
  ;; The sketch of the preprocessing step is as follows:
  ;;
  ;;  * Walk the query, find all instances of :pred
  ;;  * For each of them, subtitute a special, autogenerated capture
  ;;  * When we get results, first look for our special captures, and for each of them,
  ;;    run the original predicate function. If it succeeds, output the match data,
  ;;    skipping our magical captures, otherwise the entire query pattern has failed
  ;;
  ;; Considerations:
  ;;
  ;;  * When we run predicates for our special captures, we need to provide them with
  ;;    arguments, which will generally be other captured nodes. However, we need to get
  ;;    the scopes right in case the same name is reused in multiple places in the
  ;;    pattern. Fortunately, testing suggests that tree-sitter itself only has one scope,
  ;;    the entire top-level pattern in the query, which can be easily solved using
  ;;    `tsc-query-matches'. Overall, it seems that the predicate behaviour is very poorly
  ;;    documented (as are most aspects of TS), and also rather inconsistent and
  ;;    contradictory. For example, given the following python input:
  ;;
  ;;    -----------
  ;;
  ;;    if 13:
  ;;        42
  ;;        42
  ;;        42
  ;;        42
  ;;
  ;;    -----------
  ;;
  ;;    The following query fails to capture anything:
  ;;      (if_statement ((integer) @num (.match? @num "13"))
  ;;                    (block (expression_statement ((integer) @num (.match? @num "42")))))
  ;;    which would follow the documented behaviour of "predicate must match _all_ nodes
  ;;    unless any- is used".
  ;;
  ;;    But these two capture every @num successfully:
  ;;      (if_statement ((integer) @num (.match? @num "13"))
  ;;                    (block (expression_statement ((integer) @num (.any-match? @num "42")))))
  ;;      (if_statement ((integer) @num (.match? @num "13"))
  ;;                    (block (expression_statement ((integer) @num))))
  ;;    which very much contradicts that idea.
  ;;
  ;;    This might need some tweaking to get results that match what in-the-wild code
  ;;    expects without needing to reimplement all of predicate matching logic
  ;;    ourselves. treesit.el does not, at the moment, support any-* predicate variants,
  ;;    so that might make things easier.
  ;;
  ;;    HOWEVER, further testing does suggest that, at least with differently-named nodes,
  ;;    scoping is a thing. I.e., given the query:
  ;;      (if_statement condition: ((integer) @numcond)
  ;;                    consequence: (block (expression_statement ((integer) @num
  ;;                                                               (:equal @numcond @numalt))))
  ;;                    alternative: (else_clause body: (block (expression_statement (integer) @numalt))))
  ;;    The capture errors out because @numalt is not visible in the scope in which :equal is declared
  ;;
  ;;    UPDATE: even more testing indicates that there are no scopes, BUT, captures are
  ;;    only visible after they've been first mentioned, in depth-first order. I.e. in the
  ;;    example above, after @numalt has been declared, a predicate can refer to @num or
  ;;    @numcond just as well and it's considered valid.

  ;; Other notes:
  ;;  * Treesitter's upstream docs on the subject are awful and explain nothing
  ;;  * treesit.el's `:match' predicate seems to be equivalent to TSC's `.any-match?',
  ;;    save for the argument order being reversed.  There isn't an obvious equivalent to
  ;;    `.match?', but that shouldn't be a problem
  ;;  * For `:equal', the behaviour is seemingly the same as `.eq?'. Again, no equivalent
  ;;    of `.any-eq?'
  (let ((query (tsp--query-as-sexp query))
        ;; TSC needs to get a list of queries (even if the list contains
        ;; only one element), whereas treesit.el accepts a sexp
        ;; representing just one query, not wrapped in a list. Make sure
        ;; we always have a list of queries here
        (query (if (every #'sequencep query)
                   query
                 (list query)))
        (prefix "tsp--transformed-pred-")
        (counter 0)
        ;; Store every place where :pred appears in the query, so we can emulate them
        (predicates ())
        ;; In case one of the captures in the query somehow manages to conflict with one
        ;; of the ones we generate
        (conflicts ()))
    ;; Meat of the compiler. Here we translate from treesit.el's :foo syntax to TSC's
    ;; .foo?, and also precompile :pred so we can emulate it later in `treesit-query-capture'
    (cl-flet ((transform (elem _parents)
                (pcase elem
                  ((and `(:pred ,fn . ,args)
                        (guard (functionp fn))
                        (guard (every (lambda (x) (or (tsp--node-capture-p x)
                                                      (stringp x)))
                                      args)))
                   (let ((capture (cl-loop for cand = (intern (format "%s%s" prefix counter))
                                           do (incf counter)
                                           while (memq cand conflicts)
                                           finally return cand))
                         (args (cl-loop for arg in args
                                        collect (if (symbolp arg)
                                                    ;; Chop off the leading @ to make matching esier
                                                    (intern (substring (symbol-name arg) 1))
                                                  arg))))
                     (push `(,capture ,fn . ,args) predicates)
                     ;; Captures in queries are prefixed with @, but captures are
                     ;; returned,without, so we need to store unprefixed name in
                     ;; predicates but return prefix result
                     (intern (format "@%s" capture))))
                  ((and `(:pred ,fn . ,args)
                        (guard (not (functionp fn))))
                   (signal 'treesit-query-error (format "Invalid :pred function: %s. :pred only supports functions defined with DEFUN" fn)))
                  (`(:pred . ,_)
                   (signal 'treesit-query-error (format "Invalid :pred use: %s" elem)))
                  ((and `(:match ,pat ,node)
                        (guard (stringp pat)))
                   `(.match? ,node ,pat))
                  (`(:match . ,_)
                   (signal 'treesit-query-error (format "Invalid :match use: %s" elem)))
                  (`(:equal ,left ,right) `(.eq? ,left ,right))
                  (`(:equal . ,_)
                   (signal 'treesit-query-error (format "Invalid :equal use: %s" elem)))
                  ((and `(,sym . ,_)
                        (guard (keywordp sym)))
                   (signal 'treesit-query-error (format "Unknown predicate: %s" elem)))
                  ((pred keywordp)
                   (signal 'treesit-query-error (format "Invalid predicate use: %s" elem)))
                  (_ elem))))
    (tsp--walk-query query
                     (lambda (elem _)
                       (when (and (symbolp elem)
                                  (string-prefix-p prefix (tsp--sym-name elem)))
                         (push elem conflicts))))
    (tsp--wrap-query (tsc-make-query (tree-sitter-require language)
                                     (cl-loop for pattern in query
                                              for transformed = (tsp--walk-query pattern #'transform t)
                                              ;; Because TS's "scope" for what captures can be referenced in a predicate
                                              ;; is essentially "anything that comes earlier in the text of the same
                                              ;; pattern, no matter the nesting structure", we do a depth-first walk to
                                              ;; grab an ordered list of when captures are declared
                                              for scope = ()
                                              do (tsp--walk-query transformed
                                                                  (lambda (elem _)
                                                                    (when (tsp--node-capture-p elem)
                                                                      (pushnew elem scope))))
                                              ;; Now that we have a "scope", just walk every sublist in order, and error out
                                              ;; if anything references things it shouldn't be able to see
                                              do (cl-loop for (capture . visible) on scope
                                                          do (when-let ((maybe-pred (alist-get capture predicates))
                                                                        (fn (car maybe-pred))
                                                                        (args (cdr maybe-pred))
                                                                        (invalid (remove-if-not (lambda (x)
                                                                                                  (and (tsp--node-capture-p x)
                                                                                                       (not (memq x visible))))
                                                                                                args)))
                                                               (signal 'treesit-query-error (format "Unknown capture group %s in %s"
                                                                                                    (mapconcat #'prin1-to-string invalid ", ")
                                                                                                    `(:pred ,fn ,@args)))))
                                              collect transformed))
                     (reverse predicates)))))

(defun treesit-query-capture (node query &optional beg end node-only)
  "Query NODE with patterns in QUERY.

Return a list of (CAPTURE_NAME . NODE).  CAPTURE_NAME is the name
assigned to the node in PATTERN.  NODE is the captured node.

QUERY is either a string query, a sexp query, or a compiled query.
See Info node ‘(elisp)Pattern Matching’ for how to write a query in
either string or sexp form.  When using repeatedly, a compiled query
is much faster than a string or sexp one, so it is recommend to
compile your query if it will be used repeatedly.

BEG and END, if both non-nil, specify the region of buffer positions
in which the query is executed.  Any matching node whose span overlaps
with the region between BEG and END are captured, it doesn’t have to
be completely in the region.

If NODE-ONLY is non-nil, return a list of nodes.

Besides a node, NODE can also be a parser, in which case the root node
of that parser is used.
NODE can also be a language symbol, in which case the root node of a
parser for that language is used.  If such a parser doesn’t exist, it
is created.

Signal ‘treesit-query-error’ if QUERY is malformed or something else
goes wrong.  You can use ‘treesit-query-validate’ to validate and debug
the query."
  (tsp--unwrap-node (node parser :allow-coerce-parser t) node
    (tsp--unwrap-query (query predicates
                              :language (treesit-parser-language parser))
        query
      (let* ((buffer (treesit-parser-buffer parser))
             (matches (tsc-query-matches query node
                                         (lambda (beg-byte end-byte)
                                           (with-current-buffer buffer
                                             (buffer-substring (byte-to-position beg-byte)
                                                               (byte-to-position end-byte)))))))
        (cl-loop for (i . captures) across matches
                 append
                 (cl-loop named inner
                          with captures = (cl-coerce captures 'list)
                          for (capture . node-ptr) in captures
                          ;; If it's a previously pre-compiled :pred, we need to check if it matches
                          for (pred-fn . args) = (alist-get capture predicates)
                          for satisfied = (when pred-fn
                                            (apply pred-fn (cl-loop for arg in args
                                                                    collect (or (when-let ((node (alist-get arg captures)))
                                                                                  (cons node parser))
                                                                                ;; If it's not in captures, then it must be a string
                                                                                arg))))
                          if (and pred-fn (not satisfied))
                          return nil
                          if (not pred-fn)
                          collect (cons capture (tsp--wrap-node node-ptr parser))))))))

;; We need to be able to parse string form of TS queries back into sexps, because we need
;; to rewrite them in order to implement treesit.el's `:pred' functionality. `read' can't
;; be used because it trips up on the #predicate syntax, and rolling out a one-off parser
;; for sexps seems easier than figuring out how to use `parse-partial-sexp'
(defun tsp--unexpand-query (string)
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
                           (:quantifier (intern (concat ":" string)))
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
               (cl-block nil
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
                         (error "Syntax error at %s, ':' is only valid at the end of field names" where))))
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
              (error "Input query not terminated properly, unbalanced parens or quotes?"))
            (when token
              (push (finalise-token (apply #'string (nreverse token))
                                    state char where)
                    result))
            finally return (nreverse result))))

(defun tsp--query-as-sexp (query)
  "Ensure that QUERY is in SEXP form.
If it's a string, run `tsp--unexpand-query', else return it unchanged"
  (if (stringp query)
      (tsp--unexpand-query query)
    query))

(defun tsp--walk-query (query fn &optional subst)
  "Walk the QUERY, recursively running FN on each element.

FN will be called on each subtree before descending, then on each
of its elements. It should be a function of two arguments,
\(ELEM PARENTS), where ELEM is the element or subtree being
visited, and PARENTS is a list of elements descended into to get
to ELEM. If SUBST is t, FN' return value will be used instead
of the element if it's different. If a substitution was made, it
will not be further descended into.

Return the tree, possibly modified."
  (cl-labels ((walk (visited parents)
                    (cl-loop with parents = (cons visited parents)
                             for elem being the elements of visited
                             for candidate = (funcall fn elem parents)
                             for candidate = (if subst candidate elem)
                             if (and (sequencep elem)
                                     (eq candidate elem))
                             collect (walk elem parents) into ret
                             else collect candidate into ret
                             finally return (if (sequencep visited)
                                                (cl-coerce ret (type-of visited))
                                              ret))))
      (walk (tsp--query-as-sexp query) ())))

(defun tsp--node-capture-p (sym)
  "Return non-nil if SYM is a node capture (ie. symbol of the form @something)."
  (and (symbolp sym)
       (string-prefix-p "@" (symbol-name sym))))

(provide 'treesit-polyfill-query)

;;; treesit-polyfill-query.el ends here

;; Local Variables:
;; nameless-current-name: "treesit"
;; End:
