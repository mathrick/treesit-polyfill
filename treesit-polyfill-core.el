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

(defun tsp--get-target-buffer (buffer)
  "Get the target buffer for treesit operations. This means if
  BUFFER is nil, use `current-buffer'. For indirect buffers, look
  up their base buffer and return that instead."
  (let ((buffer (or buffer (current-buffer))))
    (or (buffer-base-buffer buffer) buffer)))

(defun tsp--sync-parser-buffer-map ()
  "Reflect the contents of `tsp--buffer-parser-map' into `tsp--parser-buffer-map'."
  (cl-loop for buf being the hash-keys of tsp--buffer-parser-map
               using (hash-value parsers)
           ;; cl-loop generates buggy code if we do "for parser in
           ;; parsers" in the outer loop, have to use nested
           do (cl-loop for parser in parsers
                       do (setf (gethash parser tsp--parser-buffer-map) buf))))

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

(defun treesit-parser-list (&optional buffer)
  "Return BUFFER’s parser list.

BUFFER defaults to the current buffer.  If that buffer is an indirect
buffer, its base buffer is used instead.  That is, indirect buffers
use their base buffer’s parsers."
  (gethash (tsp--get-target-buffer buffer) tsp--buffer-parser-map))

(defun treesit-parser-buffer (parser)
  "Return the buffer of PARSER."
  (unless (gethash parser tsp--parser-buffer-map)
    ;; Try syncing the hash tables to see if we can find the buffer
    (tsp--sync-parser-buffer-map))
  (or (gethash parser tsp--parser-buffer-map)
      (error "Parser %s has no known buffer association. It might have been created outside of treesit.el API"
             parser)))

(defun treesit-parser-language (parser)
  "Return PARSER’s language symbol.
This symbol is the one used to create the parser."
  (tsp--lang-name (tsc-parser-language parser)))

(defun treesit-parser-root-node (parser)
  "Return the root node of PARSER."
  (error "FIXME: Not yet implemented"))

(defun treesit-parser-set-included-ranges (parser ranges)
  "Limit PARSER to RANGES.

RANGES is a list of (BEG . END), each (BEG . END) defines a region in
which the parser should operate.  Regions must not overlap, and the
regions should come in order in the list.  Signal
‘treesit-set-range-error’ if the argument is invalid, or something
else went wrong.  If RANGES is nil, the PARSER is to parse the whole
buffer."
  (error "FIXME: Not yet implemented"))

(defun treesit-parser-included-ranges (parser)
  "Return the ranges set for PARSER.
If no ranges are set for PARSER, return nil.
See also ‘treesit-parser-set-included-ranges’."
  (error "FIXME: Not yet implemented"))

(defun treesit-parser-add-notifier (parser function)
  "FUNCTION must be a function symbol, rather than a lambda form.
FUNCTION should take 2 arguments, RANGES and PARSER.  RANGES is a list
of cons cells of the form (START . END), where START and END are buffer
positions.  PARSER is the parser issuing the notification."
  (error "FIXME: Not yet implemented"))

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
  "Helper macro around `defun' which automatically unwraps a passed-in node.
NODE-AND-PARSER can either be a list of the form
\(NODE-VAR &OPTIONAL PARSER-VAR &KEY (ALLOW-NIL T) REWRAP)
or a single name, which is equivalent to (NODE-VAR).

BODY will be automatically wrapped in `tsp--unwrap-node'.

NODE-VAR and PARSER-VAR are symbols which will be bound to the
unwrapped value of node and parser respectively inside
BODY. PARSER-VAR can be omitted or '_, in which case it will not
be bound. NODE-VAR will also be included as the first argument
of the generated `defun'.

ALLOW-NIL controls whether the node passed in is allowed to be
nil (t by default). If so, the whole function will return nil in
that case.

REWRAP means the return value will be automatically wrapped via
`tsp--wrap-node', using the same parser value as was passed in."
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
     ,@(when doc (list doc))
     (,definition node ,@args)))

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

(cl-defmacro tsp--unwrap-query ((query-var predicates-var) query &body body)
  "Helper to unwrap the cons of (QUERY . PREDICATES) inside BODY.
The unwrapped values will be bound to QUERY-VAR and PREDICATES-VAR.

QUERY must satisfy `tsc-query-p'. It is also permissible to pass in
a naked QUERY (ie. not a cons), in which case the PREDICATES-VAR will
be bound to nil."
  (declare (indent defun))
  `(pcase ,query
     ((and (or `(,,query-var . ,,predicates-var))
           (guard (tsc-query-p ,query-var)))
      ,@body)
     ((pred tsc-query-p)
      (let ((,query-var ,query)
            (,predicates-var nil))
         ,@body))
     (_
      (error "%s is not a valid treesit-polyfill query, should be (cons QUERY PREDICATES)" ,query))))

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
        (prefix "@tsp--pred-")
        (counter 0)
        ;; Store every place where :pred appears in the query, so we can emulate them
        (predicates ())
        ;; In case one of the captures in the query somehow manages to conflict with one
        ;; of the ones we generate
        (conflicts ()))
    ;; Meat of the compiler. Here we translate from treesit.el's :foo syntax to TSC's
    ;; .foo?, and also precompile :pred so we can emulate it later in `treesit-query-capture'
    (cl-flet ((transform (elem parents)
                (pcase elem
                  ((and `(:pred ,fn . ,args)
                        (guard (functionp fn))
                        (guard (every (lambda (x) (or (tsp--node-capture-p x)
                                                      (stringp x)))
                                      args)))
                   (let ((capture (cl-loop for cand = (intern (format "%s%s" prefix counter))
                                           do (incf counter)
                                           while (memq cand conflicts)
                                           finally return cand)))
                     (push `(,capture ,fn . ,args) predicates)
                     capture))
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
                  (_ (progn (when (and (listp elem) (eq (first elem) :pred)) (debug)) elem)))))
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

(tsp--node-defun treesit-query-capture ((node parser) query &optional beg end node-only)
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
  (tsp--unwrap-query (query predicates) query
    ;; FIXME: Need to account for NODE being a parser or language
    (let* ((buffer (treesit-parser-buffer parser))
           (matches (tsc-query-matches query node
                                       (lambda (beg-byte end-byte)
                                         (with-current-buffer buffer
                                           (buffer-substring (byte-to-position beg-byte)
                                                             (byte-to-position end-byte)))))))
      (cl-loop for (i . captures) in matches
               collect
               (cl-loop inner with captures = (append captures nil)
                        for (capture . node-ptr) in captures
                        collect (if-let ((predicate (alist-get capture predicates)))
                                    (destructuring-bind (pred-fn &rest args)
                                        predicate
                                      )
                                  ;; If it's not one of our magic captures, just let it through
                                  (cons capture (tsp--wrap-node node-ptr parser))))))))

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
  (error "FIXME: Not yet implemented"))

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
