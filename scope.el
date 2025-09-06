;;; scope.el --- Semantic analysis for ELisp symbols  -*- lexical-binding: t; -*-

;; Copyright (C) 2024-2025  Eshel Yaron

;; Author: Eshel Yaron <me@eshelyaron.com>
;; Keywords: lisp, languages

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This library implements an analysis that determines the role of each
;; symbol in ELisp code.  The entry point for the analysis is the
;; function `scope', see its docstring for usage information.

;;; Code:

(defun scope--define-symbol-type (name parents props)
  (put name 'scope-parent-types parents)
  (put name 'scope-type-properties props)
  (add-to-list 'current-load-list `(define-symbol-type . ,name)))

;;;###autoload
(defmacro scope-define-symbol-type (name parents &rest props)
  (declare (indent defun))
  `(scope--define-symbol-type ',name ',parents ,(when props `(list ,@props))))

;;;###autoload
(defun scope-get-symbol-type-property (type prop)
  (named-let loop ((current type)
                   (parents (get type 'scope-parent-types))
                   (more nil)
                   (done nil))
    (or (plist-get (get current 'scope-type-properties) prop)
        (when-let ((next (car parents)))
          (loop (car parents) (get next 'scope-parent-types) (append (cdr parents) more) done))
        (when-let ((next (car more)))
          (loop next (let (res)
                       (dolist (per (get next 'scope-parent-types))
                         (unless (memq per done)
                           (push per res)))
                       (nreverse res))
                (cdr more) done)))))

;;;###autoload
(defun scope-symbol-type-p (sym)
  (or (get sym 'scope-parent-types) (get sym 'scope-type-properties)))

(defvar scope-read-symbol-type-history nil)

(defun scope-read-symbol-type (prompt &optional default)
  (completing-read
   (format-prompt prompt default)
   obarray #'scope-symbol-type-p 'confirm
   nil 'scope-read-symbol-type-history default))

(defvar help-mode--current-data)

;;;###autoload
(defun scope-describe-symbol-type (type)
  (interactive (list (scope-read-symbol-type
                      "Describe symbol type"
                      (when-let ((def (symbol-at-point))
                                 ((scope-symbol-type-p def)))
                        def))))
  (when (stringp type) (setq type (intern type)))
  (let ((help-buffer-under-preparation t))
    (help-setup-xref (list #'scope-describe-symbol-type type)
                     (called-interactively-p 'interactive))
    (with-help-window (help-buffer)
      (with-current-buffer standard-output
        (insert "Symbol type "
                (substitute-quotes (concat "`" (symbol-name type) "'"))
                ":\n\n"
                (substitute-quotes
                 (or (scope-get-symbol-type-property type :doc)
                     "Undocumented.")))
        (when-let ((parents (get type 'scope-parent-types)))
          (insert "\n\nParent types: "
                  (mapconcat (lambda (parent)
                               (let ((name (symbol-name parent)))
                                 (substitute-quotes
                                  (concat
                                   "`"
                                   (buttonize
                                    name #'scope-describe-symbol-type name
                                    "mouse-2, RET: describe this symbol type")
                                   "'"))))
                             parents ", ")))
        (setq help-mode--current-data
              (list :symbol type :type 'define-symbol-type
                    :file (find-lisp-object-file-name type 'define-symbol-type)))))))

(scope-define-symbol-type symbol-type ()
  :doc "Symbol type names."
  :face 'semel-symbol-type
  :help  (cl-constantly "Symbol type")
  :completion (cl-constantly #'scope-symbol-type-p)
  :namespace 'symbol-type)

(scope-define-symbol-type symbol-type-definition (symbol-type)
  :doc "Symbol type name definitions."
  :definition t
  :face 'semel-symbol-type-definition
  :help (cl-constantly "Symbol type definition")
  :imenu "Symbol Type"
  :namespace 'symbol-type)

(scope-define-symbol-type variable ()
  :doc "Variable names."
  :face 'semel-free-variable
  :help (lambda (beg end def)
          (cond
           ((equal beg def) "Local variable definition")
           (def             "Local variable")
           (t
            (if-let ((sym (intern (buffer-substring-no-properties beg end))))
                (lambda (&rest _)
                  (let ((val (if (boundp sym) (truncate-string-to-width (prin1-to-string (symbol-value sym)) 60 nil nil t) "#<unbound>")))
                    (if-let ((doc (documentation-property sym 'variable-documentation t)))
                        (format "Special variable `%S'.\n\nValue: %s\n\n%s" sym val doc)
                      (format "Special variable `%S'.\n\nValue: %s" sym val))))
              "Special variable"))))
  :completion (lambda ()
                (let ((local-vars (scope-local-variables)))
                  (lambda (sym) (or (elisp--shorthand-aware-boundp sym)
                                    (memq sym local-vars)))))
  :namespace 'variable)

(declare-function semel--help-echo          "semel")
(declare-function semel--function-help-echo "semel")

(scope-define-symbol-type face ()
  :doc "Face names."
  :face 'semel-face
  :help (lambda (beg end _def)
          (semel--help-echo beg end 'face-documentation "Face"))
  :completion (cl-constantly #'facep)
  :namespace 'face)

(scope-define-symbol-type callable ()
  :doc "Abstract symbol type of function-like symbols."
  :completion (cl-constantly #'elisp--shorthand-aware-fboundp)
  :namespace 'function)

(scope-define-symbol-type function (callable)
  :doc "Function names."
  :face 'semel-function-call
  :help (lambda (beg end def)
          (cond ((equal beg def) "Local function definition")
                (def             "Local function call")
                (t (if-let ((sym (intern-soft (buffer-substring-no-properties beg end))))
                       (apply-partially #'semel--function-help-echo sym)
                     "Function call")))))

(scope-define-symbol-type command (function)
  :doc "Command names.")

(scope-define-symbol-type non-local-exit (function)
  :doc "Functions that do not return."
  :face 'semel-non-local-exit
  :help (lambda (beg end _def)
          (if-let ((sym (intern-soft (buffer-substring-no-properties beg end))))
              (apply-partially #'semel--function-help-echo sym)
            "Non-local exit")))

(scope-define-symbol-type macro (callable)
  :doc "Macro names."
  :face 'semel-macro-call
  :help (lambda (beg end _def)
          (if-let ((sym (intern-soft (buffer-substring-no-properties beg end))))
              (apply-partially #'semel--function-help-echo sym)
            "Macro call")))

(scope-define-symbol-type special-form (callable)
  :doc "Special form names."
  :face 'semel-special-form
  :help (lambda (beg end _def)
          (if-let ((sym (intern-soft (buffer-substring-no-properties beg end))))
              (apply-partially #'semel--function-help-echo sym)
            "Special form")))

(scope-define-symbol-type throw-tag ()
  :doc "Symbols used as `throw'/`catch' tags."
  :face 'semel-throw-tag
  :help (cl-constantly "`throw'/`catch' tag"))

(scope-define-symbol-type warning-type ()
  :doc "Byte-compilation warning types."
  :face 'font-lock-type-face
  :help (cl-constantly "Warning type")
  :completion (cl-constantly (lambda (sym) (memq sym byte-compile-warning-types))))

(scope-define-symbol-type feature ()
  :doc "Feature names."
  :face 'semel-feature
  :help (cl-constantly "Feature")
  :completion (cl-constantly #'featurep)
  :namespace 'feature)

(scope-define-symbol-type declaration ()
  :doc "Function attribute declaration types."
  :face 'font-lock-variable-use-face
  :help (cl-constantly "Declaration")
  :completion (cl-constantly
               (lambda (sym) (or (alist-get sym macro-declarations-alist)
                                 (alist-get sym defun-declarations-alist)))))

(scope-define-symbol-type rx-construct ()
  :doc "`rx' constructs."
  :face 'semel-rx
  :help (cl-constantly "`rx' construct"))

(scope-define-symbol-type theme ()
  :doc "Custom theme names."
  :face 'semel-theme
  :help (cl-constantly "Theme")
  :completion (cl-constantly #'custom-theme-p))

(scope-define-symbol-type thing ()
  :doc "`thing-at-point' \"thing\" identifiers."
  :face 'font-lock-type-face
  :help (cl-constantly "Thing (text object)")
  :completion
  (cl-constantly
   (lambda (sym)
     (or
      (assq sym (bound-and-true-p thing-at-point-provider-alist))
      (assq sym (bound-and-true-p bounds-of-thing-at-point-provider-alist))
      (get sym 'thing-at-point)
      (get sym 'bounds-of-thing-at-point)
      (get sym 'beginning-op)
      (get sym 'end-op)))))

(scope-define-symbol-type slot ()
  :doc "EIEIO slots."
  :face 'font-lock-builtin-face
  :help (cl-constantly "Slot"))

(scope-define-symbol-type widget-type ()
  :doc "Widget types."
  :face 'font-lock-type-face
  :help (cl-constantly "Widget type")
  :completion (cl-constantly (lambda (sym) (get sym 'widget-type)))
  :namespace 'widget-type)

(scope-define-symbol-type type ()
  :doc "ELisp object type names."
  :face 'font-lock-type-face
  :help (cl-constantly "Type")
  :completion (cl-constantly (lambda (sym) (get sym 'cl--class))))

(scope-define-symbol-type group ()
  :doc "Customization groups."
  :face 'font-lock-type-face
  :help (cl-constantly "Customization group")
  :completion (cl-constantly (lambda (sym) (get sym 'group-documentation))))

(scope-define-symbol-type nnoo-backend ()
  :doc "`nnoo' backend names."
  :face 'font-lock-type-face
  :help (cl-constantly "`nnoo' backend"))

(scope-define-symbol-type condition ()
  :doc "`condition-case' conditions."
  :face 'semel-condition
  :help (lambda (beg end _def)
          (if-let ((sym (intern (buffer-substring-no-properties beg end))))
              (lambda (&rest _)
                (let ((msg (get sym 'error-message)))
                  (apply #'concat
                         "`condition-case' condition"
                         (when (and msg (not (string-empty-p msg)))
                           `(": " ,msg)))))
            "`condition-case' condition"))
  :completion (cl-constantly (lambda (sym) (get sym 'error-conditions)))
  :namespace 'condition)

(scope-define-symbol-type ampersand ()
  :doc "Argument list markers, such as `&optional' and `&rest'."
  :face 'font-lock-type-face
  :help (cl-constantly "Arguments separator"))

(scope-define-symbol-type constant ()
  :doc "Self-evaluating symbols."
  :face 'font-lock-builtin-face
  :help (cl-constantly "Constant"))

(scope-define-symbol-type defun ()
  :doc "Function definitions."
  :definition t
  :face 'font-lock-function-name-face
  :help (cl-constantly "Function definition")
  :imenu "Function"
  :namespace 'function)

(scope-define-symbol-type defcmd (defun)
  :doc "Command definitions."
  :help (cl-constantly "Command definition")
  :imenu "Command")

(scope-define-symbol-type defvar ()
  :doc "Variable definitions."
  :definition t
  :face 'font-lock-variable-name-face
  :help (cl-constantly "Special variable definition")
  :imenu "Variable"
  :namespace 'variable)

(scope-define-symbol-type defface ()
  :doc "Face definitions."
  :definition t
  :face 'font-lock-variable-name-face
  :help (cl-constantly "Face definition")
  :imenu "Face"
  :namespace 'face)

(scope-define-symbol-type major-mode ()
  :doc "Major mode names."
  :face 'semel-major-mode-name
  :help (lambda (beg end _def)
          (if-let ((sym (intern (buffer-substring-no-properties beg end))))
              (lambda (&rest _)
                (if-let ((doc (documentation sym)))
                    (format "Major mode `%S'.\n\n%s" sym doc)
                  "Major mode"))
            "Major mode"))
  :completion (cl-constantly (lambda (sym) (get sym 'major-mode-name)))
  :namespace 'function)

(scope-define-symbol-type block ()
  :doc "`cl-block' block names."
  :help (lambda (beg _end def)
          (if (equal beg def) "Block definition" "Block")))

(scope-define-symbol-type icon ()
  :doc "Icon names."
  :face 'font-lock-type-face
  :help (cl-constantly "Icon")
  :completion (cl-constantly (lambda (sym) (get sym 'icon--properties)))
  :namespace 'icon)

(scope-define-symbol-type deficon ()
  :doc "Icon definitions."
  :definition t
  :face 'font-lock-type-face
  :help (cl-constantly "Icon definition")
  :imenu "Icon"
  :namespace 'icon)

(scope-define-symbol-type oclosure ()
  :doc "OClosure type names."
  :face 'font-lock-type-face
  :help (lambda (beg end _def)
          (if-let ((sym (intern (buffer-substring-no-properties beg end))))
              (lambda (&rest _)
                (if-let ((doc (oclosure--class-docstring (get sym 'cl--class))))
                    (format "OClosure type `%S'.\n\n%s" sym doc)
                  "OClosure type"))
            "OClosure type"))
  :completion (cl-constantly (lambda (sym)
                            (oclosure--class-p (get sym 'cl--class))))
  :namespace 'oclosure)

(scope-define-symbol-type defoclosure ()
  :doc "OClosure type definitions."
  :definition t
  :face 'font-lock-type-face
  :help (cl-constantly "OClosure type definition")
  :imenu "OClosure type"
  :namespace 'oclosure)

(scope-define-symbol-type coding ()
  :doc "Coding system names."
  :face 'font-lock-type-face
  :help (lambda (beg end _def)
          (if-let ((sym (intern (buffer-substring-no-properties beg end))))
              (lambda (&rest _)
                (if-let ((doc (coding-system-doc-string sym)))
                    (format "Coding system `%S'.\n\n%s" sym doc)
                  "Coding system"))
            "Coding system"))
  :completion (cl-constantly #'coding-system-p)
  :namespace 'coding)

(scope-define-symbol-type defcoding ()
  :doc "Coding system definitions."
  :definition t
  :face 'font-lock-type-face
  :help (cl-constantly "Coding system definition")
  :imenu "Coding system"
  :namespace 'coding)

(scope-define-symbol-type charset ()
  :doc "Charset names."
  :face 'font-lock-type-face
  :help (lambda (beg end _def)
          (if-let ((sym (intern (buffer-substring-no-properties beg end))))
              (lambda (&rest _)
                (if-let ((doc (charset-description sym)))
                    (format "Charset `%S'.\n\n%s" sym doc)
                  "Charset"))
            "Charset"))
  :completion (cl-constantly #'charsetp)
  :namespace 'charset)

(scope-define-symbol-type defcharset ()
  :doc "Charset definitions."
  :definition t
  :face 'font-lock-type-face
  :help (cl-constantly "Charset definition")
  :imenu "Charset"
  :namespace 'charset)

(defun scope--completion-category-p (c)
  (or (get c 'completion-category-documentation)
      (alist-get c completion-category-defaults)
      (alist-get c completion-category-overrides)))

(scope-define-symbol-type completion-category ()
  :doc "Completion categories."
  :face 'font-lock-type-face
  :help (lambda (beg end _def)
          (if-let ((sym (intern (buffer-substring-no-properties beg end))))
              (lambda (&rest _)
                (if-let ((doc (get sym 'completion-category-documentation)))
                    (format "Completion category `%S'.\n\n%s" sym doc)
                  "Completion category"))
            "Completion category"))
  :completion (cl-constantly #'scope--completion-category-p)
  :namespace 'completion-category)

(scope-define-symbol-type completion-category-definition ()
  :doc "Completion category definitions."
  :definition t
  :face 'font-lock-type-face
  :help (cl-constantly "Completion category definition")
  :imenu "Completion category"
  :namespace 'completion-category)

(defvar scope-counter nil)

(defvar scope-local-functions nil)

(defvar scope--local nil)

(defvar scope-callback #'ignore)

(defvar scope-current-let-alist-form nil)

(defvar scope-gen-id-alist nil)

(defun scope-local-variables (&optional pos)
  "Return list of local variable names in scope at POS."
  (let (all res)
    (save-excursion
      (if pos (goto-char pos) (setq pos (point)))
      (beginning-of-defun)
      (scope (lambda (_type beg len _id &optional _def)
               (when (<= beg pos (+ beg len))
                 (setq all (nconc (mapcar #'car scope--local) all))))))
    (dolist (sym all)
      (let* ((var sym)) (unless (memq var res) (setq res (cons var res)))))
    res))

(defsubst scope-local-new (sym pos &optional local)
  "Return new local context with SYM bound at POS.

Optional argument LOCAL is a local context to extend."
  (cons (cons sym (or pos (cons 'gen (incf scope-counter)))) local))

(defsubst scope-sym-pos (sym)
  (when (symbol-with-pos-p sym) (symbol-with-pos-pos sym)))

(defsubst scope-sym-bare (sym)
  (cond
   ((symbolp sym) sym)
   ((symbol-with-pos-p sym) (bare-symbol sym))))

(defsubst scope-report (type beg len &optional id def)
  (funcall scope-callback type beg len id (or def (and (numberp id) id))))

(defun scope-s (local sym)
  (let* ((beg (scope-sym-pos sym))
         (bare (scope-sym-bare sym))
         (name (symbol-name bare))
         (len (length name))
         (scope--local local))
    (when (and beg (not (booleanp bare)))
      (cond
       ((keywordp bare) (scope-report 'constant beg len))
       ((and scope-current-let-alist-form (= (aref name 0) ?.))
        (if (and (length> name 1) (= (aref name 1) ?.))
            ;; Double dot espaces `let-alist'.
            (let* ((unescaped (intern (substring name 1)))
                   (id (alist-get unescaped local)))
              (scope-report 'variable beg len id))
          (scope-report 'variable beg len
                        (list 'let-alist (car scope-current-let-alist-form) bare)
                        (cdr scope-current-let-alist-form))))
       (t
        (let ((id (alist-get bare local)))
          (scope-report 'variable beg len id)))))))

(defun scope-let-1 (local0 local bindings body)
  (if bindings
      (let* ((binding (ensure-list (car bindings)))
             (sym (car binding))
             (bare (scope-sym-bare sym))
             (len (length (symbol-name bare)))
             (beg (scope-sym-pos sym)))
        (when beg (scope-report 'variable beg len beg))
        (scope-1 local0 (cadr binding))
        (scope-let-1 local0
                     (if bare (scope-local-new bare beg local) local)
                     (cdr bindings) body))
    (scope-n local body)))

(defun scope-let (local bindings body)
  (scope-let-1 local local bindings body))

(defun scope-let* (local bindings body)
  (if bindings
      (let* ((binding (ensure-list (car bindings)))
             (sym (car binding))
             (bare (bare-symbol sym))
             (len (length (symbol-name bare)))
             (beg (scope-sym-pos sym)))
        (when beg (scope-report 'variable beg len beg))
        (scope-1 local (cadr binding))
        (scope-let*
         (scope-local-new bare beg local) (cdr bindings) body))
    (scope-n local body)))

(defun scope-interactive (local intr spec modes)
  (when (symbol-with-pos-p intr)
    (scope-report 'special-form
                  (symbol-with-pos-pos intr)
                  (length (symbol-name (scope-sym-bare intr)))))
  (scope-1 local spec)
  (mapc #'scope-major-mode-name modes))

(defun scope-lambda (local args body)
  "Analyze (lambda ARGS BODY) function definition in LOCAL context."
  (let ((l local))
    (when (listp args)
      (dolist (arg args)
        (when-let ((bare (bare-symbol arg))
                   (beg (scope-sym-pos arg)))
          (unless (memq bare '(&optional &rest))
            (setq l (scope-local-new bare beg l))))))
    ;; Handle docstring.
    (cond
     ((and (consp (car body))
           (or (symbol-with-pos-p (caar body))
               (symbolp (caar body)))
           (eq (bare-symbol (caar body)) :documentation))
      (scope-s local (caar body))
      (scope-1 local (cadar body))
      (setq body (cdr body)))
     ((stringp (car body)) (setq body (cdr body))))
    ;; Handle `declare'.
    (when-let ((form (car body))
               (decl (car-safe form))
               ((or (symbol-with-pos-p decl)
                    (symbolp decl)))
               ((eq (bare-symbol decl) 'declare)))
      (when (symbol-with-pos-p decl)
        (scope-report 'macro
                      (symbol-with-pos-pos decl)
                      (length (symbol-name (bare-symbol decl)))))
      (dolist (spec (cdr form))
        (when-let ((head (car-safe spec))
                   (bare (scope-sym-bare head)))
          (when (symbol-with-pos-p head)
            (scope-report 'declaration
                          (symbol-with-pos-pos head)
                          (length (symbol-name bare))))
          (cl-case bare
            (completion (scope-sharpquote local (cadr spec)))
            (interactive-only
             (when-let ((bare (scope-sym-bare (cadr spec)))
                        ((not (eq bare t))))
               (scope-sharpquote local (cadr spec))))
            (obsolete
             (when-let ((bare (scope-sym-bare (cadr spec))))
               (scope-sharpquote local (cadr spec))))
            ((compiler-macro gv-expander gv-setter)
             ;; Use the extended lexical environment `l'.
             (scope-sharpquote l (cadr spec)))
            (modes (mapc #'scope-major-mode-name (cdr spec)))
            (interactive-args
             (dolist (arg-form (cdr spec))
               (when-let ((arg (car-safe arg-form)))
                 (scope-s l arg)
                 (when (consp (cdr arg-form))
                   (scope-1 local (cadr arg-form)))))))))
      (setq body (cdr body)))
    ;; Handle `interactive'.
    (when-let ((form (car body))
               (intr (car-safe form))
               ((or (symbol-with-pos-p intr)
                    (symbolp intr)))
               ((eq (bare-symbol intr) 'interactive)))
      (scope-interactive local intr (cadar body) (cddar body))
      (setq body (cdr body)))
    ;; Handle ARGS.
    (when (listp args)
      (dolist (arg args)
        (and (symbol-with-pos-p arg)
             (let* ((beg (symbol-with-pos-pos arg))
                    (bare (bare-symbol arg))
                    (len (length (symbol-name bare))))
               (when (and beg (not (eq bare '_)))
                 (if (memq bare '(&optional &rest))
                     (scope-report 'ampersand beg len)
                   (scope-report 'variable beg len beg)))))))
    ;; Handle BODY.
    (scope-n l body)))

(defun scope-defun (local name args body)
  (when-let ((beg (scope-sym-pos name))
             (bare (scope-sym-bare name)))
    (scope-report
     (let ((tmp body))
       (when (stringp (car-safe tmp)) (pop tmp))
       (when (eq 'declare (scope-sym-bare (car-safe (car-safe tmp)))) (pop tmp))
       (if (eq 'interactive (scope-sym-bare (car-safe (car-safe tmp))))
           'defcmd
         'defun))
     beg (length (symbol-name bare))))
  (scope-lambda local args body))

(defun scope-cond (local clauses)
  (dolist (clause clauses) (scope-n local clause)))

(defun scope-setq (local args)
  (let ((var nil) (val nil))
    (while args
      (setq var  (car  args)
            val  (cadr args)
            args (cddr args))
      (scope-s local var)
      (scope-1 local val))))

(defun scope-defvar (local name init)
  (when-let ((beg (scope-sym-pos name))
             (bare (scope-sym-bare name)))
    (scope-report 'defvar beg (length (symbol-name bare))))
  (scope-1 local init))

(defun scope-condition-case (local var bodyform handlers)
  (let* ((bare (bare-symbol var))
         (beg (when (symbol-with-pos-p var) (symbol-with-pos-pos var)))
         (l (scope-local-new bare beg local)))
    (when beg
      (scope-report 'variable beg (length (symbol-name bare)) beg))
    (scope-1 local bodyform)
    (dolist (handler handlers)
      (dolist (cond-name (ensure-list (car-safe handler)))
        (when-let* ((cbeg (scope-sym-pos cond-name))
                    (cbare (scope-sym-bare cond-name))
                    (clen (length (symbol-name cbare))))
          (cond
           ((booleanp cbare))
           ((keywordp cbare) (scope-report 'constant cbeg clen))
           (t                (scope-report 'condition cbeg clen)))))
      (scope-n l (cdr handler)))))

(defvar scope-flet-alist nil)

(defun scope-flet (local defs body)
  (if defs
      (let* ((def (car defs))
             (func (car def))
             (exps (cdr def))
             (beg (scope-sym-pos func))
             (bare (bare-symbol func)))
        (when beg
          (scope-report 'function beg (length (symbol-name bare)) beg))
        (if (cdr exps)
            ;; def is (FUNC ARGLIST BODY...)
            (scope-cl-lambda local (car exps) (cdr exps))
          ;; def is (FUNC EXP)
          (scope-1 local (car exps)))
        (let ((scope-flet-alist (scope-local-new bare beg scope-flet-alist)))
          (scope-flet local (cdr defs) body)))
    (scope-n local body)))

(defun scope-labels (local defs forms)
  (if defs
      (let* ((def (car defs))
             (func (car def))
             (args (cadr def))
             (body (cddr def))
             (beg (scope-sym-pos func))
             (bare (bare-symbol func)))
        (when beg
          (scope-report 'function beg (length (symbol-name bare)) beg))
        (let ((scope-flet-alist (scope-local-new bare beg scope-flet-alist)))
          (scope-lambda local args body)
          (scope-flet local (cdr defs) forms)))
    (scope-n local forms)))

(defvar scope-block-alist nil)

(defun scope-block (local name body)
  (if name
      (let* ((beg (scope-sym-pos name))
             (bare (bare-symbol name)))
        (when beg
          (scope-report 'block beg (length (symbol-name bare)) beg))
        (let ((scope-block-alist (scope-local-new bare beg scope-block-alist)))
          (scope-n local body)))
    (scope-n local body)))

(defun scope-return-from (local name result)
  (when-let ((bare (and (symbol-with-pos-p name) (bare-symbol name)))
             (pos (alist-get bare scope-block-alist)))
    (scope-report 'block
                  (symbol-with-pos-pos name) (length (symbol-name bare)) pos))
  (scope-1 local result))

(defvar scope-assume-func-p nil)

(defun scope-sharpquote (local arg)
  (cond
   ((or (symbol-with-pos-p arg) (symbolp arg))
    (let ((bare (bare-symbol arg))
          (beg (scope-sym-pos arg)))
      (cond
       ((or (functionp bare) (memq bare scope-local-functions) scope-assume-func-p)
        (when beg
          (scope-report 'function beg (length (symbol-name bare)))))
       ((or (assq bare scope-flet-alist) (consp arg))
        (scope-1 local arg)))))
   ((consp arg) (scope-1 local arg))))

(defun scope-loop-for-and (local rest)
  (if (eq (scope-sym-bare (car rest)) 'and)
      (scope-loop-for local local (cadr rest) (cddr rest))
    (scope-loop local rest)))

(defun scope-loop-for-by (local0 local expr rest)
  (scope-1 local0 expr)
  (scope-loop-for-and local rest))

(defun scope-loop-for-to (local0 local expr rest)
  (scope-1 local0 expr)
  (when-let ((bare (scope-sym-bare (car rest)))
             (more (cdr rest)))
    (cond
     ((eq bare 'by)
      (scope-loop-for-by local0 local (car more) (cdr more)))
     (t (scope-loop-for-and local rest)))))

(defun scope-loop-for-from (local0 local expr rest)
  (scope-1 local0 expr)
  (when-let ((bare (scope-sym-bare (car rest)))
             (more (cdr rest)))
    (cond
     ((memq bare '(to upto downto below above))
      (scope-loop-for-to local0 local (car more) (cdr more)))
     ((eq bare 'by)
      (scope-loop-for-by local0 local (car more) (cdr more)))
     (t (scope-loop-for-and local rest)))))

(defun scope-loop-for-= (local0 local expr rest)
  (scope-1 local0 expr)
  (when-let ((bare (scope-sym-bare (car rest)))
             (more (cdr rest)))
    (cond
     ((eq bare 'then)
      (scope-loop-for-by local0 local (car more) (cdr more)))
     (t (scope-loop-for-and local rest)))))

(defun scope-loop-for-being-the-hash-keys-of-using (local form rest)
  (let* ((var (cadr form))
         (bare (scope-sym-bare var))
         (beg (scope-sym-pos var)))
    (when beg
      (scope-report 'variable beg (length (symbol-name bare)) beg))
    (scope-loop-for-and (scope-local-new bare beg local) rest)))

(defun scope-loop-for-being-the-hash-keys-of (local0 local expr rest)
  (scope-1 local0 expr)
  (when-let ((bare (scope-sym-bare (car rest)))
             (more (cdr rest)))
    (cond
     ((eq bare 'using)
      (scope-loop-for-being-the-hash-keys-of-using local (car more) (cdr more)))
     (t (scope-loop-for-and local rest)))))

(defun scope-loop-for-being-the-hash-keys (local0 local word rest)
  (when-let ((bare (scope-sym-bare word)))
    (cond
     ((eq bare 'of)
      (scope-loop-for-being-the-hash-keys-of local0 local (car rest) (cdr rest))))))

(defun scope-loop-for-being-the (local0 local word rest)
  (when-let ((bare (scope-sym-bare word)))
    (cond
     ((memq bare '(buffer buffers))
      (scope-loop-for-and local rest))
     ((memq bare '( hash-key hash-keys
                    hash-value hash-values
                    key-code key-codes
                    key-binding key-bindings))
      (scope-loop-for-being-the-hash-keys local0 local (car rest) (cdr rest))))))

(defun scope-loop-for-being (local0 local next rest)
  (scope-loop-for-being-the
   local0 local (car rest)
   (if (memq (scope-sym-bare next) '(the each)) (cdr rest) rest)))

(defun scope-loop-for (local0 local vars rest)
  (if vars
      ;; FIXME: var need not be a symbol, see
      ;; `cl-macs-loop-destructure-cons' test in cl-macs-tests.el.
      (let* ((var (car (ensure-list vars)))
             (bare (bare-symbol var))
             (beg (scope-sym-pos var)))
        (when beg
          (scope-report 'variable beg (length (symbol-name bare)) beg))
        (scope-loop-for local0 (scope-local-new bare beg local) (cdr-safe vars) rest))
    (when-let ((bare (scope-sym-bare (car rest)))
               (more (cdr rest)))
      (cond
       ((memq bare '(from upfrom downfrom))
        (scope-loop-for-from local0 local (car more) (cdr more)))
       ((memq bare '( to upto downto below above
                      in on in-ref))
        (scope-loop-for-to local0 local (car more) (cdr more)))
       ((memq bare '(by
                     across across-ref))
        (scope-loop-for-by local0 local (car more) (cdr more)))
       ((eq bare '=)
        (scope-loop-for-= local0 local (car more) (cdr more)))
       ((eq bare 'being)
        (scope-loop-for-being local0 local (car more) (cdr more)))))))

(defun scope-loop-repeat (local form rest)
  (scope-1 local form)
  (scope-loop local rest))

(defvar scope-loop-into-vars nil)

(defun scope-loop-collect (local expr rest)
  (scope-1 local expr)
  (let ((bw (scope-sym-bare (car rest)))
        (more (cdr rest)))
    (if (eq bw 'into)
        (let* ((var (car more))
               (bare (scope-sym-bare var))
               (beg (scope-sym-pos var)))
          (if (memq bare scope-loop-into-vars)
              (progn
                (scope-s local var)
                (scope-loop local (cdr more)))
            (when beg
              (scope-report 'variable
                            beg (length (symbol-name bare)) beg))
            (let ((scope-loop-into-vars (cons bare scope-loop-into-vars)))
              (scope-loop (scope-local-new bare beg local) (cdr more)))))
      (scope-loop local rest))))

(defun scope-loop-with-and (local rest)
  (if (eq (scope-sym-bare (car rest)) 'and)
      (scope-loop-with local (cadr rest) (cddr rest))
    (scope-loop local rest)))

(defun scope-loop-with (local var rest)
  (let* ((bare (scope-sym-bare var))
         (beg (symbol-with-pos-pos var))
         (l (scope-local-new bare beg local))
         (eql (car rest)))
    (when beg
      (scope-report 'variable beg (length (symbol-name bare)) beg))
    (if (eq (scope-sym-bare eql) '=)
        (let* ((val (cadr rest)) (more (cddr rest)))
          (scope-1 local val)
          (scope-loop-with-and l more))
      (scope-loop-with-and l rest))))

(defun scope-loop-do (local form rest)
  (scope-1 local form)
  (if (consp (car rest))
      (scope-loop-do local (car rest) (cdr rest))
    (scope-loop local rest)))

(defun scope-loop-named (local name rest)
  (let* ((beg (scope-sym-pos name))
         (bare (scope-sym-bare name)))
    (when beg
      (scope-report 'block beg (length (symbol-name bare)) beg))
    (let ((scope-block-alist (scope-local-new bare beg scope-block-alist)))
      (scope-loop local rest))))

(defun scope-loop-finally (local next rest)
  (if-let ((bare (scope-sym-bare next)))
      (cond
       ((eq bare 'do)
        (scope-loop-do local (car rest) (cdr rest)))
       ((eq bare 'return)
        (scope-1 local (car rest))
        (scope-loop local (cdr rest))))
    (if (eq (scope-sym-bare (car-safe next)) 'return)
        (progn
          (scope-1 local (cadr next))
          (scope-loop local (cdr rest)))
      (scope-loop-do local next rest))))

(defun scope-loop-initially (local next rest)
  (if (eq (scope-sym-bare next) 'do)
      (scope-loop-do local (car rest) (cdr rest))
    (scope-loop-do local next rest)))

(defvar scope-loop-if-depth 0)

(defun scope-loop-if (local keyword condition rest)
  (scope-1 local condition)
  (let ((scope-loop-if-depth (1+ scope-loop-if-depth)))
    (scope-loop
     ;; `if' binds `it'.
     (scope-local-new 'it (scope-sym-pos keyword) local)
     rest)))

(defun scope-loop-end (local rest)
  (let ((scope-loop-if-depth (1- scope-loop-if-depth)))
    (unless (minusp scope-loop-if-depth)
      (scope-loop local rest))))

(defun scope-loop-and (local rest)
  (when (plusp scope-loop-if-depth) (scope-loop local rest)))

(defun scope-loop (local forms)
  (when forms
    (let* ((kw (car forms))
           (bare (scope-sym-bare kw))
           (rest (cdr forms)))
      (cond
       ((memq bare '(for as))
        (scope-loop-for local local (car rest) (cdr rest)))
       ((memq bare '( repeat while until always never thereis iter-by
                      return))
        (scope-loop-repeat local (car rest) (cdr rest)))
       ((memq bare '(collect append nconc concat vconcat count sum maximize minimize))
        (scope-loop-collect local (car rest) (cdr rest)))
       ((memq bare '(with))
        (scope-loop-with local (car rest) (cdr rest)))
       ((memq bare '(do)) (scope-loop-do local (car rest) (cdr rest)))
       ((memq bare '(named)) (scope-loop-named local (car rest) (cdr rest)))
       ((memq bare '(finally)) (scope-loop-finally local (car rest) (cdr rest)))
       ((memq bare '(initially)) (scope-loop-initially local (car rest) (cdr rest)))
       ((memq bare '(if when unless)) (scope-loop-if local kw (car rest) (cdr rest)))
       ((memq bare '(end)) (scope-loop-end local rest))
       ((memq bare '(and else)) (scope-loop-and local rest))))))

(defun scope-named-let (local name bindings body)
  (let ((bare (scope-sym-bare name))
        (beg (scope-sym-pos name)))
    (when beg
      (scope-report 'function beg (length (symbol-name bare)) beg))
    (dolist (binding bindings)
      (let* ((sym (car (ensure-list binding)))
             (beg (symbol-with-pos-pos sym))
             (bare (bare-symbol sym)))
        (when beg
          (scope-report 'variable beg (length (symbol-name bare)) beg))
        (scope-1 local (cadr binding))))
    (let ((l local))
      (dolist (binding bindings)
        (when-let ((sym (car (ensure-list binding)))
                   (bare (scope-sym-bare sym)))
          (setq l (scope-local-new bare (scope-sym-pos sym) l))))
      (let ((scope-flet-alist (scope-local-new bare beg scope-flet-alist)))
        (scope-n l body)))))

(defun scope-with-slots (local spec-list object body)
  (scope-1 local object)
  (scope-let local spec-list body))

(defun scope-rx (local regexps)
  (dolist (regexp regexps) (scope-rx-1 local regexp)))

(defvar scope-rx-alist nil)

(defun scope-rx-1 (local regexp)
  (if (consp regexp)
      (let* ((head (car regexp))
             (bare (scope-sym-bare head)))
        (when (and bare (symbol-with-pos-p head))
          (scope-report 'rx-construct
                        (symbol-with-pos-pos head) (length (symbol-name bare))
                        (alist-get bare scope-rx-alist)))
        (cond
         ((memq bare '(literal regex regexp eval))
          (scope-1 local (cadr regexp)))
         ((memq bare '( seq sequence and :
                        or |
                        zero-or-more 0+ * *?
                        one-or-more 1+ + +?
                        zero-or-one optional opt \? \??
                        = >= ** repeat
                        minimal-match maximal-match
                        group submatch
                        group-n submatch-n))
          (scope-rx local (cdr regexp)))))
    (when-let (((symbol-with-pos-p regexp))
               (bare (scope-sym-bare regexp)))
      (scope-report 'rx-construct
                    (symbol-with-pos-pos regexp) (length (symbol-name bare))
                    (alist-get bare scope-rx-alist)))))

(defun scope-rx-define (local name rest)
  (when-let ((bare (scope-sym-bare name)))
    (scope-report 'rx-construct
                  (symbol-with-pos-pos name) (length (symbol-name bare)) nil))
  (if (not (cdr rest))
      (scope-rx-1 local (car rest))
    (let ((l scope-rx-alist)
          (args (car rest))
          (rx (cadr rest)))
      (dolist (arg args)
        (and (symbol-with-pos-p arg)
             (let* ((beg (symbol-with-pos-pos arg))
                    (bare (bare-symbol arg))
                    (len (length (symbol-name bare))))
               (when beg
                 (if (memq (bare-symbol arg) '(&optional &rest _))
                     (scope-report 'ampersand beg len)
                   (scope-report 'rx-construct beg len beg))))))
      (dolist (arg args)
        (when-let ((bare (bare-symbol arg))
                   (beg (scope-sym-pos arg)))
          (unless (memq bare '(&optional &rest))
            (setq l (scope-local-new bare beg l)))))
      (let ((scope-rx-alist l))
        (scope-rx-1 local rx)))))

(defun scope-rx-let (local bindings body)
  (if-let ((binding (car bindings)))
      (let ((name (car binding)) (rest (cdr binding)))
        (when-let ((bare (scope-sym-bare name))
                   (beg (symbol-with-pos-pos name)))
          (scope-report 'rx-construct
                        beg (length (symbol-name bare)) beg))
        (if (cdr rest)
            (let ((l scope-rx-alist)
                  (args (car rest))
                  (rx (cadr rest)))
              (dolist (arg args)
                (and (symbol-with-pos-p arg)
                     (let* ((beg (symbol-with-pos-pos arg))
                            (bare (bare-symbol arg))
                            (len (length (symbol-name bare))))
                       (when beg
                         (if (memq (bare-symbol arg) '(&optional &rest _))
                             (scope-report 'ampersand beg len)
                           (scope-report 'rx-construct beg len beg))))))
              (dolist (arg args)
                (when-let ((bare (bare-symbol arg))
                           (beg (scope-sym-pos arg)))
                  (unless (memq bare '(&optional &rest))
                    (setq l (scope-local-new bare beg l)))))
              (let ((scope-rx-alist l))
                (scope-rx-1 local rx))
              (let ((scope-rx-alist (scope-local-new (scope-sym-bare name)
                                                     (scope-sym-pos name)
                                                     scope-rx-alist)))
                (scope-rx-let local (cdr bindings) body)))
          (scope-rx-1 local (car rest))
          (let ((scope-rx-alist (scope-local-new (scope-sym-bare name)
                                                 (scope-sym-pos name)
                                                 scope-rx-alist)))
            (scope-rx-let local (cdr bindings) body))))
    (scope-n local body)))

(defun scope-gv-define-expander (local name handler)
  (when-let* ((beg (scope-sym-pos name)) (bare (scope-sym-bare name)))
    (scope-report 'defun beg (length (symbol-name bare))))
  (scope-1 local handler))

(defun scope-gv-define-simple-setter (local name setter rest)
  (when-let* ((beg (scope-sym-pos name)) (bare (scope-sym-bare name)))
    (scope-report 'defun beg (length (symbol-name bare))))
  (when-let* ((beg (scope-sym-pos setter)) (bare (scope-sym-bare setter)))
    (scope-report 'function beg (length (symbol-name bare))))
  (scope-n local rest))

(defun scope-catch (local tag body)
  (when-let* (((memq (car-safe tag) '(quote \`)))
              (sym (cadr tag))
              (beg (scope-sym-pos sym))
              (bare (scope-sym-bare sym)))
    (scope-report 'throw-tag beg (length (symbol-name bare))))
  (scope-n local body))

(defun scope-face (face)
  (if (or (scope-sym-bare face)
          (keywordp (scope-sym-bare (car-safe face))))
      (scope-face-1 face)
    (mapc #'scope-face-1 face)))

(defun scope-face-1 (face)
  (cond
   ((symbol-with-pos-p face)
    (when-let ((beg (scope-sym-pos face)) (bare (scope-sym-bare face)))
      (scope-report 'face beg (length (symbol-name bare)))))
   ((keywordp (scope-sym-bare (car-safe face)))
    (let ((l face))
      (while l
        (let ((kw (car l))
              (vl (cadr l)))
          (setq l (cddr l))
          (when-let ((bare (scope-sym-bare kw))
                     ((keywordp bare)))
            (when-let ((beg (scope-sym-pos kw))
                       (len (length (symbol-name bare))))
              (scope-report 'constant beg len))
            (when (eq bare :inherit)
              (when-let ((beg (scope-sym-pos vl)) (fbare (scope-sym-bare vl)))
                (scope-report 'face beg (length (symbol-name fbare))))))))))))

(defun scope-deftype (local name args body)
  (when-let* ((beg (scope-sym-pos name)) (bare (scope-sym-bare name)))
    (scope-report 'type beg (length (symbol-name bare))))
  (scope-lambda local args body))

(defun scope-widget-type (_local form)
  (when-let (((memq (scope-sym-bare (car-safe form)) '(quote \`)))
             (type (cadr form)))
    (scope-widget-type-1 type)))

(defun scope-widget-type-1 (type)
  (cond
   ((symbol-with-pos-p type)
    (when-let* ((beg (scope-sym-pos type)) (bare (scope-sym-bare type)))
      (scope-report 'widget-type
                    (symbol-with-pos-pos type)
                    (length (symbol-name (bare-symbol type))))))
   ((consp type)
    (let ((head (car type)))
      (when-let ((beg (scope-sym-pos head)) (bare (scope-sym-bare head)))
        (scope-report 'widget-type beg (length (symbol-name bare))))
      (when-let ((bare (scope-sym-bare head)))
        (scope-widget-type-arguments bare (cdr type)))))))

(defun scope-widget-type-keyword-arguments (head kw args)
  (when-let ((beg (scope-sym-pos kw))
             (len (length (symbol-name (bare-symbol kw)))))
    (scope-report 'constant beg len))
  (cond
   ((and (memq head '(plist alist))
         (memq kw   '(:key-type :value-type)))
    (scope-widget-type-1 (car args)))
   ((memq kw '(:action :match :match-inline :validate))
    (when-let* ((fun (car args))
                (beg (scope-sym-pos fun))
                (bare (scope-sym-bare fun)))
      (scope-report 'function beg (length (symbol-name bare)))))
   ((memq kw '(:args))
    (mapc #'scope-widget-type-1 (car args))))
  ;; TODO: (restricted-sexp :match-alternatives CRITERIA)
  (scope-widget-type-arguments head (cdr args)))

(defun scope-widget-type-arguments (head args)
  (let* ((arg (car args))
         (bare (scope-sym-bare arg)))
    (if (keywordp bare)
        (scope-widget-type-keyword-arguments head bare (cdr args))
      (scope-widget-type-arguments-1 head args))))

(defun scope-widget-type-arguments-1 (head args)
  (cl-case head
    ((list cons group vector choice radio set repeat checklist)
     (mapc #'scope-widget-type-1 args))
    ((function-item)
     (when-let* ((fun (car args))
                 (beg (scope-sym-pos fun))
                 (bare (scope-sym-bare fun)))
       (scope-report 'function beg (length (symbol-name bare)))))
    ((variable-item)
     (when-let* ((var (car args))
                 (beg (scope-sym-pos var))
                 (bare (scope-sym-bare var)))
       (scope-report 'variable beg (length (symbol-name bare)))))))

(defun scope-quoted-group (_local sym-form)
  (when-let* (((eq (scope-sym-bare (car-safe sym-form)) 'quote))
              (sym (cadr sym-form))
              (beg (scope-sym-pos sym))
              (bare (scope-sym-bare sym)))
    (scope-report 'group beg (length (symbol-name bare)))))

(defun scope-defmethod-1 (local0 local args body)
  (if args
      (let ((arg (car args)) (bare nil))
        (cond
         ((consp arg)
          (let* ((var (car arg))
                 (spec (cadr arg)))
            (cond
             ((setq bare (scope-sym-bare var))
              (when-let* ((beg (scope-sym-pos var))
                          (len (length (symbol-name bare))))
                (scope-report 'variable beg len beg))
              (cond
               ((consp spec)
                (let ((head (car spec)) (form (cadr spec)))
                  (and (eq 'eql (scope-sym-bare head))
                       (not (or (symbolp form) (symbol-with-pos-p form)))
                       (scope-1 local0 form))))
               ((symbol-with-pos-p spec)
                (when-let* ((beg (symbol-with-pos-pos spec))
                            (bare (bare-symbol spec))
                            (len (length (symbol-name bare))))
                  (scope-report 'type beg len))))
              (scope-defmethod-1
               local0 (scope-local-new bare (scope-sym-pos var) local)
               (cdr args) body)))))
         ((setq bare (scope-sym-bare arg))
          (cond
           ((memq bare '(&optional &rest &body _))
            (when-let ((beg (scope-sym-pos arg)))
              (scope-report 'ampersand beg (length (symbol-name bare))))
            (scope-defmethod-1 local0 local (cdr args) body))
           ((eq bare '&context)
            (let* ((expr-type (cadr args))
                   (expr (car expr-type))
                   (spec (cadr expr-type))
                   (more (cddr args)))
              (when-let ((beg (scope-sym-pos arg)))
                (scope-report 'ampersand beg (length (symbol-name bare))))
              (scope-1 local0 expr)
              (cond
               ((consp spec)
                (let ((head (car spec)) (form (cadr spec)))
                  (and (eq 'eql (scope-sym-bare head))
                       (not (or (symbolp form) (symbol-with-pos-p form)))
                       (scope-1 local0 form))))
               ((symbol-with-pos-p spec)
                (when-let* ((beg (symbol-with-pos-pos spec))
                            (bare (bare-symbol spec))
                            (len (length (symbol-name bare))))
                  (scope-report 'type beg len beg))))
              (scope-defmethod-1 local0 local more body)))
           (t
            (when-let* ((beg (scope-sym-pos arg))
                        (len (length (symbol-name bare))))
              (scope-report 'variable beg len beg))
            (scope-defmethod-1
             local0 (scope-local-new bare (scope-sym-pos arg) local)
             (cdr args) body))))))
    (scope-n local body)))

;; (defun scope-defmethod (local name rest)
;;   (when (and (symbol-with-pos-p (car rest))
;;              (eq (bare-symbol (car rest)) :extra))
;;     (setq rest (cddr rest)))
;;   (when (and (symbol-with-pos-p (car rest))
;;              (memq (bare-symbol (car rest)) '(:before :after :around)))
;;     (setq rest (cdr rest)))
;;   (scope-defmethod-1 local local name (car rest)
;;                      (if (stringp (cadr rest)) (cddr rest) (cdr rest))))

(defun scope-defmethod (local name rest)
  "Analyze method definition for NAME with args REST in LOCAL context."
  (when-let* ((beg (scope-sym-pos name)) (bare (scope-sym-bare name)))
    (scope-report 'defun beg (length (symbol-name bare))))
  ;; [EXTRA]
  (when (eq (scope-sym-bare (car rest)) :extra)
    (scope-s local (car rest))
    (setq rest (cddr rest)))
  ;; [QUALIFIER]
  (when (keywordp (scope-sym-bare (car rest)))
    (scope-s local (car rest))
    (setq rest (cdr rest)))
  ;; ARGUMENTS
  (scope-defmethod-1 local local (car rest) (cdr rest)))

(defun scope-cl-defun (local name arglist body)
  (let ((beg (scope-sym-pos name))
        (bare (scope-sym-bare name)))
    (when beg (scope-report 'defun beg (length (symbol-name bare))))
    (let ((scope-block-alist (scope-local-new bare beg scope-block-alist)))
      (scope-cl-lambda local arglist body))))

(defun scope-cl-lambda (local arglist body)
  (scope-cl-lambda-1 local arglist nil body))

(defun scope-cl-lambda-1 (local arglist more body)
  (cond
   (arglist
    (if (consp arglist)
        (let ((head (car arglist)))
          (if (consp head)
              (scope-cl-lambda-1 local head (cons (cdr arglist) more) body)
            (let ((bare (scope-sym-bare head)))
              (if (memq bare '(&optional &rest &body &key &aux &whole &cl-defs &cl-quote))
                  (progn
                    (when-let ((beg (scope-sym-pos head)))
                      (scope-report 'ampersand beg (length (symbol-name bare))))
                    (cl-case bare
                      (&optional (scope-cl-lambda-optional local (cadr arglist) (cddr arglist) more body))
                      (&cl-defs (scope-cl-lambda-defs local (cadr arglist) (cddr arglist) more body))
                      ((&rest &body) (scope-cl-lambda-rest local (cadr arglist) (cddr arglist) more body))
                      (&key (scope-cl-lambda-key local (cadr arglist) (cddr arglist) more body))
                      (&aux (scope-cl-lambda-aux local (cadr arglist) (cddr arglist) more body))
                      (&whole (scope-cl-lambda-1 local (cdr arglist) more body))))
                (when-let ((beg (scope-sym-pos head)))
                  (scope-report 'variable beg (length (symbol-name bare)) beg))
                (scope-cl-lambda-1 (scope-local-new bare (scope-sym-pos head) local)
                                   (cdr arglist) more body)))))
      (scope-cl-lambda-1 local (list '&rest arglist) more body)))
   (more (scope-cl-lambda-1 local (car more) (cdr more) body))
   (t (scope-lambda local nil body))))


(defun scope-cl-lambda-defs (local arg arglist more body)
  (when (consp arg)
    (let ((def (car arg))
          (defs (cdr arg)))
      (scope-1 local def)
      (dolist (d defs) (scope-n local (cdr-safe d)))))
  (scope-cl-lambda-1 local arglist more body))

(defun scope-cl-lambda-optional (local arg arglist more body)
  (let* ((a (ensure-list arg))
         (var (car a))
         (l local)
         (init (cadr a))
         (svar (caddr a)))
    (scope-1 local init)
    (if (consp var)
        (scope-cl-lambda-1 l var (cons (append (when svar (list svar))
                                               (cons '&optional arglist))
                                       more)
                           body)
      (when-let ((bare (scope-sym-bare svar)))
        (when-let ((beg (scope-sym-pos svar)))
          (scope-report 'variable beg (length (symbol-name bare)) beg))
        (setq l (scope-local-new bare (scope-sym-pos svar) l)))
      (when-let ((bare (scope-sym-bare var)))
        (when-let ((beg (scope-sym-pos var)))
          (scope-report 'variable beg (length (symbol-name bare)) beg))
        (setq l (scope-local-new bare (scope-sym-pos var) l)))
      (cond
       (arglist
        (let ((head (car arglist)))
          (if-let ((bare (scope-sym-bare head))
                   ((memq bare '(&rest &body &key &aux))))
              (progn
                (when-let ((beg (scope-sym-pos head)))
                  (scope-report 'ampersand beg (length (symbol-name bare))))
                (cl-case bare
                  ((&rest &body) (scope-cl-lambda-rest l (cadr arglist) (cddr arglist) more body))
                  (&key (scope-cl-lambda-key l (cadr arglist) (cddr arglist) more body))
                  (&aux (scope-cl-lambda-aux l (cadr arglist) (cddr arglist) more body))))
            (scope-cl-lambda-optional l head (cdr arglist) more body))))
       (more (scope-cl-lambda-1 l (car more) (cdr more) body))
       (t (scope-lambda l nil body))))))

(defun scope-cl-lambda-rest (local var arglist more body)
  (let* ((l local))
    (if (consp var)
        (scope-cl-lambda-1 l var (cons arglist more) body)
      (when-let ((bare (scope-sym-bare var)))
        (when-let ((beg (scope-sym-pos var)))
          (scope-report 'variable beg (length (symbol-name bare)) beg))
        (setq l (scope-local-new bare (scope-sym-pos var) l)))
      (cond
       (arglist
        (let ((head (car arglist)))
          (if-let ((bare (scope-sym-bare head))
                   ((memq bare '(&key &aux))))
              (progn
                (when-let ((beg (scope-sym-pos head)))
                  (scope-report 'ampersand beg (length (symbol-name bare))))
                (cl-case bare
                  (&key (scope-cl-lambda-key l (cadr arglist) (cddr arglist) more body))
                  (&aux (scope-cl-lambda-aux l (cadr arglist) (cddr arglist) more body))))
            (scope-cl-lambda-1 l (car more) (cdr more) body))))
       (more (scope-cl-lambda-1 l (car more) (cdr more) body))
       (t (scope-lambda l nil body))))))

(defun scope-cl-lambda-key (local arg arglist more body)
  (let* ((a (ensure-list arg))
         (var (car a))
         (l local)
         (init (cadr a))
         (svar (caddr a))
         (kw (car-safe var)))
    (scope-1 local init)
    (and kw (or (symbolp kw) (symbol-with-pos-p kw))
         (cadr var)
         (not (cddr var))
         ;; VAR is (KEYWORD VAR)
         (setq var (cadr var)))
    (when-let ((bare (scope-sym-bare kw))
               ((keywordp bare)))
      (when-let ((beg (scope-sym-pos kw)))
        (scope-report 'constant beg (length (symbol-name bare))))
      (setq l (scope-local-new bare (scope-sym-pos svar) l)))
    (if (consp var)
        (scope-cl-lambda-1 l var (cons (append (when svar (list svar))
                                               (cons '&key arglist))
                                       more)
                           body)
      (when-let ((bare (scope-sym-bare svar)))
        (when-let ((beg (scope-sym-pos svar)))
          (scope-report 'variable beg (length (symbol-name bare)) beg))
        (setq l (scope-local-new bare (scope-sym-pos svar) l)))
      (when-let ((bare (scope-sym-bare var)))
        (when-let ((beg (scope-sym-pos var)))
          (scope-report 'variable beg (length (symbol-name bare)) beg))
        (setq l (scope-local-new bare (scope-sym-pos var) l)))
      (cond
       (arglist
        (let ((head (car arglist)))
          (if-let ((bare (scope-sym-bare head))
                   ((memq bare '(&aux &allow-other-keys))))
              (progn
                (when-let ((beg (scope-sym-pos head)))
                  (scope-report 'ampersand beg (length (symbol-name bare))))
                (cl-case bare
                  (&aux (scope-cl-lambda-aux l (cadr arglist) (cddr arglist) more body))
                  (&allow-other-keys (scope-cl-lambda-1 l (car more) (cdr more) body))))
            (scope-cl-lambda-key l head (cdr arglist) more body))))
       (more (scope-cl-lambda-1 l (car more) (cdr more) body))
       (t (scope-lambda l nil body))))))

(defun scope-cl-lambda-aux (local arg arglist more body)
  (let* ((a (ensure-list arg))
         (var (car a))
         (l local)
         (init (cadr a)))
    (scope-1 local init)
    (if (consp var)
        (scope-cl-lambda-1 l var (cons arglist more) body)
      (when-let ((bare (scope-sym-bare var)))
        (when-let ((beg (scope-sym-pos var)))
          (scope-report 'variable beg (length (symbol-name bare)) beg))
        (setq l (scope-local-new bare (scope-sym-pos var) l)))
      (cond
       (arglist (scope-cl-lambda-aux l (car arglist) (cdr arglist) more body))
       (more (scope-cl-lambda-1 l (car more) (cdr more) body))
       (t (scope-lambda l nil body))))))

(defvar scope-macrolet-alist nil)

(defun scope-cl-macrolet (local bindings body)
  (if-let ((b (car bindings)))
      (let ((name (car b))
            (arglist (cadr b))
            (mbody (cddr b)))
        (scope-cl-lambda local arglist mbody)
        (when-let ((bare (scope-sym-bare name)))
          (when-let ((beg (scope-sym-pos name)))
            (scope-report 'macro beg (length (symbol-name bare)) beg))
          (let ((scope-macrolet-alist (scope-local-new bare (scope-sym-pos name) scope-macrolet-alist)))
            (scope-cl-macrolet local (cdr bindings) body))))
    (scope-n local body)))

(defun scope-define-minor-mode (local mode _doc body)
  (let ((explicit-var nil) (command t))
    (while-let ((kw (car-safe body))
                (bkw (scope-sym-bare kw))
                ((keywordp bkw)))
      (when-let ((beg (scope-sym-pos kw)))
        (scope-report 'constant beg (length (symbol-name bkw))))
      (cl-case bkw
        ((:init-value :keymap :after-hook :initialize)
         (scope-1 local (cadr body)))
        (:lighter (scope-mode-line-construct local (cadr body)))
        ((:interactive)
         (let ((val (cadr body)))
           (when (consp val) (mapc #'scope-major-mode-name val))
           (setq command val)))
        ((:variable)
         (let* ((place (cadr body))
                (tail (cdr-safe place)))
           (if (and tail (let ((symbols-with-pos-enabled t))
                           (or (symbolp tail) (functionp tail))))
               (progn
                 (scope-1 local (car place))
                 (scope-sharpquote local tail))
             (scope-1 local place)))
         (setq explicit-var t))
        ((:group)
         (scope-quoted-group local (cadr body)))
        ((:predicate)                   ;For globalized minor modes.
         (scope-global-minor-mode-predicate (cadr body)))
        ((:on :off)
         (let ((obod (cdr body)))
           (while (and obod (not (keywordp (scope-sym-bare (car obod)))))
             (scope-1 local (pop obod)))
           (setq body (cons bkw (cons nil obod))))))
      (setq body (cddr body)))
    (when-let ((bare (scope-sym-bare mode)) (beg (scope-sym-pos mode))
               (typ (if command 'defcmd 'defun)))
      (scope-report typ beg (length (symbol-name bare)))
      (unless explicit-var
        (scope-report 'defvar beg (length (symbol-name bare)))))
    (scope-n local body)))

(defun scope-global-minor-mode-predicate (pred)
  (if (consp pred)
      (if (eq 'not (scope-sym-bare (car pred)))
          (mapc #'scope-global-minor-mode-predicate (cdr pred))
        (mapc #'scope-global-minor-mode-predicate pred))
    (scope-major-mode-name pred)))

(defun scope-major-mode-name (mode)
  (when-let* ((beg (scope-sym-pos mode))
              (bare (bare-symbol mode))
              ((not (booleanp bare)))
              (len (length (symbol-name bare))))
    (scope-report 'major-mode beg len)))

(defun scope-mode-line-construct (_local format)
  (scope-mode-line-construct-1 format))

(defun scope-mode-line-construct-1 (format)
  (cond
   ((symbol-with-pos-p format)
    (scope-report 'variable
                  (symbol-with-pos-pos format)
                  (length (symbol-name (bare-symbol format)))))
   ((consp format)
    (let ((head (car format)))
      (cond
       ((or (stringp head) (consp head) (integerp head))
        (mapc #'scope-mode-line-construct-1 format))
       ((or (symbolp head) (symbol-with-pos-p head))
        (scope-s nil head)
        (cl-case (bare-symbol head)
          (:eval
           (scope-1 nil (cadr format)))
          (:propertize
           (scope-mode-line-construct-1 (cadr format))
           (when-let* ((props (cdr format))
                       (symbols-with-pos-enabled t)
                       (val-form (plist-get props 'face)))
             (scope-face-1 val-form)))
          (otherwise
           (scope-mode-line-construct-1 (cadr format))
           (scope-mode-line-construct-1 (caddr format))))))))))

(defvar scope-safe-macros t
  "Specify which macros are safe to expand.

If this is t, macros are considered safe by default.  Otherwise, this is
a (possibly empty) list of safe macros.")

(defvar scope-unsafe-macros
  '( static-if cl-eval-when eval-when-compile eval-and-compile let-when-compile
     rx cl-macrolet nnoo-define-basics))

(defun scope-safe-macro-p (macro)
  (and (not (memq macro scope-unsafe-macros))
       (or (eq scope-safe-macros t)
           (memq macro scope-safe-macros)
           (get macro 'safe-macro)
           (trusted-content-p))))

(defvar warning-minimum-log-level)

(defmacro scope-define-analyzer (fsym args &rest body)
  (declare (indent defun))
  (let ((analyzer (intern (concat "scope--analyze-" (symbol-name fsym)))))
    `(progn
       (defun ,analyzer ,args ,@body)
       (put ',fsym 'scope-analyzer #',analyzer))))

(defmacro scope--define-function-analyzer (fsym args type &rest body)
  (declare (indent defun))
  (let* ((helper (intern (concat "scope--analyze-" (symbol-name fsym) "-1"))))
    `(progn
       (defun ,helper ,args ,@body)
       (scope-define-analyzer ,fsym (l f &rest args)
         (scope-report-s f ',type)
         (apply #',helper args)
         (scope-n l args)))))

(defmacro scope-define-function-analyzer (fsym args &rest body)
  (declare (indent defun))
  `(scope--define-function-analyzer ,fsym ,args function ,@body)
  ;; (let* ((helper (intern (concat "scope--analyze-" (symbol-name fsym) "-1"))))
  ;;   `(progn
  ;;      (defun ,helper ,args ,@body)
  ;;      (scope-define-analyzer ,fsym (l f &rest args)
  ;;        (scope-report-s f 'function)
  ;;        (apply #',helper args)
  ;;        (scope-n l args))))
  )

(defmacro scope-define-macro-analyzer (fsym args &rest body)
  (declare (indent defun))
  (let* ((helper (intern (concat "scope--analyze-" (symbol-name fsym) "-1"))))
    `(progn
       (defun ,helper ,args ,@body)
       (scope-define-analyzer ,fsym (l f &rest args)
         (scope-report-s f 'macro)
         (apply #',helper l args)))))

(defmacro scope-define-special-form-analyzer (fsym args &rest body)
  (declare (indent defun))
  (let* ((helper (intern (concat "scope--analyze-" (symbol-name fsym) "-1"))))
    `(progn
       (defun ,helper ,args ,@body)
       (scope-define-analyzer ,fsym (l f &rest args)
         (scope-report-s f 'macro)
         (apply #',helper l args)))))

(defun scope--unqoute (form)
  (when (memq (scope-sym-bare (car-safe form)) '(quote function \`))
    (cadr form)))

(scope-define-analyzer with-suppressed-warnings (l f warnings &rest body)
  (scope-report-s f 'macro)
  (dolist (warning warnings)
    (when-let* ((wsym (car-safe warning)))
      (scope-report-s wsym 'warning-type)))
  (scope-n l body))

(scope-define-analyzer eval (l f form &optional lexical)
  (scope-report-s f 'function)
  (if-let ((quoted (scope--unqoute form)))
      (scope-1 l quoted)
    (scope-1 l form))
  (scope-1 l lexical))

(scope-define-function-analyzer defalias (sym _def &optional _docstring)
  (when-let ((quoted (scope--unqoute sym))) (scope-report-s quoted 'defun)))

(scope-define-function-analyzer oclosure--define
  (&optional name _docstring parent-names _slots &rest props)
  (when-let ((quoted (scope--unqoute name))) (scope-report-s quoted 'defoclosure))
  (when-let ((qs (scope--unqoute parent-names)))
    (dolist (q qs)
      (scope-report-s q 'oclosure)))
  (while-let ((kw (car-safe props))
              (bkw (scope-sym-bare kw))
              ((keywordp bkw)))
    (scope-report-s kw 'constant)
    (cl-case bkw
      (:predicate
       (when-let ((q (scope--unqoute (cadr props)))) (scope-report-s q 'defun))))
    (setq props (cddr props))))

(scope-define-function-analyzer define-charset
  (&optional name _docstring &rest _props)
  (when-let ((quoted (scope--unqoute name))) (scope-report-s quoted 'defcharset)))

(scope-define-function-analyzer define-charset-alias
  (&optional alias charset)
  (when-let ((quoted (scope--unqoute alias))) (scope-report-s quoted 'defcharset))
  (when-let ((quoted (scope--unqoute charset))) (scope-report-s quoted 'charset)))

(scope-define-function-analyzer charset-chars
  (&optional charset &rest _)
  (when-let ((quoted (scope--unqoute charset))) (scope-report-s quoted 'charset)))

(dolist (sym '(charset-description charset-info charset-iso-final-char
                                   charset-long-name charset-plist
                                   charset-short-name
                                   get-charset-property put-charset-property
                                   list-charset-chars
                                   set-charset-plist
                                   set-charset-priority
                                   unify-charset
                                   locale-charset-to-coding-system))
  (put sym 'scope-analyzer #'scope--analyze-charset-chars))

(scope-define-function-analyzer define-coding-system
  (&optional name _docstring &rest _props)
  (when-let ((quoted (scope--unqoute name))) (scope-report-s quoted 'defcoding)))

(scope-define-function-analyzer define-coding-system-alias
  (&optional alias coding-system)
  (when-let ((quoted (scope--unqoute alias))) (scope-report-s quoted 'defcoding))
  (when-let ((quoted (scope--unqoute coding-system))) (scope-report-s quoted 'coding)))

(scope-define-function-analyzer decode-coding-region
  (&optional _start _end coding-system &rest _)
  (when-let ((quoted (scope--unqoute coding-system))) (scope-report-s quoted 'coding)))

(put 'encode-coding-region 'scope-analyzer #'scope--analyze-decode-coding-region)

(scope-define-function-analyzer decode-coding-string
  (&optional _string coding-system &rest _)
  (when-let ((quoted (scope--unqoute coding-system))) (scope-report-s quoted 'coding)))

(dolist (sym '(encode-coding-char encode-coding-string))
  (put sym 'scope-analyzer #'scope--analyze-decode-coding-string))

(scope-define-function-analyzer coding-system-mnemonic
  (&optional coding-system &rest _)
  (when-let ((quoted (scope--unqoute coding-system))) (scope-report-s quoted 'coding)))

(dolist (sym '(add-to-coding-system-list
               check-coding-system
               coding-system-aliases
               coding-system-base
               coding-system-category
               coding-system-change-eol-conversion
               coding-system-change-text-conversion
               coding-system-charset-list
               coding-system-doc-string
               coding-system-eol-type
               coding-system-eol-type-mnemonic
               coding-system-get
               coding-system-plist
               coding-system-post-read-conversion
               coding-system-pre-write-conversion
               coding-system-put
               coding-system-translation-table-for-decode
               coding-system-translation-table-for-encode
               coding-system-type
               describe-coding-system
               prefer-coding-system
               print-coding-system
               print-coding-system-briefly
               revert-buffer-with-coding-system
               set-buffer-file-coding-system
               set-clipboard-coding-system
               set-coding-system-priority
               set-default-coding-systems
               set-file-name-coding-system
               set-keyboard-coding-system
               set-next-selection-coding-system
               set-selection-coding-system
               set-terminal-coding-system
               universal-coding-system-argument))
  (put sym 'scope-analyzer #'scope--analyze-coding-system-mnemonic))

(scope-define-function-analyzer thing-at-point (thing &optional _)
  (when-let ((quoted (scope--unqoute thing))) (scope-report-s quoted 'thing)))

(dolist (sym '( forward-thing
                beginning-of-thing
                end-of-thing
                bounds-of-thing-at-point))
  (put sym 'scope-analyzer #'scope--analyze-thing-at-point))

(scope-define-function-analyzer bounds-of-thing-at-mouse (_event thing)
  (when-let ((quoted (scope--unqoute thing))) (scope-report-s quoted 'thing)))

(scope-define-function-analyzer thing-at-mouse (_event thing &optional _)
  (when-let ((quoted (scope--unqoute thing))) (scope-report-s quoted 'thing)))

(scope-define-function-analyzer custom-declare-variable (sym _default _doc &rest args)
  (when-let ((quoted (scope--unqoute sym))) (scope-report-s quoted 'defvar))
  (while-let ((kw (car-safe args))
              (bkw (scope-sym-bare kw))
              ((keywordp bkw)))
    (cl-case bkw
      (:type
       (when-let ((quoted (scope--unqoute (cadr args)))) (scope-widget-type-1 quoted)))
      (:group
       (when-let ((quoted (scope--unqoute (cadr args)))) (scope-report-s quoted 'group))))
    (setq args (cddr args))))

(scope-define-function-analyzer custom-declare-group (sym _members _doc &rest args)
  (when-let ((quoted (scope--unqoute sym))) (scope-report-s quoted 'group))
  (while-let ((kw (car-safe args))
              (bkw (scope-sym-bare kw))
              ((keywordp bkw)))
    (cl-case bkw
      (:group
       (when-let ((quoted (scope--unqoute (cadr args)))) (scope-report-s quoted 'group))))
    (setq args (cddr args))))

(scope-define-function-analyzer custom-declare-face (face spec _doc &rest args)
  (when-let ((q (scope--unqoute face))) (scope-report-s q 'defface))
  (when-let ((q (scope--unqoute spec)))
    (when (consp q) (dolist (s q) (scope-face (cdr s)))))
  (while-let ((kw (car-safe args))
              (bkw (scope-sym-bare kw))
              ((keywordp bkw)))
    (cl-case bkw
      (:group
       (when-let ((q (scope--unqoute (cadr args)))) (scope-report-s q 'group))))
    (setq args (cddr args))))

(defun scope-typep (type)
  (cond
   ((or (symbolp type) (symbol-with-pos-p type))
    (unless (booleanp (scope-sym-bare type))
      (scope-report-s type 'type)))
   ((consp   type)
    (cond
     ((memq (scope-sym-bare (car type)) '(and or not))
      (mapc #'scope-typep (cdr type)))
     ((eq (scope-sym-bare (car type)) 'satisfies)
      (scope-report-s (cadr type) 'function))))))

(scope-define-function-analyzer cl-typep (_val type)
  (when-let ((q (scope--unqoute type)))
    (scope-typep q)))

(scope-define-function-analyzer pulse-momentary-highlight-region (_start _end &optional face)
  (when-let ((q (scope--unqoute face))) (scope-face q)))

(scope--define-function-analyzer throw (tag _value) non-local-exit
  (when-let ((q (scope--unqoute tag))) (scope-report-s q 'throw-tag)))

(scope--define-function-analyzer signal (error-symbol &optional _data) non-local-exit
  (when-let ((q (scope--unqoute error-symbol))) (scope-report-s q 'condition)))

(scope--define-function-analyzer kill-emacs                     (&rest _) non-local-exit)
(scope--define-function-analyzer abort-recursive-edit           (&rest _) non-local-exit)
(scope--define-function-analyzer top-level                      (&rest _) non-local-exit)
(scope--define-function-analyzer exit-recursive-edit            (&rest _) non-local-exit)
(scope--define-function-analyzer tty-frame-restack              (&rest _) non-local-exit)
(scope--define-function-analyzer error                          (&rest _) non-local-exit)
(scope--define-function-analyzer user-error                     (&rest _) non-local-exit)
(scope--define-function-analyzer minibuffer-quit-recursive-edit (&rest _) non-local-exit)
(scope--define-function-analyzer exit-minibuffer                (&rest _) non-local-exit)

(scope-define-function-analyzer run-hooks (&rest hooks)
  (dolist (hook hooks)
    (when-let ((q (scope--unqoute hook))) (scope-report-s q 'variable))))

(scope-define-function-analyzer fboundp (symbol)
  (when-let ((q (scope--unqoute symbol))) (scope-report-s q 'function)))

(scope-define-function-analyzer overlay-put (&optional _ov prop val)
  (when-let ((q (scope--unqoute prop))
             ((eq (scope-sym-bare q) 'face))
             (face (scope--unqoute val)))
    (scope-face face)))

(scope-define-function-analyzer add-face-text-property (&optional _start _end face &rest _)
  (when-let ((q (scope--unqoute face))) (scope-face q)))

(scope-define-function-analyzer boundp (var &rest _)
  (when-let ((q (scope--unqoute var))) (scope-report-s q 'variable)))

(dolist (sym '( set symbol-value define-abbrev-table
                special-variable-p local-variable-p
                local-variable-if-set-p add-variable-watcher
                get-variable-watchers remove-variable-watcher
                default-value set-default make-local-variable
                buffer-local-value add-to-list add-to-history find-buffer
                add-hook remove-hook run-hook-with-args run-hook-wrapped))
  (put sym 'scope-analyzer #'scope--analyze-boundp))

(scope-define-function-analyzer defvaralias (new base &optional _docstring)
  (when-let ((q (scope--unqoute new))) (scope-report-s q 'defvar))
  (when-let ((q (scope--unqoute base))) (scope-report-s q 'variable)))

(scope-define-function-analyzer define-error (name _message &optional parent)
  (when-let ((q (scope--unqoute name))) (scope-report-s q 'condition))
  (when-let ((q (scope--unqoute parent)))
    (dolist (p (ensure-list q)) (scope-report-s p 'condition))))

(scope-define-function-analyzer featurep (feature &rest _)
  (when-let ((q (scope--unqoute feature))) (scope-report-s q 'feature)))

(put 'provide 'scope-analyzer #'scope--analyze-featurep)
(put 'require 'scope-analyzer #'scope--analyze-featurep)

(scope-define-function-analyzer put-text-property (&optional _ _ prop val _)
  (when (memq (scope-sym-bare (scope--unqoute prop)) '(mouse-face face))
    (when-let ((q (scope--unqoute val))) (scope-face q))))

(put 'remove-overlays 'scope-analyzer #'scope--analyze-put-text-property)

(scope-define-function-analyzer propertize (_string &rest props)
  (while props
    (cl-case (scope-sym-bare (scope--unqoute (car props)))
      ((face mouse-face)
       (when-let ((q (scope--unqoute (cadr props)))) (scope-face q))))
    (setq props (cddr props))))

(scope-define-function-analyzer eieio-defclass-internal (name superclasses _ _)
  (when-let ((q (scope--unqoute name))) (scope-report-s q 'type))
  (when-let ((q (scope--unqoute superclasses)))
    (dolist (sup q) (scope-report-s sup 'type))))

(scope-define-function-analyzer cl-struct-define
  (name _doc parent _type _named _slots _children _tab _print)
  (when-let ((q (scope--unqoute name)))   (scope-report-s q 'type))
  (when-let ((q (scope--unqoute parent))) (scope-report-s q 'type)))

(scope-define-function-analyzer define-widget (name class _doc &rest args)
  (when-let ((q (scope--unqoute name)))  (scope-report-s q 'widget-type))
  (when-let ((q (scope--unqoute class))) (scope-report-s q 'widget-type))
  (while-let ((kw (car-safe args))
              (bkw (scope-sym-bare kw))
              ((keywordp bkw)))
    (cl-case bkw
      (:type
       (when-let ((q (scope--unqoute (cadr args)))) (scope-widget-type-1 q)))
      (:args
       (when-let ((q (scope--unqoute (cadr args)))) (mapc #'scope-widget-type-1 q))))
    (setq args (cddr args))))

(scope-define-function-analyzer provide-theme (name &rest _)
  (when-let ((q (scope--unqoute name))) (scope-report-s q 'theme)))

(put 'custom-declare-theme 'scope-analyzer #'scope--analyze-provide-theme)

(scope-define-function-analyzer eieio-oref (_obj slot)
  (when-let ((q (scope--unqoute slot))) (scope-report-s q 'slot)))

(dolist (fun '(slot-boundp slot-makeunbound slot-exists-p eieio-oref-default))
  (put fun 'scope-analyzer #'scope--analyze-eieio-oref))

(scope-define-function-analyzer eieio-oset (_obj slot _value)
  (when-let ((q (scope--unqoute slot))) (scope-report-s q 'slot)))

(put 'eieio-oset-default 'scope-analyzer #'scope--analyze-eieio-oset)

(scope-define-function-analyzer derived-mode-p (modes &rest _obsolete)
  (when-let ((q (scope--unqoute modes))) (scope-report-s q 'major-mode)))

(scope-define-function-analyzer scope-report (type &rest _)
  (when-let ((q (scope--unqoute type))) (scope-report-s q 'symbol-type)))

(scope-define-function-analyzer scope-report-s (&optional _sym type)
  (when-let ((q (scope--unqoute type))) (scope-report-s q 'symbol-type)))

(scope-define-function-analyzer icons--register (&optional name parent _spec _doc kws)
  (when-let ((q (scope--unqoute name))) (scope-report-s q 'deficon))
  (when-let ((q (scope--unqoute parent))) (scope-report-s q 'icon))
  (when-let ((q (scope--unqoute kws)))
    (while-let ((kw (car-safe q))
                (bkw (scope-sym-bare kw))
                ((keywordp bkw)))
      (scope-report-s kw 'constant)
      (cl-case bkw
        (:group (scope-report-s (cadr q) 'group)))
      (setq q (cddr q)))))

(scope-define-function-analyzer setopt--set (&optional var _val)
  (when-let ((q (scope--unqoute var))) (scope-report-s q 'variable)))

(scope-define-function-analyzer autoload (&optional func _file _doc int &rest _)
  (when-let ((q (scope--unqoute func))) (scope-report-s q 'function))
  (when-let ((q (scope--unqoute int)) ((listp q)))
    (dolist (mode q) (scope-report-s mode 'major-mode))))

(scope-define-function-analyzer minibuffer--define-completion-category (&optional name parents &rest _)
  (when-let ((q (scope--unqoute name)))    (scope-report-s q 'completion-category-definition))
  (when-let ((q (scope--unqoute parents)))
    (dolist (p (ensure-list q)) (scope-report-s p 'completion-category))))

;; (scope-define-macro-analyzer define-completion-category (l &optional name parent &rest rest)
;;   (scope-report-s name 'completion-category-definition)
;;   (scope-report-s parent 'completion-category)
;;   (scope-n l rest))

(scope-define-function-analyzer completion-table-with-category (&optional category _table)
  (when-let ((q (scope--unqoute category))) (scope-report-s q 'completion-category)))

(defun scope--easy-menu-do-define-menu (menu)
  (let ((items (cdr menu)))
   (while-let ((kw (car-safe items))
               (bkw (scope-sym-bare kw))
               ((keywordp bkw)))
     (scope-report-s kw 'constant)
     (cl-case bkw
       ((:active :label :visible) (scope-1 nil (cadr items)))
       ((:filter) (scope-sharpquote nil (cadr items))))
     (setq items (cddr items)))
   (dolist (item items)
     (cond
      ((vectorp item)
       (when (length> item 2)
         (scope-sharpquote nil (aref item 1))
         (let ((it (cddr (append item nil))))
           (scope-1 nil (car it))
           (while-let ((kw (car-safe it))
                       (bkw (scope-sym-bare kw))
                       ((keywordp bkw)))
             (scope-report-s kw 'constant)
             (cl-case bkw
               ((:active :enable :label :visible :suffix :selected) (scope-1 nil (cadr it))))
             (setq it (cddr it))))))
      ((consp item) (scope--easy-menu-do-define-menu item))))))

(scope-define-function-analyzer easy-menu-do-define (&optional _symbol _maps _doc menu)
  (when-let ((q (scope--unqoute menu)))
    (scope--easy-menu-do-define-menu q)))

(scope-define-function-analyzer define-key (&optional _keymaps _key def _remove)
  (when-let ((q (scope--unqoute def)))
    (cond
     ((eq (scope-sym-bare (car-safe q)) 'menu-item)
      (let ((fn (caddr q)) (it (cdddr q)))
        (scope-sharpquote nil fn)
        (while-let ((kw (car-safe it))
                    (bkw (scope-sym-bare kw))
                    ((keywordp bkw)))
          (scope-report-s kw 'constant)
          (cl-case bkw
            ((:active :enable :label :visible :suffix :selected) (scope-1 nil (cadr it)))
            ((:filter) (scope-sharpquote nil (cadr it))))
          (setq it (cddr it)))))
     ((or (symbolp q) (symbol-with-pos-p q))
      (scope-report-s q 'function)))))

(scope-define-macro-analyzer define-globalized-minor-mode (l global mode turn-on &rest body)
  (scope-report-s mode 'function)
  (scope-report-s turn-on 'function)
  (scope-define-minor-mode l global nil body))

(scope-define-macro-analyzer lambda (l args &rest body)
  (scope-lambda l args body))

(defun scope-oclosure-lambda-1 (local0 local bindings args body)
  (if bindings
      (let* ((binding (ensure-list (car bindings)))
             (sym (car binding))
             (bare (scope-sym-bare sym))
             (len (length (symbol-name bare)))
             (beg (scope-sym-pos sym)))
        (when beg (scope-report 'variable beg len beg))
        (scope-1 local0 (cadr binding))
        (scope-oclosure-lambda-1
         local0 (if bare (scope-local-new bare beg local) local)
         (cdr bindings) args body))
    (scope-lambda local args body)))

(defun scope-oclosure-lambda (local spec args body)
  (let ((type (car-safe spec)))
    (scope-report-s type 'oclosure))
  (scope-oclosure-lambda-1 local local (cdr-safe spec) args body))

(scope-define-macro-analyzer oclosure-lambda (l &optional spec args &rest body)
  (scope-oclosure-lambda l spec args body))

(scope-define-macro-analyzer cl-loop (l &rest clauses)
  (scope-loop l clauses))

(scope-define-macro-analyzer named-let (l name bindings &rest body)
  (scope-named-let l name bindings body))

(scope-define-macro-analyzer cl-flet (l bindings &rest body)
  (scope-flet l bindings body))

(scope-define-macro-analyzer cl-labels (l bindings &rest body)
  (scope-labels l bindings body))

(scope-define-macro-analyzer with-slots (l spec-list object &rest body)
  (scope-with-slots l spec-list object body))

(scope-define-macro-analyzer cl-defmethod (l name &rest rest)
  (scope-defmethod l name rest))

(scope-define-macro-analyzer cl-destructuring-bind (l args expr &rest body)
  (scope-1 l expr)
  (scope-cl-lambda l args body))

(scope-define-macro-analyzer declare-function (l &optional fn _file arglist _fileonly)
  (scope-report-s fn 'function)
  (scope-lambda l (and (listp arglist) arglist) nil))

(scope-define-macro-analyzer cl-block (l name &rest body)
  (scope-block l name body))

(scope-define-macro-analyzer cl-return-from (l name &optional result)
  (scope-return-from l name result))

(scope-define-macro-analyzer rx (l &rest regexps)
  ;; Unsafe macro!
  (scope-rx l regexps))

(scope-define-macro-analyzer cl-tagbody (l &rest body)
  (let (labels statements)
    (while body
      (let ((head (pop body)))
        (if (consp head)
            (push head statements)
          (push head labels))))
    (scope-cl-tagbody l (nreverse labels) (nreverse statements))))

(defvar scope-label-alist nil)

(defun scope-cl-tagbody (l labels statements)
  (if labels
      (let* ((label (car labels))
             (bare (scope-sym-bare label)))
        (when-let ((beg (scope-sym-pos label)))
          (scope-report 'label beg (length (symbol-name bare)) beg))
        (let ((scope-label-alist
               (if bare
                   (scope-local-new bare (scope-sym-pos label) scope-label-alist)
                 scope-label-alist)))
          (scope-cl-tagbody l (cdr labels) statements)))
    (scope-n l statements)))

(scope-define-macro-analyzer go (_l label)
  ;; TODO: Change to a local macro defintion induced by `cl-tagbody'.
  (when-let ((bare (scope-sym-bare label))
             (pos (alist-get bare scope-label-alist))
             (beg (scope-sym-pos label)))
    (scope-report 'label beg (length (symbol-name bare)) pos)))

(scope-define-macro-analyzer rx-define (l name &rest rest)
  (scope-rx-define l name rest))

(scope-define-macro-analyzer rx-let (l bindings &rest body)
  (scope-rx-let l bindings body))

(scope-define-macro-analyzer let-when-compile (l bindings &rest body)
  ;; Unsafe macro!
  (scope-let* l bindings body))

(scope-define-macro-analyzer cl-eval-when (l _when &rest body)
  ;; Unsafe macro!
  (scope-n l body))

(scope-define-macro-analyzer cl-macrolet (l bindings &rest body)
  ;; Unsafe macro!
  (scope-cl-macrolet l bindings body))

(scope-define-macro-analyzer cl-symbol-macrolet (l bindings &rest body)
  ;; Unsafe macro!
  (scope-let* l bindings body))

(scope-define-macro-analyzer nnoo-define-basics (_l &optional backend)
  ;; Unsafe macro!
  (let* ((bare (bare-symbol backend))
         (len (length (symbol-name bare)))
         (beg (scope-sym-pos backend)))
    (when beg (scope-report 'nnoo-backend beg len))))

(scope-define-macro-analyzer gv-define-expander (l name handler)
  (scope-gv-define-expander l name handler))

(scope-define-macro-analyzer gv-define-simple-setter (l name setter &rest rest)
  (scope-gv-define-simple-setter l name setter rest))

(scope-define-macro-analyzer cl-deftype (l name arglist &rest body)
  (scope-deftype l name arglist body))

(scope-define-macro-analyzer define-minor-mode (l &optional mode doc &rest body)
  (when mode (scope-define-minor-mode l mode doc body)))

(scope-define-macro-analyzer setq-local (l &rest args)
  (scope-setq l args))

(put 'setq-default 'scope-analyzer #'scope--analyze-setq-local)

(scope-define-macro-analyzer cl-defun (l name arglist &rest body)
  (scope-cl-defun l name arglist body))

(put 'cl-defmacro 'scope-analyzer #'scope--analyze-cl-defun)

(scope-define-macro-analyzer defun (l &optional name arglist &rest body)
  (when name (scope-defun l name arglist body)))

(put 'defmacro 'scope-analyzer #'scope--analyze-defun)
(put 'ert-deftest 'scope-analyzer #'scope--analyze-defun)

(scope-define-macro-analyzer scope-define-symbol-type (l &optional name parents &rest props)
  (scope-report-s name 'symbol-type-definition)
  (dolist (parent parents) (scope-report-s parent 'symbol-type))
  (while-let ((kw (car-safe props))
              (bkw (scope-sym-bare kw))
              ((keywordp bkw)))
    (scope-report-s kw 'constant)
    (cl-case bkw
      (:face
       (if-let ((q (scope--unqoute (cadr props)))) (scope-face-1 q)
         (scope-1 l (cadr props))))
      (otherwise (scope-1 l (cadr props))))
    (setq props (cddr props))))

(scope-define-macro-analyzer cl-letf (l bindings &rest body)
  (let ((l0 l))
    (dolist (binding bindings)
      (let ((place (car binding)))
        (if (or (symbol-with-pos-p place) (symbolp place))
            (let* ((bare (bare-symbol place))
                   (len (length (symbol-name bare)))
                   (beg (scope-sym-pos place)))
              (when beg (scope-report 'variable beg len beg))
              (setq l (scope-local-new bare beg l)))
          (scope-1 l0 place))
        (scope-1 l0 (cadr binding))))
    (scope-n l body)))

(scope-define-macro-analyzer setf (l &rest args)
  (scope-n l args))

(scope-define-macro-analyzer cl-callf (l &rest args)
  (scope-sharpquote l (car args))
  (scope-n l (cdr args)))

(put 'cl-callf2 'scope-analyzer #'scope--analyze-cl-callf)

(dolist (sym '( pop push with-memoization cl-pushnew incf decf
                ;; The following macros evaluate unsafe code.
                ;; Never expand them!
                static-if eval-when-compile eval-and-compile))
  (put sym 'scope-analyzer #'scope--analyze-setf))

(scope-define-macro-analyzer seq-let (l args sequence &rest body)
  (scope-1 l sequence)
  (dolist (arg args)
    (let* ((bare (scope-sym-bare arg))
           (len (length (symbol-name bare)))
           (beg (scope-sym-pos arg)))
      (if (eq bare '&rest)
          (scope-report 'ampersand beg len)
        (when beg (scope-report 'variable beg len beg))
        (setq l (scope-local-new bare beg l)))))
  (scope-n l body))

(scope-define-analyzer let-alist (l f alist &rest body)
  (scope-report-s f 'macro)
  (scope-1 l alist)
  (let ((scope-current-let-alist-form
         (cons (or (scope-sym-pos f) (cons 'gen (incf scope-counter)))
               (scope-sym-pos f))))
    (scope-n l body)))

(scope-define-special-form-analyzer let (l bindings &rest body)
  (scope-let-1 l l bindings body))

(scope-define-special-form-analyzer let* (l bindings &rest body)
  (scope-let* l bindings body))

(scope-define-special-form-analyzer cond (l &rest clauses)
  (scope-cond l clauses))

(scope-define-special-form-analyzer setq (l &rest args)
  (scope-setq l args))

(scope-define-special-form-analyzer defvar (l &optional sym init _doc)
  (when sym (scope-defvar l sym init)))

(put 'defconst 'scope-analyzer #'scope--analyze-defvar)

(scope-define-special-form-analyzer condition-case (l var bodyform &rest handlers)
  (scope-condition-case l var bodyform handlers))

(scope-define-special-form-analyzer function (l &optional arg)
  (when arg (scope-sharpquote l arg)))

(scope-define-special-form-analyzer quote (_l _arg)) ;Do nothing.

(scope-define-special-form-analyzer catch (l tag &rest body)
  (scope-catch l tag body))

(defun scope-report-s (sym type)
  (when-let* ((beg (scope-sym-pos sym)) (bare (bare-symbol sym)))
    (scope-report type beg (length (symbol-name bare)))))

(defun scope-1 (local form)
  (cond
   ((consp form)
    (let* ((f (car form)) (bare (scope-sym-bare f))
           (forms (cdr form)) (this nil))
      (when bare
        (cond
         ((setq this (assq bare scope-flet-alist))
          (scope-report
           'function (symbol-with-pos-pos f) (length (symbol-name bare)) (cdr this))
          (scope-n local forms))
         ((setq this (assq bare scope-macrolet-alist))
          (when (symbol-with-pos-p f)
            (scope-report
             'macro (symbol-with-pos-pos f) (length (symbol-name bare)) (cdr this)))
          ;; Local macros can be unsafe, so we do not expand them.
          ;; Hence we cannot interpret their arguments.
          )
         ((setq this (function-get bare 'scope-analyzer)) (apply this local form))
         ((special-form-p bare) (scope-report-s f 'special-form) (scope-n local forms))
         ((macrop bare) (scope-report-s f 'macro)
          (cond
           ((eq (get bare 'edebug-form-spec) t) (scope-n local forms))
           ((scope-safe-macro-p bare)
            (let* ((warning-minimum-log-level :emergency)
                   (macroexp-inhibit-compiler-macros t)
                   (symbols-with-pos-enabled t)
                   (message-log-max nil)
                   (inhibit-message t)
                   (macroexpand-all-environment
                    (append (mapcar #'list scope-unsafe-macros) macroexpand-all-environment))
                   (expanded (ignore-errors (macroexpand-1 form macroexpand-all-environment))))
              (scope-1 local expanded)))))
         ((or (functionp bare) (memq bare scope-local-functions) scope-assume-func-p)
          (scope-report-s f 'function) (scope-n local forms))))))
   ((symbol-with-pos-p form) (scope-s local form))))

(defun scope-n (local body) (dolist (form body) (scope-1 local form)))

;;;###autoload
(defun scope (callback &optional stream)
  "Read and analyze code from STREAM, reporting findings via CALLBACK.

Call CALLBACK for each analyzed symbol SYM with arguments TYPE, POS,
LEN, ID and DEF, where TYPE is a symbol that specifies the semantics of
SYM; POS is the position of SYM in STREAM; LEN is SYM's length; ID is an
object that uniquely identifies (co-)occurrences of SYM in the current
defun; and DEF is the position in which SYM is locally defined, or nil.
If SYM is itself a binding occurrence, then POS and BINDER are equal.
If SYM is not lexically bound, then BINDER is nil.  This function
ignores `read-symbol-shorthands', so SYM and LEN always correspond to
the symbol as it appears in STREAM.

If STREAM is nil, it defaults to the current buffer.

This function recursively analyzes Lisp forms (HEAD . TAIL), usually
starting with a top-level form, by inspecting HEAD at each level."
  (let ((scope-counter 0)
        (scope-callback callback)
        (read-symbol-shorthands nil)
        (max-lisp-eval-depth 32768))
    (scope-1 nil (read-positioning-symbols (or stream (current-buffer))))))

(provide 'scope)
;;; scope.el ends here
