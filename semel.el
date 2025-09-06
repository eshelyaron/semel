;;; semel.el --- Semantic highlighting for Emacs Lisp -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Eshel Yaron

;; Author: Eshel Yaron <me@eshelyaron.com>
;; Keywords:

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

;; This library defines a minor mode `semel-mode' that provides
;; semantic highlighting and associated features for Emacs Lisp code.
;; To enable it in all `emacs-lisp-mode' buffers, say:
;;
;;   (add-hook 'emacs-lisp-mode-hook #'semel-mode)
;;
;; `semel-mode' integrates with `cursor-sensor-mode' to highlight all
;; occurrences of the variable at point across its scope, so it is
;; recommended to also enable `cursor-sensor-mode' similarly.

;;; Code:

(require 'scope)

(defgroup semel nil
  "Semantic highlight for Emacs Lisp."
  :group 'lisp)

(defface semel-symbol-at-mouse '((t :background "#fff6d8"))
  "Face for highlighting the symbol at mouse in Emacs Lisp code.")

(defface semel-free-variable '((t :inherit underline))
  "Face for highlighting free variables in Emacs Lisp code.")

(defface semel-condition '((t :foreground "red"))
  "Face for highlighting `condition-case' conditions in Emacs Lisp code.")

(defface semel-major-mode-name '((t :foreground "#006400"))
  "Face for highlighting major mode names in Emacs Lisp code.")

(defface semel-face '((t :inherit font-lock-type-face))
  "Face for highlighting face names in Emacs Lisp code.")

(defface semel-symbol-type '((t :foreground "#00008b" :inherit font-lock-function-call-face))
  "Face for highlighting symbol type names in Emacs Lisp code.")

(defface semel-symbol-type-definition '((t :foreground "#00008b" :inherit font-lock-function-name-face))
  "Face for highlighting symbol type names in Emacs Lisp code.")

(defface semel-function-call '((t :inherit font-lock-function-call-face))
  "Face for highlighting function calls in Emacs Lisp code.")

(defface semel-non-local-exit '((t :inherit elisp-function-call :underline "red"))
  "Face for highlighting function calls in Emacs Lisp code.")

(defface semel-macro-call '((t :inherit font-lock-keyword-face))
  "Face for highlighting macro calls in Emacs Lisp code.")

(defface semel-special-form '((t :inherit elisp-macro-call))
  "Face for highlighting special forms in Emacs Lisp code.")

(defface semel-throw-tag '((t :inherit font-lock-constant-face))
  "Face for highlighting `catch'/`throw' tags in Emacs Lisp code.")

(defface semel-feature '((t :inherit font-lock-constant-face))
  "Face for highlighting feature names in Emacs Lisp code.")

(defface semel-rx '((t :foreground "#00008b"))
  "Face for highlighting `rx' constructs in Emacs Lisp code.")

(defface semel-theme '((t :inherit font-lock-constant-face))
  "Face for highlighting custom theme names in Emacs Lisp code.")

(defface semel-binding-variable
  '((t :slant italic :inherit font-lock-variable-name-face))
  "Face for highlighting binding occurrences of variables in Emacs Lisp code.")

(defface semel-bound-variable '((t :slant italic))
  "Face for highlighting bound occurrences of variables in Emacs Lisp code.")

(defface semel-variable-at-point '((t :inherit bold))
  "Face for highlighting (all occurrences of) the variable at point.")

(defun semel--function-help-echo (sym &rest _)
  (when (fboundp sym)
    (with-temp-buffer
      (let ((standard-output (current-buffer)))
        (insert "`" (symbol-name sym) "' is ")
        (describe-function-1 sym))
      (buffer-string))))

(defun semel--help-echo-1 (str sym prop &rest _)
  (if-let ((doc (documentation-property sym prop t)))
      (format "%s `%S'.\n\n%s" str sym doc)
    str))

(defun semel--help-echo (beg end prop str)
  (if-let ((sym (intern-soft (buffer-substring-no-properties beg end))))
      (apply-partially #'semel--help-echo-1 str sym prop)
    str))

(defun semel-local-references (pos)
  "Return references to local variable at POS as (BEG . LEN) cons cells."
  (let (all cur)
    (save-excursion
      (goto-char pos)
      (beginning-of-defun)
      (scope (lambda (_type beg len id &optional _def)
               (when (<= beg pos (+ beg len))
                 (setq cur id))
               (when id (setf (alist-get beg all nil nil nil) (list len id))))))
    (seq-keep
     (pcase-lambda (`(,beg ,len ,id)) (when (equal id cur) (cons beg len)))
     all)))

(defun semel-highlight-variable (pos)
  "Highlight variable at POS along with its co-occurrences."
  (pcase-dolist (`(,beg . ,len) (semel-local-references pos))
    (let ((ov (make-overlay beg (+ beg len))))
      (overlay-put ov 'face 'semel-variable-at-point)
      (overlay-put ov 'semel-highlight-variable t))))

(defun semel-unhighlight-variable (pos)
  "Remove variable highlighting across top-level form at POS."
  (save-excursion
    (goto-char pos)
    (beginning-of-defun)
    (remove-overlays (point) (progn (end-of-defun) (point))
                     'semel-highlight-variable t)))

(defun semel-cursor-sensor (pos)
  "Return `cursor-sensor-functions' for Semel symbol at POS."
  (list
   (lambda (_win old dir)
     (cl-case dir
       (entered (semel-highlight-variable pos))
       (left (semel-unhighlight-variable old))))))

(defun semel--annotate-symbol-with-help-echo (type beg end def)
  (put-text-property
   beg end 'help-echo
   (when-let ((fun (scope-get-symbol-type-property type :help)))
     (funcall fun beg end def))))

(defun semel-fontify-symbol (type sym len id &optional def)
  (semel--annotate-symbol-with-help-echo type sym (+ sym len) def)
  (let ((face (cond
               ((null id) (scope-get-symbol-type-property type :face))
               ((equal sym def) 'semel-binding-variable)
               (t 'semel-bound-variable))))
    (add-face-text-property sym (+ sym len) face t)
    (put-text-property sym (+ sym len) 'mouse-face `(,face semel-symbol-at-mouse))
    (when id
      (put-text-property sym (+ sym len 1) 'cursor-sensor-functions
                         ;; Get a fresh list with SYM hardcoded,
                         ;; so that the value is distinguishable
                         ;; from the value in adjacent regions.
                         (semel-cursor-sensor sym)))))

(defun semel-fontify-region (beg end)
  "Fontify symbols between BEG and END according to their meaning."
  (save-excursion
    (goto-char beg)
    (while (< (point) end) (ignore-errors (scope #'semel-fontify-symbol)))))

(defun semel-fontify-region-advice (orig beg end &optional verbose)
  (condition-case nil
      (let ((beg (save-excursion (goto-char beg) (beginning-of-defun)
                                 (point)))
            (end (save-excursion (goto-char end) (end-of-defun)
                                 (skip-chars-backward " \t\n")
                                 (point))))
        (funcall orig beg end verbose)
        (semel-fontify-region beg end)
        `(jit-lock-bounds ,beg . ,end))
    (scan-error nil)))

;;;###autoload
(define-minor-mode semel-mode
  "Semantic highlighting for Emacs Lisp."
  :interactive (emacs-lisp-mode)
  (if semel-mode
      (add-function :around (local 'font-lock-fontify-region-function)
                    #'semel-fontify-region-advice)
    (remove-function (local 'font-lock-fontify-region-function)
                     #'semel-fontify-region-advice)))

(provide 'semel)
;;; semel.el ends here
