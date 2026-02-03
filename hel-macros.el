;;; hel-macros.el --- Macros -*- lexical-binding: t; -*-
;;
;; Copyright © 2025 Yuriy Artemyev
;;
;; Author: Yuriy Artemyev <anuvyklack@gmail.com>
;; Maintainer: Yuriy Artemyev <anuvyklack@gmail.com>
;; Version: 0.0.1
;; Homepage: https://github.com/anuvyklack/hel
;; Package-Requires: ((emacs "29.1"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Code:

(require 'cl-lib)
(require 'dash)

(defmacro hel-defvar-local (symbol &optional initvalue docstring)
  "The same as `defvar-local' but additionaly marks SYMBOL as permanent
buffer local variable."
  (declare (indent defun)
           (doc-string 3)
           (debug (symbolp &optional form stringp)))
  `(prog1 (defvar ,symbol ,initvalue ,docstring)
     (make-variable-buffer-local ',symbol)
     (put ',symbol 'permanent-local t)))

(defvar hel--advices nil
  "Inner variable for `hel-define-advice'.")

(defmacro hel-define-advice (symbol args &rest body)
  "Wrapper around `define-advice' that automatically add/remove advice
when `hel-mode' is toggled on or off.

\(fn SYMBOL (HOW LAMBDA-LIST &optional NAME) &rest BODY)"
  (declare (indent 2) (doc-string 3) (debug (sexp sexp def-body)))
  (unless (listp args)
    (signal 'wrong-type-argument (list 'listp args)))
  (unless (<= 2 (length args) 4)
    (signal 'wrong-number-of-arguments (list 2 4 (length args))))
  (let* ((how (nth 0 args))
         (lambda-list (nth 1 args))
         (name (or (nth 2 args) 'hel))
         (advice (intern (format "%s@%s" symbol name))))
    `(prog1 (defun ,advice ,lambda-list ,@body)
       (cl-pushnew '(,symbol ,how ,advice) hel--advices
                   :test #'equal)
       (when hel-mode
         (advice-add ',symbol ,how #',advice)))))

(defmacro hel-advice-add (symbol how function)
  "Wrapper around `advice-add' that automatically add/remove advice
when `hel-mode' is toggled on or off"
  `(progn
     (cl-pushnew (list ,symbol ,how ,function) hel--advices
                 :test #'equal)
     (when hel-mode
       (advice-add ,symbol ,how ,function))))

(defmacro hel-recenter-point-on-jump (&rest body)
  "Recenter point on jumps during BODY evaluating if it lands out of the screen.
This macro calls `redisplay' internally and should be used with care to avoid
flickering."
  (declare (indent 0) (debug t))
  `(let ((scroll-conservatively 0))
     (prog1 (progn ,@body)
       ;; Update the screen so that the temporary value for
       ;; `scroll-conservatively' is taken into account.
       (unless hel-executing-command-for-fake-cursor
         (redisplay)))))

(defmacro hel-save-region (&rest body)
  "Evaluate BODY with preserving original region.
The difference from `save-mark-and-excursion' is that both point and mark are
saved as markers and correctly handle case when text was inserted before region."
  (declare (indent 0) (debug t))
  (let ((pnt (gensym "point"))
        (beg (gensym "region-beg"))
        (end (gensym "region-end"))
        (dir (gensym "region-dir")))
    `(if (use-region-p)
         (let ((deactivate-mark nil)
               (,beg (copy-marker (region-beginning) t))
               (,end (copy-marker (region-end)))
               (,dir (hel-region-direction)))
           (unwind-protect
               (save-excursion ,@body)
             (hel-set-region ,beg ,end ,dir)
             (set-marker ,beg nil)
             (set-marker ,end nil)))
       ;; else
       (let ((,pnt (copy-marker (point) t)))
         (unwind-protect
             (save-excursion ,@body)
           (if mark-active (deactivate-mark))
           (goto-char ,pnt)
           (set-marker ,pnt nil))))))

(defmacro hel-restore-region-on-error (&rest body)
  (declare (indent 0) (debug t))
  (let ((region (gensym "region"))
        (point-pos (gensym "point"))
        (something-goes-wrong? (make-symbol "something-goes-wrong?")))
    `(let ((,region (hel-region))
           (,point-pos (point))
           (,something-goes-wrong? t))
       (unwind-protect
           (prog1 (progn ,@body)
             (setq ,something-goes-wrong? nil))
         (when ,something-goes-wrong?
           (if ,region
               (apply #'hel-set-region ,region)
             (goto-char ,point-pos)))))))

(defmacro hel-define-command (command args &rest body)
  "Define Hel COMMAND.
Wrapper around `defun' macro, that additionally takes following keyword
parameters:

`:multiple-cursors'
  - t    Command will be executed for all cursors;
  - nil  Command will be executed only for main cursor.

`:merge-selections'
  Any Emacs lisp FORM, that will be evaluated after COMMAND execution
  and if it evaluates to non-nil overlapping selections (regions) will
  be merged into single selection.

\(fn COMMAND (ARGS...) [DOC] [[KEY VALUE]...] BODY...)"
  (declare (indent defun)
           (doc-string 3)
           (debug ( &define name
                    [&optional lambda-list]
                    [&optional stringp]
                    [&rest keywordp sexp]
                    [&optional ("interactive" [&rest form])]
                    def-body)))
  (let (doc properties key value)
    ;; collect docstring
    (setq doc (pcase (car-safe body)
                ((and `(format . ,_) doc-form)
                 (eval doc-form t))
                ((and (pred stringp) doc)
                 doc)))
    (when doc (pop body))
    ;; collect keywords
    (while (keywordp (car-safe body))
      (setq key   (pop body)
            value (pop body))
      (pcase key
        (:multiple-cursors
         (push `(put ',command 'multiple-cursors ,(if (eq value t) t ''false))
               properties))
        (:merge-selections
         (push `(put ',command 'merge-selections ,(if (symbolp value)
                                                      `',value
                                                    `(lambda () ,value)))
               properties))))
    ;; macro expansion
    `(progn
       (defun ,command (,@args)
         ,@(when doc `(,doc))
         ,@body)
       ,@properties)))

(provide 'hel-macros)
;;; hel-macros.el ends here
