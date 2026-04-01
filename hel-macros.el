;;; hel-macros.el -*- lexical-binding: t -*-
;;
;; Copyright © 2025-2026 Yuriy Artemyev
;;
;; Author: Yuriy Artemyev <anuvyklack@gmail.com>
;; Maintainer: Yuriy Artemyev <anuvyklack@gmail.com>
;; Version: 0.10.0
;; Homepage: https://github.com/anuvyklack/hel
;; Package-Requires: ((emacs "29.1"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Code:

(eval-when-compile (require 'cl-lib))
(require 'map)
(require 'dash)
(require 'hel-vars)
(require 'hel-common)

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
     (cl-pushnew (list ,symbol ,how ,function) hel--advices :test #'equal)
     (when hel-mode
       (advice-add ,symbol ,how ,function))))

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
  (-let* ((doc (pcase (car-safe body)
                 ((and `(format . ,_) doc-form)
                  (eval doc-form t))
                 ((and (pred stringp) doc)
                  doc)))
          ((kwargs . body) (hel-split-keyword-args (if doc (cdr body) body)))
          (properties (->> kwargs
                           (map-apply (lambda (key value)
                                        (pcase key
                                          (:multiple-cursors
                                           `(put ',command 'multiple-cursors ,value))
                                          (:merge-selections
                                           `(put ',command 'merge-selections
                                                 ,(if (symbolp value)
                                                      `',value
                                                    `(lambda () ,value))))))))))
    ;; macro expansion
    `(progn
       (defun ,command (,@args)
         ,@(when doc `(,doc))
         ,@body)
       ,@properties)))

(provide 'hel-macros)
;;; hel-macros.el ends here
