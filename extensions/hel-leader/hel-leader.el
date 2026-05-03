;;; hel-leader.el -*- lexical-binding: t -*-
;;
;; Copyright © 2025-2026 Yuriy Artemyev
;;
;; Author: Yuriy Artemyev <anuvyklack@gmail.com>
;; Maintainer: Yuriy Artemyev <anuvyklack@gmail.com>
;; Version: 2.0
;; Homepage: https://github.com/anuvyklack/hel
;; Package-Requires: ((emacs "29.1"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; How to read this table: If you evaluate `read-key' and press a key from the
;; first column, the value you get is in the second column. If you pass the
;; value from the second column to the `single-key-description' function, you
;; get the value shown in the third column.
;;
;; │ Key  -> │ `read-key'   ->  │ `single-key-description' │
;; ├─────────┼──────────────────┼──────────────────────────┤
;; │ TAB     │ 9                │ "TAB"                    │
;; │ C-i     │ 9                │ "TAB"                    │
;; │         │ 'tab             │ "<tab>"                  │
;; │ C-TAB   │ 'C-tab           │ "C-<tab>"                │
;; │ S-TAB   │ 'backtab         │ "<bactab>"               │
;; │ M-TAB   │                  │                          │
;; │ C-S-TAB │ 'C-S-iso-lefttab │ "C-S-<iso-lefttab>"      │
;; │ C-M-TAB │                  │                          │
;; │ M-S-TAB │                  │                          │
;; ├─────────┼──────────────────┼──────────────────────────┤
;; │ RET     │ 13               │ "RET"                    │
;; │ C-m     │ 13               │ "RET"                    │
;; │         │ 'return          │ "<return>"               │
;; │ C-RET   │ 'C-return        │ "C-<return>"             │
;; │ S-RET   │ 'S-return        │ "S-<return>"             │
;; │ M-RET   │ 134217741        │ "M-RET"                  │
;; │ C-S-RET │ 'C-S-return      │ "C-S-<return>"           │
;; │ C-M-RET │ 'C-M-return      │ "C-M-<return>"           │
;; │ M-S-RET │ 'M-S-return      │ "M-S-<return>"           │
;;
;;; Code:

(require 's)
(require 'dash)
(eval-when-compile (require 'hel-macros))
(require 'hel-core)

;;; Keybindings

(hel-keymap-global-set :state '(normal motion)
  "SPC"      'hel-leader
  "C-w SPC"  'hel-leader-other-window
  "C-h k"    'hel-leader-describe-key
  "<f1> k"   'hel-leader-describe-key
  "<help> k" 'hel-leader-describe-key)

;;; Custom variables

(defgroup hel-leader nil
  "Custom group for `hel-leader'."
  :group 'hel-leader-module)

(defcustom hel-leader-meta-prefix "m"
  "The key coresponding to \"M-\" modifier."
  :group 'hel-leader
  :type 'string)

(defcustom hel-leader-ctrl-meta-prefix "g"
  "The key coresponding to \"C-M-\" modifier."
  :group 'hel-leader
  :type 'string)

(defcustom hel-leader-echo t
  "Whether to show hel-leader messages in the echo area."
  :group 'hel-leader
  :type 'boolean)

(defcustom hel-leader-message-prefix "hel-leader: "
  "The prefix string for hel-leader messages."
  :group 'hel-leader
  :type 'string)

(defvar-keymap hel-leader-map
  :doc "hel-leader service keys."
  "DEL"         #'hel-leader-undo ;; DEL in Emacs corresponds to the backspace key
  "<backspace>" #'hel-leader-undo
  "ESC"         #'hel-leader-quit
  "<escape>"    #'hel-leader-quit
  "C-g"         #'hel-leader-quit
  ;; "<remap> <keyboard-quit>" #'hel-leader-quit
  )

;;; Internal vars

(defvar hel-leader--keys nil
  "List with keys entered in hel-leader state.
Keys are strings that satisfies `key-valid-p'")

(defvar hel-leader--pending-modifier nil
  "Stores the pending modifier symbol to be applied to the next key.")

(defvar hel-leader--command nil
  "The command that hel-leader found.")

(defvar hel-leader--preview-is-active nil)

;;; Interactive commands

(hel-define-command hel-leader ()
  "Activate hel-leader and interactively evaluate found command."
  :multiple-cursors nil
  (interactive)
  (when-let* ((cmd (hel-leader-start)))
    (setq this-command cmd
          hel-this-command this-command)
    (call-interactively cmd)))

(hel-define-command hel-leader-other-window ()
  (interactive)
  :multiple-cursors nil
  (other-window-prefix)
  (hel-leader))

(hel-define-command hel-leader-describe-key (key-list &optional buffer)
  "Wrapper around `describe-key', that correctly handle key chords entered
with hel-leader. If Helpful package is loaded, `helpful-key' will be used instead
of `describe-key'."
  :multiple-cursors nil
  (interactive (list (help--read-key-sequence)))
  (pcase (key-binding (caar key-list))
    ('hel-leader (when-let* ((cmd (hel-leader-start)))
                   (if (fboundp 'helpful-command)
                       (helpful-command cmd)
                     (describe-command cmd))))
    (_ (if (fboundp 'helpful-key)
           (helpful-key (caar key-list))
         (describe-key key-list buffer)))))

;;; Core

(defun hel-leader-start ()
  "Enter hel-leader state.
When EXECUTE is non-nil execute the found command.
Return the found command."
  ;; Try to make this command transparent.
  (setq this-command last-command)
  (setq hel-leader--keys nil
        hel-leader--pending-modifier nil
        hel-leader--command nil)
  (hel-leader--show-preview)
  (unwind-protect
      (while (not (eq (hel-leader--handle-input-event (read-key))
                      :quit))
        (hel-leader--show-preview))
    (hel-leader--hide-preview))
  hel-leader--command)

(defun hel-leader--handle-input-event (event)
  "Handle input EVENT. Return `:quit' if handling is completed."
  ;; (setq last-command-event last-input-event)
  (let ((key (single-key-description event)))
    (if-let* ((cmd (keymap-lookup hel-leader-map key)))
        (call-interactively cmd)
      ;; else
      (cond ((and (equal '("C-x") hel-leader--keys)
                  (equal "x" key)
                  (null hel-leader--pending-modifier))
             (setq hel-leader--pending-modifier "C-"))
            (hel-leader--pending-modifier
             (cl-callf -snoc hel-leader--keys
               (pcase hel-leader--pending-modifier
                 ("C-"   (hel-leader--add-control key))
                 ("M-"   (hel-leader--add-meta key))
                 ("C-M-" (hel-leader--add-control-meta key))))
             (setq hel-leader--pending-modifier nil))
            ((and (equal hel-leader-meta-prefix key)
                  (hel-leader--meta-keybindings-available-p))
             (setq hel-leader--pending-modifier "M-"))
            ((and (equal hel-leader-ctrl-meta-prefix key)
                  (hel-leader--meta-keybindings-available-p))
             (setq hel-leader--pending-modifier "C-M-"))
            (hel-leader--keys
             (cl-callf -snoc hel-leader--keys key))
            ;; All following conditions assumes that `hel-leader--keys' are empty.
            ((equal "c" key)
             (setq hel-leader--keys '("C-c")
                   hel-leader--pending-modifier "C-"))
            ((equal "x" key)
             (setq hel-leader--keys '("C-x")
                   hel-leader--pending-modifier nil))
            (t
             (setq hel-leader--keys (list "C-c" key))))
      ;; Try execute collected keys
      (when hel-leader--keys
        (let ((cmd (hel-leader--lookup-key hel-leader--keys)))
          (cond ((commandp cmd t)
                 (setq hel-leader--command cmd)
                 :quit)
                ((keymapp cmd))
                (t
                 (message "%s is undefined" (s-join " " hel-leader--keys))
                 :quit)))))))

(defun hel-leader-undo ()
  "Pop the last input."
  (interactive)
  (setq this-command last-command)
  (cond (hel-leader--pending-modifier
         (setq hel-leader--pending-modifier nil))
        (hel-leader--keys
         (cl-callf -butlast hel-leader--keys))
        (t
         (when hel-leader-echo (message "hel-leader exit"))
         :quit)))

(defun hel-leader-quit ()
  "Quit hel-leader state."
  (interactive)
  (setq this-command last-command)
  (when hel-leader-echo (message "hel-leader exit"))
  :quit) ; Indicate that hel-leader loop should be stopped

(defun hel-leader--add-control (key)
  (pcase key
    ("TAB" "C-<tab>")
    ("RET" "C-RET") ;; "C-<return>"
    ("ESC" "ESC")
    (_ (if (s-contains? "C-" key)
           key
         (concat "C-" (hel-leader--handle-shift key))))))

(defun hel-leader--add-meta (key)
  (if (s-contains? "C-" key)
      (hel-leader--add-control-meta key)
    (pcase key
      ("TAB" "M-TAB") ;; "M-<tab>"
      ("RET" "M-RET") ;; "M-<return>"
      ("ESC" "ESC")
      (_ (concat "M-" key)))))

(defun hel-leader--add-control-meta (key)
  (pcase key
    ("TAB" "C-M-<tab>")
    ("RET" "C-M-<return>")
    ("ESC" "ESC")
    (_ (concat "C-M-" (s-with key
                        (s-replace "C-" "")
                        (s-replace "M-" "")
                        (hel-leader--handle-shift))))))

(defun hel-leader--handle-shift (str)
  "Convert capical ASCII letters the following way: \"K\" -> \"S-k\".
`key-parse' for \"C-K\" and \"C-k\" returns the same event. You must
pass \"C-S-k\" instead. This is relevant only for ASCII. For Unicode,
for example for Cyrillic letters \"C-Ф\" and \"C-ф\" `key-parse' returns
different events."
  (if (and (length= str 1)
           (<= ?A (string-to-char str) ?Z))
      (concat "S-" (downcase str))
    str)
  ;; (let ((event (seq-first (key-parse key))))
  ;;   (if (equal '(shift) (event-modifiers event))
  ;;       (concat "S-" (single-key-description
  ;;                     (event-basic-type event)))
  ;;     key))
  )

(defun hel-leader--meta-keybindings-available-p ()
  "Return non-nil if there are keybindings that starts with Meta prefix."
  ;; Key sequences starts with ESC are accessible via Meta key.
  (key-binding (->> (-snoc hel-leader--keys "ESC")
                    (s-join " ")
                    (key-parse))))

(defun hel-leader--lookup-key (keys)
  "Return the command bound to KEYS.
KEYS should be a string of a list of strings."
  ;; (keymap-lookup nil (-some->> keys (s-join " ")))
  (key-binding (key-parse (cond ((proper-list-p keys)
                                 (s-join " " keys))
                                ((stringp keys)
                                 keys)))))

;;; Which-key integration

(declare-function which-key--create-pages "which-key" (keys &optional prefix-keys prefix-title))
(declare-function which-key--show-page "which-key" (&optional n))
(declare-function which-key--hide-popup "which-key" ())
(declare-function which-key--get-keymap-bindings-1 "which-key" (keymap start &optional prefix filter all ignore-commands))
(defvar which-key-show-prefix)
(defvar which-key-side-window-location)
(defvar which-key-idle-delay)
(defvar which-key--last-try-2-loc)
(defvar which-key--pages-obj)
(defvar which-key-sort-order)

(defun hel-leader--show-preview ()
  "Show preview with possible continuations for the keys
that were entered in the hel-leader."
  (hel-leader--show-message)
  (when (and (bound-and-true-p which-key-mode)
             (or hel-leader--preview-is-active
                 (sit-for which-key-idle-delay t)))
    (hel-leader--which-key-show)
    (hel-leader--show-message)
    (setq hel-leader--preview-is-active t)))

(defun hel-leader--hide-preview ()
  (when hel-leader--preview-is-active
    (which-key--hide-popup)
    (setq hel-leader--preview-is-active nil)))

;; Adopted from `which-key--create-buffer-and-show'
(defun hel-leader--which-key-show (&optional prefix-title)
  "Fill `which-key--buffer' with key descriptions and display it."
  (let ((which-key-show-prefix nil)
        (bindings (hel-leader--which-key-get-bindings))
        (prefix (-some->> hel-leader--keys
                  (s-join " ")
                  (key-parse))))
    (cond ((null bindings)
           (when hel-leader--keys
             (message "%s-  which-key: There are no keys to show"
                      (s-join " " hel-leader--keys))))
          ((listp which-key-side-window-location)
           (setq which-key--last-try-2-loc
                 (apply 'which-key--try-2-side-windows
                        bindings prefix prefix-title
                        which-key-side-window-location)))
          (t
           (setq which-key--pages-obj (which-key--create-pages
                                       bindings prefix prefix-title))
           (which-key--show-page)))))

(defun hel-leader--which-key-get-bindings ()
  "Return a list of (KEY SEPARATOR DESCRIPTION) lists."
  (let ((bindings (unless hel-leader--pending-modifier
                    (cond ((not hel-leader--keys)
                           `(("x" . "group:C-x")
                             ("c" . "group:C-c C-")
                             (,hel-leader-meta-prefix . "group:M-")
                             (,hel-leader-ctrl-meta-prefix . "group:C-M-")))
                          ((equal hel-leader--keys '("C-x"))
                           '(("x" . "group:C-x C-"))))))
        (prefix (if (or hel-leader--keys
                        hel-leader--pending-modifier)
                    (-some->> hel-leader--keys (s-join " ") (key-parse))
                  (key-parse "C-c")))
        ;; Accepts (KEYS . DESCRIPTION) cons-cell and return non-nil if it
        ;; should be shown in Which-Key popup.
        (filter (-lambda ((keys . _description))
                  (let ((key (->> keys (split-string) (-last-item))))
                    (not (hel-leader--occupied-key-p key)))))
        (ignore '( self-insert-command digit-argument
                   ignore ignore-event company-ignore)))
    (dolist (map (current-active-maps t))
      (setq bindings (which-key--get-keymap-bindings-1 map bindings prefix
                                                       filter nil ignore)))
    (setq bindings
          (-filter (-lambda ((keys . _description))
                     (let ((key (->> keys (split-string) (-last-item))))
                       (pcase hel-leader--pending-modifier
                         ('nil (not (or (s-starts-with? "C-" key)
                                        (s-starts-with? "M-" key))))
                         ("C-" (and (s-starts-with? "C-" key)
                                    (not (s-starts-with? "C-M-" key))))
                         (_ ;; (or "M-" "C-M-")
                          (s-starts-with? hel-leader--pending-modifier key)))))
                   bindings))
    (-each `((,hel-leader-meta-prefix . "M-")
             (,hel-leader-ctrl-meta-prefix . "C-M-"))
      (-lambda ((key . modifier))
        (when (and (not (equal hel-leader--pending-modifier modifier))
                   (->> bindings
                        (-find (-lambda ((keys . _description))
                                 (s-starts-with? modifier key)))))
          (let ((key (->> (-snoc hel-leader--keys key)
                          (s-join " "))))
            (setq bindings (cons (cons key (s-concat "group:" modifier))
                                 (->> bindings
                                      (-remove (-lambda ((keys . _description))
                                                 (equal keys key))))))))))
    (when which-key-sort-order
      (cl-callf sort bindings which-key-sort-order))
    (which-key--format-and-replace bindings)))

(defun hel-leader--show-message ()
  "Show message in echo area for current hel-leader input."
  (when hel-leader-echo
    (let ((message-log-max) ;; disable message logging
          (argument (-> (pcase current-prefix-arg
                          ('(4) "C-u ")
                          ('(16) "C-u C-u ")
                          ('- "C-u -")
                          ((pred integerp)
                           (format "C-u %d" current-prefix-arg))
                          ((guard current-prefix-arg)
                           (concat current-prefix-arg " "))
                          (_ ""))
                        (propertize 'face 'font-lock-comment-face)))
          (keys (-> (if (or hel-leader--keys
                            hel-leader--pending-modifier)
                        (-snoc hel-leader--keys
                               hel-leader--pending-modifier)
                      '("C-c"))
                    (-non-nil)
                    (string-join " ")
                    (propertize 'face 'font-lock-string-face))))
      (message "%s%s%s" hel-leader-message-prefix argument keys))))

(defun hel-leader--occupied-key-p (key)
  "Return non-nil if KEY with all modifiers stripped is used by hel-leader
itself and hence unavailable."
  (keymap-lookup hel-leader-map (s-with key
                                  (s-replace "C-" "")
                                  (s-replace "M-" "")
                                  (s-replace "S-" ""))))

;;; .
(provide 'hel-leader)
;;; hel-leader.el ends here
