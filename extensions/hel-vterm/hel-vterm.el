;;; hel-vterm.el --- Hel–Vterm integration -*- lexical-binding: t -*-
;;
;; Copyright (C) 2026 Yuriy Artemyev
;;
;; Author: Yuriy Artemyev <anuvyklack@gmail.com>
;; Maintainer: Yuriy Artemyev <anuvyklack@gmail.com>
;; Version: 0.9.0
;; Homepage: https://github.com/anuvyklack/hel
;; Package-Requires: ((emacs "29.1"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Code:

(provide 'hel-macros)
(require 'hel-core)
(require 'vterm)

;;;; Keybindings

(hel-keymap-set vterm-mode-map
  "C-c C-t" 'hel-local-mode)

(hel-keymap-set vterm-mode-map :state 'normal
  "[ ["   'vterm-previous-prompt
  "] ]"   'vterm-next-prompt
  "g h"   'hel-vterm-first-non-blank
  "i"     'hel-vterm-insert
  "a"     'hel-vterm-append
  "I"     'hel-vterm-insert-prompt
  "A"     'hel-vterm-append-prompt
  "o"     'undefined
  "O"     'undefined
  "c"     'hel-vterm-change
  "d"     'hel-vterm-cut
  "D"     'hel-vterm-delete
  "u"     'vterm-undo
  "p"     'hel-vterm-paste-after
  "P"     'hel-vterm-paste-before
  "G"     'vterm-reset-cursor-point
  ;;
  "C-o"   'vterm--self-insert
  "C-<i>" 'vterm--self-insert)

(hel-keymap-set vterm-mode-map :state 'insert
  "C-w"   'vterm--self-insert)

;;;; Commands

;; gh
(hel-define-command hel-vterm-first-non-blank ()
  "Move the cursor to the first non-blank character after the prompt."
  :multiple-cursors t
  :merge-selections t
  (interactive)
  (setq this-command 'hel-vterm-first-non-blank)
  (let ((beg (if (or (eq last-command this-command)
                     hel--extend-selection)
                 (mark)
               (point)))
        (end (if (vterm-cursor-in-command-buffer-p (point))
                 (vterm-beginning-of-line)
               ;; else
               (hel-beginning-of-line)
               (skip-syntax-forward " " (line-end-position))
               (backward-prefix-chars)
               (point))))
    (hel-set-region beg end)))

;; i
(hel-define-command hel-vterm-insert ()
  "Switch to Insert state before selection."
  :multiple-cursors nil
  (interactive)
  (hel-with-each-cursor
    (when (use-region-p)
      (hel-ensure-region-direction -1))
    (vterm-goto-char (point)))
  (hel-insert-state 1))

;; a
(hel-define-command hel-vterm-append ()
  "Switch to Insert state after selection."
  :multiple-cursors nil
  (interactive)
  (vterm-goto-char (1+ (point)))
  (hel-with-each-cursor
    (when (use-region-p)
      (hel-ensure-region-direction 1)
      (when (hel-linewise-selection-p)
        (backward-char)))
    (vterm-goto-char (point)))
  (hel-insert-state 1))

;; I
(hel-define-command hel-vterm-insert-prompt ()
  "Goto beginning of prompt and switch to Insert state."
  :multiple-cursors nil
  (interactive)
  (hel-disable-multiple-cursors-mode)
  (deactivate-mark)
  (vterm-goto-char (vterm--get-prompt-point))
  (hel-insert-state 1))

;; A
(hel-define-command hel-vterm-append-prompt ()
  "Goto end of prompt and switch to Insert state."
  :multiple-cursors nil
  (interactive)
  (hel-disable-multiple-cursors-mode)
  (deactivate-mark)
  (vterm-goto-char (vterm--get-end-of-line))
  (hel-insert-state 1))

;; c
(hel-define-command hel-vterm-change ()
  "Delete selection and switch to Insert state."
  :multiple-cursors nil
  (interactive)
  (hel-with-each-cursor
    (if (use-region-p)
        (vterm-delete-region (max (region-beginning)
                                  (vterm--get-prompt-point))
                             (min (region-end)
                                  (vterm--get-end-of-line)))
      (vterm-send-backspace)))
  (hel-insert-state 1))

;; d
(hel-define-command hel-vterm-cut ()
  "Kill (cut) text in selection — i.e. delete it and put in the `kill-ring'.
Without selection delete character before point."
  :multiple-cursors t
  :merge-selections t
  (interactive)
  (if (use-region-p)
      (vterm-delete-region (max (region-beginning)
                                (vterm--get-prompt-point))
                           (min (region-end)
                                (vterm--get-end-of-line)))
    (vterm-send-backspace))
  (hel-extend-selection -1))

;; D
(hel-define-command hel-vterm-delete ()
  "Delete text in selection, without modifying the `kill-ring'.
Without selection delete character after point."
  :multiple-cursors t
  :merge-selections t
  (interactive)
  (if (use-region-p)
      (vterm-delete-region (max (region-beginning)
                                (vterm--get-prompt-point))
                           (min (region-end)
                                (vterm--get-end-of-line)))
    (vterm-send-delete))
  (hel-extend-selection -1))

;; p
(hel-define-command hel-vterm-paste-after (&optional arg)
  "Paste after selection."
  :multiple-cursors t
  (interactive "P")
  (hel-paste #'vterm-yank 1))

;; P
(hel-define-command hel-vterm-paste-before (&optional arg)
  "Paste before selection."
  :multiple-cursors t
  (interactive "P")
  (hel-paste #'vterm-yank -1))

;;; .
(provide 'hel-vterm)
;;; hel-vterm.el ends here
