;;; hel-agent-shell.el -*- lexical-binding: t -*-
;;
;; Copyright © 2025-2026 Yuriy Artemyev
;;
;; Author: Yuriy Artemyev <anuvyklack@gmail.com>
;; Maintainer: Yuriy Artemyev <anuvyklack@gmail.com>
;; Version: 0.10.0
;; Homepage: https://github.com/anuvyklack/hel
;; Package-Requires: ((emacs "29.1") (agent-shell "0.50.1")
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; The Hel extension that integrates it with Agent-shell.
;; https://github.com/xenodium/agent-shell
;;
;;; Code:

(eval-when-compile (require 'hel-macros))
(eval-when-compile (require 'hel-multiple-cursors-core))
(eval-when-compile (require 'dash))
(require 'hel-commands)
(require 'agent-shell)

;;; Keybindings

(defvar-keymap hel-agent-shell-local-leader-map
  :doc "<local-leader> in agent-shell mode."
  "RET"   '("set session mode" . agent-shell-set-session-mode)
  "<tab>" '("cycle session mode" . agent-shell-cycle-session-mode)
  ","     '("compose prompt" . agent-shell-prompt-compose)
  "?"     '("help menu" . agent-shell-help-menu)
  "m"     '("set model" . agent-shell-set-session-model)
  "v"     '("switch viewport/shell mode" . agent-shell-other-buffer)
  "s"     '("send screenshot" . agent-shell-send-screenshot)
  "!"     '("shell command" . agent-shell-insert-shell-command-output)
  "p"     '("pending request add" . agent-shell-queue-request)
  "P"     '("pending request remove" agent-shell-remove-pending-request)
  "t"     '("token usage" . agent-shell-show-usage))

(defvar-keymap agent-shell-viewport-view-local-leader-map
  :doc "<local-leader> in agent-shell viewport mode."
  :parent hel-agent-shell-local-leader-map
  "<tab>" '("cycle session mode" . agent-shell-viewport-cycle-session-mode)
  "RET"   '("set session mode" . agent-shell-viewport-set-session-mode)
  "m"     '("set model" . agent-shell-viewport-set-session-model)
  "?"     '("help menu" . agent-shell-viewport-help-menu))

(defvar-keymap agent-shell-viewport-edit-local-leader-map
  :doc "<local-leader> in agent-shell compose buffer."
  :parent agent-shell-viewport-view-local-leader-map
  ","   '("send prompt" . agent-shell-viewport-compose-send)
  "?"   '("help menu" . agent-shell-viewport-compose-help-menu))

(hel-keymap-set agent-shell-mode-map
  "C-r" 'comint-history-isearch-backward-regexp)

(hel-keymap-set agent-shell-mode-map :state 'normal
  "] ]" 'hel-agent-shell-next-item
  "[ [" 'hel-agent-shell-previous-item
  "C-j" 'hel-agent-shell-next-item
  "C-k" 'hel-agent-shell-previous-item
  "z j" 'hel-agent-shell-next-item
  "z k" 'hel-agent-shell-previous-item
  "z u" 'hel-agent-shell-previous-item
  "p"   'hel-agent-shell-paste-dwim
  ","    hel-agent-shell-local-leader-map)

(hel-set-initial-state 'agent-shell-viewport-view-mode 'motion)

(hel-keymap-set agent-shell-viewport-view-mode-map :state 'motion
  "i"   'hel-normal-state
  "h"   'left-char
  "j"   'next-line
  "k"   'previous-line
  "l"   'right-char
  "] ]" 'agent-shell-viewport-next-page
  "[ [" 'agent-shell-viewport-previous-page
  "C-j" 'agent-shell-viewport-next-item
  "C-k" 'agent-shell-viewport-previous-item
  ","    agent-shell-viewport-view-local-leader-map)

(hel-keymap-set agent-shell-viewport-view-mode-map :state 'normal
  "<escape>" 'hel-motion-state
  "q"        'bury-buffer)

(hel-keymap-set agent-shell-viewport-edit-mode-map :state 'normal
  "[ [" 'agent-shell-viewport-compose-peek-last
  "C-k" 'agent-shell-viewport-previous-history
  "C-j" 'agent-shell-viewport-next-history
  "C-r" 'agent-shell-viewport-search-history
  "p"   'hel-agent-shell-paste-dwim
  ;;
  "Z Z" 'agent-shell-viewport-compose-send
  "Z Q" 'agent-shell-viewport-compose-cancel
  ","    agent-shell-viewport-edit-local-leader-map)

(hel-keymap-set agent-shell-diff-mode-map
  "C-j" 'diff-hunk-next
  "C-k" 'diff-hunk-prev)

;;; Commands

;; C-j or ]]
(defun hel-agent-shell-next-item ()
  "Go to next item."
  (declare (modes agent-shell-mode))
  (interactive)
  (let* ((prompt-pos (save-mark-and-excursion
                       (when (comint-next-prompt 1)
                         (point))))
         (block-pos (save-mark-and-excursion
                      (agent-shell-ui-forward-block)))
         (button-pos (save-mark-and-excursion
                       (agent-shell-next-permission-button)))
         (next-pos (->> (list prompt-pos
                              block-pos
                              button-pos)
                        (delq nil)
                        (apply #'min))))
    (when next-pos
      (deactivate-mark)
      (goto-char next-pos)
      (when (eq next-pos prompt-pos)
        (comint-skip-prompt)))))

;; C-k or [[
(defun hel-agent-shell-previous-item ()
  "Go to previous item."
  (declare (modes agent-shell-mode))
  (interactive)
  (let* ((current-pos (point))
         (prompt-pos (save-mark-and-excursion
                       (when-let* (((comint-next-prompt -1))
                                   (pos (point))
                                   ((< pos current-pos)))
                         pos)))
         (block-pos (save-mark-and-excursion
                      (when-let* ((pos (agent-shell-ui-backward-block))
                                  ((< pos current-pos)))
                        pos)))
         (button-pos (save-mark-and-excursion
                       (when-let* ((pos (agent-shell-previous-permission-button))
                                   ((< pos current-pos)))
                         pos)))
         (next-pos (-some->> (list prompt-pos
                                   block-pos
                                   button-pos)
                     (delq nil)
                     (apply #'max))))
    (when next-pos
      (deactivate-mark)
      (goto-char next-pos)
      (when (eq next-pos prompt-pos)
        (comint-skip-prompt)))))

;; p
(hel-define-command hel-agent-shell-paste-dwim (&optional arg)
  "Paste image or text from clipboard into `agent-shell'.

If the clipboard contains an image, save it and insert as file context.
Otherwise, invoke `hel-paste-after' with ARG as usual.

Needs external utilities.  See `agent-shell-clipboard-image-handlers'
for details."
  :multiple-cursors nil
  (declare (modes agent-shell-mode))
  (interactive "*P")
  (if-let* (((window-system))
            (screenshots-dir (agent-shell--dot-subdir "screenshots"))
            (image-path (agent-shell--save-clipboard-image
                         :destination-dir screenshots-dir
                         :no-error t)))
      (agent-shell-insert
       :text (agent-shell--get-files-context :files (list image-path))
       :shell-buffer (agent-shell--shell-buffer))
    ;; else
    (funcall-interactively #'hel-paste-after arg)))

;;; .
(provide 'hel-agent-shell)
;;; hel-agent-shell.el ends here
