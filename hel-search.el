;;; hel-search.el --- Search related functionality -*- lexical-binding: t -*-
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

(eval-when-compile
  (require 'cl-lib)
  (require 'hel-macros))
(require 'dash)
(require 'hel-vars)
(require 'hel-common)
(require 'hel-multiple-cursors-core)
(require 'hel-core)

;;; Old
;;;; Utilities

(defun hel-read-regexp (prompt)
  (let ((message-log-max nil)
        (history-add-new-input nil))
    (read-string prompt nil 'hel-regex-history nil t)))

(defun hel-add-to-regex-history (regex)
  (when (length> regex 2) ;; Reduce the noise level.
    (let ((history-delete-duplicates t))
      (add-to-history 'hel-regex-history regex hel-regex-history-max)))
  (set-register '/ regex)
  (message "Register / set: %s" regex))

(defun hel-last-search-pattern ()
  "Return regexp from \"/\" register."
  (if-let* ((pattern (get-register '/))
            ((stringp pattern))
            ((not (string-empty-p pattern))))
      (hel-pcre-to-elisp pattern)
    (user-error "Register / is empty")))

(defun hel-buffer-visible-ranges (buffer)
  "Return list with (START . END) positions for all windows displaying the buffer."
  (->> (get-buffer-window-list buffer)
       (-map (lambda (win)
               (cons (window-start win)
                     (window-end win))))))

(cl-defun hel-re-search-with-wrap (regexp &optional (direction 1))
  "Search REGEXP from the point toward the DIRECTION.
If nothing found, wrap around the buffer and search up to the point."
  (when (and (use-region-p)
             (/= direction (hel-region-direction)))
    (goto-char (mark-marker)))
  (or (re-search-forward regexp nil t direction)
      ;; If nothing found — wrap around buffer end and try again.
      (let ((point (point)))
        (goto-char (if (< direction 0) (point-max) (point-min)))
        (if (re-search-forward regexp point t direction)
            (message "Wrapped around buffer")))))

(defun hel-regexp-match-ranges (regexp start end &optional invert)
  "Return list of ranges that match REGEXP within [START, END) positions.
If INVERT is non-nil return list with complements of ranges that match REGEXP."
  (save-excursion
    (goto-char start)
    (ignore-errors
      (let (ranges)
        (condition-case nil
            (while (re-search-forward regexp end t)
              (-let (((bounds &as beg . end) (hel-match)))
                ;; Signal if we stack in infinite loop. This happens, for
                ;; example, when regexp consists only of "^" or "$".
                (when (equal bounds (car-safe ranges))
                  (signal 'error nil))
                ;; Skip invisible matches.
                (unless (or (invisible-p beg)
                            (invisible-p (1- end)))
                  (push bounds ranges))))
          (error (setq ranges nil)))
        (cl-callf nreverse ranges)
        (if invert
            (hel-invert-ranges ranges start end)
          ranges)))))

(defun hel-invert-ranges (ranges start end)
  "Return the list with complements to RANGES withing [START, END] positions.

  ((1 . 2) (3 . 4) (5 . 6))  =>  ((start . 1) (2 . 3) (4 . 5) (6 . end))

RANGES is a list of cons cells with positions (START . END)."
  (when ranges
    (let (result)
      (dolist (range ranges)
        (when (< start (car range))
          (push (cons start (car range)) result))
        (setq start (cdr range)))
      (when (< start end)
        (push (cons start end) result))
      (nreverse result))))

;;;; Highlight

(defun hel--put-overlays (ranges face &optional overlays)
  "Paint FACE over RANGES.
Reuse OVERLAYS if provided. Return the list with overlays. "
  (prog1 (->> ranges
              (-map (-lambda ((start . end))
                      (hel-put-highlight start end face '(nil . 50)
                                         (if overlays (pop overlays))))))
    ;; Clean up unused overlays.
    (-each overlays #'delete-overlay)))

;; Slot names beginning with a dash ("-bounds") are private; the convention
;; keeps their accessors double-dashed ("hel-highlight--bounds").
(cl-defstruct (hel-highlight (:constructor hel-highlight-create)
                             (:copier nil) (:predicate nil))
  ( regexp  nil :type string)
  ( buffer  nil :read-only t)
  ( face    nil :read-only t)
  ( bounds  nil :read-only t
    :documentation
    "Optional list of (START . END) scopes within which matches to REGEXP are
highlighted. When nil, scopes are derived from the visible windows and
DIRECTION.")
  ( direction nil :type number
    :documentation
    "Optional. 1 or -1. With nil BOUNDS, the scope spans from point to the
window edge toward DIRECTION (whole window when nil). Ignored if BOUNDS is set.")
  ( invert nil :type boolean :read-only t
    :documentation "Highlight the complement of the matched ranges.")
  ( overlays nil :documentation "Active highlight OVERLAYS."))

(defun hel-highlight-equal (h1 h2)
  "Return t if two `hel-highlight' objects are equal to each other."
  (and h1 h2
       (equal (hel-highlight-regexp h1) (hel-highlight-regexp h2))
       (eq    (hel-highlight-buffer h1) (hel-highlight-buffer h2))
       (eq    (hel-highlight-face h1) (hel-highlight-face h2))
       (equal (hel-highlight-bounds h1) (hel-highlight-bounds h2))
       (eql   (hel-highlight-direction h1) (hel-highlight-direction h2))
       (eq    (hel-highlight-invert h1) (hel-highlight-invert h2))))

(defun hel-highlight-cleanup (hl)
  "Cleanup all highlighting setup by `hel-highlight' object."
  (mapc #'delete-overlay (hel-highlight-overlays hl))
  (setf (hel-highlight-overlays hl) nil))

(defun hel-highlight-ranges (hl)
  "Return list of (START . END) scopes within which HL highlights matches."
  (or (hel-highlight-bounds hl)
      (let ((dir (hel-highlight-direction hl)))
        (->> (get-buffer-window-list)
             (-keep (lambda (win)
                      (when (window-live-p win)
                        (cond ((null dir)
                               (cons (window-start win)
                                     (window-end win)))
                              ((and (< dir 0)
                                    (< (window-start win) (point)))
                               (cons (window-start win)
                                     (min (point) (window-end win))))
                              ((and (< 0 dir)
                                    (< (point) (window-end win)))
                               (cons (max (window-start win) (point))
                                     (window-end win)))))))))))

(defun hel-highlight-update (hl)
  "Recompute and repaint HL's overlays. Return non-nil if there were matches."
  (with-current-buffer (or (hel-highlight-buffer hl)
                           (current-buffer))
    (let* ((regexp (hel-highlight-regexp hl))
           (invert (hel-highlight-invert hl))
           (ranges (and regexp
                        (not (string-empty-p regexp))
                        (-mapcat (-lambda ((beg . end))
                                   (hel-regexp-match-ranges regexp beg end invert))
                                 (hel-highlight-ranges hl)))))
      (setf (hel-highlight-overlays hl)
            (hel--put-overlays ranges
                               (hel-highlight-face hl)
                               (hel-highlight-overlays hl)))
      ranges)))

;;;; Select: s and S

;; TODO: count and folds for matches inside folds (currently skipped).
(defun hel-search-interactively-in-noncontiguous-regions (bounds &optional invert)
  "Return ranges matching an interactively entered regexp within BOUNDS.
BOUNDS is a list of cons cells (START . END) that defines the limits within
which search is performed. With INVERT non-nil, return the complement ranges.
Shows a live preview, a match count, and scrolls the first match into view."
  (let ((mark-active nil) ; temporarily deactivate selection in this function body
        (face 'region)
        regexp overlays timer count-ov
        (start  (make-symbol "hel-select-interactively--start-session"))
        (stop   (make-symbol "hel-select-interactively--stop-session"))
        (after  (make-symbol "hel-select-interactively--after-change"))
        (update (make-symbol "hel-select-interactively--update")))
    (fset start
          (lambda ()
            (add-hook 'after-change-functions after nil t)
            (add-hook 'minibuffer-exit-hook stop nil t)
            (setq count-ov (hel-search--make-count-overlay))
            (with-minibuffer-selected-window
              (setq overlays (hel--put-overlays bounds face)))))
    (fset after
          (lambda (&rest _)
            (-some-> timer (cancel-timer))
            (setq regexp (let ((s (minibuffer-contents-no-properties)))
                           (unless (string-empty-p s)
                             (hel-pcre-to-elisp s))))
            (setq timer (run-at-time hel-update-highlight-delay nil update))))
    (fset update
          (lambda ()
            (with-minibuffer-selected-window
              (let ((ranges (if regexp
                                (-mapcat (-lambda ((beg . end))
                                           (hel-regexp-match-ranges
                                            regexp beg end invert))
                                         bounds))))
                (setq overlays (hel--put-overlays (or ranges bounds) face overlays))
                ;; Scroll the first match into view.
                (when ranges
                  (let ((pos (->> ranges (-map #'car) (apply #'min))))
                    (unless (pos-visible-in-window-p pos)
                      (save-excursion (goto-char pos) (recenter)))))
                ;; Show the match count.
                (when (overlayp count-ov)
                  (overlay-put count-ov 'after-string
                               (if ranges
                                   (format " [%d]" (length ranges)))))))))
    (fset stop
          (lambda ()
            (-some-> timer (cancel-timer))
            (-some-> count-ov (delete-overlay))
            (-each overlays #'delete-overlay)))
    ;; main
    (when-let* ((pattern (condition-case nil
                             (minibuffer-with-setup-hook start
                               (hel-read-regexp (if invert "split: " "select: ")))
                           (quit))) ;; "C-g"
                ((stringp pattern))
                ((not (string-empty-p pattern)))
                (regexp (hel-pcre-to-elisp pattern))
                (ranges (-mapcat (-lambda ((beg . end))
                                   (hel-regexp-match-ranges regexp beg end invert))
                                 bounds)))
      (hel-add-to-regex-history pattern)
      ranges)))

;;;; Filter: K and M-K

(defvar hel-filter--regions-overlays nil "List of fake regions overlays.")
(defvar hel-filter--regions-contents nil "List of fake regions content.")
(defvar hel-filter--invert nil)
(defvar hel-filter--timer nil "Debounce timer for the filter live preview.")
(defvar hel-filter--count-ov nil "Minibuffer overlay for the kept count.")

(defun hel-filter-selections (&optional invert)
  "Keep selections that match regexp entered.
If INVERT is non-nil — remove selections that match regexp."
  (unless hel-multiple-cursors-mode
    (user-error "No multiple selections"))
  (hel-with-real-cursor-as-fake
    (let* ((cursors (hel-all-fake-cursors))
           (regions-overlays (-map (lambda (cursor)
                                     (overlay-get cursor 'fake-region))
                                   cursors))
           (regions-contents (-map (lambda (cursor)
                                     (buffer-substring-no-properties
                                      (overlay-get cursor 'point)
                                      (overlay-get cursor 'mark)))
                                   cursors)))
      (setq hel-filter--regions-overlays regions-overlays
            hel-filter--regions-contents regions-contents
            hel-filter--invert invert)
      (deactivate-mark)
      (-each cursors #'delete-overlay)
      (if-let* ((pattern (condition-case nil
                             (minibuffer-with-setup-hook #'hel-filter--start-session
                               (hel-read-regexp (if invert "remove: " "keep: ")))
                           (quit)))
                ((not (string-empty-p pattern)))
                (regexp (hel-pcre-to-elisp pattern))
                (flags (-map (lambda (str)
                               (xor (string-match regexp str)
                                    hel-filter--invert))
                             hel-filter--regions-contents))
                ((-contains? flags t)))
          (cl-loop for cursor in cursors
                   for flag in flags
                   do (if flag
                          (progn
                            (hel--set-cursor-overlay cursor (overlay-get cursor 'point))
                            (overlay-put (overlay-get cursor 'fake-region)
                                         'face 'region))
                        (hel--delete-fake-cursor cursor)))
        ;; Else restore all cursors
        (dolist (cursor cursors)
          (hel--set-cursor-overlay cursor (overlay-get cursor 'point))
          (overlay-put (overlay-get cursor 'fake-region)
                       'face 'region))))))

(defun hel-filter--start-session ()
  (add-hook 'after-change-functions #'hel-filter--update-hook nil t)
  (add-hook 'minibuffer-exit-hook #'hel-filter--stop-session nil t)
  (setq hel-filter--count-ov (hel-search--make-count-overlay)))

(defun hel-filter--stop-session ()
  (when hel-filter--timer
    (cancel-timer hel-filter--timer)
    (setq hel-filter--timer nil))
  (when hel-filter--count-ov
    (delete-overlay hel-filter--count-ov)
    (setq hel-filter--count-ov nil)))

(defun hel-filter--update-hook (&rest _)
  (when hel-filter--timer
    (cancel-timer hel-filter--timer))
  (setq hel-filter--timer
        (run-at-time hel-update-highlight-delay nil
                     #'hel-filter--do-update)))

(defun hel-filter--do-update ()
  "Highlight current matches during a filter selections session."
  (let* ((regions-overlays hel-filter--regions-overlays)
         (pattern (minibuffer-contents-no-properties))
         (regexp (unless (string-empty-p pattern) (hel-pcre-to-elisp pattern)))
         (flags (and regexp
                     (let ((flags (-map (lambda (str)
                                          (if (string-match regexp str) t))
                                        hel-filter--regions-contents)))
                       (if hel-filter--invert (-map #'not flags) flags)))))
    (if (and flags (-contains? flags t))
        (progn
          (cl-loop for overlay in regions-overlays
                   for flag in flags
                   do (overlay-put overlay 'face (if flag 'region)))
          (when (overlayp hel-filter--count-ov)
            (overlay-put hel-filter--count-ov 'after-string
                         (format " [%d/%d]" (-count #'identity flags)
                                 (length flags)))))
      ;; Else highlight all regions.
      (dolist (ov regions-overlays)
        (overlay-put ov 'face 'region))
      (when (overlayp hel-filter--count-ov)
        (overlay-put hel-filter--count-ov 'after-string nil)))))

;;;; Find char: f F t T

(defun hel-find-char (char direction exclusive?)
  (let* ((case (let (case-fold-search)
                 (not (string-match-p "[A-Z]" (char-to-string char)))))
         (pattern (pcase char
                    (?\t "\t") ;; TAB
                    ((or ?\r ?\n) "\n") ;; RET
                    ;; (?\e) ;; ESC
                    ;; (?\d) ;; DEL <backspace>
                    ;; (_ (char-fold-to-regexp (char-to-string char)))
                    (_ (regexp-quote (char-to-string char)))))
         (hl (hel-highlight-create :buffer (current-buffer)
                                   :regexp pattern
                                   :face 'hel-search-highlight))
         (case-fold-search case)
         (deactivate-mark nil))
    (cl-flet ((search (dir)
                (let ((case-fold-search case))
                  (if exclusive?
                      (cond ((<= 0 dir direction) ;; t n
                             (forward-char))
                            ((<= dir direction 0) ;; T n
                             (backward-char)))
                    ;; else
                    (cond ((< dir 0 direction) ;; f N
                           (backward-char))
                          ((< direction 0 dir) ;; F N
                           (forward-char))))
                  ;; Search through folds (visible? nil): on a match inside a
                  ;; closed overlay fold, `hel-search--reveal-position' below
                  ;; opens it. Passing visible? non-nil would skip folded
                  ;; matches and hit a latent arg-misalignment in `hel-search'.
                  (if (hel-search pattern dir nil t)
                      (prog1 t
                        (setf (hel-highlight-direction hl) dir)
                        (save-match-data
                          (hel-highlight-update hl))
                        (if exclusive?
                            (cond ((<= 0 dir direction) ;; t n
                                   (backward-char))
                                  ((<= dir direction 0) ;; T n
                                   (forward-char)))
                          ;; not exclusive?
                          (cond ((< dir 0 direction) ;; f N
                                 (forward-char))
                                ((< direction 0 dir) ;; F N
                                 (backward-char))))
                        ;; Reveal the fold at the landing position.
                        (hel-search--reveal-position (point)))
                    ;; else
                    (prog1 nil
                      (hel-highlight-cleanup hl))))))
      (when (search direction)
        (let ((next (lambda () (interactive) (search direction)))
              (prev (lambda () (interactive) (search (- direction))))
              (on-exit (lambda () (hel-highlight-cleanup hl))))
          (set-transient-map (define-keymap
                               "n" next
                               "N" prev)
                             t on-exit))))))

;;; New
;;;; Customization

(defgroup hel-search nil
  "Hel search functionality."
  :prefix 'hel-search-)

(defcustom hel-search-rehide-folds t
  "If non-nil, re-hide temporary opened folds when cursor moves out of them."
  :type 'boolean
  :group 'hel-search)

;;;;; Lazy highlight customization

(defgroup hel-lazy-highlight nil
  "Lazy highlighting feature for matching strings."
  :prefix "hel-lazy-highlight-"
  :group 'hel-search)

(defcustom hel-lazy-highlight-cleanup t
  "Controls whether to remove extra highlighting after a search.
If this is nil, extra highlighting can be \"manually\" removed with
\\[hel-lazy-highlight-cleanup]."
  :type 'boolean
  :group 'hel-lazy-highlight)

(defcustom hel-lazy-highlight-initial-delay 0.25
  "Seconds to wait before beginning to lazily highlight all matches.
This setting only has effect when the search string is shorter than
`hel-lazy-highlight-no-delay-length' characters."
  :type 'number
  :group 'hel-lazy-highlight)

(defcustom hel-lazy-highlight-no-delay-length 3
  "For search strings at least this long, lazy highlight starts immediately.
For shorter search strings, `hel-lazy-highlight-initial-delay' applies."
  :type 'integer
  :group 'hel-lazy-highlight)

(defcustom hel-lazy-highlight-interval 0 ; 0.0625
  "Seconds between successive lazily highlighting rounds."
  :type 'number
  :group 'hel-lazy-highlight)

(defcustom hel-lazy-highlight-buffer-max-at-a-time 200 ; 20 (bug#48581)
  "Maximum matches to highlight at a time in buffer scanning phase.
A value of nil means highlight all matches in the buffer."
  :type '(choice (const :tag "All" nil)
                 (integer :tag "Some"))
  :group 'hel-lazy-highlight)

;;;; Utils

(cl-defun +hel-search (regexp &optional bound (direction 1))
  "Find the first match for the REGEXP toward the DIRECTION.
Return list (MATCH-DATA OVERLAYS) where:
- MATCH-DATA is the same as `match-data' returns;
- OVERLAYS is a list with openable overlays that currently hide the match.

This function modifies the match data that `match-beginning',
`match-end' and `match-data' access."
  (let (found)
    (while (and (not found)
                (re-search-forward regexp bound t direction))
      (when-let* ((visible (hel-range-visible? (match-beginning 0) (match-end 0))))
        (setq found (list (match-data) (if (consp visible)
                                           visible)))))
    found))

(cl-defun hel-match (&optional (match-data (match-data)))
  "Return cons cell with bounds of the first match group in `match-data'.
If there were no match groups in the last used regexp — return the bounds
of the full regexp match."
  (setq match-data (-partition 2 match-data))
  (-let [(start end) (or (-second-item match-data)
                         (-first-item match-data))]
    (cons start end))
  ;; (set-match-data match-data)
  ;; (if (match-beginning 1)
  ;;     (cons (match-beginning 1) (match-end 1))
  ;;   (cons (match-beginning 0) (match-end 0)))
  )

(defun hel-put-highlight (start end face &optional priority overlay)
  (-doto (if overlay
             (move-overlay overlay start end)
           ;; Bug#77121: highlight overlays must be non-sticky at both ends.
           (make-overlay start end nil t nil))
    (overlay-put 'face face)
    (overlay-put 'priority priority)
    (overlay-put 'modification-hooks (list
                                      (lambda (ov flag _beg _end &optional _len)
                                        (when flag (delete-overlay ov)))))))

(defun hel-search-highlight-all-matches (match-data &optional overlays)
  "Highlight all submatches in MATCH-DATA.
Return list with overlays. Reuse OVERLAYS if provided.

The faces used to do the highlights are named `isearch-group-1', `isearch-group-2',
etc. (By default, only these 2 are defined.) When there are more matches than
faces, then faces are reused from the beginning, in a cyclical manner, so the
`isearch-group-1' face is isreused for the third match. If you want to use more
distinctive colors, you can define more of these faces using the same numbering
scheme."
  (setq match-data (-partition 2 match-data))
  (prog1 (cons
          ;; Whole match
          (-let [(start end) (car match-data)]
            (hel-put-highlight start end 'isearch '(nil . 51)
                               (if overlays (pop overlays))))
          ;; Submatches
          (let ((group 0))
            (->> (cdr match-data)
                 (-map (-lambda ((start end))
                         (when (and (integer-or-marker-p start)
                                    (integer-or-marker-p end))
                           (let* ((next (intern-soft (format "isearch-group-%d"
                                                             (cl-incf group))))
                                  (face (if (facep next)
                                            next
                                          (setq group 0)
                                          'isearch-group-1)))
                             (hel-put-highlight start end face '(nil . 52)
                                                (if overlays (pop overlays))))))))))
    ;; Remove remaining overlays
    (-each overlays #'delete-overlay)))

;;;; Search session

(cl-defstruct (hel-search-session (:constructor hel-search-session--create)
                                  (:predicate nil)
                                  (:copier nil))
  ;; Public
  regexp
  (start    nil :documentation "Limit that bounds the search area.")
  (end      nil :documentation "Limit that bounds the search area.")
  (callback nil :documentation "Executed when searching session is complete.")
  ;; Private
  (counter  0   :documentation "Running counter.")
  (total    nil :documentation "Total matches; nil while scanning in progress.")
  (timer    nil :documentation "Active timer for the scanning pipeline.")
  (scan-pos nil :documentation "Where the buffer scan left off.")
  ;; It is a list during scanning and a sorted vector after the scan completes.
  (overlays nil :documentation "Search matches overlays.")
  (buffer nil :read-only t)
  (buffer-hash nil))

(cl-defun hel-search-session-create ( &optional regexp start end
                                      &key callback)
  "Create `hel-search-session' object.
Run search session if REGEXP is provided."
  (let ((self (hel-search-session--create
               :regexp regexp
               :start start :end end
               :callback (or callback 'hel-search-session--update-modeline)
               :buffer (current-buffer)
               :buffer-hash (buffer-hash))))
    (when regexp
      (setf (hel-search-session-timer self)
            (run-at-time nil nil 'hel-search-session--scan-window self)))
    self))

(defun hel-search-session-cleanup (self)
  "Destructor for `hel-search-session' objects."
  (-some-> (hel-search-session-timer self) (cancel-timer))
  ;; The overlays slot may be either a list (during a scan) or a vector (after
  ;; scan is completed); `mapc' iterates both.
  (mapc #'delete-overlay (hel-search-session-overlays self))
  (setf (hel-search-session-counter self)     0
        (hel-search-session-total self)       nil
        (hel-search-session-scan-pos self)    nil
        (hel-search-session-overlays self)    nil
        (hel-search-session-timer self)       nil
        (hel-search-session-buffer-hash self) nil))

(defun hel-search-session-restart (self &optional regexp)
  (hel-search-session-cleanup self)
  (if regexp
      (setf (hel-search-session-regexp self) regexp)
    (setf regexp (hel-search-session-regexp self)))
  (when regexp
    (setf (hel-search-session-buffer-hash self)
          (buffer-hash (hel-search-session-buffer self)))
    (setf (hel-search-session-timer self)
          (run-at-time (if (length< regexp hel-lazy-highlight-no-delay-length)
                           hel-lazy-highlight-initial-delay)
                       nil 'hel-search-session--scan-window self))))

(defun hel-search-session--scan-window (self)
  (if (not (buffer-live-p (hel-search-session-buffer self)))
      (hel-search-session-cleanup self)
    (with-current-buffer (hel-search-session-buffer self)
      (let ((regexp (hel-search-session-regexp self))
            (search-start (max (or (hel-search-session-start self) (point-min))
                               (window-group-start)))
            (search-end   (min (or (hel-search-session-end self) (point-max))
                               (window-group-end)))
            window-overlays)
        (save-excursion
          (goto-char search-start)
          (let (match)
            (while (setq match (+hel-search regexp search-end))
              (-let* (((match-data _closed-overlays) match)
                      ((beg . end) (hel-match match-data)))
                (if (= beg end)
                    ;; Ensure forward progress on zero-length matches like
                    ;; "^" or "$" to avoid an infinite loop.
                    (unless (eobp) (forward-char 1))
                  (push (hel-put-highlight beg end 'lazy-highlight '(nil . 50))
                        window-overlays))))))
        (cl-callf nreverse window-overlays)
        (setf (hel-search-session-timer self)
              (run-at-time hel-lazy-highlight-interval nil
                           'hel-search-session--scan-buffer
                           self window-overlays))))))

(defun hel-search-session--scan-buffer (self window-overlays)
  (if (not (buffer-live-p (hel-search-session-buffer self)))
      (hel-search-session-cleanup self)
    (with-current-buffer (hel-search-session-buffer self)
      (let ((regexp (hel-search-session-regexp self))
            (start (or (hel-search-session-scan-pos self)
                       (hel-search-session-start self)
                       (point-min)))
            (end   (or (hel-search-session-end self) (point-max)))
            (n 0)
            match)
        (save-excursion
          (goto-char start)
          (while (and (< n hel-lazy-highlight-buffer-max-at-a-time)
                      (setq match (+hel-search regexp end)))
            (-let* (((match-data _closed-overlays) match)
                    ((beg . end) (hel-match match-data)))
              (cl-incf n)
              (if (= beg end)
                  ;; Zero-width match: advance to avoid an infinite loop.
                  ;; Can be when regexp consists only of "^" or "$".
                  (unless (eobp) (forward-char 1))
                (if (and window-overlays
                         (= beg (overlay-start (car window-overlays))))
                    (progn
                      (cl-callf + (hel-search-session-counter self)
                                  (length window-overlays))
                      (setf (hel-search-session-overlays self)
                            (nconc (nreverse window-overlays)
                                   (hel-search-session-overlays self)))
                      (setq window-overlays nil)
                      (goto-char (-> (hel-search-session-overlays self)
                                     (-first-item)
                                     (overlay-end))))
                  ;; else
                  (cl-incf (hel-search-session-counter self))
                  (push (hel-put-highlight beg end 'lazy-highlight '(nil . 50))
                        (hel-search-session-overlays self))))))
          (setf (hel-search-session-scan-pos self) (point)))
        (if (>= n hel-lazy-highlight-buffer-max-at-a-time)
            ;; Limit hit: reschedule the next cycle.
            (setf (hel-search-session-timer self)
                  (run-at-time hel-lazy-highlight-interval nil
                               'hel-search-session--scan-buffer
                               self window-overlays))
          ;; Search finished.
          (setf (hel-search-session-timer self) nil
                (hel-search-session-total self) (hel-search-session-counter self))
          ;; Reverse list and convert it to vector.
          (setf (hel-search-session-overlays self)
                (vconcat (nreverse (hel-search-session-overlays self))))
          (-some-> (hel-search-session-callback self) (funcall self)))))))

(defun hel-search-session-next-match (self direction)
  "Find the next match from point in DIRECTION.
Return (START END OVERLAYS INDEX) list where:
- START, END are bounds of match;
- OVERLAYS is a list with openable overlays that currently hide the match.
- INDEX"
  (save-excursion
    (let ((pos (if (use-region-p)
                   (if (< direction 0) (region-beginning) (region-end))
                 (point)))
          (total (hel-search-session-total self)))
      (if total
          ;; Search session is complete.
          (unless (= total 0)
            (let* ((overlays (hel-search-session-overlays self))
                   (index (if (< 0 direction)
                              (hel-search-session--next-match self pos)
                            (hel-search-session--previous-match self pos)))
                   (overlay (elt overlays index))
                   (start (overlay-start overlay))
                   (end (overlay-end overlay))
                   (closed-overlays (let ((val (hel-range-visible? start end)))
                                      (if (consp val) val))))
              (list start end closed-overlays index)))
        ;; else
        (if-let* ((regexp (hel-search-session-regexp self))
                  (match (or (+hel-search regexp nil direction)
                             (progn
                               (goto-char (if (< 0 direction)
                                              (point-min)
                                            (point-max)))
                               (+hel-search regexp pos direction)))))
            (-let* (((match-data closed-overlays) match)
                    ((start . end) (hel-match match-data)))
              (list start end closed-overlays nil)))))))

(defun hel-search-session--next-match (self pos)
  "Return the index of the next overlay that starts after POS."
  (let* ((overlays (hel-search-session-overlays self))
         (low 0)
         (high (hel-search-session-total self))
         (mid 0)
         (result 0))
    (while (< low high)
      (setq mid (/ (+ low high) 2))
      (if (<= pos (overlay-start (elt overlays mid)))
          (setq result mid
                high mid)
        (setq low (1+ mid))))
    result))

(defun hel-search-session--previous-match (self pos)
  "Return the index of the previous overlay that ends before POS."
  (let ((overlays (hel-search-session-overlays self))
        (low 0)
        (high (hel-search-session-total self))
        (mid 0)
        (result (1- (hel-search-session-total self))))
    (while (< low high)
      (setq mid (/ (+ low high) 2))
      (if (<= (overlay-end (elt overlays mid)) pos)
          (setq result mid
                low (1+ mid))
        (setq high mid)))
    result))

(defun hel-search-session--current-match (self pos)
  (let ((overlays (hel-search-session-overlays self))
        (low 0)
        (high (hel-search-session-total self))
        (mid 0)
        ov found)
    (while (and (not found) (< low high))
      (setq mid (/ (+ low high) 2)
            ov (elt overlays mid))
      (cond ((<= (overlay-start ov) pos (overlay-end ov))
             (setq found ov))
            ((< pos (overlay-start ov))
             (setq high mid))
            (t
             (setq low (1+ mid)))))
    (if found mid)))

(defun hel-search-session--update-modeline (ss)
  (with-current-buffer (hel-search-session-buffer ss)
    (if-let* ((total (hel-search-session-total ss))
              ((not (zerop total))))
        (progn
          (setq hel-search--total total
                hel-search--current (-some-> (hel-search-session--current-match ss (point))
                                      (1+)))
          (add-hook 'post-command-hook 'hel-search--clean-current 95 t))
      ;; else
      (setq hel-search--total nil
            hel-search--current nil))
    (force-mode-line-update)))

(defun hel-search--clean-current ()
  (if-let* ((current hel-search--current)
            (ss hel-search--session)
            (overlays (hel-search-session-overlays ss))
            (ov (elt overlays (1- current)))
            ((<= (overlay-start ov) (point) (overlay-end ov))))
      nil
    (remove-hook 'post-command-hook 'hel-search--clean-current t)
    (setq hel-search--current nil)
    (force-mode-line-update)))

;;;; Search: /, ?, n, N, *

(cl-defun hel-search-interactively (prompt &optional (direction 1))
  (redisplay) ; To ensure `window-start' position is not stale.
  (let* ((ss (hel-search-session--create
              :buffer (current-buffer)
              :buffer-hash (buffer-hash)
              :callback #'hel-search-session--update-modeline))
         (point (point))
         (win-start (window-start))
         (win-hscroll (window-hscroll))
         target opened-overlays)
    (cl-flet*
        ((update (regexp)
           (with-minibuffer-selected-window
             (hel-recenter-point-on-jump
               (goto-char point)
               (set-window-start nil win-start :noforce)
               (set-window-hscroll nil win-hscroll)
               (if-let* ((regexp)
                         (match (progn
                                  (hel-search-session-restart ss regexp)
                                  (hel-search-session-next-match ss direction))))
                   (-let [(start end closed-overlays) match]
                     (goto-char (if (< direction 0) start end))
                     (setq target (hel-put-highlight start end 'region 100 target))
                     (-each closed-overlays #'hel-temporary-open-overlay)
                     (cl-callf append opened-overlays closed-overlays))
                 ;; else
                 (-some-> target (delete-overlay))
                 (hel-search-session-cleanup ss)
                 (setq target nil
                       hel-search--total nil
                       hel-search--current nil)
                 (let ((message-log-max nil))
                   (message (propertize "No matches" 'face 'error)))))))
         (after-change (_beg _end _len)
           (hel-search-session-cleanup ss)
           (unless (input-pending-p)
             (update (let ((s (minibuffer-contents-no-properties)))
                       (unless (string-empty-p s)
                         (hel-pcre-to-elisp s))))))
         (start ()
           (add-hook 'after-change-functions #'after-change nil t)))
      ;; main
      (deactivate-mark)
      (when-let* ((input (condition-case nil
                             (minibuffer-with-setup-hook #'start
                               (hel-read-regexp prompt))
                           (t
                            (hel-search-session-cleanup ss))))
                  ((not (string-empty-p input))))
        (hel-add-to-regex-history input)
        (let ((regexp (hel-pcre-to-elisp input)))
          (unless (equal regexp (hel-search-session-regexp ss))
            (update regexp)))
        (when target
          (hel-recenter-point-on-jump
            (hel-set-region (overlay-start target) (overlay-end target)))
          (delete-overlay target))
        (-let [(open close)
               (->> opened-overlays
                    (-separate (lambda (ov)
                                 (and (< (overlay-start ov) (point))
			              (<= (point) (overlay-end ov))))))]
          (-each open #'hel-open-overlay)
          (-each close #'hel-close-temrporary-opened-overlay))
        ss))))

;; /
(hel-define-command hel-search-forward ()
  "Search forward for an interactively entered regexp; select the first match."
  :multiple-cursors nil
  :merge-selections t
  (interactive)
  (-some-> hel-search--session (hel-search-session-cleanup))
  (setq hel-search--current nil
        hel-search--total nil
        hel-search--session (hel-search-interactively "/" 1)
        hel-search--direction 1)
  (add-hook 'pre-command-hook 'hel-search--clean-current 95 t))

;; ?
(hel-define-command hel-search-backward ()
  "Search backward for an interactively entered regexp; select the first match."
  :multiple-cursors nil
  :merge-selections t
  (interactive)
  (-some-> hel-search--session (hel-search-session-cleanup))
  (setq hel-search--current nil
        hel-search--total nil
        hel-search--session (hel-search-interactively "?" -1)
        hel-search--direction -1))

;; n
(hel-define-command hel-search-next (count)
  "Select next COUNT search match."
  :multiple-cursors nil
  :merge-selections t
  (interactive "p")
  (when-let* ((regexp (hel-last-search-pattern)))
    (if (memq last-command '(hel-search-forward    ; /
                             hel-search-backward   ; ?
                             hel-search-next       ; n
                             hel-search-previous)) ; N
        ;; After "?" command, "n" and "N" keys are swapped:
        ;; "n" jump backward, "N" — forward.
        (if (< hel-search--direction 0) (cl-callf - count))
      ;; else
      (setq hel-search--direction 1)
      (hel-push-point))
    ;;
    (if-let* ((ss hel-search--session))
        (unless (and (equal (buffer-hash) (hel-search-session-buffer-hash ss))
                     (equal regexp (hel-search-session-regexp ss)))
          (hel-search-session-restart ss regexp))
      (setq hel-search--session (hel-search-session-create regexp)))
    ;;
    (let ((region-dir (if (use-region-p) (hel-region-direction) 1)))
      (hel-recenter-point-on-jump
        (hel-motion-loop (dir count)
          (when-let* ((match (hel-search-session-next-match hel-search--session dir)))
            (-let [(start end closed-overlays index) match]
              (when (and hel--extend-selection (use-region-p))
                (hel-create-fake-cursor-from-point))
              (hel-set-region start end region-dir)
              (-each closed-overlays #'hel-open-overlay)
              (setq hel-search--current (if index (1+ index)))
              (add-hook 'post-command-hook 'hel-search--clean-current 95 t))))))))

;; N
(hel-define-command hel-search-previous (count)
  "Select previous COUNT search match."
  :multiple-cursors nil
  :merge-selections t
  (interactive "p")
  (hel-search-next (- count)))

;; *
(hel-define-command hel-construct-search-pattern ()
  "Construct search pattern from all current selections and store it to / register.
Auto-detect word boundaries at the beginning and end of the search pattern."
  :multiple-cursors nil
  (interactive)
  (let ((quote-fn (if hel-use-pcre-regex #'rxt-quote-pcre #'regexp-quote))
        patterns)
    (hel-with-each-cursor
      (when (use-region-p)
        (let* ((beg (region-beginning))
               (end (region-end))
               (open-word-boundary
                (cond ((= beg (pos-bol))
                       (->> (buffer-substring-no-properties beg (1+ beg))
                            (string-match-p "[[:word:]]")))
                      (t
                       (->> (buffer-substring-no-properties (1- beg) (1+ beg))
                            (string-match-p "[^[:word:]][[:word:]]")))))
               (close-word-boundary
                (cond ((= end (pos-eol))
                       (->> (buffer-substring-no-properties (1- end) end)
                            (string-match-p "[[:word:]]")))
                      (t
                       (->> (buffer-substring-no-properties (1- end) (1+ end))
                            (string-match-p "[[:word:]][^[:word:]]")))))
               (string (->> (buffer-substring-no-properties (point) (mark))
                            (funcall quote-fn))))
          (push (concat (if open-word-boundary "\\b")
                        string
                        (if close-word-boundary "\\b"))
                patterns))))
    (setq patterns (nreverse (-uniq patterns)))
    (when patterns
      (let* ((separator (if hel-use-pcre-regex "|" "\\|"))
             (regexp (apply #'concat (-interpose separator patterns))))
        (hel-add-to-regex-history regexp)
        (-some-> hel-search--session (hel-search-session-cleanup))
        (setq hel-search--session (hel-search-session-create
                                   (hel-pcre-to-elisp regexp)))))))

;; M-*
(hel-define-command hel-construct-search-pattern-no-bounds ()
  "Construct search pattern from all current selection and store it to / register.
Do not auto-detect word boundaries in the search pattern."
  :multiple-cursors nil
  (interactive)
  (let ((quote (if hel-use-pcre-regex #'rxt-quote-pcre #'regexp-quote))
        patterns)
    (hel-with-each-cursor
      (when (use-region-p)
        (push (funcall quote (buffer-substring-no-properties (point) (mark)))
              patterns)))
    (cl-callf nreverse patterns)
    (when patterns
      (let* ((separator (if hel-use-pcre-regex "|" "\\|"))
             (regexp (apply #'concat (-interpose separator patterns))))
        (hel-add-to-regex-history regexp)
        (-some-> hel-search--session (hel-search-session-cleanup))
        (setq hel-search--session (hel-search-session-create
                                   (hel-pcre-to-elisp regexp)))))))

;;; .
(provide 'hel-search)
;;; hel-search.el ends here
