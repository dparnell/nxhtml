;;; n-back.el --- n-back game
;;
;; Author: Lennart Borgman (lennart O borgman A gmail O com)
;; Created: 2009-05-23 Sat
;; Version:
;; Last-Updated: 2009-05-24 Sun
;; URL:
;; Keywords:
;; Compatibility:
;;
;; Features that might be required by this library:
;;
  ;; `winsize'.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; n-back game for brain training.  See `n-back' for more information.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change log:
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

(require 'winsize) ;; Ehum...
;;(require 'new-key-seq-widget)

(defvar n-back-game-window nil)
(defvar n-back-game-buffer nil)

(defvar n-back-ctrl-window nil)
(defvar n-back-ctrl-buffer nil)

(defvar n-back-info-window nil)
(defvar n-back-info-buffer nil)

(defgroup n-back nil
  "Customizations for `n-back' game.
Bug: does not work without this line???"
  :group 'games)

(defcustom n-back-level 1
  "n-back level."
  :type '(radio (const 1)
                 (const 2)
                 (const 3)
                 (const 4))
  :group 'n-back)

(defcustom n-back-active-match-types '(position color)
  "Active match types."
  :type '(set (const position)
              (const color)
              (const sound))
  :group 'n-back)

(defcustom n-back-colors
  '("red" "green" "yellow" "pink" "gray" "light blue")
  "Random colors to display."
  :type '(repeat color)
  :group 'n-back)

(defcustom n-back-words "you cat going me forest crying brown"
  "Random words to display."
  :type 'string
  :group 'n-back)

(defvar n-back-sound-files nil)
;;(n-back-get-sound-files)
(defun n-back-get-sound-files ()
  (let ((dir (nth 0 n-back-sounds))
        (regexp (nth 1 n-back-sounds)))
    (setq n-back-sound-files (directory-files dir nil regexp))))

(defcustom n-back-sounds '("c:/program files/brain workshop/res" "piano-")
  "Random sounds location."
  :type '(list (directory :tag "Directory")
               (regexp :tag "File name regexp"))
  :group 'n-back)

(defun n-back-toggle-position ()
  (interactive)
  (n-back-toggle 'position))

(defun n-back-toggle-color ()
  (interactive)
  (n-back-toggle 'color))

(defun n-back-toggle-sound ()
  (interactive)
  (n-back-toggle 'sound))

(defun n-back-toggle (match-type)
  (if (memq match-type n-back-active-match-types)
      (setq n-back-active-match-types (delq match-type n-back-active-match-types))
    (setq n-back-active-match-types (cons match-type n-back-active-match-types)))
  (setq n-back-active-match-types
        (sort n-back-active-match-types
              (lambda (a b)
                (let ((all '(position color sound)))
                  (< (length (memq a all))
                     (length (memq b all)))))))
  (n-back-init-control-status)
  (n-back-update-control-buffer)
  (n-back-update-info))

(defvar n-back-control-mode-map nil)

(defun n-back-decrease-speed ()
  (interactive)
  (setq n-back-sec-per-trial (+ n-back-sec-per-trial 0.25))
  (when (> n-back-sec-per-trial 5.0)
    (setq n-back-sec-per-trial 5.0))
  (n-back-update-info))

(defun n-back-increase-speed ()
  (interactive)
  (setq n-back-sec-per-trial (- n-back-sec-per-trial 0.25))
  (when (< n-back-sec-per-trial 1.0)
    (setq n-back-sec-per-trial 1.0))
  (n-back-update-info))

(defun n-back-help ()
  (interactive)
  (describe-function 'n-back))

(defun n-back-change-level (level)
  (interactive (progn
                 (if (and (numberp last-input-event)
                          (>= last-input-event ?1)
                          (<= last-input-event ?9))
                     (list (- last-input-event ?0))
                   (list (string-to-int (read-string "Level: "))))))
  (setq n-back-level level)
  (n-back-update-control-buffer)
  (n-back-update-info)
  )

(defun n-back-make-keymap ()
  (let ((map (make-sparse-keymap)))
    (define-key map [?1] 'n-back-change-level)
    (define-key map [?2] 'n-back-change-level)
    (define-key map [?3] 'n-back-change-level)
    (define-key map [?4] 'n-back-change-level)
    (define-key map [?5] 'n-back-change-level)
    (define-key map [?6] 'n-back-change-level)
    (define-key map [??] 'n-back-help)
    (define-key map [?\ ] 'n-back-play)
    (define-key map [(control ?g)] 'n-back-stop)
    (define-key map [?-] 'n-back-decrease-speed)
    (define-key map [?+] 'n-back-increase-speed)
    (define-key map [?t ?p] 'n-back-toggle-position)
    (define-key map [?t ?c] 'n-back-toggle-color)
    (define-key map [?t ?s] 'n-back-toggle-sound)
    (define-key map (n-back-key-binding 'position) 'n-back-position-answer)
    (define-key map (n-back-key-binding 'color)    'n-back-color-answer)
    (define-key map (n-back-key-binding 'sound)    'n-back-sound-answer)
    ;;(define-key map [t] 'ignore)
    (setq n-back-control-mode-map map)))

(defun n-back-key-binding (what)
  (nth
   (case what
    (position 0)
    (color    1)
    (sound    2))
   n-back-keys))

(defcustom n-back-keys
  '(
    [?a]
    [?f]
    [?l]
    )
  "Key bindings for answering."
  :type '(list
          (key-sequence :tag "position key")
          (key-sequence :tag "color key")
          (key-sequence :tag "sound key")
          )
  :set (lambda (sym val)
         (set-default sym val)
         (n-back-make-keymap))
  :group 'n-back)

(defcustom n-back-sec-per-trial 3.0
  "Seconds per trial."
  :type 'float
  :group 'n-back)

;;;###autoload
(defun n-back ()
  "Start n-back game.
Just follow the on screen instructions to play the game.

This game is shamelessly modeled after Brain Workshop, see URL
`http://brainworkshop.sourceforge.net/'. Not all features there
are implemented here."
  (interactive)
  (n-back-cancel-timers)
  (n-back-setup-windows)
  (n-back-init-control-status)
  )

(defconst n-back-match-types
  '((position ": position match" nil)
    (color    ": color match" nil)
    (sound    ": sound match" nil)
    ))

(defconst n-back-control-status nil)

;;(n-back-set-match-status 'position 'bad)
(defun n-back-set-match-status (match-type status)
  (unless (memq status '(ok bad nil)) (error "bad status=%s" status))
  (let ((entry (assoc match-type n-back-control-status)))
    (setcar (cddr entry) status)
    ))

;;(n-back-clear-match-status)
(defun n-back-clear-match-status ()
  (dolist (entry n-back-control-status)
    (setcar (cddr entry) nil)
    ))

;; (n-back-init-control-status)
(defun n-back-init-control-status ()
  (setq n-back-control-status nil)
  (dolist (what n-back-active-match-types)
    (setq n-back-control-status
          (cons (assoc what n-back-match-types)
                n-back-control-status))))

(defsubst n-back-is-playing ()
  (timerp n-back-timer))

;;(n-back-update-control-buffer)
(defun n-back-update-control-buffer ()
  (with-current-buffer n-back-ctrl-buffer
    (setq buffer-read-only nil)
    (erase-buffer)
    (insert (propertize (format "%s %s-back"
                                (let ((n (length n-back-active-match-types)))
                                  (cond
                                   ((= 1 n) "Single")
                                   ((= 2 n) "Dual")
                                   ((= 3 n) "Triple")
                                   ))
                                n-back-level
                                ) 'face '(:background "gold"))
            (propertize
             (if (n-back-is-playing) "  Press C-g to stop" "  Press SPACE to play")
             'face '(:foreground "blue"))
            (if (n-back-is-playing) (format "  Left %s" n-back-trials-left) "")
            "\n")
    ;;(unless n-back-control-status (n-back-init-control-status))
    (dolist (entry n-back-control-status)
      (let ((what (nth 0 entry))
            (msg  (nth 1 entry))
            (sts  (nth 2 entry))
            )
        (setq msg (concat (key-description (n-back-key-binding what)) msg))
        (cond
         ((eq sts 'bad)
          (setq msg (propertize msg 'face '(:foreground "red"))))
         ((eq sts 'ok)
          (setq msg (propertize msg 'face '(:foreground "green")))))
        (insert msg "   "))
      )
    (insert "\n\n")
    (or (n-back-is-playing)
        (not n-back-result)
        (insert (format "Result: %S" n-back-result)))
    (setq buffer-read-only t)
    (with-selected-window n-back-ctrl-window
      (goto-char 1))
    ))

(defun n-back-update-info ()
  (set-window-buffer n-back-info-window n-back-info-buffer)
  (with-current-buffer n-back-info-buffer
    (setq buffer-read-only nil)
    (erase-buffer)

    (insert (propertize "n-back" 'face '(:background "gold"))
            "  "
            (propertize "Help: ?" 'face '(:background "green")))

    (insert "\n\nGame settings:")

    (insert (format "\n  Level: %s " n-back-level)
            (propertize "change: number 1-9" 'face '(:background "green")))
    (insert "\n  Match types: ")
    (dolist (type n-back-active-match-types)
      (insert (format "%s " type)))
    (insert (propertize "toggle: t" 'face '(:background "green")))

    (insert (format "\n  %.2f seconds per trial " n-back-sec-per-trial)
            (propertize "change: +/-" 'face '(:background "green")))

    (setq buffer-read-only t)
    )
  )

(defun n-back-setup-windows ()
  (delete-other-windows)
  (set-window-dedicated-p (selected-window) nil)
  ;; Info
  (split-window-horizontally)
  (setq n-back-info-window (next-window (frame-first-window)))
  (setq n-back-info-buffer (get-buffer-create "* n-back info *"))
  (with-current-buffer n-back-info-buffer (n-back-control-mode))
  (n-back-update-info)
  ;; Control
  (split-window-vertically)
  (setq n-back-ctrl-window (next-window (frame-first-window)))
  (setq n-back-ctrl-buffer (get-buffer-create "* n-back control *"))
  (set-window-buffer n-back-ctrl-window n-back-ctrl-buffer)
  (with-current-buffer n-back-ctrl-buffer (n-back-control-mode))
  (n-back-update-control-buffer)
  (fit-window-to-buffer n-back-ctrl-window)
  (set-window-dedicated-p n-back-ctrl-window t)
  ;; Game
  (setq n-back-game-window (frame-first-window))
  (setq n-back-game-buffer (get-buffer-create "*n-back game*"))
  (set-window-buffer n-back-game-window n-back-game-buffer)
  (set-window-dedicated-p n-back-game-window t)
  (n-back-clear-game-window)
  (with-current-buffer n-back-game-buffer (n-back-control-mode))
  ;; Position in control window
  (select-window n-back-ctrl-window)
  )

;;(n-back-display "str" 1 0 3 3 6)
(defun n-back-display (str x y cols rows max-strlen color)
  ;; (unless (and (window-live-p n-back-game-window)
  ;;              (eq (window-frame n-back-game-window) (selected-frame))
  ;;              )
  ;;   (n-back-setup-windows))
  (message "(n-back-display %s %s %s %s %s %s)" str x y cols rows max-strlen)
  (unless (< x cols) (error "not x=%s < cols=%s" x cols))
  ;;(unless (< y rows) (error "not y=%s < rows=%s" y rows))
  (with-current-buffer n-back-game-buffer
    (let* ((tot-str "")
           ;; Pad spaces left, two right, four between
           (game-w (window-width n-back-game-window))
           (pad-x 0)
           (scale (/ (* 1.0 game-w)
                     (+ (* 2 pad-x)
                        (* (1- cols) 4)
                        (* cols max-strlen))))
           (str-diff (- max-strlen (length str)))
           (str-l-len (/ str-diff 2))
           (str-r-len (- max-strlen (length str) str-l-len))
           (str-disp (propertize
                      (concat (make-string str-l-len 32) str (make-string str-r-len 32))
                      'face (list :background color :height scale)))
           (col-str (concat
                     (make-string pad-x ?p)
                     (make-string
                      (+ (* x (+ 4 max-strlen)))
                      32
                      ;;?x
                      )))
           ;; Pad lines above and below, two between
           (pad-y 0)
           (game-h (window-height n-back-game-window))
           (game-h-scaled (/ game-h scale))
           (lines-between (/ (- game-h-scaled rows (* 2 pad-y))
                             (1- rows)))
           (row-str (make-string (+ pad-x
                                    (floor (* y (1+ lines-between))))
                                 ?\n))
           )
      ;;(message "scale=%s, game-w=%s, colstr='%s', lines-between=%s" scale game-w col-str lines-between)
      (setq show-trailing-whitespace nil)
      (setq cursor-type nil)
      (erase-buffer)
      (setq tot-str row-str)
      (setq tot-str (concat tot-str col-str))
      ;;(setq tot-str (concat tot-str str-disp))
      ;;(message "len tot-str=%s" (length tot-str))
      (insert (propertize tot-str 'face (list :height scale)))
      (insert str-disp)
      )))

;; (setq timer-list nil)
;;(n-back-display-in-timer)
;; (setq n-back-trials-left 3)

(defun n-back-clear-game-window ()
  (with-current-buffer n-back-game-buffer
    (erase-buffer)))

(defun n-back-play ()
  (interactive)
  ;;(n-back-setup-windows)
  (unless n-back-active-match-types
    (error "No active match types"))
  (when (memq 'sound n-back-active-match-types) (n-back-get-sound-files))
  (setq n-back-result nil)
  (n-back-init-control-status)
  (n-back-cancel-timers)
  (n-back-start-main-timer)
  (n-back-update-control-buffer)
  )

(defun n-back-display-in-timer ()
  ;;(message "n-back-trials-left=%s" n-back-trials-left)
  (condition-case nil
      (progn
        (n-back-add-result)
        (if (>= 0 (setq n-back-trials-left (1- n-back-trials-left)))
            (progn
              (n-back-cancel-timers)
              (n-back-update-control-buffer)
              (fit-window-to-buffer n-back-ctrl-window)
              (message "Game over"))
          (let* ((str "") ;(format "%s" n-back-trials-left))
                 (use-position (memq 'position n-back-active-match-types))
                 (use-color (memq 'color n-back-active-match-types))
                 (use-sound (memq 'sound n-back-active-match-types))
                 (max-strlen 5)
                 (cols 3)
                 (rows 3)
                 (x (if use-position (random 3) 1))
                 (y (if use-position (random 3) 1))
                 (color (nth (if use-color (random (length n-back-colors)) 0) n-back-colors))
                 )
            (ring-insert n-back-ring (list str x y color))
            (n-back-display str x y cols rows max-strlen color)
            (n-back-clear-match-status)
            (n-back-update-control-buffer)
            (setq n-back-clear-timer (run-with-timer 0.5 nil 'n-back-clear-game-window)))))
    ((debug error)
     nil)))



;;; Answers

(defvar n-back-result nil)
;;(defvar n-back-answers nil)

(defun n-back-add-result ()
  "Add result of last trial."
  (when (= (ring-length n-back-ring) (1+ n-back-level))
    ;;((color "F: color match" nil) (position "A: position match" nil))
    (dolist (sts-entry n-back-control-status)
      (let* ((what (nth 0 sts-entry))
             (sts  (nth 2 sts-entry))
             (matches (n-back-matches what))
             (num (cond
                   ((eq sts 'ok) 1)
                   ((eq sts 'bad) 2)
                   ((eq sts nil) (when matches 3))
                   (t (error "bad status=%s" sts))))
             (res-entry (when num (assoc what n-back-result)))
             (lst (when num (nthcdr num res-entry))))
        (when num
          (if res-entry
              (setcar lst (1+ (car lst)))
            (setq res-entry (list what 0 0 0))
            (setq lst (nthcdr num res-entry))
            (setq n-back-result (cons res-entry n-back-result))))))))

(defun n-back-matches-position ()
    (let* ((comp-item (ring-ref n-back-ring n-back-level))
           (curr-item (ring-ref n-back-ring 0))
           (comp-x (nth 1 comp-item))
           (curr-x (nth 1 curr-item))
           (comp-y (nth 2 comp-item))
           (curr-y (nth 2 curr-item)))
      (and (= comp-y curr-y)
           (= comp-x curr-x))))

(defun n-back-matches (what)
  (cond
   ((eq what 'position) (n-back-matches-position))
   ((eq what 'color) (n-back-matches-color))
   (t (error "Unknown match type: %s" what))))

(defun n-back-position-answer ()
  (interactive)
  ;;(message "n-back-position-answer here a, ring-size=%s" (ring-size n-back-ring))
  (when (and (memq 'position n-back-active-match-types)
             (> (ring-length n-back-ring) n-back-level))
    ;;(message "n-back-position-answer here b")
    (let ((sts (if (n-back-matches-position) 'ok 'bad)))
      (n-back-set-match-status 'position sts)
      (n-back-update-control-buffer))))

(defun n-back-matches-color ()
  (let* ((comp-item (ring-ref n-back-ring n-back-level))
         (curr-item (ring-ref n-back-ring 0))
         (comp-color (nth 3 comp-item))
         (curr-color (nth 3 curr-item)))
    (equal comp-color curr-color)))

(defun n-back-color-answer ()
  (interactive)
  (when (and (memq 'color n-back-active-match-types)
             (> (ring-length n-back-ring) n-back-level))
    (let ((sts (if (n-back-matches-color) 'ok 'bad)))
      (n-back-set-match-status 'color sts)
      (n-back-update-control-buffer))))

(defun n-back-stop ()
  (interactive)
  (n-back-cancel-timers)
  (n-back-update-control-buffer)
  (message "Stopped n-back game")
  (with-current-buffer n-back-game-buffer
    (let ((buffer-read-only))
      (erase-buffer)
      (insert "Stopped"))))

(define-derived-mode n-back-control-mode nil "N-back"
  "Mode for controling n-back game."
  ;;(setq cursor-type nil)
  (set (make-local-variable 'viper-emacs-state-mode-list) '(n-back-control-mode))
  (set (make-local-variable 'viper-emacs-state-hook) nil) ;; invis cursor
  (abbrev-mode -1)
  (setq show-trailing-whitespace nil)
  (visual-line-mode 1)
  (n-back-make-keymap))

(defvar n-back-trials-left nil)
(defvar n-back-timer nil)
(defvar n-back-clear-timer nil)

(defun n-back-cancel-timers ()
  (when (timerp n-back-timer)
    (cancel-timer n-back-timer))
  (setq n-back-timer)
  (when (timerp n-back-clear-timer)
    (cancel-timer n-back-clear-timer))
  (setq n-back-clear-timer nil)
  (winsize-set-mode-line-colors nil))

(defcustom n-back-trials 20
  "Number of trials per session."
  :type 'integer
  :group 'n-back)

(defvar n-back-ring nil)
(defun n-back-start-main-timer ()
  (n-back-cancel-timers)
  (winsize-set-mode-line-colors t)
  (setq n-back-ring (make-ring (1+ n-back-level)))
  (with-current-buffer n-back-game-buffer (erase-buffer))
  (setq n-back-trials-left (+ n-back-trials n-back-level 1))
  (random t)
  (setq n-back-timer
        (run-with-timer
         n-back-sec-per-trial
         n-back-sec-per-trial
         'n-back-display-in-timer)))

(provide 'n-back)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; n-back.el ends here
