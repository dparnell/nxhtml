;;; nxml-where.el --- Show XML path
;;
;; Author: Lennart Borgman
;; Maintainer:
;; Created: Tue Dec 19 14:59:01 2006
(defconst nxml-where:version "0.52");; Version:
;; Lxast-Updated: Thu Mar 01 23:16:35 2007 (3600 +0100)
;; Keywords:
;; Compatibility:
;;
;; Features that might be required by this library:
;;
;;   `cl'.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change log:
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

;; This buffer is for notes you don't want to save, and for Lisp evaluation.
;; If you want to create a file, visit that file with C-x C-f,
;; then enter the text in that file's own buffer.

(eval-when-compile
  (require 'cl)
  (unless (featurep 'nxhtml-autostart)
    (let ((efn (expand-file-name "../autostart.el")))
      (load efn))
    (require 'nxml-mode)))

(defvar nxml-where-once-update-timer nil)
(make-variable-buffer-local 'nxml-where-once-update-timer)
(put 'nxml-where-once-update-timer 'permanent-local t)

(defvar nxml-where-second-update-timer nil)
(make-variable-buffer-local 'nxml-where-second-update-timer)
(put 'nxml-where-second-update-timer 'permanent-local t)

(defvar nxml-where-ovls nil)
(make-variable-buffer-local 'nxml-where-ovls)
(put 'nxml-where-ovls 'permanent-local t)

(defun nxml-where-cancel-once ()
  (when (timerp nxml-where-once-update-timer)
    (cancel-timer nxml-where-once-update-timer)
    (setq nxml-where-once-update-timer nil)))

(defun nxml-where-cancel-second ()
  (when (timerp nxml-where-second-update-timer)
    (cancel-timer nxml-where-second-update-timer)
    (setq nxml-where-second-update-timer nil)))

(defun nxml-where-start-second (next-point buffer)
  (message "start second")
  (setq nxml-where-second-update-timer
        (run-with-idle-timer idle-update-delay
                             nil
                             'nxml-where-add-overlays
                             next-point
                             (current-buffer))))

(defun nxml-where-update-buffers ()
  (when (boundp 'nxml-where-mode)
    (dolist (buf (buffer-list))
      (when nxml-where-mode
        (nxml-where-update-once)))))

(defun nxml-where-update-once ()
  ;;(message "\nupd-once. this-command=%s" this-command)
  (condition-case err
      (progn
        (nxml-where-cancel-once)
        (setq nxml-where-once-update-timer
              (run-with-idle-timer
               (* 0.2 idle-update-delay)
               nil
               'nxml-where-update
               (current-buffer))))
    (error
     (message "%s" (error-message-string err)))))
(put 'nxml-where-update-once 'permanent-local-hook t)

(defun nxml-where-stop-updating ()
  (remove-hook 'post-command-hook 'nxml-where-update-once t)
  ;;(nxml-where-update-once)
  )

(defun nxml-where-restart-updating ()
  (nxml-where-update-once)
  (add-hook 'post-command-hook 'nxml-where-update-once nil t))

(defgroup nxml-where nil
  "Customization group for nxml-where."
  :group 'nxhtml
  :group 'nxml)

(define-toggle nxml-where-tag+id t
  "Show tags + id in path if non-nil.
If nil show only tag names."
  :set (lambda (sym val)
         (set-default sym val)
         (nxml-where-update-buffers))
  :group 'nxml-where)

(define-toggle nxml-where-header t
  "Show header with XML-path if non-nil."
  :set (lambda (sym val)
         (set-default sym val)
         (nxml-where-update-buffers))
  :group 'nxml-where)

(define-toggle nxml-where-marks t
  "Show marks in buffer for XML-path if non-nil."
  :set (lambda (sym val)
         (set-default sym val)
         (nxml-where-update-buffers))
  :group 'nxml-where)

(define-toggle nxml-where-only-inner nil
  "Mark only inner-most tag."
  :set (lambda (sym val)
         (set-default sym val)
         (nxml-where-update-buffers))
  :group 'nxml-where)

(define-toggle nxml-where-only-tags-with-id t
  "Show only tags with id in the header line."
  :set (lambda (sym val)
         (set-default sym val)
         (nxml-where-update-buffers))
  :group 'nxml-where)

(defface nxml-where-marking
  '((t (:inherit secondary-selection)))
  "The default face used for marking tags in path."
  :group 'nxml-where)

(defcustom nxml-where-marking 'nxml-where-marking
  "Variable pointing to the face used for marking tags in path."
  :type 'face
  :set (lambda (sym val)
         (set-default sym val)
         (nxml-where-update-buffers))
  :group 'nxml-where)

(defcustom nxml-where-header-attributes '("id" "name")
  "List of attributes `nxml-where-header' should display."
  :type '(repeat string)
  :set (lambda (sym val)
         (set-default sym val)
         (nxml-where-update-buffers))
  :group 'nxml-where)

(defcustom nxml-where-widen t
  "If non-nil and narrowed widen before getting XML path."
  :type 'boolean
  :group 'nxml-where)


(defvar nxml-where-saved-header-line-format nil)
(make-variable-buffer-local 'nxml-where-saved-header-line-format)
(put 'nxml-where-saved-header-line-format 'permanent-local t)

(defun nxml-where-save-header-line-format ()
  (unless nxml-where-saved-header-line-format
    (setq nxml-where-saved-header-line-format header-line-format)))

(defun nxml-where-restore-header-line-format ()
  (setq header-line-format nxml-where-saved-header-line-format))

(defvar nxml-where-modes '(nxml-mode nxhtml-mode))

(defun nxml-where-is-nxml ()
  (or (derived-mode-p 'nxml-mode)
      (and (featurep 'mumamo)
           mumamo-multi-major-mode
           (let ((major-mode (mumamo-main-major-mode)))
             (derived-mode-p 'nxml-mode)))))

(defun nxml-where-mode-start ()
  (message "START")
  (unless (nxml-where-is-nxml)
    (error "Can't display XML path since major mode is not nxml-mode child."))
  (add-hook 'after-change-major-mode-hook 'nxml-where-turn-off-unless-nxml nil t)
  (nxml-where-save-header-line-format)
  (setq nxml-where-inner-start nil)
  (setq nxml-where-path nil)
  (setq nxml-where-old-path nil)
  (nxml-where-restart-updating))

(defun nxml-where-mode-stop ()
  (message "STOP")
  (nxml-where-stop-updating)
  (nxml-where-restore-header-line-format)
  (nxml-where-handle-old-path)
  (setq nxml-where-old-path nil)
  (setq nxml-where-path nil))

(defun nxml-where-turn-off-unless-nxml ()
  (unless (nxml-where-is-nxml)
    (nxml-where-mode-stop)))
(put 'nxml-where-turn-off-unless-nxml 'permanent-local-hook t)

(define-minor-mode nxml-where-mode
  "Shows path in mode line."
  :global nil
  :group 'nxml-where
  (if nxml-where-mode
      ;;Turn it on
      (nxml-where-mode-start)
    ;; Turn it off
    (nxml-where-mode-stop)
    ))
(put 'nxml-where-mode 'permanent-local t)

(defun nxml-where-turn-on-in-nxml-child ()
  "Turn on `nxml-where-mode' if possible.
This is possible if `major-mode' in the buffer is derived from
`nxml-mode'."
  (when (derived-mode-p 'nxml-mode)
    (nxml-where-mode 1)))

(define-globalized-minor-mode nxml-where-global-mode nxml-where-mode
  nxml-where-turn-on-in-nxml-child
  :group 'nxml-where)
;; The problem with global minor modes:
(when (and nxml-where-global-mode
           (not (boundp 'define-global-minor-mode-bug)))
  (nxml-where-global-mode 1))

(defun nxml-where-update (buffer)
  (message "nxml-where-update %s" buffer)
  (if (and (bufferp buffer)
           (buffer-live-p buffer))
      (save-restriction
        (when nxml-where-widen (widen))
        (nxml-where-add-overlays nil buffer))
    ;; Kill timer if buffer is gone
    ;;(cancel-timer timer-event-last)
    ))

(defvar nxml-where-get-id-pattern
  (rx space
      (eval (cons 'or nxml-where-header-attributes))
      (0+ space)
      ?=
      (0+ space)
      ?\"
      (0+ (not (any ?\")))
      ?\"
      ))

(defvar nxml-where-tag+id-pattern
  ;;(insert ;; -------------------
  (rx ?<
      (submatch
       (1+ (char "-a-z0-9:"))
       )
      (0+ (1+ space)
          (1+ (any "a-z"))
          (0+ space)
          ?=
          (0+ space)
          ?\"
          (0+ (not (any ?\")))
          ?\"
          )
      (0+ space)
      (opt ?/)
      ?>)
  ;;) ;; -------------------
  )

(defun nxml-where-mark (start end)
  (let (ovl
        (ovls (overlays-at start)))
    (dolist (o ovls)
      (when (and (eq (overlay-get o 'face) nxml-where-marking)
                 (= start (overlay-start o))
                 (= end   (overlay-end o)))
        (setq ovl o)))
    (unless ovl
      (setq ovl (make-overlay start end))
      (setq nxml-where-ovls (cons ovl nxml-where-ovls))
      (overlay-put ovl 'face nxml-where-marking)
      (move-overlay ovl start end))
    ovl))

(defvar nxml-where-inner-start nil
  "Last inner element start tag position. Internal use.")
(make-variable-buffer-local 'nxml-where-inner-start)
(put 'nxml-where-inner-start 'permanent-local t)

(defun nxml-where-mark-forward-element (bottom)
  (save-restriction
    (let ((here (point))
          (end-of-narrow
           (progn
             (goto-char bottom)
             (line-end-position 4)))
          start end ovl)
      ;; Fix-me: Narrow how much?
      (setq end-of-narrow (max (+ 4000 (window-end))
                               end-of-narrow))
      (setq end-of-narrow (min (point-max)
                               end-of-narrow))
      (narrow-to-region here end-of-narrow)
      (condition-case err
          (progn
            (goto-char here)
            (nxml-forward-element)
            (when (looking-back "</[a-z0-9]+>")
              (setq start (match-beginning 0))
              (setq end (point))
              (setq ovl (nxml-where-mark start end))))
        (error
         (message "nxml-where-mark-forw err=%s" err)))
      (goto-char here)
      ovl)))

(defvar nxml-where-path nil)
(make-variable-buffer-local 'nxml-where-path)
(put 'nxml-where-path 'permanent-local t)

(defvar nxml-where-old-path nil)
(make-variable-buffer-local 'nxml-where-old-path)
(put 'nxml-where-old-path 'permanent-local t)

(defun nxml-where-add-overlays (this-point buffer)
  (message "nxml-where-add-overlays %s %s, point=%s" this-point buffer (point))
  (with-current-buffer buffer
    (unless nxml-where-marks
      (when nxml-where-ovls
        (dolist (o nxml-where-ovls)
          (delete-overlay o))
        (setq nxml-where-ovls nil)))
    (unless nxml-where-header
      (setq header-line-format nil))
    (when (and nxml-where-mode
               (or nxml-where-header nxml-where-marks)
               (if this-point
                   (> this-point (point-min))
                 t))
      (let ((ovls nxml-where-ovls)
            ovl
            (here (point))
            next-point
            (is-first (not this-point))
            same-as-last
            )
        ;; If on beginning of tag step forward one char.
        (unless (or (eobp)
                    this-point
                    (not (eq  ?< (char-after))))
          (forward-char))
        (unless this-point (setq this-point (point)))
        (goto-char this-point)
        (setq next-point
              (catch 'err
                (condition-case err
                    (nxml-backward-up-element)
                  (error
                   (if (equal err '(error "No parent element"))
                       (throw 'err nil)
                     (message "nxml-where error: %S" err)
                     (throw 'err "uh?"))))
                (when is-first
                  (setq same-as-last
                        (and nxml-where-inner-start
                             (= (point) (car nxml-where-inner-start))))
                  (unless same-as-last
                    ;;(setq nxml-where-path nil)
                    (when nxml-where-path
                      (setq nxml-where-old-path (reverse nxml-where-path))
                      (setq nxml-where-path nil))
                    (nxml-where-cancel-second))
                  (when same-as-last
                    (throw 'err 'same-as-last))
                  ;;(setq nxml-where-inner-start (point))
                  (setq is-first nil)
                  ;;(dolist (o ovls) (delete-overlay o))
                  (setq nxml-where-ovls nil)
                  (let ((ovl (nth 1 nxml-where-inner-start)))
                    (when (overlayp ovl) (delete-overlay ovl)))
                  (setq nxml-where-inner-start
                        (list (point)
                              (nxml-where-mark-forward-element here))))
                (save-match-data
                  (let (ovl)
                    (while (and nxml-where-old-path
                                (> (caar nxml-where-old-path) (point)))
                      (setq ovl (nth 2 (car nxml-where-old-path)))
                      (message "delete 1: point=%s, ovl=%s" (point) (car nxml-where-old-path))
                      (when (overlayp ovl) (delete-overlay ovl))
                      (setq nxml-where-old-path
                            (cdr nxml-where-old-path))))
                  (unless (looking-at nxml-where-tag+id-pattern)
                    (throw 'err nil))
                  (let ((start (point))
                        (end (match-end 0))
                        (tag (match-string-no-properties 1))
                        (all (match-string-no-properties 0))
                        (old-rec (car nxml-where-old-path))
                        rec
                        ovl)
                    (when nxml-where-tag+id
                      (when (string-match nxml-where-get-id-pattern all)
                        (setq tag (concat tag (match-string 0 all)))))
                    (setq tag (concat "<" tag ">"))
                    ;; Keep old overlay if it is ok.
                    (if (and old-rec
                             (= (nth 0 old-rec) start)
                             (string= (nth 1 rec) tag))
                        (progn
                          (while nxml-where-old-path
                            (setq rec (car nxml-where-old-path))
                            (setq nxml-where-old-path (cdr nxml-where-old-path))
                            (setq nxml-where-path (cons rec nxml-where-path)))
                          (throw 'err (1- (nth 0 rec))))
                      (when nxml-where-marks
                        (setq ovl (nxml-where-mark start end)))
                      (setq rec (list start tag ovl)))
                    (setq nxml-where-path (cons rec nxml-where-path)))
                  )
                (throw 'err (max (1- (point)) (point-min)))))
                                        ;)
        (goto-char here)
        ;;(setq next-point "test err")
        (message "next-point=%s" next-point)
        (if next-point
            (cond
             ((stringp next-point)
              (setq header-line-format next-point))
             ((eq 'same-as-last next-point)
              nil)
             (t
              (nxml-where-cancel-second)
              (if nxml-where-only-inner
                  (progn
                    (nxml-where-handle-old-path)
                    (setq nxml-where-path nil))
                (setq nxml-where-second-update-timer
                      (run-with-timer (* 0.2 idle-update-delay)
                                      nil
                                      'nxml-where-start-second
                                      next-point (current-buffer))))))
          (nxml-where-handle-old-path)
          (let ((path (mapcar (lambda (elt)
                                (cadr elt))
                              nxml-where-path)))
            (unless path
              (setq path (list (if (looking-at "[[:space:]]*\\'")
                                   "(After last tag)"
                                 "(Before first tag)"))))
            (setq nxml-where-path nil)
            (if (null path)
                (setq path " *Error* ")
              ;; Throw away <html>
              (let* ((first (car path))
                     (html "<html")
                     (hlen (length html)))
                (when (and (> (length first) hlen)
                           (string= html (substring first 0 hlen)))
                  (setq path (cdr path))))
              (unless path
                (setq path (list "(At html start)"))))
            (let* ((sp (substring (format "%s" path) 1 -1))
                   (label " Path: ")
                   (totlen (+ (length sp) (length label)))
                   header)
              (when (> totlen (window-width))
                (setq sp (concat "... " (substring sp (+ (- totlen (window-width)) 4)))))
              (setq header (concat label sp))
              (when nxml-where-header
                (setq header-line-format header)))))))))

(defun nxml-where-handle-old-path ()
  (while nxml-where-old-path
    (setq ovl (nth 2 (car nxml-where-old-path)))
    (message "delete 2: ovl=%s" (car nxml-where-old-path))
    (when (overlayp ovl) (delete-overlay ovl))
    (setq nxml-where-old-path (cdr nxml-where-old-path)))
 (setq nxml-where-old-path (reverse nxml-where-path)))

(provide 'nxml-where)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; nxml-where.el ends here
