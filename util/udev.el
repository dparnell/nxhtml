;;; udev.el --- Helper functions for updating from dev sources
;;
;; Author: Lennart Borgman (lennart O borgman A gmail O com)
;; Created: 2008-08-24
;; Version: 0.5
;; Last-Updated: 2008-08-24T18:19:41+0200 Sun
;; URL:
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
;; When you want to fetch and install sources from a repository you
;; may have to call several async processes and wait for the answer
;; before calling the next function. These functions may help you with
;; this.
;;
;; See `udev-call-first-step' for more information. Or look in the
;; file udev-cedet.el for examples.
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
;; published by the Free Software Foundation; either version 2, or
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

(eval-when-compile (require 'cl))

(defvar udev-orig-sentinel nil
  "Old sentinel function remembered by `udev-call-this-step'.")
(make-variable-buffer-local 'udev-orig-sentinel)

(defvar udev-log-buffer nil
  "Log buffer pointer for sentinel function.")
(make-variable-buffer-local 'udev-log-buffer)

(defvar udev-is-log-buffer nil
  "This is t if this is an udev log/control buffer.")
(make-variable-buffer-local 'udev-is-log-buffer)

(defun udev-check-is-log-buffer (buffer)
  "Check that BUFFER is an udev log/control buffer."
  (with-current-buffer buffer
    (unless udev-is-log-buffer
      (error "Internat error, not a log buffer: %s" buffer))))

(defun udev-call-first-step (log-buffer steps)
  "Set up and call first step.
Buffer LOG-BUFFER is used for log messages and controling of the
execution of the functions in list STEPS which are executed one
after another."
  (let ((buffer (get-buffer log-buffer))
        this-chain)
    (unless buffer (setq buffer (get-buffer-create log-buffer)))
    (with-current-buffer buffer
      (setq buffer-read-only t)
      (setq udev-is-log-buffer t)
      (setq this-chain
            (cons nil
                  (cons log-buffer
                        (copy-tree steps))))
      (setcar this-chain (cddr this-chain))
      (setq udev-this-chain this-chain)
      (assert (eq (car steps) (udev-this-step buffer)) t)
      (udev-call-this-step log-buffer)
      )))

(defun udev-chain (log-buffer)
  (udev-check-is-log-buffer log-buffer)
  (with-current-buffer log-buffer
    udev-this-chain))

(defun udev-this-step (log-buffer)
  (let ((this-chain (udev-chain log-buffer)))
    (caar this-chain)))

(defun udev-goto-next-step (log-buffer)
  (let* ((this-chain (udev-chain log-buffer))
        (this-step (car this-chain)))
    (setcar this-chain (cdr this-step))))

(defun udev-call-this-step (log-buffer)
  "Call the current function in LOG-BUFFER.
If this function returns a buffer and the buffer has a process
then change the process sentinel to `udev-compilation-sentinel'.
Otherwise continue to call the next function.

Also put a log message in in LOG-BUFFER with a link to the buffer
returned above if any."
  (with-current-buffer log-buffer
    (widen)
    (goto-char (point-max))
    (let* ((inhibit-read-only t)
           (this-step (udev-this-step log-buffer))
           here
           buf
           proc)
      (if (not this-step)
          (insert (propertize "\nFinished\n" 'face 'compilation-info))
        (insert "\nStep: ")
        (setq here (point))
        (insert (pp-to-string this-step))
        (setq buf (funcall this-step))
        (when (bufferp buf)
          (make-text-button here (point)
                            'buffer buf
                            'action (lambda (btn)
                                      (display-buffer
                                       (button-get btn 'buffer))))
          (setq proc (get-buffer-process buf)))
        ;; Setup for next step
        (with-current-buffer buf
          ;; Make a copy here for the sentinel function.
          (setq udev-log-buffer log-buffer))
        (if proc
            (progn
              (with-current-buffer buf
                (setq udev-orig-sentinel (process-sentinel proc))
                (set-process-sentinel proc 'udev-compilation-sentinel)))
          ;;(message "proc is nil")
          (udev-call-next-step log-buffer 0 nil)
          )))))

(defun udev-call-next-step (log-buffer prev-exit-status exit-status-buffer)
  "Go to next step in LOG-BUFFER and call `udev-call-this-step'.
However if PREV-EXIT-STATUS \(which is the exit status from the
previous step) is not 0 and there is in EXIT-STATUS-BUFFER no
`udev-continue-on-error-function' then stop and insert an error
message in LOG-BUFFER."
  (with-current-buffer log-buffer
    (if (or (= 0 prev-exit-status)
            (with-current-buffer exit-status-buffer
              (when udev-continue-on-error-function
                (funcall udev-continue-on-error-function exit-status-buffer))))
        (progn
          (udev-goto-next-step log-buffer)
          (udev-call-this-step log-buffer))
      (let ((inhibit-read-only t))
        (insert " ")
        (insert (propertize "Error" 'face 'compilation-error))))))

(defun udev-compilation-sentinel (proc msg)
  "Sentinel to use for processes started by `udev-call-this-step'.
Check for error messages and call next step."
  ;;(message "udev-compilation-sentinel proc=%s msg=%s" proc msg)
  (let ((buf (process-buffer proc))
        (exit-status (process-exit-status proc)))
    (with-current-buffer buf
      ;;(message "  proc=%s, buf=%s, status=%s exit-status=%s orig-sentinel=%s" proc buf (process-status proc) (process-exit-status proc) udev-orig-sentinel)
      (when udev-orig-sentinel
        (funcall udev-orig-sentinel proc msg))
      (when (and (eq 'exit (process-status proc))
                 (= 0 exit-status))
        ;; Check for errors
        (let ((here (point))
              (err-point 1)
              (has-error nil))
          (widen)
          (goto-char (point-min))
          (setq has-error
                (catch 'found-error
                  (while err-point
                    (setq err-point
                          (next-single-property-change err-point 'face))
                    (when err-point
                      (let ((face (get-text-property err-point 'face)))
                        (when (memq 'compilation-error face)
                          (throw 'found-error t)))))))
          (when has-error
            (setq exit-status 1)
            (goto-char (point-max))
            (let ((inhibit-read-only t))
              (insert (propertize "There were errors" 'font-lock-face 'compilation-error)))
            (udev-set-compilation-end-message buf 'exit (cons "has errors" 1)))
          (goto-char here)
          ))
      (unless (member proc compilation-in-progress)
        (udev-call-next-step udev-log-buffer exit-status (current-buffer))))))

(defun udev-set-compilation-end-message (buffer process-status status)
  "Change the message shown after compilation.
This is similar to `compilation-end-message'."
  (with-current-buffer buffer
    (setq mode-line-process
          (let ((out-string (format ":%s [%s]" process-status (cdr status)))
                (msg (format "%s %s" mode-name
                             (replace-regexp-in-string "\n?$" "" (car status)))))
            (message "%s" msg)
            (propertize out-string
                        'help-echo msg 'face (if (> (cdr status) 0)
                                                 'compilation-error
                                               'compilation-info))))))

(defvar udev-continue-on-error-function nil
  "One-time helper to resolve exit status error problem.
This can be used for example after calling `cvs diff' which
returns error exit status if there is a difference - even though
there does not have to be an error.")
(make-variable-buffer-local 'udev-continue-on-error-function)


(provide 'udev)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; udev.el ends here
