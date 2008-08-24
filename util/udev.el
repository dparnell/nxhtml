;;; udev.el --- Helper functions for updating from dev sources
;;
;; Author: Lennart Borgman (lennart O borgman A gmail O com)
;; Created: 2008-08-24T01:46:50+0200 Sat
;; Version:
;; Last-Updated:
;; URL:
;; Keywords:
;; Compatibility:
;;
;; Features that might be required by this library:
;;
;;   None
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

(defvar udev-orig-sentinel nil)
(make-variable-buffer-local 'udev-orig-sentinel)

(defvar udev-this-step nil)
(make-variable-buffer-local 'udev-this-step)

(defun udev-compilation-sentinel (proc msg)
  (message "udev-compilation-sentinel proc=%s msg=%s" proc msg)
  (let ((buf (process-buffer proc))
        (exit-status (process-exit-status proc)))
    (with-current-buffer buf
      (message "  proc=%s, buf=%s, status=%s exit-status=%s orig-sentinel=%s" proc buf (process-status proc) (process-exit-status proc) udev-orig-sentinel)
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
        (udev-call-next-step exit-status udev-cedet-continue-on-error)))))

(defun udev-set-compilation-end-message (buffer process-status status)
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

(defvar udev-continue-on-error nil)
(make-variable-buffer-local 'udev-continue-on-error)

(defun udev-call-this-step ()
  (with-current-buffer udev-cedet-update-buffer
    (widen)
    (goto-char (point-max))
    (let ((inhibit-read-only t)
          (this-step (car udev-this-step))
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
        (if proc
            (progn
              (with-current-buffer buf
                (setq udev-orig-sentinel (process-sentinel proc))
                (set-process-sentinel proc 'udev-compilation-sentinel)))
          (message "proc is nil")
          (udev-call-next-step 0 nil)
          )))))

(defun udev-call-next-step (prev-exit-status continue-on-error)
  (with-current-buffer udev-cedet-update-buffer
    (if (or continue-on-error
            (= 0 prev-exit-status))
        (progn
          (setq udev-this-step (cdr udev-this-step))
          (udev-call-this-step))
      (let ((inhibit-read-only t))
        (insert " ")
        (insert (propertize "Error" 'face 'compilation-error))))))


(provide 'udev)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; udev.el ends here
