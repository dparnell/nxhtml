;;; rxx.el --- Additional routines for rx regexps
;;
;; Author: Lennart Borgman (lennart O borgman A gmail O com)
;; Created: 2010-06-05 Sat
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
;;; Change Log:
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

(eval-when-compile (require 'cl))

(defvar rxr-tree nil)
(defvar rxr-state nil)

(defvar my-rxr-result nil)
(defun my-rxr-insert ()
  "testing"
  (interactive)
  (insert "(rx "
          (format "%S" my-rxr-result)
          ")"))
(defun my-rxr-parse ()
  "testing"
  (interactive)
  (save-restriction
    (widen)
    (narrow-to-region (point-at-bol) (point-at-eol))
    (let ((src (buffer-substring-no-properties (point-max) (point-min)))
          (res (rxr-parse)))
      (setq my-rxr-result res)
      (message "parsed \"%s\" =>%S" src res))))
(global-set-key [(f9)] 'my-rxr-parse)
(global-set-key [(shift f9)] 'my-rxr-insert)

(defun rxr-state ()
  (if (numberp (car rxr-state))
      'STRING
    (car rxr-state)))

(defun rxr-parse ()
  (setq rxr-state nil)
  (goto-char (point-min))
  (rxr-parse-1 'and))

(defun rxr-parse-1 (what)
  (push (point) rxr-state)
  (let (expr
        (str-beg (point))
        str-end
        result
        ret-result
        found-my-end)
    (while (not (or (eobp)
                    ret-result))
      (setq expr (list (char-after) (rxr-state)))
      (forward-char)
      (cond
        ( (equal expr '(?\\ STRING))
          (setq str-end (1- (point)))
          (push 'BS/2 rxr-state)
          )
        ( (equal expr '(?\\ BS/2))
          (pop rxr-state)
          )
        ( (equal expr '(?\( BS/2))
          (push (buffer-substring-no-properties str-beg str-end) result)
          (push (rxr-parse-1 'submatch) result)
          (setq str-beg (point))
          )
        ( (equal expr '(?\) BS/2))
          (push (buffer-substring-no-properties str-beg str-end) result)
          (setq ret-result result)
          )
        ( (equal expr '(?\| BS/2))
          ;; Fix-me: semantic? Probably single items on both sides.
          (let ((last-string (buffer-substring-no-properties str-beg str-end)))
            (unless (zerop (length last-string))
              (push last-string result)))
          (let ((last (pop result))
                or-result)
            (unless last (error "\| without previous element"))
            (setq or-result (cadr (rxr-parse-1 'or)))
            (push (list 'or last or-result) result))
          (setq str-beg (point))
          )
        ( (eq (nth 1 expr) 'STRING)
          )
        ( t (error "expr=%S" expr)))
      (when (eq what 'or)
        ;; We only want one
        (setq ret-result result))
      )
    (when (eobp)
      (let ((tail (buffer-substring-no-properties str-beg (point))))
        (unless (zerop (length tail))
          (push tail result)))
      (setq ret-result result))
    (pop rxr-state)
    (let ((res-inner (reverse ret-result)))
      (if (and (eq 'and what)
               (= 1 (length res-inner)))
          (car res-inner)
        (cons what res-inner)))
    ))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; rxx.el ends here
