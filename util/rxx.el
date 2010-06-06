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
(require 'web-vcs)

(defvar rxx-tree nil)

(defvar my-rxx-result nil)
(defun my-rxx-insert ()
  "testing"
  (interactive)
  (insert "(rx "
          (format "%S" my-rxx-result)
          ")"))
(defun my-rxx-parse (str-syntax)
  "testing"
  (interactive "P")
  (save-restriction
    (widen)
    (narrow-to-region (point-at-bol) (point-at-eol))
    (let* ((src (buffer-substring-no-properties (point-max) (point-min)))
           (res-rx (rxx-parse str-syntax))
           evaled-done
           (res-rx-to-string (condition-case err
                                 (prog1
                                     (eval (list 'rx res-rx))
                                   (setq evaled-done t))
                               (error (error-message-string err))))
           (res-rx-again (when res-rx-to-string
                           (with-temp-buffer
                             (insert res-rx-to-string)
                             (rxx-parse str-syntax))))
           (same-str (string= src res-rx-to-string))
           (nearly-same-str (or same-str
                                (string= (concat "\\(?:" src "\\)")
                                         res-rx-to-string)))
           (same-rx (or same-str (equal res-rx-again res-rx)))
           (res-rx-again-str (if (or same-rx (not res-rx-again))
                                 ""
                               (concat ", again=" (prin1-to-string res-rx-again))))
           (ok-face '(:foreground "black" :background "green"))
           (maybe-face '(:foreground "black" :background "yellow"))
           (nearly-face '(:foreground "black" :background "yellow green"))
           (fail-face '(:foreground "black" :background "red"))
           (res-face
            (cond (same-str ok-face)
                  (nearly-same-str nearly-face)
                  (same-rx  maybe-face)
                  (t fail-face))))
      (setq my-rxx-result res-rx)
      (message "res-rx-to-string=%s" res-rx-to-string)
      (when same-str (setq res-rx-to-string "EQUAL"))
      (web-vcs-message-with-face
       res-face
       "parsed \"%s\" =>%S, res-rx-to-string=%s%s" src res-rx res-rx-to-string res-rx-again-str))))

(global-set-key [(f9)] 'my-rxx-parse)
(global-set-key [(shift f9)] 'my-rxx-insert)

(defun rxx-parse (str-syntax)
  "Parse buffer regexp between point min and max.
If STR-SYNTAX is non-nil \\ must be doubled and things like \\n
are recognized."
  (web-vcs-message-with-face 'highlight "regexp src=%S" (buffer-string))
  (goto-char (point-min))
  (rxx-parse-1 'and-top str-syntax))

(defun rxx-parse-1 (what str-syntax)
  (unless (memq what '(and-top
                       or-top
                       and or
                       group
                       submatch))
    (error "internal error, what=%s" what))
  (let ((state '(DEFAULT))
        expr
        (str-beg (point))
        str-end
        result
        ret-result
        found-my-end)
    (while (not (or (eobp)
                    ret-result))
      (setq expr (list (char-after) (car state)))
      (forward-char)
      (cond
       ( (equal expr '(?\\ DEFAULT))
         (setq str-end (1- (point)))
         (push (if str-syntax 'BS1 'BS2) state)
         )
        ( (equal expr '(?\\ BS1))
          ;; string syntax:
          (pop state)
          (push 'BS2 state)
          )
        ( (eq (nth 1 expr) 'BS1)
          (pop state)
          (push 'DEFAULT state)
          )
        ( (equal expr '(?\( BS2))
          (pop state)
          (push (buffer-substring-no-properties str-beg str-end) result)
          ;; Just look ahead, that is most simple.
          (if (not (eq (char-after) ??))
              (progn
                (push (rxx-parse-1 'submatch str-syntax) result)
                (setq str-beg (point)))
            (forward-char)
            ;; \(?
            (if (not (eq (char-after) ?:))
                ;; (?nn:...) (?24:...)
                (let ((n-beg (point))
                      nn)
                  (skip-chars-forward "0-9")
                  (setq nn (string-to-number (buffer-substring-no-properties n-beg (1- (point)))))
                  ;; fix-me: this can't be used until rx knows about it.
                  (error "Found (?%d:, but can't handle it" nn))
              ;; \(?:
              (forward-char)
              (push (rxx-parse-1 'and str-syntax) result)
              (setq str-beg (point))))
          )
        ( (equal expr '(?\) BS2))
          (when (memq what '(and-top or-top))
            (error "Trailing \\) in regexp"))
          (push (buffer-substring-no-properties str-beg str-end) result)
          (pop state)
          (setq ret-result result)
          (setq str-beg (point))
          )
        ( (equal expr '(?\| BS2))
          ;; Fix-me: semantic? Probably single items on both sides.
          (let ((last-string (buffer-substring-no-properties str-beg str-end)))
            (push last-string result))
          (let ((last (pop result))
                or-result)
            (unless last (error "\| without previous element"))
            (let ((or (if (eq 'and-top what) 'or-top 'or)))
              (setq or-result (cadr (rxx-parse-1 or str-syntax))))
            (pop state)
            (push (list 'or last or-result) result))
          (setq str-beg (point))
          )
        ( (eq (nth 1 expr) 'DEFAULT)
          )
        ( t (error "expr=(%c %s)" (car expr) (cadr expr))))
      (when (eq what 'or)
        ;; We only want one
        (setq ret-result result))
      )
    (unless (eq 'DEFAULT (car state)) ;; Initial state
      (error "Internal error: expected DEFAULT on state stack=%S" state))
    (pop state)
    (when state (error "Internal error: state rest=%S, what=%s" state what))
    (when (eobp)
      (if (and (not result)
               (not (memq what '(and-top or-top))))
          (error "Unfinished regexp, missing \\), what=%s" what)
        (let ((tail (buffer-substring-no-properties str-beg (point))))
          (unless (zerop (length tail))
            (push tail result)))
        (setq ret-result result)))
    (let ((res-inner (reverse ret-result)))
      (if (and (memq what '(and and-top))
               (= 1 (length res-inner)))
          (car res-inner)
        (setq what (case what
                     (and-top 'and)
                     (or-top 'or)
                     (t what)))
        (if (not (memq what '(and submatch)))
            (cons what res-inner)
          (when (equal "" (car res-inner))
            ;; Remove helping ""
            (message "res-inner a=%S" res-inner)
            (setq res-inner (cdr res-inner))
            (message "res-inner b=%S" res-inner)
            )
          (if (or (> 1 (length res-inner))
                  (not (eq what 'and)))
              (cons what res-inner)
            (setq res-inner (car res-inner))
            (message "res-inner c=%S" res-inner)
            res-inner
            ))
        ))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; rxx.el ends here
