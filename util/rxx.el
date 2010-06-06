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
           (res-rx-rec (rxx-parse str-syntax))
           (dummy (message "res-rx-rec=%S" res-rx-rec))
           (res-rx-ok (car res-rx-rec))
           (res-rx (when res-rx-ok (cdr res-rx-rec)))
           evaled-done
           (res-rx-to-string (condition-case err
                                 (prog1
                                     (eval (list 'rx res-rx))
                                   (setq evaled-done t))
                               (error (error-message-string err))))
           (res-rx-again-rec (when res-rx-to-string
                               (with-temp-buffer
                                 (insert res-rx-to-string)
                                 (rxx-parse str-syntax))))
           (res-rx-again-ok (car res-rx-again-rec))
           (res-rx-again (when res-rx-again-ok (cdr res-rx-again-rec)))
           (same-str (string= src res-rx-to-string))
           (nearly-same-str (or same-str
                                (string= (concat "\\(?:" src "\\)")
                                         res-rx-to-string)))
           (same-rx-again (or same-str (equal res-rx-again res-rx)))
           (res-rx-again-str (if (or same-rx-again (not res-rx-again))
                                 ""
                               (concat ", again=" (prin1-to-string res-rx-again))))
           (ok-face '(:foreground "black" :background "green"))
           (maybe-face '(:foreground "black" :background "yellow"))
           (nearly-face '(:foreground "black" :background "yellow green"))
           (fail-face '(:foreground "black" :background "red"))
           (bad-regexp-face '(:foreground "black" :background "gray"))
           (res-face
            (cond (same-str ok-face)
                  (nearly-same-str nearly-face)
                  (same-rx-again  maybe-face)
                  (t fail-face))))
      (if (not res-rx-ok)
          (let* ((bad (cdr res-rx-rec))
                 (bad-msg (car bad))
                 (bad-pos (cdr bad))
                 (bad-pre  (buffer-substring-no-properties (point-min) bad-pos))
                 (bad-post (buffer-substring-no-properties bad-pos (point-max))))
            (web-vcs-message-with-face bad-regexp-face "parsed \"%s\" => %s: \"%s\" HERE \"%s\"" src bad-msg bad-pre bad-post))
        (setq my-rxx-result res-rx)
        (message "res-rx-to-string=%s" res-rx-to-string)
        (when same-str (setq res-rx-to-string (concat "EQUAL STR=" res-rx-to-string)))
        (when same-rx-again (setq res-rx-again "EQUAL RX"))
        (web-vcs-message-with-face
         res-face
         "parsed \"%s\" => %S => \"%s\" => %S" src res-rx res-rx-to-string res-rx-again)))))

(global-set-key [(f9)] 'my-rxx-parse)
(global-set-key [(shift f9)] 'my-rxx-insert)

(defun rxx-parse (str-syntax)
  "Parse buffer regexp between point min and max.
If STR-SYNTAX is non-nil \\ must be doubled and things like \\n
are recognized."
  (web-vcs-message-with-face 'highlight "regexp src=%S" (buffer-string))
  (goto-char (point-min))
  (let* (ok
         (res (catch 'bad-regexp
                (prog1
                    (rxx-parse-1 'and-top str-syntax)
                  (setq ok t)))))
    (if ok
        (cons t res)
      (cons nil res))))

(defun rxx-parse-1 (what str-syntax)
  (unless (memq what '(and-top
                       or-top
                       and or
                       group
                       submatch
                       any))
    (error "internal error, what=%s" what))
  (let* ((this-state (case what
                       (any 'CHARS)
                       (t 'DEFAULT)))
         (state `(,this-state))
         expr
        (str-beg (point))
        str-end
        result
        ret-result
        (want-single-item (memq what '(or))))
    (while (not (or (eobp)
                    ret-result))
      (setq expr (list (char-after) (car state)))
      (forward-char)
      (cond
       ( (equal expr '(?\[  DEFAULT))
         (push (buffer-substring-no-properties str-beg (1- (point))) result)
         (push (rxx-parse-1 'any str-syntax) result)
         (setq str-beg (point))
         )
       ( (equal expr '(?\]  CHARS))
         (push (buffer-substring-no-properties str-beg (1- (point))) result)
         (setq str-beg (point))
         (setq ret-result result)
         )
       ( (or (equal expr '(??  DEFAULT))
             (equal expr '(?+  DEFAULT))
             (equal expr '(?*  DEFAULT)))
         (when (< str-beg (point))
           (when (< str-beg (1- (point)))
             (push (buffer-substring-no-properties str-beg (- (point) 2))
                   result))
           (push (buffer-substring-no-properties (- (point) 2) (1- (point)))
                 result))
         (unless result
           (throw 'bad-regexp (cons "No previous term before ?" (point))))
         (let ((last (pop result))
               ;; Fix-me: use single chars later
               (op (case (car expr)
                     (?? 'opt)
                     (?+ 'one-or-more)
                     (?* 'zero-or-more))))
           (push (list op last) result))
         (setq str-beg (point))
         )
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
            (throw 'bad-regexp (cons "Trailing \\) in regexp" (- (point) 2))))
          (push (buffer-substring-no-properties str-beg str-end) result)
          (pop state)
          (setq ret-result result)
          (setq str-beg (point))
          )
        ( (equal expr '(?\| BS2))
          ;; Fix-me: semantic? Probably single items on both sides.
          (let ((last-string (buffer-substring-no-properties str-beg str-end)))
            (when (or (not (zerop (length last-string)))
                      (not result))
              (push last-string result)))
          (let ((last (pop result))
                or-result)
            (unless last (throw 'bad-regexp (cons "\| without previous element" (point))))
            (let ((or (if (eq 'and-top what) 'or 'or)))
              (setq or-result (cadr (rxx-parse-1 or str-syntax))))
            (pop state)
            (setq or-result (list last or-result))
            (let ((or-chars (catch 'or-chars
                              (dolist (in or-result)
                                (unless (and (stringp in)
                                             (= 1 (length in)))
                                  (throw 'or-chars nil)))
                              (mapconcat 'identity or-result ""))))
              (if or-chars
                  (push (list 'any or-chars) result)
                (push (cons 'or or-result) result))))
          (setq str-beg (point))
          )
        ( (eq (nth 1 expr) 'DEFAULT)
          ;; Normal char for matching
          (when want-single-item
            ;; We should be in DEFAULT state then
            (unless (eq 'DEFAULT (car state))
              (error "Internal error: want single item, but curr state=%S" (car state)))
            (skip-chars-forward "^[+*?\\\\")
            (when (memq (char-after) '(?+ ?* ??))
              (backward-char))
            (push (buffer-substring-no-properties str-beg (point)) result)
            (setq ret-result result)
            (setq str-beg (point))
            )
          )
        ( (eq (nth 1 expr) 'CHARS)
          )
        ( t (error "expr=(%c %s)" (car expr) (cadr expr))))
      (when (eq what 'or)
        ;; We only want one
        (setq ret-result result))
      )
    (unless (eq this-state (car state)) ;; Initial state
      (error "Internal error: expected this-state on state stack=%S" this-state state))
    (pop state)
    (when state (error "Internal error: state rest=%S, what=%s" state what))
    (when (eobp)
      (if (and (not result)
               (not (memq what '(and-top or-top))))
          (let ((miss (case what
                        (any "\\]")
                        (t "\\)"))))
            (throw 'bad-regexp (cons (format "Unfinished regexp, missing %s, what=%s" miss what) (point))))
        (let ((tail (buffer-substring-no-properties str-beg (point))))
          (unless (zerop (length tail))
            (push tail result)))
        (setq ret-result result)))
    ;; Return value:
    (setq what (case what
                 (and-top 'and)
                 (or-top 'or)
                 (t what)))
    (let ((res-inner (reverse ret-result)))
      (when (memq what '(and submatch))
        (setq res-inner (delete "" res-inner)))
      (cond
       ( (and (memq what '(and and-top))
              (= 1 (length res-inner)))
         (car res-inner)
         )
       ( (not (memq what '(and submatch)))
         (cons what res-inner)
         )
       ( (or (< 1 (length res-inner))
             (not (eq what 'and)))
         (cons what res-inner)
         )
       ( t
         (setq res-inner (car res-inner))
         (message "res-inner c=%S" res-inner)
         res-inner
         ))
        )))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; rxx.el ends here
