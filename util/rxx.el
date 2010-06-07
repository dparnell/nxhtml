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

(defvar my-rxx-test-details t)

(defvar my-rxx-result nil)
(defun my-rxx-insert ()
  "testing"
  (interactive)
  (insert "(rx "
          (format "%S" my-rxx-result)
          ")"))

(defun my-rxx-parse-all (str-syntax)
  "Test all rows in buffer."
  (interactive "P")
  (widen)
  (let ((my-rxx-test-details nil))
    (goto-char (point-min))
    (while (not (eobp))
      (my-rxx-parse str-syntax)
      (forward-line 1))))

(defun my-rxx-parse (str-syntax)
  "testing"
  (interactive "P")
  (save-restriction
    (widen)
    (narrow-to-region (point-at-bol) (point-at-eol))
    (let* ((src (buffer-substring-no-properties (point-max) (point-min)))
           (res-rx-rec (rxx-parse str-syntax))
           (dummy (when my-rxx-test-details (message "res-rx-rec=%S" res-rx-rec)))
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
        (when my-rxx-test-details (message "res-rx-to-string=%s" res-rx-to-string))
        (when same-str (setq res-rx-to-string (concat "EQUAL STR=" res-rx-to-string)))
        (when same-rx-again (setq res-rx-again "EQUAL RX"))
        (web-vcs-message-with-face
         res-face
         "parsed \"%s\" => %S => \"%s\" => %S" src res-rx res-rx-to-string res-rx-again)))))

(global-set-key [(f9)] 'my-rxx-parse)
(global-set-key [(control f9)] 'my-rxx-parse-all)
(global-set-key [(shift f9)] 'my-rxx-insert)

(defun rxx-parse (str-syntax)
  "Parse buffer regexp between point min and max.
If STR-SYNTAX is non-nil \\ must be doubled and things like \\n
are recognized."
  (when my-rxx-test-details (web-vcs-message-with-face 'highlight "regexp src=%S" (buffer-string)))
  (goto-char (point-min))
  (let* (ok
         (res (catch 'bad-regexp
                (prog1
                    (rxx-parse-1 'and-top str-syntax nil)
                  (setq ok t)))))
    (if ok
        (cons t res)
      (cons nil res))))

(defmacro rxx-parse-consume ()
  ;; Play with
  ;; - state
  ;; - str-beg, str-end
  ;; - result
  ;; - sub call
  )
(defun rxx-parse-1 (what str-syntax end-with)
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
        (want-single-item (not (memq what '(and and-top submatch)))))
    (while (not (or (eobp)
                    ret-result))
      (setq expr (list (char-after) (car state)))
      (forward-char)
      (cond
       ;; Fix-me: greedy, times
       ( (equal expr '(?\[ CHARS))
         (unless (eq (char-after) ?:)
           (throw 'bad-regexp (cons "[ inside a char alt must start a char class, [:" (point))))
         (let ((pre (buffer-substring-no-properties str-beg (1- (point)))))
           (unless (zerop (length pre))
             (push pre result)))
         (forward-char)
         (let ((cbeg (point))
               class
               class-sym)
           (skip-chars-forward "^:")
           (setq class (buffer-substring-no-properties cbeg (point)))
           (forward-char)
           (unless (eq (char-after) ?\])
             (throw 'bad-regexp (cons "Char class must end with :]" (point))))
           (setq class-sym (intern-soft class))
           (unless class-sym
             (throw 'bad-regexp (cons (format "Unknown char class %S" class) (point))))
           (push class-sym result)
           (forward-char 2)
           )
         (setq str-beg (point))
         )
       ( (equal expr '(?\_ BS2))
         (pop state)
         (push 'BS2_ state)
         )
       ( (equal expr '(?\< BS2_))
         (let ((pre (buffer-substring-no-properties str-beg (- (point) 3))))
           (unless (zerop (length pre))
             (push pre result)))
         (pop state)
         (setq str-beg (point))
         (push 'symbol-start result)
         )
       ( (equal expr '(?\> BS2_))
         (let ((pre (buffer-substring-no-properties str-beg (- (point) 3))))
           (unless (zerop (length pre))
             (push pre result)))
         (pop state)
         (setq str-beg (point))
         (push 'symbol-end result)
         )
       ( (equal expr '(?\< BS2))
         (let ((pre (buffer-substring-no-properties str-beg (- (point) 2))))
           (unless (zerop (length pre))
             (push pre result)))
         (pop state)
         (setq str-beg (point))
         (push 'bow result)
         )
       ( (equal expr '(?w BS2))
         (let ((pre (buffer-substring-no-properties str-beg (- (point) 2))))
           (unless (zerop (length pre))
             (push pre result)))
         (pop state)
         (setq str-beg (point))
         (push '(any word) result)
         )
       ( (equal expr '(?W BS2))
         (let ((pre (buffer-substring-no-properties str-beg (- (point) 2))))
           (unless (zerop (length pre))
             (push pre result)))
         (pop state)
         (setq str-beg (point))
         (push '(not (any word)) result)
         )
       ( (equal expr '(?\> BS2))
         (let ((pre (buffer-substring-no-properties str-beg (- (point) 2))))
           (unless (zerop (length pre))
             (push pre result)))
         (pop state)
         (setq str-beg (point))
         (push 'eow result)
         )
       ( (equal expr '(?b BS2))
         (let ((pre (buffer-substring-no-properties str-beg (- (point) 2))))
           (unless (zerop (length pre))
             (push pre result)))
         (pop state)
         (setq str-beg (point))
         (push 'word-boundary result)
         )
       ( (equal expr '(?B BS2))
         (let ((pre (buffer-substring-no-properties str-beg (- (point) 2))))
           (unless (zerop (length pre))
             (push pre result)))
         (pop state)
         (setq str-beg (point))
         (push 'not-word-boundary result)
         )
       ( (equal expr '(?\= BS2))
         (let ((pre (buffer-substring-no-properties str-beg (- (point) 2))))
           (unless (zerop (length pre))
             (push pre result)))
         (pop state)
         (setq str-beg (point))
         (push 'point result)
         )
       ( (equal expr '(?\` BS2))
         (let ((pre (buffer-substring-no-properties str-beg (- (point) 2))))
           (unless (zerop (length pre))
             (push pre result)))
         (pop state)
         (setq str-beg (point))
         (push 'buffer-start result)
         )
       ( (equal expr '(?\' BS2))
         (let ((pre (buffer-substring-no-properties str-beg (- (point) 2))))
           (unless (zerop (length pre))
             (push pre result)))
         (pop state)
         (setq str-beg (point))
         (push 'buffer-end result)
         )
       ( (equal expr '(?^ DEFAULT))
         (let ((pre (buffer-substring-no-properties str-beg (1- (point)))))
           (unless (zerop (length pre))
             (push pre result)))
         (setq str-beg (point))
         (push 'bol result)
         )
       ( (equal expr '(?$ DEFAULT))
         (let ((pre (buffer-substring-no-properties str-beg (1- (point)))))
           (unless (zerop (length pre))
             (push pre result)))
         (setq str-beg (point))
         (push 'eol result)
         )
       ( (equal expr '(?. DEFAULT))
         (let ((pre (buffer-substring-no-properties str-beg (1- (point)))))
           (unless (zerop (length pre))
             (push pre result)))
         (setq str-beg (point))
         (push 'nonl result)
         )
       ( (equal expr '(?\[  DEFAULT))
         (push (buffer-substring-no-properties str-beg (1- (point))) result)
         (push (rxx-parse-1 'any str-syntax "]") result)
         (setq str-beg (point))
         )
       ( (equal expr '(?\]  CHARS))
         (if (string= end-with "]")
             (setq end-with nil)
           (throw 'bad-regexp (list "Trailing ]" (1- (point)))))
         (push (buffer-substring-no-properties str-beg (1- (point))) result)
         (setq ret-result result)
         (setq str-beg (point))
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
         (unless result (push "" result))
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
                (push (rxx-parse-1 'submatch str-syntax "\\)") result)
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
              (push (rxx-parse-1 'and str-syntax "\\)") result)
              (setq str-beg (point))))
          )
        ( (equal expr '(?\) BS2))
          ;; (when (memq what '(and-top or-top))
          ;;   (throw 'bad-regexp (cons "Trailing \\) in regexp" (- (point) 2))))
          (if (string= end-with "\\)")
              (setq end-with nil)
            (throw 'bad-regexp (cons "Trailing \\) in regexp" (- (point) 2))))
          (push (buffer-substring-no-properties str-beg str-end) result)
          (pop state)
          (setq ret-result result)
          (setq str-beg (point))
          )
        ( (equal expr '(?\| BS2))
          ;; Fix-me: we only want the last char here. And perhaps there is no string before.
          (when (> str-end str-beg)
            (let ((last-string (buffer-substring-no-properties str-beg (1- str-end))))
              (when (or (not (zerop (length last-string)))
                        (not result))
                (push last-string result)))
            (push (buffer-substring-no-properties (1- str-end) str-end) result))
          (let ((last (pop result))
                or-result)
            ;;(unless last (throw 'bad-regexp (cons "\| without previous element" (point))))
            (unless last (setq last "")) ;; Will be made an 'opt below.
            (let ((or (if (eq 'and-top what) 'or 'or)))
              (setq or-result (cadr (rxx-parse-1 or str-syntax nil))))
            (pop state)
            (setq or-result (list last or-result))
            ;; Rework some degenerate cases to make it easier to test
            ;; if we have done things right.
            (when (= 2 (length or-result))
              (setq or-result (delete "" or-result)))
            (if (= 1 (length or-result))
                ;; One side of or is empty so it is an 'opt.
                (push (cons 'opt or-result) result)
              (let ((or-chars (catch 'or-chars
                                (dolist (in or-result)
                                  (unless (and (stringp in)
                                               (= 1 (length in)))
                                    (throw 'or-chars nil)))
                                (mapconcat 'identity or-result ""))))
                ;; Only single chars so this is an 'any.
                (if or-chars
                    (push (list 'any or-chars) result)
                  (push (cons 'or or-result) result)))))
          (setq str-beg (point))
          )
        ( (eq (nth 1 expr) 'DEFAULT) ;; Normal char that matches itself
          (when want-single-item
            (push (buffer-substring-no-properties str-beg (point)) result)
            (setq str-beg (point)) ;; why?
            )
          )
        ( (eq (nth 1 expr) 'CHARS)
          )
        ( t (error "expr=(%c %s)" (car expr) (cadr expr))))
      (when want-single-item ;;(eq what 'or)
        ;; We only want one
        (setq ret-result result))
      )

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; Clean up and test after loop before return.

    (unless (eq this-state (car state)) ;; Initial state
      (error "Internal error: expected %S on state stack=%S" this-state state))
    (pop state)
    (when state (error "Internal error: state rest=%S, what=%s" state what))
    (when (eobp)
      (if end-with
          (throw 'bad-regexp (cons (format "Unfinished regexp, missing %s, what=%s" end-with what) (point)))
        (let ((tail (buffer-substring-no-properties str-beg (point))))
          (when (or (not result)
                    (not (zerop (length tail))))
            (push tail result)))))
    (setq ret-result result)

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
         (when my-rxx-test-details (message "res-inner c=%S" res-inner))
         res-inner)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; rxx.el ends here
