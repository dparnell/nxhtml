;;; htmlw.el --- Hide some tags for writing text in XHTML
;;
;; Author: Lennart Borgman (lennart O borgman A gmail O com)
;; Created: 2008-10-03T01:29:44+0200 Thu
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

(defface htmlw-base
  '((t (:inherit font-lock-type-face)))
  "doc")

(defface htmlw-em
  '((t (:inherit htmlw-base :slant italic)))
  "doc")

(defface htmlw-strong
  '((t (:inherit htmlw-base :weight bold)))
  "doc")

(defface htmlw-link
  '((t (:inherit htmlw-base :underline t)))
  "doc")

(defconst htmlw-tag-list
  '(("i"      htmlw-em-tag-actions)
    ("b"      htmlw-strong-tag-actions)
    ("em"     htmlw-em-tag-actions)
    ("strong" htmlw-strong-tag-actions)
    ("a"      htmlw-a-tag-actions)
    ))
(defconst htmlw-single-tag-list
  '(
    ("img" htmlw-img-tag-actions)
    ))

(defun htmlw-em-tag-actions (tag-begin tag-end overlay)
  (overlay-put overlay 'face 'htmlw-em))

(defun htmlw-strong-tag-actions (tag-begin tag-end overlay)
  (overlay-put overlay 'face 'htmlw-strong))

(defun htmlw-img-tag-actions (tag-begin tag-end overlay)
  (save-match-data
    (let ((here (point-marker))
          href)
      (save-restriction
        (narrow-to-region tag-begin tag-end)
        (goto-char tag-begin)
        (when (looking-at (rx (*? anything)
                              (1+ space)
                              "src=\""
                              (submatch
                               (+ (not (any "\"\n"))))
                              "\""))
          (setq href (match-string-no-properties 1))))
      (when href
        (overlay-put overlay 'display (concat "image " href))
        (overlay-put overlay 'htmlw-url href)
        )
      (goto-char (point)))))

(defun htmlw-a-tag-actions (tag-begin tag-end overlay)
  (save-match-data
    (let ((here (point-marker))
          href)
      (save-restriction
        (narrow-to-region tag-begin tag-end)
        (goto-char tag-begin)
        (when (looking-at (rx (*? anything)
                              (1+ space)
                              "href=\""
                              (submatch
                               (+ (not (any "\"\n"))))
                              "\""))
          (setq href (match-string-no-properties 1))))
      (when href
        (overlay-put overlay 'face 'htmlw-link)
        (overlay-put overlay 'help-echo href)
        (overlay-put overlay 'mouse-face 'highlight)
        (overlay-put overlay 'htmlw-url href)
        )
      (goto-char (point)))))

(defun htmlw-get-ovl-from-keymap ()
  (catch 'ranges
    (dolist (ovl (overlays-at (point)))
      (let ((ranges (overlay-get ovl 'htmlw)))
        (when ranges
          (throw 'ranges ovl))))))

(defun htmlw-toggle-hiding-this ()
  "Toggle display of current tag."
  (interactive)
  (let* ((ovl (htmlw-get-ovl-from-keymap))
         (hiding-ranges (overlay-get ovl 'htmlw))
         (invis (get-text-property (caar hiding-ranges) 'invisible)))
    (if invis
        (progn
          (overlay-put ovl 'html-face (overlay-get ovl 'face))
          (overlay-put ovl 'face 'highlight))
      (overlay-put ovl 'face (overlay-get ovl 'html-face)))
    (mumamo-with-buffer-prepared-for-jit-lock
     (dolist (range hiding-ranges)
       (if invis
           (put-text-property (car range) (cdr range) 'invisible nil)
         (put-text-property (car range) (cdr range) 'invisible 'htmlw))))))

(defun htmlw-browse-link ()
  "Browse link."
  (interactive)
  (let* ((ovl (htmlw-get-ovl-from-keymap))
         (url (overlay-get ovl 'htmlw-url)))
    (unless url
      (error "No link in this tag"))
    (browse-url url)
    ))

(defvar htmlw-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map [(control ?c) ?+] 'htmlw-toggle-hiding-this)
    (define-key map [(control ?c) ?!] 'htmlw-browse-link)
    (define-key map [mouse-1] 'htmlw-browse-link)
    map))

;;(htmlw-make-hide-tags-regexp)
(defun htmlw-make-hide-tags-regexp ()
  (let ((tags-re
         (mapconcat 'identity
          (mapcar (lambda (elt)
                    (if (stringp elt)
                        elt
                      (car elt)))
                  htmlw-tag-list)
          "\\|"))
        ;; fix-me: single tags
        )
    (concat
     "<\\(?1:"
     "\\(?:" tags-re "\\)"
     "\\)[^>]*>\\(?3:[^<]*\\)\\(?2:</\\1>\\)"
     )))

;; after-change-functions
(defvar htmlw-pending-changes nil)
;;(setq htmlw-pending-changes nil)
;;(set-default 'htmlw-pending-changes nil)
(make-variable-buffer-local 'htmlw-pending-changes)
(put 'htmlw-pending-changes 'permanent-local t)

(defun htmlw-after-change (start end pre-len)
  (add-to-list 'htmlw-pending-changes
               (cons (copy-marker start) (copy-marker end))))
(put 'htmlw-after-change 'permanent-hook t)

(defun htmlw-post-command ()
  (condition-case err
      (htmlw-post-command-1)
    (error (message "htmlw-post-command error: %s" err))))
(put 'htmlw-post-command 'permanent-hook t)

(defun htmlw-post-command-1 ()
  (let ((pending htmlw-pending-changes)
        (here (point-marker)))
    (dolist (pend pending)
      (let* ((start (car pend))
             (end   (cdr pend))
             (start-new start)
             (end-new end))
        (dolist (ovl (overlays-in start end))
          (when (overlay-get ovl 'htmlw)
            (setq start-new (min start-new (overlay-start ovl)))
            (setq end-new (max end-new (overlay-end ovl)))
            ;; Check if visible
            (when (get-text-property start-new 'invisible)
              (setcar pend nil))
            ))
        (if (and start-new (/= start start-new))
            (setcar pend start-new)
          (goto-char start)
          (skip-chars-backward "^>")
          (backward-char)
          (skip-chars-backward "^<")
          (setcar pend (1- (point)))
          )
        (if (/= end end-new)
            (setcdr pend end-new)
          (goto-char end)
          (skip-chars-forward "^<")
          (when (eq ?/ (char-after (1+ (point))))
            (skip-chars-forward "^>")
            (setcdr pend (1+ (point))))
          )
        ))
    (setq pending (assq-delete-all nil pending))
    ;; Fix-me: sorting direction ...
    (setq pending (sort pending
                        (lambda (rec-a rec-b)
                          (< (car rec-a) (car rec-b)))))
    ;; Fix-me: delete dublicates, merge
    ;; Fix-me: make markers?
    (dolist (pend pending)
      (htmlw-reveal-tags (car pend) (cdr pend))
      (htmlw-hide-tags (car pend) (cdr pend)))
    (setq htmlw-pending-changes nil)
    (goto-char here)
    ))

(defun htmlw-hide-tags (start end)
  (let ((here (point-marker)))
    (save-restriction
      (widen)
      (goto-char start)
      (save-match-data
        (let ((hide-tags-regexp (htmlw-make-hide-tags-regexp)))
          (mumamo-with-buffer-prepared-for-jit-lock
           (while (re-search-forward hide-tags-regexp end t)
             (let ((ovl (make-overlay (match-beginning 0) (match-end 0)))
                   (tag-fun (cadr (assoc (match-string-no-properties 1)
                                        htmlw-tag-list
                                        )))
                   hiding-ranges
                   )
               (overlay-put ovl 'face 'font-lock-variable-name-face)
               (overlay-put ovl 'keymap htmlw-keymap)
               (when tag-fun
                 (funcall tag-fun (match-end 1) (match-beginning 3) ovl))
               (setq hiding-ranges
                     (list (cons (1- (match-beginning 1)) (match-beginning 3))
                           (cons (match-beginning 2) (match-end 2))))
               (overlay-put ovl 'htmlw hiding-ranges)
               (dolist (range hiding-ranges)
                 (put-text-property (car range) (cdr range) 'invisible 'htmlw))
               ;;(put-text-property (1- (match-beginning 1)) (match-beginning 3) 'invisible 'htmlw)
               ;;(put-text-property (match-beginning 2) (match-end 2) 'invisible 'htmlw)
               )
             )))))
    (goto-char here)))

(defun htmlw-reveal-tags (start end)
  (let ((here (point-marker)))
    (save-restriction
      (widen)
      (goto-char (point-min))
      (save-match-data
        (mumamo-with-buffer-prepared-for-jit-lock
         (remove-text-properties start
                                 end
                                 '(invisible htmlw))
         (dolist (ovl (overlays-in start end))
           (when (overlay-get ovl 'htmlw)
             (delete-overlay ovl)))
         )))
    (goto-char here)))

(define-minor-mode htmlw-write-mode
  "doc"
  :group 'nxhtml
  (save-restriction
    (widen)
    (if htmlw-write-mode
        (progn
         (htmlw-hide-tags (point-min) (point-max))
         (add-hook 'after-change-functions 'htmlw-after-change nil t)
         (add-hook 'pre-command-hook 'htmlw-pre-command nil t)
         (add-hook 'post-command-hook 'htmlw-post-command nil t)
         (setq buffer-invisibility-spec
               (if (listp buffer-invisibility-spec)
                   (cons 'htmlw buffer-invisibility-spec)
                 (list 'htmlw buffer-invisibility-spec)
                 ))
         )
      (htmlw-reveal-tags (point-min) (point-max))
      (remove-hook 'after-change-functions 'htmlw-after-change t)
      (remove-hook 'pre-command-hook 'htmlw-pre-command t)
      (remove-hook 'post-command-hook 'htmlw-post-command t)
      (setq buffer-invisibility-spec (delq 'htmlw buffer-invisibility-spec))
      )))
(put htmlw-write-mode 'permanent-local t)

(defun visible-point-pre-command ()
  (condition-case err
      (visible-point-pre-command-1)
    (error (message "visible-point-pre-command error: %s" err))))
(put 'visible-point-pre-command 'permanent-hook t)

(defun visible-point-pre-command-1 ()
  ;; Fix-me: widen?
  (setq visible-point-pre-column (current-column))
  (setq visible-point-pre-point (point-marker))
  (setq visible-point-pre-line (line-number-at-pos))
  )

(defvar visible-point-pre-column nil)
(put 'visible-point-pre-column 'permanent-local t)
(defvar visible-point-pre-point nil)
(put 'visible-point-pre-point 'permanent-local t)
(defvar visible-point-pre-line nil)
(put 'visible-point-pre-line 'permanent-local t)

(defun visible-point-post-command ()
  (condition-case err
      (visible-point-post-command-1)
    (error (message "visible-point-post-command error: %s" err))))
(put 'visible-point-post-command 'permanent-hook t)

(defun visible-point-post-command-1 ()
  ;;(setq visible-point-pre-column (current-column))
  ;;(setq visible-point-pre-line (line-number-at-pos))
  ;; If in invisible portion move out
  (when (invisible-p (point))
    (let* ((pre-point-line (line-number-at visible-point-pre-point))
           (current-line (line-number-at-pos))
           (point-dir
            (cond ((> (point) visible-point-pre-point)
                   1)
                  ((< (point) visible-point-pre-point)
                   -1)))
           (line-dir
            (cond ((> current-line pre-point-line)
                   1)
                  ((< current-line pre-point-line)
                   -1)))
           )
      ;; First by point
      (when point-dir
        (while (invisible-p (point))
          (cond ((=  1 point-dir)
                 (goto-char
                  (next-single-char-property-change (point) 'invisible))
                 ((= -1 point-dir)
                  (goto-char
                   (previous-single-char-property-change (point) 'invisible))
                  (t (error "point-dir=%s" point-dir)))))))
      ;; By line
      (when (and line-dir
                 (= pre-point-line (line-number-at-pos)))
        (cond ((=  1 line-dir) (forward-line))
              ((= -1 line-dir) (forward-line -1))
              (t (error "line-dir=%s" line-dir))))
      (while (invisible-p (point))
        (cond ((=  1 point-dir)
               (goto-char
                (next-single-char-property-change (point) 'invisible))
               ((= -1 point-dir)
                (goto-char
                 (previous-single-char-property-change (point) 'invisible))
                (t (error "point-dir=%s" point-dir)))))
        ))))

(define-minor-mode visible-point-mode
  "doc"
  :group 'nxhtml
  (if visible-point-mode
      (progn
        (add-hook 'pre-command-hook 'visible-point-pre-command nil t)
        (add-hook 'post-command-hook 'visible-point-post-command nil t)
        )
    (remove-hook 'pre-command-hook 'visible-point-pre-command t)
    (remove-hook 'post-command-hook 'visible-point-post-command t)
    ))

(provide 'htmlw)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; htmlw.el ends here
