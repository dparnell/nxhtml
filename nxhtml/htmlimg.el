;;; htmlimg.el --- Display images in (X)HTML.
;;
;; Author: Lennart Borgman (lennart O borgman A gmail O com)
;; Created: 2008-09-27
(defconst htmlimg:version "0.5") ;; Version:
;; Last-Updated: 2008-09-27T13:26:46+0200 Sat
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
;; Display images referenced in <img src="..." /> inline.  See
;; `htmlimg-mode' and `htmlimg-toggle-img-display'.
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

(defvar htmlimg-img-regexp
  (rx "<img"
      (1+ space)
      (0+ (1+ (not (any " <>")))
          (1+ space))
      "src=\""
      (group (1+ (not (any "\""))))
      "\""
      (*? anything)
      "/>"))

(defgroup htmlimg nil
  "Customization group for htmlimg."
  :group 'nxhtml)

(defcustom htmlimg-margins '(50 . 5)
  "Margins when displaying image."
  :type '(cons (integer :tag "Left margin")
               (integer :tag "Top margin"))
  :set (lambda (sym val)
         (set-default sym val)
         (when (fboundp 'htmlimg-update-all-buffers)
           (htmlimg-update-all-buffers)))
  :group 'htmlimg)

(defcustom htmlimg-slice '(0 0 400 100)
  "How to slice images."
  :type '(choice (const :tag "Show whole images" nil)
                 (list :tag "Show slice of image"
                       (integer :tag "Top")
                       (integer :tag "Left")
                       (integer :tag "Width")
                       (integer :tag "Height")))
  :set (lambda (sym val)
         (set-default sym val)
         (when (fboundp 'htmlimg-update-all-buffers)
           (htmlimg-update-all-buffers)))
  :group 'htmlimg)

(defun htmlimg-next (pt display-image)
  "Display or hide next image after point PT.
If DISPLAY-IMAGE is non-nil then display image, otherwise hide it.

Return non-nil if an img tag was found."
  (let (src end img)
    (goto-char pt)
    (when (setq res (re-search-forward htmlimg-img-regexp nil t))
      (setq src (match-string-no-properties 1))
      (setq end (match-end 0))
      (if display-image
          (if (not (file-exists-p src))
              (mumamo-with-buffer-prepared-for-jit-lock
               (put-text-property (- end 2) (- end 1)
                                  'display
                                  "/>")
               (put-text-property (- end 1) end
                                  'display
                                  (propertize " Image not found"
                                              'face font-lock-warning-face)
                                  ))
            ;; Get src value before macro since buffer-file-name is nil inside.
            (setq src (expand-file-name
                       src
                       (file-name-directory (buffer-file-name))))
            (mumamo-with-buffer-prepared-for-jit-lock
             (put-text-property (- end 2) (- end 1)
                                'display
                                "/>\n")
             (setq img (create-image src nil nil
                                     :relief 5
                                     :margin htmlimg-margins))
             (when htmlimg-slice
               (let* ((sizes (image-size img t))
                      (width  (car sizes))
                      (height (cdr sizes))
                      (sl-left (nth 0 htmlimg-slice))
                      (sl-top (nth 1 htmlimg-slice))
                      (sl-width (nth 2 htmlimg-slice))
                      (sl-height (nth 3 htmlimg-slice))
                      )
                 (when (> sl-left width) (setq sl-left 0))
                 (when (> (+ sl-left sl-width) width)
                   (setq sl-width (- width sl-left)))
                 (when (> sl-top height) (setq sl-top 0))
                 (when (> (+ sl-top sl-height) height)
                   (setq sl-height (- height sl-top)))
                 (setq img (list img))
                 (setq img (cons
                            (append '(slice)
                                    htmlimg-slice
                                    (list sl-top sl-left sl-width sl-height)
                                    nil)
                            img))))
             (put-text-property (- end 1) end
                                'display img)))
        (mumamo-with-buffer-prepared-for-jit-lock
         (put-text-property (- end 2) end
                            'display nil)))
      (mumamo-with-buffer-prepared-for-jit-lock
       (put-text-property (- end 2) end
                          'htmlimg-display display-image)))
    res))

(defvar htmlimg-timer nil)
(make-variable-buffer-local 'htmlimg-timer)
(put 'htmlimg-timer 'permanent-local t)

(defun htmlimg-cancel-timer ()
  "Cancel timer for displaying/hiding images."
  (when htmlimg-timer
    (cancel-timer htmlimg-timer)
    (setq htmlimg-timer nil)))

(defun htmlimg-request-update (start end)
  "Request update of images display between START and END."
  (htmlimg-cancel-timer)
  (setq htmlimg-timer
        (run-with-idle-timer idle-update-delay
                             nil
                             'htmlimg-update-in-timer
                             start
                             end
                             (current-buffer))))

(defun htmlimg-update-in-timer (start end buffer)
  "Update image display between START and END in buffer BUFFER."
  (with-current-buffer buffer
    (let ((here (point))
          res
          prop-pos1
          prop-pos2)
      (save-restriction
        (widen)
        (goto-char start)
        (setq prop-pos1 start)
        (mumamo-with-buffer-prepared-for-jit-lock
         (while (setq prop-pos1
                      (next-single-property-change prop-pos1 'htmlimg-display))
           (put-text-property prop-pos1 (+ 2 prop-pos1) 'htmlimg-display nil)
           (put-text-property prop-pos1 (+ 2 prop-pos1) 'display nil)))
        (goto-char start)
        (setq res (save-match-data
                    (htmlimg-next (point) htmlimg-mode))))
      (when (and res
                 (< res end))
        (htmlimg-request-update res end))
      (goto-char here))))

(defun htmlimg-after-change (beg end pre-len)
  "Actions to take after a change in buffer.
This is put in `after-change-functions'.  For BEG, END and
PRE-LEN see that function."
  (let ((here (point)))
    (goto-char beg)
    (setq beg (line-beginning-position -2))
    (goto-char end)
    (setq end (line-end-position 3))
    (htmlimg-request-update beg end)))

(defun htmlimg-update-whole-buffer ()
  "Request update of image display in the current buffer."
  (save-restriction
    (widen)
    (htmlimg-request-update (point-min) (point-max))))

(defun htmlimg-update-all-buffers ()
  "Request update of image display in all buffers.
Update image display in all buffers where the option
`htmlimg-mode' is on."
  (dolist (buff (buffer-list))
    (with-current-buffer buff
      (when htmlimg-mode
        (htmlimg-update-whole-buffer)))))

;;;###autoload
(define-minor-mode htmlimg-mode
  "Display <img ...> images inline.
Images are displayed below the <img ...> tag using the margins in
`htmlimg-margins'.  The whole image or a slice of it may be
displayed, see `htmlimg-slice'.

See also the command `htmlimg-toggle-img-display'."
  :keymap nil
  :group 'htmlimg
  (if htmlimg-mode
      (add-hook 'after-change-functions 'htmlimg-after-change nil t)
    (remove-hook 'after-change-functions 'htmlimg-after-change t))
  (htmlimg-cancel-timer)
  (htmlimg-update-whole-buffer))
(put 'htmlimg-mode 'permanent-local t)

;;;###autoload
(defun htmlimg-toggle-img-display (point)
  "Toggle display of img image at point POINT.
See also the command `htmlimg-mode'."
  (interactive (list (point)))
  (save-match-data
    (let ((here (point))
          img-start
          img-end
          )
      (skip-chars-backward "^<")
      (unless (and (> (point) (point-min))
                   (= ?\< (char-before))
                   (progn
                     (backward-char)
                     (looking-at htmlimg-img-regexp)))
        (goto-char here)
        (error "No image here"))
      (setq img-start (point))
      (setq img-end (- (match-end 0) 2))
      (setq is-displayed (get-text-property img-end 'htmlimg-display))
      (htmlimg-next (point) (not is-displayed))
      (goto-char here))))


(provide 'htmlimg)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; htmlimg.el ends here
