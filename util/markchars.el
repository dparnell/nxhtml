;;; markchars.el --- Mark chars fitting certain characteristics
;;
;; Author: Lennart Borgman (lennart O borgman A gmail O com)
;; Created: 2010-03-22 Mon
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
;; Mark special chars, by default nonascii chars. See `markchars-mode'.
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

;; (re-search-forward "[[:nonascii:]]")
;; äåö

;;;###autoload
(defgroup markchars nil
  "Customization group for `markchars-mode'."
  :group 'convenience)

(defface markchars-light
  '((t (:underline "light blue")))
  "Light face for `markchars-mode' char marking."
  :group 'markchars)

(defface markchars-heavy
  '((t (:underline "magenta")))
  "Heavy face for `markchars-mode' char marking."
  :group 'markchars)

(defcustom markchars-face 'markchars-heavy
  "Pointer to face used for marking chars."
  :type 'face
  :group 'markchars)

(defcustom markchars-pattern "[[:nonascii:]]+"
  "Regexp for characters to mark.
This is supposed to match single characters, or rather one or
more such characters, but you can of course \(mis)use it anyway
you want.

By default it matches nonascii-chars."
  :type 'regexp
  :group 'markchars)

(defvar markchars-keywords nil
  "Keywords for font lock.")

(defun markchars-set-keywords ()
  "Set `markchars-keywords' from options."
  (set (make-local-variable 'markchars-keywords)
       (list
        (list markchars-pattern
              (list 0 '(put-text-property (match-beginning 0) (match-end 0)
                                          'face markchars-face))))))

;;;###autoload
(defun markchars-next-special-char ()
  "Go to char after next chars matching `markchars-pattern'."
  (interactive)
  (unless (re-search-forward markchars-pattern nil t)
    (message "Found no chars matching %S forward" markchars-pattern)))

;;;###autoload
(define-minor-mode markchars-mode
  "Mark special characters.
Which characters to mark are defined by `markchars-pattern'.

The default is to mark nonascii chars with a magenta underline.

If you change anything in the customization group `markchars' you
must restart this minor mode for the changes to take effect."
  :group 'markchars
  (if markchars-mode
      (progn
        (markchars-set-keywords)
        (font-lock-add-keywords nil markchars-keywords))
    (font-lock-remove-keywords nil markchars-keywords))
  (font-lock-fontify-buffer))

;;;###autoload
(define-globalized-minor-mode markchars-global-mode markchars-mode
  (lambda () (markchars-mode 1))
  :group 'markchars)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; markchars.el ends here
