;;; tyda.el --- Lookup words in swe/eng dictionary at tyda.se
;;
;; Author: Lennart Borgman (lennart O borgman A gmail O com)
;; Created: 2008-08-26T02:51:27+0200 Tue
;; Version: 0.1
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
;; Lookup swedish or english words in the dictionary at 
;;   http://www.tyda.se/?rid=651940&w=
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

(defvar tyda-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [(alt mouse-1)] 'tyda-lookup-word)
    (define-key map [(control ?c) ?.] 'tyda-lookup-word)
    map))

(defun tyda-lookup-word (word)
  (interactive (list (or (word-at-point)
                         (read-string "Lookup word: "))))
  (browse-url (concat "http://www.tyda.se/?rid=651940&w=" word)))

(define-minor-mode tyda-mode
  "Minor mode for looking up words at URL `http://tyda.se/'.
This requires that you are using Firefox as your web browser and
have installed the tyda-add on."
  :lighter " Tyda")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; tyda.el ends here
