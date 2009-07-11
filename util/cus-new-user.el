;;; cus-new-user.el --- Customize some important options
;;
;; Author: Lennart Borgman (lennart O borgman A gmail O com)
;; Created: 2009-07-10 Fri
;; Version: 0.2
;; Last-Updated: 2009-07-10 Fri
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
;;    Customize significant options for which different user
;;    environment expectations might dictate different defaults.
;;
;;    After an idea of Scot Becker on Emacs Devel.
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

;;(customize-for-new-user)
;;;###autoload
(defun customize-for-new-user (&optional name)
  "Show special customization page for new user.
"
  (interactive)
  ;;(setq debug-on-error t)
  ;;(setq buffer-read-only t)
  (require 'cus-edit)
  (let ((inhibit-read-only t)
        fill-pos)
    (pop-to-buffer (custom-get-fresh-buffer (or name "*Customizations for New Users*")))
    (buffer-disable-undo)
    (Custom-mode)
    (erase-buffer)
    (setq fill-pos (point))
    (widget-insert
     "Below are some custom options that new users often may want to
tweak since they may make Emacs a bit more like what they expect from using other software in their environment.

Since Emacs runs in many environment and an Emacs user may use
several of them it is hard to decide by default what a user
wants/expects.  Therefor you are given the possibility to easily
do those changes here.

Note that this is just a collection of normal custom options.
There are no new options here.

")
    (fill-region fill-pos (point))

    ;; Normal custom buffer header
    (let ((init-file (or custom-file user-init-file)))
      ;; Insert verbose help at the top of the custom buffer.
      (when custom-buffer-verbose-help
        (widget-insert "Editing a setting changes only the text in this buffer."
                       (if init-file
                           "
To apply your changes, use the Save or Set buttons.
Saving a change normally works by editing your init file."
                         "
Currently, these settings cannot be saved for future Emacs sessions,
possibly because you started Emacs with `-q'.")
                       "\nFor details, see ")
        (widget-create 'custom-manual
                       :tag "Saving Customizations"
                       "(emacs)Saving Customizations")
        (widget-insert " in the ")
        (widget-create 'custom-manual
                       :tag "Emacs manual"
                       :help-echo "Read the Emacs manual."
                       "(emacs)Top")
        (widget-insert "."))
      (widget-insert "\n")
      ;; The custom command buttons are also in the toolbar, so for a
      ;; time they were not inserted in the buffer if the toolbar was in use.
      ;; But it can be a little confusing for the buffer layout to
      ;; change according to whether or nor the toolbar is on, not to
      ;; mention that a custom buffer can in theory be created in a
      ;; frame with a toolbar, then later viewed in one without.
      ;; So now the buttons are always inserted in the buffer.  (Bug#1326)
;;;    (when (not (and (bound-and-true-p tool-bar-mode) (display-graphic-p)))
      (if custom-buffer-verbose-help
          (widget-insert "\n
 Operate on all settings in this buffer that are not marked HIDDEN:\n"))
      (let ((button (lambda (tag action active help icon)
                      (widget-insert " ")
                      (if (eval active)
                          (widget-create 'push-button :tag tag
                                         :help-echo help :action action))))
            (commands custom-commands))
        (apply button (pop commands)) ; Set for current session
        (apply button (pop commands)) ; Save for future sessions
        (if custom-reset-button-menu
            (progn
              (widget-insert " ")
              (widget-create 'push-button
                             :tag "Reset buffer"
                             :help-echo "Show a menu with reset operations."
                             :mouse-down-action 'ignore
                             :action 'custom-reset))
          (widget-insert "\n")
          (apply button (pop commands)) ; Undo edits
          (apply button (pop commands)) ; Reset to saved
          (apply button (pop commands)) ; Erase customization
          (widget-insert "  ")
          (pop commands) ; Help (omitted)
          (apply button (pop commands)))) ; Exit
      (widget-insert "\n\n")

      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;; Editor emulator level

      (widget-insert "\n")
      (setq fill-pos (point))
      (widget-insert
"Emacs can emulate some common editing behaviours (and some uncommon too).
For the most common ones you can decide if you want to use them here:
")
      (fill-region fill-pos (point))
      (cusnu-mark-part-desc fill-pos (point))

      ;; CUA Mode
      (cusnu-insert-options '((cua-mode custom-variable)))

      ;; Viper Mode
      (widget-insert "\n")
      (widget-insert (propertize "Viper" 'face 'custom-variable-tag))
      (widget-insert ":")
      (setq fill-pos (point))
      (widget-insert "
   Viper is currently set up in a special way, please see the
   command `viper-mode'.  You can use custom to set up most of
   it.  However if you want to load Viper at startup you must
   explicitly include \(require 'viper) in your .emacs.
")
      (fill-region fill-pos (point))

      ;; Viper Mode
      (backward-delete-char 1)
      (cusnu-insert-options '((viper-mode custom-variable)))

      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;; OS specific

      (widget-insert "\n")
      (setq fill-pos (point))
      (widget-insert (format "OS specific options (%s): \n" system-type))
      (fill-region fill-pos (point))
      (cusnu-mark-part-desc fill-pos (point))

      (if cusno-insert-os-spec-fun
          (funcall cusno-insert-os-spec-fun)
       (widget-insert "No OS specific customizations.\n"))

      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;; Disputed settings

      (widget-insert "\n")
      (setq fill-pos (point))
      (widget-insert
"Some old time Emacs users want to change the options below:
")
      (fill-region fill-pos (point))
      (cusnu-mark-part-desc fill-pos (point))

      (cusnu-insert-options '((global-visual-line-mode custom-variable)))
      (cusnu-insert-options '((word-wrap custom-variable)))
      (cusnu-insert-options '((blink-cursor-mode custom-variable)))
      (cusnu-insert-options '((tool-bar-mode custom-variable)))
      (cusnu-insert-options '((tooltip-mode custom-variable)))
      ;;(cusnu-insert-options '((initial-scratch-message custom-variable)))

      (widget-insert "\n")
      (setq fill-pos (point))
      (widget-insert
"My skin options - for exporting custom options to other users (or yourself on another computer)")
      (fill-region fill-pos (point))
      (cusnu-mark-part-desc fill-pos (point))

      (widget-insert "\n")
      (cusnu-insert-options '((cusnu-my-skin-options custom-variable)))
      (widget-insert "\n")
      (widget-create 'push-button
                     :tag "Export my skin options"
                     :action (lambda (&rest ignore)
                               (let ((use-dialog-box nil))
                                 (call-interactively 'cusnu-export-my-skin-options))))

      ;; Finish setup buffer
      (mapc 'custom-magic-reset custom-options)
      (cusnu-make-xrefs)
      (widget-setup)
      (buffer-enable-undo)
      (goto-char (point-min)))))

(defvar cusno-insert-os-spec-fun nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Example on Emacs+Emacw32
(when (boundp 'emacsw32-version)
  (defun cusnu-emacsw32-show-custstart (&rest args)
    (emacsw32-show-custstart))
  (setq cusno-insert-os-spec-fun 'cusnu-insert-emacsw32-specific-part)
  (defun cusnu-insert-emacsw32-specific-part ()
    (cusnu-insert-options '((w32-meta-style custom-variable)))
    (widget-insert "\n")
    (widget-insert (propertize "EmacsW32" 'face 'custom-variable-tag))
    (widget-insert "
   Easy setup for Emacs+EmacsW32.")
    (widget-insert "\n   ")
    (widget-create 'push-button :tag "Customize EmacsW32"
                   ;;:help-echo help
                   :action 'cusnu-emacsw32-show-custstart)
    (widget-insert "\n")))
;; End example
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun cusnu-mark-part-desc (beg end)
  (let ((ovl (make-overlay beg end)))
    (overlay-put ovl 'face 'highlight)))

(defun cusnu-make-xrefs (&optional beg end)
  (save-restriction
    (when (or beg end)
      (unless beg (setq beg (point-min)))
      (unless end (setq end (point-max)))
      (narrow-to-region beg end))
    (let ((here (point)))
      (goto-char (point-min))
      (cusnu-help-insert-xrefs 'cusnu-help-xref-button)
      (goto-char here))))

(defun widget-info-link-action (widget &optional event)
  "Open the info node specified by WIDGET."
  (info-other-window (widget-value widget)))

(defun widget-documentation-string-value-create (widget)
  ;; Insert documentation string.
  (let ((doc (widget-value widget))
	(indent (widget-get widget :indent))
	(shown (widget-get (widget-get widget :parent) :documentation-shown))
	(start (point)))
    (if (string-match "\n" doc)
	(let ((before (substring doc 0 (match-beginning 0)))
	      (after (substring doc (match-beginning 0)))
	      button)
	  (when (and indent (not (zerop indent)))
	    (insert-char ?\s indent))
	  (insert before ?\s)
	  (widget-documentation-link-add widget start (point))
	  (setq button
		(widget-create-child-and-convert
		 widget (widget-get widget :visibility-widget)
		 :help-echo "Show or hide rest of the documentation."
		 :on "Hide Rest"
		 :off "More"
		 :always-active t
		 :action 'widget-parent-action
		 shown))
	  (when shown
	    (setq start (point))
	    (when (and indent (not (zerop indent)))
	      (insert-char ?\s indent))
	    (insert after)
	    (widget-documentation-link-add widget start (point))
            (cusnu-make-xrefs start (point))
            )
	  (widget-put widget :buttons (list button)))
      (when (and indent (not (zerop indent)))
	(insert-char ?\s indent))
      (insert doc)
      (widget-documentation-link-add widget start (point))))
  (insert ?\n))
(defun cusnu-help-xref-button (match-number type what &rest args)
  (let ((beg (match-beginning match-number))
        (end (match-end match-number)))
  (if nil
      (let ((ovl (make-overlay beg end)))
        (overlay-put ovl 'face 'highlight))
    (let* ((tag (match-string match-number))
           (value what)
            (wid-type (cond
                       ((eq type 'help-variable)
                        'variable-link)
                       ((eq type 'help-function)
                        'function-link)
                       ((eq type 'help-info)
                        'custom-manual)
                       (t nil)))
          )
      (when wid-type
        (delete-region beg end)
        (backward-char)
        ;;(tag action active help icon)
        (widget-create wid-type
                       ;;tag
                       :value value
                       :tag tag
                       :keymap custom-mode-link-map
                       :follow-link 'mouse-face
                       :button-face 'custom-link
                       :mouse-face 'highlight
                       :pressed-face 'highlight
                       ;;:help-echo help
                       )))))
    )

;; Override default ... ;-)
(define-widget 'documentation-link 'link
  "Link type used in documentation strings."
  ;;:tab-order -1
  :help-echo "Describe this symbol"
  :button-face 'custom-link
  :action 'widget-documentation-link-action)

(defun cusnu-xref-niy (&rest ignore)
  (message "Not implemented yet"))

(defun cusnu-describe-function (wid &rest ignore)
  (let ((fun (widget-get wid :what))
        )
    (describe-function fun)))

(defun cusnu-help-insert-xrefs (help-xref-button)
  ;; The following should probably be abstracted out.
  (unwind-protect
      (progn
        ;; Info references
        (save-excursion
          (while (re-search-forward help-xref-info-regexp nil t)
            (let ((data (match-string 2)))
              (save-match-data
                (unless (string-match "^([^)]+)" data)
                  (setq data (concat "(emacs)" data))))
              (funcall help-xref-button 2 'help-info data))))
        ;; URLs
        (save-excursion
          (while (re-search-forward help-xref-url-regexp nil t)
            (let ((data (match-string 1)))
              (funcall help-xref-button 1 'help-url data))))
        ;; Mule related keywords.  Do this before trying
        ;; `help-xref-symbol-regexp' because some of Mule
        ;; keywords have variable or function definitions.
        (if help-xref-mule-regexp
            (save-excursion
              (while (re-search-forward help-xref-mule-regexp nil t)
                (let* ((data (match-string 7))
                       (sym (intern-soft data)))
                  (cond
                   ((match-string 3) ; coding system
                    (and sym (coding-system-p sym)
                         (funcall help-xref-button 6 'help-coding-system sym)))
                   ((match-string 4) ; input method
                    (and (assoc data input-method-alist)
                         (funcall help-xref-button 7 'help-input-method data)))
                   ((or (match-string 5) (match-string 6)) ; charset
                    (and sym (charsetp sym)
                         (funcall help-xref-button 7 'help-character-set sym)))
                   ((assoc data input-method-alist)
                    (funcall help-xref-button 7 'help-character-set data))
                   ((and sym (coding-system-p sym))
                    (funcall help-xref-button 7 'help-coding-system sym))
                   ((and sym (charsetp sym))
                    (funcall help-xref-button 7 'help-character-set sym)))))))
        ;; Quoted symbols
        (save-excursion
          (while (re-search-forward help-xref-symbol-regexp nil t)
            (let* ((data (match-string 8))
                   (sym (intern-soft data)))
              (if sym
                  (cond
                   ((match-string 3)  ; `variable' &c
                    (and (or (boundp sym) ; `variable' doesn't ensure
                                        ; it's actually bound
                             (get sym 'variable-documentation))
                         (funcall help-xref-button 8 'help-variable sym)))
                   ((match-string 4)   ; `function' &c
                    (and (fboundp sym) ; similarly
                         (funcall help-xref-button 8 'help-function sym)))
                   ((match-string 5) ; `face'
                    (and (facep sym)
                         (funcall help-xref-button 8 'help-face sym)))
                   ((match-string 6)) ; nothing for `symbol'
                   ((match-string 7)
;;;  this used:
;;; 			  #'(lambda (arg)
;;; 			      (let ((location
;;; 				     (find-function-noselect arg)))
;;; 				(pop-to-buffer (car location))
;;; 				(goto-char (cdr location))))
                    (funcall help-xref-button 8 'help-function-def sym))
                   ((and
                     (facep sym)
                     (save-match-data (looking-at "[ \t\n]+face\\W")))
                    (funcall help-xref-button 8 'help-face sym))
                   ((and (or (boundp sym)
                             (get sym 'variable-documentation))
                         (fboundp sym))
                    ;; We can't intuit whether to use the
                    ;; variable or function doc -- supply both.
                    (funcall help-xref-button 8 'help-symbol sym))
                   ((and
                     (or (boundp sym)
                         (get sym 'variable-documentation))
                     (or
                      (documentation-property
                       sym 'variable-documentation)
                      (condition-case nil
                          (documentation-property
                           (indirect-variable sym)
                           'variable-documentation)
                        (cyclic-variable-indirection nil))))
                    (funcall help-xref-button 8 'help-variable sym))
                   ((fboundp sym)
                    (funcall help-xref-button 8 'help-function sym)))))))
        ;; An obvious case of a key substitution:
        (save-excursion
          (while (re-search-forward
                  ;; Assume command name is only word and symbol
                  ;; characters to get things like `use M-x foo->bar'.
                  ;; Command required to end with word constituent
                  ;; to avoid `.' at end of a sentence.
                  "\\<M-x\\s-+\\(\\sw\\(\\sw\\|\\s_\\)*\\sw\\)" nil t)
            (let ((sym (intern-soft (match-string 1))))
              (if (fboundp sym)
                  (funcall help-xref-button 1 'help-function sym)))))
        ;; Look for commands in whole keymap substitutions:
        (save-excursion
          ;; Make sure to find the first keymap.
          (goto-char (point-min))
          ;; Find a header and the column at which the command
          ;; name will be found.

          ;; If the keymap substitution isn't the last thing in
          ;; the doc string, and if there is anything on the
          ;; same line after it, this code won't recognize the end of it.
          (while (re-search-forward "^key +binding\n\\(-+ +\\)-+\n\n"
                                    nil t)
            (let ((col (- (match-end 1) (match-beginning 1))))
              (while
                  (and (not (eobp))
                       ;; Stop at a pair of blank lines.
                       (not (looking-at "\n\\s-*\n")))
                ;; Skip a single blank line.
                (and (eolp) (forward-line))
                (end-of-line)
                (skip-chars-backward "^ \t\n")
                (if (and (>= (current-column) col)
                         (looking-at "\\(\\sw\\|\\s_\\)+$"))
                    (let ((sym (intern-soft (match-string 0))))
                      (if (fboundp sym)
                          (funcall help-xref-button 0 'help-function sym))))
                (forward-line))))))
    ;;(set-syntax-table stab)
    ))

(defun cusnu-insert-options (options)
  (widget-insert "\n")
  (setq custom-options
        (append
         (if (= (length options) 1)
             (mapcar (lambda (entry)
                       (widget-create (nth 1 entry)
                                      ;;:documentation-shown t
                                      :custom-state 'unknown
                                      :tag (custom-unlispify-tag-name
                                            (nth 0 entry))
                                      :value (nth 0 entry)))
                     options)
           (let ((count 0)
                 (length (length options)))
             (mapcar (lambda (entry)
                       (prog2
                           (message "Creating customization items ...%2d%%"
                                    (/ (* 100.0 count) length))
                           (widget-create (nth 1 entry)
                                          :tag (custom-unlispify-tag-name
                                                (nth 0 entry))
                                          :value (nth 0 entry))
                         (setq count (1+ count))
                         (unless (eq (preceding-char) ?\n)
                           (widget-insert "\n"))
                         (widget-insert "\n")))
                     options)))
         custom-options))
  (unless (eq (preceding-char) ?\n)
    (widget-insert "\n"))
  )

(defun cusnu-is-custom-obj (sym)
  "Return non-nil if symbol SYM is customizable."
  (or (get sym 'custom-type)
      (get sym 'face)
      (get sym 'custom-group)
      ))

(define-widget 'custom-symbol 'symbol
  "A customizable symbol."
  :prompt-match 'cusnu-is-custom-obj
  :prompt-history 'widget-variable-prompt-value-history
  :complete-function (lambda ()
		       (interactive)
		       (lisp-complete-symbol 'cusnu-is-custom-obj))
  :tag "Custom option")

(defun cusnu-set-my-skin-options (sym val)
  (set-default sym val)
  (let ((group (nth 0 val))
        (doc   (nth 1 val))
        (members (nth 2 val)))
    (custom-declare-group group nil doc)
    (put group 'custom-group nil)
    (dolist (opt members)
      (let ((type (cond
                   ((get opt 'face) 'custom-face)
                   ((get opt 'custom-type) 'custom-variable)
                   ((get opt 'custom-group 'custom-group)))))
        (when type
          (custom-add-to-group group opt type))))))

(defcustom cusnu-my-skin-options '(my-skin-group "My skin group" nil)
  "My custom options.
You can export these to a file with
`cusnu-export-my-skin-options' so that others can use them."
  :type '(list (symbol :tag "My custom group name")
               (string :tag "My custom group description")
               (repeat custom-symbol))
  :set 'cusnu-set-my-skin-options
  )

(defun cusnu-export-my-skin-options (file)
  "Export to file FILE custom options in `cusnu-my-skin-options'."
  (interactive "FTo file: ")
  (when (file-exists-p file)
    (error "File %s already exists" file))
  (let ((grp (nth 0 cusnu-my-skin-options))
        (buf (find-file file)))
    (with-current-buffer buf
      (insert (format-time-string ";; Here is my skin custom group %Y-%m-%d.\n")))
    (cusnu-export-cust-group grp buf)))

(defun cusnu-export-cust-group (group buf)
  "Export custom group GROUP to end if buffer BUF."
  (let (start
        (doc (get group 'group-documentation))
        (members (mapcar (lambda (rec)
                           (car rec))
                         (get group 'custom-group))))
    (with-current-buffer buf
      (emacs-lisp-mode)
      (insert (format ";;;;;; Customization group %s\n" group))
      (insert "
;; This file defines the group and sets the options in it, but does
;; not save the values to your init file.
")
      (insert (format "(let ((grp '%s))\n" group))
      (insert (format "  (custom-declare-group grp nil %S)\n" doc))
      (insert "  (put grp 'custom-group nil)\n")
      (dolist (opt members)
        (let ((my-val (or (get opt 'saved-value)
                           (get opt 'customized-value)))
              (type (cond
                     ((get opt 'face) 'custom-face)
                     ((get opt 'custom-type) 'custom-variable)
                     ((get opt 'custom-group 'custom-group)))))
          (when (and type my-val)
            (insert (format "  (custom-add-to-group grp '%s '%s)\n"
                            opt type)))))
      (insert "  (custom-set-variables\n")
      (dolist (opt members)
        (let ((my-val (or (get opt 'saved-value)
                           (get opt 'customized-value))))
          (when my-val
            (insert (format "   '(%s %S)\n" opt (custom-quote (symbol-value opt)))))))
      (insert "   ))\n")
      )))

(provide 'cus-new-user)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; cus-new-user.el ends here
