;;; mozadd.el --- Additional functionality for MozRepl
;;
;; Author: Lennart Borgman (lennart O borgman A gmail O com)
;; Created: 2009-07-22 Wed
(defconst mozadd:version "0.2") ;; Version:
;; Last-Updated: 2009-08-04 Tue
;; URL:
;; Keywords:
;; Compatibility:
;;
;; Features that might be required by this library:
;;
  ;; `cc-cmds', `cc-defs', `cc-engine', `cc-vars', `comint', `json',
  ;; `moz', `regexp-opt', `ring'.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; Live tracking of editing changes, see
;;   `mozadd-mirror-mode'
;;   `mozadd-refresh-edited-on-save-mode'
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

(require 'moz)
(require 'json)
(eval-when-compile (require 'org))
(require 're-builder)
(require 'url-util)
;;(load 'isearch)

(defun mozadd-warning (format-string &rest args)
  (let ((str (apply 'format format-string args)))
    (message "%s" (propertize str 'face 'secondary-selection))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Refresh Firefox after save etc

;; Partly after an idea on EmacsWiki

(defvar mozadd-edited-buffer nil)
(setq mozadd-edited-buffer nil)

;;;###autoload
(define-minor-mode mozadd-refresh-edited-on-save-mode
  "Refresh mozadd edited file in Firefox when saving file.
The mozadd edited file is the file in the last buffer visited in
`mozadd-mirror-mode'.

You can use this for example when you edit CSS files.

The mozadd edited file must be shown in Firefox and visible."
  :lighter "MozRefresh"
  (if mozadd-refresh-edited-on-save-mode
      (add-hook 'after-save-hook 'mozadd-queue-reload-mozilla-edited-file nil t)
    (remove-hook 'after-save-hook 'mozadd-queue-reload-mozilla-edited-file t)))
(put 'mozadd-refresh-edited-on-save-mode 'permanent-local t)

;;;###autoload
(define-globalized-minor-mode global-mozadd-refresh-edited-on-save-mode
  mozadd-refresh-edited-on-save-mode
  (lambda ()
    (when (or (derived-mode-p 'css-mode)
              (mozadd-html-buffer-file-p))
      (mozadd-refresh-edited-on-save-mode 1))))

(defun mozadd-queue-reload-mozilla-edited-file ()
  "Reload edited file."
  (when (buffer-live-p mozadd-edited-buffer)
    (if (buffer-modified-p mozadd-edited-buffer)
        (mozadd-warning "Mozadd: Edited buffer %s is not saved, can't reload browser."
                          (buffer-name mozadd-edited-buffer))
      (mozadd-add-queue-get-mirror-location)
      (mozadd-add-task-1 'mozadd-send-refresh-edited-to-mozilla))))

(defun mozadd-send-refresh-edited-to-mozilla ()
  "Update the remote mozrepl instance"
  (with-current-buffer mozadd-edited-buffer
    (if (not (mozadd-edited-file-is-shown))
        (mozadd-warning "Mozadd: Edited buffer %s is not shown, can't reload browser."
                          (buffer-name mozadd-edited-buffer))
      (comint-send-string (inferior-moz-process)
                          "setTimeout(BrowserReload(), \"1000\");")))
  (mozadd-exec-next))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Mirror html buffer in Firefox

;; Partly after an idea on
;; http://people.internetconnection.net/2009/02/interactive-html-development-in-emacs/

;; Fun, it kind of works, but is perhaps totally useless .... - slow
;; and maybe scrolling... - but the file I am testing with have 3000
;; lines...

;; Fix-me: How do you get the currently shown page in Firefox?

(defun mozadd-perhaps-start ()
  "Start if MozRepl if not running. Return message if not ok."
  (unless (buffer-live-p inferior-moz-buffer)
    (condition-case err
        (progn
          (inferior-moz-start-process)
          nil)
      (error (error-message-string err)))))

(defvar mozadd-mirror-location nil)
(make-variable-buffer-local 'mozadd-mirror-location)
(put 'mozadd-mirror-location 'permanent-local t)

(defvar mozadd-initial-mirror-location nil)
(make-variable-buffer-local 'mozadd-initial-mirror-location)
(put 'mozadd-initial-mirror-location 'permanent-local t)

;;(mozadd-get-comint-string-part "\"hi\" there")
(defun mozadd-get-comint-string-part (comint-output)
  (save-match-data
    (message "Received from Mozilla (%d chars) ..." (length comint-output))
    (if (string-match "^\".*?\"" comint-output)
        (match-string 0 comint-output)
      comint-output)))

(defun mozadd-get-initial-mirror-location (comint-output)
  ;;(message "mozadd-get-initial-mirror-location %S" comint-output)
  (with-current-buffer mozadd-edited-buffer
    (setq mozadd-initial-mirror-location (mozadd-get-comint-string-part comint-output))
    (require 'org)
    (setq mozadd-initial-mirror-location
	  (org-link-escape mozadd-initial-mirror-location
                           org-link-escape-chars-browser)))

  (mozadd-exec-next)
  comint-output)

(defun mozadd-get-mirror-location (comint-output)
  ;;(message "mozadd-get-mirror-location %S" comint-output)
  (with-current-buffer mozadd-edited-buffer
    (setq mozadd-mirror-location (mozadd-get-comint-string-part comint-output)))
  (mozadd-exec-next)
  comint-output)

(defun mozadd-add-queue-get-mirror-location ()
  (mozadd-add-task "content.location.href" 'mozadd-get-mirror-location))

(defun mozadd-skip-output-until-prompt (comint-output)
  ;;(message "mozadd-skip-output-until-prompt %S" comint-output)
  (if (not (string-match-p "\\(\\w+\\)> $" comint-output))
      ""
    (message "Recieved ready (prompt) from Mozilla %s" (current-time-string))
    (mozadd-exec-next)
    comint-output
    ""
    ))

(defun mozadd-queue-send-buffer-content-to-mozilla (buffer)
  (mozadd-add-queue-get-mirror-location)
  (setq mozadd-edited-buffer buffer)
  ;; Queue only a single send of buffer content.
  (mozadd-add-task-1 'mozadd-send-buffer-content-to-mozilla t))

(defun mozadd-edited-file-is-shown ()
  (with-current-buffer mozadd-edited-buffer
    (string= mozadd-mirror-location mozadd-initial-mirror-location)))

(defvar mozadd-send-buffer-hook '(mozadd-isearch-send-buffer-hook-fun
                                  mozadd-re-builder-send-buffer-hook-fun)
  "Hook run before sending to the moz process.
Called by `mozadd-send-buffer-content-to-mozilla' before sending
buffer content.

Every function in the hook is called with one parameter, a symbol
whose variable value is a list.  The functions should add to this
list a record with information where they want the CSS property
outline added.  The record should have the format

  (START-TAG-END . OUTLINE-STYLE)

- START-TAG-END is the end of a start tag \(i.e. the position of
  the '>').
- OUTLINE-STYLE is the CSS style for the outline \(for example
  '1px solid red')."  )

;;(setq where-points (sort where-points '<)))))
;; (setq y nil)
;; (push 0 (symbol-value 'y))

(defun mozadd-send-buffer-content-to-mozilla ()
  "Update the remote mozrepl instance.
This runs the hook `mozadd-send-buffer-hook' before sending."
  (with-current-buffer mozadd-edited-buffer
    (if (mozadd-edited-file-is-shown)
        (mozadd-requeue-me-as-task
         (concat "content.document.body.innerHTML="
                 (json-encode
                  (save-restriction
                    (widen)
                    (let ((where-points nil)
                          (str "")
                          (p1 (point-min)))
                      ;; If nxml-where-mode is on add corresponding outline style.
                      (run-hook-with-args 'mozadd-send-buffer-hook 'where-points)
                      ;; (when (and (boundp 'nxml-where-mode) nxml-where-mode)
                      ;;   (mapc (lambda (ovl)
                      ;;           (when (overlay-get ovl 'nxml-where)
                      ;;             (when (/= ?/ (1+ (char-after (overlay-start ovl))))
                      ;;               (push (1- (overlay-end ovl)) where-points))))
                      ;;         (overlays-in (point-min) (point-max)))
                      ;;   (setq where-points (sort where-points '<)))
                      (setq where-points (sort where-points (lambda (a b)
                                                              (< (car a) (car b)))))
                      ;;(message "where-points =%S" where-points)
                      (dolist (rec where-points)
                        (let ((p2    (nth 0 rec))
                              (style (nth 1 rec)))
                          (setq str (concat str
                                            (buffer-substring-no-properties p1
                                                                            p2)))
                          (setq str (concat str " style=\"outline: " style "\""))
                          (setq p1 p2)))
                      (setq str (concat str
                                        (buffer-substring-no-properties p1
                                                                        (point-max))))
                      ;;(message "STRSTR=\n%s" str)
                      str)))
                 ";")
         'mozadd-skip-output-until-prompt)
      (mozadd-skip-current-task))
    ;; Timer to avoid looping
    (run-with-idle-timer 0 nil 'mozadd-maybe-exec-next)
    ))

(defvar mozadd-current-task nil)
(setq mozadd-current-task nil)

(defvar mozadd-task-queue nil)
(setq mozadd-task-queue nil)
;;(mozadd-add-task "content.location.href" 'mozadd-get-initial-mirror-location)
;;(mozadd-add-task "hi" 1)
;;(mozadd-add-task "hm" 2)

(defun mozadd-clear-exec-queue ()
  (setq mozadd-current-task nil)
  (setq mozadd-task-queue nil)
  (when (buffer-live-p inferior-moz-buffer)
    (with-current-buffer inferior-moz-buffer
      (dolist (fun (buffer-local-value 'comint-preoutput-filter-functions (current-buffer)))
        (remove-hook 'comint-preoutput-filter-functions fun t)))))

(defun mozadd-add-task (input task)
  (mozadd-add-task-1 (list input task)))

(defun mozadd-add-task-1 (task &optional single)
  (when single
    (setq mozadd-task-queue (delete task mozadd-task-queue)))
  (setq mozadd-task-queue (cons task mozadd-task-queue))
  (setq mozadd-task-queue (reverse mozadd-task-queue))
  ;;(message "add-task: mozadd-task-queue=%S, current=%s" mozadd-task-queue mozadd-current-task)
  (mozadd-maybe-exec-next))

(defun mozadd-maybe-exec-next ()
  ;;(message "mozadd-maybe-exec-next, current=%s" mozadd-current-task)
  (unless mozadd-current-task
    (mozadd-exec-next)))

(defun mozadd-exec-next ()
  (when mozadd-current-task
    (let* ((old-task mozadd-current-task) ;;(pop mozadd-task-queue))
           (old-filter (when (listp old-task) (nth 1 old-task))))
      (when (and old-filter (buffer-live-p inferior-moz-buffer))
        (with-current-buffer inferior-moz-buffer
          (remove-hook 'comint-preoutput-filter-functions old-filter t)))))
  (setq mozadd-current-task nil)
  (when mozadd-task-queue
    (let* ((this  (pop mozadd-task-queue))
           (input (when (listp this) (nth 0 this)))
           (task  (when (listp this) (nth 1 this))))
      (setq mozadd-current-task this)
      ;;(message "EXEC: %s" this)
      (message "EXEC task: %s" task)
      (if (not (listp this))
          (funcall this)
        (when (buffer-live-p inferior-moz-buffer)
          (with-current-buffer inferior-moz-buffer
            (add-hook 'comint-preoutput-filter-functions task nil t)))
        (message "Sending to Mozilla now (%d chars) ..." (length input))
        (comint-send-string (inferior-moz-process) input)))))

(defun mozadd-skip-current-task ()
  ;;(message "mozadd-skip-current-task")
  ;;(pop mozadd-task-queue)
  (setq mozadd-current-task nil))

(defun mozadd-requeue-me-as-task (input task)
  (mozadd-skip-current-task)
  ;;(message "mozadd-requeue-me-as-task %S %S" input task)
  (setq mozadd-task-queue (cons (list input task) mozadd-task-queue)))

(defcustom mozadd-browseable-file-extensions
  '("html" "htm" "xhtml")
  "File extensions possibly viewable in a web browser."
  :type '(repeat (string :tag "File extension (without leading dot)"))
  :group 'mozadd)

(defun mozadd-html-buffer-file-p ()
  "Return non-nil if buffer file is viewable in a web browser."
  (when (buffer-file-name)
    (member (file-name-extension (buffer-file-name))
            mozadd-browseable-file-extensions)))

(defvar mozadd-update-key [(control ?c)(control ?c)])
(defvar mozadd-submatch-key [(control ?c)(control ?0)])
(defvar mozadd-mirror-mode-map
  (let ((map (make-sparse-keymap "MozMirror")))
    (define-key map mozadd-update-key 'mozadd-update-mozilla)
    (define-key map mozadd-submatch-key 'mozadd-set-outline-regexp-submatch-num)
    (define-key map [(control ?c)(control ?a)] 'mozadd-toggle-auto-update)
    (define-key map [(control ?c)(control ?b)] 'mozadd-add-href-base)
    ;; Fix-me: menu
    map))

(defun mozadd-add-href-base (url)
  "Add <base href=... /> tag with url URL."
  (interactive "sOriginal URL: ")
  (if (zerop (length url))
             (message "No URL given")
    (let ((urlobj (url-generic-parse-url url)))
      (cond
       ((not (member (url-type urlobj) '("http" "https")))
        (message "Must be a http url"))
       ((not (url-fullness urlobj))
        (message "Must be a full url"))
       (t
        (save-excursion
          (let ((re-base (rx "<base"
                             (+ whitespace)
                             "href=\""
                             (submatch
                              (+ (not (any "\""))))
                             "\""))
                (re-head (rx "<head"
                             (* (not (any ">")))
                             ">")))
            (goto-char (point-min))
            (cond ((re-search-forward re-base nil t)
                   (delete-region (match-beginning 1) (match-end 1))
                   (goto-char (match-beginning 1))
                   (insert url)
                   (message "Change old base tag to new url"))
                  ((re-search-forward re-head nil t)
                   (insert "\n\n<base href=\"" url "\" />\n\n")
                   (message "Added base tag"))
                  (t
                   (error "Can't find <head ...>, don't know where to add <base ...> tag"))))))))))



;;;###autoload
(define-minor-mode mozadd-mirror-mode
  "Mirror content of current file buffer in Firefox.
When you turn on this mode the file you are editing will be
opened in Firefox.

\\<mozadd-mirror-mode-map>
Updating of Firefox can be made with \\[mozadd-update-mozilla].

This can be done also during `isearch-mode' and from
`re-builder'.  Tags containing matches are then shown as CSS
outlines in Firefox.  To show submatches instead use
\\[mozadd-set-outline-regexp-submatch-num].

The style for the outlines is `mozadd-matches-outline-style'.

If `nxml-where-mode' is on the marks will also be shown in
Firefox as CSS outline style.  These outlines have the style
`mozadd-xml-path-outline-style'.

If you are editing a file from a web URL you may want to add a
<base href=... /> tag to get the page looking better in Firefox.
You can add that with the command \\[mozadd-add-href-base].

When updating Firefox the hook `mozadd-send-buffer-hook' is run
first.  \(This adds the CSS outlines above.)

For the mirroring to work the edited file must be shown in
Firefox and visible.

Updating Firefox can also be done automatically.  In this case
every change you make in the buffer will trigger a redraw in
Firefox - regardless of if you save the file or not.  This is may
be slow currently.

See also `mozadd-refresh-edited-on-save-mode' which can be used
when you edit CSS files."
  :lighter " MozMirror"
  :group 'mozadd
  (if mozadd-mirror-mode
      (unless (catch 'ok
                (unless (mozadd-html-buffer-file-p)
                  (mozadd-warning "You can only mirror html file buffers")
                  (throw 'ok nil))
                (when (buffer-modified-p)
                  (mozadd-warning "Please save buffer first")
                  (throw 'ok nil))
                (let ((msg (mozadd-perhaps-start)))
                  (when msg
                    (mozadd-warning msg)
                    (throw 'ok nil)))
                (mozadd-clear-exec-queue)
                (setq mozadd-edited-buffer (current-buffer))
                (mozadd-add-task (concat "content.location.href = "
                                         "\"file:///" (buffer-file-name) "\";")
                                 'mozadd-get-initial-mirror-location)
                (add-hook 'after-change-functions 'mozadd-auto-update-mozilla t t)
                (add-hook 'nxhtml-where-hook 'mozadd-auto-update-mozilla t t)
                (add-hook 'post-command-hook 'mozadd-edited-buffer-post-command)
                ;; Fix-me: move to isearch-mode-hook
                (define-prefix-command 'mozadd-isearch-control-c-map)
                (define-key isearch-mode-map [(control ?c)] 'mozadd-isearch-control-c-map)
                (define-key isearch-mode-map mozadd-update-key 'mozadd-update-mozilla)
                (define-key isearch-mode-map mozadd-submatch-key 'mozadd-set-outline-regexp-submatch-num)
                (define-key reb-mode-map mozadd-update-key 'mozadd-update-mozilla-from-reb)
                (define-key reb-mode-map mozadd-submatch-key 'mozadd-set-outline-regexp-submatch-from-reb)
                t)
        (setq mozadd-mirror-mode nil))
    (setq mozadd-edited-buffer nil)
    (remove-hook 'post-command-hook 'mozadd-edited-buffer-post-command)
    (remove-hook 'nxhtml-where-hook 'mozadd-auto-update-mozilla t)
    (remove-hook 'after-change-functions 'mozadd-auto-update-mozilla t)
    (define-key isearch-mode-map mozadd-update-key 'isearch-other-meta-char)
    (define-key isearch-mode-map mozadd-submatch-key 'isearch-other-meta-char)
    (define-key reb-mode-map mozadd-update-key nil)
    (define-key reb-mode-map mozadd-submatch-key nil)
    ))
(put 'mozadd-mirror-mode 'permanent-local t)

(defun mozadd-edited-buffer-post-command ()
  "Check if we are in a new edited buffer."
  (when mozadd-mirror-mode
    (setq mozadd-edited-buffer (current-buffer))))


(defvar mozadd-buffer-content-to-mozilla-timer nil)

(defvar mozadd-auto-update-mirror nil)
(put 'mozadd-auto-update-mirror 'permanent-local t)
(defun mozadd-toggle-auto-update ()
  "Toggle auto update in `mozadd-mirror-mode'."
  (interactive)
  (set (make-local-variable 'mozadd-auto-update-mirror)
       (not mozadd-auto-update-mirror)))

(defun mozadd-auto-update-mozilla (&rest ignored)
  (when mozadd-auto-update-mirror
    (mozadd-update-mozilla)))
(put 'mozadd-auto-update-mozilla 'permanent-local-hook t)

(defun mozadd-update-mozilla-from-reb ()
  "Update Firefox from re-builder."
  (interactive)
  (with-current-buffer reb-target-buffer
    (mozadd-update-mozilla)))

(defun mozadd-update-mozilla ()
  "Update Firefox."
  (interactive)
  (if (not mozadd-mirror-mode)
      (message "This buffer is not mirrored in Firefox")
    (when (timerp mozadd-buffer-content-to-mozilla-timer)
      (cancel-timer mozadd-buffer-content-to-mozilla-timer))
    (setq mozadd-buffer-content-to-mozilla-timer
          (run-with-idle-timer 1 nil 'mozadd-queue-send-buffer-content-to-mozilla (current-buffer)))
    (message "Asked Firefox to update %s..." (current-time-string))))

;; (define-minor-mode mozadd-isearch-matches-mode
;;   "Show isearch matches when updating buffer in Firefox.
;; Use the style in `mozadd-matches-outline-style' for the
;; outlines."
;;   :group 'mozadd
;;   (if mozadd-isearch-matches-mode
;;       (add-hook 'mozadd-send-buffer-hook 'mozadd-isearch-send-buffer-hook-fun nil t)
;;     (remove-hook 'mozadd-send-buffer-hook 'mozadd-isearch-send-buffer-hook-fun t)))

(defcustom mozadd-matches-outline-style "1px solid red"
  "CSS style for matches outline when shown in Firefox.
This is added as

  style=\"outline: THIS-STYLE\""
  :type 'string
  :group 'mozadd)

(defcustom mozadd-xml-path-outline-style "1px solid green"
  "CSS style for `nxml-where-mode' outline when shown in Firefox.
This is added as

  style=\"outline: THIS-STYLE\""
  :type 'string
  :group 'mozadd)

(defvar mozadd-outline-regexp-submatch-num 0)
(put 'mozadd-outline-regexp-submatch-num 'permanent-local t)
(defun mozadd-set-outline-regexp-submatch-num (num)
  "Set submatch number for regexp's outlines.
Set submatch num to NUM.

This number is used when showing matches for isearch and
re-builder.  It is per buffer."
  (interactive "NMozadd mirror outline regexp submatch num in this buffer: ")
  (set (make-local-variable 'mozadd-outline-regexp-submatch-num) num))

(defun mozadd-set-outline-regexp-submatch-from-reb (num)
  "Set submatch number for regexp's outlines."
  (interactive "NMozadd mirror outline regexp submatch num for target buffer: ")
  (with-current-buffer reb-target-buffer
    (mozadd-set-outline-regexp-submatch-num num)))

(defun mozadd-isearch-send-buffer-hook-fun (mozadd-points)
  "Add outlines to tags matched by isearch when updating Firefox.
Use the style in `mozadd-matches-outline-style' for the
outlines."
  ;; isearch-lazy-highlight-overlays isearch-overlay
  (when isearch-mode
    (let ((pattern isearch-string)
          (is-regexp isearch-regexp)
          (submatch mozadd-outline-regexp-submatch-num)
          (outline-style mozadd-matches-outline-style))
      ;;(message "isearch mozadd: %s %s %s" pattern is-regexp submatch)
      (mozadd-add-matches-outlines mozadd-points pattern is-regexp submatch outline-style))))

(defun mozadd-re-builder-send-buffer-hook-fun (mozadd-points)
  "Add outlines to tags matched by re-builder when updating Firefox.
Use the style in `mozadd-matches-outline-style' for the
outlines."
  ;; isearch-lazy-highlight-overlays isearch-overlay
  (when reb-overlays
    (let ((pattern reb-regexp)
          (is-regexp t)
          (submatch mozadd-outline-regexp-submatch-num)
          (outline-style mozadd-matches-outline-style))
      ;;(message "re-builder mozadd: %s %s %s" pattern is-regexp submatch)
      (mozadd-add-matches-outlines mozadd-points pattern is-regexp submatch outline-style))))

(defun mozadd-add-matches-outlines (mozadd-points pattern is-regexp submatch outline-style)
  (let ((my-points (symbol-value mozadd-points))
        (matches nil) ;;`(,(overlay-start isearch-overlay)))
        tag-starts
        (here (point)))
    (save-match-data
      (goto-char (point-min))
      (while (if is-regexp
                 (re-search-forward pattern nil t)
               (search-forward pattern nil t))
        (let ((match-point (if is-regexp
                               (match-beginning submatch)
                             (- (point) (length pattern))
                             )))
          ;;(message "match-point=%s, submatch=%s, is-regexp=%s" match-point submatch is-regexp)
          (unless (memq match-point matches)
            (setq matches (cons match-point matches)))))
      (dolist (match matches)
        (let (tag-start
              tag-start-end)
          (goto-char match)
          (while (and (not tag-start)
                      (search-backward "<" nil t))
            (unless (eq ?/
                        (char-after (1+ (point))))
              (setq tag-start (point))))
          (when tag-start
            (when (search-forward ">" nil t)
              (setq tag-start-end (1- (point)))
              (unless (memq tag-start-end tag-starts)
                (setq tag-starts (cons tag-start-end tag-starts))))))))
    (dolist (start tag-starts)
      (let ((rec `(,start ,outline-style)))
        (setq my-points (cons rec my-points))))
    (set mozadd-points my-points)
    (goto-char here)))


(provide 'mozadd)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; mozadd.el ends here
