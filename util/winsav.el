;;; winsav.el --- Save and restore window structure
;;
;; Author: Lennart Borgman
;; Created: Sun Jan 14 2007
;; Version: 0.72
;; Last-Updated: 2009-05-01 Fri
;; Keywords:
;; Compatibility:
;;
;; Features that might be required by this library:
;;
  ;; `cl', `desktop'.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; This library was orignally written to solve the problem of adding a
;; window to the left of some windows in a frame
;;
;; ___________
;; |    |    |
;; | 1  | 2  |
;; |____|____|
;; |         |
;; |    3    |
;; |_________|
;;
;; so that the window structure on the frame becomes
;;
;; ___________
;; |  |  |   |
;; |  | 1| 2 |
;; | B|__|___|
;; | A|      |
;; | R|  3   |
;; |__|______|
;;
;;
;; This problem can be solved by this library.  However the solution in
;; this library is a bit more general: You first copy the window
;; structure and then restore that into another window.  To do the
;; above you first copy the window structure in the first frame above
;; with `winsav-get-window-tree'.  Then you create windows like this:
;;
;; ___________
;; |  |      |
;; |  |      |
;; | B|      |
;; | A|      |
;; | R|      |
;; |__|______|
;;
;;
;; Finally you use `winsav-put-window-tree' to put the window
;; structure into the right window.  (Of course you could have put BAR
;; above, under etc.)
;;
;;
;; You can use this library to restore frame configuration when you
;; start Emacs.  To do that you can put this at the end of your
;; .emacs:
;;
;;   (desktop-save-mode 1)
;;   (winsav-save-mode 1)
;;
;; You can also use this to save configurations that you later switch
;; between.  See the functions
;;
;;   `winsav-save-named-config', `winsav-change-config'
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Bugs and limitations:
;;
;; Juanma Barranquero has pointed out there is a serious limitation in
;; this way of doing it when overlays with 'window properties are
;; used.  The problem is that any pointers to windows are made invalid
;; since they are deleted.  So in fact any code that relies on saved
;; pointers to windows will have problem if the window is one of those
;; that are involved here.
;;
;; To overcome this problem when doing something like inserting a BAR
;; window (see above) a new window has to be inserted in the existing
;; window tree on a frame in a way that is currently not supported in
;; Emacs.
;;
;; It would be nice to be have primitives to manipulate the window
;; tree more generally from elisp.  That requires implementation of
;; them at the C level of course.
;;
;; However it is probably much easier to implement it quite a bit less
;; general.  The concept of splitting is maybe then the right level to
;; search for primitives at.
;;
;; My conclusion is that it will take some time to find suitable
;; primitives for this.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change log:
;;
;; Version 0.72:
;;
;; - Format of window structure changed in Emacs 23. Adopted to that.
;; - Added save and restore of frame/window configurations between
;;   Emacs sessions.
;;
;; Version 0.71:
;;
;; - Added rotation of window structure.
;;
;; Version 0.70:
;;
;; - Support for save and restore from file.
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:


(eval-when-compile (require 'cl))

(defun winsav-upper-left-window(&optional frame w)
  (let* ((tree (if w w (car (window-tree frame))))
         (is-split (not (windowp tree))))
    (if (not is-split)
        tree
      (winsav-upper-left-window frame (nth 2 tree)))))


(defvar winsav-after-get-hooks nil
  "Hook to run after at the end of `winsav-get-window-tree'.
The functions in this hook are called with one parameter which is
the same as the return value from the function above.")

(defvar winsav-after-put-hooks nil
  "Hook to run after at the end of `winsav-put-window-tree'.
The functions in this hook are called with one parameter which is
a list where each element is a list \(old-win new-win) where
OLD-WIN are the window from `winsav-get-window-tree' and NEW-WIN
is the newly created corresponding window.  This list is the same
as the return value from the function above.")

(defun winsav-get-window-tree(&optional frame)
  "Get window structure.
This returns an object with current windows with values, buffers,
points and the selected window.

FRAME is the frame to save structure from. If nil use selected.

At the very end of this function the hook `winsav-after-get' is
run."
  (let* ((upper-left (winsav-upper-left-window frame))
         (num -1)
         sel-num)
    (dolist (w (window-list frame nil upper-left))
      (setq num (1+ num))
      (when (eq w (selected-window))
        (setq sel-num num)))
    (let ((ret (list sel-num
                     (winsav-get-window-tree-1 frame nil))))
      (run-hook-with-args 'winsav-after-get-hooks ret)
      ret)))

(defun winsav-get-window-tree-1(frame w)
  (let ((tree (if w w (car (window-tree frame)))))
    (if (windowp tree)
        ;; Fix-me: Do not mix buffer and windows this way. Store
        ;; buffers first, separetly. Use desktop format for that - but
        ;; save pointers to the buffers!!!
        ;;
        ;; Or ..., just let desktop do that job and find the buffers
        ;; through the buffer name + file name ...
        (with-current-buffer (window-buffer tree)
          (list (window-buffer tree)
                ;; buffer
                (buffer-name)
                (buffer-file-name)
                ;;buffer-read-only
                ;;(if mumamo-multi-major-mode mumamo-multi-major-mode major-mode)
                ;;minor-modes
                ;;buffer locals
                ;;(cons (+ 0 (mark-marker)) (mark-active))
                ;; window
                (window-point tree)
                (window-edges tree)
                (window-scroll-bars tree)
                (window-fringes tree)
                (window-margins tree)
                (window-hscroll tree)
                ;; misc
                (window-dedicated-p tree)
                (window-redisplay-end-trigger tree)
                (window-start tree)
                tree))
      (let* ((dir (nth 0 tree))
             (split (nth 1 tree))
             (wt (cddr tree))
             (wsubs (mapcar (lambda(wc)
                              (winsav-get-window-tree-1 nil wc))
                            wt)))
        (append (list dir split) wsubs)))))

;;;###autoload
(defun winsav-put-window-tree (saved-tree window &optional copy-win-ovl win-ovl-all-bufs)
  "Put window structure SAVED-TREE into WINDOW.
Restore a structure SAVED-TREE returned from
`winsav-get-window-tree' into window WINDOW.

If COPY-WIN-OVL is non-nil then overlays having a 'window
property pointing to one of the windows in SAVED-TREE where this
window still is shown will be copied to a new overlay with
'window property pointing to the corresponding new window.

If WIN-OVL-ALL-BUFS is non-nil then all buffers will be searched
for overlays with a 'window property of the kind above.

At the very end of this function the hook `winsav-after-put' is
run."
  (let* ((sel-num (nth 0 saved-tree))
         (tree    (nth 1 saved-tree))
         nsiz
         nh
         nw
         osiz
         oh
         ow
         scale-w
         scale-h
         first-win
         winsav-put-return)
    (unless (or (bufferp (car tree))
                (eq 'buffer (car tree)))
      (setq nsiz (window-edges window))
      (setq nh (- (nth 3 nsiz) (nth 1 nsiz)))
      (setq nw (- (nth 2 nsiz) (nth 0 nsiz)))
      (setq osiz (cadr tree))
      (setq oh (- (nth 3 osiz) (nth 1 osiz)))
      (setq ow (- (nth 2 osiz) (nth 0 osiz)))
      (setq scale-w (unless (= ow nw) (/ nw (float ow))))
      (setq scale-h (unless (= oh nh) (/ nh (float oh)))))
    (setq first-win (winsav-put-window-tree-1 tree window scale-w scale-h t 1))
    (select-window first-win)
    (when sel-num (other-window sel-num))
    (winsav-fix-win-ovl winsav-put-return copy-win-ovl win-ovl-all-bufs)
    (run-hook-with-args 'winsav-after-put-hooks winsav-put-return)
    winsav-put-return))

(defun winsav-put-window-tree-1 (saved-tree window scale-w scale-h first-call level)
  (if (or (bufferp (car saved-tree))
          ;;(not (car saved-tree))
          (eq 'buffer (car saved-tree))
          )
      (let ((buffer  (nth 0 saved-tree))
            ;; buffer
            (bufnam  (nth 1 saved-tree))
            (filnam  (nth 2 saved-tree))
            ;;(mark    (nth 3 saved-tree))
            ;; window
            (point   (nth 3 saved-tree))
            (edges   (nth 4 saved-tree))
            (scroll  (nth 5 saved-tree))
            (fringe  (nth 6 saved-tree))
            (margs   (nth 7 saved-tree))
            (hscroll (nth 8 saved-tree))
            (dedic   (nth 9 saved-tree))
            (trigger (nth 10 saved-tree))
            (start   (nth 11 saved-tree))
            (ovlwin  (nth 12 saved-tree))
            scr2
            (misbuf  " *Winsav information: Buffer is gone*"))
        (or (windowp ovlwin)
            (not ovlwin)
          (error "Parameter mismatch, ovlwin not window: %s" ovlwin))
        (when first-call
          (add-to-list 'winsav-put-return (list ovlwin window))
          (when (eq 'buffer buffer)
            (when filnam
              (setq buffer (winsav-find-file-noselect filnam)))
            (if (buffer-live-p buffer)
                (unless (string= bufnam (buffer-name buffer))
                  (rename-buffer bufnam))
              (setq buffer (get-buffer-create bufnam))))
          (set-window-buffer window buffer)
          (set-window-dedicated-p window dedic)
          ;; Strange incompatibility in scroll args:
          (setq scr2 (list (nth 0 scroll) (nth 2 scroll) (nth 3 scroll)))
          (apply 'set-window-scroll-bars (append (list window) scr2))
          (apply 'set-window-fringes (append (list window) fringe))
          (set-window-margins window (car margs) (cdr margs))
          (set-window-hscroll window hscroll)
          (set-window-redisplay-end-trigger window trigger))
        (let* ((nsiz (window-edges window))
               (nh (- (nth 3 nsiz) (nth 1 nsiz)))
               (nw (- (nth 2 nsiz) (nth 0 nsiz)))
               (osiz edges) ;(nth 2 saved-tree))
               (oh (- (nth 3 osiz) (nth 1 osiz)))
               (ow (- (nth 2 osiz) (nth 0 osiz)))
               (diff-w (- (if scale-w
                              (round (* scale-w ow))
                            ow)
                          nw))
               (diff-h (- (if scale-h
                              (round (* scale-h oh))
                            oh)
                          nh)))
          ;; Avoid rounding naggings:
          (when (> (abs diff-h) 1)
            (bw-adjust-window window diff-h nil))
          (when (> (abs diff-w) 1)
            (bw-adjust-window window diff-w t)))
        ;; Fix-me: there were some problems getting point correctly. Don't know why...
        (with-selected-window window
          (with-current-buffer (window-buffer window)
            (goto-char point))
          (set-window-point window point)
          ;;(unless (buffer-live-p buffer) (setq point 1) (setq start 1))
          (set-window-start window start)
          ;; Maybe point got off screen?
          (when (/= point (window-point window))
            (set-window-point window point)))
        window)
    (let* ((ver (car saved-tree))
           (wtree (list (cons window (caddr saved-tree))))
           (nwin window)
           pwin
           pdelta
           (first-win nwin))
      ;; First split to get it in correct order
      (when first-call
        (dolist (subtree (cdddr saved-tree))
          (setq pwin nwin)
          (message "nwin edges=%s, ver=%s" (window-edges nwin) ver)
          (let ((split-err nil)
                (window-min-height 1)
                (window-min-width 1))
            (setq nwin (split-window nwin nil (not ver))))
          ;; Make the previous window as small as permitted to allow
          ;; splitting as many times as possible
          (setq pdelta (-
                        (if ver
                            window-min-height
                          window-min-width)
                        (if ver
                            (window-width pwin)
                          (window-height pwin))))
          ;;(message "pwin=%s, edges=%s, pdelta=%s, ver=%s" pwin (window-edges pwin) pdelta ver)
          ;; No reason to fail here:
          (condition-case err
              (adjust-window-trailing-edge pwin pdelta (not ver))
            (error
             ;;(message "awt=>%s" (error-message-string err))
             nil
             ))
          ;; Add to traverse
          (add-to-list 'wtree
                       (cons nwin subtree)
                       t)))
      ;; Now traverse. Sizing is a bit tricky, multiple runs have to
      ;; be done (as in balance-windows).
      (let (tried-sizes
            last-sizes
            (windows (window-list (selected-frame))))
        (while (not (member last-sizes tried-sizes))
          (when last-sizes (setq tried-sizes (cons last-sizes tried-sizes)))
          (setq last-sizes (mapcar (lambda (w)
                                     (window-edges w))
                                   windows))
          (dolist (wsub (reverse wtree))
            (select-window (car wsub))
            (winsav-put-window-tree-1 (cdr wsub) (selected-window)
                                      scale-w scale-h
                                      first-call
                                      (1+ level)
                                      ))
          (setq first-call nil)
          ))
      first-win)))

(defun winsav-fix-win-ovl(win-list copy-win-ovl win-ovl-all-bufs)
  (let ((oldwins (mapcar (lambda(elt)
                           (car elt))
                         win-list))
        ovlwin
        window)
    (let (buffers)
      (if win-ovl-all-bufs
          (setq buffers (buffer-list))
        (mapc (lambda(w)
                (when (window-live-p w)
                  (add-to-list 'buffers (window-buffer w))))
              oldwins))
      (dolist (buf buffers)
        (with-current-buffer buf
          (save-restriction
            (widen)
            (dolist (overlay (overlays-in (point-min) (point-max)))
              (when (setq ovlwin (car (memq (overlay-get overlay 'window) oldwins)))
                (setq window (cadr (assoc ovlwin win-list)))
                ;; If the old window is still alive then maybe copy
                ;; overlay, otherwise change the 'window prop. However
                ;; copy only if COPY-WIN-OVL is non-nil.
                (if (not (and (window-live-p ovlwin)
                              (window-frame ovlwin)))
                    (overlay-put overlay 'window window)
                  (when copy-win-ovl
                    (let* ((props (overlay-properties overlay))
                           (start (overlay-start overlay))
                           (end   (overlay-end   overlay))
                           ;; Fix-me: start and end marker props
                           (newovl (make-overlay start end)))
                      (while props
                        (let ((key (car props))
                              (val (cadr props)))
                          (setq props (cddr props))
                          (when (eq key 'window)
                            (setq val window))
                          (overlay-put newovl key val))))))))))))))


;;; Window rotating and mirroring

(defun winsav-rotate (mirror transpose)
  "Rotate window configuration on selected frame.
MIRROR should be either 'mirror-left-right, 'mirror-top-bottom or
nil.  In the first case the window configuration is mirrored
vertically and in the second case horizontally.  If MIRROR is nil
the configuration is not mirrored.

If TRANSPOSE is non-nil then the window structure is transposed
along the diagonal from top left to bottom right (in analogy with
matrix transosition).

If called interactively MIRROR will is 'mirror-left-right by
default, but 'mirror-top-bottom if called with prefix.  TRANSPOSE
is t. This mean that the window configuration will be turned one
quarter clockwise (or counter clockwise with prefix)."
  (interactive (list
                (if current-prefix-arg
                    'mirror-left-right
                  'mirror-top-bottom)
                t))
  (let* ((wintree (winsav-get-window-tree))
         (tree (cadr wintree))
         (win-config (current-window-configuration)))
    ;;(winsav-log "old-wintree" wintree)
    (winsav-transform-1 tree mirror transpose)
    ;;(winsav-log "new-wintree" wintree)
    ;;
    ;; Fix-me: Stay in corresponding window. How?
    (delete-other-windows)
    (condition-case err
        (winsav-put-window-tree wintree (selected-window))
      (error
       (set-window-configuration win-config)
       (message "Can't rotate: %s" (error-message-string err))))
    ))

(defun winsav-transform-edges (edges)
  (let ((le (nth 0 edges))
        (te (nth 1 edges))
        (re (nth 2 edges))
        (be (nth 3 edges)))
    (list te le be re)))

(defun winsav-transform-1 (tree mirror transpose)
  (let* ((vertical (nth 0 tree))
         (edges    (nth 1 tree))
         (subtrees (nthcdr 2 tree))
         )
    ;;(winsav-log "tree 1" tree)
    (when transpose
      (cond
       ((eq vertical nil)
        (setcar tree t))
       ((eq vertical t)
        (setcar tree nil))
       (t
        (error "Uh? vertical=%S" vertical))))
    (setcar (nthcdr 1 tree) (winsav-transform-edges edges))
    (dolist (subtree subtrees)
      (if (bufferp (car subtree))
          (when transpose
            (let ((edges    (nth 4 subtree)))
              ;;(winsav-log "subtree 1" subtree)
              (setcar (nthcdr 4 subtree) (winsav-transform-edges edges))
              ;;(winsav-log "subtree 2" subtree)
              ))
        (winsav-transform-1 subtree mirror transpose)))
    (when (case mirror
            ('mirror-top-bottom vertical)
            ('mirror-left-right (not vertical))
            (nil) ;; Don't mirror
            (t
             (error "Uh? mirror=%s" mirror)))
      (setcdr (nthcdr 1 tree) (reverse subtrees))
      )
    ))

;;(global-set-key [f11] 'winsav-rotate)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; winsav+.el ends here

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;; TEST ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (defun winsav-log-buffer ()
;;   (get-buffer-create "winsav log buffer"))

;; (defun winsav-log (mark obj)
;;   (with-current-buffer (winsav-log-buffer)
;;     (insert "=== " mark "===\n" (pp-to-string obj))))

;; (global-set-key [f2] 'winsav-test-get)
;; (global-set-key [f3] 'winsav-test-put)
;; (defvar winsav-saved-window-tree nil)

;; (defun winsav-test-get()
;;   (interactive)
;;   (setq winsav-saved-window-tree (winsav-get-window-tree)))

;; (defun winsav-test-put()
;;   (interactive)
;;   (let ((ret (winsav-put-window-tree winsav-saved-window-tree
;;                                      (selected-window))))
;;     ;;(message "ret=%s" ret)
;;     ))

(defun winsav-serialize (obj)
  (prin1-to-string obj))

(defun winsav-de-serialize-window-tree (str)
  (save-match-data
    (let ((read-str
           (replace-regexp-in-string (rx "#<buffer "
                                         (1+ (not (any ">")))
                                         ">")
                                     "buffer"
                                     str))
          obj-last
          obj
          last)
      (setq read-str
            (replace-regexp-in-string (rx "#<window "
                                          (1+ (not (any ">")))
                                          ">")
                                      "nil"
                                      read-str))
      (setq obj-last (read-from-string read-str))
      (setq obj (car obj-last))
      (setq last (cdr obj-last))
      ;; Fix me, maby check there are only spaces left (or trim them above...)
      obj)))

(defun winsav-serialize-to-file (obj file)
  (with-current-buffer (find-file-noselect file)
    ;;(erase-buffer)
    (save-restriction
      (widen)
      (goto-char (point-max))
      (insert (winsav-serialize obj)
              "\n"))
    ;;(basic-save-buffer)
    ))

(defvar winsave-serialize-window-tree-hook nil
  "Called after saving a frame.
The functions in this hook should take two parameters, the frame
and the buffer to write information too.  The information should
be written as elisp code to exectute after restoring the frame.
When loading the file the symbol `winsav-last-loaded-frame' will
be the restored frame.")

(defun winsav-serialize-window-tree-to-file (frame file)
  (with-current-buffer (find-file-noselect file)
    (save-restriction
      (widen)
      (goto-char (point-max))
      (let ((start nil)
            (end nil)
            (obj (winsav-get-window-tree frame))
            (frm-par (frame-parameters frame))
            )
        (setq frm-par
              (delq nil
                    (mapcar (lambda (elt)
                              (cond
                               ((memq (car elt)
                                      '(
                                        ;;explicit-name
                                        ;;name
                                        ;;parent-id
                                        ;;title
                                        alpha
                                        auto-lower
                                        auto-raise
                                        background-color
                                        background-mode
                                        border-color
                                        border-width
                                        buffer-predicate
                                        cursor-color
                                        cursor-type
                                        font
                                        font-backend
                                        foreground-color
                                        fullscreen
                                        icon-name
                                        icon-type
                                        icon-left
                                        icon-top
                                        internal-border-width
                                        left-fringe
                                        line-spacing
                                        menu-bar-lines
                                        modeline
                                        mouse-color
                                        right-fringe
                                        screen-gamma
                                        scroll-bar-width
                                        tool-bar-lines
                                        top left width height
                                        tty-color-mode ;; ??
                                        unsplittable
                                        user-position
                                        user-size
                                        vertical-scroll-bars
                                        visibility
                                        ))
                                elt)
                               ((eq (car elt) 'minibuffer)
                                (let ((val (cdr elt)))
                                  ;;(message "elt=%s, val=%s, frame=%s same=%s" elt val frame (and (windowp val) (eq (window-frame val) frame)))
                                  (if (not (windowp val))
                                      elt
                                    (if (eq (window-frame val) frame)
                                        nil
                                      (cons 'minibuffer nil)))))
                               ))
                            frm-par)))
        (insert "(let* ((winsav-last-loaded-frame (make-frame '"
                (winsav-serialize frm-par)
                "))\n"
                "       (win (frame-first-window winsav-last-loaded-frame)))\n"
                "  (winsav-put-window-tree\n"
                "  '")
        (setq start (point))
        (insert (winsav-serialize obj) "\n")
        (setq end (copy-marker (point) t))
        (replace-regexp (rx "#<buffer "
                            (1+ (not (any ">")))
                            ">")
                        "buffer"
                        nil start end)
        (replace-regexp (rx "#<window "
                            (1+ (not (any ">")))
                            ">")
                        "nil"
                        nil start end)
        (goto-char end)
        (insert "    win)\n\n")
        (dolist (fun winsave-serialize-window-tree-hook)
          (funcall fun frame (current-buffer)))
        (insert "  )\n\n\n")
        ))))

(defun winsav-de-serialize-window-tree-from-file (file)
  (with-current-buffer (find-file-noselect file)
    (save-restriction
      (widen)
      (let ((start (point))
            (end nil))
        (forward-list)
        (setq end (point))
        ;;(goto-char (point-min))
        (winsav-de-serialize-window-tree (buffer-substring-no-properties start end))))))

(defun winsav-save-frame-to-file (file frame)
  (winsav-serialize-window-tree-to-file
   frame
   file))

(defun winsav-restore-from-file (file)
  (winsav-put-window-tree
   (winsav-de-serialize-window-tree-from-file file)
   (selected-window)))

(defun winsav-find-file-noselect (filename)
  (let ((buf (find-file-noselect filename)))
    (unless buf
      (setq buf (generate-new-buffer filename))
      (with-current-buffer buf
        (insert "Winsav could not find the file " filename)
        (set-buffer-modified-p nil)))
    buf))

(defun winsav-full-file-name (&optional dirname)
  "Return the full name of the winsav file in DIRNAME.
DIRNAME omitted or nil means use `desktop-dirname'."
  (expand-file-name winsav-base-file-name (or dirname
                                              ;;desktop-dirname
                                              "~/"
                                              )))

(defun winsav-save-frames (&optional dirname)
  ;;(interactive)
  (let ((file (winsav-full-file-name dirname)))
    (with-current-buffer (find-file-noselect file)
      (erase-buffer)
      (dolist (frm (frame-list))
        (winsav-save-frame-to-file file frm))
      (save-buffer 0) ;; No backups
      )))

;;(winsav-tell-restored-1 "hi")
(defun winsav-tell-restored-1 (dirname)
  (momentary-string-display
   (propertize
    (format "\n\n\n  Changed to winsav config '%s'\n\n\n" dirname)
    'face 'secondary-selection)
   (window-start)
   ;;nil "Type RET when done reading"
   ))

(defun winsav-tell-restored (dirname)
  (run-with-idle-timer 1 nil 'winsav-tell-restored-1 dirname))

;;(winsav-restore-frames)
;;;###autoload
(defun winsav-restore-frames (&optional dirname)
  ;;(interactive)
  (let ((old-frames (frame-list)))
    (load (winsav-full-file-name dirname))
    (dolist (old old-frames)
      (delete-frame old))
    (winsav-maximize-all-nearly-max-frames)
    (winsav-tell-restored (file-name-nondirectory
                           (directory-file-name dirname)))
    t))

(defun winsav-nearly-maximized (frame)
  (let* ((top (frame-parameter frame 'top))
         (left (frame-parameter frame 'left))
         (width (frame-pixel-width frame))
         (height (frame-pixel-height frame))
         (display-width (display-pixel-width))
         (display-height (display-pixel-height))
         (char-height (frame-char-height frame))
         (height-diff (- display-height height))
         (terminal-type (framep frame))
         )
    ;;(message "w=%s/%s, h=%s/%s, ch=%s, hd=%s" width display-width height display-height char-height height-diff)
    (cond
     ((eq 'w32 terminal-type)
      (and (equal top '(+ -4))
           (equal left '(+ -4))
           (= width display-width)
           (< height-diff (* 4 char-height))
           )))))

(defun winsav-maximize-nearly-maximized (frame)
  (when (winsav-nearly-maximized frame)
    (let ((terminal-type (framep frame)))
      (cond
       ((eq 'w32 terminal-type)
        (message "max %s" frame)
        ;;(select-frame-set-input-focus frame)
        (select-frame frame)
        (w32-send-sys-command 61488))))))

;;(winsav-maximize-all-nearly-max-frames)
(defun winsav-maximize-all-nearly-max-frames ()
  (let ((sel-frm (selected-frame)))
    (dolist (frame (frame-list))
      (winsav-maximize-nearly-maximized frame))
    (run-with-idle-timer 0 nil 'select-frame-set-input-focus sel-frm)))

(defgroup winsav nil
  "Save frames and windows when you exit Emacs."
  :group 'frames)

;;;###autoload
(define-minor-mode winsav-save-mode
  "Toggle winsav saving mode.
With numeric ARG, turn winsav saving on if ARG is positive, off
otherwise.  If winsav saving is turned on, the frames and windows
are saved from one session to another.  See variable
`desktop-save' and function `desktop-read' for details."
  :global t
  :group 'winsav)

(defun winsav-save-mode-off ()
  "Disable `winsav-save-mode'.  Provided for use in hooks."
  (winsav-save-mode 0))

;; Put us before desktop to allow killing non-visible buffers before exit
(require 'desktop)
(add-hook 'kill-emacs-hook 'winsav-kill)

(defun winsav-kill ()
  (when winsav-save-mode
    (winsav-save-frames)
    ))

;; Run after desktop at startup
(defun winsav-after-init ()
  (when winsav-save-mode
    (run-with-idle-timer 0.1 nil 'winsav-restore-frames)))

(add-hook 'after-init-hook 'winsav-after-init t)

(defcustom winsav-base-file-name
  (convert-standard-filename ".emacs.winsav")
  "Name of file for Emacs winsav, excluding the directory part."
  :type 'file
  :group 'winsav)

;;; Switching configurations

;; fix-me: defadvice this to keep track of opened files
;; (defun desktop-restore-file-buffer (desktop-buffer-file-name
;;                                     desktop-buffer-name
;;                                     desktop-buffer-misc)

(defvar winsav-desktop-restored-buffers nil)

(defadvice desktop-restore-file-buffer (after
                                        winsav-advice-desktop-restore-file-buffer
                                        disabled
                                        compile
                                        )
  (setq winsav-desktop-restored-buffers
        (cons ad-return-value winsav-desktop-restored-buffers))
  )

;; (winsav-save-named-config "testing")
;; (winsav-save-named-config "testing2")
(defun winsav-save-named-config (name)
  (interactive "swinsav - Save current frame and buffer config under name: ")
  (let* ((conf-dir (winsav-full-config-dir-name name))
         desktop-dirname ;; We do not want to change it
        )
    (mkdir conf-dir t)
    (desktop-save conf-dir t) ;; Do not lock the file
    (winsav-save-frames conf-dir)
  ))

;; (winsav-restore-named-config "testing")
(defun winsav-restore-named-config (name)
  (let* ((conf-dir (winsav-full-config-dir-name name))
         desktop-dirname ;; We do not want to change it
         desktop-file-modtime
         )
    ;; Let desktop read the files and record them
    (ad-enable-advise 'desktop-restore-file-buffer 'after 'winsav-advice-desktop-restore-file-buffer)
    (ad-activate 'desktop-restore-file-buffer)
    (desktop-read conf-dir)
    (desktop-release-lock conf-dir)
    (ad-disable-advise 'desktop-restore-file-buffer 'after 'winsav-advice-desktop-restore-file-buffer)
    (ad-deactivate 'desktop-restore-file-buffer)
    ;; Delete file buffers that we do not use now.
    (winsav-restore-frames conf-dir)))

(defun winsav-full-config-dir-name (name)
  (let* ((base-dir (concat (winsav-full-file-name) ".d"))
         (conf-dir (expand-file-name name base-dir)))
    (setq conf-dir (file-name-as-directory conf-dir))
    (message "conf-dir=%s" conf-dir)
    conf-dir))

(defun winsav-change-config ()
  (interactive)
  (let* ((base-dir (concat (winsav-full-file-name) ".d"))
         hist
         (dirs (directory-files base-dir t))
         config)
    (setq dirs (mapcar (lambda (f)
                         (when (file-directory-p f)
                           (let ((name (file-name-nondirectory f)))
                             (unless (member name '("." ".."))
                               name))))
                       (directory-files base-dir t)))
    (setq dirs (delq nil dirs))
    (setq hist dirs)
    (setq config (completing-read "winsav - Choose configuration (default startup config): "
                                  dirs nil t nil 'hist))
    (if (string= "" config)
        (progn
          (desktop-read)
          (winsav-after-init)
          )
      (winsav-restore-named-config config)
      )))

(provide 'winsav)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; winsav.el ends here
