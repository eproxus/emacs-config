;;; Custom Keyboard Shortcuts and Functions

;;; === Custom keybindings ===================================================

;;; --- OS X special ---------------------------------------------------------

(if (eq system-type 'darwin)
    (progn
      (global-set-key [kp-delete] 'delete-char) ; Make delete key work
      (global-set-key (kbd "M-2") "@")
      (global-set-key (kbd "M-1") "`")
      (global-set-key (kbd "M-;") "\\")
      (global-set-key (kbd "M--") "|")
      (global-unset-key (kbd "C--"))   ; negative-argument
      (global-unset-key (kbd "C-M--")) ; negative-argument
))

;;; --- Keybindings using existing functions ---------------------------------

(global-set-key [f5]             'compile)
(global-set-key [f6]             'next-error)
(global-set-key [S-backspace]    'shrink-whitespaces)          ; Delete white space around cursor with Shift-BACKSPACEs
(global-set-key (kbd "C-1")      'delete-other-windows)
(global-set-key (kbd "C-2")      'split-window-vertically)
(global-set-key (kbd "C-3")      'split-window-horizontally)

(global-set-key (kbd "C-R")      'query-replace)               ; search and replace with Ctrl-R
(global-set-key (kbd "C-S-R")    'query-replace-regexp)        ; search and replace (regexp) with Ctrl-Shift-R

(global-set-key (kbd "C-Y")      'redo)                        ; 'redo', that is, revert the last 'undo'
(global-set-key (kbd "C-/")      'comment-region)              ; Comment the region with Ctrl-/
(global-set-key (kbd "C-?")      'uncomment-region)            ; Uncomment the region with Ctrl-?

(global-set-key [\C-tab]         'other-window)                ; Change window
(global-set-key (kbd "C-B")      'ido-switch-buffer)           ; Change buffer

(global-set-key (kbd "C-.")      'erl-find-source-under-point) ; Jump to function definition
(global-set-key (kbd "C-,")      'erl-find-source-unwind)      ; Jump back to where we came from

(global-set-key (kbd "C-L")      'goto-line)                   ; Jump to line

(global-set-key (kbd "C-S-W")    'undo-kill-buffer)            ; Revive killed buffer
(global-set-key (kbd "C-Q")      'save-buffers-kill-terminal)  ; Quit Emacs

(global-set-key (kbd "M-q")      'fill-paragraph)              ; Re-flow paragraph
(global-set-key (kbd "C-E")      'align)                       ; Align according to normal rules
(global-set-key (kbd "C-S-E")    'align-repeat)                ; Align regexp repeat
(global-set-key (kbd "C-U")      'iedit-mode)                  ; Interactive Edit (replace expression)
;(global-set-key (kbd "C-i")      'quoted-insert)               ; Insert any character

(global-set-key (kbd "M-a")      'smex)                        ; Super Meta-X
(global-set-key (kbd "M-A")      'smex-major-mode-commands)
(global-set-key (kbd "M-x")      'execute-extended-command)

;; Auto complete
(global-set-key (kbd "C-t")      'hippie-expand)

;; flymake
(global-set-key (kbd "C-e")      'flymake-display-err-menu-for-current-line)
(global-set-key (kbd "M-<down>") 'flymake-goto-next-error)
(global-set-key (kbd "M-<up>")   'flymake-goto-prev-error)

;; Recent files
(global-set-key (kbd "C-S-O")    'recentf-ido-find-file)

; search forward with Ctrl-F
(global-set-key (kbd "C-F") 'isearch-forward)
(define-key isearch-mode-map (kbd "C-F") (lookup-key isearch-mode-map "\C-s"))
(define-key minibuffer-local-isearch-map (kbd "C-F")
  (lookup-key minibuffer-local-isearch-map "\C-S"))

; search forward (regexp) with Ctrl-Shift-F
(global-set-key (kbd "C-S-F") 'isearch-forward-regexp)
(define-key isearch-mode-map (kbd "C-S-F") (lookup-key isearch-mode-map "\C-M-r"))
(define-key minibuffer-local-isearch-map (kbd "C-S-F")
  (lookup-key minibuffer-local-isearch-map "\C-M-R"))

;;; --- Keybindings using my custom functions ---------------------------------

(global-set-key (kbd "C-=") 'duplicate-current-line)           ; Duplicate a line with Ctrl-=
(global-set-key (kbd "C-<down>") 'move-current-line-downward)  ; Switch a line downwards with Ctrl-<down>
(global-set-key (kbd "C-<up>") 'move-current-line-upward)      ; Switch a line upwards with Ctrl-<up>

(global-set-key (kbd "S-<delete>") 'nuke-line)                 ; Delete the whole line with Shift-DEL
(global-set-key (kbd "S-<kp-delete>") 'nuke-line)              ; Delete the whole line with Shift-DEL (OS X)

(global-set-key (kbd "C-D")   'call-last-kbd-macro)            ; Run macro
(global-set-key (kbd "C-S-D") 'toggle-kbd-macro-recording-on)  ; Start/stop macro

;;; === Custom functions =====================================================

(defun duplicate-current-line ()
  "Duplicate the current line. There is a little bug: when current line is the last line of the buffer, this will not work as expected. Anyway, that's ok for me."
  (interactive)
  (let ((previous-column (current-column)))
    (let ((start (progn (beginning-of-line) (point)))
          (end (progn (next-line 1) (beginning-of-line) (point))))
      (insert-buffer-substring (current-buffer) start end)
      (forward-line -1))
    (move-to-column previous-column)))

(defun move-current-line-downward ()
  "Move current line downward once."
  (interactive)
  (let ((previous-column (current-column)))
    (forward-line)
    (transpose-lines 1)
    (forward-line -1)
    (move-to-column previous-column)))

(defun move-current-line-upward ()
  "Move current line upward once."
  (interactive)
  (let ((previous-column (current-column)))
    (transpose-lines 1)
    (forward-line -2)
    (move-to-column previous-column)))

(defun nuke-line ()
  (interactive)
  (let ((previous-column (current-column)))
    (kill-whole-line 1)
    (move-to-column previous-column)))

(defun at-character ()
  (interactive)
  (insert-char 64 1))

(defun align-repeat (start end regexp)
  "Repeat alignment with respect to
     the given regular expression."
  (interactive "r\nsAlign regexp: ")
  (align-regexp start end
                (concat "\\(\\s-*\\)" regexp) 1 1 t))

(defun recentf-ido-find-file ()
  "Find a recent file using Ido."
  (interactive)
  (let* ((file-assoc-list
        (mapcar (lambda (x)
            (cons (file-name-nondirectory x)
              x))
          recentf-list))
     (filename-list
      (remove-duplicates (mapcar #'car file-assoc-list)
                 :test #'string=))
     (filename (ido-completing-read "Find recent file: "
                    filename-list
                    nil
                    t)))
    (when filename
      (find-file (cdr (assoc filename
                 file-assoc-list))))))

(defun undo-kill-buffer (arg)
  "Re-open the last buffer killed.  With ARG, re-open the nth buffer."
  (interactive "p")
  (let ((recently-killed-list (copy-sequence recentf-list))
     (buffer-files-list
      (delq nil (mapcar (lambda (buf)
                  (when (buffer-file-name buf)
                (expand-file-name (buffer-file-name buf)))) (buffer-list)))))
    (mapc
     (lambda (buf-file)
       (setq recently-killed-list
         (delq buf-file recently-killed-list)))
     buffer-files-list)
    (find-file
     (if arg (nth arg recently-killed-list)
       (car recently-killed-list)))))

(defun toggle-kbd-macro-recording-on ()
  "One-key keyboard macros: turn recording on."
  (interactive)
  (define-key global-map (this-command-keys)
   'toggle-kbd-macro-recording-off)
  (start-kbd-macro nil))

(defun toggle-kbd-macro-recording-off ()
  "One-key keyboard macros: turn recording off."
  (interactive)
  (define-key global-map (this-command-keys)
    'toggle-kbd-macro-recording-on)
  (end-kbd-macro))