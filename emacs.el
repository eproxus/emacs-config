(let ((default-directory "~/.emacs.d/plugins"))
  (normal-top-level-add-subdirs-to-load-path))
(let ((default-directory "~/.emacs.d/modes"))
  (normal-top-level-add-subdirs-to-load-path))
(let ((default-directory "~/.emacs.d"))
  (normal-top-level-add-to-load-path '("themes")))

;; Make Command become Ctrl for Emacs on OS X.
(setq mac-command-modifier 'ctrl)

;; Stop annoying sounds
(setq ring-bell-function 'ignore)

(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(ac-auto-start 4)
 '(ac-dwim t)
 '(ac-modes (quote (emacs-lisp-mode lisp-interaction-mode c-mode cc-mode c++-mode java-mode perl-mode cperl-mode python-mode ruby-mode ecmascript-mode javascript-mode js2-mode php-mode css-mode makefile-mode sh-mode fortran-mode f90-mode ada-mode xml-mode sgml-mode erlang-mode)))
 '(auto-save-file-name-transforms (quote ((".*" "~/.emacs.d/autosaves/\\1" t))))
 '(backup-by-copying t)
 '(backup-directory-alist (quote ((".*" . "~/.emacs.d/backups"))))
 '(c-basic-offset tab-width)
 '(case-fold-search t)
 '(color-theme-load-all-themes nil)
 '(column-number-mode t)
 '(compilation-auto-jump-to-first-error nil)
 '(compilation-context-lines 5)
 '(compilation-error-regexp-alist (mapcar (quote car) compilation-error-regexp-alist-alist))
 '(compilation-read-command nil)
 '(compilation-scroll-output t)
 '(compilation-window-height 10)
 '(compile-command "cd .. && make run_test")
 '(cperl-indent-level tab-width)
 '(cua-mode t nil (cua-base))
 '(current-language-environment "Latin-1")
 '(default-input-method "latin-1-prefix")
 '(delete-old-versions t)
 '(delete-selection-mode nil nil (delsel))
 '(flymake-no-changes-timeout 0.1)
 '(global-font-lock-mode t nil (font-lock))
 '(global-hl-line-mode t)
 '(hippie-expand-try-functions-list (quote (yas/hippie-try-expand try-expand-dabbrev try-expand-dabbrev-all-buffers try-expand-all-abbrevs try-expand-list try-expand-line try-expand-dabbrev-from-kill try-complete-file-name-partially try-complete-file-name)))
 '(hscroll-margin 0)
 '(ido-enable-dot-prefix t)
 '(ido-enable-flex-matching t)
 '(ido-everywhere t)
 '(ido-ignore-files (quote ("\\`CVS/" "\\`#" "\\`.#" "\\`\\.\\./" "\\`\\./")))
 '(indent-tabs-mode nil)
 '(inhibit-startup-screen t)
 '(ispell-program-name "aspell")
 '(mouse-wheel-mode t)
 '(mouse-wheel-progressive-speed nil)
 '(mouse-wheel-scroll-amount (quote (3 ((shift) . 8) ((control)))))
 '(p4-user-email "adam.lindberg@erlang-solutions.com")
 '(p4-verbose nil)
 '(pc-selection-mode t)
 '(recentf-arrange-rules (quote (("Erlang files (%d)" ".\\.erl\\'") ("Elisp files (%d)" ".\\.el\\'") ("Java files (%d)" ".\\.java\\'") ("C/C++ files (%d)" "c\\(pp\\)?\\'"))))
 '(recentf-exclude (quote ("whsettings" "\\.ido\\.last")))
 '(recentf-mode t)
 '(refactorerl-base-path "~/Applications/refactorerl")
 '(safe-local-variable-values (quote ((erlang-indent-level . 4))))
 '(scalable-fonts-allowed t)
 '(scroll-bar-mode (quote right))
 '(scroll-down-aggressively 0.01)
 '(scroll-margin 0)
 '(scroll-up-aggressively 0.01)
 '(show-paren-mode t)
 '(show-paren-style (quote expression))
 '(show-trailing-whitespace t)
 '(tab-stop-list (quote (4 8 12 16 20 24 28 32 36 40 44 48 52 56 60 64 68 72 76 80 84 88 92 96 100 104 108 112 116 120)))
 '(tab-width 4)
 '(tabbar-cycling-scope (quote tabs))
 '(tool-bar-mode nil nil (tool-bar))
 '(truncate-lines t)
 '(vc-follow-symlinks nil)
 '(visible-bell nil)
 '(whitespace-modes (quote (ada-mode asm-mode autoconf-mode awk-mode c-mode c++-mode cc-mode change-log-mode cperl-mode electric-nroff-mode emacs-lisp-mode f90-mode fortran-mode html-mode html3-mode java-mode jde-mode ksh-mode latex-mode LaTeX-mode lisp-mode m4-mode makefile-mode modula-2-mode nroff-mode objc-mode pascal-mode perl-mode prolog-mode python-mode scheme-mode sgml-mode sh-mode shell-script-mode simula-mode tcl-mode tex-mode texinfo-mode vrml-mode xml-mode erlang-mode)))
 '(wrangler-search-paths (quote ("/home/alind" "../ebin" "." "../include")))
 '(yas/fallback-behavior (quote call-other-command))
 '(yas/root-directory (quote ("~/.emacs.d/snippets" "~/.emacs.d/plugins/yasnippet/snippets")) nil (yasnippet))
 '(yas/skip-and-clear-key "C-g"))

;(set-frame-font "-b&h-lucidatypewriter-medium-r-normal-sans-12-120-75-75-m-70-iso8859-1")

;; Create the autosave + backup dir if necessary, since emacs won't.
(make-directory "~/.emacs.d/autosaves/" t)
(make-directory "~/.emacs.d/backups/" t)

(defun restore-saved-window-size()
  (unless (load "~/.emacs.d/whsettings" t nil t)
    (setq saved-window-size '(80 30)))
  (nconc default-frame-alist `((width . ,(car saved-window-size))
                               (height . ,(cadr saved-window-size)))))

(defun save-window-size-if-changed (&optional unused)
  (let ((original-window-size  `(,(frame-width) ,(frame-height))))
    (unless (equal original-window-size saved-window-size)
      (with-temp-buffer
        (setq saved-window-size original-window-size)
        (insert (concat "(setq saved-window-size '"
                        (prin1-to-string saved-window-size) ")"))
        (write-file "~/.emacs.d/whsettings")))))

(restore-saved-window-size)
(add-hook 'window-size-change-functions 'save-window-size-if-changed)

(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "#2e3436" :foreground "#d3d7cf" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 140 :width normal :family "Menlo"))))
 '(compilation-error ((t (:inherit font-lock-warning-face :foreground "red"))))
 '(cursor ((t (:background "white" :foreground "black"))))
 '(flymake-errline ((((class color) (background dark)) (:background "#691A1B" :underline "#ef2929" :weight bold))))
 '(flymake-warnline ((((class color) (background dark)) (:background "#8f5902" :underline "#fce94f" :weight bold))))
 '(font-lock-fixme-face ((t (:stipple nil :background "Black" :foreground "OrangeRed1" :inverse-video nil :underline t :slant normal :weight bold))) t)
 '(font-lock-function-name-face ((t (:foreground "#729fcf" :underline t :weight bold))))
 '(font-lock-string-face ((t (:foreground "#c29fbe" :slant italic))))
 '(font-lock-variable-name-face ((t (:foreground "#c4a000" :weight bold))))
 '(header-line ((default (:inherit mode-line :box (:line-width 1 :color "darkgrey"))) (((class color grayscale) (background light)) (:background "grey90" :foreground "grey20" :box nil))))
 '(hl-line ((t (:background "gray23"))))
 '(minibuffer-prompt ((t (:foreground "#729fcf" :weight bold))))
 '(mumamo-background-chunk-major ((((class color) (min-colors 88) (background dark)) nil)))
 '(mumamo-background-chunk-submode1 ((((class color) (min-colors 88) (background dark)) (:background "gray15"))))
 '(mumamo-background-chunk-submode2 ((((class color) (min-colors 88) (background dark)) (:background "gray15"))))
 '(mumamo-background-chunk-submode3 ((((class color) (min-colors 88) (background dark)) (:background "grey15"))))
 '(mumamo-background-chunk-submode4 ((((class color) (min-colors 88) (background dark)) (:background "gray15"))))
 '(rng-error ((t (:inherit flymake-errline))))
 '(show-paren-match ((nil (:background "gray15"))))
 '(show-paren-mismatch ((((class color)) (:background "red4"))))
 '(tabbar-default-face ((t (:inherit variable-pitch))))
 '(tabbar-selected-face ((t (:inherit tabbar-default-face :background "white" :box (:line-width 2 :color "white") :weight bold))))
 '(tabbar-unselected-face ((t (:inherit tabbar-default-face :box (:line-width 2 :color "white" :style released-button)))))
 '(yas/field-highlight-face ((t (:background "DimGrey" :underline "white")))))

;; -----------------------------------------------------------------------------

;; Erlang Mode
(setq erlang-man-root-dir "/usr/local/Cellar/erlang/R14B01")
(setq erlang-root-dir "/usr/local/Cellar/erlang/R14B01")
(setq exec-path (cons "/usr/local/Cellar/erlang/R14B01/bin" exec-path))
(require 'erlang-start)
(setq auto-mode-alist (append auto-mode-alist
                              '(("\\.rel$" . erlang-mode)
                                ("\\.app$" . erlang-mode)
                                ("\\.appSrc$" . erlang-mode)
                                ("\\.app.src$" . erlang-mode)
                                ("\\.hrl$" . erlang-mode)
                                ("\\.erl$" . erlang-mode)
                                ("\\.yrl$" . erlang-mode))))

;; Distel
(require 'distel)
(distel-setup)
;(setq distel-tags-compliant nil)

;(setq erl-nodename-cache 'emacs@localhost)
;(setq inferior-erlang-machine-options '("-sname" "emacs@localhost"))
;(setq distel-modeline-node "emacs@localhost")
;(inferior-erlang)
;(switch-to-buffer "*scratch*")

;; inspired by http://www.emacswiki.org/cgi-bin/wiki/PrettyLambda
(defun pretty-erlang ()
  (interactive)
  (let ((replacements '(("\\(->\\)"             . "→")
                        ("\\(<-\\)"             . "←")
                        ;("\\(||\\)"            . "‖")
                        ("[^=]\\(==\\)[^=]"     . "≈")
                        ("\\(=:=\\)"            . "≡")
                        ("\\(=/=\\)"            . "≢")
                        ("[^=]\\(/=\\)"         . "≠")
                        ("\\(=<\\)"             . "≤")
                        ("\\(>=\\)"             . "≥")
                        ("\\(<<\\)"             . "«")
                        ("\\(>>\\)"             . "»")
                        ;("\\(::\\)"            . "⁛")
                        ;("\\(~s\\)"            . "Ⓢ")
                        ;("\\(~p\\)"            . "Ⓟ")
                        ;("\\(~w\\)"            . "Ⓦ")
                        ("\\(~n\\)"             . "↵")
                        ;("\\(\\<fun\\>\\)[( ]" . "λ")
                        ;("\\(\\<catch\\>\\) "  . "☂")
                        )))
    (font-lock-add-keywords
     nil
     (mapcar
      (lambda (pair)
        `(,(car pair)
          (0 (prog1 nil
               (compose-region (match-beginning 0) (match-end 0) ,(cdr pair))))))
      replacements))))
(add-hook 'erlang-mode-hook 'pretty-erlang)

;; Interactively Do Things (smart tab-completion in find file etc.)
(require 'ido)
(ido-mode t)

;; Super Meta X! (uses ido for M-x)
(require 'smex)
(smex-initialize)

(require 'auto-complete)
(global-auto-complete-mode t)

(hl-line-mode t)

;; Xah Lee's ergonomic Ergomacs Keybindings
;; http://xahlee.org/emacs/ergonomic_emacs_keybinding.html
(load "ergonomic_keybinding_dvorak")

; a package which enables text selection with the shift and arrow keys
(load-library "pc-select")
(pc-selection-mode)

;; Move mode line to top (into the header line actually)
(setq default-header-line-format default-mode-line-format
      default-mode-line-format nil)

;; Color Themes
;(require 'color-theme-autoload "color-theme-autoloads")
(require 'color-theme)
(require 'color-theme-tango-3)
(eval-after-load "color-theme"
  '(progn (color-theme-initialize)
          (color-theme-tango-3)))

;; Outlining
; Erlang is enabled in the erlang-mode-hook above.
(add-hook 'emacs-lisp-mode-hook (lambda () (hs-minor-mode 1)))

;; Flymake
(require 'flymake)
(defun flymake-erlang-init ()
  (let* ((temp-file (flymake-init-create-temp-buffer-copy
                     'flymake-create-temp-inplace))
         (local-file (file-relative-name
                      temp-file
                      (file-name-directory buffer-file-name))))
    (list "~/Applications/bin/flymake" (list local-file))))

(setq flymake-allowed-file-name-masks '(("\\.erl\\'" flymake-erlang-init)))
(add-hook 'after-save-hook 'flymake-start-syntax-check)

(defun my-flymake-show-help ()
  (when (get-char-property (point) 'flymake-overlay)
    (let ((help (get-char-property (point) 'help-echo)))
      (if help (message "%s" help)))))

(add-hook 'post-command-hook 'my-flymake-show-help)

;; Graphviz mode
;(load-file "~/.emacs.d/graphviz-dot-mode.el")

;; CSS Settings
(setq cssm-indent-level 4)
(setq cssm-newline-before-closing-bracket t)
(setq cssm-indent-function #'cssm-c-style-indenter)
(setq cssm-mirror-mode nil)

;; Markdown mode
(require 'markdown-mode)
(setq auto-mode-alist
      (cons '("\\.md" . markdown-mode) auto-mode-alist))

;(require 'yasnippet)
;(yas/initialize)

;; Remove trailing whitespace in buffer, untabify before saving
;; - excluded make, because it barfs on spaces instead of tabs as indentation
(add-hook 'before-save-hook
          (lambda ()
            (delete-trailing-whitespace)
            (when (not (memq major-mode '(makefile-mode makefile-bsdmake-mode)))
              (whitespace-cleanup))))

(add-hook 'align-load-hook
          (lambda ()
            (add-to-list 'align-rules-list
                         '(erlang-align
                           (regexp . ",\\(\\s-+\\)")
                           (repeat . t)
                           (modes quote (erlang-mode))))))

;; Kill the minibuffer when switching buffer with the mouse
(defun stop-using-minibuffer ()
  "kill the minibuffer"
  (when (and (>= (recursion-depth) 1) (active-minibuffer-window))
    (abort-recursive-edit)))
(add-hook 'mouse-leave-buffer-hook 'stop-using-minibuffer)

;; ack frontend
(add-to-list 'load-path "~/.emacs.d/full-ack")
(autoload 'ack-same "full-ack" nil t)
(autoload 'ack "full-ack" nil t)
(autoload 'ack-find-same-file "full-ack" nil t)
(autoload 'ack-find-file "full-ack" nil t)

;; Automatically insert matching parenthesises etc.
(require 'autopair)
(autopair-global-mode 1)
(setq autopair-blink nil)

;; Inline edit of text (variables etc)
(require 'iedit)

;; Highlight FIXME, BUG, TODO etc.
(require 'fixme-mode)

;; HTTP Twiddle mode to make HTTP requests from Emacs
(require 'http-twiddle)

(defun try-load-file (file)
  "Try to load a file and do nothing if it doesn't exist"
  (if (file-exists-p file)
      (load-file file)))

;; Load private code
(try-load-file "~/.emacs.d/private.el")

;; Load my custom shortcuts
(try-load-file "~/.emacs.d/keyconfig.el")