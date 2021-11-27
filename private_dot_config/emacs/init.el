;; *** Ads ***

;; Remove GNU advertisements

(setq inhibit-startup-message t)

(global-unset-key "\C-h\C-c")
(global-unset-key "\C-h\C-d")
(global-unset-key "\C-h\C-p")
(global-unset-key "\C-h\C-w")
(global-unset-key "\C-hn")
(global-unset-key "\C-h\C-n")
(global-unset-key "\C-h\C-m")

;; *** GC ***

(setq gc-cons-threshold 10000000)

;; *** I/O ***

(setq read-process-output-max 1000000)

;; *** Packages ***

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; *** i18n ***

(set-language-environment "UTF-8")

(add-to-list 'safe-local-variable-values '(encoding . utf-8))

;; *** UI tweaks ***

;; Use echo area instead of (ugly) UI tooltips
(tooltip-mode 0)

;; Show small indicators at the gutter where
;; the buffer starts and ends
(setq-default indicate-buffer-boundaries t)

;; Blinking cursor gonna blink
(setq blink-cursor-delay 0
      blink-cursor-interval 0.1
      blink-cursor-blinks 0)

;; Paste goes to the current text cursor position
(setq mouse-yank-at-point t)

(defun dm>frame-title ()
  (let ((f (buffer-file-name)))
    (if f
        (abbreviate-file-name f)
      (buffer-name))))

;; Show current file or buffer name in title
(setq frame-title-format
      '((:eval (dm>frame-title))))

;; * Visual bell *

;; Visual bell on macOS is ugly, so make our own by flashing the mode line

(defun dm>invert-mode-line ()
  (invert-face 'mode-line)
  (invert-face 'mode-line-inactive))

(defun dm>ring-bell-flash-mode-line ()
  "Do a visual bell by quickly flashing mode line."
  (dm>invert-mode-line)
  (run-with-timer 0.1 nil 'dm>invert-mode-line))

(if (eq system-type 'darwin)
    (setq ring-bell-function #'dm>ring-bell-flash-mode-line))
(setq visible-bell nil)

;; *** Fonts ***

(set-face-attribute 'default nil :family "Liberation Mono" :height 110)

;; *** Input ***

;; C-x 8 prefix map is useless
(define-key key-translation-map "\C-x8" nil)

;; Use right Option as AltGr (extra symbols), not as regular Alt
(if (eq system-type 'darwin)
    (setq ns-right-alternate-modifier 'none))

;; Scroll by moving cursor, not viewport around
(if (eq system-type 'darwin) ;; Due to some reason these are other way around in mwheel.el
    (setq mouse-wheel-down-event 'wheel-down
          mouse-wheel-up-event 'wheel-up))

(setq mwheel-scroll-up-function 'previous-line
      mwheel-scroll-down-function 'next-line)

(setq mouse-wheel-scroll-amount '(1))

;; Let OS handle the speeding up of scrolling
(setq mouse-wheel-progressive-speed nil)

;; Disable useless mapping of C-x C-left / C-x C-right
(define-key global-map [XF86Forward] nil)
(define-key global-map [XF86Back] nil)

;; I use these functions
(put 'narrow-to-region 'disabled nil)
(put 'downcase-region 'disabled nil)

(straight-use-package 'which-key)
(which-key-mode t)

;; *** Autorevert ***

(global-auto-revert-mode t)

;; *** emacsclient ***

(server-start)

;; *** Editing ***

;; Do not visually wrap text
(setq-default truncate-lines t)
(setq truncate-partial-width-windows nil)

;; Show current column in modeline
(setq column-number-mode t)

;; Fold text to 120 columns
(setq-default fill-column 120)

;; Move cursor by logical (not visual) lines, even if the text is wrapped
(setq line-move-visual nil)

;; Keep cursor to the end of line when moving around vertically
(setq track-eol t)

;; * Highlight full expressions *

(setq show-paren-delay 0)
(setq show-paren-style 'expression)
(setq show-paren-when-point-inside-paren t)

(set-face-attribute 'show-paren-match nil :weight 'bold :background 'unspecified)

(show-paren-mode 1)

;; *** Whitespace ***

;; This `require' is needed because whitespace-display-mappings are modified below
(require 'whitespace)

(setq whitespace-line-column 120)

;; Do not display spaces as dots
(setq-default
 whitespace-display-mappings
 (remove '(space-mark ?\  [?·] [?.]) whitespace-display-mappings))

;; Show tabs as light-gray chevrons, not black
(set-face-attribute 'whitespace-tab nil :foreground "gray90" :background 'unspecified)

;; Show overly long lines as gray
(set-face-attribute 'whitespace-line nil :background "gray80")

(setq whitespace-style
      '(face             ;; Enable visualization via faces
        trailing         ;; Highlight trailing spaces
        tabs             ;; Display tabs via faces (to mellow their tab-mark chevron look)
        tab-mark         ;; Display tabs as chevrons
        lines-tail       ;; Highlight lines over `whitespace-line-column' long
        empty            ;; Highlight empty lines at the beginning/end of the buffer
        space-before-tab ;; Highlight spaces before TABs
        space-mark       ;; Display hard spaces (regular spaces are removed in assignment above)
        ))

(global-whitespace-mode 1)

(defun dm>disable-whitespace ()
  (whitespace-mode 0))

;; Not needed in dired
(add-hook 'dired-mode-hook #'dm>disable-whitespace)

;; No double space after dot
(setq sentence-end-double-space nil)

(defun dm>disable-indent-tabs ()
  (setq indent-tabs-mode nil))

;; *** Window navigation ***

(defun dm>switch-to-previous-window ()
  (interactive)
  (other-window -1))

(defun dm>switch-to-next-window ()
  (interactive)
  (other-window 1))

(global-set-key (kbd "C-,") #'dm>switch-to-previous-window)
(global-set-key (kbd "C-.") #'dm>switch-to-next-window)

;; *** Sessions ***

(savehist-mode 1)
(setq savehist-additional-variables
      '(kill-ring search-ring regexp-search-ring))
(setq history-length 1000)

(save-place-mode 1)

;; *** Backup files ***

(setq make-backup-files nil)

;; *** Environment ***

(when (eq system-type 'darwin)
  (straight-use-package 'exec-path-from-shell)
  (require 'exec-path-from-shell)
  (exec-path-from-shell-initialize))

;; *** Modes ***

;; * LSP *

(straight-use-package 'lsp-mode)
(straight-use-package 'lsp-ui)

;; Do not use default configuration: it enables too much UX
(setq lsp-auto-configure nil)

;; No slowpokes around
(setq lsp-idle-delay 0.01)

;; * Flycheck *

(straight-use-package 'flycheck)

;; Give me errors without delay
(setq flycheck-display-errors-delay 0)

;; * Shell *

(setq sh-basic-offset 2)

;; * Go *

(straight-use-package 'go-mode)

(when (eq system-type 'darwin)
  (exec-path-from-shell-copy-env "GOPATH"))

;; * YAML *

(straight-use-package 'yaml-mode)

;; * Lisp-likes *

(straight-use-package 'paredit)
(autoload 'enable-paredit-mode "paredit" "Turn on pseudo-structural editing of Lisp code." t)

(straight-use-package 'rainbow-delimiters)

;; Required to set face attributes below
(require 'rainbow-delimiters)

(set-face-attribute 'rainbow-delimiters-depth-1-face nil :foreground "#000000")
(set-face-attribute 'rainbow-delimiters-depth-2-face nil :foreground "#ee0000")
(set-face-attribute 'rainbow-delimiters-depth-3-face nil :foreground "#0000ee")
(set-face-attribute 'rainbow-delimiters-depth-4-face nil :foreground "#ee7600")
(set-face-attribute 'rainbow-delimiters-depth-5-face nil :foreground "#00eeee")
(set-face-attribute 'rainbow-delimiters-depth-6-face nil :foreground "#cdcd00")
(set-face-attribute 'rainbow-delimiters-depth-7-face nil :foreground "#ff40ff")
(set-face-attribute 'rainbow-delimiters-depth-8-face nil :foreground "#32cd32")
(set-face-attribute 'rainbow-delimiters-depth-9-face nil :foreground "#b03060")

(set-face-attribute 'rainbow-delimiters-unmatched-face nil :foreground "#8b0000" :background "#ff4500")
(set-face-attribute 'rainbow-delimiters-mismatched-face nil :foreground "#8b0000" :background "#ff4500")

;; * elisp *

(add-hook 'emacs-lisp-mode-hook #'enable-paredit-mode)
(add-hook 'emacs-lisp-mode-hook #'rainbow-delimiters-mode)
(add-hook 'emacs-lisp-mode-hook #'dm>disable-indent-tabs)

;; * clojure, EDN *

(straight-use-package 'clojure-mode)

(add-hook 'clojure-mode #'enable-paredit-mode)
(add-hook 'clojure-mode #'rainbow-delimiters-mode)

(add-to-list 'auto-mode-alist '("\\.edn\\'" . clojure-mode))

;; * JSON *

(straight-use-package 'json-mode)

(defun dm>json-set-offset ()
  (setq-local js-indent-level 2))

(add-hook 'json-mode-hook #'dm>json-set-offset)

;; * Markdown *

(straight-use-package 'markdown-mode)

;; *** Compilation ***

;; Autosave modified buffers
(setq compilation-ask-about-save nil)

;; Stop at first error
(setq compilation-scroll-output 'first-error)

(global-set-key (kbd "<f6>") 'compile)

;; *** VCS ***

(straight-use-package 'magit)
(straight-use-package 'git-gutter)

(global-git-gutter-mode 1)

;; I do not use anyting but Git in Emacs anyway
(setq vc-handled-backends nil)
