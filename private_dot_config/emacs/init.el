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

;; This is the easiest way to get rid of the startup message
(defun startup-echo-area-message () "")

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

;; Make highlight less visually harsh

(set-face-attribute 'highlight nil :background "gray90")

;; font-lock faces

(set-face-attribute 'font-lock-keyword-face nil :foreground "#888888")
(set-face-attribute 'font-lock-comment-face nil :foreground "#00bb00")
(set-face-attribute 'font-lock-string-face nil :foreground "#008800")
(set-face-attribute 'font-lock-constant-face nil :foreground "blue1")
(set-face-attribute 'font-lock-function-name-face nil :foreground "blue1")
(set-face-attribute 'font-lock-variable-name-face nil :foreground 'unspecified)
(set-face-attribute 'font-lock-builtin-face nil :foreground 'unspecified)
(set-face-attribute 'font-lock-type-face nil :foreground "#008080")

;; * Visual bell *

;; Default visual bell is ugly, so make our own by flashing the mode line

(defun dm>invert-mode-line ()
  (invert-face 'mode-line)
  (invert-face 'mode-line-inactive))

(defun dm>ring-bell-flash-mode-line ()
  "Do a visual bell by quickly flashing mode line."
  (dm>invert-mode-line)
  (run-with-timer 0.1 nil 'dm>invert-mode-line))

(setq ring-bell-function #'dm>ring-bell-flash-mode-line)

;; *** Fonts ***

(if (eq system-type 'darwin)
    (set-face-attribute 'default nil :family "Liberation Mono" :height 110)
  (set-face-attribute 'default nil :family "Liberation Mono" :height 84))

;; *** Input ***

;; C-x 8 prefix map is useless
(define-key key-translation-map "\C-x8" nil)

;; Use right Option as AltGr (extra symbols), not as regular Alt
(if (eq system-type 'darwin)
    (setq ns-right-alternate-modifier 'none))

;; Scroll by moving cursor, not viewport around

;; Due to some reason these are other way around in mwheel.el
(if (eq system-type 'darwin)
    (setq mouse-wheel-down-event 'wheel-down
          mouse-wheel-up-event 'wheel-up)
  (setq mouse-wheel-down-event 'mouse-5
        mouse-wheel-up-event 'mouse-4))

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

;; * SEARCH *

;; FIXME: Add "grep" shortcut that defaults to "git grep" inside Git repositories

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

(global-set-key (kbd "s-o") 'find-file)
(global-set-key (kbd "s-s") 'save-buffer)
(global-set-key (kbd "s-w") 'kill-this-buffer)

(defun dm>new-buffer ()
  (interactive)
  (let ((buf (generate-new-buffer "untitled")))
    (switch-to-buffer buf)
    (funcall initial-major-mode)
    (setq buffer-offer-save t)))

(global-set-key (kbd "s-n") 'dm>new-buffer)

;; *** Search ***

(straight-use-package 'rg)

(global-set-key (kbd "s-f") #'rg)
(global-set-key (kbd "<f11>") #'previous-error)
(global-set-key (kbd "<f12>") #'next-error)

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

;; *** OS integration ***

;; * Darwin

(defun dm>kitty ()
  (interactive)
  (start-process "kitty" nil "kitty" "--single-instance"))

(defun dm>darwin-dir ()
  (interactive)
  (start-process "finder" nil "open" "."))

(defun dm>darwin-setup-os-integration ()
  (global-set-key (kbd "s-t") #'dm>kitty)
  (global-set-key (kbd "s-r") #'dm>darwin-dir)

  (global-unset-key (kbd "s-k")) ; kill-this-buffer, use Cmd-W instead
  (global-unset-key (kbd "s-q")) ; save-buffers-kill-emacs, just disable it
  (global-unset-key (kbd "s-p")) ; ns-print-buffer, unused
  (global-unset-key (kbd "s-n")) ; ns-make-frame, unused
)

(if (eq system-type 'darwin)
    (dm>darwin-setup-os-integration))

;; * Linux/X/Wayland

(defun dm>x-terminal-emulator ()
  (interactive)
  (start-process "x-terminal-emulator" nil "x-terminal-emulator"))

(defun dm>xdg-dir ()
  (interactive)
  (start-process "xdg-dir" nil "xdg-open" "."))

(defun dm>linux-setup-os-integration ()
  (global-set-key (kbd "s-t") #'dm>x-terminal-emulator)
  (global-set-key (kbd "s-r") #'dm>xdg-dir))

(if (eq system-type 'gnu/linux)
    (dm>linux-setup-os-integration))

;; *** Modes ***

;; * LSP *

(straight-use-package 'lsp-mode)
(straight-use-package 'lsp-ui)

(require 'lsp-ui) ;; lsp-ui-doc--frame-visible-p is used below

;; Do not use default configuration: it enables too much UX
(setq lsp-auto-configure nil)

;; No slowpokes around
(setq lsp-idle-delay 0.01)

;; Enable symbol highlight (what lsp-ui usually does)
(defun dm>lsp-enable-symbol-highlight ()
  (setq lsp-enable-symbol-highlighting t)
  (add-hook 'lsp-on-idle-hook #'lsp--document-highlight nil t))
(add-hook 'lsp-configure-hook #'dm>lsp-enable-symbol-highlight)

;; Disable ElDoc in LSP, lsp-ui enables it unconditionally
(defun dm>lsp-disable-eldoc ()
  (eldoc-mode 0))
(add-hook 'lsp-configure-hook #'dm>lsp-disable-eldoc)

;; Can be used by any mode
(defun dm>lsp-ui-doc-toggle ()
  (interactive)
  (if (lsp-ui-doc--frame-visible-p)
      (lsp-ui-doc-hide)
    (lsp-ui-doc-show)))

;; Configuration of lsp-ui doc window
(setq lsp-ui-doc-header t
      lsp-ui-doc-include-signature t)

;; * Flycheck *

(straight-use-package 'flycheck)

;; Give me errors without delay
(setq flycheck-display-errors-delay 0)

;; * Shell *

(setq sh-basic-offset 2)

;; * Go *

(straight-use-package 'go-mode)

;; Needed to add keys to go-mode-map
(require 'go-mode)

(when (eq system-type 'darwin)
  (exec-path-from-shell-copy-env "GOPATH"))

(add-hook 'go-mode-hook #'lsp)

;; Configure LSP for Go

(require 'lsp-go) ;; Load gopls support
(require 'lsp) ;; lsp-defun is used below
(require 'lsp-ui-flycheck) ;; Load lsp-flycheck support

;; Autoformat on save
(defun dm>go-mode-hook ()
  (add-hook 'before-save-hook #'lsp-format-buffer nil 'local)
  (add-hook 'before-save-hook #'lsp-organize-imports nil 'local))
(add-hook 'go-mode-hook #'dm>go-mode-hook)

;; Show documentation on Ctrl-F1
(define-key go-mode-map (kbd "C-<f1>") #'dm>lsp-ui-doc-toggle)

;; Open documentation in browser on Shift-Ctrl-F1
(defconst dm>godoc-re "(https://godoc\\.org/\\(.*?\\))")

(defun dm>godoc--find-open (contents)
  (save-match-data
    (when (string-match dm>godoc-re contents)
      (browse-url (concat "https://pkg.go.dev/" (match-string 1 contents))))))

(lsp-defun dm>godoc--open-callback ((hover &as &Hover? :contents) bounds buffer)
  (if (hash-table-p contents)
      (dm>godoc--find-open (gethash "value" contents))))

(defun dm>godoc-open ()
  "Open pkg.go.dev documentation for the current symbol"
  (interactive)
  (dm>godoc--open-callback
   (lsp-request "textDocument/hover" (lsp--text-document-position-params))
   (or (bounds-of-thing-at-point 'symbol) (cons (point) (1+ (point))))
   (current-buffer)))
(define-key go-mode-map (kbd "C-S-<f1>") #'dm>godoc-open)

;; Run golangci-lint after LSP lint

(require 'lsp-diagnostics)
(straight-use-package 'flycheck-golangci-lint)

(flycheck-golangci-lint-setup)

(defun dm>flycheck-add-next-checker-golangci-lint ()
  (unless (seq-contains-p (flycheck-get-next-checkers 'lsp) 'golangci-lint)
    (flycheck-add-next-checker 'lsp 'golangci-lint)))

(defun dm>flycheck-remove-next-checker-golangci-lint ()
  (when (seq-contains-p (flycheck-get-next-checkers 'lsp) 'golangci-lint)
    (flycheck-remove-next-checker 'lsp 'golangci-lint)))

(defun dm>window-switch-apply-flycheck-golangci-lint (&optional arg)
  (if (eq major-mode 'go-mode)
      (dm>flycheck-add-next-checker-golangci-lint)
    (dm>flycheck-remove-next-checker-golangci-lint)))

(add-hook 'window-state-change-functions #'dm>window-switch-apply-flycheck-golangci-lint)
(add-hook 'after-change-major-mode-hook #'dm>window-switch-apply-flycheck-golangci-lint)

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
