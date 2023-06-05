;; -*- lexical-binding: t -*-

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
  (if-let ((f (buffer-file-name)))
      (abbreviate-file-name f)
    (buffer-name)))

;; Show current file or buffer name in title
(setq frame-title-format
      '((:eval (dm>frame-title))))

;; Make highlight less visually harsh

(set-face-attribute 'highlight nil :background "gray90")

;; font-lock faces

(set-face-attribute 'font-lock-keyword-face nil :foreground "#888888")
(set-face-attribute 'font-lock-comment-face nil :foreground "#aaffaa")
(set-face-attribute 'font-lock-string-face nil :foreground "#40af40")
(set-face-attribute 'font-lock-constant-face nil :foreground "#6060ff")
(set-face-attribute 'font-lock-function-name-face nil :foreground "#6060ff")
(set-face-attribute 'font-lock-variable-name-face nil :foreground 'unspecified)
(set-face-attribute 'font-lock-builtin-face nil :foreground 'unspecified)
(set-face-attribute 'font-lock-type-face nil :foreground "#008080")
(set-face-attribute 'highlight nil :background "#333333")

;; Disable keybinding suggestions (they display over minibuffer contents)

(setq suggest-key-bindings nil)

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

;; ** Modern text zoom keybindings **

;; Remove default bindings
(define-key ctl-x-map [(control ?+)] nil)
(define-key ctl-x-map [(control ?-)] nil)
(define-key ctl-x-map [(control ?=)] nil)
(define-key ctl-x-map [(control ?0)] nil)

;; Set modern bindings not only on Mac
(define-key global-map (kbd "s-=") 'text-scale-adjust)
(define-key global-map (kbd "s-+") 'text-scale-adjust)
(define-key global-map (kbd "s--") 'text-scale-adjust)
(define-key global-map (kbd "s-0") 'text-scale-adjust)

;; Smoother adjustment
(setq text-scale-mode-step 1.1)

;; *** Fonts ***

(if (eq system-type 'darwin)
    (set-face-attribute 'default nil :family "Liberation Mono" :height 110)
  (set-face-attribute 'default nil :family "Liberation Mono" :height 90))

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

(define-key global-map (kbd "s-.") 'execute-extended-command)

;; *** eldoc ***

(setq eldoc-idle-delay 0.01)

;; *** Autorevert ***

(global-auto-revert-mode t)
(with-eval-after-load 'autorevert
  ;; Modern machines are pretty fast, no need to wait 2.5s between reverts
  (setq auto-revert--lockout-interval 0.01))

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

(setq show-paren-delay 0.01)
(setq show-paren-style 'expression)
(setq show-paren-when-point-inside-paren t)

(set-face-attribute 'show-paren-match nil :weight 'bold :background 'unspecified)

(show-paren-mode 1)

;; * M-d/M-\ should not save text to kill ring *

(defun dm>delete-word (arg)
  (interactive "p")
  (delete-region (point) (progn (forward-word arg) (point))))

(defun dm>backward-delete-word (arg)
  (interactive "p")
  (dm>delete-word (- arg)))

(global-set-key (kbd "C-<backspace>") #'dm>backward-delete-word)
(global-set-key (kbd "M-<delete>") #'dm>backward-delete-word)
(global-set-key (kbd "M-DEL") #'dm>backward-delete-word)
(global-set-key (kbd "C-<delete>") #'dm>delete-word)
(global-set-key (kbd "M-d") #'dm>delete-word)

;; *** Whitespace ***

(with-eval-after-load 'whitespace
  (setq whitespace-line-column 120)

  ;; Do not display spaces as dots
  (setq-default
   whitespace-display-mappings
   (remove '(space-mark ?\  [?·] [?.]) whitespace-display-mappings))

  ;; Show tabs as light-gray chevrons, not black
  (set-face-attribute 'whitespace-tab nil :foreground "#4d4d4d" :background 'unspecified)

  ;; Show overly long lines as gray
  (set-face-attribute 'whitespace-line nil :background "#4d4d4d")

  (setq whitespace-style
      '(face             ;; Enable visualization via faces
        trailing         ;; Highlight trailing spaces
        tabs             ;; Display tabs via faces (to mellow their tab-mark chevron look)
        tab-mark         ;; Display tabs as chevrons
        lines-tail       ;; Highlight lines over `whitespace-line-column' long
        empty            ;; Highlight empty lines at the beginning/end of the buffer
        space-before-tab ;; Highlight spaces before TABs
        space-mark       ;; Display hard spaces (regular spaces are removed in assignment above)
        )))

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

(straight-use-package 'exec-path-from-shell)
(when (eq system-type 'darwin)
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

;; * Projects *

(straight-use-package 'projectile)

(with-eval-after-load 'project
  (require 'projectile) ;; bbatsov/projectile#1846
  (add-hook 'project-find-functions #'project-projectile))


;; * Native compilation *

(setq native-comp-async-report-warnings-errors 'silent)

;; *** Modes ***

;; * LSP *

(defun dm>eglot-organize-imports ()
  ;; - try to save a file with imports that are already organized
  ;; - gopls responds that source.organizeImports code action is not applicable
  ;; - eglot raises an error
  ;; Hence we have to suppress this completely useless error.
  (condition-case exc
      (call-interactively 'eglot-code-action-organize-imports)
    (error
     (let ((msg (error-message-string exc)))
       (unless (string= msg "[eglot] No \"source.organizeImports\" code actions here")
         (error msg))))))

(defun dm>eglot-save-modified-files-after (orig-fun &rest args)
  (let* (modified-files
         (remember-modified-file
          (lambda (&rest args)
            (setq modified-files (cons (buffer-file-name) modified-files)))))
    (unwind-protect
        (progn
          (advice-add 'eglot--apply-text-edits :after remember-modified-file)
          (apply orig-fun args))
      (advice-remove 'eglot--apply-text-edits remember-modified-file)
      (dolist (modified-file modified-files)
        (with-current-buffer (find-buffer-visiting modified-file)
          (print modified-file)
          (save-buffer))))))

;; Save files after rename
(with-eval-after-load 'eglot
  (advice-add 'eglot-rename :around #'dm>eglot-save-modified-files-after))

;; * Flycheck *

(straight-use-package 'flycheck)

;; Give me errors without delay
(setq flycheck-display-errors-delay 0.01)

;; * Shell *

(setq sh-basic-offset 2)

;; * Go *

(straight-use-package 'go-mode)

(when (eq system-type 'darwin)
  (exec-path-from-shell-copy-env "GOPATH"))

;; Format on save

(defun dm>go-mode-format-before-save ()
  (add-hook 'before-save-hook #'eglot-format-buffer -10 t)
  (add-hook 'before-save-hook #'dm>eglot-organize-imports nil t))

(with-eval-after-load 'go-mode
  (add-hook 'go-mode-hook 'eglot-ensure)
  (add-hook 'go-mode-hook #'dm>go-mode-format-before-save))

;; * YAML *

(straight-use-package 'yaml-mode)

;; * Lisp-likes *

(straight-use-package 'paredit)
(autoload 'enable-paredit-mode "paredit" "Turn on pseudo-structural editing of Lisp code." t)

(straight-use-package 'rainbow-delimiters)

(defun dm>paredit-backward-delete-word ()
  "Kill a word backward, skipping over any intervening delimiters."
  (interactive)
  (if (not (or (bobp)
               (eq (char-syntax (char-before)) ?w)))
      (let ((end (point)))
        (backward-word 1)
        (forward-word 1)
        (goto-char (min end (point)))
        (let* ((parse-state (paredit-current-parse-state))
               (state
                (paredit-kill-word-state parse-state 'char-before)))
          (while (and (< (point) end)
                      (progn
                        (setq parse-state
                              (parse-partial-sexp (point) (1+ (point))
                                                  nil nil parse-state))
                        (or (eq state
                                (paredit-kill-word-state parse-state
                                                         'char-before))
                            (progn (backward-char 1) nil)))))
          (if (and (eq state 'comment)
                   (eq ?\# (char-after (point)))
                   (eq ?\| (char-before (point))))
              (backward-char 1)))))
  (dm>backward-delete-word 1))

(defun dm>paredit-forward-delete-word ()
  "Kill a word forward, skipping over intervening delimiters."
  (interactive)
  (let ((beginning (point)))
    (skip-syntax-forward " -")
    (let* ((parse-state (paredit-current-parse-state))
           (state (paredit-kill-word-state parse-state 'char-after)))
      (while (not (or (eobp)
                      (eq ?w (char-syntax (char-after)))))
        (setq parse-state
              (progn (forward-char 1) (paredit-current-parse-state))
;;               (parse-partial-sexp (point) (1+ (point))
;;                                   nil nil parse-state)
              )
        (let* ((old-state state)
               (new-state
                (paredit-kill-word-state parse-state 'char-after)))
          (cond ((not (eq old-state new-state))
                 (setq parse-state
                       (paredit-kill-word-hack old-state
                                               new-state
                                               parse-state))
                 (setq state
                       (paredit-kill-word-state parse-state
                                                'char-after))
                 (setq beginning (point)))))))
    (goto-char beginning)
    (dm>delete-word 1)))

(defun dm>setup-paredit ()
  (define-key paredit-mode-map (kbd "C-<backspace>") #'dm>paredit-backward-delete-word)
  (define-key paredit-mode-map (kbd "M-<delete>") #'dm>paredit-backward-delete-word)
  (define-key paredit-mode-map (kbd "M-DEL") #'dm>paredit-backward-delete-word)
  (define-key paredit-mode-map (kbd "C-<delete>") #'dm>paredit-forward-delete-word)
  (define-key paredit-mode-map (kbd "M-d") #'dm>paredit-forward-delete-word))

(eval-after-load 'paredit #'dm>setup-paredit)

(with-eval-after-load 'rainbow-delimiters
  (set-face-attribute 'rainbow-delimiters-depth-1-face nil :foreground "#ffffff")
  (set-face-attribute 'rainbow-delimiters-depth-2-face nil :foreground "#ee0000")
  (set-face-attribute 'rainbow-delimiters-depth-3-face nil :foreground "#0000ee")
  (set-face-attribute 'rainbow-delimiters-depth-4-face nil :foreground "#ee7600")
  (set-face-attribute 'rainbow-delimiters-depth-5-face nil :foreground "#00eeee")
  (set-face-attribute 'rainbow-delimiters-depth-6-face nil :foreground "#cdcd00")
  (set-face-attribute 'rainbow-delimiters-depth-7-face nil :foreground "#ff40ff")
  (set-face-attribute 'rainbow-delimiters-depth-8-face nil :foreground "#32cd32")
  (set-face-attribute 'rainbow-delimiters-depth-9-face nil :foreground "#b03060")
  (set-face-attribute 'rainbow-delimiters-unmatched-face nil :foreground "#8b0000" :background "#ff4500")
  (set-face-attribute 'rainbow-delimiters-mismatched-face nil :foreground "#8b0000" :background "#ff4500"))

;; * elisp *

(add-hook 'emacs-lisp-mode-hook #'enable-paredit-mode)
(add-hook 'emacs-lisp-mode-hook #'rainbow-delimiters-mode)
(add-hook 'emacs-lisp-mode-hook #'dm>disable-indent-tabs)

;; * clojure, EDN *

(straight-use-package 'clojure-mode)

(add-hook 'clojure-mode #'enable-paredit-mode)
(add-hook 'clojure-mode #'rainbow-delimiters-mode)

(add-to-list 'auto-mode-alist '("\\.edn\\'" . clojure-mode))

;; * Zig **

(straight-use-package 'zig-mode)

;; * Terraform *

(straight-use-package 'terraform-mode)

;; * JSON *

(straight-use-package 'json-mode)

(defun dm>json-set-offset ()
  (setq-local js-indent-level 2))

(add-hook 'json-mode-hook #'dm>json-set-offset)

;; * Markdown *

(straight-use-package 'markdown-mode)

;; * Groovy *

(straight-use-package 'groovy-mode)

(add-hook 'groovy-mode-hook #'dm>disable-indent-tabs)

;; *** Compilation ***

;; Autosave modified buffers
(setq compilation-ask-about-save nil)

;; Stop at first error
(setq compilation-scroll-output 'first-error)

(global-set-key (kbd "<f6>") 'compile)

;; *** VCS ***

(straight-use-package 'magit)

;; I do not use anyting but Git in Emacs anyway
(setq vc-handled-backends nil)

;; When opening a file from 'git commit', show a 72-limit column
;; FIXME (dottedmag): maybe replicate what magit does?

(defun dm>is-git-commit-buffer ()
  "Checks if the file is opened by 'git commit'."
  (and buffer-file-name
       (string= "COMMIT_EDITMSG" (file-name-nondirectory buffer-file-name))))

(defun dm>git-commit-show-fill-column ()
    "Display fill-column indicator in buffers opened by 'git commit'"
  (when (dm>is-git-commit-buffer)
    (setq fill-column 72)
    (display-fill-column-indicator-mode)))

(add-hook 'find-file-hook #'dm>git-commit-show-fill-column)

;; *** Copilot ***

(straight-use-package
 '(copilot :type git :host github :repo "zerolfx/copilot.el"
           :post-build (make-symbolic-link "../../repos/copilot.el/dist"
                        (concat user-emacs-directory "straight/build/copilot/dist") t)))

(add-hook 'straight-use-package-post-build-functions #'dm>link-copilot-el)

;; FIXME (dottedmag): Linux
(when (eq system-type 'darwin)
  (setq copilot-node-executable
        "/opt/homebrew/opt/node@16/bin/node"))
(add-hook 'prog-mode-hook 'copilot-mode)

(global-set-key (kbd "C-<return>") 'copilot-accept-completion)
(global-set-key (kbd "M-p") 'copilot-previous-completion)
(global-set-key (kbd "M-n") 'copilot-next-completion)
