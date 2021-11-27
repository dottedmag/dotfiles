;; *** Ads ***

;; Remove GNU advertisements and not very helpful help-related
;; keybindings

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

;; *** Input ***

(define-key key-translation-map "\C-x8" nil) ;; C-x 8 prefix map is useless

;; Use right Option as AltGr (extra symbols), not as regular Alt
(if (eq system-type 'darwin)
    (setq ns-right-alternate-modifier 'none))

;; *** emacsclient ***

(server-start)

;; *** Editing ***

(setq-default truncate-lines t)

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

;; *** Languages ***

;; * Shell *

(setq sh-basic-offset 2)

;; *** VCS ***

(straight-use-package 'magit)
