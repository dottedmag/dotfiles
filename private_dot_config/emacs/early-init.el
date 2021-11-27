;; Do some initialization that is too late to do when
;; init.el is read:
;;
;; - Disable packages that are not in use and loaded before init.el
;; - Tweak UI before first UI frame is created

;; I do not use package.el

(setq package-enable-at-startup nil)

;; I do not use scroll bar and tool bar

(tool-bar-mode -1)
(scroll-bar-mode -1)

;; I do not use menu bar (except macOS GUI where it is integrated in OS UI)

(defun dm>hide-menu-if-terminal-frame (frame)
  ;; Compare with t explicitly, because framep returns t for terminal
  ;; frames, various symbols for GUI frames and nil otherwise
  (if (eq (framep frame) t)
      (set-frame-parameter frame'menu-bar-lines 0)))

(defun dm>hide-menu-in-terminal-frames ()
  (add-hook 'after-make-frame-functions #'dm>hide-menu-if-terminal-frame))

(defun dm>hide-menu-everywhere ()
  (menu-bar-mode -1))

(if (eq system-type 'darwin)
    (dm>hide-menu-in-terminal-frames)
  (dm>hide-menu-everywhere))
