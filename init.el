;; ----------------------------------------------------------------------------
;; .clang_complete reading
;; ----------------------------------------------------------------------------

(defun read-c-flags ()
  "list of flags from upward-found .clang_complete file, nil if not found"

  (defun upward-find-file (filename &optional startdir)
    (let ((dirname (expand-file-name (if startdir startdir ".")))
          (found nil)
          (top nil))
      (while (not (or found top))
             (if (string= (expand-file-name dirname) "/") (setq top t))
             (if (file-exists-p (expand-file-name filename dirname))
               (setq found t)
               (setq dirname (expand-file-name ".." dirname))))
      (if found (concat dirname "/") nil)))

  (defun read-lines (path)
    (with-temp-buffer
      (insert-file-contents path)
      (split-string (buffer-string) "\n" t)))

  (let ((path (upward-find-file ".clang_complete")))
    (if path (read-lines (concat path ".clang_complete")) nil)))


;; ----------------------------------------------------------------------------
;; packages
;; ----------------------------------------------------------------------------

(require 'package)
(setq package-archives '(("melpa" . "http://melpa.milkbox.net/packages/")
                         ("org" . "http://orgmode.org/elpa/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize)


;; --- color-theme-sanityinc-tomorrow -----------------------------------------

(load-theme 'sanityinc-tomorrow-night t)


;; --- evil -------------------------------------------------------------------

(setq evil-default-cursor (quote (t "#e0e0e0")))
(setq evil-want-C-u-scroll t)

(require 'evil)
(evil-mode 1)


;; --- flx-ido ----------------------------------------------------------------

(require 'flx-ido)
(ido-mode 1)
(ido-everywhere 1)
(flx-ido-mode 1)
(setq ido-use-faces nil)


;; --- projectile -------------------------------------------------------------

(projectile-global-mode)
(define-key evil-normal-state-map ",f" 'projectile-find-file)
(define-key evil-normal-state-map " " 'projectile-find-file)


;; --- multi-term -------------------------------------------------------------

(require 'multi-term)
(setq multi-term-program "/bin/bash")
(define-key evil-normal-state-map ",t" 'multi-term)


;; --- project-explorer -------------------------------------------------------

(require 'project-explorer)


;; --- git-gutter-fringe ------------------------------------------------------

(require 'git-gutter-fringe)
(global-git-gutter-mode t)


;; --- yasnippet --------------------------------------------------------------

(require 'yasnippet)
(yas-global-mode 1)


;; --- auto-complete ----------------------------------------------------------

(require 'auto-complete)

;; c
(require 'auto-complete-clang-async)
(defun ac-cc-mode-setup ()
  (setq ac-clang-complete-executable "/usr/local/opt/llvm/bin/clang-complete")
  (setq ac-sources '(ac-source-clang-async))
  (setq ac-clang-cflags (read-c-flags))
  (ac-clang-launch-completion-process))

;; common
(defun ac-common-setup ()
  ())
(defun my-ac-config ()
  (add-hook 'c-mode-common-hook 'ac-cc-mode-setup)
  (add-hook 'auto-complete-mode-hook 'ac-common-setup)
  (global-auto-complete-mode t))

(my-ac-config)


;; --- flycheck ---------------------------------------------------------------

(add-hook 'after-init-hook #'global-flycheck-mode)

;; c
(defun my-flycheck-c-config ()
  (defun read-c-includes ()
    (defun include-path-flag-p (s)
      (cond ((>= (length s) (length "-I"))
             (string-equal (substring s 0 (length "-I")) "-I"))
            (t nil)))
    (mapcar #'(lambda (s) (substring s 2))
            (remove-if-not 'include-path-flag-p (read-c-flags))))
  (setq flycheck-clang-include-path (read-c-includes)))
(add-hook 'c-mode-common-hook 'my-flycheck-c-config)


;; ----------------------------------------------------------------------------
;; interface
;; ----------------------------------------------------------------------------

;; don't use mac fullscreen
(setq ns-use-native-fullscreen nil)

;; cleanup
(menu-bar-mode 0)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(set-default 'truncate-lines t)
(setq initial-scratch-message "")
(setq inhibit-startup-message t)

;; transparency
(set-frame-parameter (selected-frame) 'alpha '(98 98))
(add-to-list 'default-frame-alist '(alpha 98 98))

;; initial frame size
(when window-system (set-frame-size (selected-frame) 141 53))

;; font
(defvar *default-font*
  "-apple-Menlo-medium-normal-normal-*-11-*-*-*-m-0-iso10646-1")
(when window-system (set-face-font 'default *default-font*))

;; white cursor
(set-cursor-color "#ffffff") 


;; ----------------------------------------------------------------------------
;; keys
;; ----------------------------------------------------------------------------

;; windmove
(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))


;; ----------------------------------------------------------------------------
;; formatting
;; ----------------------------------------------------------------------------

(setq-default indent-tabs-mode nil)

;; c
(require 'cc-mode)
(setq c-default-style "bsd" c-basic-offset 4)
(c-set-offset 'case-label '+)
(define-key c-mode-base-map (kbd "RET") 'newline-and-indent)


