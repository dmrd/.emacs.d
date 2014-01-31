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

(defun require-package (package)
  "same as require, but installs package if needed"
  (progn
    (unless (package-installed-p package)
      (unless (assoc package package-archive-contents)
        (package-refresh-contents))
      (package-install package))
    (require package)))


;; --- color-theme-sanityinc-tomorrow -----------------------------------------

(require-package 'color-theme-sanityinc-tomorrow)
(load-theme 'sanityinc-tomorrow-night t)


;; --- evil -------------------------------------------------------------------

(setq evil-default-cursor (quote (t "#e0e0e0")))
(setq evil-want-C-u-scroll t)

(require-package 'evil)
(evil-mode 1)


;; --- flx-ido ----------------------------------------------------------------

(require-package 'flx-ido)
(ido-mode 1)
(ido-everywhere 1)
(flx-ido-mode 1)
(setq ido-use-faces nil)


;; --- ido-ubiquitos ----------------------------------------------------------

(require-package 'ido-ubiquitous)
(ido-ubiquitous-mode)


;; --- smex -------------------------------------------------------------------

(require-package 'smex)
(smex-initialize)


;; --- projectile -------------------------------------------------------------

(require-package 'projectile)
(projectile-global-mode)


;; --- multi-term -------------------------------------------------------------

(require-package 'multi-term)
(setq multi-term-program "/bin/bash")


;; --- project-explorer -------------------------------------------------------

(require-package 'project-explorer)
(setq pe/width 23)


;; --- git-gutter-fringe ------------------------------------------------------

(require-package 'git-gutter-fringe)
(global-git-gutter-mode t)


;; --- yasnippet --------------------------------------------------------------

(require-package 'yasnippet)
(yas-global-mode 1)


;; --- company-mode -----------------------------------------------------------

(require-package 'company)
(defun my-company-c-config ()
 (setq company-clang-arguments (read-c-flags)))
(add-hook 'c-mode-common-hook 'my-company-c-config)

(global-company-mode t)
(setq company-idle-delay 0.2)


;; --- flycheck ---------------------------------------------------------------

(require-package 'flycheck)
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


;; --- perspective ------------------------------------------------------------

(require-package 'perspective)
(add-hook 'after-init-hook #'(lambda () (persp-mode 1)))


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

;; gdb
(setq gdb-many-windows t)


;; ----------------------------------------------------------------------------
;; keys
;; ----------------------------------------------------------------------------

;; find
(define-key evil-normal-state-map ",f" 'projectile-find-file)
(define-key evil-normal-state-map " " 'projectile-find-file)

;; apps
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
(define-key evil-normal-state-map ",t" 'multi-term)
(define-key evil-normal-state-map ",p" 'project-explorer-open)

;; persp
(define-key evil-normal-state-map ";w" 'persp-switch)
(define-key evil-normal-state-map ";r" 'persp-rename)
(define-key evil-normal-state-map ";k" 'persp-kill)
(define-key evil-normal-state-map ",a" 'persp-add-buffer)
(define-key evil-normal-state-map ",i" 'persp-import)
(define-key evil-normal-state-map ",k" 'persp-remove-buffer)

;; splits
(define-key evil-normal-state-map ",s" 'split-window-below)
(define-key evil-normal-state-map ",v" 'split-window-right)

;; buffers
(define-key evil-normal-state-map "\C-p" nil)
(global-set-key (kbd "C-p") 'previous-buffer)
(define-key evil-normal-state-map "\C-n" nil)
(global-set-key (kbd "C-n") 'next-buffer)

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


