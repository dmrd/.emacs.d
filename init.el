;; ------------------------------------------------------------------------------
;; --- Environment --------------------------------------------------------------
;; ------------------------------------------------------------------------------
(add-to-list 'load-path "/usr/texbin")
(add-to-list 'exec-path "/usr/local/bin")

;; Backup files
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))
(setq make-backup-files nil)


;; ------------------------------------------------------------------------------
;; --- Packages -----------------------------------------------------------------
;; ------------------------------------------------------------------------------

(require 'package)
(setq package-archives '(("melpa" . "http://melpa.milkbox.net/packages/")
                         ("org" . "http://orgmode.org/elpa/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize)

(defun safe-install (package)
  "Install only if needed"
  (unless (package-installed-p package)
    (unless (assoc package package-archive-contents)
      (package-refresh-contents))
    (package-install package)))

(defun require-package (package)
  "same as require, but installs package if needed"
  (progn
    (safe-install package)
    (require package)))


;; --- misc --------------------------------------------------------------------
(require-package 'autopair)
(require-package 'cider)
(require-package 'ess)
(require-package 'org)
(require-package 'paredit)
;; (require-package 'powerline)

;; Special case: Auctex is loaded with 'tex
;; (safe-install 'auctex)
;; (require 'tex)

;; --- deft --------------------------------------------------------------------

(require-package 'deft)
(setq deft-directory "~/Dropbox/org/")
(setq deft-extension "org")
(setq deft-use-filename-as-title t)
(setq deft-text-mode 'org-mode)
(global-set-key (kbd "<f9>") 'deft)
(setq deft-use-filename-as-title t)


;; --- evil -------------------------------------------------------------------

(setq evil-want-C-u-scroll t)
(setq evil-default-cursor '(t "#e0e0e0"))

(require-package 'evil)
(evil-mode 1)


;; --- surround ---------------------------------------------------------------

(require-package 'surround)
(global-surround-mode 1)


;; --- flx-ido ----------------------------------------------------------------

(require-package 'flx-ido)
(ido-mode 1)
(ido-everywhere 1)
(flx-ido-mode 1)
(setq ido-enable-flex-matching t)
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
(setq term-unbind-key-list '("C-z" "C-x" "C-c" "C-y" "<ESC>"
                             "C-h" "C-l" "C-k" "C-j"))


;; --- project-explorer -------------------------------------------------------

(require-package 'project-explorer)
(setq evil-emacs-state-modes
      (car '('project-explorer-mode evil-emacs-state-modes)))
(setq pe/width 23)


;; --- git-gutter-fringe ------------------------------------------------------

(require-package 'git-gutter-fringe)
(global-git-gutter-mode t)


;; --- yasnippet --------------------------------------------------------------

(require-package 'yasnippet)
(yas-global-mode 1)


;; --- company-mode -----------------------------------------------------------

(require-package 'company)
(global-company-mode t)
(setq company-idle-delay 0.2)


;; --- flycheck ---------------------------------------------------------------

(require-package 'flycheck)
(add-hook 'after-init-hook #'global-flycheck-mode)


;; --- perspective ------------------------------------------------------------

(require-package 'perspective)
(add-hook 'after-init-hook #'(lambda () (persp-mode 1)))


;; --- magit ------------------------------------------------------------------

(require-package 'magit)
;(require-package 'magit-filenotify)
(global-set-key (kbd "C-x g") 'magit-status)


;; --- Guide Key ---------------------------------------------------------------

(require-package 'guide-key)
(setq guide-key/guide-key-sequence '("C-x", "C-c"))
(setq guide-key/recursive-key-sequence-flag t)
(guide-key-mode 1)


;; ----------------------------------------------------------------------------
;; --- manual -----------------------------------------------------------------
;; ----------------------------------------------------------------------------

(add-to-list 'load-path "~/.emacs.d/lisp/")


;; ----------------------------------------------------------------------------
;; --- Appearance -------------------------------------------------------------
;; ----------------------------------------------------------------------------

;; Theme
(require-package 'zenburn-theme)
(load-theme 'zenburn t)

;; Font
(add-to-list 'default-frame-alist '(font . "Menlo-11"))

;; Cleaner Mac fullscreen
;(setq ns-use-native-fullscreen nil)

;; Numbering and paren matching
(global-linum-mode 1)
(column-number-mode t)
(show-paren-mode 1)

(require-package 'highlight-current-line)
(global-hl-line-mode)
(setq highlight-current-line-globally t)
(setq highlight-current-line-high-faces nil)
(setq highlight-current-line-whole-line nil)
(setq hl-line-face (quote highlight))


;; Minimal Emacs
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(unless (display-graphic-p) (menu-bar-mode -1))
(set-default 'truncate-lines t)
(setq inhibit-startup-message t)
(setq inhibit-splash-screen t)
(setq ring-bell-function 'ignore)
(defalias 'yes-or-no-p 'y-or-n-p)  ; y versus yes

;; transparency
(set-frame-parameter (selected-frame) 'alpha '(90 90))
(add-to-list 'default-frame-alist '(alpha 90 90))

;; initial frame size
(when window-system (set-frame-size (selected-frame) 141 53))

;; gdb battlestation mode
(setq gdb-many-windows t)


;; ----------------------------------------------------------------------------
;; --- Keys -------------------------------------------------------------------
;; ----------------------------------------------------------------------------

;; leader
(define-key evil-normal-state-map (kbd ",w") 'save-buffer)
(define-key evil-normal-state-map (kbd ",e") (kbd "C-x C-e"))
(define-key evil-normal-state-map (kbd ",bd") 'kill-this-buffer)
(define-key evil-normal-state-map (kbd ",P") 'package-list-packages)
(define-key evil-normal-state-map (kbd ",h") 'help-for-help-internal)

(define-key evil-normal-state-map (kbd "SPC o") 'imenu)
(define-key evil-normal-state-map (kbd "SPC b") 'switch-to-buffer)
(define-key evil-normal-state-map (kbd "SPC k") 'ido-kill-buffer)
(define-key evil-normal-state-map (kbd "SPC p") 'projectile-find-file)

;; Find
(define-key evil-normal-state-map ",f" 'projectile-find-file)
(define-key evil-normal-state-map (kbd "SPC f") 'ido-find-file)

(require-package 'helm)
(define-key evil-normal-state-map (kbd "SPC e") 'helm-recentf)
(define-key evil-normal-state-map (kbd "SPC t") 'helm-etags-select)
(define-key evil-normal-state-map (kbd "SPC y") 'helm-show-kill-ring)

;; Search across files
(require-package 'helm-swoop)
(define-key evil-normal-state-map (kbd "SPC l") 'helm-swoop)
(define-key evil-normal-state-map (kbd "SPC L") 'helm-multi-swoop)

;; Apps
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
(define-key evil-visual-state-map (kbd "SPC SPC") 'smex)
(define-key evil-normal-state-map (kbd "SPC SPC") 'smex)
(define-key evil-normal-state-map ",t" 'multi-term)
(define-key evil-normal-state-map ",p" 'project-explorer-open)

;; Perspective
(define-key evil-normal-state-map (kbd "SPC wb") 'persp-add-buffer)
(define-key evil-normal-state-map (kbd "SPC wc") 'persp-rename)
(define-key evil-normal-state-map (kbd "SPC wi") 'persp-import)
(define-key evil-normal-state-map (kbd "SPC wk") 'persp-kill)
(define-key evil-normal-state-map (kbd "SPC wn") 'persp-new)
(define-key evil-normal-state-map (kbd "SPC wr") 'persp-remove-buffer)
(define-key evil-normal-state-map (kbd "SPC ws") 'persp-switch)

;; Splits
(define-key evil-normal-state-map ",s" 'evil-window-split)
(define-key evil-normal-state-map ",v" 'evil-window-vsplit)
(when (fboundp 'windmove-default-keybindings) (windmove-default-keybindings))
(global-set-key (kbd "C-h")  'windmove-left)
(global-set-key (kbd "C-l") 'windmove-right)
(global-set-key (kbd "C-k")    'windmove-up)
(global-set-key (kbd "C-j")  'windmove-down)
(global-set-key (kbd "C-q")  'delete-window)

;; Buffers
(define-key evil-normal-state-map "\C-p" nil)
(global-set-key (kbd "C-p") 'previous-buffer)
(define-key evil-normal-state-map "\C-n" nil)
(global-set-key (kbd "C-n") 'next-buffer)

;; Navigations
(define-key evil-normal-state-map (kbd "Y") (kbd "y$"))

(require-package 'evil-matchit)
(define-key evil-normal-state-map "%" 'evilmi-jump-items)

(require-package 'key-chord)
(key-chord-mode 1)
(setq key-chord-two-keys-delay 0.3)
(key-chord-define evil-insert-state-map "jk" 'evil-normal-state)
(key-chord-define evil-insert-state-map "kj" 'evil-normal-state)

(require-package 'ace-jump-mode)
(key-chord-define evil-normal-state-map "jc" 'ace-jump-char-mode)
(key-chord-define evil-normal-state-map "jl" 'ace-jump-line-mode)
(key-chord-define evil-normal-state-map "jw" 'ace-jump-word-mode)


;; Comments
(define-key evil-visual-state-map (kbd ",cc") 'comment-region)
(define-key evil-visual-state-map (kbd ",cb") 'comment-box)
(define-key evil-visual-state-map (kbd ",cu") 'uncomment-region)

;; Bindings from vim-unimpaired
(define-key evil-normal-state-map (kbd "[ e") (kbd "ddkP"))
(define-key evil-normal-state-map (kbd "] e") (kbd "ddp"))
(define-key evil-normal-state-map (kbd "[ q") 'previous-error)
(define-key evil-normal-state-map (kbd "] q") 'next-error)

;; escape minibuffer
(defun minibuffer-keyboard-quit ()
  "Abort recursive edit.
In Delete Selection mode, if the mark is active, just deactivate it;
then it takes a second \\[keyboard-quit] to abort the minibuffer."
  (interactive)
  (if (and delete-selection-mode transient-mark-mode mark-active)
      (setq deactivate-mark  t)
    (when (get-buffer "*Completions*") (delete-windows-on "*Completions*"))
    (abort-recursive-edit)))
(define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)

;; ----------------------------------------------------------------------------
;; --- formatting -------------------------------------------------------------
;; ----------------------------------------------------------------------------

(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq indent-line-function 'insert-tab)

;; -----------------------------------------------------------------------------
;; --- Language Specific -------------------------------------------------------
;; -----------------------------------------------------------------------------

;; --- C -----------------------------------------------------------------------
(require 'cc-mode)
(setq c-default-style "bsd" c-basic-offset 4)
(c-set-offset 'case-label '+)
(define-key c-mode-base-map (kbd "RET") 'newline-and-indent)

;; --- Clojure -----------------------------------------------------------------
(require-package 'clojure-mode)
(add-hook 'clojure-mode-hook 'paredit-mode)

;; --- Python ------------------------------------------------------------------


(require-package 'python-mode)

; use IPython
(setq-default py-shell-name "ipython")
(setq-default py-which-bufname "IPython")
; use the wx backend, for both mayavi and matplotlib
(setq py-python-command-args
  '("--gui=wx" "--pylab=wx" "-colors" "Linux"))
(setq py-force-py-shell-name-p t)

; switch to the interpreter after executing code
(setq py-shell-switch-buffers-on-execute-p t)
(setq py-switch-buffers-on-execute-p t)
; don't split windows
(setq py-split-windows-on-execute-p nil)
; try to automagically figure out indentation
(setq py-smart-indentation t)

;; (require-package 'company-jedi)
;; (add-to-list 'company-backends 'company-jedi)
;; (add-hook 'python-mode-hook 'company-jedi-start)
;; (setq company-jedi-python-bin "python3")
(require-package 'jedi)
(add-hook 'python-mode-hook 'jedi:setup)
(setq jedi:setup-keys t)
(setq jedi:complete-on-dot t)

(provide 'init.el)
