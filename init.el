;; ----------------------------------------------------------------------------
;; --- Environment ------------------------------------------------------------
;; ----------------------------------------------------------------------------
(when (< emacs-major-version 24)
  (error "This setup requires Emacs v24, or higher. You have: v%d"
         emacs-major-version))

(add-to-list 'load-path "/usr/texbin")
(add-to-list 'exec-path "/usr/local/bin")

;; Backup files
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))
(setq make-backup-files nil)


;; ----------------------------------------------------------------------------
;; --- Packages ---------------------------------------------------------------
;; ----------------------------------------------------------------------------

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


;; ----------------------------------------------------------------------------
;; --- Navigation -------------------------------------------------------------
;; ----------------------------------------------------------------------------

;; --- deft -------------------------------------------------------------------
; Use to do quick searching through org files

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


;; --- helm -------------------------------------------------------------------
(require-package 'helm)
(setq helm-c-locate-command
      (case system-type
        ('gnu/linux "locate -i -r %s")
        ('berkeley-unix "locate -i %s")
        ('windows-nt "es %s")
        ('darwin "mdfind -name %s %s")
        (t "locate %s"))
      )


;; --- projectile -------------------------------------------------------------
(require-package 'projectile)
(projectile-global-mode)


;; --- project-explorer -------------------------------------------------------
(require-package 'project-explorer)
(setq pe/width 23)


;; --- Window Number ----------------------------------------------------------
;; Switch to # window with Command-#
(require-package 'window-number)
(window-number-mode 1)
(window-number-define-keys window-number-mode-map "s-")


;; --- Winner Mode ------------------------------------------------------------
;; Undo/redo window configuration with C-c left/right
(winner-mode 1)


;; --- autopair ---------------------------------------------------------------
(require-package 'autopair)


;; --- surround ---------------------------------------------------------------
; Emulate change surround from vim  (cs'" etc.)
(require-package 'surround)
(global-surround-mode 1)


;; --- perspective ------------------------------------------------------------
;; (require-package 'perspective)
;; (add-hook 'after-init-hook #'(lambda () (persp-mode 1)))

;; (require-package 'persp-mode)
;;  ;; switch off animation of restoring window configuration
;; (setq wg-morph-on nil)


;; ----------------------------------------------------------------------------
;; --- Completion -------------------------------------------------------------
;; ----------------------------------------------------------------------------

;; --- company-mode -----------------------------------------------------------
; Autocompletion
(require-package 'company)
(global-company-mode t)
(setq company-idle-delay 0.2)

(define-key company-active-map (kbd "\C-n") 'company-select-next)
(define-key company-active-map (kbd "\C-p") 'company-select-previous)
(define-key company-active-map (kbd "\C-d") 'company-show-doc-buffer)
(define-key company-active-map (kbd "<tab>") 'company-complete)


;; --- yasnippet --------------------------------------------------------------
(require-package 'yasnippet)
(yas-global-mode 1)


;; ----------------------------------------------------------------------------
;; --- Matching ---------------------------------------------------------------
;; ----------------------------------------------------------------------------

;; --- flx-ido ----------------------------------------------------------------
(require-package 'flx-ido)
(ido-mode 1)
(ido-everywhere 1)
(flx-ido-mode 1)
(setq ido-enable-flex-matching t)
(setq ido-use-faces nil)


;; --- ido-ubiquitous ---------------------------------------------------------
(require-package 'ido-ubiquitous)
(ido-ubiquitous-mode)


;; --- smex -------------------------------------------------------------------
(require-package 'smex)
(smex-initialize)


;; ----------------------------------------------------------------------------
;; --- Tools ------------------------------------------------------------------
;; ----------------------------------------------------------------------------

;; --- magit ------------------------------------------------------------------
(require-package 'magit)
;(require-package 'magit-filenotify)
(global-set-key (kbd "C-x g") 'magit-status)


;; --- flycheck ---------------------------------------------------------------
; Inline errors in gutter
(require-package 'flycheck)
(add-hook 'after-init-hook #'global-flycheck-mode)


;; ----------------------------------------------------------------------------
;; --- Misc settings ----------------------------------------------------------
;; ----------------------------------------------------------------------------

;; --- Number of things to save -----------------------------------------------
(setq kill-ring-max 500)
(setq recentf-max-menu-items 100)


;; --- Delete trailing whitespace on save -------------------------------------
(add-hook 'before-save-hook 'delete-trailing-whitespace)


;; ----------------------------------------------------------------------------
;; --- Manual -----------------------------------------------------------------
;; ----------------------------------------------------------------------------

(add-to-list 'load-path "~/.emacs.d/lisp/")


;; ----------------------------------------------------------------------------
;; --- Appearance -------------------------------------------------------------
;; ----------------------------------------------------------------------------

;; --- Powerline --------------------------------------------------------------
(require-package 'powerline)
(powerline-default-theme)


;; --- git-gutter-fringe ------------------------------------------------------
(require-package 'git-gutter-fringe)
(global-git-gutter-mode t)
(setq git-gutter:verbosity 0)


;; --- Theme ------------------------------------------------------------------
;; (require-package 'color-theme)
;; (require-package 'color-theme-molokai)
;; (color-theme-molokai)
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

;; Highlight lines over 80 characters
 (require-package 'column-marker)
 (column-marker-1 80)
 (column-marker-2 100)

;; Show EOL and tab characters
;; (require-package 'whitespace)
;; (setq whitespace-style '(face lines-tail tabs tab-mark newline-mark trailing))
;; (setq whitespace-display-mappings
;;           '((newline-mark ?\n   [?\x00AC ?\n] [?$ ?\n]) ; end-of-line
;;             ))
;;  ¬▸

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
(set-frame-parameter (selected-frame) 'alpha '(99 99))
(add-to-list 'default-frame-alist '(alpha 99 99))

;; initial frame size
(when window-system (set-frame-size (selected-frame) 141 53))

;; ----------------------------------------------------------------------------
;; --- Keys -------------------------------------------------------------------
;; ----------------------------------------------------------------------------

;; leader
(define-key evil-normal-state-map (kbd ",w") 'save-buffer)
(define-key evil-normal-state-map (kbd ",e") (kbd "C-x C-e"))
(define-key evil-normal-state-map (kbd ",bd") 'kill-this-buffer)
(define-key evil-normal-state-map (kbd ",P") 'package-list-packages)
(define-key evil-normal-state-map (kbd ",h") 'help-for-help-internal)

(define-key evil-normal-state-map (kbd "SPC i") 'imenu)
(define-key evil-normal-state-map (kbd "SPC b") 'switch-to-buffer)
(define-key evil-normal-state-map (kbd "SPC k") 'ido-kill-buffer)

;; Find
(require-package 'helm-projectile)
(define-key evil-normal-state-map (kbd "SPC p") 'helm-projectile)
(define-key evil-normal-state-map (kbd "SPC f") 'ido-find-file)
(define-key evil-normal-state-map (kbd "SPC s") 'helm-for-files)  ;;search

;; "everything"
(define-key evil-normal-state-map (kbd "SPC e") 'helm-mini)
(define-key evil-normal-state-map (kbd "SPC t") 'helm-etags-select)
(define-key evil-normal-state-map (kbd "SPC y") 'helm-show-kill-ring)

;; Search across files
;; Also M-i during C-s search to switch to helm
(require-package 'helm-swoop)
(define-key evil-normal-state-map (kbd "SPC l") 'helm-swoop)
(define-key evil-normal-state-map (kbd "SPC L") 'helm-multi-swoop-all)

 (require-package 'helm-ag)
 (define-key evil-normal-state-map (kbd "SPC a") 'helm-ag)

;; Enable folding
(require-package 'fold-dwim)
(fold-dwim-outline-nested-p)
(define-key evil-normal-state-map (kbd ";")      'fold-dwim-toggle)
(define-key evil-normal-state-map (kbd ",fh")    'fold-dwim-hide-all)
(define-key evil-normal-state-map (kbd ",fs")  'fold-dwim-show-all)

;; Apps
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
(define-key evil-visual-state-map (kbd "SPC SPC") 'smex)
(define-key evil-normal-state-map (kbd "SPC SPC") 'smex)
(define-key evil-normal-state-map ",t" 'eshell)
(define-key evil-normal-state-map ",p" 'project-explorer-open)

;; Perspective
(define-key evil-normal-state-map (kbd "SPC wb") #'persp-add-buffer)
(define-key evil-normal-state-map (kbd "SPC wc") #'persp-rename)
(define-key evil-normal-state-map (kbd "SPC wi") #'persp-import-buffers)
(define-key evil-normal-state-map (kbd "SPC wk") #'persp-kill)
(define-key evil-normal-state-map (kbd "SPC wr") #'persp-remove-buffer)
(define-key evil-normal-state-map (kbd "SPC ws") #'persp-switch)
(define-key evil-normal-state-map (kbd "SPC wt")
  #'persp-temporarily-display-buffer)
(define-key evil-normal-state-map (kbd "SPC ww") #'persp-save-state-to-file)
(define-key evil-normal-state-map (kbd "SPC wl") #'persp-load-state-from-file)

;; Splits
(define-key evil-normal-state-map ",s" 'evil-window-split)
(define-key evil-normal-state-map ",v" 'evil-window-vsplit)
(when (fboundp 'windmove-default-keybindings) (windmove-default-keybindings))
(global-set-key (kbd "C-<tab>")  'other-window)
(global-set-key (kbd "C-h")  'windmove-left)
(global-set-key (kbd "C-l") 'windmove-right)
(global-set-key (kbd "C-k")    'windmove-up)
(global-unset-key (kbd "C-j"))  ;; Fix lisp mode binding
(global-set-key (kbd "C-j")  'windmove-down)
(global-set-key (kbd "C-q")  'delete-window)

;; Buffers
(define-key evil-normal-state-map "\C-p" nil)
(global-set-key (kbd "C-p") 'previous-buffer)
(define-key evil-normal-state-map "\C-n" nil)
(global-set-key (kbd "C-n") 'next-buffer)

;; Make Y consistent with D behavior
(define-key evil-normal-state-map (kbd "Y") (kbd "y$"))

;; Jump between matching parens
(require-package 'evil-matchit)
(define-key evil-normal-state-map "%" 'evilmi-jump-items)

;; Exit insert mode
(require-package 'key-chord)
(key-chord-mode 1)
(setq key-chord-two-keys-delay 0.1)
(key-chord-define evil-insert-state-map "jk" 'evil-normal-state)
(key-chord-define evil-insert-state-map "kj" 'evil-normal-state)

;; Jump to char/line/word
(require-package 'ace-jump-mode)
(define-key evil-normal-state-map "gc" 'ace-jump-char-mode)
(define-key evil-normal-state-map "gl" 'ace-jump-line-mode)
(define-key evil-normal-state-map "gw" 'ace-jump-word-mode)


;; Comments
(define-key evil-visual-state-map (kbd ",cc") 'comment-region)
(define-key evil-visual-state-map (kbd ",cb") 'comment-box)
(define-key evil-visual-state-map (kbd ",cu") 'uncomment-region)

;; Bindings from vim-unimpaired
(define-key evil-normal-state-map (kbd "[ e") (kbd "ddkP"))
(define-key evil-normal-state-map (kbd "] e") (kbd "ddp"))
(define-key evil-normal-state-map (kbd "[ q") 'previous-error)
(define-key evil-normal-state-map (kbd "] q") 'next-error)
(define-key evil-normal-state-map (kbd "[ g") 'git-gutter:previous-hunk)
(define-key evil-normal-state-map (kbd "] g") 'git-gutter:next-hunk)

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
;; --- Formatting -------------------------------------------------------------
;; ----------------------------------------------------------------------------

(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq indent-line-function 'insert-tab)


;; ----------------------------------------------------------------------------
;; --- Language Specific ------------------------------------------------------
;; ----------------------------------------------------------------------------

;; --- C ----------------------------------------------------------------------

(require 'cc-mode)
(setq c-default-style "bsd" c-basic-offset 4)
(c-set-offset 'case-label '+)
(define-key c-mode-base-map (kbd "RET") 'c-indent-new-comment-line)

;; gdb battlestation mode
(setq gdb-many-windows t)


;; --- Clojure ----------------------------------------------------------------

(require-package 'cider)  ; Repl
(require-package 'clojure-mode)

(require-package 'paredit)
(add-hook 'clojure-mode-hook 'paredit-mode)

;; --- Haskell ----------------------------------------------------------------

(add-to-list 'exec-path "~/.cabal/bin")
(require-package 'haskell-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
(require-package 'flycheck-hdevtools)
(eval-after-load 'flycheck '(require 'flycheck-hdevtools))
(eval-after-load "haskell-mode"
  '(progn
    (define-key haskell-mode-map (kbd "C-c C-b") 'haskell-interactive-switch)
    (define-key haskell-mode-map (kbd "C-c C-z") 'haskell-interactive-switch)
    (define-key haskell-mode-map (kbd "C-c C-l") 'haskell-process-load-file)
    (define-key haskell-mode-map (kbd "C-c C-t") 'haskell-process-do-type)
    (define-key haskell-mode-map (kbd "C-c C-i") 'haskell-process-do-info)
    (define-key haskell-mode-map (kbd "C-c C-e") 'inferior-haskell-send-decl)
    ;; Unbind default keys
    (define-key haskell-mode-map (kbd "C-c M-.") nil)
    (define-key haskell-mode-map (kbd "C-c C-d") nil)
    (define-key haskell-mode-map (kbd "C-x C-d") nil)))

;; --- Javascript -------------------------------------------------------------
(setq js-indent-level 2)  ;; set JavaScript indent to two spaces

;; --- Java -------------------------------------------------------------------

;; --- Python -----------------------------------------------------------------

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
;(setq py-split-windows-on-execute-p nil)
; try to automagically figure out indentation
(setq py-smart-indentation t)


;; Autocompletion
(require-package 'jedi)
(add-hook 'python-mode-hook 'jedi:setup)
(setq jedi:setup-keys t)
(setq jedi:complete-on-dot t)


;; --- Tex --------------------------------------------------------------------

;; Special case: Auctex is loaded with 'tex
(safe-install 'auctex)
(require 'tex)
(getenv "PATH")
(setenv "PATH"
        (concat
         "/usr/texbin:/usr/local/bin:"
         (getenv "PATH")))


;; ----------------------------------------------------------------------------
;; --- Org Mode ---------------------------------------------------------------
;; ----------------------------------------------------------------------------

(require-package 'org)

(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c b") 'org-iswitchb)

(setq org-directory "~/Dropbox/org")
(setq org-mobile-inbox-for-pull "~/Dropbox/org/inbox.org")
(setq org-mobile-directory "~/Dropbox/Apps/MobileOrg")

(global-set-key (kbd "C-c c") 'org-capture)
(setq org-default-notes-file (concat org-directory "/notes.org"))
(setq org-capture-templates
      '(("t" "Todo" entry (file+headline (concat org-directory "/todo.org") "Todo")
             "* TODO %?\n  %i\n")
        ("w" "Work" entry (file+headline (concat org-directory "/work.org") "Todo")
             "* TODO %?\n  %i\n")
        ("l" "Learn" entry (file+headline (concat org-directory "/learn.org") "Learn")
             "* %?\nEntered on %U\n  %i\n")
        ("f" "Someday" entry (file+headline (concat org-directory "/todo.org") "Someday")
             "* %?\nEntered on %U\n  %i\n")
        ("c" "Consume" entry (file+headline (concat org-directory "/consume.org") "New")
             "* %?\n")
        ("i" "Ideas" entry (file+headline (concat org-directory "/ideas.org") "New")
             "* %?\nEntered on %U\n  %i\n")
        ("s" "Scratch" entry (file+headline (concat org-directory "/scratch.org") "Scratch")
             "* %?\nEntered on %U\n")
        ("d" "Dream" entry (file+headline (concat org-directory "/journal.org") "Dreams")
             "* %?\nEntered on %U\n")
        ("j" "Journal" entry (file+datetree (concat org-directory "/journal.org"))
             "* %?\nEntered on %U\n")))

:; Export options
(setq org-export-html-style-include-scripts nil
      org-export-html-style-include-default nil)

;; Set export filename for main agenda
 (setq org-agenda-custom-commands
           '(("X" agenda "" nil ("~/.emacs.d/tmp/agenda.txt"))))

;; Automatically update agenda every 30 minutes
(run-with-timer 0 (* 30 60) 'org-store-agenda-views)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-agenda-files (quote ("~/Dropbox/org/todo.org" "~/Dropbox/org/scratch.org" "~/Dropbox/org/work.org"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(provide 'init.el)

(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
