(menu-bar-mode -1)
(scroll-bar-mode -1)
;;(toggle-scroll-bar -1)
(tool-bar-mode -1) ; Disable tool bar 
(setq inhibit-splash-screen t)
(setq ido-everywhere t)
(setq ido-enable-flex-matching t)
;;(ido-mode t)

(setq nord-uniform-mode-lines t)

;; Set up the visible bell
(setq visible-bell t)
(set-face-attribute 'default nil :font "Fira Code" :height 110)

(column-number-mode)
(global-display-line-numbers-mode t)
; load directory for configuration files for emacs
(add-to-list 'load-path (concat user-emacs-directory "setup-files/"))
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; Disable line numbers for some modes
(dolist (mode '(org-mode-hook
                term-mode-hook
                shell-mode-hook
                treemacs-mode-hook
                eshell-mode-hook
		neotree-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(defun pulse-line (&rest _)
      "Pulse the current line."
      (pulse-momentary-highlight-one-line (point)))

;;(dolist (command '(scroll-up-command scroll-down-command
;;                   recenter-top-bottom other-window))
;;  (advice-add command :after #'pulse-line))

(global-set-key (kbd "M-<up>") 
  (lambda () 
    (interactive) 
    (pulse-line)))

;;(require 'package)
(setq package-archives
      '(("melpa" . "https://melpa.org/packages/")
	("org" . "https://orgmode.org/elpa/")
	("elpa" . "https://elpa.gnu.org/packages/")
	;("melpa-stable" . "https://stable.melpa.org/packages/")
	))
;;The line below is needed!
(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)


;;Load $PATH variable
(use-package exec-path-from-shell
  :ensure t)

(exec-path-from-shell-initialize)
;;(when (memq window-system '(mac ns x))
;;  (exec-path-from-shell-initialize))
;;Uncomment to print path
;;(getenv "PATH")


;; Let the desktop background show through
;; (set-frame-parameter (selected-frame) 'alpha '(97 . 100))
;; (add-to-list 'default-frame-alist '(alpha . (90 . 90)))

(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 1))

(use-package ivy
  :diminish
  :config
  (ivy-mode 1))

(use-package all-the-icons)

;;(use-package projectile
;;  :ensure t
;;  :config
;;  (define-key projectile-mode-map (kbd "C-x p") 'projectile-command-map)
;;  (projectile-mode +1))

;;(use-package treemacs
;;  :demand t
;;  :config
;;  (setq treemacs-follow-after-init t
;;        treemacs-width 30
;;	treemacs-width-increment 1
;;        treemacs-indentation 1
;;        treemacs-follow-after-init t
;;        treemacs-recenter-after-file-follow nil
;;        treemacs-silent-refresh t
;;        treemacs-silent-filewatch t
;;        treemacs-change-root-without-asking t
;;        treemacs-sorting 'alphabetic-desc
;;        treemacs-show-hidden-files t
;;        treemacs-never-persist nil
;;	;; Do not add treemacs as part of window cycles
;;        treemacs-is-never-other-window t
;;        ;;treemacs-indentation-string (propertize " ⫶ " 'face 'font-lock-comment-face)
;;	)
;;  
;;  ;;	(setq treemacs-follow-after-init t
;;  ;;				treemacs-is-never-other-window t
;;  ;;				treemacs-width 20)
;;  (treemacs-follow-mode t)
;;  (treemacs-filewatch-mode t)
;;  (treemacs-git-mode 'simple)
;;  (treemacs-fringe-indicator-mode t)
;;;;  :hook (after-init . treemacs)
;;  :bind
;;  (:map global-map
;;	([f8]   . treemacs)
;;	("C-<f8>" . treemacs-select-window))
;;)

;;(use-package neotree)
;;(require 'neotree)
;;(global-set-key [f8] 'neotree-toggle)

(use-package kaolin-themes
  :config
;;  (load-theme 'kaolin-dark t)
  (load-theme 'kaolin-light t)
;;  (load-theme 'kaolin-valley-light t)
;;  (load-theme 'kaolin-aurora t)
;;  (load-theme 'kaolin-bubblegum t)
;;  (load-theme 'kaolin-eclipse t)
;;  (load-theme 'kaolin-galaxy t)
;;  (load-theme 'kaolin-ocean t)
;;  (load-theme 'kaolin-temple t)
;;  (load-theme 'kaolin-valley-dark t)
  (kaolin-treemacs-theme)
)

(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 15)))

;; ---- Put backup files neatly away                                                 
(let ((backup-dir "~/tmp/emacs/backups")
      (auto-saves-dir "~/tmp/emacs/auto-saves/"))
  (dolist (dir (list backup-dir auto-saves-dir))
    (when (not (file-directory-p dir))
      (make-directory dir t)))
  (setq backup-directory-alist `(("." . ,backup-dir))
        auto-save-file-name-transforms `((".*" ,auto-saves-dir t))
        auto-save-list-file-prefix (concat auto-saves-dir ".saves-")
        tramp-backup-directory-alist `((".*" . ,backup-dir))
        tramp-auto-save-directory auto-saves-dir))

(setq backup-by-copying t    ; Don't delink hardlinks                           
      delete-old-versions t  ; Clean up the backups                             
      version-control t      ; Use version numbers on backups,                  
      kept-new-versions 5    ; keep some new versions                           
      kept-old-versions 2)   ; and some old ones, too
;; ---- end backup

;;(use-package lsp-mode
;;  :ensure t
;;  :bind (:map lsp-mode-map
;;              ("C-c d" . lsp-describe-thing-at-point)
;;              ("C-c a" . lsp-execute-code-action))
;;  :bind-keymap ("C-c l" . lsp-command-map)  
;;  :config
;;  (lsp-enable-which-key-integration t))
;;
;;(use-package company
;;  :ensure t
;;  :hook ((emacs-lisp-mode . (lambda ()
;;                              (setq-local company-backends '(company-elisp))))
;;         (emacs-lisp-mode . company-mode))
;;  :config 
;;  ;;  (company-keymap--unbind-quick-access company-active-map)
;;  (company-tng-configure-default)
;;  :custom
;;  (company-minimum-prefix-length 1)
;;  (company-idle-delay 0.1)
;;  (company-show-quick-access "off")  
;;  (company-quick-access-hint-function (lambda (param) " unknown")))

;; company-quick-access-hint-function  
;;(use-package company-box
;;  :hook (company-mode . company-box-mode))

(use-package flycheck
  :ensure t)

;;;;; Go
;;(use-package go-mode
;;  :ensure t
;;  :hook ((go-mode . lsp-deferred)
;;         (go-mode . company-mode))
;;  :bind (:map go-mode-map
;;              ("<f6>"  . gofmt)
;;              ("C-c 6" . gofmt))
;;  :config
;;  (require 'lsp-go)
;;  ;; https://github.com/golang/tools/blob/master/gopls/doc/analyzers.md
;;  (setq lsp-go-analyses
;;        '((fieldalignment . t)
;;          (nilness        . t)
;;          (unusedwrite    . t)
;;          (unusedparams   . t)))
;;  ;; GOPATH/bin
;;  (add-to-list 'exec-path "~/go/bin")
;;  ;; requires goimports to be installed
;;  (setq gofmt-command "goimports"))
;;
;;(global-set-key (kbd "<f5>") #'recompile)

(use-package magit
  :commands magit-status
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))


;; Org mode configurations
;;https://github.com/zzamboni/dot-emacs/blob/master/init.org

;; treesitter configuration
(setq treesit-language-source-alist
   '((bash "https://github.com/tree-sitter/tree-sitter-bash")
     (cmake "https://github.com/uyha/tree-sitter-cmake")
     (css "https://github.com/tree-sitter/tree-sitter-css")
     (elisp "https://github.com/Wilfred/tree-sitter-elisp")
     (go "https://github.com/tree-sitter/tree-sitter-go" "master" "src")
     (html "https://github.com/tree-sitter/tree-sitter-html")
     (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
     (json "https://github.com/tree-sitter/tree-sitter-json")
     (make "https://github.com/alemuller/tree-sitter-make")
     (markdown "https://github.com/ikatyang/tree-sitter-markdown")
     (python "https://github.com/tree-sitter/tree-sitter-python")
     (toml "https://github.com/tree-sitter/tree-sitter-toml")
     (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
     (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
     (yaml "https://github.com/ikatyang/tree-sitter-yaml")
     (java "https://github.com/tree-sitter/tree-sitter-java")))

;;Ensure  the langauages are available by running below
;;(mapc #'treesit-install-language-grammar (mapcar #'car treesit-language-source-alist))

(setq major-mode-remap-alist
 '((yaml-mode . yaml-ts-mode)
   (bash-mode . bash-ts-mode)
   (js2-mode . js-ts-mode)
   (typescript-mode . typescript-ts-mode)
   (json-mode . json-ts-mode)
   (css-mode . css-ts-mode)
   (go-mode . go-ts-mode)   
   (python-mode . python-ts-mode)))

(setq treesit-load-name-override-list '((js "libtree-sitter-gomod" "tree_sitter_go")))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(doom-modeline kaolin-themes all-the-icons ivy which-key flycheck exec-path-from-shell)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
