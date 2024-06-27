;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;; This is for emacs 29;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


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

;;(set-face-attribute 'default nil :font "Fira Code" :height 110)
(set-face-attribute 'default nil :font "JetBrains Mono" :height 110)
;;(set-face-attribute 'default nil :font "Inconsolata" :height 122)

(column-number-mode)
(global-display-line-numbers-mode t)
; load directory for configuration files for emacs
(add-to-list 'load-path (concat user-emacs-directory "setup-files/"))
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;;(set-frame-parameter nil 'alpha-background 70)
;;(add-to-list 'default-frame-alist '(alpha-background . 70))

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
  :ensure t
  :config
  (exec-path-from-shell-initialize)
  )

;;(when (memq window-system '(mac ns x))
;;  (exec-path-from-shell-initialize))
;;Uncomment to print path
;;(getenv "PATH")


;; Let the desktop background show through
;; (set-frame-parameter (selected-frame) 'alpha '(97 . 100))
;; (add-to-list 'default-frame-alist '(alpha . (90 . 90)))

(use-package dashboard
  :ensure t
  :config
  (dashboard-setup-startup-hook)
  (setq dashboard-banner-logo-title "Emacs ❤️"))

(use-package company
  :ensure t
  :config
  (add-hook 'after-init-hook 'global-company-mode)
  (company-quickhelp-mode))

(use-package company-quickhelp :ensure t)

(use-package which-key
  :ensure t
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.5))

(use-package ivy
  :ensure t
  :diminish
  :config
  (ivy-mode 1))

(use-package all-the-icons
  :ensure t)

(use-package yasnippet
  :ensure t
  :config
  (setq yas-snippet-dirs '("~/.emacs.d29/.emacs.d/yassnippets"))
  (yas-global-mode 1))

;;Sticking to projectile for now
;;(require 'project)

;;Projectile
;;Recursive discovery is configured by specifying the search depth in a cons cell
;;(setq projectile-project-search-path '("~/projects/" "~/work/" ("~/github" . 1)))
(use-package projectile
  :ensure t
  :init
  (projectile-mode +1)
  :bind
  (:map projectile-mode-map
        ("s-p" . projectile-command-map)
        ("C-c p" . projectile-command-map))
  :config
  (setq projectile-project-search-path '(("~/workspace/sources/" . 1) )))


;; The line below is needed to get svg working with treemacs. Still and issue with emacs29
(setq image-types (cons 'svg image-types))

(use-package treemacs
  :ensure t
  :demand t
  :config
  (setq treemacs-follow-after-init t
        treemacs-width 30
	treemacs-width-increment 1
        treemacs-indentation 1
        treemacs-follow-after-init t
        treemacs-recenter-after-file-follow nil
        treemacs-silent-refresh t
        treemacs-silent-filewatch t
        treemacs-change-root-without-asking t
        treemacs-sorting 'alphabetic-desc
        treemacs-show-hidden-files t
        treemacs-never-persist nil
	;; Do not add treemacs as part of window cycles
        treemacs-is-never-other-window t
        ;;treemacs-indentation-string (propertize " ⫶ " 'face 'font-lock-comment-face)
	)
  
  ;;	(setq treemacs-follow-after-init t
  ;;				treemacs-is-never-other-window t
  ;;				treemacs-width 20)
  (treemacs-follow-mode t)
  (treemacs-filewatch-mode t)
  (treemacs-git-mode 'simple)
  (treemacs-fringe-indicator-mode t)
;;  :hook (after-init . treemacs)
  :bind
  (:map global-map
	([f8]   . treemacs)
	("C-<f8>" . treemacs-select-window))
)

;;(use-package neotree)
;;(require 'neotree)
;;(global-set-key [f8] 'neotree-toggle)

(use-package kaolin-themes
  :ensure t
  :config
;;  (load-theme 'kaolin-dark t)
;;  (load-theme 'kaolin-light t)
;;  The best light theme below!  
;;  (load-theme 'kaolin-valley-light t)
  (load-theme 'kaolin-aurora t)
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

(use-package flycheck
  :ensure t)

(use-package magit
  :ensure t
  :commands magit-status
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))


(use-package gptel
  :ensure t)


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

(setq treesit-load-name-override-list '((js "tree-sitter-gomod" "tree-sitter-go")))



;; Does not work with treesit.el which comes with emacs
;;(require 'tree-sitter)
;;(use-package ts-fold
;;  :load-path "/home/rahul/workspace/emacspkgs/ts-fold")

(use-package eshell-toggle
  :ensure t
  :custom
  (eshell-toggle-size-fraction 3)
  (eshell-toggle-use-projectile-root t)
  (eshell-toggle-run-command nil)
  (eshell-toggle-init-function #'eshell-toggle-init-ansi-term)
  :bind
  ("C-c t" . eshell-toggle))

;;For Python LSP support install a pylsp server as shown below:
;;sudo apt search pylsp
;;sudo apt install python3-pylsp python3-pylsp-isort python3-pylsp-black

;; Ensure eglot while editing go files
(use-package eglot
  :ensure t
  :defer t
  :hook ((go-ts-mode . eglot-ensure) (python-ts-mode . eglot-ensure)))

;; The below hack was to ensure elgot detects the right java root, doesnt work though!
;;(with-eval-after-load 'eglot
;;  (let ((cache
;;         (expand-file-name (md5 (project-root (eglot--current-project)))
;;                           (locate-user-emacs-file
;;                            "eglot-eclipse-jdt-cache"))))
;;    (add-to-list 'eglot-server-programs
;;                 `(java-mode "jdtls" "-data" ,cache))))

(use-package writeroom-mode
  :ensure t)

(use-package org
  :config
  (setq org-startup-indented t)
  (add-hook 'org-mode-hook #'visual-line-mode))

(global-set-key "\C-cr" 'recentf)

;; Not working how
;;(use-package org-bullets
;;  :custom
;;  (org-bullets-bullet-list '("◉" "☯" "○" "☯" "✸" "☯" "✿" "☯" "✜" "☯" "◆" "☯" "▶"))
;;  (org-ellipsis "⤵")
;;  :hook (org-mode . org-bullets-mode))

;;(add-to-list 'eglot-server-programs '((go-mode go-ts-mode) .
;;    ("gopls" :initializationOptions
;;      (:hints (:parameterNames t
;;               :rangeVariableTypes t
;;               :functionTypeParameters t
;;               :assignVariableTypes t
;;               :compositeLiteralFields t
;;               :compositeLiteralTypes t
;;               :constantValues t)))))


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(gptel eglot chatgpt-shell writeroom-mode ts-fold eshell-toggle yasnippet projectile company-quickhelp company magit treemacs doom-modeline kaolin-themes all-the-icons ivy which-key flycheck exec-path-from-shell)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
