;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;; This is for emacs 29/30;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(menu-bar-mode -1)
(scroll-bar-mode -1)
;;;(toggle-scroll-bar -1)
(tool-bar-mode -1) ; Disable tool bar 
(setq inhibit-splash-screen t)
(setq ido-everywhere t)
(setq ido-enable-flex-matching t)
;;(ido-mode t)

(setq nord-uniform-mode-lines t)

;; Set up the visible bell
(setq visible-bell t)
;;(setq-default tab-width 4)
;;(set-face-attribute 'default nil :font "Fira Code" :height 180)
;;(set-face-attribute 'default nil :font "IBM Plex Mono" :height 180)
;;(set-face-attribute 'default nil :font "JetBrains Mono" :height 180)
;;(set-face-attribute 'default nil :font "Nunito" :height 180)
;;(set-face-attribute 'fixed-pitch nil :font "Nunito" :height 180)
;;(set-face-attribute 'default nil :font "Inconsolata" :height 122)

(column-number-mode)
(global-display-line-numbers-mode t)
; load directory for configuration files for emacs
(add-to-list 'load-path (concat user-emacs-directory "setup-files/"))
(add-to-list 'default-frame-alist '(fullscreen . maximized))

(setq custom-file (locate-user-emacs-file "custom.el"))
(load custom-file :no-error-if-file-is-missing)

;;;The following snippet will prevent those buffers from popping up.
;;;They are still available in the buffer list, if they are needed.
(add-to-list 'display-buffer-alist
             '("\\`\\*\\(Warnings\\|Compile-Log\\)\\*\\'"
               (display-buffer-no-window)
               (allow-no-window . t)))


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

(if (eq system-type 'darwin)
    (progn
      (setq mac-command-modifier 'meta)))

;;(require 'package)

(setq package-archives
      '(("melpa" . "https://melpa.org/packages/")
	("org" . "https://orgmode.org/elpa/")
	("elpa" . "https://elpa.gnu.org/packages/")
	("melpa-stable" . "https://stable.melpa.org/packages/")
	("nongnu" . "https://elpa.nongnu.org/nongnu/")
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
  (exec-path-from-shell-initialize))

(setq desktop-path '("~/emacs_save/"))
(desktop-save-mode 1)

;;(when (memq window-system '(mac ns x))
;;  (exec-path-from-shell-initialize))
;;Uncomment to print path
;;(getenv "PATH")

;; eat: Emulate A Terminal (https://codeberg.org/akib/emacs-eat)
(use-package eat
    :preface
    (defun my--eat-open (file)
        "Helper function to open files from eat terminal."
        (interactive)
        (if (file-exists-p file)
                (find-file-other-window file t)
            (warn "File doesn't exist")))
    :init
    (add-to-list 'project-switch-commands '(eat-project "Eat terminal") t)
    (add-to-list 'project-switch-commands '(eat-project-other-window "Eat terminal other window") t)
    (add-to-list 'project-kill-buffer-conditions '(major-mode . eat-mode))
    :config
    (add-to-list 'eat-message-handler-alist (cons "open" 'my--eat-open))
    (setq process-adaptive-read-buffering nil) ; makes EAT a lot quicker!
    (setq eat-term-name "xterm-256color") ; https://codeberg.org/akib/emacs-eat/issues/119"
    (setq eat-kill-buffer-on-exit t)
    (setq eat-shell-prompt-annotation-failure-margin-indicator "")
    (setq eat-shell-prompt-annotation-running-margin-indicator "")
    (setq eat-shell-prompt-annotation-success-margin-indicator "")
    ;; Performance optimizations to reduce flickering
    (setq eat-enable-blinking-text nil)  ; Prevent blinking cursor/text redraws
    (setq eat-enable-alternative-display t)  ; Better screen management
    (setq eat-maximum-latency 0.1)  ; Reduce input latency from default 0.2
    ;; Optimize scrolling behavior
    (add-hook 'eat-mode-hook
              (lambda ()
                (setq-local scroll-margin 0)  ; Reduce scroll-triggered redraws
                (setq-local scroll-conservatively 101)))  ; Smoother scrolling
    ;; Disable transparency in eat buffers to reduce redraw overhead
    (add-hook 'eat-mode-hook
              (lambda ()
                (set-frame-parameter (selected-frame) 'alpha '(100 . 100))))
    ;; Fix Claude Code status line flickering by ensuring consistent line height
    (add-hook 'eat-mode-hook
              (lambda ()
                (setq-local line-spacing 0)  ; Fixed line spacing
                (setq-local default-text-properties '(line-height 1.0))  ; Consistent line height
                (face-remap-add-relative 'default :height 1.0))))

(with-eval-after-load 'eat
    (global-set-key (kbd "C-c o t") 'eat)
    (global-set-key (kbd "C-c o T") 'eat-other-window)
    (define-key project-prefix-map (kbd "t") 'eat-project)
    (define-key project-prefix-map (kbd "T") 'eat-project-other-window))

;; Let the desktop background show through
;; (set-frame-parameter (selected-frame) 'alpha '(97 . 100))
;; (add-to-list 'default-frame-alist '(alpha . (90 . 90)))

(use-package undo-fu
  :config
  (global-unset-key (kbd "C-z"))
  (global-set-key (kbd "C-z")   'undo-fu-only-undo)
  (global-set-key (kbd "C-S-z") 'undo-fu-only-redo))

(use-package dashboard
  :ensure t
  :config
  (dashboard-setup-startup-hook)
  (setq dashboard-banner-logo-title "Emacs ❤️"))

(use-package corfu
  ;; Optional customizations
  :custom
  ;; (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
    (corfu-auto t)                 ;; Enable auto completion
  ;; (corfu-separator ?\s)          ;; Orderless field separator
  ;; (corfu-quit-at-boundary nil)   ;; Never quit at completion boundary
  ;; (corfu-quit-no-match nil)      ;; Never quit, even if there is no match
  ;; (corfu-preview-current nil)    ;; Disable current candidate preview
  ;; (corfu-preselect 'prompt)      ;; Preselect the prompt
  ;; (corfu-on-exact-match nil)     ;; Configure handling of exact matches
  ;; (corfu-scroll-margin 5)        ;; Use scroll margin
    
  :init
  (global-corfu-mode))

(use-package kind-icon
  :ensure t
  :after corfu
  :custom
  (kind-icon-default-style
   '(:padding 0 :stroke 0 :margin 0 :radius 0 :height 0.7 :scale 1.0 :background nil))
  ; (kind-icon-blend-background t)
  ; (kind-icon-default-face 'corfu-default) ; only needed with blend-background
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

;; Add extensions
(use-package cape
  ;; Bind prefix keymap providing all Cape commands under a mnemonic key.
  ;; Press C-c p ? to for help.
  :bind ("C-c p" . cape-prefix-map) ;; Alternative keys: M-p, M-+, ...
  ;; Alternatively bind Cape commands individually.
  ;; :bind (("C-c p d" . cape-dabbrev)
  ;;        ("C-c p h" . cape-history)
  ;;        ("C-c p f" . cape-file)
  ;;        ...)
  :init
  ;; Add to the global default value of `completion-at-point-functions' which is
  ;; used by `completion-at-point'.  The order of the functions matters, the
  ;; first function returning a result wins.  Note that the list of buffer-local
  ;; completion functions takes precedence over the global list.
  (add-hook 'completion-at-point-functions #'cape-dabbrev)
  (add-hook 'completion-at-point-functions #'cape-file)
  (add-hook 'completion-at-point-functions #'cape-elisp-block)
  ;; (add-hook 'completion-at-point-functions #'cape-history)
  ;; ...
)

;; A few more useful configurations...
(use-package emacs
  :custom
  ;; TAB cycle if there are only few candidates
  ;; (completion-cycle-threshold 3)

  ;; Enable indentation+completion using the TAB key.
  ;; `completion-at-point' is often bound to M-TAB.
  ;;(tab-always-indent 'complete)

  ;; Emacs 30 and newer: Disable Ispell completion function. As an alternative,
  ;; try `cape-dict'.
  (text-mode-ispell-word-completion nil)

  ;; Hide commands in M-x which do not apply to the current mode.  Corfu
  ;; commands are hidden, since they are not used via M-x. This setting is
  ;; useful beyond Corfu.
  (read-extended-command-predicate #'command-completion-default-include-p))

;;Switched to corfu above from company
;;(use-package company
;;  :ensure t
;;  :config
;;  (add-hook 'after-init-hook 'global-company-mode)
;;  (company-quickhelp-mode))
;;
;;(use-package company-quickhelp :ensure t)

(use-package ace-window
  :ensure t
  :bind (("M-o" . ace-window)) ;; Override default window switch
  :config
  ;; Customize key labels for window selection
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  ;; Set scope to visible frames
  (setq aw-scope 'frame)
  ;; Show dispatch menu with '?'
  (setq aw-dispatch-always t)
  ;; Customize face for labels
  (set-face-attribute 'aw-leading-char-face nil
                      :foreground "white"
                      :background "red"
                      :weight bold
                      :height 1.2)
  ;; Optional: Enable dispatch actions without modifier
  (setq aw-dispatch-alist
        '((?x aw-delete-window "Delete Window")
          (?m aw-swap-window "Swap Windows")
          (?M aw-move-window "Move Window")
          (?c aw-copy-window "Copy Window")
          (?j aw-switch-buffer-in-window "Select Buffer")
          (?n aw-flip-window)
          (?u aw-switch-buffer-other-window "Switch Buffer Other Window")
          (?f aw-split-window-fair "Split Fair Window")
          (?v aw-split-window-vert "Split Vert Window")
          (?h aw-split-window-horz "Split Horz Window")
          (?o delete-other-windows "Delete Other Windows")
          (?t aw-transpose-frame "Transpose Frame")))
  ;; Reduce distraction by minimizing background dimming
  (setq aw-background nil))

(use-package which-key
  :ensure t
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.5))

;;(use-package ivy
;;  :ensure t
;;  :diminish
;;  :config
;;  (ivy-mode 1))

(use-package dired
  :ensure nil
  :commands (dired)
  :hook
  ((dired-mode . dired-hide-details-mode)
   (dired-mode . hl-line-mode))
  :config
  (setq dired-recursive-copies 'always)
  (setq dired-recursive-deletes 'always)
  (setq delete-by-moving-to-trash t)
  (setq dired-dwim-target t))

(use-package dired-subtree
  :ensure t
  :after dired
  :bind
  ( :map dired-mode-map
    ("<tab>" . dired-subtree-toggle)
    ("TAB" . dired-subtree-toggle)
    ("<backtab>" . dired-subtree-remove)
    ("S-TAB" . dired-subtree-remove))
  :config
  (setq dired-subtree-use-backgrounds nil))

(use-package vertico
  :ensure t
  :custom
  (vertico-cycle t)
  :init
  (vertico-mode))

(use-package savehist
  :init
  (savehist-mode))

(use-package marginalia
  :after vertico
  :ensure t
  :custom
  (marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light nil))
  :init
  (marginalia-mode))

(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)  
  (completion-category-overrides '((file (styles basic partial-completion)))))

;;(use-package all-the-icons
;;  :ensure t)

;; To make the icons visble it might be required to install the icons with
;; m-x nerd-icons-install-fonts

(use-package nerd-icons
  :ensure t)

(use-package nerd-icons-completion
  :ensure t
  :after marginalia
  :config
  (add-hook 'marginalia-mode-hook #'nerd-icons-completion-marginalia-setup))

(use-package nerd-icons-corfu
  :ensure t
  :after corfu
  :config
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

(use-package nerd-icons-dired
  :ensure t
  :hook
  (dired-mode . nerd-icons-dired-mode))


(use-package yasnippet
  :ensure t
  :config
  (setq yas-snippet-dirs '("~/.emacs.d/yassnippets"))
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
;;        ("C-c p" . projectile-command-map)
	)
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

;; In all of the following, WEIGHT is a symbol such as `semibold',
;; `light', `bold', or anything mentioned in `modus-themes-weights'.
;;(setq modus-themes-to-toggle '(modus-operandi-tinted modus-vivendi-tinted))

;;(load-theme 'modus-operandi t)
;;(load-theme 'modus-operandi-tinted t)
;;(load-theme 'modus-operandi-tritanopia t)
;;(load-theme 'modus-operandi-deuteranopia t)
;;(load-theme 'modus-vivendi t)
;;(load-theme 'modus-vivendi-tinted t)
;;(load-theme 'modus-vivendi-tritanopia t)
;;(load-theme 'modus-vivendi-deuteranopia t)

(use-package ef-themes
  :ensure t
  :config
  (setq ef-themes-to-toggle '(ef-maris-light ef-maris-dark))
  ;; Load a specific ef-theme (e.g., ef-summer). Replace with your preferred theme.
  (load-theme 'ef-maris-dark t)
  ;; Optionally, set up a keybinding to cycle through ef-themes
  (global-set-key (kbd "C-c t") #'ef-themes-select)
  ;; Enable theme cycling with a transient menu (optional)
  )

(use-package fontaine
  :ensure t
  :config
  ;; Define font presets
  (setq fontaine-presets
        '((regular
           :default-family "JetBrains Mono"
           :default-weight normal
           :default-height 140
           :fixed-pitch-family "JetBrains Mono"
           :variable-pitch-family "Fira Sans"
           :variable-pitch-height 1.05
           :bold-weight bold
           :italic-slant italic
           :line-number-height 0.9)
          (large
           :default-family "JetBrains Mono"
           :default-weight semilight
           :default-height 180
           :fixed-pitch-family "JetBrains Mono"
           :variable-pitch-family "Source Sans Pro"
           :variable-pitch-height 1.05
           :bold-weight bold
           :italic-slant italic
           :mode-line-active-height 0.9
           :mode-line-inactive-height 0.9)
          (presentation
           :default-family "Iosevka"
           :default-weight semilight
           :default-height 220
           :fixed-pitch-family "Iosevka"
           :variable-pitch-family "Fira Sans"
           :variable-pitch-height 1.1
           :bold-weight extrabold
           :italic-slant italic)
          (t
           :default-family "Monospace"
           :default-weight regular
           :default-height 100
           :fixed-pitch-family nil
           :variable-pitch-family "Sans"
           :variable-pitch-height 1.0
           :bold-weight bold
           :italic-slant italic)))
  ;; Set default preset at startup
  (fontaine-set-preset (or (fontaine-restore-latest-preset) 'regular))
  ;; Enable fontaine-mode to persist presets
  (fontaine-mode 1)
  ;; Persist font settings when switching themes
  (dolist (hook '(modus-themes-after-load-theme-hook ef-themes-post-load-hook))
    (add-hook hook #'fontaine-apply-current-preset)))

;;(use-package kaolin-themes
;;  :ensure t
;;  :config
;;  (load-theme 'kaolin-dark t)
;;  (load-theme 'kaolin-light t)
;;  The best light theme below!  
;;  (load-theme 'kaolin-valley-light t)
;;  (load-theme 'kaolin-aurora t)
;;  (load-theme 'kaolin-bubblegum t)
;;  (load-theme 'kaolin-eclipse t)
;;  (load-theme 'kaolin-galaxy t)
;;  (load-theme 'kaolin-ocean t)
;;  (load-theme 'kaolin-temple t)
;;  (load-theme 'kaolin-valley-dark t)
;;  (kaolin-treemacs-theme)
;;)

;;(use-package doom-modeline
;;  :init (doom-modeline-mode 1)
;;  :custom
;;  (doom-modeline-height 20)
;;  )

(use-package savehist
  :ensure nil ; it is built-in
  :hook (after-init . savehist-mode))

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


(use-package git-gutter
  :ensure t
  :hook (prog-mode . git-gutter-mode)
  :config
  (setq git-gutter:update-interval 0.02))

(use-package git-gutter-fringe
  :ensure t
  :config
  (define-fringe-bitmap 'git-gutter-fr:added [224] nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:modified [224] nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:deleted [128 192 224 240] nil nil 'bottom))

;; If the below does not work use packge-install and then search for rg to install
(use-package rg
  :ensure t)
(rg-enable-menu)
(global-set-key (kbd "C-c s") #'rg-menu)

(use-package imenu-list
  :ensure t)
;;(global-set-key (kbd "C-'") #'imenu-list-smart-toggle)
(global-set-key (kbd "C-'") #'imenu-list)
;;(rg-enable-default-bindings)

(use-package editorconfig
  :ensure t
  :config
  (editorconfig-mode 1))

(load "~/Documents/keys/anthropic/anthropic.el")
(setq gptel-model "claude-3-7-sonnet-20241022")  ; Latest as of late 2024; update if newer
(use-package gptel
  :ensure t  ; Ensures gptel is installed via package.el
  :bind
  ("C-c g" . gptel-send)  
  :config
  ;; Define Anthropic backend
  (setq gptel-backend
        (gptel-make-anthropic "Claude"
          :stream t  ; Enable streaming responses
          :key anthropic-api-key)))
  ;; Set default model



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
     (java "https://github.com/tree-sitter/tree-sitter-java")
     (c "https://github.com/tree-sitter/tree-sitter-c")))

;;*************************************IMPORTANT!***************************************
;;Ensure  the langauages are available by running below
;;(mapc #'treesit-install-language-grammar (mapcar #'car treesit-language-source-alist))

(add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-ts-mode))
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . tsx-ts-mode))

(setq major-mode-remap-alist
 '((yaml-mode . yaml-ts-mode)
   (bash-mode . bash-ts-mode)
   (js2-mode . js-ts-mode)
   (typescript-mode . typescript-ts-mode)
   (json-mode . json-ts-mode)
   (css-mode . css-ts-mode)
   (go-mode . go-ts-mode)   
   (python-mode . python-ts-mode)
   (java-mode . java-ts-mode)
   (c-mode . c-ts-mode)))

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
  :bind (:map eglot-mode-map
	      ("C-x l a" . eglot-code-actions)
	      ("C-x l r" . eglot-rename)
	      ("C-x l h" . eldoc)
	      ("C-x l f" . eglot-format)
	      ("C-x l F" . eglot-format-buffer)
	      ("C-x l d" . xref-find-definitions-at-mouse)
	      ("C-x l R" . eglot-reconnect))
  :custom
  (fset #'jsonrpc--log-event #'ignore)
  (eglot-events-buffer-size 0)
  (eglot-sync-connect nil)
  (eglot-connect-timeout nil)
  (eglot-autoshutdown t)
  (eglot-send-changes-idle-time 3)
  (flymake-no-changes-timeout 5)
;;  (eldoc-echo-area-use-multiline-p nil)
  (setq eglot-ignored-server-capabilities '( :documentHighlightProvider))
  :hook (
	 (go-ts-mode . eglot-ensure)
	 (python-ts-mode . eglot-ensure))
  )

;;Scroll the compilation window when needed
(setq compilation-scroll-output t)
(add-hook 'compilation-finish-functions 'switch-to-buffer-other-window 'compilation)

;;enable which function mode to determine the current function under the cursor
(which-function-mode t)

(defun mvn-compile ()
  "Traveling up the path, find build.xml file and run compile"
  (interactive)
  (save-buffer)
  (with-temp-buffer
    (cd (project-root (project-current)))
    (set (make-local-variable 'compile-command)
	 "mvn clean install")
    (call-interactively 'compile)))

(defun mvn-test()
  "Traveling up the path, find build.xml file and run compile"
  (interactive)
  (save-buffer)
  ;;(which-function-mode) needs to be enabled, but it works!
  (let* ((source (file-name-base buffer-file-name)))
    (with-temp-buffer
      (cd (project-root (project-current)))
      (set (make-local-variable 'compile-command)
	   (format "mvn test -Dtest=\"%s\"" source))
      (call-interactively 'compile))))

(defun mvn-test-fn()
  "Traveling up the path, find build.xml file and run compile"
  (interactive)
  (save-buffer)
  (let* ((curr-fn (string-replace "." "#" (which-function))))
    (with-temp-buffer
      (cd (project-root (project-current)))
      (set (make-local-variable 'compile-command)
	   (format "mvn test -Dtest=\"%s\"" curr-fn))
      (call-interactively 'compile))))

;; The following approaches are similar
;; (cd (project-root (project-current)))
;; (while (and (not (file-exists-p "pom.xml"))
;; 	  (not (equal "/" default-directory)))
;; (cd ".."))

(defun java-exec ()
  "Traveling up the path, find build.xml file and run compile"
  (interactive)
  (save-buffer)
  (let* ((source (file-name-sans-extension buffer-file-name)))
    (with-temp-buffer
      (cd (project-root (project-current)))
      (let* ((actual-file (string-replace default-directory "" source))
	     (actual-file (string-replace "src/main/java/" "" actual-file))
	     (actual-file (string-replace "src/test/java/" "" actual-file))	     
	     (actual-file (string-replace "/" "." actual-file)))
	(set (make-local-variable 'compile-command)
	     (format "mvn exec:java -Dexec.mainClass=\"%s\"" actual-file))
	(call-interactively 'compile)))))

;; JAVA LSP Support
;; 1. Downloads : https://download.jboss.org/jbosstools/static/jdt.ls/stable/
;; 2. Rename downloaded java-linux-x64-1.25.0-1023.vsix to '*.zip' and extract.
;; 3. Use netbeans LSP instead of jdtls
;; ln -s /home/rahulraj/tools/java-linux-x64-1.25.0-1023/extension/server/bin/jdtls $HOME/local/bin/jdtls

(require 'eglot)

;; Clean jdt:// URI handler that caches decompiled sources
(defun rr/jdt-file-name-handler (operation &rest args)
  "Support Eclipse jdtls `jdt://' uri scheme."
  (let* ((uri (car args))
         (cache-dir "/tmp/.eglot")
         (source-file
          (expand-file-name
           (file-name-concat
            cache-dir
            (save-match-data
              (when (string-match "jdt://contents/\\(.*?\\)/\\(.*\\)\.class\\?" uri)
                (format "%s.java" (replace-regexp-in-string "/" "." (match-string 2 uri) t t))))))))
    (unless (file-readable-p source-file)
      (let ((content (jsonrpc-request (eglot-current-server) :java/classFileContents (list :uri uri)))
            (metadata-file (format "%s.%s.metadata"
                                   (file-name-directory source-file)
                                   (file-name-base source-file))))
        (unless (file-directory-p cache-dir) (make-directory cache-dir t))
        (with-temp-file source-file (insert content))
        (with-temp-file metadata-file (insert uri))))
    source-file))

(add-to-list 'file-name-handler-alist '("\\`jdt://" . rr/jdt-file-name-handler))

;; Configure eglot for Java with proper initialization options
(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
               `((java-mode java-ts-mode) .
                 ("jdtls"
                  "-data" ,(expand-file-name "~/.cache/jdtls-workspace/")
                  :initializationOptions
                  (:extendedClientCapabilities (:classFileContentsSupport t)
                   :settings
                   (:java
                    (:format
                     (:enabled t
                      :settings (:url "file:///home/rahulraj/tools/google-java-format/eclipse-java-google-style.xml"))
                     :sources (:organizeImports (:enabled t))
                     :maven (:downloadSources t)
                     :eclipse (:downloadSources t)
                     :configuration (:maven (:userSettings ,(expand-file-name "~/.m2/settings.xml")))
                     :import (:maven (:enabled t))
                     :autobuild (:enabled t))))))))

(add-hook 'java-mode-hook (lambda ()
			    (remove-hook 'eglot-connect-hook #'eglot-signal-didChangeConfiguration t)))

;; NOTE: hack workspace/didChangeConfiguration
;;(define-advice eglot-signal-didChangeConfiguration (:override (server) override)
;;    "Send a `:workspace/didChangeConfiguration' signal to SERVER.
;;When called interactively, use the currently active server"
;;    (interactive (list (eglot--current-server-or-lose)))
;;    (when-let ((settings (eglot--workspace-configuration-plist server)))
;;      (jsonrpc-notify
;;       server :workspace/didChangeConfiguration
;;       (list :settings settings))))

;;The below snippet is needed to ensure that eglot-code-actions are executed properly
(with-eval-after-load 'eglot
  (cl-defmethod eglot-execute-command
    (_server (_cmd (eql java.apply.workspaceEdit)) arguments)
    "Eclipse JDT breaks spec and replies with edits as arguments."
    (mapc #'eglot--apply-workspace-edit arguments)))

(use-package zig-mode :ensure t)

(use-package vterm :ensure t)

;;(use-package combobulate
;;   :custom
;;   ;; You can customize Combobulate's key prefix here.
;;   ;; Note that you may have to restart Emacs for this to take effect!
;;   (combobulate-key-prefix "C-c o")
;;   :hook ((prog-mode . combobulate-mode))
;;   ;; Amend this to the directory where you keep Combobulate's source
;;   ;; code.
;;   :load-path ("~/.emacs.d/combobulate"))

(use-package writeroom-mode
  :ensure t)

(use-package chatgpt-shell
  :ensure t)


(load "~/Documents/keys/chatgpt-emacs/chatgpt.el")


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
 ;;'(java-ts-mode-indent-offset 2)
 '(package-selected-packages
   '(dired-subtree nerd-icons-completion nerd-icons-dired nerd-icons-corfu nerd-icons kind-icon cape corfu marginalia orderless vertico eglot-java git-gutter-fringe git-gutter imenu-list rg zig-mode breadcrumb gptel eglot chatgpt-shell writeroom-mode ts-fold eshell-toggle yasnippet projectile company-quickhelp company magit treemacs doom-modeline kaolin-themes all-the-icons ivy which-key flycheck exec-path-from-shell)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'narrow-to-region 'disabled nil)
