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

(require 'package)
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
(setq use-package-always-ensure t)

(use-package nord-theme
  :init (load-theme 'nord t))
;;(load-theme 'tango-dark)
;;(use-package doom-themes
;;  :init (load-theme 'doom-palenight t))

(use-package ivy
  :diminish
  :config
  (ivy-mode 1))

(use-package all-the-icons)

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


(org-babel-do-load-languages
 'org-babel-load-languages
 (mapcar (lambda (lang) (cons lang t))
         `(ditaa
           dot
           octave
           perl
           python
           ruby
	   java
           ,(if (locate-library "ob-shell") 'shell 'sh)
           sqlite
           )))

(put 'downcase-region 'disabled nil)

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

;; Org mode Tag color
(require 'org)

(add-to-list 'org-tag-faces '("@.*" . (:foreground "red")))

;; Reset the global variable to nil, just in case org-mode has already beeen used.
(when org-tags-special-faces-re
  (setq org-tags-special-faces-re nil))

(defun org-get-tag-face (kwd)
  "Get the right face for a TODO keyword KWD.
If KWD is a number, get the corresponding match group."
  (if (numberp kwd) (setq kwd (match-string kwd)))
  (let ((special-tag-face (or (cdr (assoc kwd org-tag-faces))
                              (and (string-match "^@.*" kwd)
                                   (cdr (assoc "@.*" org-tag-faces))))))
    (or (org-face-from-face-or-color 'tag 'org-tag special-tag-face)
        'org-tag)))
(recentf-mode 1)
(save-place-mode 1)

(use-package neotree)
;;(require 'neotree)
(global-set-key [f8] 'neotree-toggle)

(use-package lsp-mode
  :ensure t
  :bind (:map lsp-mode-map
              ("C-c d" . lsp-describe-thing-at-point)
              ("C-c a" . lsp-execute-code-action))
  :bind-keymap ("C-c l" . lsp-command-map)
  :config
  (lsp-enable-which-key-integration t))

(use-package company
  :ensure t
  :hook ((emacs-lisp-mode . (lambda ()
                              (setq-local company-backends '(company-elisp))))
         (emacs-lisp-mode . company-mode))
  :config 
  ;;  (company-keymap--unbind-quick-access company-active-map)
  (company-tng-configure-default)
  :custom
  (company-minimum-prefix-length 1)
  (company-idle-delay 0.1)
  (company-show-quick-access "off")  
  (company-quick-access-hint-function (lambda (param) " unknown")))

;; company-quick-access-hint-function  
;;(use-package company-box
;;  :hook (company-mode . company-box-mode))

(use-package flycheck
  :ensure t)

;;; Go
(use-package go-mode
  :ensure t
  :hook ((go-mode . lsp-deferred)
         (go-mode . company-mode))
  :bind (:map go-mode-map
              ("<f6>"  . gofmt)
              ("C-c 6" . gofmt))
  :config
  (require 'lsp-go)
  ;; https://github.com/golang/tools/blob/master/gopls/doc/analyzers.md
  (setq lsp-go-analyses
        '((fieldalignment . t)
          (nilness        . t)
          (unusedwrite    . t)
          (unusedparams   . t)))
  ;; GOPATH/bin
  (add-to-list 'exec-path "~/go/bin")
  ;; requires goimports to be installed
  (setq gofmt-command "goimports"))

(global-set-key (kbd "<f5>") #'recompile)

(use-package magit
  :commands magit-status
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))
