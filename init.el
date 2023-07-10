;;; early config

;; no splash
(setq inhibit-splash-screen t)

;; general performance
(setq gc-cons-threshold (* 100 1024 1024)
      read-process-output-max (* 5 1024 1024))

;; set up external custom file
(setq custom-file (locate-user-emacs-file "custom.el"))

;; macos native compilation fix
(when (eq system-type 'darwin)
  (customize-set-variable 'native-comp-driver-options '("-Wl,-w")))

;; lexical binding
(setq lexical-binding t)

;; package archives
(require 'package)

(dolist (archive '(("nongnu" . "https://elpa.nongnu.org/nongnu/")
		   ("melpa" . "https://melpa.org/packages/")
		   ("org" . "https://orgmode.org/elpa/")))
  (add-to-list 'package-archives archive))

;; pull latest package info
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; use package imenu integration
(setq use-package-enable-imenu-support t)

;; ensure use-package is available (pre-29)
(unless (package-installed-p 'use-package)
  (package-install 'use-package)
  (require 'use-package))

;; misc. early settings
(use-package emacs
  :init
  (setq lexical-binding t
        inhibit-splash-screen t
        gc-cons-threshold (* 100 1024 1024)
        read-process-output-max (* 5 1024 1024)
        ring-bell-function 'ignore
        tabs-always-indent nil
        indent-tabs-mode nil
        use-short-answers t
        kill-buffer-query-functions (remq 'process-kill-buffer-query-function
                                          kill-buffer-query-functions)
        mac-command-modifier 'meta
	mac-option-modifier 'super
        sentence-end-double-space nil)
  :bind ("C-M-<backspace>" . #'backward-kill-sexp))

;; set up external custom file
(use-package cus-edit
  :init
  (setq custom-file (locate-user-emacs-file "custom.el")))

;; macos native compilation fix
(use-package comp
  :if (eq system-type 'darwin)
  :custom
  (native-comp-driver-options '("-Wl,-w")))

;; trying to make wrapping less ugly
(use-package adaptive-wrap
  :ensure t
  :hook ((visual-line-mode . adaptive-wrap-prefix-mode)
         (org-mode . (lambda () (setq-local adaptive-wrap-extra-indent 4))))
  :init
  (global-visual-line-mode 1))

;; helper function for setting paths in emacs dir
(defun my/initialize-emacs-dir-path (sub-path)
    (let ((path (expand-file-name sub-path user-emacs-directory)))
      (make-directory path t)
      path))

(use-package files
  :ensure f
  :hook (before-save . delete-trailing-whitespace)
  :init
  (setq
   require-final-newline t
   ;; reduce nuisance prompts
   confirm-nonexistent-file-or-buffer nil
   ;; automatically refresh any file visiting buffers
   revert-without-query '(".*")
   ;; keep transient files tidy (part 1)
   my/transient-files-backup-dir (my/initialize-emacs-dir-path "tmp/backups/")
   my/transient-files-auto-save-dir (my/initialize-emacs-dir-path "tmp/auto-saves/")
   backup-directory-alist `(("." . ,my/transient-files-backup-dir))
   auto-save-file-name-transforms `((".*" ,my/transient-files-auto-save-dir t)))
  :bind (("C-r" . #'revert-buffer)
	 ("C-x C-r" . #'set-visited-file)))

(use-package emacs
  :ensure f
  :init
  (setq
   ;; keep transient files tidy (part 2)
   my/transient-files-auto-save-prefix (my/initialize-emacs-dir-path "tmp/auto-saves/sessions")
   auto-save-list-file-prefix my/transient-files-auto-save-prefix
   create-lockfiles nil))

;; contextually repeat commands with a single key press
(use-package repeat-mode
  :ensure f
  :init
  (repeat-mode 1))

(use-package simple
  :ensure f
  :init
  ;; repeat-like behavior when popping mark
  (setq set-mark-command-repeat-pop t)
  (column-number-mode 1)
  (line-number-mode 1)
  :config
  ;; better default behavior for M-SPC
  (global-set-key [remap just-one-space] #'cycle-spacing))

;; delete selected region on insert
(use-package delsel
  :ensure f
  :init
  (delete-selection-mode 1))

;; update buffers when files change
(use-package autorevert
  :ensure f
  :init
  ;; keep modeline up to date
  (setq auto-revert-check-vc-info t)
  (global-auto-revert-mode 1))

;; forward compatibility
(use-package compat
  :ensure t
  :config
  (when (not (fboundp #'scratch-buffer))
    (defun scratch-buffer ()
      (interactive)
      (display-buffer (get-scratch-buffer-create)))))

;; make buffers unique based on directory
(use-package uniquify
  :demand t
  :config
  (setq uniquify-buffer-name-style 'post-forward-angle-brackets))

(use-package ibuffer
  :ensure f
  :bind ("C-x C-b" . #'ibuffer))

;; helpful window operations
(use-package window
  :ensure f
  :init
  (setq my/window-map (make-sparse-keymap))
  (bind-key "C-c w" my/window-map)
  :bind (("C-c s" . #'scratch-buffer)
	 :map my/window-map
         ("t" . #'window-toggle-side-windows)
         ("o" . #'other-window)
         ("b" . #'balance-windows)
	 :repeat-map my/window-repeat-map
	 ("<right>" . #'next-buffer)
	 ("<left>" . #'previous-buffer)))

(use-package windmove
  :ensure f
  :after window
  :init
  :bind (:map my/window-map
         ("<down>" . #'windmove-down)
         ("<up>" . #'windmove-up)
         ("<left>" . #'windmove-left)
         ("<right>" . #'windmove-right)
         :repeat-map my/windmove-repeat-map
         ("<down>" . #'windmove-down)
         ("<up>" . #'windmove-up)
         ("<left>" . #'windmove-left)
         ("<right>" . #'windmove-right)))

(use-package winner
  :ensure f
  :after window
  :init (winner-mode 1)
  :bind (:map my/window-map
         ("u" . #'winner-undo)
         ("r" . #'winner-redo)
         :repeat-map my/winner-repeat-map
         ("u" . #'winner-undo)
         ("r" . #'winner-redo)))

;; window display behavior
(use-package window
  :ensure f
  :config
  ;; adjust transient buffer behaviors to personal preference
  (defun my/display-buffer-in-side-window-and-select (buffer alist)
    (let ((window (display-buffer-in-side-window buffer alist)))
      (select-window window)))

  (setq window-sides-slots '(0 1 1 1)
	display-buffer-alist
	;; TODO: add rules for pp-eval/expand and geiser
	;; TODO: create a system for generalizing the display rules I want,
	;;       so display definitions don't need to be centralized here.
	'(;; top bar interactive
          ("^\\*[[:alnum:]-\.]*\\(shell\\|term\\|eshell\\|vterm\\|Python\\)\\*$"
           (my/display-buffer-in-side-window-and-select)
           (side . top)
           (slot . 1)
           (window-height . 0.20))
          ;; top bar informational
          ("^\\*\\(Occur\\|Flymake\\|xref\\|grep\\|docker-\\)"
           (my/display-buffer-in-side-window-and-select)
           (select. t)
           (side . top)
           (slot . 1)
           (window-height . 0.20))
          ;;
          ("^\\*\\(scratch\\)"
           (my/display-buffer-in-side-window-and-select)
           (select. t)
           (side . top)
           (slot . 1)
           (window-height . 0.20))
          ;; side bar information
          ("^\\*\\(\[Hh]elp\\|info\\|documentation\\|Metahelp\\)"
           (my/display-buffer-in-side-window-and-select)
           (side . right)
           (slot . 1)
           (window-width . 0.30))
          ("^\\*\\( docker\\)"
           (my/display-buffer-in-side-window-and-select)
           (side . right)
           (slot . 1)
           (window-width . 0.30)))))

;; get helpful prompts for keys
(use-package which-key
  :ensure t
  :init
  (which-key-mode))

;; improved help
(use-package helpful
  :ensure t
  :bind (([remap describe-function] . #'helpful-callable)
	 ([remap describe-variable] . #'helpful-variable)
	 ([remap describe-command] . #'helpful-command)
	 ([remap describe-key] . #'helpful-key)
	 ("C-c C-d" . #'helpful-at-point)))

;; get examples in help

;; define modal-like menus in a declarational style
(use-package hydra
  :ensure t)

;; improved search and completion functionality
;; vertical suggestions for many minibuffer operations
(use-package vertico
  :ensure t
  :init
  (setq vertico-buffer-display-action
        '(display-buffer-in-side-window
          (window-height . 0.20)
          (side . top)
          (window-parameters . ((mode-line-format . ("  ↑ make selection above ↑")))))
	 vertico-cycle t)
  (vertico-mode)
  (vertico-buffer-mode))

;; improved file navigation
(use-package vertico-directory
  :ensure nil
  :after vertico
  :bind (:map vertico-map
         ("RET" . vertico-directory-enter)
         ("DEL" . vertico-directory-delete-char)
         ("M-DEL" . vertico-directory-delete-word))
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))

;; additional data with suggestions
(use-package marginalia
  :ensure t
  :init
  (marginalia-mode))

;; completion engine supporting richer search
(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles partial-completion)))))

;; make searches interactive
(use-package consult
  ;; Replace bindings. Lazily loaded due by `use-package'.
  :bind (;; C-c bindings (mode-specific-map)
         ("C-c m x" . consult-mode-command)
         ("C-c h i" . consult-info)
         ([remap Info-search] . consult-info)
         ;; C-x indings (ctl-x-map)
         ("C-x M-:" . consult-complex-command)
         ("C-x b" . consult-buffer)
         ("C-x 4 b" . consult-buffer-other-window)
         ("C-x 5 b" . consult-buffer-other-frame)
         ("C-x r b" . consult-bookmark)
         ("C-x p b" . consult-project-buffer)
         ("C-c p b" . consult-project-buffer)
         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store)
         ("C-M-#" . consult-register)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)
         ;; M-g bindings (goto-map)
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flymake)
         ("M-g g" . consult-goto-line)
         ("M-g M-g" . consult-goto-line)
         ("M-g o" . consult-outline)
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ;; M-s bindings (search-map)
         ("M-s f" . consult-find)
         ("M-s D" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ("M-s e" . consult-isearch-history)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
         ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
         ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
         ("M-s L" . consult-line-multi)            ;; needed by consult-line to detect isearch
         :map minibuffer-local-map
         ("M-s" . consult-history)                 ;; orig. next-matching-history-element
         ("M-r" . consult-history))                ;; orig. previous-matching-history-element

  :init
  ;; TODO: this should be conditional on OS
  ;; gnu locate is glocate using macports
  (setq consult-locate-args "glocate --ignore-case")

  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)

  (advice-add #'register-preview :override #'consult-register-window)

  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)
  :config
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   :preview-key '(:debounce 0.4 any))

  (setq consult-narrow-key "<"))

;;; writable grep buffers

(use-package wgrep
  :ensure t
  :demand t)

;;; context menus with embark

(use-package embark
  :ensure t
  :bind
  (("C-." . embark-act)
   ("C-c a a" . embark-act)
   ("C-;" . embark-dwim)
   ("C-c a d" . embark-dwim)
   ("C-h B" . embark-bindings))
  :init
  (setq prefix-help-command #'embark-prefix-help-command)
  :config
  ;; TODO: maybe remove this??
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

;; consult integration
(use-package embark-consult
  :ensure t
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

;; which-key integration
(defun embark-which-key-indicator ()
  "An embark indicator that displays keymaps using which-key.
The which-key help message will show the type and value of the
current target followed by an ellipsis if there are further
targets."
  (lambda (&optional keymap targets prefix)
    (if (null keymap)
        (which-key--hide-popup-ignore-command)
      (which-key--show-keymap
       (if (eq (plist-get (car targets) :type) 'embark-become)
           "Become"
         (format "Act on %s '%s'%s"
                 (plist-get (car targets) :type)
                 (embark--truncate-target (plist-get (car targets) :target))
                 (if (cdr targets) "…" "")))
       (if prefix
           (pcase (lookup-key keymap prefix 'accept-default)
             ((and (pred keymapp) km) km)
             (_ (key-binding prefix 'accept-default)))
         keymap)
       nil nil t (lambda (binding)
                   (not (string-suffix-p "-argument" (cdr binding))))))))

(setq embark-indicators
  '(embark-which-key-indicator
    embark-highlight-indicator
    embark-isearch-highlight-indicator))

(defun embark-hide-which-key-indicator (fn &rest args)
  "Hide the which-key indicator immediately when using the completing-read prompter."
  (which-key--hide-popup-ignore-command)
  (let ((embark-indicators
         (remq #'embark-which-key-indicator embark-indicators)))
      (apply fn args)))

(advice-add #'embark-completing-read-prompter
            :around #'embark-hide-which-key-indicator)

;; directory viewer
(use-package dired
  :after files
  :hook ((dired-mode . dired-hide-details-mode)
         (dired-mode . (lambda () (dired-omit-mode))))
  :init
  (setq insert-directory-program
        (if (and (eq system-type 'darwin)
                 (executable-find "gls"))
            "gls"
          insert-directory-program)
        dired-listing-switches "-al --dired --group-directories-first -h -G"
        dired-omit-files "\\`[.]?#\\|\\.java\\|snap\\|System\\|\\.ssh\\|\\.gitconfig\\|\\.wget\\|\\.aspell\\|\\.cache\\|\\.lesshst\\|\\.gftp\\|\\.pki\\|\\.gnome\\|VirtualBox\\|master\\.tar\\.gz\\|\\.wine\\|plan9port\\|\\.idm\\|\\.font\\|\\.iso\\|\\.cargo\\|lib\\|amd64\\|\\.gnupg\\|\\.python\\|\\.var\\|\\.local\\|\\`[.][.]?\\'"))

(use-package all-the-icons
  :ensure t
  :config
  (setq all-the-icons-scale-factor 0.8))

(use-package all-the-icons-dired
  :ensure t
  :after (all-the-icons dired)
  :hook ((dired-mode . all-the-icons-dired-mode))
  :config
  (setq all-the-icons-dired-monochrome nil))

;;; mac usability tweaks

;; load system path on startup
(use-package exec-path-from-shell
  :ensure t
  :init
  (exec-path-from-shell-initialize))

;;; visual tweaks

;; create after theme hook
(use-package custom
  :config
  (defvar after-enable-theme-hook nil
    "Normal hook run after enabling a theme.")

  (defun run-after-enable-theme-hook (&rest _args)
    "Run `after-enable-theme-hook'."
    (run-hooks 'after-enable-theme-hook))

  (advice-add 'enable-theme :after #'run-after-enable-theme-hook))

;; visual tweaks to apply after theme
(use-package faces
  :after (custom fringe tab-bar lsp-mode doom-themes)
  :config
  (setq my/default-fixed-pitch-font "JetBrains Mono"
	my/default-variable-pitch-font "Iosevka Aile"
	my/default-fixed-pitch-height 270
	my/default-variable-pitch-height 270)

  (defun my/apply-preferred-fonts ()
    (interactive)
    (set-face-attribute 'default nil
			:font my/default-fixed-pitch-font
			:weight 'light
			:height my/default-fixed-pitch-height)
    (set-face-attribute 'fixed-pitch nil
			:font my/default-fixed-pitch-font
			:weight 'light
			:height my/default-fixed-pitch-height)
    (set-face-attribute 'variable-pitch nil
			:font my/default-variable-pitch-font
			:weight 'light
			:height my/default-variable-pitch-height)
    (set-face-attribute 'bold nil :weight 'normal))

  (defun my/tweak-lsp-mode-faces ()
    (set-face-attribute 'lsp-face-highlight-write nil
			:weight 'normal)
    (set-face-attribute 'lsp-face-highlight-textual nil
			:weight 'normal))

  (defun my/tweak-tab-bar-faces ()
    (set-face-attribute 'tab-bar nil
			:background (doom-color 'modeline-bg-alt)
			:foreground (doom-color 'modeline-bg-alt))
    (set-face-attribute 'tab-bar-tab nil
			:background (doom-color 'modeline-bg-alt)
			:weight 'normal)
    (set-face-attribute 'tab-bar-tab-inactive nil
			:foreground (doom-color 'modeline-fg-alt)
			:background (doom-color 'modeline-bg-alt)
			:weight 'normal))

  (defun my/tweak-fringe-faces ()
    (set-face-attribute 'fringe nil
			:background (doom-color 'bg)
			:foreground (doom-color 'bg)))

  :hook ((after-enable-theme-hook . my/apply-preferred-fonts)
	 (after-enable-theme-hook . my/tweak-lsp-mode-faces)
	 (after-enable-theme-hook . my/tweak-tab-bar-faces)
	 (after-enable-theme-hook . my/tweak-fringe-faces)))

(use-package doom-themes
  :ensure t
  :config
  (setq doom-themes-enable-bold nil
        doom-themes-enable-italic nil)

  ;; my theme selections for toggling
  (setq my/light-theme 'doom-nord-light
        my/dark-theme 'doom-nord-aurora
        my/active-theme my/dark-theme)

  ;; toggle theme
  (defun my/theme-toggle (&optional light-or-dark)
    (interactive)
    (setq my/active-theme
          (or (and (eq light-or-dark :light) my/light-theme)
              (and (eq light-or-dark :dark) my/dark-theme)
              (and (eq my/active-theme my/dark-theme) my/light-theme)
              (and (eq my/active-theme my/light-theme) my/dark-theme)
              my/dark-theme))
    (mapcar 'disable-theme custom-enabled-themes)
    (load-theme my/active-theme t))

  ;; switch themes with system
  (when (eq system-type 'darwin)
    (defun my/match-theme-to-system ()
      (let ((appearance (plist-get (mac-application-state)
                                   :appearance)))
        (my/theme-toggle
         (if (string-equal
              appearance
              "NSAppearanceNameDarkAqua")
             :dark
           :light))))

    (add-hook 'mac-effective-appearance-change-hook
              #'my/match-theme-to-system))
  
  (load-theme my/active-theme t)
  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config)
  :bind ("C-t t" . #'my/theme-toggle))

(use-package doom-modeline
  :ensure t
  :init
  (setq doom-modeline-icon nil
        doom-modeline-vcs-max-length 36
        doom-modeline-support-imenu nil
        doom-modeline-height 0
        doom-modeline-bar-width 0
        doom-modeline-project-detection nil
        doom-modeline-battery nil
        doom-modeline-time nil
        doom-modeline-display-misc-in-all-mode-lines nil)
  :hook (after-init . doom-modeline-mode))

;;; tabs and tabspaces

;; add minimal tab bar
(use-package tab-bar
  :init
  (setq tab-bar-close-button-show nil
	tab-bar-new-button-show nil
	tab-bar-tab-hints nil
	tab-bar-new-tab-choice "*scratch*")
  (tab-bar-mode 1)
  (tab-rename "default" 0)
  ;; fix bug with tab-new and side windows
  (define-advice
      tab-bar-new-tab
      (:before (&rest args) my/side-window-new-tab-fix)
    (when (window-parameter (selected-window) 'window-side)
      (select-window (window-main-window)))))

;; isolate tabs
(use-package tabspaces
  :ensure t
  :after tab-bar
  :hook (after-init . tabspaces-mode)
  :commands (tabspaces-switch-or-create-workspace
             tabspaces-open-or-create-project-and-workspace)
  :custom
  (tabspaces-use-filtered-buffers-as-default t)
  (tabspaces-default-tab "default")
  (tabspaces-remove-to-default t)
  (tabspaces-include-buffers '("*scratch*" "*Messages*"))
  ;; sessions
  ;; (tabspaces-session t)
  ;; (tabspaces-session-auto-restore t)
  :config
  (project-known-project-roots)
  ;; embed tabspaces within tab keymap
  (bind-key "s" tabspaces-command-map tab-prefix-map))

(use-package project
  :after tab-bar
  :config
  (when (not (fboundp 'project-name))
    (defun project-name (proj)
      (cl-second (reverse (split-string (cdr proj) "/")))))

  (defun my/project-tab-name ()
    (interactive)
    (tab-bar-rename-tab (project-name (project-current))))
  :bind (:map project-prefix-map
	 ("TAB" . #'my/project-tab-name))
  :bind-keymap ("C-c p" . project-prefix-map))

;; TODO: should this go under consult and then consult given :after tabspaces?
;; tab isolation w/ consult
(with-eval-after-load 'consult
  ;; hide full buffer list (still available with "b" prefix)
  (consult-customize consult--source-buffer :hidden t :default nil)
  ;; set consult-workspace buffer list
  (defvar consult--source-workspace
    (list :name     "Workspace Buffers"
          :narrow   ?w
          :history  'buffer-name-history
          :category 'buffer
          :state    #'consult--buffer-state
          :default  t
          :items    (lambda () (consult--buffer-query
                                :predicate #'tabspaces--local-buffer-p
                                :sort 'visibility
                                :as #'buffer-name)))

    "Set workspace buffer list for consult-buffer.")
  (add-to-list 'consult-buffer-sources 'consult--source-workspace))

;; TODO: move to early init?
(use-package tool-bar
  :init
  (tool-bar-mode -1))

(use-package scroll-bar
  :init
  (scroll-bar-mode -1))

(use-package menu-bar
  :init
  (unless (memq window-system '(mac ns))
    (menu-bar-mode -1)))

;; highlight line everywhere
(use-package hl-line
  :after (vterm eshell)
  :init
  (global-hl-line-mode 1)
  :hook ((vterm-mode eshell-mode)
	 . (lambda ()
	     (setq-local global-hl-line-mode nil))))

;; display line numbers for code buffers
(use-package display-line-numbers
  :hook (prog-mode . display-line-numbers-mode))

;; display only left fringe
(use-package fringe
  :init
  (fringe-mode '(8 . 0)))

;;; terminal

;; vterm
(use-package vterm
  :ensure t
  :after project
  :bind (:map project-prefix-map
         ("t" . project-vterm))
  :preface
  (defun project-vterm ()
    (interactive)
    (defvar vterm-buffer-name)
    (let* ((default-directory (project-root     (project-current t)))
           (vterm-buffer-name (project-prefixed-buffer-name "vterm"))
           (vterm-buffer (get-buffer vterm-buffer-name)))
      (if (and vterm-buffer (not current-prefix-arg))
          (pop-to-buffer vterm-buffer  (bound-and-true-p display-comint-buffer-action))
        (vterm))))
  :init
  (add-to-list 'project-switch-commands     '(project-vterm "Vterm") t)
  (add-to-list 'project-kill-buffer-conditions  '(major-mode . vterm-mode))
  :config
  (setq vterm-copy-exclude-prompt t
        vterm-disable-bold t
        vterm-max-scrollback 100000
        vterm-tramp-shells '(("ssh" "/bin/bash")
                             ("podman" "/bin/bash"))))

;;; git tooling

;; magical git porcelean
(use-package magit
  :ensure t
  :init
  (setq my/git-prefix-map (make-sparse-keymap))
  (bind-key "C-c g" my/git-prefix-map)
  :config
  (setq magit-bury-buffer-function 'magit-restore-window-configuration
	magit-display-buffer-function 'magit-display-buffer-fullframe-status-v1
	magit-pre-display-buffer-hook 'magit-save-window-configuration
        magit-save-repository-buffers 'dontask)

  ;; hardcode system git for performance on mac
  (when (eq system-type 'darwin)
    (setq magit-git-executable "/usr/bin/git"))

  :hook (after-save . magit-after-save-refresh-status)
  :bind (:map my/git-prefix-map
	 ("b" . #'magit-blame-addition)
	 ("g" . #'magit-status)
	 ("f" .  #'magit-file-dispatch)))

;; forge integration
(use-package sqlite3
  :ensure t)

(use-package forge
  :ensure t
  :after (magit sqlite3))

;; see changes in fringe
(use-package diff-hl
  :ensure t
  :after magit
  :init
  (global-diff-hl-mode)
  :bind (:map diff-hl-command-map
         ("s" . #'diff-hl-show-hunk-stage-hunk)
	 :map my/git-prefix-map
	 ("H" . #'diff-hl-show-hunk-next)
	 ("h" . #'diff-hl-show-hunk-previous)))

;; TODO: make ignored a grey block?
(use-package diff-hl-dired
  :after (dired diff-hl)
  :hook (dired-mode . diff-hl-dired-mode))

;; TODO: drop popup for repeat map?
(use-package diff-hl-inline-popup
  :bind (:map diff-hl-inline-popup-transient-mode-map
         ("s" . #'diff-hl-show-hunk-stage-hunk)))

;; travel through time
(use-package git-timemachine
  :ensure t
  :after magit
  :bind (:map my/git-prefix-map
	 ("t" . #'git-timemachine)))

;; jump to buffer or region in forge
(use-package browse-at-remote
  :ensure t
  :after magit
  :bind (:map my/git-prefix-map
	 ("r" . #'browse-at-remote)))

;; handle conflics with a quick menu
(use-package smerge-mode
  :after (hydra magit)
  :config
  (defhydra my/smerge-hydra
    (:color pink :hint nil :post (smerge-auto-leave))
    "
^Move^       ^Keep^               ^Diff^                 ^Other^
^^-----------^^-------------------^^---------------------^^-------
_n_ext       _b_ase               _<_: upper/base        _C_ombine
_p_rev       _u_pper              _=_: upper/lower       _r_esolve
^^           _l_ower              _>_: base/lower        _k_ill current
^^           _a_ll                _R_efine
^^           _RET_: current       _E_diff
"
    ("n" smerge-next)
    ("p" smerge-prev)
    ("b" smerge-keep-base)
    ("u" smerge-keep-upper)
    ("l" smerge-keep-lower)
    ("a" smerge-keep-all)
    ("RET" smerge-keep-current)
    ("\C-m" smerge-keep-current)
    ("<" smerge-diff-base-upper)
    ("=" smerge-diff-upper-lower)
    (">" smerge-diff-base-lower)
    ("R" smerge-refine)
    ("E" smerge-ediff)
    ("C" smerge-combine-with-next)
    ("r" smerge-resolve)
    ("k" smerge-kill-current)
    ("ZZ" (lambda ()
            (interactive)
            (save-buffer)
            (bury-buffer))
     "Save and bury buffer" :color blue)
    ("q" nil "cancel" :color blue))
  :hook (magit-diff-visit-file
	 . (lambda ()
             (when smerge-mode
               (my/smerge-hydra/body)))))

;;; docker tooling

(use-package dockerfile-mode
  :ensure t)

(use-package docker
  :ensure t
  :bind ("C-c D" . docker))

;;; language server

(use-package lsp-mode
  :ensure t
  :init
  (setq lsp-keymap-prefix "C-c l"
        lsp-enable-file-watchers nil
        lsp-headerline-breadcrumb-enable nil
        lsp-imenu-index-function #'lsp-imenu-create-categorized-index))

(use-package dap-mode
  :ensure t
  :after hydra
  :init
  (setq my/dap-mode-map (make-sparse-keymap)
	dap-auto-configure-features '(sessions locals))
  (bind-key "C-c d" my/dap-mode-map)
  :bind (:map my/dap-mode-map
         ("n" . #'dap-next)
         ("i" . #'dap-step-in)
         ("o" . #'dap-step-out)
         ("c" . #'dap-continue)
         ("h" . #'dap-hydra)
         ("r" . #'dap-debug-restart)
         ("d" . #'dap-debug)
         ("b" . #'dap-breakpoint-toggle)
         ("s" . #'dap-disconnect)))

(use-package dap-python
  :ensure nil
  :after dap-mode
  :config
  (setq dap-python-debugger 'debugpy)
  (dap-register-debug-template
   "Python :: Attach to Dockerized Django"
   (list :name "Python :: Attach to Dockerized Django"
         :type "python"
         :request "attach"
         :connect '(:port 5679 :host "localhost")
         :django t
         :pathMappings '((("localRoot" . "${workspaceFolder}")
                          ("remoteRoot" . "/app/"))))))

;;; data formats

;; conf
(use-package conf-mode
  :after (display-line-numbers)
  :mode ("\\..*ignore\\'" "\\.fish\\'")
  :hook (conf-mode . display-line-numbers-mode))

;; csv
(use-package csv-mode
  :ensure t)

;; yaml
(use-package yaml-mode
  :ensure t
  :after (display-line-numbers)
  ;; yaml doesn't inherit from conf mode
  :hook (yaml-mode . display-line-numbers-mode))

;; toml
(use-package toml-mode
  :ensure t)

;; ini
(use-package ini-mode
  :ensure t)

;;; programming language support

;;; general

;; error reporting
(use-package flymake
  :config
  (define-fringe-bitmap
    'my/caution-tape-bitmap
    (let ((arrow (vector
                  #b11000011
                  #b10000111
                  #b00001111
                  #b00011111
                  #b00111110
                  #b01111100
                  #b11111000
                  #b11110000
                  #b11100001))
          (bitmap (vector)))
      (dotimes (_ (+ 2 (/ my/default-fixed-pitch-height
                          (length arrow)
                          10)))
        (setq bitmap (cl-concatenate 'vector bitmap arrow)))
      bitmap))

  (dolist (var '(flymake-note-bitmap
                 flymake-warning-bitmap
                 flymake-error-bitmap))
    (set var (cons 'my/caution-tape-bitmap
                   (cdr (symbol-value var))))))

;; run .env/rc automagically
(use-package envrc
  :ensure t
  :init
  ;; I'm not ready to turn it on yet
  ;; (envrc-global-mode)
  )

;; display current defun in modeline
(use-package which-func
  :after (prog-mode)
  :hook (prog-mode . which-function-mode))

;; spellcheck strings and comments
(use-package flyspell
  :after (prog-mode markdown-mode org git-commit)
  :init
  (setq flyspell-mode-map (make-sparse-keymap)
        flyspell-mouse-map (make-sparse-keymap))
  :hook ((prog-mode . flyspell-prog-mode)
         ((markdown-mode org-mode git-commit-mode) . flyspell-mode)))

;; improved structural navigation and editing
(use-package puni
  :ensure t
  :bind (:map prog-mode-map
         ("C-M-f" . puni-forward-sexp-or-up-list)
         ("C-M-b" . puni-backward-sexp-or-up-list)
         ;; slurping & barfing
         ("C-}" . puni-barf-forward)
         ("C-)" . puni-slurp-forward)
         ("C-(" . puni-slurp-backward)
         ("C-{" . puni-barf-backward)
         ;; depth changing
         ("C-M-r" . puni-raise)
         ("M-=" . puni-splice)
         ("M-_" . puni-split)
         ("M-<up>" . puni-splice-killing-backward)
         ("M-<down>" . puni-splice-killing-forward)))

;; insert closing delimiters
(use-package elec-pair
  :init
  (electric-pair-mode 1))

;; define keybinding missing in 28?
(use-package emacs
  :bind ("C-M-<backspace>" . #'backward-kill-sexp))

;; completions nice-to-haves
(setq tab-always-indent 'complete
      completion-auto-help 'always
      completion-auto-wrap t
      completion-auto-select 'second-tab)

;; improved context-based completions
(global-set-key [remap dabbrev-expand] #'hippie-expand)

;; completions at cursor because I'm not emacs enough...
(use-package corfu
  :ensure t
  :custom
  (corfu-cycle t)
  ;; (corfu-auto t)
  (corfu-separator ?\s)
  :init
  (global-corfu-mode))

;; symbols for completion swag
(use-package svg-lib
  :ensure t)
(use-package kind-icon
  :ensure t
  :after corfu
  :custom
  (kind-icon-default-face 'corfu-default)
  (kind-icon-use-icons nil)
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

;;; elisp

;;; (guile) scheme
(use-package geiser-mit
  :ensure t)

(use-package geiser-guile
  :after geiser-mit
  :ensure t)

(if (eq system-type 'darwin)
    (add-to-list 'Info-directory-list
                 "/opt/local/share/info/"))

;;; python

;; consult imenu integration
(setq my/lsp-mode-imenu-types
      '((?f "Functions")
        (?F "Fields")
        (?m "Methods")
        (?M "Modules")
        (?c "Classes")
        (?C "Constants")
        (?v "Variables")
        (?e "Enums")
        (?E "Enum Members")
        (?i "Interfaces")
        (?s "Strings")
        (?S "Structs")
        (?n "Numbers")
        (?N "Namespaces")
        (?p "Properties")
        (?P "Packages")
        (?b "Booleans")
        (?a "Arrays")
        (?o "Objects")
        (?O "Operators")
        (?k "Keys")
        (?K "Constructors")
        (?_ "Nulls")
        (?t "Type Parameters")
        (?! "Events")
        (?d "Files")))

;; lsp based imenu for python
(with-eval-after-load 'consult-imenu
  (add-to-list 'consult-imenu-config
               `(python-mode
                 :types ,my/lsp-mode-imenu-types)))

;; lsp based imenu for js
(with-eval-after-load 'consult-imenu
  (add-to-list 'consult-imenu-config
               `(js-mode
                 :types ,my/lsp-mode-imenu-types)))

;; setup pyright
(use-package lsp-pyright
  :ensure t
  :init
  (setq lsp-pyright-disable-organize-imports t
        lsp-pyright-auto-import-completions t
        lsp-pyright-use-library-code-for-types t
        lsp-pyright-diagnostic-mode "onlyOpenFiles")

  (add-hook 'python-mode-hook
            (lambda ()
              (require 'lsp-pyright)
              (lsp))))

;; formatting
(use-package python-black
  :ensure t
  :demand t
  :after python-ts-mode
  :config
  (define-key python-ts-mode-map (kbd "C-c c f") '("format-buffer" . python-black-buffer))
  (define-key python-ts-mode-map (kbd "C-c c F") '("format-region" . python-black-region)))

(use-package python-isort
  :ensure t
  :demand t
  :after python-ts-mode
  :config
  (define-key python-ts-mode-map (kbd "C-c c o") '("organize-imports" . python-isort-buffer)))

;; virtual environments
(use-package pyvenv
  :ensure t
  :after python-ts-mode
  :init
  (setq my/python-venv-map (make-sparse-keymap))
  (define-key python-ts-mode-map (kbd "C-c c v") `("virtualenv" . ,my/python-venv-map))
  (define-key my/python-venv-map (kbd "a") '("activate" . pyvenv-activate))
  (define-key my/python-venv-map (kbd "d") '("de-activate" . pyvenv-deactivate)))

;; test running

;; c

;; enable lsp automatically
(add-hook 'c-mode-hook #'lsp)

;; use k&r style
(setq c-default-style "linux"
      c-basic-offset 4)

;; common-lisp
(use-package sly
  :ensure t
  :init
  (setq inferior-lisp-program "sbcl"))

;; elixir

;; javascript

;; remove horrible mode keymap
(with-eval-after-load 'js
  (define-key js-mode-map (kbd "M-.") #'xref-find-definitions))

;; enable lsp by default
(add-hook 'js-mode-hook #'lsp)

;; format with prettier
(use-package prettier-js
  :ensure t
  :after js
  :commands (prettier-js)
  :init
  (define-key js-base-mode-map (kbd "C-c c f ") #'prettier-js))

;; vue

;; use web-mode for vue since vue-mode is a little jank
(use-package web-mode
  :ensure t
  :config
  (define-derived-mode my/vue-mode web-mode "Vue.js")
  (add-to-list 'auto-mode-alist '("\\.vue\\'" . my/vue-mode)))

;; run lsp automatically
(add-hook 'my/vue-mode-hook #'lsp)

;;; writing

(use-package emacs
  :init
  (setq sentence-end-double-space nil))

(use-package olivetti
  :ensure t
  :hook (((org-mode
           markdown-mode
           message-mode)
          . olivetti-mode)))

;;; markdown

;; markdown mode
(use-package markdown-mode
  :ensure t)

;;; org

;; org config
(use-package org
  :hook (org-mode . org-indent-mode)
  :config
  (setq org-default-notes-file "~/org/notes.org"
        org-startup-with-inline-images nil
        org-return-follows-link t)

  (add-to-list 'org-modules 'org-tempo)

  (add-hook
   'org-mode-hook
   (lambda ()
     (setq-local electric-pair-inhibit-predicate
                 `(lambda (c)
                    (if (char-equal c ?<)
                        t
                      (,electric-pair-inhibit-predicate c))))))

  (org-babel-do-load-languages
   'org-babel-load-languages
   '((shell . t)
     (awk . t)
     (sed . t)
     (python . t)
     (sql . t)
     (js . t)
     (emacs-lisp . t)))

  (setq org-babel-default-header-args:sh
        '((:session . "sh")
          (:results . "verbatim"))))

;; org-capture project notes
(defun my/get-project-notes-or-default-filepath ()
  (or (and (project-current)
           (expand-file-name
            "notes.org"
            (project-root (project-current))))
      (progn
        (message "No project found. Using default notes file.")
        org-default-notes-file)))

(defun my/initialize-org-file-with-headline (filepath headline)
  (progn
    ;; initialize empty file if none exists
    (when (not (file-exists-p filepath))
      (message "File doesn't exist... Initializing...")
      (with-temp-buffer (write-file filepath)))
    ;; add headline
    (with-temp-buffer
      (insert-file-contents filepath)
      (let* ((headline-fmt (concat "* " headline))
             (headline-exists (save-excursion
                                (goto-char (point-min))
                                (search-forward headline-fmt nil t))))
        (when (not headline-exists)
          (message "No existing headline... Adding to file...")
          (save-excursion
            (goto-char (point-max))
            (insert "\n" headline-fmt))
          (write-file filepath))))
    filepath))

(defun my/project-or-default-file+headline (headline)
  `(file+headline
    ,(my/initialize-org-file-with-headline
      (my/get-project-notes-or-default-filepath)
      headline)
    ,headline))

(setq my/project-capture-prefix "p")

(defun my/org-capture-project ()
  (interactive)
  (let* ((my/project-capture-list
          `(("t" "TODO" entry
             ,(my/project-or-default-file+headline "Tasks")
             "* TODO %?\nContext:%a\n")
            ("n" "Note" entry
             ,(my/project-or-default-file+headline "Notes")
             "* %?\nContext:%a\n")))
         (org-capture-templates
          (cl-concatenate
           'list
           org-capture-templates
           (when (project-current)
             (cons
              `(,my/project-capture-prefix "project")
              (mapcar
               (lambda (xs)
                 (cons (concat my/project-capture-prefix
                               (car xs))
                       (cdr xs)))
               my/project-capture-list))))))
    (org-capture)))

;; capture notes on the fly
(use-package org-capture
  :config
  (setq org-capture-templates '())
  (define-key global-map (kbd "M-c") #'my/org-capture-project))

;; rebind capitalize because it's still useful
(define-key global-map (kbd "M-C") #'capitalize-dwim)

;; do http requests with org
(use-package verb
  :ensure t
  :after org
  :init
  (define-key org-mode-map (kbd "C-c C-r") `("verb" . verb-command-map)))

;; import from postman
(use-package impostman
  :ensure t)

;; do so-so presentations with org
(use-package org-present
  :ensure t
  :hook ((org-present-mode . (lambda ()
                               (org-present-big)
                               (org-display-inline-images)
                               (org-present-hide-cursor)
                               (org-present-read-only)))
         (org-present-mode-quit . (lambda ()
                                    (org-present-small)
                                    (org-remove-inline-images)
                                    (org-present-show-cursor)
                                    (org-present-read-write)))))

;;; email

(use-package sendmail
  :init
  (setq send-mail-function 'sendmail-send-it
        sendmail-program "/opt/local/bin/msmtp"
        mail-specify-envelope-from t
        message-sendmail-envelope-from 'header
        mail-envelope-from 'header))

(use-package notmuch
  :ensure t
  :bind (("C-c m c" . #'notmuch-mua-mail)
         ("C-c m i" . #'notmuch)))

;; scratch
(with-eval-after-load 'which-func
  (defun my/kill-new-function-at-point ()
    (interactive)
    (kill-new (which-function))))

(define-advice
    org-change-tag-in-region
    (:around (&rest args) my/dont-move-point)
  (interactive)
  (save-excursion
    (call-interactively (car args) (cdr args))))


(use-package ns-auto-titlebar
  :init
  (when (eq system-type 'darwin)
    (ns-auto-titlebar-mode 1)))

;; text scale adjust is useful

;; load external custom file
(load custom-file)
