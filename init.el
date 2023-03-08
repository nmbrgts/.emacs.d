;;; general usability

;; set up external custom file
(setq custom-file (locate-user-emacs-file "custom.el"))

;; macos native compilation fix
(when (eq system-type 'darwin)
  (customize-set-variable 'native-comp-driver-options '("-Wl,-w")))

;; lexical binding
(setq lexical-binding t)

;; set up package archives
(require 'package)

(dolist (archive '(("nongnu" . "https://elpa.nongnu.org/nongnu/")
		   ("melpa" . "https://melpa.org/packages/")
		   ("org" . "https://orgmode.org/elpa/")))
  (add-to-list 'package-archives archive))

;; pull latest package info
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; for pre 29 emacs
(unless (package-installed-p 'use-package)
  (package-install 'use-package)
  (require 'use-package))

;; keep yes/no prompts brief
;; TODO: this doesn't work?
;; (defalias 'yes-or-no-p 'y-or-no-p)

;; pls no more ring
(setq ring-bell-function 'ignore)

;; pls no more C-M-i
(setq tabs-always-indent 'complete)

;; pls no more tabs
(setq-default indent-tabs-mode nil)

;; pls no more ugly wrap
(use-package adaptive-wrap
  :ensure t
  :hook (visual-line-mode . adaptive-wrap-prefix-mode)
  :init
  (global-visual-line-mode 1))

;; pls no more dangling ws
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; pls delete highlighted regions on input
(delete-selection-mode 1)

;; keep transient files tidy
(setq tn/transient-files-backup-dir (expand-file-name "tmp/backups/" user-emacs-directory)
      tn/transient-files-auto-save-dir (expand-file-name "tmp/auto-saves/" user-emacs-directory)
      tn/transient-files-auto-save-prefix (expand-file-name "sessions" tn/transient-files-auto-save-dir))

(make-directory tn/transient-files-auto-save-dir t)
(make-directory tn/transient-files-backup-dir t)

(setq backup-directory-alist `(("." . ,tn/transient-files-backup-dir))
      auto-save-file-name-transforms `((".*" ,tn/transient-files-auto-save-dir t))
      auto-save-list-file-prefix tn/transient-files-auto-save-prefix
      create-lockfiles nil)

;; update buffers when files change
(global-auto-revert-mode 1)

;; adjust transient buffer behaviors to personal preference
(defun my/display-buffer-in-side-window-and-select (buffer alist)
  (let ((window (display-buffer-in-side-window buffer alist)))
    (select-window window)))

(setq window-sides-slots '(0 1 1 1)
      display-buffer-alist
      '(;; top bar interactive
        ("^\\*[[:alnum:]-]*\\(shell\\|term\\|eshell\\|vterm\\|Python\\)\\*$"
         (my/display-buffer-in-side-window-and-select)
         (side . top)
         (slot . 1)
         (window-height . 15))
        ;; top bar informational
        ("^\\*\\(Occur\\|Flymake\\|xref\\|grep\\|docker-\\)"
         (my/display-buffer-in-side-window-and-select)
         (select. t)
         (side . top)
         (slot . 1)
         (window-height . 15))
        ;; side bar information
        ("^\\*\\(\[Hh]elp\\|info\\|documentation\\|Metahelp\\)"
         (my/display-buffer-in-side-window-and-select)
         (side . right)
         (slot . 1)
         (window-width . 0.25))
        ("^\\*\\( docker container log\\)"
         (my/display-buffer-in-side-window-and-select)
         (side . right)
         (slot . 1)
         (window-width . 0.5))))

;; get helpful prompts for keys
(use-package which-key
  :ensure t
  :init
  (which-key-mode))

;; put minibuffer near top
(use-package mini-frame
  :ensure t
  :custom
  (mini-frame-show-parameters
   '((top . 0.2)
     (width . 0.7)
     (left . 0.5)))
  :init
  (mini-frame-mode))

;; define modal-like menus in a declarational style
(use-package hydra
  :ensure t)

;; improved search and completion functionality
;; vertical suggestions for many minibuffer operations
(use-package vertico
  :ensure t
  :init
  (vertico-mode)
  (setq vertico-cycle t))

;; additional data with suggestions
(use-package marginalia
  :ensure t
  :bind (("M-A" . marginalia-cycle)
         :map minibuffer-local-map
         ("M-A" . marginalia-cycle))
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

;;; mac usability tweaks

;; load system path on startup
(use-package exec-path-from-shell
  :ensure t
  :init
  (exec-path-from-shell-initialize))

;; comand -> meta and option -> super
(setq mac-command-modifier 'meta
      mac-option-modifier 'super)

;;; visual tweaks

;; preferred fonts
(setq tn/default-fixed-pitch-font "Iosevka Fixed"
      tn/default-variable-pitch-font "Iosevka Aile")

(defun my/apply-preferred-fonts ()
  (set-face-attribute 'default nil :font tn/default-fixed-pitch-font :weight 'light :height 250)
  (set-face-attribute 'fixed-pitch nil :font tn/default-fixed-pitch-font :weight 'light :height 250)
  (set-face-attribute 'variable-pitch nil :font tn/default-variable-pitch-font :weight 'light :height 250)
  (set-face-attribute 'bold nil :weight 'normal))

;; create after theme hook
(defvar after-enable-theme-hook nil
  "Normal hook run after enabling a theme.")

(defun run-after-enable-theme-hook (&rest _args)
  "Run `after-enable-theme-hook'."
  (run-hooks 'after-enable-theme-hook))

(advice-add 'enable-theme :after #'run-after-enable-theme-hook)

;; run font tweaks after theme change
(add-hook 'after-enable-theme-hook #'my/apply-preferred-fonts)

;; add minimal tab bar
(setq tab-bar-close-button-show nil
      tab-bar-new-button-show nil
      tab-bar-tab-hints nil)

(tab-bar-mode 1)

(tab-rename "welcome" 0)

;; move tab-bar-map
(define-key global-map (kbd "M-t") tab-prefix-map)

;; bind tab in a similar way to buffers
(define-key global-map (kbd "C-x t") #'tab-bar-switch-to-tab)

;; isolate tabs
(use-package tabspaces
  :ensure t
  :hook (after-init . tabspaces-mode) ;; use this only if you want the minor-mode loaded at startup.
  :commands (tabspaces-switch-or-create-workspace
             tabspaces-open-or-create-project-and-workspace)
  :custom
  (tabspaces-use-filtered-buffers-as-default t)
  (tabspaces-default-tab "Default")
  (tabspaces-remove-to-default t)
  (tabspaces-include-buffers '("*scratch*"))
  ;; sessions
  ;; (tabspaces-session t)
  ;; (tabspaces-session-auto-restore t)
  :config
  (define-key tab-prefix-map (kbd "s") tabspaces-command-map)
  (define-key project-prefix-map (kbd "p") #'tabspaces-open-or-create-project-and-workspace)
  (define-key project-prefix-map (kbd "P") #'project-switch-project))

;; rename current tab when switching projects within tabspace
;; TODO: Can this integrate with tabspace buffer narrowing?
(defun my/switch-tab-name-with-project (&rest _)
  (let ((pname (project-name (project-current))))
    (tab-bar-rename-tab pname)))

(advice-add #'project-switch-project :after 'my/switch-tab-name-with-project)

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

;; remove tool bar and scroll bar
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; highlight line everywhere
(global-hl-line-mode 1)

;; display line numbers for code buffers
(add-hook 'prog-mode-hook #'display-line-numbers-mode)

;; color theme
(use-package modus-themes
  :ensure t
  :config
  (setq modus-themes-italic-constructs nil
        modus-themes-bold-constructs nil
        modus-themes-common-palette-overrides
        '((border-mode-line-active bg-mode-line-active)
          (border-mode-line-inactive bg-mode-line-inactive)))

  (load-theme 'modus-operandi t)

  (define-key global-map (kbd "C-c t t") #'modus-themes-toggle))

;; pretty mode line
(use-package mood-line
  :ensure t
  :config
  (setq mood-line-glyph-alist  mood-line-glyphs-fira-code)
  (mood-line-mode))

;;; git tooling

;; magical git porcelean
(use-package magit
  :ensure t
  :config
  (setq magit-bury-buffer-function 'magit-restore-window-configuration
	magit-display-buffer-function 'magit-display-buffer-fullcolumn-most-v1
	magit-pre-display-buffer-hook 'magit-save-window-configuration)

  (add-hook 'after-save-hook 'magit-after-save-refresh-status t)

  (define-key global-map (kbd "C-c g b") #'magit-blame-addition)
  (define-key global-map (kbd "C-c g g") #'magit-status)
  (define-key global-map (kbd "C-c g f") #'magit-file-dispatch))

;; forge integration
(use-package forge
  :ensure t
  :after magit)

;; see changes in fringe
(use-package diff-hl
  :ensure t
  :init
  (global-diff-hl-mode)
  :config
  (define-key diff-hl-command-map (kbd "s") #'diff-hl-show-hunk-stage-hunk)
  (define-key global-map (kbd "C-c g H") #'diff-hl-show-hunk-next)
  (define-key global-map (kbd "C-c g h") #'diff-hl-show-hunk-previous))

(use-package diff-hl-inline-popup
  :ensure f
  :config
  (define-key diff-hl-inline-popup-transient-mode-map (kbd "s") #'diff-hl-show-hunk-stage-hunk))

;; travel through time
(use-package git-timemachine
  :ensure t
  :config
  (define-key global-map (kbd "C-c g t") #'git-timemachine))

;; jump to buffer or region in forge
(use-package browse-at-remote
  :ensure t
  :config
  (define-key global-map (kbd "C-c g r") #'browse-at-remote))

;; handle conflics with a quick menu
(use-package smerge-mode
  :ensure f
  :after hydra
  :config
  (defhydra hydra-smerge (:color pink
                                 :hint nil
                                 :pre (smerge-mode 1)
                                 ;; Disable `smerge-mode' when quitting hydra if
                                 ;; no merge conflicts remain.
                                 :post (smerge-auto-leave))
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
    ("q" nil "cancel" :color blue))

  (define-key global-map (kbd "C-c g s") '("smerge hydra" . hydra-smerge/body)))

;;; tree-sitter tweaks

;; add mac-ports installed tree-sitter grammars
(setq treesit-extra-load-path '("/opt/local/lib"))

;; create a flag for checking for treesit
(setq my/treesit-enabled  (and (fboundp 'treesit-available-p)
                               (treesit-available-p)))

;;; language server

;; shared keymap setup
(setq my/code-keymap (make-sparse-keymap))
(define-key prog-mode-map (kbd "C-c c") `("code" . ,my/code-keymap))

;; setup eglot
(use-package eglot
  :ensure t  ;; ensure required until 29
  :after project
  :custom
  (eglot-autoshutdown
   t
   "Automatically shutdown when there are no buffers to manage")
  (eglot-extend-to-xref
   t
   "Apply eglot on out-of-project xref jumps")
  :init
  (setq my/lsp-keymap (make-sparse-keymap))

  ;; common use lsp operations
  (define-key my/code-keymap (kbd "l") `("lsp" . ,my/lsp-keymap))
  (define-key my/lsp-keymap (kbd "c") '("lsp-connect" . eglot-reconnect))
  (define-key my/lsp-keymap (kbd "s") '("lsp-shutdown" . eglot-shutdown))

  ;; lsp powered code actions
  (define-key my/code-keymap (kbd "r") '("rename-symbol" . eglot-rename))
  (define-key my/code-keymap (kbd "e") '("error-list" . flymake-show-buffer-diagnostics))
  (define-key my/code-keymap (kbd "d") '("jump-to-definition" . xref-find-definitions))
  (define-key my/code-keymap (kbd "n") '("navigate" . imenu))
  (define-key my/code-keymap (kbd "g") '("jump-to-references" . xref-find-references)))

;;; language support

;;; general

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
              ("M-r" . puni-raise)
              ("M-=" . puni-splice)
              ("M-_" . puni-split)
              ("M-<up>" . puni-splice-killing-backward)
              ("M-<down>" . puni-splice-killing-forward)))

;; insert closing delimiters
(electric-pair-mode 1)

;; completions nice-to-haves
(setq tab-always-indent 'complete
      completion-auto-help 'always
      completion-auto-wrap t
      completion-auto-select 'second-tab)

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

;;; python

;; use tree-sitter mode by default
(when my/treesit-enabled
  (add-to-list 'major-mode-remap-alist '(python-mode . python-ts-mode)))

;; start eglot automatically
(when my/treesit-enabled
  (add-hook 'python-ts-mode-hook #'eglot-ensure))

(add-hook 'python-mode-hook #'eglot-ensure)

;; make eglot unquestioningly use pyright
(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
               '((python-ts-mode python-mode)
                 . ("pyright-langserver" "--stdio"))))

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

;; use tree-sitter mode by default
(when my/treesit-enabled
  (add-to-list 'major-mode-remap-alist '(c-mode . c-ts-mode)))

;; enable eglot automatically
(when my/treesit-enabled
  (add-hook 'c-ts-mode-hook #'eglot-ensure))

;; use k&r style
(setq c-default-style '((java-mode . "java")
                        (awk-mode . "awk")
                        (other . "k&r")))


;; elixir

;; javascript

;;; org

;; load external custom file
(load custom-file)
