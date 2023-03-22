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

;; pls no more nuisance prompts
(setq use-short-answers t
      confirm-nonexistent-file-or-buffer nil
      revert-without-query '(".*")
      kill-buffer-query-functions (remq 'process-kill-buffer-query-function
                                        kill-buffer-query-functions))

;; keep transient files tidy
(setq my/transient-files-backup-dir (expand-file-name "tmp/backups/" user-emacs-directory)
      my/transient-files-auto-save-dir (expand-file-name "tmp/auto-saves/" user-emacs-directory)
      my/transient-files-auto-save-prefix (expand-file-name "sessions" my/transient-files-auto-save-dir))

(make-directory my/transient-files-auto-save-dir t)
(make-directory my/transient-files-backup-dir t)

(setq backup-directory-alist `(("." . ,my/transient-files-backup-dir))
      auto-save-file-name-transforms `((".*" ,my/transient-files-auto-save-dir t))
      auto-save-list-file-prefix my/transient-files-auto-save-prefix
      create-lockfiles nil)

;; update buffers when files change
(global-auto-revert-mode 1)

;; buffer keymap
(let ((map (make-sparse-keymap)))
  (define-key map (kbd "k") #'kill-buffer)
  (define-key map (kbd "b") #'consult-buffer)
  (define-key map (kbd "B") #'ibuffer)
  (define-key map (kbd "n") #'next-buffer)
  (define-key map (kbd "p") #'previous-buffer)
  (define-key map (kbd "r") #'revert-buffer)
  (define-key map (kbd "s") #'scratch-buffer)
  (define-key global-map (kbd "C-c b") `("buffer" . ,map)))

;; window undo/redo
(winner-mode 1)

;; window map
(let ((map (make-sparse-keymap)))
  (define-key map (kbd "k") #'delete-window)
  (define-key map (kbd "K") #'delete-other-windows)
  (define-key map (kbd "s") #'split-window-right)
  (define-key map (kbd "S") #'split-window-below)
  (define-key map (kbd "n") #'windmove-down)
  (define-key map (kbd "p") #'windmove-up)
  (define-key map (kbd "f") #'windmove-right)
  (define-key map (kbd "b") #'windmove-left)
  (define-key map (kbd "B") #'balance-windows)
  (define-key map (kbd "u") #'winner-undo)
  (define-key map (kbd "r") #'winner-redo)
  (define-key map (kbd "p") #'window-toggle-side-windows)
  (define-key global-map (kbd "C-c w") `("window" . ,map)))

;; adjust transient buffer behaviors to personal preference
(defun my/display-buffer-in-side-window-and-select (buffer alist)
  (let ((window (display-buffer-in-side-window buffer alist)))
    (select-window window)))

(setq window-sides-slots '(0 1 1 1)
      display-buffer-alist
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

;; modal editing
(use-package meow
  :ensure t
  :init
  (defun meow-setup ()
    (setq meow-cheatsheet-layout meow-cheatsheet-layout-qwerty)
    ;; (meow-motion-overwrite-define-key
    ;;  '("j" . meow-next)
    ;;  '("k" . meow-prev)
    ;;  '("<escape>" . ignore))
    (meow-leader-define-key
     ;; SPC j/k will run the original command in MOTION state.
     ;; '("j" . "H-j")
     ;; '("k" . "H-k")
     ;; Use SPC (0-9) for digit arguments.
     '("1" . meow-digit-argument)
     '("2" . meow-digit-argument)
     '("3" . meow-digit-argument)
     '("4" . meow-digit-argument)
     '("5" . meow-digit-argument)
     '("6" . meow-digit-argument)
     '("7" . meow-digit-argument)
     '("8" . meow-digit-argument)
     '("9" . meow-digit-argument)
     '("0" . meow-digit-argument)
     '("/" . meow-keypad-describe-key)
     '("?" . meow-cheatsheet))
    (meow-normal-define-key
     '("0" . meow-expand-0)
     '("9" . meow-expand-9)
     '("8" . meow-expand-8)
     '("7" . meow-expand-7)
     '("6" . meow-expand-6)
     '("5" . meow-expand-5)
     '("4" . meow-expand-4)
     '("3" . meow-expand-3)
     '("2" . meow-expand-2)
     '("1" . meow-expand-1)
     '("-" . negative-argument)
     '(";" . meow-reverse)
     '("," . meow-inner-of-thing)
     '("." . meow-bounds-of-thing)
     '("[" . meow-beginning-of-thing)
     '("]" . meow-end-of-thing)
     '("a" . meow-append)
     '("A" . meow-open-below)
     '("b" . meow-back-word)
     '("B" . meow-back-symbol)
     '("c" . meow-change)
     '("d" . meow-delete)
     '("D" . meow-backward-delete)
     '("e" . meow-next-word)
     '("E" . meow-next-symbol)
     '("f" . meow-find)
     '("g" . meow-cancel-selection)
     '("G" . meow-grab)
     '("h" . meow-left)
     '("H" . meow-left-expand)
     '("i" . meow-insert)
     '("I" . meow-open-above)
     '("j" . meow-next)
     '("J" . meow-next-expand)
     '("k" . meow-prev)
     '("K" . meow-prev-expand)
     '("l" . meow-right)
     '("L" . meow-right-expand)
     '("m" . meow-join)
     '("n" . meow-search)
     '("o" . meow-block)
     '("O" . meow-to-block)
     '("p" . meow-yank)
     '("q" . meow-quit)
     '("Q" . meow-goto-line)
     '("r" . meow-replace)
     '("R" . meow-swap-grab)
     '("s" . meow-kill)
     '("t" . meow-till)
     '("u" . meow-undo)
     '("U" . meow-undo-in-selection)
     '("v" . meow-visit)
     '("w" . meow-mark-word)
     '("W" . meow-mark-symbol)
     '("x" . meow-line)
     '("X" . meow-goto-line)
     '("y" . meow-save)
     '("Y" . meow-sync-grab)
     '("z" . meow-pop-selection)
     '("'" . repeat)
     '("<escape>" . ignore)))
  (require 'meow)
  (meow-setup)
  (define-key global-map (kbd "C-c T k") #'meow-global-mode))

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

  (add-hook 'eldoc-documentation-functions #'embark-eldoc-first-target)
  :config
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

;; use which-key instead of embark menus
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
(setq my/default-fixed-pitch-font "Iosevka Fixed"
      my/default-variable-pitch-font "Iosevka Aile")

(defun my/apply-preferred-fonts ()
  (set-face-attribute 'default nil :font my/default-fixed-pitch-font :weight 'light :height 250)
  (set-face-attribute 'fixed-pitch nil :font my/default-fixed-pitch-font :weight 'light :height 250)
  (set-face-attribute 'variable-pitch nil :font my/default-variable-pitch-font :weight 'light :height 250)
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

;;; tabs and tabspaces

;; add minimal tab bar
(setq tab-bar-close-button-show nil
      tab-bar-new-button-show nil
      tab-bar-tab-hints nil)

(tab-bar-mode 1)

(tab-rename "default" 0)

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
  (project-known-project-roots)
  (define-key tab-prefix-map (kbd "s") tabspaces-command-map)
  (define-key project-prefix-map (kbd "p") #'tabspaces-open-or-create-project-and-workspace)
  (define-key project-prefix-map (kbd "P") #'project-switch-project))

;; rename current tab when switching projects within tabspace
(defun my/switch-tab-name-with-project (&rest _)
  (let ((pname (project-name (project-current))))
    (tab-bar-rename-tab pname)))

(advice-add #'project-switch-project :after 'my/switch-tab-name-with-project)

;; tabs open to scratch buffer
;; TODO: it would be neat to set this dynamically
(setq tab-bar-new-tab-choice "*scratch*")

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

;; tab keymap
(with-eval-after-load 'tabspaces
  (let ((tab-map (make-sparse-keymap))
        (tabspace-map (make-sparse-keymap)))
    (define-key global-map (kbd "C-c t") `("tabs/spaces" . ,tab-map))
    (define-key tab-map (kbd "n") #'tab-next)
    (define-key tab-map (kbd "p") #'tab-previous)
    (define-key tab-map (kbd "t") #'tab-switch)
    (define-key tab-map (kbd "k") #'tab-close)
    (define-key tab-map (kbd "K") #'tab-close-other)
    (define-key tab-map (kbd "s") `("tabspaces" . ,tabspace-map))
    (define-key tabspace-map (kbd "c") #'tabspaces-remove-current-buffer)
    (define-key tabspace-map (kbd "C") #'tabspaces-clear-buffers)
    (define-key tabspace-map (kbd "K") #'tabspaces-kill-buffers-close-workspace)
    (define-key tabspace-map (kbd "s") #'tabspaces-switch-or-create-workspace)
    (define-key tabspace-map (kbd "S") #'tab-detach)))

;; move project keymap
(with-eval-after-load 'project
  (define-key global-map (kbd "C-c p") `("project" . ,project-prefix-map)))

;; use project find file if in project
(defun my/dwim-find-file ()
  (interactive)
  (if (project-current)
      (project-find-file)
    (call-interactively #'find-file)))

;; use project dired if in project
(defun my/dwim-dired ()
  (interactive)
  (if (project-current)
      (project-dired)
    (call-interactively #'dired)))

;; file keymap
(with-eval-after-load 'project
  (let ((map (make-sparse-keymap)))
    (define-key global-map (kbd "C-c f") `("file" . ,map))
    (define-key map (kbd "f") #'my/dwim-find-file)
    (define-key map (kbd "F") #'find-file)
    (define-key map (kbd "d") #'my/dwim-dired)
    (define-key map (kbd "D") #'dired)
    (define-key map (kbd "j") #'dired-jump)
    (define-key map (kbd "s") #'save-buffer)
    (define-key map (kbd "r") #'rename-visited-file)))

;; remove tool bar and scroll bar
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; highlight line everywhere
(global-hl-line-mode 1)

;; ... except for in vterm and eshell
(defun my/disable-global-hl-line-mode ()
  (setq-local global-hl-line-mode nil))

(dolist (hook '(vterm-mode-hook
                eshell-mode-hook))
  (add-hook hook #'my/disable-hl-line-mode))

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

  (define-key global-map (kbd "C-c T t") #'modus-themes-toggle))

;; pretty mode line
(use-package mood-line
  :ensure t
  :config
  (setq mood-line-glyph-alist  mood-line-glyphs-fira-code)
  (mood-line-mode))

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
  (setq vterm-copy-exclude-prompt t)
  (setq vterm-max-scrollback 100000)
  (setq vterm-tramp-shells '(("ssh" "/bin/bash")
                             ("podman" "/bin/bash"))))

;;; git tooling

;; keymap
(setq my/git-prefix-map (make-sparse-keymap))

;; mnemonic binding
(define-key global-map (kbd "C-c g") `("git" . ,my/git-prefix-map))

;; meow friendly binding
(define-key global-map (kbd "C-c G") `("git" . ,my/git-prefix-map))

;; magical git porcelean
(use-package magit
  :ensure t
  :config
  (setq magit-bury-buffer-function 'magit-restore-window-configuration
	magit-display-buffer-function 'magit-display-buffer-fullcolumn-most-v1
	magit-pre-display-buffer-hook 'magit-save-window-configuration)

  ;; hardcode system git for performance on mac
  (when (eq system-type 'darwin)
    (setq magit-git-executable "/usr/bin/git"))

  (add-hook 'after-save-hook 'magit-after-save-refresh-status t)

  (define-key my/git-prefix-map (kbd "b") #'magit-blame-addition)
  (define-key my/git-prefix-map (kbd "g") #'magit-status)
  (define-key my/git-prefix-map (kbd "f") #'magit-file-dispatch))

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
  (define-key my/git-prefix-map (kbd "H") #'diff-hl-show-hunk-next)
  (define-key my/git-prefix-map (kbd "h") #'diff-hl-show-hunk-previous))

(use-package diff-hl-inline-popup
  :ensure f
  :config
  (define-key diff-hl-inline-popup-transient-mode-map (kbd "s") #'diff-hl-show-hunk-stage-hunk))

;; travel through time
(use-package git-timemachine
  :ensure t
  :config
  (define-key my/git-prefix-map (kbd "t") #'git-timemachine))

;; jump to buffer or region in forge
(use-package browse-at-remote
  :ensure t
  :config
  (define-key my/git-prefix-map (kbd "r") #'browse-at-remote))

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

  (define-key my/git-prefix-map (kbd "s") '("smerge hydra" . hydra-smerge/body)))

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

;; use treesit tsx mode if possible
(when my/treesit-enabled
  (add-to-list 'major-mode-remap-alist '(js-mode . js-ts-mode)))

;; remove horrible mode keymap
(with-eval-after-load 'js
  (define-key js-mode-map (kbd "M-.") #'xref-find-definitions))

;; enable eglot by default
(dolist (hook '(js-ts-mode-hook
                js-mode-hook))
  (add-hook hook #'eglot-ensure))

;; format with prettier
(use-package prettier-js
  :ensure t
  :after js
  :commands (prettier-js)
  :init
  (define-key js-mode-map (kbd "C-c c f ") #'prettier-js)
  (define-key js-ts-mode-map (kbd "C-c c f") #'prettier-js))

;; vue

;; use web-mode for vue since vue-mode is a little jank
(use-package web-mode
  :ensure t
  :config
  (define-derived-mode my/vue-mode web-mode "Vue.js")
  (add-to-list 'auto-mode-alist '("\\.vue\\'" . my/vue-mode)))

;; run eglot automatically
(add-hook 'my/vue-mode-hook #'eglot-ensure)

;; register language server for vue
(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs '(my/vue-mode . ("vls" "--stdio"))))

;;; org

;; load external custom file
(load custom-file)
