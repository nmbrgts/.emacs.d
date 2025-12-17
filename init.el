;; -*- lexical-binding: t; -*-

;;; early config

(setq native-comp-async-report-warnings-errors 'silent)

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

;; better manage emacs transient files
(use-package no-littering
  :ensure t
  :demand t
  :config
  (no-littering-theme-backups))

;; quelpa
(unless (package-installed-p 'quelpa)
  (with-temp-buffer
    (url-insert-file-contents
     "https://raw.githubusercontent.com/quelpa/quelpa/master/quelpa.el")
    (eval-buffer)
    (quelpa-self-upgrade)))

;; quelpa use-package integration
(quelpa
 '(quelpa-use-package
   :fetcher git
   :url "https://github.com/quelpa/quelpa-use-package.git"))
(require 'quelpa-use-package)

;; load private config
(let ((private-config (no-littering-expand-etc-file-name "private.el")))
  (if (file-exists-p private-config)
      (load private-config :no-error)))

;; misc. early settings
(use-package emacs
  :custom
  ((cursor-type 'bar)
   (tab-width 4))
  :init
  (setq inhibit-splash-screen t
        gc-cons-threshold (* 100 1024 1024)
        read-process-output-max (* 5 1024 1024)
        ring-bell-function 'ignore
        tabs-always-indent nil
        use-short-answers t
        kill-buffer-query-functions
        (remq 'process-kill-buffer-query-function
              kill-buffer-query-functions)
        mac-command-modifier 'meta
        mac-option-modifier 'super
        sentence-end-double-space nil)
  (setq-default indent-tabs-mode nil)
  :bind ("C-M-<backspace>" . #'backward-kill-sexp))

;; set up external custom file
(use-package cus-edit
  :init
  (setq custom-file (no-littering-expand-etc-file-name "custom.el")))

;; macos native compilation fix
(use-package comp
  :if (eq system-type 'darwin)
  :custom
  (native-comp-driver-options '("-Wl,-w")))

;; trying to make wrapping less ugly
(use-package adaptive-wrap
  :ensure t
  :hook ((visual-line-mode . adaptive-wrap-prefix-mode))
  :init
  (global-visual-line-mode 1))

;; misc. file settings
(use-package files
  :ensure nil
  :hook (before-save
         . (lambda ()
             (when (not (and (boundp 'markdown-mode)
                             markdown-mode))
               (delete-trailing-whitespace))))
  :init
  (setq
   require-final-newline t
   ;; reduce nuisance prompts
   confirm-nonexistent-file-or-buffer nil
   ;; automatically refresh any file visiting buffers
   revert-without-query '(".*"))
  :bind (("C-r" . #'revert-buffer)
         ("C-x C-r" . #'set-visited-file-name)))

(use-package whitespace
  :bind (("C-c t SPC" . whitespace-mode)))

(use-package emacs
  :ensure nil
  :init
  (setq
   ;; keep transient files tidy
   create-lockfiles nil)

  (defun nmbrgts/recompile-packages ()
    (interactive)
    (byte-recompile-directory package-user-dir nil 'force)))

;; contextually repeat commands with a single key press
(use-package repeat
  :ensure nil
  :init
  (repeat-mode 1))

(use-package simple
  :ensure nil
  :init
  ;; repeat-like behavior when popping mark
  (setq set-mark-command-repeat-pop t)
  (column-number-mode 1)
  (line-number-mode 1)
  :config
  ;; better default behavior for M-SPC
  (global-set-key [remap just-one-space] #'cycle-spacing)
  :bind (("C-c t RET" . #'toggle-word-wrap)
         ("C-c s" . #'scratch-buffer)))

;; delete selected region on insert
(use-package delsel
  :ensure nil
  :init
  (delete-selection-mode 1))

;; update buffers when files change
(use-package autorevert
  :ensure nil
  :init
  ;; keep modeline up to date
  (setq auto-revert-check-vc-info t
        global-auto-revert-non-file-buffers t)
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
  :ensure nil
  :bind ("C-x C-b" . #'ibuffer))

;; helpful window operations

(setq nmbrgts/window-map (make-sparse-keymap))
(bind-key "C-c w" nmbrgts/window-map)

(use-package window
  :ensure nil
  :bind (("C-x O" . (lambda ()
                      (interactive)
                      (setq repeat-map 'other-window-repeat-map)
                      (other-window -1)))
         :map nmbrgts/window-map
         ("s" . #'window-toggle-side-windows)
         ("=" . #'balance-windows)
         :repeat-map nmbrgts/buffer-move-repeat-map
         ("<right>" . #'next-buffer)
         ("<left>" . #'previous-buffer)))

(use-package windmove
  :ensure nil
  :bind ( :map nmbrgts/window-map
          ("<down>" . #'windmove-down)
          ("<up>" . #'windmove-up)
          ("<left>" . #'windmove-left)
          ("<right>" . #'windmove-right)
          :repeat-map nmbrgts/windmove-repeat-map
          ("<down>" . #'windmove-down)
          ("<up>" . #'windmove-up)
          ("<left>" . #'windmove-left)
          ("<right>" . #'windmove-right)))

(use-package winum
  :ensure t
  :init
  (winum-set-keymap-prefix (kbd "C-c w"))
  (winum-mode))

(use-package ace-window
  :ensure t
  :bind ( :map nmbrgts/window-map
          ("a" . #'ace-window)))

(use-package winner
  :ensure nil
  :init
  (setq winner-dont-bind-my-keys t)
  (winner-mode 1)
  :bind ( :map nmbrgts/window-map
          ("u" . #'winner-undo)
          ("r" . #'winner-redo)
          :repeat-map nmbrgts/winner-repeat-map
          ("u" . #'winner-undo)
          ("r" . #'winner-redo)))

;; window display behavior
(use-package window
  :ensure nil
  :config
  ;; adjust transient buffer behaviors to personal preference
  (defun nmbrgts/display-buffer-in-side-window-and-select (buffer alist)
    (if-let ((window (display-buffer-in-side-window buffer alist)))
        (progn
          (select-window window))))

  (setq window-sides-slots '(0 1 1 1)
        display-buffer-alist
        ;; TODO: add rules for pp-eval/expand and geiser
        ;; TODO: create a system for generalizing the display rules I want,
        ;;       so display definitions don't need to be centralized here.
        '(;; top bar interactive
          ("^\\*[[:alnum:]-\.]*\\(shell\\|term\\|eshell\\|vterm\\|eat\\|Python\\|ielm\\)\\*$"
           (nmbrgts/display-buffer-in-side-window-and-select)
           (side . bottom)
           (slot . 1)
           (window-height . 0.30))
          ("^\\*vterm:"
           (display-buffer-reuse-window display-buffer-in-side-window)
           (side . bottom)
           (slot . 1)
           (window-height . 0.30))
          ("^\\*\\gptel-\\(chat\\|agent\\):"
           (nmbrgts/display-buffer-in-side-window-and-select)
           (side . right)
           (slot . 1)
           (window-width . 0.40))
          ;; top bar informational
          ("^\\*\\(Occur\\|Flymake\\|xref\\|grep\\|docker-\\)"
           (nmbrgts/display-buffer-in-side-window-and-select)
           (select. t)
           (side . top)
           (slot . 1)
           (window-height . 0.20))
          ;; side bar information
          ("^\\*\\(\[Hh]elp\\|info\\|documentation\\|Metahelp\\|lsp-help\\|man\\)"
           (nmbrgts/display-buffer-in-side-window-and-select)
           (side . right)
           (slot . 1)
           (window-width . 0.40))
          ;; quick doodling
          ("^\\*\\(.*scratch\\)"
           (nmbrgts/display-buffer-in-side-window-and-select)
           (select. t)
           (side . bottom)
           (slot . 1)
           (window-height . 0.30))
          ;; side bar tooling
          ("^\\*\\( docker\\|compilation\\)"
           (nmbrgts/display-buffer-in-side-window-and-select)
           (side . right)
           (slot . 1)
           (window-width . 0.40))))

  (defun nmbrgts/promote-side-window-buffer (arg)
    (interactive "P")
    (if (not (window-parameter (selected-window) 'window-side))
        (message "Error: Selected window is not a side window!")
      (let ((display-buffer-alist '())
            (buf (current-buffer))
            (arg-num (prefix-numeric-value arg))
            (non-side-window
             (car (seq-filter
                   (lambda (w)
                     (not (window-parameter w 'window-side)))
                   (window-list)))))
        (delete-window)
        (select-window non-side-window)
        (cond ((>= arg-num 16)
               (progn
                 (when (seq-filter
                        (lambda (w)
                          (window-parameter w 'window-side))
                        (window-list))
                   (window-toggle-side-windows))
                 (delete-other-windows)
                 (switch-to-buffer buf)))
              ((>= arg-num 4)
               (switch-to-buffer buf))
              (t
               (select-window (display-buffer buf)))))))

  (setq nmbrgts/last-quit-side-window nil)

  (defun nmbrgts/quit-side-window ()
    (interactive)
    (if (not (window-parameter (selected-window) 'window-side))
        (message "Error: Selected window is no a side window!")
      (progn
        (setq nmbrgts/last-quit-side-window (buffer-name (current-buffer)))
        (delete-window))))

  (defun nmbrgts/revive-side-window ()
    (interactive)
    (if nmbrgts/last-quit-side-window
        (display-buffer nmbrgts/last-quit-side-window)
      (message "Error: No side window to display!")))

  ;; note: this isn't working with some operations like `magit-mode-bury-buffer'
  ;; note: doesn't include lsp treemacs error list
  ;; note: revive should change focus
  ;; note: revive is non-symetrical if window isn't on the display a-list
  (defun nmbrgts/set-last-quit ()
    (if (window-parameter (selected-window) 'window-side)
        (setq nmbrgts/last-quit-side-window (buffer-name (current-buffer)))))

  :hook ((quit-window . nmbrgts/set-last-quit))
  :bind (("s-P" . #'nmbrgts/promote-side-window-buffer)
         ("s-q" . #'nmbrgts/quit-side-window)
         ("s-r" . #'nmbrgts/revive-side-window)))

;; improved help
(use-package helpful
  :ensure t
  :bind (([remap describe-function] . #'helpful-callable)
         ([remap describe-variable] . #'helpful-variable)
         ([remap describe-command] . #'helpful-command)
         ([remap describe-key] . #'helpful-key)
         ("C-c C-d" . #'helpful-at-point)))

;; keycasting
(use-package keycast
  :ensure t
  :demand t
  :bind ("C-c t k" . #'keycast-tab-bar-mode))

;; get examples in help

;; define modal-like menus in a declarational style
(use-package hydra
  :ensure t)

;; improved search and completion functionality
;; vertical suggestions for many minibuffer operations
(use-package vertico
  :ensure t
  :custom
  ((vertico-cycle t))
  :init
  (vertico-mode))

(use-package vertico-buffer
  :when t
  :after vertico
  :custom
  (vertico-buffer-display-action
   '(display-buffer-in-side-window
     (window-height . 0.20)
     (side . top)))
  :init
  (define-advice vertico--setup
      (:after
       (&rest args)
       nmbrgts/vertico-buffer-mode-set-line-format)
    (setq-local
     mode-line-format (propertize
                       "" ;"  * ↑ make selection above ↑ *"
                       'face 'mode-line-emphasis)))

  (vertico-buffer-mode))

;; improved file navigation
(use-package vertico-directory
  :ensure nil
  :after vertico
  :bind ( :map vertico-map
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
  :ensure t
  :after tabspaces
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
         ("M-e" . consult-isearch-history)
         ("M-s e" . consult-isearch-history)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         :map minibuffer-local-map
         ("M-s" . consult-history)
         ("M-r" . consult-history)
         :map project-prefix-map
         ("b" . consult-project-buffer))
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

  (setq consult-narrow-key "<")

  ;; hide full buffer list (still available with "b" prefix)
  (consult-customize consult--source-buffer :hidden t :default nil)
  ;; set consult-workspace buffer list
  (defvar consult--source-workspace
    (list :name "Workspace Buffers"
          :narrow ?w
          :history 'buffer-name-history
          :category 'buffer
          :state #'consult--buffer-state
          :default t
          :items (lambda () (consult--buffer-query
                             :predicate #'tabspaces--local-buffer-p
                             :sort 'visibility
                             :as #'buffer-name)))

    "Set workspace buffer list for consult-buffer.")

  (add-to-list 'consult-buffer-sources 'consult--source-workspace)

  (defvar consult--source-vterm
    (list :name "Vterm Buffers"
          :narrow ?v
          :history 'buffer-name-history
          :category 'buffer
          :state #'consult--buffer-state
          :hidden t
          :default nil
          :items (lambda () (consult--buffer-query
                             :mode #'vterm-mode
                             :sort 'visibility
                             :as #'buffer-name))))

  (add-to-list 'consult-buffer-sources 'consult--source-vterm))

;;; writable grep buffers

(use-package wgrep
  :ensure t
  :demand t)

;;; context menus with embark

(use-package embark
  :ensure t
  :after which-key
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
                 (window-parameters (mode-line-format . none))))

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
              :around #'embark-hide-which-key-indicator))

;; consult integration
(use-package embark-consult
  :ensure t
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

;; which-key

(use-package which-key
  :ensure nil
  :demand t
  :init
  (which-key-mode))

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
        dired-omit-files "\\`[.]?#\\|\\.java\\|snap\\|System\\|\\.ssh\\|\\.gitconfig\\|\\.wget\\|\\.aspell\\|\\.cache\\|\\.lesshst\\|\\.gftp\\|\\.pki\\|\\.gnome\\|VirtualBox\\|master\\.tar\\.gz\\|\\.wine\\|plan9port\\|\\.idm\\|\\.font\\|\\.iso\\|\\.cargo\\|lib\\|amd64\\|\\.gnupg\\|\\.python\\|\\.var\\|\\.local\\|\\`[.][.]?\\'"
        dired-auto-revert-buffer t))

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

(use-package all-the-icons-ibuffer
  :ensure t
  :after (all-the-icons)
  :hook (ibuffer-mode . #'all-the-icons-ibuffer-mode))

;;; mac usability tweaks

;; load system path on startup
(use-package exec-path-from-shell
  :ensure t
  :init
  (exec-path-from-shell-initialize))

;; mouse bad
(use-package inhibit-mouse
  :ensure t
  :custom
  (inhibit-mouse-adjust-mouse-highlight t)
  (inhibit-mouse-adjust-show-help-function t)
  :config
  (if (daemonp)
      (add-hook 'server-after-make-frame-hook #'inhibit-mouse-mode)
    (inhibit-mouse-mode 1))
  :bind ("C-c t m" . #'inhibit-mouse-mode))

;;; themes

(use-package doom-themes
  :ensure t
  :config
  (doom-themes-visual-bell-config))

(use-package modus-themes
  :ensure t
  :config
  (defun nmbrgts/tweak-theme ()
    (modus-themes-with-colors
      (custom-set-faces
       `(mode-line ((,c :box nil)))
       `(mode-line-active ((,c :box nil)))
       `(mode-line-inactive ((,c :box nil)))
       `(mode-line-highlight ((,c :box nil)))
       `(tab-bar-tab ((,c :box nil)))
       `(tab-bar-tab-inactive ((,c :background ,bg-mode-line-inactive
                                   :foreground ,fg-mode-line-inactive
                                   :weight normal
                                   :box nil)))
       `(fringe ((,c :background ,bg-main :foreground ,bg-main)))
       `(keycast-key ((,c :box nil)))
       `(child-frame-border ((,c :background ,constant)))
       `(eros-result-overlay-face ((,c :inherit modus-themes-slant
                                       :foreground ,comment)))
       '(lsp-face-highlight-read ((t (:inherit highlight :weight light))))
       '(lsp-face-highlight-write ((t (:inherit highlight :weight light))))
       '(lsp-face-highlight-textual ((t (:inherit highlight :weight light))))
       `(doom-modeline-bar ((,c :background ,keybind))))))
  :hook (modus-themes-after-load-theme . nmbrgts/tweak-theme))

(use-package ef-themes
  :ensure t)

(use-package fontaine
  :ensure t
  :config
  (setq fontaine-presets
        '((nmbrgts/regular
           :default-family "JetBrains Mono"
           :default-weight light
           :default-height 270
           :fixed-pitch-family "Iosevka Aile"
           :bold-weight light)
          (nmbrgts/small
           :default-family "JetBrains Mono"
           :default-weight light
           :default-height 200
           :fixed-pitch-family "Iosevka Aile"
           :bold-weight light)))
  (fontaine-mode +1)
  (fontaine-set-preset 'nmbrgts/regular)

  (defun nmbrgts/fontaine-toggle()
    (interactive)
    (if (eq fontaine-current-preset 'nmbrgts/regular)
        (fontaine-set-preset 'nmbrgts/small)
      (fontaine-set-preset 'nmbrgts/regular)))

  :bind ("C-c t s" . #'nmbrgts/fontaine-toggle)
  :hook ((modus-themes-after-load-theme
          . (lambda ()
              (fontaine-set-preset fontaine-current-preset)))
         (fontaine-set-preset
          . (lambda ()
              (setq doom-modeline--font-height-cache (make-hash-table)
                    doom-modeline-bar-width (floor
                                             (/ (face-attribute 'default :height)
                                                6)))))))

(use-package frame
  :ensure nil
  :bind (("C-c t f" . #'toggle-frame-maximized)
         ("C-c t F" . #'toggle-frame-fullscreen)))

(use-package doom-modeline
  :ensure t
  :init
  (setq doom-modeline-icon nil
        doom-modeline-percent-position nil
        doom-modeline-position-line-format '("")
        doom-modeline-hud nil
        doom-modeline-modal nil
        doom-modeline-vcs-max-length 40
        doom-modeline-support-imenu nil
        doom-modeline-height 0
        doom-modeline-bar-width 40
        doom-modeline-project-detection 'project
        doom-modeline-battery nil
        doom-modeline-time nil
        doom-modeline-display-misc-in-all-mode-lines nil
        doom-modeline-lsp t
        doom-modeline-workspace-name nil
        doom-modeline-env-version nil
        doom-modeline-buffer-encoding nil
        doom-modeline-window-width-limit 120
        doom-modeline-buffer-file-name-style 'relative-from-project)
  :hook ((after-init . doom-modeline-mode)
         (modus-themes-after-load-theme
          . (lambda ()
              (when (bound-and-true-p doom-modeline-mode)
                (doom-modeline-mode -1)
                (doom-modeline-mode +1))))))

;;; tabs and tabspaces

;; add minimal tab bar
(use-package tab-bar
  :init
  (defun nmbrgts/tab-name-format-function (tab idx)
    (let ((current-p (eq (car tab) 'current-tab)))
      (propertize
       (concat (if tab-bar-tab-hints (format "%d " i) "")
               " "
               (alist-get 'name tab)
               " "
               (or (and tab-bar-close-button-show
                        (not (eq tab-bar-close-button-show
                                 (if current-p 'non-selected 'selected)))
                        tab-bar-close-button)
                   ""))
       'face (funcall tab-bar-tab-face-function tab))))

  (setq tab-bar-close-button-show nil
        tab-bar-new-button-show nil
        tab-bar-tab-hints nil
        tab-bar-auto-width nil
        tab-bar-new-tab-choice "*scratch*"
        tab-bar-tab-name-format-function 'nmbrgts/tab-name-format-function)
  (tab-bar-mode 1)
  (tab-rename "default" 0)
  ;; fix bug with tab-new and side windows
  (define-advice
      tab-bar-new-tab
      (:before (&rest args) nmbrgts/side-window-new-tab-fix)
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
  ;; temp fix remove later
  (defun tabspaces--local-buffer-p (buffer)
    "Return whether BUFFER is in the list of local buffers."
    (or (member (buffer-name buffer) tabspaces-include-buffers)
        (memq buffer (frame-parameter nil 'buffer-list))))
  (project-known-project-roots)
  :bind-keymap ("C-c TAB" . tabspaces-command-map))

(use-package project
  :after tab-bar
  :config
  (defun nmbrgts/project-tab-name ()
    (interactive)
    (tab-bar-rename-tab (project-name (project-current))))
  :bind ( :map project-prefix-map
          ("TAB" . #'nmbrgts/project-tab-name)))

;; TODO: move to early init?
(use-package tool-bar
  :hook
  (after-init . (lambda () (tool-bar-mode -1))))

(use-package scroll-bar
  :init
  (scroll-bar-mode -1))

(use-package menu-bar
  :init
  (unless (memq window-system '(mac ns))
    (menu-bar-mode -1))
  :bind (("C-c t e" . #'toggle-debug-on-error)
         ("C-c t q" . #'toggle-debug-on-quit)))

;; highlight line everywhere
(use-package hl-line
  :init
  (global-hl-line-mode 1)
  :hook ((vterm-mode eshell-mode eat-mode)
         . (lambda ()
             (setq-local global-hl-line-mode nil))))

;; display line numbers for code buffers
(use-package display-line-numbers
  :hook (prog-mode . display-line-numbers-mode)
  :init
  (setq display-line-numbers-widen t
        display-line-numbers-grow-only t
        display-line-numbers-width-start 4))

;; display only left fringe
(use-package fringe
  :demand t
  :init
  (fringe-mode '(8 . 0)))

;;; terminal

;; vterm
(use-package vterm
  :ensure t
  :after project
  :preface
  (defun nmbrgts/-launch-vterm-in-dir ()
    (require 'vterm)
    (defvar vterm-buffer-name) ; needed for dynamic scoping
    (let ((vterm-buffer-name
           (format vterm-buffer-name-string
                   (string-replace "\n" "" (substring (shell-command-to-string "fish_title") 0 -1)))))
      (vterm)))

  (defun nmbrgts/project-vterm ()
    (interactive)

    (let ((default-directory (project-root (project-current t))))
      (nmbrgts/-launch-vterm-in-dir)))

  (defun nmbrgts/dir-vterm ()
    (interactive)
    (nmbrgts/-launch-vterm-in-dir))

  (defun nmbrgts/dwim-vterm ()
    (interactive)
    (if (project-current)
        (call-interactively #'nmbrgts/project-vterm)
      (call-interactively #'nmbrgts/dir-vterm)))
  :init
  (add-to-list 'project-switch-commands '(nmbrgts/project-vterm "Vterm") t)
  (add-to-list 'project-kill-buffer-conditions '(major-mode . vterm-mode))
  :custom
  ((vterm-copy-exclude-prompt t)
   (vterm-always-compile-module t)
   (vterm-disable-bold t)
   (vterm-max-scrollback 100000)
   (vterm-buffer-name-string "*vterm:%s*")
   (vterm-tramp-shells '(("ssh" "/bin/bash")
                         ("podman" "/bin/bash"))))
  :bind (("C-c v" . #'nmbrgts/dwim-vterm)
         ("C-c C-v" . #'nmbrgts/dir-vterm)
         ("C-c M-v" . #'vterm)
         :map project-prefix-map
         ("t" . #'nmbrgts/project-vterm)))

;; emulate a terminal
(use-package eat
  :ensure t)

;;; git tooling

;; magical git porcelean
(use-package magit
  :ensure t
  :init
  (setq nmbrgts/git-prefix-map (make-sparse-keymap))
  (bind-key "C-c g" nmbrgts/git-prefix-map)
  :config
  (setq magit-bury-buffer-function 'magit-restore-window-configuration
        magit-display-buffer-function 'magit-display-buffer-fullframe-status-v1
        magit-pre-display--hook 'magit-save-window-configuration
        magit-save-repository-buffers 'dontask)

  (add-to-list 'project-switch-commands
               '(magit-project-status "Magit" ?m))

  ;; hardcode system git for performance on mac
  (when (eq system-type 'darwin)
    (setq magit-git-executable "/usr/bin/git"))

  :hook (after-save . magit-after-save-refresh-status)
  :bind ( :map nmbrgts/git-prefix-map
          ("b" . #'magit-blame-addition)
          ("g" . #'magit-status)
          ("f" . #'magit-file-dispatch)))

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
  :bind ( :map diff-hl-command-map
          ("s" . #'diff-hl-show-hunk-stage-hunk)
          :map nmbrgts/git-prefix-map
          ("H" . #'diff-hl-show-hunk-next)
          ("h" . #'diff-hl-show-hunk-previous)
          ("n" . #'diff-hl-next-hunk)
          ("p" . #'diff-hl-previous-hunk)
          :repeat-map nmbrgts/diff-hl-repeat-map
          ("n" . #'diff-hl-next-hunk)
          ("p" . #'diff-hl-previous-hunk))
  :hook (magit-post-refresh . #'diff-hl-magit-post-refresh))

;; TODO: make ignored a grey block?
(use-package diff-hl-dired
  :after (dired diff-hl)
  :hook (dired-mode . diff-hl-dired-mode))

;; travel through time
(use-package git-timemachine
  :ensure t
  :after magit
  :bind ( :map nmbrgts/git-prefix-map
          ("t" . #'git-timemachine)))

;; jump to buffer or region in forge
(use-package browse-at-remote
  :ensure t
  :after magit
  :bind ( :map nmbrgts/git-prefix-map
          ("r" . #'browse-at-remote)))

;; TODO: move to transient?
;; handle conflicts with a quick menu
(use-package smerge-mode
  :after (hydra magit)
  :config
  (defhydra nmbrgts/smerge-hydra
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
               (nmbrgts/smerge-hydra/body)))))

(use-package ediff-wind
  :init
  (setq ediff-window-setup-function #'ediff-setup-windows-plain))

;; view todos
(use-package hl-todo
  :ensure t)

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
        lsp-modeline-diagnostics-enable nil
        lsp-modeline-code-actions-enable nil
        lsp-imenu-index-function #'lsp-imenu-create-categorized-index
        ;; list of lsp-mode imenu types for consult-imenu
        nmbrgts/lsp-mode-imenu-types
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
  :hook (lsp-mode
         . (lambda ()
             (require 'consult-imenu)
             (if (not (boundp 'nmbrgts/consult-imenu-lsp-types))
                 (setq-local nmbrgts/consult-imenu-lsp-types
                             `(,major-mode :types ,nmbrgts/lsp-mode-imenu-types)))
             (if lsp-mode
                 (progn
                   (make-local-variable 'consult-imenu-config)
                   (add-to-list 'consult-imenu-config
                                nmbrgts/consult-imenu-lsp-types))
               (setq-local consult-imenu-config
                           (delq nmbrgts/consult-imenu-lsp-types
                                 consult-imenu-config))))))

(use-package consult-lsp
  :after (consult lsp-mode)
  :ensure t
  :bind (("M-g s" . #'consult-lsp-symbols)
         ("M-g d" . #'consult-lsp-diagnostics)))

(use-package dap-mode
  :ensure t
  :after hydra
  :init
  (setq nmbrgts/dap-mode-map (make-sparse-keymap)
        dap-auto-configure-features '(sessions locals))
  (bind-key "C-c d" nmbrgts/dap-mode-map)
  :bind ( :map nmbrgts/dap-mode-map
          ("n" . #'dap-next)
          ("i" . #'dap-step-in)
          ("o" . #'dap-step-out)
          ("c" . #'dap-continue)
          ("h" . #'dap-hydra)
          ("r" . #'dap-debug-restart)
          ("d" . #'dap-debug)
          ("b" . #'dap-breakpoint-toggle)
          ("s" . #'dap-disconnect)))

;;; data formats

;; conf
(use-package conf-mode
  :after (display-line-numbers)
  :mode ("\\..*ignore\\'" "\\.fish\\'" "\\.*rc\\'")
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

;; terraform
(use-package terraform-mode
  :ensure t
  :after (lsp-mode)
  :init
  (add-to-list 'lsp-disabled-clients 'tfls)
  :custom (terraform-indent-level 4)
  :hook ((terraform-mode . outline-minor-mode)
         (terraform-mode . lsp)))

;;; llm / agentic tools

(use-package gptel
  :ensure t
  :config
  (setq gptel-default-mode #'org-mode
        gptel-model 'qwen2.5-coder:3b
        gptel-backend (gptel-make-ollama "gptel-chat:ollama"
                        :host "localhost:11434"
                        :stream t
                        :models '(qwen3-coder:30b
                                  qwen2.5-coder:3b))))

;;; programming language support

;;; general

(use-package treesit
  :config
  (add-to-list 'treesit-extra-load-path "/usr/local/lib/"))

(use-package indent-bars
  :quelpa (indent-bars
           :fetcher github
           :repo "jdtsmith/indent-bars")
  :hook ((modus-themes-after-load-theme
          . (lambda ()
              (when (bound-and-true-p indent-bars-mode)
                (indent-bars-mode -1)
                (indent-bars-mode +1)))))
  :bind ("C-c t i" . indent-bars-mode)
  :init
  (setq
   indent-bars-pattern "."
   indent-bars-width-frac 0.01
   indent-bars-pad-frac 0.01
   indent-bars-zigzag nil
   indent-bars-highlight-current-depth nil
   indent-bars-highlight-current-depth nil))

;; run .env/rc automagically
(use-package envrc
  :ensure t
  :init
  (envrc-global-mode))

;; display current defun in modeline
(use-package which-func
  :after (prog-mode)
  :hook (prog-mode . which-function-mode)
  :config
  (defun nmbrgts/kill-new-function-at-point ()
    (interactive)
    (kill-new (which-function))))

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
  :bind ( :map prog-mode-map
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

;; jump to char
(use-package avy
  :ensure t
  :bind
  (("M-g c" . #'avy-goto-char)
   ("M-g C" . #'avy-goto-char-2)
   ("M-g w" . #'avy-goto-word-1)
   ("M-g l" . #'avy-goto-line)
   :map isearch-mode-map
   ("C-'" . #'avy-isearch)))

;; lint code for common spelling errors (experimental)
(use-package emacs
  :ensure nil
  :init
  (defvar typos-flymake--proc nil)

  (defun typos-flymake--parse-output (source proc report-fn)
    (let ((rx "^-:\\(?1:[0-9]+\\):\\(?2:[0-9]+\\): error: \\(?3:.*\\)$")
          (rowidx 1)
          (colidx 2)
          (msgidx 3))
      (with-current-buffer (process-buffer proc)
        (goto-char (point-min))
        (cl-loop
         while (search-forward-regexp rx nil t)
         for msg = (match-string msgidx)
         ;; TODO: `flymake-diag-region' will return the bounds of then entire
         ;;       identifier. It would be nice to have a function that returns the
         ;;       bounds of the misspelled sub-word.
         for (beg . end) = (flymake-diag-region
                            source
                            (string-to-number
                             (match-string rowidx))
                            (string-to-number
                             (match-string colidx)))
         for type = :note
         collect (flymake-make-diagnostic
                  source beg end type msg)
         into diags
         finally (funcall report-fn diags)))))

  (defun typos-flymake--backend (report-fn &rest _args)
    "Flymake backend for typos-cli"
    (unless (executable-find "typos")
      (error "Cannot find a suitable checker"))

    (when (process-live-p typos-flymake--proc)
      (kill-process typos-flymake--proc))

    (let ((source (current-buffer)))
      (save-restriction
        (widen)
        (setq typos-flymake--proc
              (make-process
               :name "typos-flymake"
               :noquery t
               :connection-type 'pipe
               :buffer (generate-new-buffer " *typos-flymake*")
               :command '("typos" "--format" "brief" "-")
               :sentinel
               (lambda (proc _event)
                 (when (eq 'exit (process-status proc))
                   (unwind-protect
                       (when (with-current-buffer source
                               (eq proc typos-flymake--proc))
                         (typos-flymake--parse-output source proc report-fn))
                     (kill-buffer (process-buffer proc)))))
               ))
        (process-send-region typos-flymake--proc (point-min) (point-max))
        (process-send-eof typos-flymake--proc))))

  (defun typos-flymake--setup ()
    "Enable typos-flymake in the current buffer."
    (add-hook 'flymake-diagnostic-functions #'typos-flymake--backend nil t))
  :hook
  ((prog-mode . flymake-mode)
   (prog-mode . typos-flymake--setup)))

;; insert closing delimiters
(use-package elec-pair
  :init
  (electric-pair-mode 1))

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
  ((corfu-cycle t)
   (corfu-separator ?\s))
  :config
  (add-to-list 'corfu--frame-parameters '(left-fringe . 0))
  :init
  (global-corfu-mode))

(use-package cape
  :ensure t
  :bind ("C-c p" . cape-prefix-map))

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
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter)
  :hook (modus-themes-after-load-theme . kind-icon-reset-cache))

;; snippets

(use-package yasnippet
  :ensure t
  :hook (prog-mode . yas-minor-mode)
  :bind ( :map yas-minor-mode-map
          ("TAB" . nil)))

(use-package yasnippet-snippets
  :after yasnippet
  :ensure t)

(use-package consult-yasnippet
  :after (yasnippet consult)
  :ensure t)

;; compilation improvements

(use-package fancy-compilation
  :ensure t
  :after compile
  :commands (fancy-compilation-mode)
  :config
  (setq fancy-compilation-override-colors t)
  (fancy-compilation-mode)
  (dolist
      (faces
       '((fancy-compilation-default-face . default)
         (fancy-compilation-function-name-face . font-lock-function-name-face)
         (fancy-compilation-line-number-face . line-number)
         (fancy-compilation-column-number-face . shadow)
         (fancy-compilation-info-face . italic)
         (fancy-compilation-warning-face . warning)
         (fancy-compilation-error-face . error)
         (fancy-compilation-complete-success-face . success)
         (fancy-compilation-complete-error-face . error)))
    (set-face-attribute
     (car faces)
     nil
     :background 'unspecified
     :foreground 'unspecified
     :inherit (cdr faces))))


;;; lisp
(use-package paren-face
  :ensure t
  :hook ((lisp-data-mode scheme-mode) . paren-face-mode))

(use-package rainbow-delimiters
  :ensure t
  :bind ("C-c t (" . #'rainbow-delimiters-mode))

;;; elisp

(use-package eros
  :ensure t
  :demand t
  :hook (emacs-lisp-mode . eros-mode))

;;; (guile) scheme
(use-package geiser-mit
  :ensure t)

(use-package geiser-guile
  :after geiser-mit
  :ensure t)

(use-package info
  :init
  (when (eq system-type 'darwin)
    (add-to-list
     'Info-directory-list
     (expand-file-name
      "share/emacs/info"
      (string-trim-right
       (shell-command-to-string "brew --prefix emacs-plus")
       "\n+")))))

;;; python

(use-package python
  :ensure nil
  :custom
  (python-flymake-command nil)
  :init
  (setq python-shell-dedicated 'project)
  (add-to-list 'major-mode-remap-alist
               '(python-mode . python-ts-mode)))

;; setup pyright
(use-package lsp-pyright
  :ensure t
  :init
  (setq lsp-pyright-langserver-command "basedpyright"
        lsp-pyright-disable-organize-imports t
        lsp-pyright-auto-import-completions t
        lsp-pyright-use-library-code-for-types t
        lsp-pyright-multi-root nil
        lsp-pyright-diagnostic-mode nil)
  :hook ((python-mode python-ts-mode)
         . (lambda ()
             (require 'lsp-pyright)
             (lsp))))

(use-package dap-python
  :ensure nil
  :after dap-mode
  :config
  (setq dap-python-debugger 'debugpy)
  (dap-register-debug-template
   "Python :: Remote Attach :: Localhost"
   (list :name "Python :: Remote Attach :: Localhost"
         :type "python"
         :request "attach"
         :connect '(:port 5678 :host "localhost")
         :django t
         :pathMappings '((("localRoot" . "${workspaceFolder}")
                          ("remoteRoot" . "/app/"))))))

;; formatting
(use-package python-black
  :ensure t
  :demand t
  :after python
  :hook ((python-mode python-ts-mode) . python-black-on-save-mode))

(use-package python-isort
  :ensure t
  :demand t
  :after python
  :hook ((python-mode python-ts-mode) . python-isort-on-save-mode))

(use-package reformatter
  :ensure t
  :demand t
  :after python
  :init
  (reformatter-define python-ruff-fix
    :program "ruff"
    :args `("--fix" ,input-file)
    :lighter " ruff-fix"
    :stdin nil
    :stdout nil)

  (reformatter-define python-reorder-python-imports
    :program "reorder-python-imports"
    :args `("--exit-zero-even-if-changed"
            "--application-directories" ,(project-root (project-current))
            "--py39-plus"
            "-")
    :lighter " reorder-python-imports")
  :hook ((python-mode python-ts-mode) . python-ruff-fix-on-save-mode))

;; virtual environments
(use-package pyvenv
  :ensure t
  :after python
  :init
  (setq nmbrgts/python-venv-map (make-sparse-keymap))
  (bind-key "C-c v" nmbrgts/python-venv-map python-mode-map)
  (defun nmbrgts/project-pyenv-activate ()
    (interactive)
    (if-let* ((prj (project-current))
              (prj-root (project-root prj))
              (venv-dir (expand-file-name ".venv" prj-root))
              (venv-dir-valid (file-exists-p venv-dir)))
        (pyvenv-activate venv-dir)
      (message (string-join `("Could not find virtualenv at path: " ,venv-dir)))))
  :bind ( :map nmbrgts/python-venv-map
          ("a" . #'pyvenv-active)
          ("d" . #'pyvenv-deactivate)
          ("p" . #'nmbrgts/project-pyenv-activate)))

;; test running

;; golang

(use-package go-mode
  :ensure t
  :hook ((go-mode . (lambda () (require 'lsp-go) (lsp)))
         (go-mode . subword-mode)))
;; lsp

(use-package lsp-go
  :init
  (setq lsp-go-use-placeholders nil
        lsp-go-use-gofumpt t))

;; formatting

(use-package reformatter
  :ensure t
  :demand t
  :after go-mode
  :init
  (reformatter-define go-gofmt
    :program "~/go/bin/gofumpt"
    :args `("-w" ,input-file)
    :lighter " GOFUPMT"
    :stdin nil
    :stdout nil)

  (reformatter-define go-goimports
    :program "/Users/nmbrgts/go/bin/goimports"
    :args `("-w" ,input-file)
    :lighter " GOIMPORTS"
    :stdout nil
    :stdin nil)

  :hook ((go-mode . go-gofmt-on-save-mode)
         (go-mode . go-goimports-on-save-mode)))

;; interactive coding

(use-package go-scratch
  :ensure t)

(use-package gorepl-mode
  :ensure t)

;; c#

(use-package csharp-mode
  :after (lsp-mode consult-imenu reformatter)
  :init
  (add-to-list 'major-mode-remap-alist
               '(csharp-mode . csharp-ts-mode))
  :hook ((csharp-mode csharp-ts-mode)
         . (lambda ()
             (require 'lsp-csharp)
             (lsp))))

(use-package dotnet
  :ensure t
  :after csharp-mode
  :init
  (setq dotnet-mode-keymap-prefix (kbd "C-c c"))
  :hook ((csharp-mode csharp-ts-mode)
         . dotnet-mode))

;; c

;; enable lsp automatically
(add-hook 'c-mode-hook
          (lambda ()
            (setq-local lsp-enable-on-type-formatting nil)
            (lsp)))

(c-add-style
 "modern-c"
 '("stroustrup"
   (c-basic-offset . 2)
   (indent-tabs-mode . nil)
   (tab-width . 2)
   (c-offsets-alist
    . ((substatement-open . 0)
       (arglist-intro . +)
       (arglist-cont-nonempty . 0)
       (arglist-close . 0)
       (statement-case-open . +)
       (brace-list-open . 0)
       (brace-list-intro . +)
       (brace-list-entry . 0)
       (block-open . 0)
       (inclass . +)
       (inline-open . 0)))))

;; style approximating clang-format's llvm formatting
(setq c-default-style "modern-c"
      lsp-clients-clangd-args '("--header-insertion-decorators=0"
                                "--enable-config"
                                "--log=verbose"))

;; common-lisp
(use-package sly
  :ensure t
  :init
  (setq inferior-lisp-program "sbcl"))

;; elixir

;; javascript
(use-package js
  :after (consult-imenu lsp-mode)
  :config
  (add-to-list 'consult-imenu-config
               `(js-mode
                 :types ,nmbrgts/lsp-mode-imenu-types))
  :hook (js-mode . lsp)
  :bind ( :map js-mode-map
          ("M-." . nil)))

(use-package prettier-js
  :ensure t
  :after js
  :commands (prettier-js)
  :bind ( :map js-mode-map
          ("C-c f b" . #'prettier-js)))

;; vue

;; use web-mode for vue since vue-mode is a little jank
(use-package web-mode
  :ensure t
  :config
  (define-derived-mode nmbrgts/vue-mode web-mode "Vue.js")
  (add-to-list 'auto-mode-alist '("\\.vue\\'" . nmbrgts/vue-mode))
  :hook (nmbrgts/vue-mode . lsp))

;;; writing

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

;;; notes

(use-package denote
  :ensure t
  :demand t
  :init
  (setq nmbrgts/note-keymap (make-sparse-keymap))
  (bind-key "C-c n" nmbrgts/note-keymap)
  :bind ( :map nmbrgts/note-keymap
          ("n" . #'denote-create-note)
          ("l" . #'denote-link)
          ("b" . #'denote-backlinks)
          ("d" . (lambda ()
                   (interactive)
                   (find-file denote-directory)))
          ("D" . #'denote-sort-dired)
          ("f" . #'denote-link-find-file)
          ("r" . #'denote-rename-file))
  :hook (dired-mode . (lambda () (denote-dired-mode))))

;;; org

;; org config
(use-package org
  :hook (org-mode
         . (lambda ()
             (setq-local electric-pair-inhibit-predicate
                         `(lambda (c)
                            (if (char-equal c ?<)
                                t
                              (,electric-pair-inhibit-predicate c))))))
  :config
  (setq org-default-notes-file "~/org/notes.org"
        org-startup-with-inline-images nil
        org-return-follows-link t)

  (add-to-list 'org-modules 'org-tempo)

  (org-babel-do-load-languages
   'org-babel-load-languages
   '((shell . t)
     (awk . t)
     (sed . t)
     (python . t)
     (sql . t)
     (js . t)
     (emacs-lisp . t)
     (verb . t)))

  (setq org-babel-default-header-args:sh
        '((:session . "sh")
          (:results . "verbatim"))))

;; do http requests with org
(use-package verb
  :ensure t
  :after org
  :init
  (bind-key "C-c C-r" verb-command-map org-mode-map))

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

;; load external custom file
(load custom-file :no-error)

;; define theme toggle and load
(use-package emacs
  :after (faces modus-themes ef-themes)
  :config
  ;; my theme selections for toggling
  (setq nmbrgts/light-theme 'ef-melissa-light
        nmbrgts/dark-theme 'ef-melissa-dark
        nmbrgts/active-theme nmbrgts/dark-theme)
  ;; toggle theme
  (defun nmbrgts/theme-toggle (&optional light-or-dark)
    (interactive)
    (setq nmbrgts/active-theme
          (or (and (eq light-or-dark 'light) nmbrgts/light-theme)
              (and (eq light-or-dark 'dark) nmbrgts/dark-theme)
              (and (eq nmbrgts/active-theme nmbrgts/dark-theme) nmbrgts/light-theme)
              (and (eq nmbrgts/active-theme nmbrgts/light-theme) nmbrgts/dark-theme)
              nmbrgts/active-theme))
    (modus-themes-load-theme nmbrgts/active-theme))

  ;; switch themes with system
  (if (and (eq system-type 'darwin)
           (boundp 'ns-system-appearance)
           (boundp 'ns-system-appearance-change-functions))
      (progn
        (message "matching emacs theme to system theme...")
        (add-to-list 'ns-system-appearance-change-functions #'nmbrgts/theme-toggle)
        (nmbrgts/theme-toggle ns-system-appearance))
    (progn
      (message "loading theme...")
      (modus-themes-load-theme nmbrgts/active-theme)))
  :bind ("C-c t t" . #'nmbrgts/theme-toggle)
  :hook ((modus-themes-after-load-theme
          . (lambda ()
              (setq vterm-environment
                    (setenv-internal vterm-environment
                                     "EMACS_THEME"
                                     (format "%s" nmbrgts/active-theme)
                                     t))))
         (modus-themes-after-load-theme
          . (lambda ()
              (mapc
               (lambda (buffer)
                 (when (s-starts-with? "*vterm:"
                                       (buffer-name buffer))
                   (with-current-buffer buffer
                     (vterm-copy-mode -1)
                     (vterm-send-stop)
                     (vterm-send-key "c" nil nil :ctrl)
                     (vterm-insert "")
                     (vterm-send-stop)
                     (vterm-reset-cursor-point)
                     (vterm-send-start)
                     (let ((in-proc (not (vterm--at-prompt-p))))
                       (when in-proc
                         (vterm-send-key "z" nil nil :ctrl)
                         (vterm-insert ""))
                       (vterm-insert (format "fish_config theme choose %s"
                                             (if (eq nmbrgts/active-theme nmbrgts/light-theme)
                                                 "light"
                                               "dark")))
                       (vterm-send-return)
                       (vterm-insert "")
                       (when in-proc
                         (vterm-insert "fg")
                         (vterm-send-return)
                         (vterm-insert ""))))))
               (buffer-list))))))
