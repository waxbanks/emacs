;;; -*- lexical-binding: t; -*-

;;; Emfy 0.2.0 <https://github.com/susam/emfy>

;;;;;;;;;; everything not marked wgh, at this moment, is from emfy

;; tk TODO:
;; define Cmd-Backspace as, or bind it to, 'delete the line up to this point'

;; reducing startup time, but this MUST be reset at the end of the process
(setq gc-cons-threshold most-positive-fixnum)

;; suppress warnings about package cl being deprecated, until i can track down what's generating them
(setq byte-compile-warnings '(cl-functions))

(require 'use-package)
(setq use-package-always-ensure t)

(use-package benchmark-init
  :ensure t
  :config
  ;; To disable collection of benchmark data after init is done.
  (add-hook 'after-init-hook 'benchmark-init/deactivate))

(use-package s)
(use-package dash)

;; why not
(setq global-mark-ring-max 32)

(desktop-save-mode 1)

(defun my-markdown-mode-final-newline-config-fn ()
  (setq require-final-newline nil))

(add-hook 'markdown-mode-hook 'my-markdown-mode-final-newline-config-fn) ;; so emacs doesn't add a final newline every fucking time we save a md file

;;(debug-on-entry 'diminish)

(quelpa
 '(quelpa-use-package
   :fetcher git
   :url "https://github.com/quelpa/quelpa-use-package.git"))
(require 'quelpa-use-package)

(require 'diminish)

(setq org-directory "~/org")

(use-package burly
  :defer t
  :quelpa (burly :fetcher github :repo "alphapapa/burly.el"))

;; keep the .emacs.d space clean, to aid in syncing files
(use-package no-littering
  :ensure t)

(add-to-list 'load-path "~/.emacs.d/lisp")

(global-set-key (kbd "s-T") 'ns-popup-font-panel) ;; so we can bind Cmd-t to a tab-related function


;; preliminaries

;; use ESC instead of C-g to quit/break/cancel -- works b/c i don't use it to sub for Option (i.e. M-)

(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; delete selection works more or less like every other editor,
;; except undoing 'select and overwrite' requires two undo steps--wgh
;; the behaviour on this varies by syntax, esp. if paredit is enabled
(delete-selection-mode 1)
(make-variable-buffer-local 'transient-mark-mode)
(put 'transient-mark-mode 'permanent-local t)
(setq-default transient-mark-mode t)

;; don't show stray whitespace.
(setq-default show-trailing-whitespace nil)
(setq-default indicate-empty-lines t)
(setq-default indicate-buffer-boundaries 'left)

;; Consider a period followed by a single space to be end of sentence.
(setq sentence-end-double-space nil)

;; don't put cursor at border of window just because we're at fill-column
;; the trouble here is that it makes the placement of a word ending right at the
;; fill column VARIABLE -- i consider this incorrect, buggy behaviour but maybe
;; i can get used to it?
;; -- not setting this yet but it's here TK if i need it TODO
(setq overflow-newline-into-fringe nil)

;; Use spaces, not tabs, for indentation.
(setq-default indent-tabs-mode nil)

;; Display the distance between two tab stops as 4 characters wide.
(setq-default tab-width 4)

;; Indentation setting for various languages.
(setq-default c-basic-offset 4)
(setq-default js-indent-level 2)
(setq-default css-indent-offset 2)

;; Highlight matching pairs of parentheses.
(setq show-paren-delay 0)
(show-paren-mode)

;; Write auto-saves and backups to separate directory.
(make-directory "~/.tmp/emacs/auto-save/" t)
(setq auto-save-file-name-transforms '((".*" "~/.tmp/emacs/auto-save/" t)))
(setq backup-directory-alist '(("." . "~/.tmp/emacs/backup/")))

;; Do not move the current file while creating backup.
(setq backup-by-copying t)

;; Disable lockfiles.
(setq create-lockfiles nil)

;; Workaround for https://debbugs.gnu.org/34341 in GNU Emacs <= 26.3.
(when (and (version< emacs-version "26.3") (>= libgnutls-version 30603))
  (setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3"))

;; Write customizations to a separate file instead of this file.
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file t)

;; Enable installation of packages from MELPA.
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
;; (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; Install packages.
(dolist (package '(markdown-mode paredit rainbow-delimiters undo-tree spaceline diminish yasnippet expand-region which-key flyspell hl-todo web-mode))
  (unless (package-installed-p package)
    (package-install package)))

(use-package paredit
  :ensure t
  :defer t
  :diminish paredit-mode)

(use-package company
  :ensure t
  :diminish company-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; spellchecking w/aspell as backend for flyspell ;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(use-package flyspell
  :ensure t
  :defer t
  :diminish flyspell-mode
  :init
  (add-hook 'prog-mode-hook 'flyspell-prog-mode)

  (dolist (hook '(text-mode-hook org-mode-hook))
    (add-hook hook (lambda () (flyspell-mode 1))))

  (dolist (hook '(change-log-mode-hook log-edit-mode-hook org-agenda-mode-hook))
    (add-hook hook (lambda () (flyspell-mode -1))))

  :config
  (setq ispell-program-name "/usr/local/bin/aspell"
        ispell-local-dictionary "en_US"
        ispell-dictionary "american" ; better for aspell
        ispell-extra-args '("--sug-mode=ultra" "--lang=en_US")
        ispell-list-command "--list"
        ispell-local-dictionary-alist '(("en_US" "[[:alpha:]]" "[^[:alpha:]]" "['‘’]"
                                      t ; Many other characters
                                      ("-d" "en_US") nil utf-8)))
  (defun flyspell-detect-ispell-args (&optional run-together)
    "if RUN-TOGETHER is true, spell check the CamelCase words."
    (let (args)
      (setq args (list "--sug-mode=ultra" "--lang=en_US"))
      (if run-together
          (setq args (append args '("--run-together" "--run-together-limit=5" "--run-together-min=2"))))
      args)))


(defun flyspell-goto-previous-error (arg)
  "Go to ARG previous spelling error."
  (interactive "p")
  (while (not (= 0 arg))
    (let ((pos (point))
          (min (point-min)))
      (when (and (eq (current-buffer) flyspell-old-buffer-error)
                 (eq pos flyspell-old-pos-error))
        (if (= flyspell-old-pos-error min)
            ;; goto beginning of buffer
            (progn
              (message "Restarting from end of buffer")
              (goto-char (point-max)))
          (backward-word 1))
        (setq pos (point)))

      ;; seek the next error
      (while (and (> pos min)
                  (let ((ovs (overlays-at pos))
                        (r '()))
                    (while (and (not r) (consp ovs))
                      (if (flyspell-overlay-p (car ovs))
                          (setq r t)
                        (setq ovs (cdr ovs))))
                    (not r)))
        (backward-word 1)
        (setq pos (point)))
      ;; save the current location for next invocation
      (setq arg (1- arg))
      (setq flyspell-old-pos-error pos)
      (setq flyspell-old-buffer-error (current-buffer))
      (goto-char pos)
      (if (= pos min)
          (progn
            (message "No more miss-spelled words!")
            (setq arg 0))))))

(use-package flyspell-correct
  :after flyspell
  :defer t
  :bind (:map flyspell-mode-map ("C-;" . flyspell-correct-wrapper)) ;; this plus flyspell-correct-ivy will do the right thing
  )

(use-package flyspell-correct-ivy
  :after flyspell-correct)

;; flow:
;; 1. C-, to go forward to the next misspelling
;; 2. C-; to bring up ivy buffer suggesting corrections
;; 3. M-o (yes, the window-nav shortcut) to pop up ACTIONS
;; 4. s to save, etc.
;; of all the flyspell-correct wrappers, ivy is the only one requiring two screens
;; helm is preferable, but so is avy -- i might try that

;; swapping default next-error for backtracking to error before point, as it'll be more useful
;; (global-set-key (kbd "C-,") 'flyspell-correct-word-before-point) ;; tk confirm
;; (global-set-key (kbd "C-<") 'flyspell-goto-next-error) ;; tk confirm
;; C-c $ is the awful binding for flyspell-correct-word-before-point, fuck that lunacy

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package centered-cursor-mode
  :ensure t)

(use-package all-the-icons-ivy-rich
  :ensure t
  :init (all-the-icons-ivy-rich-mode 1))

(use-package ivy-rich
  :ensure t
  :init (ivy-rich-mode 1))


(use-package ivy-rich
  :ensure t
  :defer t
  :config (ivy-rich-mode 1)
  (setcdr (assq t ivy-format-functions-alist) #'ivy-format-function-line))

;; visual exception indicator instead of that fucking beep on (e.g.) C-g
;; cf. https://www.emacswiki.org/emacs/AlarmBell --wgh
(setq visible-bell nil
      ring-bell-function 'double-flash-mode-line)
(defun double-flash-mode-line ()
  (let ((flash-sec (/ 1.0 20)))
    (invert-face 'mode-line)
    (run-with-timer flash-sec nil #'invert-face 'mode-line)
    (run-with-timer (* 2 flash-sec) nil #'invert-face 'mode-line)
    (run-with-timer (* 3 flash-sec) nil #'invert-face 'mode-line)))

;; C-x C-r to open a buffer of your 25 most recently visited files
(recentf-mode 1)
(setq-default recentf-max-menu-items 25)
(setq-default recentf-max-saved-items 25)
;; (global-set-key "\C-x\ \C-r" 'recentf-open-files)

;; Collection of Ridiculously Useful eXtensions
(unless (package-installed-p 'crux)
  (package-refresh-contents)
  (package-install 'crux))
(global-set-key (kbd "s-<backspace>") #'crux-kill-line-backwards) ;; tk confirm
;; was using crux version but counsel-recentf is richer
;; (global-set-key (kbd "s-r") #'crux-recentf-find-file)
(global-set-key (kbd "s-r") #'counsel-recentf)
;; (global-set-key (kbd "C-k") #'crux-smart-kill-line)
;; (crux-with-region-or-line comment-or-uncomment-region)

;; replacing the following with ace-window
;; (global-set-key (kbd "M-o") #'crux-other-window-or-switch-buffer)
(use-package ace-window ;; like avy for windows
  :defer t
  :ensure t)
(global-set-key (kbd "M-o") 'ace-window)
(setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)) ;; for ergonomics, else 12345
(setq aw-dispatch-always t)
;;; big ace-window leading characters (i.e. a, s, d, f, etc.)
(custom-set-faces
 '(aw-leading-char-face
((t (:foreground "red" :weight bold :height 3.0)))))


;; use ibuffer instead of buffer-menu
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; sane undo --wgh
(global-undo-tree-mode)
(setq-default undo-tree-auto-save-history nil)





;; trying to figure out order of operations here
(setq-default visual-fill-column-center-text t)

(adaptive-wrap-prefix-mode 1)

(use-package adoc-mode
  :ensure t
  :defer t
  :mode "\\.asciidoc\\'"
  :hook
  (adoc-mode . visual-line-mode)
  (adoc-mode . variable-pitch-mode))

;; yes --wgh
(use-package markdown-mode
  :ensure t
  :defer t
  :mode ("README\\.md\\'" . markdown-mode)
  :hook
  (markdown-mode . visual-line-mode)
  (markdown-mode . visual-fill-column-mode)
  (markdown-mode . adaptive-wrap-prefix-mode)
;;  (markdown-mode . centered-cursor-mode)
  :init
  (setq markdown-command "multimarkdown")
  (setq markdown-asymmetric-header t)
  (setq markdown-footnote-location 'immediately)
)


(defun wgh/markdown-variablefont ()
  "Markdown buffers ONLY in classy subtly varwidth font."
  (face-remap-add-relative 'default '(:family "iA Writer Quattro V"))
  (set-face-attribute 'markdown-code-face nil :font "iA Writer Mono S")
  (setq line-spacing 0.125)
)

(add-hook 'markdown-mode-hook 'wgh/markdown-variablefont)

(use-package json-mode
  :ensure t
  :defer t
  :config
  (add-to-list 'auto-mode-alist '("\\.json" . json-mode)))

(use-package yaml-mode
  :ensure t
  :defer t)

;; md-roam needs to be loaded before org-roam
;;  (setq org-roam-file-extensions '("org" "md" "mdown")) ; enable Org-roam to use markdown extension

;; (require 'md-roam)
;; (md-roam-mode 1)
;; (setq md-roam-file-extension "mdown")

;; (add-to-list 'org-roam-capture-templates
;;     '("m" "Markdown" plain "" :target
;;         (file+head "%<%Y-%m-%dT%H%M%S>.mdown"
;; "---\ntitle: ${title}\nid: %<%Y-%m-%dT%H%M%S>\ncategory: \n---\n")
;;         :unnarrowed t))

;; ignore right-to-left languages to prevent lag --wgh
(setq-default bidi-paragraph-direction 'left-to-right)

(if (version<= "27.1" emacs-version)
    (setq bidi-inhibit-bpa t))

;; prophylactic against slowdown. --wgh
(if (version<= "27.1" emacs-version)
    (global-so-long-mode 1))

;; Customize user interface.
(menu-bar-mode 1)
(when (display-graphic-p)
  (tool-bar-mode 0)
  (scroll-bar-mode 0))
(setq inhibit-startup-screen t)
(column-number-mode)

;; fix the window title
(setq frame-title-format
      '("Emacs: " (:eval (if (buffer-file-name)
                                              (abbreviate-file-name (buffer-file-name))
                                            "%b"))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;; fonts etc ;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Dark theme.
(set-face-attribute 'default t :height 180)

;; syntax highlighting everywhere
(global-font-lock-mode t)

;; used to be linum-mode, deprecated --wgh
;;
(global-display-line-numbers-mode 0)

;; better than mere word-wrap is visual-line-mode --wgh
;; and better than that is that plus visual-fill-column and its variants
;; (global-visual-line-mode 1) ;; nah i don't want it for all files
;; (add-hook 'org-mode-hook 'turn-on-visual-line-mode) ;; integrated into use-package declaration
;; (add-hook 'markdown-mode-hook 'turn-on-visual-line-mode) ;; integrated into use-package declaration
;; (add-hook 'visual-line-mode-hook 'visual-fill-column-mode) ;; integrated into use-package declaration





; tk TODO confirm keybindings work for python-mode stuff
; (use-package python-mode
;   :ensure t
;   :demand t
;   :init
;   (add-hook 'python-mode-hook (lambda ()
;                                 (setq autopair-handle-action-fns (list #'autopair-default-handle-action #'autopair-python-triple-quote-action))))
;   :bind (:map python-mode-map
;                 ("C-c f" . zoolander-format)))

; (use-package auto-virtualenvwrapper
;   :ensure t
;   :hook (python-mode . auto-virtualenvwrapper-activate)
;   :after (python-mode)

;; http://whattheemacsd.com/key-bindings.el-03.html
;; M-j to join lines --wgh

(global-set-key (kbd "M-j")
            (lambda ()
                  (interactive)
                  (join-line -1)))

;; centralize location of ~ files (backups)
;;http://whattheemacsd.com/init.el-02.html

;; Write backup files to own directory
(setq backup-directory-alist
      `(("." . ,(expand-file-name
                 (concat user-emacs-directory "backups")))))

;; Make backups of files, even when they're in version control
(setq vc-make-backup-files t)

;; in ido-find-file (C-x C-f), hit ~ anytime to go to home dir --wgh
;; http://whattheemacsd.com/setup-ido.el-02.html
;; works even though i'm using ivy/consult -- nice
(add-hook 'ido-setup-hook
 (lambda ()
   ;; Go straight home
   (define-key ido-file-completion-map
     (kbd "~")
     (lambda ()
       (interactive)
       (if (looking-back "/")
           (insert "~/")
         (call-interactively 'self-insert-command))))))

;; C-x C-r to RENAME BOTH FILE AND BUFFER --wgh
;; http://whattheemacsd.com/file-defuns.el-01.html
(defun rename-current-buffer-file ()
  "Renames current buffer and file it is visiting."
  (interactive)
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (error "Buffer '%s' is not visiting a file!" name)
      (let ((new-name (read-file-name "Rename current file+buffer: " filename)))
        (if (get-buffer new-name)
            (error "A buffer named '%s' already exists!" new-name)
          (rename-file filename new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil)
          (message "File '%s' successfully renamed to '%s'"
                   name (file-name-nondirectory new-name)))))))
(global-set-key (kbd "C-x C-r") 'rename-current-buffer-file)

(use-package monokai-theme
  :ensure t
  :config
  (load-theme 'monokai t))
(set-face-attribute 'default t :height 180)

;; (add-hook 'text-mode-hook
;;            (lambda ()
;;             (mixed-pitch-mode 1)))

;; manually make markdown italics look right? --wgh
;; i'm such a dipshit that i need this italics color
(defun markdown-red-boldital-hook ()
  (set-face-foreground 'markdown-italic-face "MediumVioletRed")
  (set-face-foreground 'markdown-bold-face "MediumVioletRed"))
(add-hook 'markdown-mode-hook 'markdown-red-boldital-hook)

(setq org-emphasis-alist
  '(("*" (bold :foreground "MediumVioletRed" ))
    ("/" (italic :foreground "MediumVioletRed"))
    ("_" underline)
    ("=" (:background "maroon" :foreground "white"))
    ("~" (:background "deep sky blue" :foreground "MidnightBlue"))
    ("+" (:strike-through t))))

;; wrap-region-mode brings us some way toward sublime-text handling of surround-on-* (i.e. emphasize region)
;; no idea why it's putting the cursor at the end of the region, at least in LISP, but...
(wrap-region-global-mode t)
(wrap-region-add-wrapper "*" "*")
(wrap-region-add-wrapper "**" "**" "+" 'markdown-mode)

;; markdown mode automatically for .md{own} and .txt; scheme for .scm
(add-to-list 'auto-mode-alist '("\\.txt\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.mdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.scm\\'" . scheme-mode))

;; always run the calculator in algebraic mode, not RPN
(add-hook 'calc-start-hook 'calc-total-algebraic-mode)

;; Enable Paredit.
(autoload 'enable-paredit-mode "paredit" "Turn on pseudo-structural editing of Lisp code." t)
(add-hook 'emacs-lisp-mode-hook 'enable-paredit-mode)
(add-hook 'eval-expression-minibuffer-setup-hook 'enable-paredit-mode)
(add-hook 'ielm-mode-hook 'enable-paredit-mode)
(add-hook 'lisp-interaction-mode-hook 'enable-paredit-mode)
(add-hook 'lisp-mode-hook 'enable-paredit-mode)
(add-hook 'scheme-mode-hook 'enable-paredit-mode)

;; Enable Rainbow Delimiters.
(add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode)
(add-hook 'ielm-mode-hook 'rainbow-delimiters-mode)
(add-hook 'lisp-interaction-mode-hook 'rainbow-delimiters-mode)
(add-hook 'lisp-mode-hook 'rainbow-delimiters-mode)

;; Customize Rainbow Delimiters.
(require 'rainbow-delimiters)
(set-face-foreground 'rainbow-delimiters-depth-1-face "#c66")  ; red
(set-face-foreground 'rainbow-delimiters-depth-2-face "#6c6")  ; green
(set-face-foreground 'rainbow-delimiters-depth-3-face "#69f")  ; blue
(set-face-foreground 'rainbow-delimiters-depth-4-face "#cc6")  ; yellow
(set-face-foreground 'rainbow-delimiters-depth-5-face "#6cc")  ; cyan
(set-face-foreground 'rainbow-delimiters-depth-6-face "#c6c")  ; magenta
(set-face-foreground 'rainbow-delimiters-depth-7-face "#ccc")  ; light gray
(set-face-foreground 'rainbow-delimiters-depth-8-face "#999")  ; medium gray
(set-face-foreground 'rainbow-delimiters-depth-9-face "#666")  ; dark gray

;; Custom command.
(defun show-current-time ()
  "Show current time."
  (interactive)
  (message (current-time-string)))

;; Custom key sequences.
;; (global-set-key (kbd "C-c t") 'show-current-time)
;; (global-set-key (kbd "C-c d") 'delete-trailing-whitespace)
;; i don't want the delete-trailing-whitespace binding
;; because trailing whitespace is meaningful in markdown.
;; good old Option+arrow
(global-set-key (kbd "M-<up>") 'markdown-backward-paragraph)
(global-set-key (kbd "M-<down>") 'markdown-forward-paragraph)
(global-set-key (kbd "M-[") 'markdown-backward-paragraph)
(global-set-key (kbd "M-]") 'markdown-forward-paragraph)
(global-set-key (kbd "s-<up>") 'beginning-of-buffer)
(global-set-key (kbd "s-<down>") 'end-of-buffer)

;; note that s- == 'super' == 'Cmd'
(global-set-key (kbd "s-Z") 'undo-tree-redo) ;; as opposed to built-in redo, obvsly


;; Start server.
;; (require 'server)
;; (unless (server-running-p)
;;   (server-start))

;; lorem ipsum
;; Key Binding	Function
;; C-c l p	lorem-ipsum-insert-paragraphs
;; C-c l s	lorem-ipsum-insert-sentences
;; C-c l l	lorem-ipsum-insert-list
;;;;;;; TODO: bind lorem<tab> to C-c l p
;; (require 'lorem-ipsum)
;; (lorem-ipsum-use-default-bindings)



;; groovy modeline
(require 'spaceline-config)
(spaceline-spacemacs-theme)
(spaceline-toggle-buffer-encoding-abbrev-off)
(setq powerline-default-separator 'wave)
(spaceline-toggle-minor-modes-off)
(spaceline-toggle-hud-on)
(setq spaceline-highlight-face-func 'spaceline-highlight-face-evil-state)
(set-face-attribute 'spaceline-evil-emacs nil :background "#be84ff")
(set-face-attribute 'spaceline-evil-insert nil :background "#5fd7ff")
(set-face-attribute 'spaceline-evil-motion nil :background "#ae81ff")
(set-face-attribute 'spaceline-evil-normal nil :background "#a6e22e")
(set-face-attribute 'spaceline-evil-replace nil :background "#f92672")
(set-face-attribute 'spaceline-evil-visual nil :background "#fd971f")
(powerline-reset)

(use-package evil
  :ensure t
  :config

  ;;(evil-mode 1)
  (use-package evil-leader
    :ensure t
    :config
    (global-evil-leader-mode t)
    (evil-leader/set-leader ",")
    (evil-leader/set-key
      "s s" 'swiper
      "d x w" 'delete-trailing-whitespace))

  (use-package evil-surround
    :ensure t
    :config (global-evil-surround-mode))

  ;; evil-goggles https://github.com/edkolev/evil-goggles
  (use-package evil-goggles
  :ensure t
  :config
  (evil-goggles-mode)

  ;; optionally use diff-mode's faces; as a result, deleted text
  ;; will be highlighed with `diff-removed` face which is typically
  ;; some red color (as defined by the color theme)
  ;; other faces such as `diff-added` will be used for other actions
  (evil-goggles-use-diff-faces)))





;; Yet Another Snippet mode -- oh my god, sublime-style tab snippets
;; snippets are at -- https://github.com/AndreaCrotti/yasnippet-snippets
;; (add-to-list 'load-path
;;               "~/.emacs.d/plugins/yasnippet")

(use-package yasnippet
  :ensure t
  :defer 10)

;;(require 'yasnippet)
(setq yas-snippet-dirs
      '("~/.emacs.d/snippets"                 ;; personal snippets
        ))
(yas-global-mode 1)

;; improve delete-current-line (Shift+Ctrl+Backspace)
;; this is NOT a replacement for Cmd+Backspace, look that up tk TODO
(defun delete-current-line ()
  "Delete (not kill) the current line."
  (interactive)
  (save-excursion
    (delete-region
     (progn (forward-visible-line 0) (point))
     (progn (forward-visible-line 1) (point)))))

;; cursor is an i-beam instead of a dancing square
(modify-all-frames-parameters (list (cons 'cursor-type 'bar)))

;; visual-fill-column mode
;; (setq-default visual-fill-column-center-text t)

;; let's get scheme running for some SICP action TODO
;;(setq geiser-mit-binary "/usr/local/bin/scheme")
;;(setq geiser-active-implementations '(mit))

(setq-default scheme-program-name "/usr/local/bin/scheme")

;; expand selected region, excellent
(require 'expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)

;; auto-update files that change externally to emacs
(global-auto-revert-mode)

;; project management
(projectile-mode +1)
;; Recommended keymap prefix on macOS
(define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
(setq projectile-switch-project-action 'neotree-projectile-action) ; switch neotree to whatever project i'm working in

(setq projectile-completion-system 'ivy)


;; visual previews of regex replacement
(use-package visual-regexp
  :defer t
  :ensure t)
;; use modern regexp packages instead of all these escaped characters, fuck that
(use-package visual-regexp-steroids
  :defer t
  :ensure t)
(require 'visual-regexp-steroids)
(setq vr/command-python "python3 /Users/wax/.emacs.d/elpa/visual-regexp-steroids-20170222.253/regexp.py")
(define-key global-map (kbd "C-c r") 'vr/replace)
(define-key global-map (kbd "C-c q") 'vr/query-replace)
;; if you use multiple-cursors, this is for you:
;; (define-key global-map (kbd "C-c m") 'vr/mc-mark)

;; nice icons for projectile-mode and centaur-tabs
(use-package all-the-icons
  :ensure t
  :defer t)

(use-package neotree
  :ensure t
  :defer t
  :custom
  (neo-theme (if (display-graphic-p) 'icons 'arrow))
  (neo-smart-open t)
  (setq neo-window-fixed-size nil)
 ;; :bind ([f8] . neotree-toggle)
  :after (all-the-icons))



(defun neotree-project-dir ()
    "Open NeoTree using the git root."
    (interactive)
    (let ((project-dir (projectile-project-root))
          (file-name (buffer-file-name)))
      (neotree-toggle)
      (if project-dir
          (if (neo-global--window-exists-p)
              (progn
                (neotree-dir project-dir)
                (neotree-find file-name)))
        (message "Could not find git project root."))))
 (global-set-key [f8] 'neotree-project-dir)

;; (use-package multiple-cursors
;;   :bind
;;   (("C-c m t" . mc/mark-all-like-this)
;;    ("C-c m m" . mc/mark-all-like-this-dwim)
;;    ("C-c m l" . mc/edit-lines)
;;    ("C-c m e" . mc/edit-ends-of-lines)
;;    ("C-c m a" . mc/edit-beginnings-of-lines)
;;    ("C-c m n" . mc/mark-next-like-this)
;;    ("C-c m p" . mc/mark-previous-like-this)
;;    ("C-c m s" . mc/mark-sgml-tag-pair)
;;    ("C-c m d" . mc/mark-all-like-this-in-defun)))
;; (use-package phi-search
;;   :defer t)
;; (use-package phi-search-mc
;;   :config (phi-search-mc/setup-keys)
;;   :defer t)
;; (use-package mc-extras
;;   :defer t
;;   :config (define-key mc/keymap (kbd "C-. =") 'mc/compare-chars))

(use-package google-this
  :defer t
  :config
  (google-this-mode 1))

(use-package alert
  :defer t
  :commands (alert)
  :init
  (setq alert-default-style 'notifier))


;; avy-complete: C-j (this is the wonderful vim-style 'jump to any found instance onscreen'

(use-package counsel
  :ensure t
  :config
  (use-package flx
    :ensure t)
  (ivy-mode 1)
  (setq ivy-height 20)
  (setq ivy-wrap t)
  )

(use-package ivy-bibtex
  :ensure t
  :defer t)

(setq bibtex-completion-bibliography
      '("~/Dropbox/highweirdness/highweirdness.bib"))

;; tell ivy/swiper which regex builders to use:
;; swiper (isearch): SPC == .*
;; all other: fuzzy matching (.* between each pair of chars)
(setq ivy-re-builders-alist '((swiper-isearch . ivy--regex-plus)
                              (counsel-grep . ivy--regex-plus)
                              (counsel-ag . ivy--regex-plus)
                              (ivy-bibtex . ivy--regex-ignore-order)
                              (t      . ivy--regex-fuzzy))
      )

(counsel-mode 1)
(counsel-projectile-mode 1)

;; (ivy-mode)
(setq ivy-use-virtual-buffers t)
(setq enable-recursive-minibuffers t)
;; ;; ;; ;; ;; enable this if you want `swiper' to use it
;; (setq search-default-mode #'char-fold-to-regexp)
(global-set-key (kbd "C-s") 'swiper-isearch) ;; C-s to cycle through matches, and C-s C-s to repeat search on next invocation
(global-set-key (kbd "s-f") 'swiper-isearch) ;; Cmd-F just like mama used to make
(global-set-key (kbd "C-c C-r") 'ivy-resume)
(global-set-key (kbd "<f6>") 'ivy-resume)
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)
(global-set-key (kbd "M-y") 'counsel-yank-pop)
(global-set-key (kbd "<f1> f") 'counsel-describe-function)
(global-set-key (kbd "<f1> v") 'counsel-describe-variable)
(global-set-key (kbd "<f1> o") 'counsel-describe-symbol)
(global-set-key (kbd "<f1> l") 'counsel-find-library)
(global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
(global-set-key (kbd "<f2> u") 'counsel-unicode-char)
;; (global-set-key (kbd "C-M-j") 'ivy-switch-buffer) ;; consider this tk
;; (global-set-key (kbd "C-c g") 'counsel-git)
;; (global-set-key (kbd "C-c j") 'counsel-git-grep)
;; (global-set-key (kbd "C-c k") 'counsel-ag)
;; (global-set-key (kbd "C-x l") 'counsel-locate)
;; (global-set-key (kbd "C-S-o") 'counsel-rhythmbox)
(define-key minibuffer-local-map (kbd "C-r") 'counsel-minibuffer-history)

;; ;; avy
(avy-setup-default)
(global-set-key (kbd "C-c C-j") 'avy-resume)
;; this was C-' but i like the 'jump' mnemonic and i hate the newline thing
;; (bind-keys* ("C-j". avy-goto-char-timer)) ;; dependent on bind-keys* macro which comes from use-package
;; following line doesn't work in every context
(global-set-key (kbd "C-'") 'avy-goto-char-timer) ;; this is the shit -- search what's visible

;; BUT!! it's C-' to use avy to jump to one of the onscreen swiper results!! TODOs

;; magit
(use-package magit
  :defer t
  :bind ("C-x g" . magit-status)
  :ensure t)

;; (use-package magit-delta
;;   :hook (magit-mode . magit-delta-mode))

;; hippie expand
(global-set-key [remap dabbrev-expand] 'hippie-expand)



;; cycle through buffers -- was C-<tab> but it's awkward, the macos command is better
;; iflipb-wrap-around does what you think; it's bizarre that this isn't the default
(require 'iflipb)
(setq iflipb-wrap-around t)
;; rejecting this simpler alternative in favour of iflipb seems only fair. fuck the doomed
;;(global-set-key [C-tab] 'next-buffer)
;;(global-set-key [C-S-tab] 'previous-buffer)
(global-set-key (kbd "s-`") 'iflipb-next-buffer) ;; macos shortcuts
(global-set-key (kbd "s-~") 'iflipb-previous-buffer)
;; rebind C-x k to maintain the iflipb buffer list
(global-set-key (kbd "C-x k") 'iflipb-kill-buffer)


(add-hook 'after-init-hook 'global-company-mode)
;; we don't need this kind of completion in markdown but it's perfect for org
(setq company-global-modes '(not markdown-mode)) ;; md-roam might be an issue?



;; improve discoverability for key commands
(use-package which-key
  :config
  (which-key-mode 1))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;; org-mode and org-roam ;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package org-bullets
  :defer t
  :after org
  :ensure t
  :hook
  (org-mode . (lambda () (org-bullets-mode 1))))

(use-package org
  ;;    :pin manual
  :defer t
  :load-path ("lisp/org-mode/lisp" "lisp/org-mode/lisp/contrib/lisp")
  :bind
  (:map org-mode-map
        ("C-'" . nil) ;; reclaiming the avy keybinding from org's agenda cycling command
        ("C-c l" . org-store-link)
        ("A-h" . org-mark-element)
        ("C-a" . org-beginning-of-line)
        ("C-e" . org-end-of-line)
        ("C-k" . org-kill-line))
  :custom
  (setq org-directory "~/org")
  (org-log-done t)
  (org-startup-indented t)
  (org-log-into-drawer t)
  (org-special-ctrl-a/e t)
  (org-special-ctrl-k t)
  (org-use-speed-commands
   (lambda ()
     (and (looking-at org-outline-regexp)
          (looking-back "^\**"))))
  (org-confirm-babel-evaluate nil)
  (org-src-fontify-natively t)
  (org-src-tab-acts-natively t)
  (org-hide-emphasis-markers t)
  (prettify-symbols-unprettify-at-point 'right-edge)
  (org-fontify-done-headline t)
  (org-tags-column 0)
  (org-todo-keyword-faces
   '(("AREA"         . "DarkOrchid1")
     ("[AREA]"       . "DarkOrchid1")
     ("PROJECT"      . "DarkOrchid1")
     ("[PROJECT]"    . "DarkOrchid1")
     ("INBOX"        . "cyan")
     ("[INBOX]"      . "cyan")
     ("PROPOSAL"     . "orange")
     ("[PROPOSAL]"   . "orange")
     ("DRAFT"        . "yellow3")
     ("[DRAFT]"      . "yellow3")
     ("INPROGRESS"   . "yellow4")
     ("[INPROGRESS]" . "yellow4")
     ("MEETING"      . "purple")
     ("[MEETING]"    . "purple")
     ("CANCELED"     . "blue")
     ("[CANCELED]"   . "blue")))
  :custom-face
  (variable-pitch ((t (:family "Avenir Next" :height 180 :weight light))))
;;  (variable-pitch ((t (:family "ETBembo" :height 180 :weight thin))))
  ;;(variable-pitch ((t (:family "Gill Sans" :height 180 :weight light))))
  ;;    (fixed-pitch ((t (:family "Inconsolata Nerd Font"))))
  (fixed-pitch ((t (:family "SF Mono" :height 180))))
  (org-indent ((t (:inherit (org-hide fixed-pitch)))))
  (org-done ((t (:foreground "PaleGreen"
                             :strike-through t))))
  :hook
  (org-mode . (lambda () (add-hook 'after-save-hook 'org-babel-tangle :append :local)))
  (org-babel-after-execute . org-redisplay-inline-images)
  (org-mode . (lambda ()
                "Beautify Org Checkbox Symbol"
                (push '("[ ]" . "☐" ) prettify-symbols-alist)
                (push '("[X]" . "☑" ) prettify-symbols-alist)
                (push '("[-]" . "⊡" ) prettify-symbols-alist)
                (prettify-symbols-mode)))
  (org-mode . visual-line-mode)
  (org-mode . visual-fill-column-mode)
 ;; (org-mode . fixed-pitch-mode)
  :config
  (font-lock-add-keywords
   'org-mode
   '(("^ *\\([-]\\) "
      (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))
  (defface org-checkbox-done-text
    '((t (:foreground "#71696A" :strike-through t)))
    "Face for the text part of a checked org-mode checkbox.")
  
  (font-lock-add-keywords
   'org-mode
   `(("^[ \t]*\\(?:[-+*]\\|[0-9]+[).]\\)[ \t]+\\(\\(?:\\[@\\(?:start:\\)?[0-9]+\\][ \t]*\\)?\\[\\(?:X\\|\\([0-9]+\\)/\\2\\)\\][^\n]*\n\\)"
      1 'org-checkbox-done-text prepend))
   'append)
  (let* ((variable-tuple
          (cond ((x-lssist-fonts   "SF Mono")         '(:font   "SF Mono"))
                ((x-list-fonts   "Source Sans Pro") '(:font   "Source Sans Pro"))
                ((x-list-fonts   "Lucida Grande")   '(:font   "Lucida Grande"))
                ((x-list-fonts   "Verdana")         '(:font   "Verdana"))
                ((x-family-fonts "Sans Serif")      '(:family "Sans Serif"))
                (nil (warn "Cannot find a Sans Serif Font."))))
         (base-font-color (face-foreground 'default nil 'default))
         (headline `(:inherit default :weight bold
                              :foreground ,base-font-color)))
    
    (custom-theme-set-faces
     'user
     `(org-level-8        ((t (,@headline ,@variable-tuple))))
     `(org-level-7        ((t (,@headline ,@variable-tuple))))
     `(org-level-6        ((t (,@headline ,@variable-tuple))))
     `(org-level-5        ((t (,@headline ,@variable-tuple))))
     `(org-level-4        ((t (,@headline ,@variable-tuple :height 1.1))))
     `(org-level-3        ((t (,@headline ,@variable-tuple :height 1.25))))
     `(org-level-2        ((t (,@headline ,@variable-tuple :height 1.5))))
     `(org-level-1        ((t (,@headline ,@variable-tuple :height 1.75))))
     `(org-headline-done  ((t (,@headline ,@variable-tuple :strike-through t))))
     `(org-document-title ((t (,@headline ,@variable-tuple
                                          :height 2.0 :underline nil))))))
  (eval-after-load 'face-remap '(diminish 'buffer-face-mode))
  (eval-after-load 'simple '(diminish 'visual-line-mode)))


;; suggested standard org bindings
(global-set-key (kbd "C-c l") #'org-store-link)
(global-set-key (kbd "C-c a") #'org-agenda)
(global-set-key (kbd "C-c c") #'org-capture)

(use-package exec-path-from-shell
  :ensure t)

(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

;; config from https://github.com/zzamboni/dot-emacs/blob/master/init.el
;; additional functions from https://systemcrafters.net/build-a-second-brain-in-emacs/5-org-roam-hacks/
;; (use-package org-roam
;;   :ensure t
;;   :demand t  ;; Ensure org-roam is loaded by default
;;   :init
;;   (setq org-roam-v2-ack t)
;;   :custom
;;   (org-roam-directory "~/orgroam-wax/")
;;   (org-roam-completion-everywhere t)
  
;;   :bind (("C-c n l" . org-roam-buffer-toggle)
;;          ("C-c n f" . org-roam-node-find)
;;          ("C-c n g" . org-roam-graph)
;;          ("C-c n n" . org-id-get-create) ;; this doesn't actually work
;;          ("C-c n i" . org-roam-node-insert)
;;          ("C-c n I" . org-roam-node-insert-immediate)
;;          ("C-c n p" . my/org-roam-find-project)
;;          ("C-c n c" . org-roam-capture)
;; ;;         ("C-c n t" . my/org-roam-capture-task) ;; i don't use this.
;;          ("C-c n b" . my/org-roam-capture-inbox)
;;          :map org-mode-map
;;          ("C-M-i" . completion-at-point)
;;          ("C-c n m" . org-roam-refile)
;;          ("C-c n t" . org-roam-tag-add)
;;          :map org-roam-dailies-map
;;          ("Y" . org-roam-dailies-capture-yesterday)
;;          ("T" . org-roam-dailies-capture-tomorrow))
;;   :bind-keymap
;;   ("C-c n d" . org-roam-dailies-map)
;;   :hook
;;   (org-roam-mode . visual-line-mode)
;;   (org-roam-mode . visual-fill-column-mode)
;;   :config
;;   (require 'org-roam-dailies) ;; Ensure the keymap is available

;;   (org-roam-db-autosync-mode))

;; (setq org-roam-mode-sections
;;       (list #'org-roam-backlinks-section
;;             #'org-roam-reflinks-section
;;             ;;#'org-roam-unlinked-references-section
;;             ))

;; (use-package vulpea
;;   :ensure t
;;   ;; hook into org-roam-db-autosync-mode you wish to enable
;;   ;; persistence of meta values (see ective section in README to
;;   ;; find out what meta means)
;;   :hook ((org-roam-db-autosync-mode . vulpea-db-autosync-enable)))

;; (add-to-list 'display-buffer-alist
;;              '("\\*org-roam\\*"
;;                (display-buffer-in-direction)
;;                (direction . right)
;;                (window-width . 0.33)
;;                (window-height . fit-window-to-buffer)))

;; (defun org-roam-node-insert-immediate (arg &rest args)
;;   (interactive "P")
;;   (let ((args (push arg args))
;;         (org-roam-capture-templates (list (append (car org-roam-capture-templates)
;;                                                   '(:immediate-finish t)))))
;;     (apply #'org-roam-node-insert args)))

;; (defun my/org-roam-filter-by-tag (tag-name)
  ;; (lambda (node)
  ;;   (member tag-name (org-roam-node-tags node))))

;; commenting out the next two defuns in order to get the normal org-agenda-files location here
;; but eventually i won't want to do that
(setq org-agenda-files '("~/org" "~/orgroam-wax"))
(setq org-default-notes-file (concat org-directory "/notes.org"))
(setq org-refile-targets '((org-agenda-files :maxlevel . 3)))
(setq org-refile-allow-creating-parent-nodes 'confirm)



;; (defun my/org-roam-list-notes-by-tag (tag-name)
;;   (mapcar #'org-roam-node-file
;;           (seq-filter
;;            (my/org-roam-filter-by-tag tag-name)
;;            (org-roam-node-list))))

;; (defun my/org-roam-refresh-agenda-list ()
;;   (interactive)
;;   (setq org-agenda-files (my/org-roam-list-notes-by-tag "Project")))

;; Build the agenda list the first time for the session
;; (my/org-roam-refresh-agenda-list)

;; (defun my/org-roam-project-finalize-hook ()
;;   "Adds the captured project file to `org-agenda-files' if the capture was not aborted."
;;   ;; Remove the hook since it was added temporarily
;;   (remove-hook 'org-capture-after-finalize-hook #'my/org-roam-project-finalize-hook)

;;   ;; Add project file to the agenda list if the capture was confirmed
;;   (unless org-note-abort
;;     (with-current-buffer (org-capture-get :buffer)
;;       (add-to-list 'org-agenda-files (buffer-file-name)))))

;; (defun my/org-roam-find-project ()
;;   (interactive)
;;   ;; Add the project file to the agenda after capture is finished
;;   (add-hook 'org-capture-after-finalize-hook #'my/org-roam-project-finalize-hook)

;;   ;; Select a project file to open, creating it if necessary
;;   (org-roam-node-find
;;    nil
;;    nil
;;    (my/org-roam-filter-by-tag "Project")
;;    :templates
;;    '(("p" "project" plain "* Goals\n\n%?\n\n* Tasks\n\n** TODO Add initial tasks\n\n* Dates\n\n"
;;       :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+category: ${title}\n#+filetags: Project")
;;       :unnarrowed t))))

;; (defun my/org-roam-capture-inbox ()
;;   (interactive)
;;   (org-roam-capture- :node (org-roam-node-create)
;;                      :templates '(("i" "inbox" plain "* %?"
;;                                   :if-new (file+head "Inbox.org" "#+title: Inbox\n")))))

;; (defun my/org-roam-capture-task ()
;;   (interactive)
;;   ;; Add the project file to the agenda after capture is finished
;; ;;  (add-hook 'org-capture-after-finalize-hook #'my/org-roam-project-finalize-hook) ;; cut b/c roam files are already in org-roam-files

;;   ;; Capture the new task, creating the project file if necessary
;;   (org-roam-capture- :node (org-roam-node-read
;;                             nil
;;                             (my/org-roam-filter-by-tag "Project"))
;;                      :templates '(("p" "project" plain "** TODO %?"
;;                                    :if-new (file+head+olp "%<%Y%m%d%H%M%S>-${slug}.org"
;;                                                           "#+title: ${title}\n#+category: ${title}\n#+filetags: Project"
;;                                                           ("Tasks"))))))

;; (defun my/org-roam-copy-todo-to-today ()
;;   (interactive)
;;   (let ((org-refile-keep t) ;; Set this to nil to delete the original!
;;         (org-roam-dailies-capture-templates
;;           '(("t" "tasks" entry "%?"
;;              :if-new (file+head+olp "%<%Y-%m-%d>.org" "#+title: %<%Y-%m-%d>\n" ("Tasks")))))
;;         (org-after-refile-insert-hook #'save-buffer)
;;         today-file
;;         pos)
;;     (save-window-excursion
;;       (org-roam-dailies--capture (current-time) t)
;;       (setq today-file (buffer-file-name))
;;       (setq pos (point)))

;;     ;; Only refile if the target file is different than the current file
;;     (unless (equal (file-truename today-file)
;;                    (file-truename (buffer-file-name)))
;;       (org-refile nil nil (list "Tasks" today-file nil pos)))))

;; (add-to-list 'org-after-todo-state-change-hook
;;              (lambda ()
;;                (when (equal org-state "DONE")
;;                  (my/org-roam-copy-todo-to-today))))


;; ;; fulltext search in org-roam dir using ag -- but this gives multiple hits per file, which i sorta dislike
;; ;; TODO: figure out a search tool (rg?) that returns 'any file that includes this string'
;; (defun jmb/counsel-ag-roam ()
;;  "Do counsel-ag on the org roam directory"
;;  (interactive)
;;  (counsel-ag nil org-roam-directory))
;; (global-set-key (kbd "C-c n r") 'jmb/counsel-ag-roam)

;; transient highlighting of certain buffer operations, e.g. paste/undo
(use-package volatile-highlights
  :defer t
  :ensure t
  :diminish
  :config
  (volatile-highlights-mode t)
  )

;; writeroom-mode is interesting, just not on a vertical monitor.
;; darkroom-mode doesn't alter the frame, which is probably what i want.
;; here's the sublimetext 'distraction free' keybinding: Cmd+Ctrl+Shift+F
;; (global-set-key (kbd "s-C-F") 'writeroom-mode)

;; TODO TK preserve neotree state/window layout when entering focus mode -- flipping a bit will do.

(use-package darkroom
  :commands darkroom-mode
  :config
  (setq darkroom-text-scale-increase 0)
  (darkroom-mode 0))

(defun dw/enter-focus-mode ()
  (interactive)
  (darkroom-mode 1)
;;  (display-line-numbers-mode 0)
  )

(defun dw/leave-focus-mode ()
  (interactive)
  (darkroom-mode 0)
;;  (display-line-numbers-mode 1)
  )

(defun dw/toggle-focus-mode ()
  (interactive)
  (if (symbol-value darkroom-mode)
    (dw/leave-focus-mode)
    (dw/enter-focus-mode)))

(global-set-key (kbd "s-C-F") 'dw/toggle-focus-mode)

;; (dw/leader-key-def
;;   "tf" '(dw/toggle-focus-mode :which-key "focus mode"))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; deft                                                                   ;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (require 'deft)
;; (setq deft-directory org-roam-directory) ;; gotta set org-roam-directory first, obvsly
;; (setq deft-extensions '("mdown" "org" "txt" "md" "org.gpg"))
;; (setq deft-default-extension "mdown")
;; (setq deft-text-mode 'markdown-mode)
;; (setq deft-use-filename-as-title t)
;; (setq deft-use-filter-string-for-filename t)
;; (setq deft-auto-save-interval 0)
;; (setq deft-current-sort-method 'title)

;;shortcut to launch deft
;; (global-set-key (kbd "C-c d") 'deft)




;; cmd-w, the macos shortcut. accept no substitutes.
(global-set-key (kbd "s-w") #'kill-current-buffer)





;; xah lee efficiency advice
(defalias 'yes-or-no-p 'y-or-n-p) ; y or n is enough
(defalias 'list-buffers 'ibuffer) ; always use ibuffer



;; (when (>= emacs-major-version 26)
;;   (pixel-scroll-mode))


;; 'org as a word processor' --> TODO/TK bring this over to my home machine!!
;; (setq org-hide-emphasis-markers t)
;; (font-lock-add-keywords 'org-mode
;;                             '(("^ +\\([-*]\\) "
;;                                (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))





(use-package ox-asciidoc
  :defer t
  :ensure t
  :after org)

;; search in org-mode; this supersedes org-rifle
(use-package org-ql
  :defer t
  :quelpa (org-ql :fetcher github :repo "alphapapa/org-ql"
            :files (:defaults (:exclude "helm-org-ql.el"))))


;; remove some minor modes from modeline -- i figure this needs to be at the end

;; no idea whether i need to diminish these before or after loading modes
(require 'diminish)
(diminish 'company-mode)
(diminish 'eldoc-mode)
(diminish 'undo-tree-mode)
(diminish 'paredit-mode)
(diminish 'ivy-mode)
(diminish 'projectile-mode)
(diminish 'filladapt-mode)
(diminish 'which-key-mode)
(diminish 'counsel-mode)
(diminish 'yas-minor-mode)
(diminish 'md-roam-mode)
(diminish 'org-indent-mode)
(diminish 'wrap-region-mode)
(diminish 'volatile-highlights-mode)
(diminish 'google-this-mode)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; sacha chua's live word count  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar count-words-buffer
  nil
  "*Number of words in the buffer.")

(defun wicked/update-wc ()
  (interactive)
  (setq count-words-buffer (number-to-string (count-words-buffer)))
  (force-mode-line-update))

; only setup timer once
(unless count-words-buffer
  ;; seed count-words-paragraph
  ;; create timer to keep count-words-paragraph updated
  (run-with-idle-timer 1 t 'wicked/update-wc))

;; add count words paragraph the mode line
(unless (memq 'count-words-buffer global-mode-string)
  (add-to-list 'global-mode-string "wc: " t)
  (add-to-list 'global-mode-string 'count-words-buffer t))

;; count number of words in current paragraph
(defun count-words-buffer ()
  "Count the number of words in the current paragraph."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (let ((count 0))
      (while (not (eobp))
	(forward-word 1)
        (setq count (1+ count)))
      count)))

;; set the fucking default font size!!
(set-face-attribute 'default t :height 180)

;; use marked.app to preview markdown files
;; Getting emacs to use the 'Marked' app
(defun markdown-preview-file ()
  "run Marked on the current file and revert the buffer"
  (interactive)
  (shell-command
   (format "open -a /Applications/Marked\\ 2.app %s"
       (shell-quote-argument (buffer-file-name))))
)

(when (eq system-type 'darwin)
  (setq insert-directory-program "/usr/local/bin/gls")) ;; to dynamically alter ls switches w/dirvish

;; better than dired
(use-package dirvish
  :ensure t
  :init
  (dirvish-override-dired-mode) ;; Let Dirvish take over Dired globally
  (global-set-key (kbd "s-d") 'dirvish-dwim)
  ;; note that C-x C-j is bound to dired-jump -- which now kicks to dirvish!!
  (global-set-key (kbd "C-S-s-d") 'dirvish-quick-access)
  :custom
  (dirvish-quick-access-entries;
   '(("h" "~/"                          "Home")
;;     ("d" "~/Downloads/"                "Downloads")
     ("d" "~/Dropbox/"                  "Dropbox")
     ("z" "~/Dropbox/zettel/"           "zettel")
     ("w" "~/Dropbox/highweirdness/"    "highweirdness")
     ("t" "~/.Trash/"                   "Trash")
     ("c" "~/code/"                     "code")
     ("e" "~/.emacs.d/"                 ".emacs.d")))
  :config
;;  (require 'dirvish-minibuffer-preview)
  ;;  (dirvish-minibuf-preview-mode)
  (dirvish-peek-mode)
  (setq dired-dwim-target t)
  (setq delete-by-moving-to-trash t)
  (setq dired-mouse-drag-files t)                   ; added in Emacs 29
  (setq mouse-drag-and-drop-region-cross-program t) ; added in Emacs 29
  (setq dired-recursive-copies 'always)
  (setq dirvish-attributes '(all-the-icons dirvish-minibuffer-preview collapse subtree-state dirvish-ls))
  ;; default ls options for dired listings
  (setq dired-listing-switches "--almost-all -l -B --group-directories-first -1 -D")
  :bind
  ;; Bind `dirvish|dirvish-side|dirvish-dwim' as you see fit
  (
   ([f5] . dirvish-side)
   ("C-c f" . dirvish-fd)
   :map dired-mode-map ; Dirvish respects all the keybindings in this map
   ;; ("h" . dired-up-directory)
   ;; ("j" . dired-next-line)
   ;; ("k" . dired-previous-line)
   ;; ("l" . dired-find-file)
   ;; ("i" . wdired-change-to-wdired-mode)
   ;; ("." . dired-omit-mode)
   ("^"   . dired-up-directory) ;; this should be default behaviour, dunno why needed to include here TK
   ("b"   . dirvish-quick-access) ;; 'b' for 'bookmark'
   ("f"   . dirvish-file-info-menu)
   ("y"   . dirvish-yank-menu)
   ("N"   . dirvish-narrow)
   ("s-["   . dirvish-history-last)
   ("s"   . dirvish-quicksort)  ; remapped `dired-sort-toggle-or-edit'
   ("?"   . dirvish-dispatch)   ; remapped `dired-summary'
   ("TAB" . dirvish-subtree-toggle)
   ("@" . dirvish-history-jump) ;; think 'address' or 'target'
   ("SPC" . scroll-up-command) ;; was bound to dirvish-history-jump but to hell w/that
   ("S-SPC" . scroll-down-command)
   ("M-n" . dirvish-history-go-forward)
   ("M-p" . dirvish-history-go-backward)
   ("M-l" . dirvish-ls-switches-menu)
   ("M-m" . dirvish-mark-menu)
   ("M-f" . dirvish-toggle-fullscreen)
   ("M-s" . dirvish-setup-menu)
   ("M-e" . dirvish-emerge-menu)
   ("M-j" . dirvish-fd-jump)))

(eval-after-load 'markdown-mode
  '(define-key markdown-mode-map (kbd "s-b") 'markdown-preview-file)) ;; duplicate sublimetext Cmd-B for 'build'




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; denote ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




(require 'denote)

(with-eval-after-load 'org-capture
;;  (require 'denote-org-capture)
  (add-to-list 'org-capture-templates
               '("n" "New note (with Denote)" plain
                 (file denote-last-path)
                 #'denote-org-capture
                 :no-save t
                 :immediate-finish nil
                 :kill-buffer t
                 :jump-to-captured t)))

;; Remember to check the doc strings of those variables.
(setq denote-directory (expand-file-name "~/Dropbox/zettel/"))
(setq denote-known-keywords '("emacs" "media" "zettelkasten" "games"))
(setq denote-infer-keywords t)
(setq denote-sort-keywords t)
(setq denote-file-type 'markdown-yaml) ; Org is the default, set others here
(setq denote-prompts '(title keywords))

;; We allow multi-word keywords by default.  The author's personal
;; preference is for single-word keywords for a more rigid workflow.
(setq denote-allow-multi-word-keywords t)

(setq denote-date-format nil) ; read doc string

;; You will not need to `require' all those individually once the
;; package is available.
;; (require 'denote-retrieve)
;; (require 'denote-link)

;; By default, we fontify backlinks in their bespoke buffer.
(setq denote-link-fontify-backlinks t)

;; Also see `denote-link-backlinks-display-buffer-action' which is a bit
;; advanced.

;; If you use Markdown or plain text files (Org renders links as buttons
;; right away)
(add-hook 'find-file-hook #'denote-link-buttonize-buffer)

;;(require 'denote-dired)
(setq denote-dired-rename-expert nil)

;; We use different ways to specify a path for demo purposes.
(setq denote-dired-directories
      (list denote-directory
            (thread-last denote-directory (expand-file-name "attachments"))
            (expand-file-name "~/Documents/books")
            (expand-file-name "~/Dropbox/zettel/starwarsd6")))

;; Generic (great if you rename files Denote-style in lots of places):
;; (add-hook 'dired-mode-hook #'denote-dired-mode)
;;
;; OR if only want it in `denote-dired-directories':
(add-hook 'dired-mode-hook #'denote-dired-mode-in-directories)

;; Here is a custom, user-level command from one of the examples we
;; showed in this manual.  We define it here and add it to a key binding
;; below.
(defun my-denote-journal ()
  "Create an entry tagged 'journal', while prompting for a title."
  (interactive)
  (denote
   (denote--title-prompt)
   '("journal")))

;; Denote DOES NOT define any key bindings.  This is for the user to
;; decide.  For example:
(let ((map global-map))
  (define-key map (kbd "C-c n j") #'my-denote-journal) ; our custom command
  (define-key map (kbd "C-c n n") #'denote)
  (define-key map (kbd "C-c n N") #'denote-type)
  (define-key map (kbd "C-c n d") #'denote-date)
  (define-key map (kbd "C-c n s") #'denote-subdirectory)
  ;; If you intend to use Denote with a variety of file types, it is
  ;; easier to bind the link-related commands to the `global-map', as
  ;; shown here.  Otherwise follow the same pattern for `org-mode-map',
  ;; `markdown-mode-map', and/or `text-mode-map'.
  (define-key map (kbd "C-c n i") #'denote-link) ; "insert" mnemonic
  (define-key map (kbd "C-c n I") #'denote-link-add-links)
  (define-key map (kbd "C-c n l") #'denote-link-find-file) ; "list" links
  (define-key map (kbd "C-c n b") #'denote-link-backlinks)
  ;; Note that `denote-rename-file' can work from any context, not
  ;; just Dired buffers.  That is why we bind it here to the
  ;; `global-map'.
  (define-key map (kbd "C-c n r") #'denote-rename-file)
  (define-key map (kbd "C-c n R") #'denote-rename-file-using-front-matter)
)

;; Key bindings specifically for Dired.
(let ((map dired-mode-map))
  (define-key map (kbd "C-c C-d C-i") #'denote-link-dired-marked-notes)
  (define-key map (kbd "C-c C-d C-r") #'denote-dired-rename-marked-files)
  
  )


(setq consult-notes-sources
      `(("Notes"  ?n ,denote-directory)
        ;; ("Books"  ?b "~/Documents/books")
        ))


;; accent-mode -- C-x C-a in markdown/text to bring up accented-character options

(use-package accent
  :ensure t
  )

(define-key markdown-mode-map "\C-x\C-a" 'accent-menu)

(setq accent-custom '((\? (¿))
                      (! (¡))                      
                      ))



;;;; enable sane right-click behaviour -- i never, ever want the default
(context-menu-mode)

;; highlight results when using M-x ag
(setq ag-highlight-search t)

;; necessary after raising this threshold up top
(setq gc-cons-threshold (* 2 1000 1000))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; shelved for now ;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; filladapt should give us autonumbering and multipar quotes in mdown;;
;; (require 'filladapt)
;; (add-hook 'markdown-mode-hook #'filladapt-mode)

;; (use-package centaur-tabs
;;   :demand
;;   :config
;;   (centaur-tabs-mode t)
;;   :bind
;;   ("C-<prior>" . centaur-tabs-backward)
;;   ("C-<next>" . centaur-tabs-forward))
;; (setq centaur-tabs-set-icons t)
;; (setq centaur-tabs-gray-out-icons 'buffer)
;; (setq centaur-tabs-set-bar 'under)
;; (setq x-underline-at-descent-line t)
;; (centaur-tabs-change-fonts "Helvetica" 160)

;; let's load a custom library
;; (load-library "markup-faces")

