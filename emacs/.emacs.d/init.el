;;;* Startup Speed Hacks
(setq frame-inhibit-implied-resize t)
(defvar last-file-name-handler-alist file-name-handler-alist)
(setq gc-cons-threshold 100000000)
(setq compilation-finish-functions (lambda (buf str)
                                     (if (null (string-match ".*exited abnormally.*" str))
                                         ;;no errors, make the compilation window go away in a few seconds
                                         (progn (run-at-time "1 sec" nil 'delete-windows-on
                                                             (get-buffer-create "*compilation*"))
                                                (message "No Compilation Errors!")))))
(setq compilation-scroll-output t)
(setq compilation-window-height 20)
(setq inhibit-compacting-font-caches t)

;;;* misc emacs stuff
(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3") ;; bugfix; remove in emacs 26.3+
(setq backup-directory-alist '(("." . "~/.emacs.d/backup")) backup-by-copying t version-control t
      delete-old-versions t kept-new-versions 5 kept-old-versions 5)
(add-to-list 'exec-path "/home/orausch/.local/bin")

(add-hook 'prog-mode-hook 'display-line-numbers-mode)

;;;* use-package
(package-initialize)
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t)
(require 'use-package-ensure)
(setq use-package-always-ensure t)
;;;* benchmark-init
;;(use-package benchark-init
;;  :config
;;  ;; To disable collection of benchmark data after init is done.
;;  (add-hook 'after-init-hook 'benchmark-init/deactivate))
;;;* evil
(use-package
  evil
  :config (evil-mode))

;; I hate emacs-state, remove it
(with-eval-after-load 'evil
  (define-key evil-motion-state-map (kbd "C-z") nil)
  (define-key evil-motion-state-map (kbd "SPC") nil))

;; suspend-frame is also stupid
(define-key global-map (kbd "C-z") nil)
(define-key global-map (kbd "C-x C-z") nil)

(use-package evil-surround
  :ensure t
  :config
  (global-evil-surround-mode 1))

(require 'org-tempo)
(use-package
  evil-org
  :after org
  :config (add-hook 'org-mode-hook 'evil-org-mode)
  (add-hook 'evil-org-mode-hook (lambda ()
                                  (evil-org-set-key-theme)))
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))

(with-eval-after-load 'evil-maps
  (define-key evil-motion-state-map (kbd "C-d") nil)
  (define-key evil-normal-state-map (kbd "C-n") nil)
  (define-key evil-normal-state-map (kbd "C-p") nil)
  (define-key evil-insert-state-map (kbd "C-n") nil)
  (define-key evil-insert-state-map (kbd "C-p") nil))

;;;* theme
(load-file "~/.dotfiles/emacs/.emacs.d/minimal-light-theme.el")

;;;* simple packages (projectile whichkey lua-mode markdown-mode)
(use-package
  projectile
  :config
  (projectile-mode +1))

(use-package
  which-key
  :config
  (which-key-mode)
  (which-key-setup-side-window-right))

(use-package
  lua-mode)

(use-package
  markdown-mode)

(use-package rg)
(use-package protobuf-mode)

;; (use-package
;;  gruvbox-theme)
;;;* dashboard
(use-package dashboard
  :ensure t
  :config
  (setq dashboard-items '((recents  . 5)
                          (projects . 5)))
  (setq dashboard-org-agenda-categories '("Tasks" "Appointments"))
  (setq dashboard-startup-banner 'logo)
  (dashboard-setup-startup-hook))

;;;* artist-mode
(defun artist-mode-toggle-emacs-state ()
  (if artist-mode
      (evil-emacs-state)
    (evil-exit-emacs-state)))

                                        ;(add-hook 'artist-mode-hook #'artist-mode-toggle-emacs-
;;;* ivy
(use-package
  ivy
  :config (setq projectile-completion-system 'ivy)
  (ivy-mode 1))

;;;* org

(require 'org)
(add-hook 'org-capture-mode-hook #'org-align-all-tags)
(add-hook 'org-mode-hook 'turn-on-visual-line-mode)
(setq org-startup-folded nil)

(add-hook 'org-mode-hook (lambda ()
                           (setq org-format-latex-options (plist-put org-format-latex-options :scale 1.5))))
(add-hook 'org-mode-hook 'flyspell-mode)
(setq org-return-follows-link t)
;;(add-hook 'org-insert-heading-hook 'evil-insert-state)
(setq org-todo-keywords '((sequence "TODO(t)" "WAITING(w)" "|" "DONE(d)" "CANCELLED(c)")))

(add-to-list 'org-agenda-files "~/org/journal/")

;; fix
;;(defun my-refile ()
;;  (interactive)
;;  (let ((org-refile-targets
;;         (mapcar (lambda (target) (append (list target) '(:maxlevel . 1)))
;;                 (directory-files "~/org/" 'full ".*org"))))
;;    (call-interactively 'org-refile)))

;;(define-key org-mode-map (kbd "C-c C-w") 'my-refile)
;;(define-key org-capture-mode-map (kbd "C-c C-w") 'my-refile)

(defun my-get-image-name ()
  (let ((i 0))
    (while
        (file-exists-p
         (concat
          "/home/orausch/org/img/"
          (file-name-nondirectory (file-name-sans-extension (buffer-file-name)))
          (int-to-string i)
          ".png"))
      (setq i (+ i 1)))
    (concat
     "/home/orausch/org/img/"
     (file-name-nondirectory (file-name-sans-extension (buffer-file-name)))
     (int-to-string i)
     ".png")
    ))


(defun insert-screenshot-at-point ()
  "Make a screenshot, save it in the img folder and insert a link to it."
  (interactive)
  (shell-command-to-string "scrot -s -o /tmp/screen.png")
  (let ((screenshot-name (my-get-image-name)))
    (shell-command-to-string (concat "mv /tmp/screen.png "
                                     screenshot-name))
    (insert (concat "[[" screenshot-name "]]"))))

(defun render-everything ()
  "Render all latex previews and toggle inline images"
  (interactive)
  (org-latex-preview '(16))
  (org-redisplay-inline-images))


(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))

(use-package adaptive-wrap
  :hook
  (org-mode . adaptive-wrap-prefix-mode))

;;;* lilypond in org
(defun org-babel-lilypond-get-header-args (mode)
  "Default arguments to use when evaluating a lilypond source block.
These depend upon whether we are in Arrange mode i.e. MODE is t."
  (cond (mode
         '((:tangle . "yes")
           (:noweb . "yes")
           (:results . "silent")
           (:cache . "yes")
           (:prologue . "\\paper{indent=0\\mm\nline-width=170\\mm\noddFooterMarkup=##f\noddHeaderMarkup=##f\nbookTitleMarkup=##f\nscoreTitleMarkup=##f}")
           (:comments . "yes")))
        (t
         '((:results . "file")
           (:prologue . "\\paper{indent=0\\mm\nline-width=170\\mm\noddFooterMarkup=##f\noddHeaderMarkup=##f\nbookTitleMarkup=##f\nscoreTitleMarkup=##f}")
           (:exports . "results")))))

;;;* org-roam

(use-package org-roam
  :hook
  (after-init . org-roam-mode)
  :custom
  (org-roam-directory "~/org/")
  :config
  ;; stolen from org-roam-dailies.el
  (setq org-roam-dailies-capture-templates
        '(("d" "daily" plain (function org-roam-capture--get-point)
           ""
           :immediate-finish t
           :file-name "journal/%<%Y-%m-%d>"
           :head "#+TITLE: Journal %<%Y-%m-%d>"))))
(require 'org-roam-protocol)

;; not sure if this is required anymore
(setq org-agenda-file-regexp "\\`\\\([^.].*\\.org\\\|[0-9]\\\{8\\\}\\\(\\.gpg\\\)?\\\)\\'")
;;;* cleverparens
(use-package smartparens)
(require 'smartparens-config)
(use-package evil-cleverparens
  :hook
  ((emacs-lisp-mode . evil-cleverparens-mode)
   (evil-cleverparens-mode . smartparens-mode)))

;;;* rainbow-delimiters
(use-package
  rainbow-delimiters
  :hook
  (emacs-lisp-mode-hook . rainbow-delimiters-mode)
  (clojure-mode-hook . rainbow-delimiters-mode)
  (cider-repl-mode-hook . rainbow-delimiters-mode)
  (python-mode-hook . rainbow-delimiters-mode))

;;;* clojure
(use-package clojure-mode
  :hook
  ((clojure-mode . evil-cleverparens-mode)
   (clojurescript-mode . evil-cleverparens-mode)))

(use-package cider
  :config
  (define-key cider-repl-mode-map (kbd "C-n") #'cider-repl-next-input)
  (define-key cider-repl-mode-map (kbd "C-p") #'cider-repl-previous-input))

;;;* Add M-x kill-process (to kill the current buffer's process).
(put 'kill-process 'interactive-form
     '(interactive
       (let ((proc (get-buffer-process (current-buffer))))
         (if (process-live-p proc)
             (unless (yes-or-no-p (format "Kill %S? " proc))
               (error "Process not killed"))
           (error (format "Buffer %s has no process" (buffer-name))))
         nil)))





;;;* magit
(use-package
  magit)

(use-package
  evil-magit
  :after magit)
;;;* c and c++


;;;* company
(use-package
  company
  :defer t
  :init (add-hook 'after-init-hook 'global-company-mode)
  :config (use-package
            company-irony
            :defer t)
  (setq company-idle-delay 0.0
        company-minimum-prefix-length 1
        company-tooltip-limit 20
        company-dabbrev-downcase nil
        company-backends '((company-irony company-gtags)))
  (define-key company-active-map (kbd "M-n") nil)
  (define-key company-active-map (kbd "M-p") nil)
  (define-key company-active-map (kbd "C-n") #'company-select-next)
  (define-key company-active-map (kbd "C-p") #'company-select-previous)
  :bind ("C-;" . company-complete-common))

(use-package
  company-quickhelp
  :init (add-hook 'company-mode-hook 'company-quickhelp-mode)
  :config (setq company-quickhelp-delay 0.0))


;;;* lsp-mode
(use-package
  lsp-mode
  :init (setq lsp-keymap-prefix "C-l")
  :hook ((python-mode . lsp)
         (c++-mode-hook . lsp))
  :commands lsp)

(require 'lsp-clients)
(use-package
  lsp-ui
  :commands lsp-ui-mode)

(use-package
  company-lsp
  :commands company-lsp
  :config
  (push 'company-lsp company-backends))

(use-package dap-mode
  :hook ((python-mode . dap-mode)
         (python-mode . dap-ui-mode)
         (python-mode . dap-tooltip-mode))
  :bind (:map dap-mode-map
              (("<f5>" . dap-next)))
  :config
  (require 'dap-python))

(use-package
  lsp-ivy
  :commands lsp-ivy-workspace-symbol
  :config
  (setq lsp-ivy-show-symbol-kind t)
  (setq lsp-ivy-filter-symbol-kind '(0 2 13))
  (setq lsp-python-ms-python-executable-cmd "/home/orausch/.local/opt/miniconda3/envs/onnx/bin/python"))

;;;* python
(use-package
  yapfify
  :after python)

(use-package python-black
  :after python)

(use-package
  conda
  :load-path "~/sources/conda.el"
  :commands conda-env-activate
  :config
  (setq conda-anaconda-home "/home/orausch/.local/opt/miniconda3/")
  (setq conda-env-home-directory "/home/orausch/.local/opt/miniconda3/")
  (conda-env-initialize-interactive-shells))

(add-hook 'inferior-python-mode-hook
          (lambda ()
            (setq
             indent-tabs-mode nil
             tab-width 4)))


(defun my-send-current-line ()
  (interactive)
  (python-shell-send-string (buffer-substring-no-properties (line-beginning-position) (line-end-position))))

(defun my-maybe-activate ()
  (interactive)
  (unless (and  (boundp 'conda-env-current-name) conda-env-current-name)
    (conda-env-activate)))

(advice-add 'python-mode :before #'my-maybe-activate)

(setq python-shell-interpreter "ipython")
(setenv "PYTHONPATH" "/home/orausch/sources/dace/")

(defun my-python-top-level-def ()
  (interactive)
  (move-beginning-of-line nil)
  (while (not (equal (string (char-after (point)))
                     "d"))
    (python-nav-backward-defun)
    (move-beginning-of-line nil)))

(use-package python-pytest
  :config
  (defun python-pytest--current-defun ()
    (save-excursion
      (my-python-top-level-def)
      (python-info-current-defun))))

(defun my-pytest-file-debug ()
  (interactive)
  (python-pytest-file (buffer-file-name) '("--pdb")))

(defun my-pytest-function-debug ()
  (interactive)
  (python-pytest-function-dwim
   (buffer-file-name)
   (python-pytest--current-defun)
   '("--pdb")))


(setenv "DACE_optimizer_interface" "")


(use-package
  lsp-python-ms
  :hook (python-mode . (lambda ()
                         (require 'lsp-python-ms)
                         (lsp))))

(use-package jupyter)

;; no idea why this works
;; see https://emacs.stackexchange.com/questions/44880/use-package-bind-not-working-as-expected
(use-package jupyter-repl
  :ensure nil
  :bind
  (:map jupyter-repl-mode-map
        (("C-p" . jupyter-repl-history-previous)
         ("C-n" . jupyter-repl-history-next))))



;;;* treemacs
(use-package neotree
  :config
  (add-hook 'neotree-mode-hook
            (lambda ()
              (define-key evil-normal-state-local-map (kbd "TAB") 'neotree-enter)
              (define-key evil-normal-state-local-map (kbd "SPC") 'neotree-quick-look)
              (define-key evil-normal-state-local-map (kbd "q") 'neotree-hide)
              (define-key evil-normal-state-local-map (kbd "RET") 'neotree-enter)
              (define-key evil-normal-state-local-map (kbd "g") 'neotree-refresh)
              (define-key evil-normal-state-local-map (kbd "n") 'neotree-next-line)
              (define-key evil-normal-state-local-map (kbd "p") 'neotree-previous-line)
              (define-key evil-normal-state-local-map (kbd "A") 'neotree-stretch-toggle)
              (define-key evil-normal-state-local-map (kbd "H") 'neotree-hidden-file-toggle))))

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

(use-package
  treemacs)

(use-package
  treemacs-projectile)

(use-package
  treemacs-evil)

(use-package
  lsp-treemacs
  :commands lsp-treemacs-errors-list)


;;;* comint mode
;; I think this was to make c-p and c-n work?
(add-hook 'comint-mode-hook (lambda ()
                              (local-set-key [14]
                                             (quote comint-next-input))
                              (local-set-key [16]
                                             (quote comint-previous-input))
                              (local-set-key [18]
                                             (quote comint-history-isearch-backward))))

(add-hook 'shell-mode-hook
      (lambda ()
        (face-remap-set-base 'comint-highlight-prompt :inherit nil)))



;;;* elfeed
(use-package elfeed)
(setq elfeed-feeds
      '(("https://xkcd.com/atom.xml" comic)
        ("https://drewdevault.com/feed.xml" blog)
        ("https://danluu.com/atom.xml" blog)
        ("http://fabiensanglard.net/rss.xml" blog)
        ("https://lwn.net/headlines/rss" blog)
        ("https://reddit-top-rss.herokuapp.com/?subreddit=space&averagePostsPerDay=5&view=rss" reddit)
        ("https://reddit-top-rss.herokuapp.com/?subreddit=linux&averagePostsPerDay=5&view=rss" reddit)
        ("https://reddit-top-rss.herokuapp.com/?subreddit=programming&averagePostsPerDay=10&view=rss" reddit)
        ("https://reddit-top-rss.herokuapp.com/?subreddit=MachineLearning&averagePostsPerDay=5&view=rss" reddit)
        ("https://reddit-top-rss.herokuapp.com/?subreddit=math&averagePostsPerDay=5&view=rss" reddit)
        ("https://reddit-top-rss.herokuapp.com/?subreddit=Formula1&averagePostsPerDay=5&view=rss" reddit)
        ("https://reddit-top-rss.herokuapp.com/?subreddit=emacs&averagePostsPerDay=1&view=rss" reddit)))
;;;* Customize
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(add-hook (quote after-init-hook) t)
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["#1e1e1e" "#D16969" "#579C4C" "#D7BA7D" "#339CDB" "#C586C0" "#85DDFF" "#d4d4d4"])
 '(clang-format-style "google")
 '(column-number-mode t)
 '(compilation-message-face (quote default))
 '(cua-global-mark-cursor-color "#11948b")
 '(cua-normal-cursor-color "#596e76")
 '(cua-overwrite-cursor-color "#a67c00")
 '(cua-read-only-cursor-color "#778c00")
 '(custom-safe-themes
   (quote
    ("d0e57771a8b61d0166fb2dde379772c6fcaf35d17f68a7f5b9a148357a6219ac" "a06658a45f043cd95549d6845454ad1c1d6e24a99271676ae56157619952394a" default)))
 '(dap-auto-show-output nil)
 '(dap-ui-controls-mode t nil (dap-ui))
 '(dired-sidebar-should-follow-file t)
 '(fci-rule-color "#37474F" t)
 '(fill-column 100)
 '(gdb-show-main t)
 '(highlight-changes-colors (quote ("#c42475" "#5e65b6")))
 '(highlight-symbol-colors
   (quote
    ("#ec90da49b1e9" "#ccb4e1bdd0ac" "#fb9eca14b38f" "#d89bd3eadcf9" "#de29dee7b293" "#f675cca1ae79" "#d05fdab7e079")))
 '(highlight-symbol-foreground-color "#5d737a")
 '(highlight-tail-colors
   (quote
    (("#f4eedb" . 0)
     ("#a8b84b" . 20)
     ("#66c1b3" . 30)
     ("#6fa5e7" . 50)
     ("#d6a549" . 60)
     ("#ed6e3e" . 70)
     ("#f46495" . 85)
     ("#f4eedb" . 100))))
 '(hl-bg-colors
   (quote
    ("#d6a549" "#ed6e3e" "#ff6243" "#f46495" "#837bdf" "#6fa5e7" "#66c1b3" "#a8b84b")))
 '(hl-fg-colors
   (quote
    ("#fffce9" "#fffce9" "#fffce9" "#fffce9" "#fffce9" "#fffce9" "#fffce9" "#fffce9")))
 '(hl-paren-colors (quote ("#11948b" "#a67c00" "#007ec4" "#5e65b6" "#778c00")))
 '(indent-tabs-mode nil)
 '(initial-buffer-choice t)
 '(jdee-db-active-breakpoint-face-colors (cons "#171F24" "#237AD3"))
 '(jdee-db-requested-breakpoint-face-colors (cons "#171F24" "#579C4C"))
 '(jdee-db-spec-breakpoint-face-colors (cons "#171F24" "#777778"))
 '(lsp-ui-doc-border "#5d737a")
 '(lua-indent-level 4)
 '(neo-autorefresh t)
 '(nrepl-message-colors
   (quote
    ("#cc1f24" "#bb3e06" "#a67c00" "#4f6600" "#a8b84b" "#005797" "#11948b" "#c42475" "#5e65b6")))
 '(objed-cursor-color "#D16969")
 '(org-adapt-indentation nil)
 '(org-babel-load-languages
   (quote
    ((emacs-lisp . t)
     (python . t)
     (C . t)
     (ditaa . t)
     (lilypond . t))))
 '(org-capture-templates
   (quote
    (("i" "Inbox" entry
      (file "~/org/inbox.org")
      "* TODO")
     ("m" "Meeting Notes- Bachelor's Thesis" entry
      (function org-journal-find-location)
      "* Meeting %(format-time-string org-journal-time-format) :meeting:dace:
%i%?")
     ("j" "Journal entry" entry
      (function org-journal-find-location)
      "*  %(format-time-string org-journal-time-format)%^{Title} :%(projectile-project-name):
%i%?")
     ("L" "Protocol Link" entry
      (file+headline "~/org/inbox.org" "Inbox")
      "* TODO %? [[%:link][%:description]]
Captured On: %U"))))
 '(org-clock-out-when-done (quote ("DONE" "WAITING")))
 '(org-export-backends (quote (ascii html icalendar latex md odt)))
 '(org-fontify-whole-heading-line t)
 '(org-roam-directory "~/org/")
 '(org-roam-graph-exclude-matcher (quote ("journal" "_tag")))
 '(org-roam-graph-viewer "~/.local/opt/firefox/firefox")
 '(package-selected-packages
   (quote
    (adaptive-wrap benchark-init rg jupyter-repl jupyter emacs-jupyter elfeed tango-plus-theme idle-highlight-mode auctex solarized-theme gruvbox-theme neotree general which-key lsp-python-ms evil-cleverparens cider treemacs-projectile dashboard python-black python-pytest org-roam posframe dap-mode lsp-ivy elisp-format org htmlize yaml-mode use-package treemacs-evil ripgrep realgud rainbow-delimiters pyvenv protobuf-mode projectile org-journal magit-popup lua-mode lsp-ui lsp-treemacs highlight-indentation ghub flycheck find-file-in-project evil-surround evil-magit evil-leader evil-commentary evil-collection dired-subtree counsel conda company-quickhelp company-lsp company-irony clang-format+ bind-map benchmark-init all-the-icons-ivy all-the-icons-dired)))
 '(pdf-view-midnight-colors (cons "#d4d4d4" "#1e1e1e"))
 '(pos-tip-background-color "#f4eedb")
 '(pos-tip-foreground-color "#5d737a")
 '(ripgrep-arguments
   (quote
    ("--type-not css" "--type-not html" "-g '!*.sdfg'" "-g '!*.ipynb'" "-g '!TAGS'" "--type-not js")))
 '(rustic-ansi-faces
   ["#1e1e1e" "#D16969" "#579C4C" "#D7BA7D" "#339CDB" "#C586C0" "#85DDFF" "#d4d4d4"])
 '(safe-local-variable-values (quote ((eval outline-hide-sublevels 4))))
 '(show-paren-mode t)
 '(smartrep-mode-line-active-bg (solarized-color-blend "#778c00" "#f4eedb" 0.2))
 '(term-default-bg-color "#fffce9")
 '(term-default-fg-color "#596e76")
 '(tool-bar-mode nil)
 '(vc-annotate-background "#1e1e1e")
 '(vc-annotate-background-mode nil)
 '(vc-annotate-color-map
   (list
    (cons 20 "#579C4C")
    (cons 40 "#81a65c")
    (cons 60 "#acb06c")
    (cons 80 "#D7BA7D")
    (cons 100 "#d8ab79")
    (cons 120 "#d99c76")
    (cons 140 "#DB8E73")
    (cons 160 "#d38b8c")
    (cons 180 "#cc88a6")
    (cons 200 "#C586C0")
    (cons 220 "#c97ca3")
    (cons 240 "#cd7286")
    (cons 260 "#D16969")
    (cons 280 "#ba6c6c")
    (cons 300 "#a37070")
    (cons 320 "#8d7374")
    (cons 340 "#37474F")
    (cons 360 "#37474F")))
 '(vc-annotate-very-old-color nil)
 '(weechat-color-list
   (quote
    (unspecified "#fffce9" "#f4eedb" "#990001" "#cc1f24" "#4f6600" "#778c00" "#785700" "#a67c00" "#005797" "#007ec4" "#93004d" "#c42475" "#006d68" "#11948b" "#596e76" "#88999b")))
 '(xterm-color-names
   ["#f4eedb" "#cc1f24" "#778c00" "#a67c00" "#007ec4" "#c42475" "#11948b" "#002b37"])
 '(xterm-color-names-bright
   ["#fffce9" "#bb3e06" "#98a6a6" "#88999b" "#596e76" "#5e65b6" "#5d737a" "#00212b"]))


(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Iosevka Fixed" :foundry "BE5N" :slant normal :weight normal :height 128 :width normal))))
 '(jupyter-repl-input-prompt ((t (:foreground "dark green"))))
 '(jupyter-repl-output-prompt ((t (:foreground "red4"))))
 '(rainbow-delimiters-depth-2-face ((t (:foreground "blue"))))
 '(rainbow-delimiters-depth-3-face ((t (:foreground "dark orange"))))
 '(rainbow-delimiters-depth-4-face ((t (:foreground "green4"))))
 '(rainbow-delimiters-depth-5-face ((t (:foreground "dark violet"))))
 '(rainbow-delimiters-depth-6-face ((t (:foreground "saddle brown")))))


;;;* keymaps
(defconst my-leader "SPC")

(use-package general
  :config)

(general-create-definer my-leader-def
  :prefix my-leader)

(my-leader-def
  :states '(normal emacs motion)

  ;; projectile
  "p" '(:ignore t :which-key "projects")
  "p f" '(projectile-find-file :which-key "find file")
  "p s" '(projectile-save-project-buffers :which-key "save buffers")
  "p k" '(projectile-kill-buffers :which-key "kill buffers")
  "p p" '(projectile-switch-project :which-key "open project")

  "r" '(rg-menu :which-key "ripgrep")

  ;; highlighting
  "h" '(:ignore t :which-key "highlighting")
  "h s" '(highlight-symbol-at-point :which-key "highlight symbol")
  "h h" '((lambda () (interactive) (unhighlight-regexp t)) :which-key "unhighlight all")

  ;; files
  "f" '(:ignore t :which-key "files")
  "f f" 'find-file
  "f i" `(,(lambda ()
             (interactive)
             (find-file "~/.dotfiles/emacs/.emacs.d/init.el")) :which-key "init.el")
  "f t" 'org-roam-dailies-today

  ;; org stuff
  "a" 'org-agenda
  "c" 'org-capture

  "q" '(quit-window :which-key"quit window")

  "p" '(:ignore t :which-key "org links")
  "p s" 'org-store-link
  "p i" 'org-insert-link

  ;; help functions
  "d" '(:ignore t :which-key "describe")
  "d v" 'describe-variable
  "d f" 'describe-function

  ;; roam
  "n" '(:ignore t :which-key "roam")
  "n l" '(org-roam :which-key "sidebar")
  "n f" '(org-roam-find-file :which-key "find file")
  "n g" '(org-roam-graph :which-key "graph")

  ;; others
  "g" 'magit-status
  "b" '(neotree-project-dir :which-key "files sidebar"))

(my-leader-def
  :keymaps 'emacs-lisp-mode-map
  :states 'normal
  ;; for use in init.el
  "o" 'org-cycle)

(my-leader-def
  :keymaps 'lsp-mode-map
  :states 'normal

  "l" '(:ignore t :which-key "lsp")
  "l l" '(lsp-find-definition :which-key "definition")
  "l g" '(lsp-find-references :which-key "references")
  "l r" '(lsp-rename :which-key "rename")
  "l s" '(lsp-ivy-workspace-symbol :which-key "find symbol"))

(my-leader-def
  :keymaps 'org-mode-map
  :states 'normal
  "s" 'insert-screenshot-at-point
  "y" 'render-everything
  "n i" '(org-roam-insert :which-key "insert link"))

(my-leader-def
  :keymaps 'smerge-mode-map
  :states 'normal
  "s" 'smerge-keep-current)

(my-leader-def
  :keymaps 'dap-mode-map
  :states 'normal

  "d" '(:ignore t :which-key "debug")
  "d d" '(dap-debug :which-key "debug")
  "d n" '(dap-next :which-key "next (<f5>)")
  "d c" 'dap-continue
  "d i" '(dap-step-in :which-key "in (<f6>)")
  "d b" 'dap-breakpoint-toggle
  "d r" 'dap-ui-repl
  "d e" 'dap-eval
  "d s" 'dap-switch-stack-frame)

(my-leader-def
  :keymaps 'python-mode-map
  :states 'normal

  "k" '(:ignore t :which-key "format code")
  "k y" '(yapfify-region-or-buffer :which-key "yapf")

  "i" '(:ignore t :which-key "interactive")
  "i i" '(jupyter-inspect-at-point :which-key "jupyter inspect at point")
  "i j" '(jupyter-run-repl :which-key "start jupyter repl")
  "i b" '(jupyter-eval-buffer :which-key "send buffer")
  "i s" '(jupyter-repl-scratch-buffer :which-key "open scratch buffer")
  "i RET" '(jupyter-eval-line-or-region :which-key "send region or line (C-c C-c)")
  "i k" '(jupyter-repl-interrupt-kernel :which-key "interrupt kernel")
  "i r" '(jupyter-repl-restart-kernel :which-key "restart kernel")

  "t" '(:ignore t :which-key "tests")
  "t b" '(python-pytest-file :which-key "buffer")
  "t f" '(python-pytest-function-dwim :which-key "function")
  "t r" '(python-pytest-repeat :which-key "repeat")
  "t B" '(my-pytest-file-debug :which-key "debug buffer")
  "t F" '(my-pytest-function-debug :which-key "debug function"))



(evil-set-initial-state 'elfeed-search-mode 'emacs)
(evil-set-initial-state 'elfeed-show-mode 'emacs)

(general-define-key
 :states '(emacs normal)
 :keymaps 'elfeed-show-mode-map
 "h" 'left-char
 "l" 'right-char
 "j" 'next-line
 "k" 'previous-line
 "q" 'quit-window)

(general-define-key
 :states '(emacs normal)
 :keymaps 'elfeed-search-mode-map
 "j" 'next-line
 "k" 'previous-line)

;; Make evil-mode up/down operate in screen lines instead of logical lines
(define-key evil-motion-state-map "j" 'evil-next-visual-line)
(define-key evil-motion-state-map "k" 'evil-previous-visual-line)
;; Also in visual mode
(define-key evil-visual-state-map "j" 'evil-next-visual-line)
(define-key evil-visual-state-map "k" 'evil-previous-visual-line)

;;;* Disable Speed hacks
(add-hook 'emacs-startup-hook (lambda ()
                                (setq gc-cons-threshold 16777216 gc-cons-percentage 0.1
                                      file-name-handler-alist last-file-name-handler-alist)))
(server-start)
;;;* make outline mode work
;; Local Variables:
;; outline-regexp: ";;;\\*+\\|\\`"
;; eval: (outline-minor-mode 1)
;; eval: (outline-hide-sublevels 4)
;; End:
