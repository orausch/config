;;;* Startup Speed Hacks
;;(setq frame-inhibit-implied-resize t)
;;(defvar last-file-name-handler-alist file-name-handler-alist)
;;(setq gc-cons-threshold 100000000)
;;(setq compilation-finish-functions (lambda (buf str)
;;                                     (if (null (string-match ".*exited abnormally.*" str))
;;                                         ;;no errors, make the compilation window go away in a few seconds
;;                                         (progn (run-at-time "1 sec" nil 'delete-windows-on
;;                                                             (get-buffer-create "*compilation*"))
;;                                                (message "No Compilation Errors!")))))
;;(setq compilation-scroll-output t)
;;(setq compilation-window-height 20)
;;(setq inhibit-compacting-font-caches t)

;;;* Modeline

;; show full buffer path in modeline
(with-eval-after-load 'subr-x
  (setq-default mode-line-buffer-identification
                '(:eval (format-mode-line (propertized-buffer-identification (or (when-let* ((buffer-file-truename buffer-file-truename)
                                                                                             (prj (cdr-safe (project-current)))
                                                                                             (prj-parent (file-name-directory (directory-file-name (expand-file-name prj)))))
                                                                                   (concat (file-relative-name (file-name-directory buffer-file-truename) prj-parent) (file-name-nondirectory buffer-file-truename)))
                                                                                 "%b"))))))
;;;* misc emacs stuff
(setq backup-directory-alist '(("." . "~/.emacs.d/backup")) backup-by-copying t version-control t
      delete-old-versions t kept-new-versions 5 kept-old-versions 5)
(add-to-list 'exec-path "/home/orausch/.local/bin")
(add-to-list 'load-path "~/.emacs.d/elisp")

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

(use-package evil-commentary)

(add-hook 'prog-mode-hook 'evil-commentary-mode)

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
(require 'minimal-light-theme)

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
  markdown-mode)

(use-package rg)
(use-package protobuf-mode)

;; (use-package
;;  gruvbox-theme)
;;;* ivy
(use-package
  ivy
  :config (setq projectile-completion-system 'ivy)
  (ivy-mode 1))

;;;* org

(require 'org-variable-pitch)

;; RICING ORG MODE
(setq org-hide-emphasis-markers t)

;; replace '-' with bullet points
;;(font-lock-add-keywords 'org-mode
;;                        '(("^ *\\([-]\\) "
;;                           (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

(require 'org-variable-pitch)
(add-hook 'org-mode-hook 'org-variable-pitch-minor-mode)



;; OTHER (MAINLY FUNCTIONAL) STUFF
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

;; fix <s expansion in org mode
(require 'org-tempo)

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
  (shell-command-to-string "maim -s /tmp/screen.png")
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

;;;* org-roam

(use-package deft
  :after org
  :custom
  (deft-recursive t)
  (deft-use-filter-string-for-filename t)
  (deft-default-extension "org")
  (deft-directory "~/org"))

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

(use-package org-roam-server)

;; not sure if this is required anymore
(setq org-agenda-file-regexp "\\`\\\([^.].*\\.org\\\|[0-9]\\\{8\\\}\\\(\\.gpg\\\)?\\\)\\'")


;;;* cleverparens
(use-package smartparens)
(require 'smartparens-config)
(use-package evil-cleverparens
  :hook
  ((emacs-lisp-mode . evil-cleverparens-mode)
   (evil-cleverparens-mode . smartparens-mode)))

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
;;;* ocaml

(use-package
  tuareg)

;;;* python
(use-package
  yapfify
  :after python)

(use-package python-black
  :after python)

(add-hook 'inferior-python-mode-hook
          (lambda ()
            (setq
             indent-tabs-mode nil
             tab-width 4)))


(defun my-send-current-line ()
  (interactive)
  (python-shell-send-string (buffer-substring-no-properties (line-beginning-position) (line-end-position))))

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



;;;* Customize
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(add-hook (quote after-init-hook) t)
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(clang-format-style "google")
 '(column-number-mode t)
 '(compilation-message-face (quote default))
 '(custom-enabled-themes (quote (leuven)))
 '(fci-rule-color "#37474F" t)
 '(fill-column 100)
 '(indent-tabs-mode nil)
 '(initial-buffer-choice t)
 '(lua-indent-level 4)
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
 '(org-roam-buffer-no-delete-other-windows t)
 '(org-roam-directory "~/org/")
 '(org-roam-graph-exclude-matcher (quote ("journal")))
 '(org-roam-graph-viewer "~/.local/opt/firefox/firefox")
 '(package-selected-packages
   (quote
    (tuareg yapfify which-key use-package rg python-black protobuf-mode projectile org-roam-server markdown-mode ivy general evil-surround evil-org evil-magit evil-commentary evil-cleverparens deft adaptive-wrap)))
 '(ripgrep-arguments
   (quote
    ("--type-not css" "--type-not html" "-g '!*.sdfg'" "-g '!*.ipynb'" "-g '!TAGS'" "--type-not js")))
 '(safe-local-variable-values (quote ((eval outline-hide-sublevels 4))))
 '(show-paren-mode t)
 '(tool-bar-mode nil)
 '(vc-annotate-background-mode nil))

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
             (find-file "~/sources/config/emacs/.emacs.d/init.el")) :which-key "init.el")
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
  "n s" '(org-roam-server-mode :which-key "start server")
  "n d" '(deft :which-key "deft (search)")

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
  "l g" '(lsp-peek-find-references :which-key "references")
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

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
;; Local Variables:
;; outline-regexp: ";;;\\*+\\|\\`"
;; eval: (outline-minor-mode 1)
;; eval: (outline-hide-sublevels 4)
;; End:
;; ## added by OPAM user-setup for emacs / base ## 56ab50dc8996d2bb95e7856a6eddb17b ## you can edit, but keep this line
(require 'opam-user-setup "~/.emacs.d/opam-user-setup.el")
;; ## end of OPAM user-setup addition for emacs / base ## keep this line
