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

;;;* use-package
(package-initialize)
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t)
(require 'use-package-ensure)
(setq use-package-always-ensure t)

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

(add-hook 'artist-mode-hook #'artist-mode-toggle-emacs-state)

;;;* ivy
(use-package 
  ivy 
  :config (setq projectile-completion-system 'ivy) 
  (ivy-mode 1))

;;;* benchmark-init
;; (use-package benchark-init
;;   :config
;;   ;; To disable collection of benchmark data after init is done.
;;   (add-hook 'after-init-hook 'benchmark-init/deactivate))

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
(add-to-list 'org-agenda-files "~/org/tasks.org") 

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
(require 'org-roam-protocol)

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




;;;* evil
(use-package 
  evil 
  :config (evil-mode))

;; I hate emacs-state, remove it
(with-eval-after-load 'evil
  (define-key evil-motion-state-map (kbd "C-z") nil))

;; suspend-frame is also stupid
(define-key global-map (kbd "C-z") nil)
(define-key global-map (kbd "C-x C-z") nil)

(use-package evil-surround
  :ensure t
  :config
  (global-evil-surround-mode 1))

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

;;;* magit
(use-package 
  magit)

(use-package 
  evil-magit 
  :after magit)
;;;* c and c++
(use-package 
  clang-format+ 
  :config (add-hook 'c-mode-common-hook #'clang-format+-mode))

(use-package 
  irony 
  :after clang-format 
  :init (add-hook 'c++-mode-hook 'irony-mode) 
  (add-hook 'c-mode-hook 'irony-mode) 
  (add-hook 'objc-mode-hook 'irony-mode) 
  :config
  ;; replace the `completion-at-point' and `complete-symbol' bindings in
  ;; irony-mode's buffers by irony-mode's function
  (defun my-irony-mode-hook () 
    (define-key irony-mode-map [remap completion-at-point] 'irony-completion-at-point-async) 
    (define-key irony-mode-map [remap complete-symbol] 'irony-completion-at-point-async)) 
  (add-hook 'irony-mode-hook 'my-irony-mode-hook) 
  (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options))

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
  :hook ((python-mode . lsp)) 
  :commands lsp)

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
  :after python ;; this makes the keybinding below work somehow
  :commands lsp-ivy-workspace-symbol 
  :config
  (setq lsp-ivy-show-symbol-kind t) 
  (setq lsp-ivy-filter-symbol-kind '(0 2 13)) 
  (setq lsp-python-ms-python-executable-cmd "/home/orausch/.local/opt/miniconda3/envs/onnx/bin/python"))

;;;* python
(use-package 
  yapfify 
  :load-path "/home/orausch/repos/yapfify"
  :after python)

(use-package python-black
  :after python)

(use-package 
  conda 
  :commands conda-env-activate
  :config
  (setq conda-anaconda-home "/home/orausch/.local/opt/miniconda3/base/")
  (setq conda-env-home-directory "/home/orausch/.local/opt/miniconda3/")
  (conda-env-initialize-interactive-shells)
  (conda-env-activate "onnx"))

(setq python-shell-interpreter "ipython" python-shell-interpreter-args "-i --simple-prompt")
(setenv "PYTHONPATH" "/home/orausch/repos/dace")

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


(use-package highlight-indent-guides
  :hook (python-mode . highlight-indent-guides-mode)
  :config
  (setq highlight-indent-guides-method 'character))
(use-package 
  lsp-python-ms 
  :hook (python-mode . (lambda () 
			 (require 'lsp-python-ms) 
			 (lsp))))


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



(use-package latex-preview-pane
  :config
  (latex-preview-pane-enable))


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
 '(custom-enabled-themes (quote (leuven)))
 '(custom-safe-themes
   (quote
    ("777a3a89c0b7436e37f6fa8f350cbbff80bcc1255f0c16ab7c1e82041b06fccd" "a339f231e63aab2a17740e5b3965469e8c0b85eccdfb1f9dbd58a30bdad8562b" "d71aabbbd692b54b6263bfe016607f93553ea214bc1435d17de98894a5c3a086" "a83f05e5e2f2538376ea2bfdf9e3cd8b7f7593b16299238c1134c1529503fa88" "bc836bf29eab22d7e5b4c142d201bcce351806b7c1f94955ccafab8ce5b20208" "fa3bdd59ea708164e7821574822ab82a3c51e262d419df941f26d64d015c90ee" "cb96a06ed8f47b07c014e8637bd0fd0e6c555364171504680ac41930cfe5e11e" "f9cae16fd084c64bf0a9de797ef9caedc9ff4d463dd0288c30a3f89ecf36ca7e" "51956e440cec75ba7e4cff6c79f4f8c884a50b220e78e5e05145386f5b381f7b" "c83c095dd01cde64b631fb0fe5980587deec3834dc55144a6e78ff91ebc80b19" "730a87ed3dc2bf318f3ea3626ce21fb054cd3a1471dcd59c81a4071df02cb601" "7c4cfa4eb784539d6e09ecc118428cd8125d6aa3053d8e8413f31a7293d43169" "6231254e74298a1cf8a5fee7ca64352943de4b495e615c449e9bb27e2ccae709" "0ad7f1c71fd0289f7549f0454c9b12005eddf9b76b7ead32a24d9cb1d16cbcbd" "3e3a1caddeee4a73789ff10ba90b8394f4cd3f3788892577d7ded188e05d78f4" "93ed23c504b202cf96ee591138b0012c295338f38046a1f3c14522d4a64d7308" "9b01a258b57067426cc3c8155330b0381ae0d8dd41d5345b5eddac69f40d409b" "6bacece4cf10ea7dd5eae5bfc1019888f0cb62059ff905f37b33eec145a6a430" "7d708f0168f54b90fc91692811263c995bebb9f68b8b7525d0e2200da9bc903c" "615123f602c56139c8170c153208406bf467804785007cdc11ba73d18c3a248b" "1d50bd38eed63d8de5fcfce37c4bb2f660a02d3dff9cbfd807a309db671ff1af" "d5f8099d98174116cba9912fe2a0c3196a7cd405d12fa6b9375c55fc510988b5" "285efd6352377e0e3b68c71ab12c43d2b72072f64d436584f9159a58c4ff545a" "e074be1c799b509f52870ee596a5977b519f6d269455b84ed998666cf6fc802a" "be9645aaa8c11f76a10bcf36aaf83f54f4587ced1b9b679b55639c87404e2499" "845103fcb9b091b0958171653a4413ccfad35552bc39697d448941bcbe5a660d" default)))
 '(dired-sidebar-should-follow-file t)
 '(fci-rule-color "#37474F")
 '(fill-column 100)
 '(gdb-show-main t)
 '(indent-tabs-mode nil)
 '(initial-buffer-choice t)
 '(jdee-db-active-breakpoint-face-colors (cons "#171F24" "#237AD3"))
 '(jdee-db-requested-breakpoint-face-colors (cons "#171F24" "#579C4C"))
 '(jdee-db-spec-breakpoint-face-colors (cons "#171F24" "#777778"))
 '(lua-indent-level 4)
 '(menu-bar-mode nil)
 '(neo-autorefresh t)
 '(objed-cursor-color "#D16969")
 '(org-adapt-indentation nil)
 '(org-babel-load-languages
   (quote
    ((emacs-lisp . t)
     (python . t)
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
 '(org-fontify-whole-heading-line t)
 '(org-roam-directory "~/org/")
 '(org-roam-graph-exclude-matcher (quote ("journal" "_tag")))
 '(org-roam-graph-viewer "~/.local/opt/firefox/firefox")
 '(package-selected-packages
   (quote
    (neotree general which-key lsp-python-ms evil-cleverparens cider treemacs-projectile highlight-indent-guides dashboard python-black python-pytest org-roam posframe dap-mode lsp-ivy elisp-format org htmlize yaml-mode use-package treemacs-evil ripgrep realgud rainbow-delimiters pyvenv protobuf-mode projectile org-journal magit-popup lua-mode lsp-ui lsp-treemacs highlight-indentation ghub flycheck find-file-in-project evil-surround evil-magit evil-leader evil-commentary evil-collection dired-subtree counsel conda company-quickhelp company-lsp company-irony clang-format+ bind-map benchmark-init all-the-icons-ivy all-the-icons-dired)))
 '(pdf-view-midnight-colors (cons "#d4d4d4" "#1e1e1e"))
 '(ripgrep-arguments
   (quote
    ("--type-not css" "--type-not html" "-g '!*.sdfg'" "-g '!*.ipynb'" "-g '!TAGS'" "--type-not js")))
 '(rustic-ansi-faces
   ["#1e1e1e" "#D16969" "#579C4C" "#D7BA7D" "#339CDB" "#C586C0" "#85DDFF" "#d4d4d4"])
 '(safe-local-variable-values (quote ((eval outline-hide-sublevels 4))))
 '(show-paren-mode t)
 '(tool-bar-mode nil)
 '(vc-annotate-background "#1e1e1e")
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
 '(vc-annotate-very-old-color nil))


(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
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
  :keymaps 'normal

  ;; projectile
  "p" '(:ignore t :which-key "projects")
  "p f" '(projectile-find-file :which-key "find file")
  "p s" '(projectile-save-project-buffers :which-key "save buffers")
  "p k" '(projectile-kill-buffers :which-key "kill buffers")
  "p p" '(projectile-switch-project :which-key "open project")
  "p r" '(projectile-ripgrep :which-key "ripgrep")

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

  "q" '(:ignore t :which-key "org links")
  "q s" 'org-store-link
  "q i" 'org-insert-link

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
  "d n" '(dap-next :which-key "next (<f5>)")
  "d c" 'dap-continue
  "d i" 'dap-step-in
  "d b" 'dap-breakpoint-toggle
  "d r" 'dap-repl
  "d e" 'dap-eval
  "d s" 'dap-switch-stack-frame)

(my-leader-def
  :keymaps 'python-mode-map
  :states 'normal

  "k" '(:ignore t :which-key "format code")
  "k y" '(yapfify-region-or-buffer :which-key "yapf")

  "t" '(:ignore t :which-key "tests")
  "t b" '(python-pytest-file :which-key "buffer")
  "t f" '(python-pytest-function-dwim :which-key "function")
  "t r" '(python-pytest-repeat :which-key "repeat")
  "t B" '(my-pytest-file-debug :which-key "debug buffer")
  "t F" '(my-pytest-function-debug :which-key "debug function"))



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
