(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3") ;; bugfix; remove in emacs 26.3+
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
(setq backup-directory-alist '(("." . "~/.emacs.d/backup")) backup-by-copying t version-control t
      delete-old-versions t kept-new-versions 5 kept-old-versions 5)

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(require 'use-package-ensure)
(setq use-package-always-ensure t)

(use-package 
  ivy 
  :config (setq projectile-completion-system 'ivy) 
  (ivy-mode 1))

;; (use-package benchmark-init
;;   :config
;;   ;; To disable collection of benchmark data after init is done.
;;   (add-hook 'after-init-hook 'benchmark-init/deactivate))

(add-hook 'org-mode-hook (lambda ()
                           (setq org-format-latex-options (plist-put org-format-latex-options :scale 1.5))))

(setq org-return-follows-link t)
;;(add-hook 'org-insert-heading-hook 'evil-insert-state)

(setq org-todo-keywords '((sequence "TODO(t)" "WAITING(w)" "|" "DONE(d)" "CANCELLED(c)")))

(require 'org-roam-protocol)



(use-package org-roam
  :hook 
  (after-init . org-roam-mode)
  :custom
  (org-roam-directory "~/org/")
  :bind (:map org-roam-mode-map
              (("C-c n l" . org-roam)
               ("C-c n f" . org-roam-find-file)
               ("C-c n b" . org-roam-switch-to-buffer)
               ("C-c n g" . org-roam-graph-show))
              :map org-mode-map
              (("C-c n i" . org-roam-insert))))


(use-package 
  evil 
  :config (evil-mode))

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

(use-package 
  elisp-format)

(use-package 
  markdown-mode)

(use-package 
  rainbow-delimiters 
  :config (add-hook 'emacs-lisp-mode-hook #'rainbow-delimiters-mode) 
  (add-hook 'python-mode-hook #'rainbow-delimiters-mode))

(use-package 
  magit 

  :config 
  :bind
  ("<f11>" . magit-status)
  (:map smerge-mode-map
	      ("C-c k" . smerge-keep-current)))


(use-package 
  evil-magit 
  :after magit)

(use-package 
  org-journal
  :after org
  :init
  (setq org-journal-dir "~/org/journal")
  (setq org-agenda-file-regexp "\\`\\\([^.].*\\.org\\\|[0-9]\\\{8\\\}\\\(\\.gpg\\\)?\\\)\\'")
  (add-to-list 'org-agenda-files org-journal-dir)
  (add-to-list 'org-agenda-files "~/org/tasks.org") 
  (setq org-journal-carryover-items "") 
  (setq org-journal-file-type 'weekly))

;;(defun my-translate-C-i (_prompt) 
;;  (if (and (= (length (this-single-command-raw-keys)) 1) 
;;	   (eql (aref (this-single-command-raw-keys) 0) ?\C-i) 
;;	   (bound-and-true-p evil-mode) 
;;	   (eq evil-state 'normal)) 
;;      (kbd "<C-i>") 
;;    (kbd "TAB")))
;;
;;(define-key key-translation-map (kbd "TAB") 'my-translate-C-i)
;;

(with-eval-after-load 'evil-maps
  (define-key evil-motion-state-map (kbd "C-d") nil))

(use-package 
  lua-mode)

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

(use-package 
  company 
  :defer t 
  :init (add-hook 'after-init-hook 'global-company-mode) 
  :config (use-package 
	    company-irony 
	    :defer t) 
  (setq company-idle-delay              0.0 company-minimum-prefix-length   1 company-tooltip-limit
	20 company-dabbrev-downcase        nil company-backends                '((company-irony
										  company-gtags))) 
  (define-key company-active-map (kbd "M-n") nil) 
  (define-key company-active-map (kbd "M-p") nil) 
  (define-key company-active-map (kbd "C-n") #'company-select-next) 
  (define-key company-active-map (kbd "C-p") #'company-select-previous) 
  (push 'company-lsp company-backends) 
  :bind ("C-;" . company-complete-common))

(use-package 
  company-quickhelp 
  :init (add-hook 'company-mode-hook 'company-quickhelp-mode) 
  :config (setq company-quickhelp-delay 0.0))

(use-package 
  yapfify 
  :after python 
  :bind (:map python-mode-map
	      ("C-c f" . yapfify-buffer)))

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
  :commands company-lsp)


(use-package python-pytest)

(use-package dap-mode
  :hook ((python-mode . dap-mode)
         (python-mode . dap-ui-mode)
         (python-mode . dap-tooltip-mode))
  :bind (:map dap-mode-map
              (("C-d n" . dap-next)
               ("C-d d" . dap-debug)
               ("C-d i" . dap-step-in)
               ("C-d o" . dap-step-out)
               ("C-d c" . dap-continue)
               ("C-d b" . dap-breakpoint-toggle)
               ("C-d e" . dap-eval)
               ("C-d s" . dap-switch-stack-frame)))

  :config
  (require 'dap-python)
  (dap-register-debug-template "backward test"
                               (list :type "python"
                                     :args "-i"
                                     :cwd nil
                                     :env '(("DACE_optimizer_interface" . ""))
                                     :target-module (expand-file-name "~/repos/dace/tests/autodiff/torch_test.py")
                                     :request "launch"
                                     :name "backward test"))
  )
  


(use-package 
  treemacs 
  :bind ("C-c d" . treemacs))

(use-package 
  lsp-ivy 
  :after python ;; this makes the keybinding below work
  :commands lsp-ivy-workspace-symbol 
  :config
  (setq lsp-ivy-show-symbol-kind t) 
  (setq lsp-ivy-filter-symbol-kind '(0 2 13)) 
  (setq lsp-python-ms-python-executable-cmd "/home/orausch/.local/opt/miniconda3/envs/onnx/bin/python")
  :bind (:map python-mode-map
	      ("C-c s" . lsp-ivy-workspace-symbol)))

(use-package 
  treemacs-evil)
(use-package 
  lsp-treemacs 
  :commands lsp-treemacs-errors-list)

(use-package 
  lsp-python-ms 
  :hook (python-mode . (lambda () 
			 (require 'lsp-python-ms) 
			 (lsp))))

(use-package 
  projectile 
  :config (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map) 
  (projectile-mode +1))


;; Opening Files
(global-set-key (kbd "<f2>") 
		(lambda() 
		  (interactive) 
		  (find-file "~/org/tasks.org")))


(defun org-journal-find-location () 
  (interactive)
  ;; Open today's journal, but specify a non-nil prefix argument in order to
  ;; inhibit inserting the heading; org-capture will insert the heading.
  (org-journal-new-entry t))

(global-set-key (kbd "<f3>") 'org-roam-today)

(add-hook 'org-capture-mode-hook #'org-align-all-tags)
(add-hook 'org-mode-hook 'turn-on-visual-line-mode)
(setq org-startup-folded nil)

(add-hook 'comint-mode-hook (lambda () 
			      (local-set-key [14] 
					     (quote comint-next-input)) 
			      (local-set-key [16] 
					     (quote comint-previous-input)) 
			      (local-set-key [18] 
					     (quote comint-history-isearch-backward))))


(defun insert-screenshot-at-point () 
  "Make a screenshot, save it in the img folder and insert a link to it." 
  (interactive) 
  (shell-command-to-string "scrot -s -o /tmp/screen.png") 
  (let ((screenshot-name (concat (read-string "Name: ") ".png")))
    (while (file-exists-p (concat "/home/orausch/org/img/" screenshot-name)) 
      (setq screenshot-name (concat (read-string "File exists. New Name: ") ".png"))) 
    (shell-command-to-string (concat "mv /tmp/screen.png " "/home/orausch/org/img/"
				     screenshot-name)) 
    (insert (concat "[[~/org/img/" screenshot-name "]]"))))

(defun render-everything ()
  "Render all latex previews and toggle inline images"
  (interactive)
  (org-latex-preview '(16))
  (org-redisplay-inline-images))

(add-hook 'org-mode-hook
          (lambda ()
            (define-key org-mode-map (kbd "C-c r") 'render-everything)))

(use-package latex-preview-pane
  :config
  (latex-preview-pane-enable))

(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c C-l") 'org-insert-link)
(global-set-key (kbd "C-c v") 'describe-variable)
(global-set-key (kbd "C-c q") 'describe-function)
(global-set-key (kbd "C-c z") 
		(lambda () 
		  (interactive) 
		  (find-file "~/.dotfiles/emacs/.emacs.d/init.el")))


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
 '(dap-python-terminal "xterm -e ")
 '(dired-sidebar-should-follow-file t)
 '(fci-rule-color "#37474F")
 '(fill-column 100)
 '(gdb-show-main t)
 '(indent-tabs-mode nil)
 '(jdee-db-active-breakpoint-face-colors (cons "#171F24" "#237AD3"))
 '(jdee-db-requested-breakpoint-face-colors (cons "#171F24" "#579C4C"))
 '(jdee-db-spec-breakpoint-face-colors (cons "#171F24" "#777778"))
 '(lua-indent-level 4)
 '(menu-bar-mode nil)
 '(objed-cursor-color "#D16969")
 '(org-adapt-indentation nil)
 '(org-babel-load-languages (quote ((emacs-lisp . t) (python . t))))
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
      "* %? [[%:link][%:description]] 
Captured On: %U"))))
 '(org-clock-out-when-done (quote ("DONE" "WAITING")))
 '(org-refile-targets (quote (("~/org/tasks.org" :maxlevel . 1))))
 '(org-roam-date-filename-format "journal/%Y-%m-%d")
 '(org-roam-date-title-format "Journal %Y-%m-%d")
 '(org-roam-directory "~/org/")
 '(org-roam-graph-exclude-matcher (quote ("journal" "_tag")))
 '(org-roam-graph-viewer "~/.local/opt/firefox/firefox")
 '(package-selected-packages
   (quote
    (python-pytest org-roam posframe dap-mode lsp-ivy elisp-format org htmlize yapfify yaml-mode use-package treemacs-evil ripgrep realgud rainbow-delimiters pyvenv protobuf-mode projectile org-journal magit-popup lua-mode lsp-ui lsp-treemacs lsp-python-ms highlight-indentation ghub flycheck find-file-in-project evil-surround evil-magit evil-leader evil-commentary evil-collection dired-subtree counsel conda company-quickhelp company-lsp company-irony clang-format+ bind-map benchmark-init all-the-icons-ivy all-the-icons-dired)))
 '(pdf-view-midnight-colors (cons "#d4d4d4" "#1e1e1e"))
 '(rustic-ansi-faces
   ["#1e1e1e" "#D16969" "#579C4C" "#D7BA7D" "#339CDB" "#C586C0" "#85DDFF" "#d4d4d4"])
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


(add-hook 'emacs-startup-hook (lambda () 
				(setq gc-cons-threshold 16777216 gc-cons-percentage 0.1
				      file-name-handler-alist last-file-name-handler-alist)))

(server-start)
