;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)
(require 'package)
;; (elpy-enable)
(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/"))

(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "C-c a") 'org-agenda)

(setq org-agenda-custom-commands 
      '(("o" "At the office" tags-todo "@office"
         ((org-agenda-overriding-header "Office")
          (org-agenda-skip-function #'my-org-agenda-skip-all-siblings-but-first)))))

(defun my-org-agenda-skip-all-siblings-but-three ()
  "Skip all but the three non-done entries."
  (let (should-skip-entry)
    (unless (org-current-is-todo)
      (setq should-skip-entry t))
    (save-excursion
      (while (and (not should-skip-entry) (org-goto-sibling t))
        (when (org-current-is-todo)
          (setq should-skip-entry t))))
    (when should-skip-entry
      (or (outline-next-heading)
          (goto-char (point-max))))))
		  
(defun org-current-is-todo ()
  (string= "TODO" (org-get-todo-state)))

(setq url-proxy-services
      '(("no_proxy" . "^\\(localhost\\|10.*\\)")
        ("http" . "www-proxy-ams.nl.oracle.com:80")
        ("https" . "www-proxy-ams.nl.oracle.com:80")))
(require 'evil-org)
(add-hook 'org-mode-hook 'evil-org-mode)
(evil-org-set-key-theme '(navigation insert textobjects additional calendar))
(require 'evil-org-agenda)


(evil-org-agenda-set-keys)

(add-hook 'emacs-lisp-mode-hook #'rainbow-delimiters-mode)
;;; ORG STUFF
;(require 'org)
;(defun +org/move-item-or-subtree (direction)
;  "Move the current item or subtree up or down, depending on direction."
;  (interactive)
;  (cl-letf ((item-fun (if (eq direction 'up) 'org-move-item-up 'org-move-item-down))
;            (subtree-fun (if (eq direction 'up) 'org-move-subtree-up 'org-move-subtree-down)))
;    (if (org-at-item-p)
;        (funcall item-fun)
;      (if (org-at-heading-p)
;          (funcall subtree-fun)
;        (error "Not an item or heading")))))
;(define-key org-mode-map (kbd "M-j") (lambda () (interactive) (+org/move-item-or-subtree 'down)))
;(define-key org-mode-map (kbd "M-k") (lambda () (interactive) (+org/move-item-or-subtree 'up)))
;(define-key org-mode-map (kbd "M-h") 'org-promote-subtree)
;(define-key org-mode-map (kbd "M-l") 'org-demote-subtree) 

;; ORG PROTOCOL
(server-start)
(require 'org-protocol)

(with-eval-after-load 'company
  (define-key company-active-map (kbd "M-n") nil)
  (define-key company-active-map (kbd "M-p") nil)
  (define-key company-active-map (kbd "C-n") #'company-select-next)
  (define-key company-active-map (kbd "C-p") #'company-select-previous))


  

;; Opening Files
(global-set-key (kbd "<f2>") (lambda() (interactive) (find-file "~/org/tasks.org")))
(global-set-key (kbd "<f3>") (lambda() (interactive) (find-file "~/org/inbox.org")))
(add-hook 'org-clock-goto-hook 'org-narrow-to-subtree)
(add-hook 'org-insert-heading-hook 'evil-insert-state)

(require 'magit)
(require 'evil-magit)
(global-set-key (kbd "<f11>") 'magit-status)
(global-set-key (kbd "<f12>") 'org-clock-goto)

(auto-fill-mode 1)
(setq fill-column 120)
(require 'evil)
(evil-mode 1)
(require 'ivy)
(ivy-mode 1)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (leuven)))
 '(elpy-modules
   (quote
    (elpy-module-company elpy-module-eldoc elpy-module-flymake elpy-module-pyvenv elpy-module-yasnippet elpy-module-django elpy-module-sane-defaults)))
 '(elpy-rpc-python-command "/home/orausch/.local/opt/miniconda3/bin/python3")
 '(fill-column 100)
 '(lua-indent-level 4)
 '(menu-bar-mode nil)
 '(org-adapt-indentation nil)
 '(org-agenda-files (quote ("~/org/tasks.org")))
 '(org-capture-templates
   (quote
    (("i" "Inbox" entry
      (file "~/org/inbox.org")
      "* TODO")
     ("l" "Link entry" entry
      (file "~/org/inbox.org")
      "* %a"))))
 '(org-clock-out-when-done (quote ("DONE" "WAITING")))
 '(org-refile-targets (quote (("~/org/tasks.org" :maxlevel . 1))))
 '(package-selected-packages
   (quote
    (lua-mode rainbow-delimiters markdown-mode elpy evil-magit magit evil-org all-the-icons-ivy counsel yasnippet-snippets org htmlize find-file-in-project evil)))
 '(python-shell-interpreter "/home/orausch/.local/opt/miniconda3/bin/python")
 '(tool-bar-mode nil))
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

(add-hook 'elpy-mode-hook
	  (lambda ()
	    (local-set-key [f4] (quote elpy-goto-definition))))

(add-hook 'comint-mode-hook
	  (lambda ()
	   (local-set-key [14] (quote comint-next-input))
	   (local-set-key [16] (quote comint-previous-input))
	   (local-set-key [18] (quote comint-history-isearch-backward))))

(setq org-todo-keywords '((sequence "TODO(t)" "WAITING(w)" "|" "DONE(d)" "CANCELLED(c)")))
