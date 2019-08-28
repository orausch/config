;;;~ /.doom.d / config.el - *-lexical - binding : t; -*-



(setq python-shell-interpreter "python3" flycheck-python-pycompile-executable "python3")

(require 'company)
(setq company-idle-delay 0.2
      company-minimum-prefix-length 3)
(company-quickhelp-mode)
(setq company-quickhelp-max-lines 40)


(defun +org/move-item-or-subtree (direction)
  "Move the current item or subtree up or down, depending on direction."
  (interactive)
  (cl-letf ((item-fun (if (eq direction 'up) 'org-move-item-up 'org-move-item-down))
            (subtree-fun (if (eq direction 'up) 'org-move-subtree-up 'org-move-subtree-down)))
    (if (org-at-item-p)
        (funcall item-fun)
      (if (org-at-heading-p)
          (funcall subtree-fun)
        (error "Not an item or heading")))))

(add-hook 'org-insert-heading-hook 'evil-insert-state)
(after! evil-snipe
  (evil-snipe-mode -1))

(after! evil-matchit
  (global-evil-matchit-mode -1))

(map! :n "s" #'evil-avy-goto-word-1)

(map! :n "`" nil)
(map! :n "`" #'+ivy/switch-buffer)
(setq comint-prompt-read-only t)
(defun browse-richterswil ()
  "Connect to araneum richterswil ssh server."
  (interactive)
  (dired "/ssh:rauscho@185.178.192.37:~/"))

(map! :leader
      :desc "Find methods"  "l"  #'counsel-imenu)
(map! :leader
      :desc "Main.org" "O" (lambda! (find-file "~/polybox/org/main.org"))
      :desc "Browse Araneum remote" "r" 'browse-richterswil)


(setq projectile-mode-line "Projectile")
(defadvice projectile-on (around exlude-tramp activate)
  "This should disable projectile when visiting a remote file"
  (unless  (--any? (and it (file-remote-p it))
                   (list
                    (buffer-file-name)
                    list-buffers-directory
                    default-directory
                    dired-directory))
    ad-do-it))
(setq flycheck-python-flake8-executable "python3")
(setq flycheck-flake8rc "~/.flake8")

(setq org-babel-python-command "python3")

;;(add-hook 'python-mode-hook #'blacken-mode)

(setq clang-format-style-option "google")
(add-hook 'c-mode-common-hook
          (function (lambda ()
                    (add-hook 'before-save-hook
                              #'clang-format-buffer nil t))))

(add-hook 'c-mode-common-hook 'google-set-c-style)
(add-hook 'c-mode-common-hook 'google-make-newline-indent)
(setq doom-theme 'doom-molokai)
(setq python-shell-interpreter "~/.local/opt/miniconda3/bin/python3")
(setq blacken-executable "~/.local/bin/black")
(setq blacken-line-length 79)
(setq org-babel-python-command "~/.local/opt/miniconda3/bin/python3")
(setq projectile-git-command "git ls-files -zc")

(defun +org/insert-screenshot-at-point ()
  "Make a screenshot, save it in the img folder and insert a link to it."
  (interactive)
  (shell-command-to-string "scrot -s -o /tmp/screen.png")
  (let ((screenshot-name (concat (read-string "Name: ") ".png")))

    (while (file-exists-p (concat "/home/orausch/polybox/org/img/" screenshot-name))
      (setq screenshot-name (concat (read-string "File exists. New Name: ") ".png")))
    (shell-command-to-string
     (concat "mv /tmp/screen.png " "/home/orausch/polybox/org/img/" screenshot-name))
    (insert (concat "[[./img/" screenshot-name "]]"))
    ))


(defun +org/my-dwim ()
    (interactive)
    (if (use-region-p)
        (let ((new-text (buffer-substring-no-properties (region-beginning) (region-end)))
          (new-file-name (concat (buffer-substring-no-properties (region-beginning) (region-end)) ".org")))
          (call-interactively 'delete-region)
          (goto-char (region-beginning))
          (insert (concat "[[file:" new-file-name "][" new-text "]]"))
          (find-file new-file-name)))
    (call-interactively +org/dwim-at-point))

(map! :after org
      :map org-mode-map
      :gi [S-return] 'org-insert-heading
      :ni "M-k" (lambda! (+org/move-item-or-subtree 'up))
      :ni "M-j" (lambda! (+org/move-item-or-subtree 'down))
      :i "M-l" #'org-demote-subtree
      :i "M-h" #'org-promote-subtree
      :nv "RET" #'+org/my-dwim
      :nv [return] #'+org/my-dwim)


(setq org-export-babel-evaluate t)

(after! org
  (setq org-list-allow-alphabetical t
        org-hide-emphasis-markers t))

(setq +doom-dashboard-functions
  '(doom-dashboard-widget-banner
    doom-dashboard-widget-shortmenu
    doom-dashboard-widget-loaded
    doom-dashboard-widget-footer))


(setq TeX-synctex-tex-flags "")
