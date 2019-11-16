;;* global settings
(menu-bar-mode -1)   ;; hide menu bar
(tool-bar-mode -1)   ;; hide tool bar
(scroll-bar-mode -1) ;; hide scroll bar
(show-paren-mode 1)

;;* general utilities 

(defun nh/back-window ()
  "switch windows with arrow keys"
  (interactive)
  (other-window -1))
(global-set-key (kbd "C-<right>") 'other-window)
(global-set-key (kbd "C-<left>") 'nh/back-window)

(defun nh/insert-date ()
  "insert today's timestamp in format '<%Y-%m-%d %a>'"
  (interactive)
  (insert (format-time-string "<%Y-%m-%d %a>")))

(defun nh/copy-buffer-file-name ()
  "Add `buffer-file-name' to `kill-ring' and echo the value to
the minibuffer"
  (interactive)
  (if buffer-file-name
      (progn
      (kill-new buffer-file-name t)
      (message buffer-file-name))
    (message "no file associated with this buffer")))

;;* init file utilities
(defvar nh/init-el "~/.emacs.d/init-minimal.el" "My init file")
(defun nh/init-file-edit ()
  "Edit org-mode version of init file specified by `my/init-org'"
  (interactive)
  (find-file nh/init-el))

(defun nh/init-file-header-occur ()
  (interactive)
  (find-file nh/init-el)
  (occur "^;;\\* "))

(defun nh/init-file-use-package-occur ()
  (interactive)
  (find-file nh/init-el)
  (occur "^(use-package"))

(defun nh/init-file-header-insert ()
  "insert ';;*' header"
  (interactive)
  (insert ";;*"))

(defun nh/init-file-load ()
  "Reload init file"
  (interactive)
  (load nh/init-el))

;;* GUI setup
(defun fix-frame (&optional frame)
  "Apply platform-specific settings."
  (interactive)
  (menu-bar-mode -1)    ;; hide menu bar
  (tool-bar-mode -1)    ;; hide tool bar
  (scroll-bar-mode -1)  ;; hide scroll bar
  (cond ((string= "ns" window-system) ;; cocoa
         (progn
           (message (format "** running %s windowing system" window-system))
           ;; key bindings for mac - see
           ;; http://stuff-things.net/2009/01/06/emacs-on-the-mac/
           ;; http://osx.iusethis.com/app/carbonemacspackage
           (set-keyboard-coding-system 'mac-roman)
           (setq mac-option-modifier 'meta)
           (setq mac-command-key-is-meta nil)
           (set-default-font-verbosely "Menlo-14")))
        ((string= "x" window-system)
         (progn
           (message (format "** running %s windowing system" window-system))
           (set-default-font-verbosely "Liberation Mono-10")
           ;; M-w or C-w copies to system clipboard
           ;; see http://www.gnu.org/software/emacs/elisp/html_node/Window-System-Selections.html
           (setq x-select-enable-clipboard t)))
        (t
         (message "** running in terminal mode"))))

;;* Package management
(require 'package)
(setq package-archives
      '(("ELPA" . "https://tromey.com/elpa/")
        ("gnu" . "https://elpa.gnu.org/packages/")
        ("melpa" . "https://melpa.org/packages/")
        ("melpa-stable" . "https://stable.melpa.org/packages/")
        ("marmalade" . "https://marmalade-repo.org/packages/")
        ))

;; TODO: fix this properly
(setq package-check-signature nil)
(package-initialize)

;; bootstrap use-package
(unless (package-installed-p 'use-package)
  (if (yes-or-no-p "use-package is not installed yet - install it? ")
      (progn
        (message "** installing use-package")
        (package-refresh-contents)
        (package-install 'use-package))
    (message "** defining fake use-package macro")
    (defmacro use-package (pkg &rest args)
      (warn
       "use-package is not installed - could not activate %s" (symbol-name pkg))
      )))

(use-package yasnippet
  :ensure t
  :init
  (progn
    (add-hook 'after-save-hook
              (lambda ()
                (when (eql major-mode 'snippet-mode)
                  (yas-reload-all)))))
  :commands (yas-global-mode)
  :mode ("\\.yasnippet" . snippet-mode))

(use-package lsp-python-ms
  :ensure t
  :hook (python-mode . (lambda ()
                         (require 'lsp-python-ms)
                         (lsp))))  ; or lsp-deferred

(use-package magit
  :ensure t)

(use-package ivy
  :ensure t
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t)
  (setq ivy-count-format "%d/%d ")
  (global-set-key (kbd "C-c C-r") 'ivy-resume))

(use-package swiper
  :ensure t
  :config
  (global-set-key (kbd "C-s") 'swiper))

(use-package counsel
  :ensure t
  :config
  (global-set-key (kbd "M-x") 'counsel-M-x)
  (global-set-key (kbd "C-x C-f") 'counsel-find-file)
  (global-set-key (kbd "C-c g") 'counsel-git)
  (global-set-key (kbd "C-c j") 'counsel-git-grep)
  (global-set-key (kbd "C-c a") 'counsel-ag)
  (global-set-key (kbd "C-x l") 'counsel-locate)
  (define-key minibuffer-local-map (kbd "C-r") 'counsel-minibuffer-history))

(use-package company-lsp
  :ensure t
  :config
  (push 'company-lsp company-backends))

(use-package hydra
  :ensure t
  :config
  (defhydra hydra-launcher (:color teal :columns 4 :post (redraw-display))
    "hydra-launcher"
    ("C-g" redraw-display "<quit>")
    ("RET" redraw-display "<quit>")
    ("b" nh/copy-buffer-file-name "nh/copy-buffer-file-name")
    ("d" nh/insert-date "nh/insert-date")
    ("D" dash-at-point "dash-at-point")
    ("e" save-buffers-kill-emacs "save-buffers-kill-emacs")
    ("f" fix-frame "fix-frame")
    ("g" hydra-toggle-mode/body "toggle mode")
    ("h" hydra-helm/body "helm commands")
    ("i" hydra-init-file/body "hydra for init file")    
    ("n" my/find-org-index "my/find-org-index")
    ("N" my/org-index-add-entry "my/org-index-add-entry")
    ("m" magit-status "magit-status")
    ("o" hydra-org-navigation/body "hydra-org-navigation")
    ("O" copy-region-or-line-other-window "copy-region-or-line-other-window")
    ("p" hydra-python/body "python menu")
    ("P" list-processes "list-processes")
    ("s" ssh-refresh "ssh-refresh")
    ("t" org-todo-list "org-todo-list")
    ("T" transpose-buffers "transpose-buffers")
    ("u" untabify "untabify")
    ("w" hydra-web-mode/body "web-mode commands"))
  (global-set-key (kbd "C-\\") 'hydra-launcher/body)

  (defhydra hydra-init-file (:color blue :columns 4 :post (redraw-display))
        "hydra-init-file"
        ("RET" redraw-display "<quit>")
        ("C-g" redraw-display "<quit>")
	("i" nh/init-file-edit "edit init file")
	("h" nh/init-file-header-occur "occur headers")
	("u" nh/init-file-use-package-occur "occur use-package declarations")
	("H" nh/init-file-header-insert "insert header")
	)
    
  ) ;; end hydra config


;;--;; org-mode
