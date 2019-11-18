;;* appearance and GUI
(blink-cursor-mode 1)
(menu-bar-mode -1)   ;; hide menu bar
(scroll-bar-mode -1) ;; hide scroll bar
(set-cursor-color "red")
(setq column-number-mode t)
(setq ediff-split-window-function 'split-window-horizontally)
;; prevent windows from being split vertically
(setq split-height-threshold nil)
(show-paren-mode 1)
(tool-bar-mode -1)   ;; hide tool bar
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)

;; save customizations in a file other than init.el
(defconst custom-file (expand-file-name "custom.el" user-emacs-directory))
(unless (file-exists-p custom-file)
  (write-region "" nil custom-file))
(load custom-file)

(setq frame-title-format
      (list (format "%s %%S: %%j " (system-name))
            '(buffer-file-name "%f" (dired-directory dired-directory "%b"))))

;; File path in title bar.
(setq frame-title-format
      (list (format "%s %%S: %%j " (system-name))
            '(buffer-file-name "%f" (dired-directory dired-directory "%b"))))

(defun nh/set-default-font-verbosely (font-name)
  (interactive)
  (message (format "** setting default font to %s" font-name))
  (condition-case nil
      (set-default-font font-name)
    (error (message (format "** Error: could not set to font %s" font-name)))))

(defun nh/fix-frame (&optional frame)
  "Apply platform-specific settings."
  (interactive)
  (cond ((string= "ns" window-system) ;; cocoa
         (progn
           (message (format "** running %s windowing system" window-system))
           ;; key bindings for mac - see
           ;; http://stuff-things.net/2009/01/06/emacs-on-the-mac/
           ;; http://osx.iusethis.com/app/carbonemacspackage
           (set-keyboard-coding-system 'mac-roman)
           (setq mac-option-modifier 'meta)
           (setq mac-command-key-is-meta nil)
           (nh/set-default-font-verbosely "Menlo-14")))
        ((string= "x" window-system)
         (progn
           (message (format "** running %s windowing system" window-system))
           (set-default-font-verbosely "Liberation Mono-10")
           ;; M-w or C-w copies to system clipboard
           ;; see http://www.gnu.org/software/emacs/elisp/html_node/Window-System-Selections.html
           (setq x-select-enable-clipboard t)))
        (t
         (message "** running in terminal mode"))))
(nh/fix-frame)

;;* startup and shutdown
(setq inhibit-splash-screen t)
(setq initial-scratch-message nil)
(setq make-backup-files nil)
(setq require-final-newline t)
(setq delete-trailing-lines nil)
(add-hook 'before-save-hook 'delete-trailing-whitespace)
;; buffers opened from command line don't create new frame
(setq ns-pop-up-frames nil)

;; require prompt before exit on C-x C-c
(defun nh/ask-before-exit ()
  (interactive)
  (cond ((y-or-n-p "Quit? (save-buffers-kill-terminal) ")
	 (save-buffers-kill-terminal))))
(global-set-key (kbd "C-x C-c") 'nh/ask-before-exit)

;; desktop
(defun nh/desktop-save-no-p ()
  "Save desktop without prompting (replaces `desktop-save-in-desktop-dir')"
  (interactive)
  (desktop-save desktop-dirname))

(if (member "--no-desktop" command-line-args)
    (message "** desktop auto-save is disabled")
  (progn
    (require 'desktop)
    (desktop-save-mode 1)
    (message "** desktop auto-save is enabled")
    (add-hook 'auto-save-hook 'nh/desktop-save-no-p)))

;;* execution environment
;; TODO: OS-specific commands
(defun nh/ssh-refresh ()
  "Reset the environment variable SSH_AUTH_SOCK"
  (interactive)
  (let (ssh-auth-sock-old (getenv "SSH_AUTH_SOCK"))
    (setenv "SSH_AUTH_SOCK"
            (car (split-string
                  (shell-command-to-string
                   (if (eq system-type 'darwin)
                       "cat ~/.ssh-auth-sock"
                     ;; "ls -t $(find /tmp/* -user $USER -name Listeners 2> /dev/null)"
                     "ls -t $(find /tmp/ssh-* -user $USER -name 'agent.*' 2> /dev/null)"
                     )))))
    (message
     (format "SSH_AUTH_SOCK %s --> %s"
             ssh-auth-sock-old (getenv "SSH_AUTH_SOCK")))))

(add-to-list 'exec-path "~/.emacs.d/bin")
(defun nh/prepend-path (path)
  "Add `path' to the beginning of $PATH unless already present."
  (interactive)
  (unless (string-match path (getenv "PATH"))
    (setenv "PATH" (concat path ":" (getenv "PATH")))))

(nh/prepend-path "~/.emacs.d/bin")

;;* other settings
(setq suggest-key-bindings 4)

(setq mouse-wheel-scroll-amount '(3 ((shift) . 3))) ;; number of lines at a time
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mosue 't) ;; scroll window under mouse
(setq scroll-step 1) ;; keyboard scroll one line at a time
(setq scroll-conservatively 1) ;; scroll by one line to follow cursor off screen
(setq scroll-margin 2) ;; Start scrolling when 2 lines from top/bottom

(global-set-key (kbd "<f5>") 'call-last-kbd-macro)

;;* general utilities

(defun nh/back-window ()
  "switch windows with C- and arrow keys"
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

(defun nh/move-line-up ()
  (interactive)
  (transpose-lines 1)
  (previous-line 2))
(global-set-key (kbd "M-<up>") 'nh/move-line-up)

(defun nh/move-line-down ()
  (interactive)
  (next-line 1)
  (transpose-lines 1)
  (previous-line 1))
(global-set-key (kbd "M-<down>") 'nh/move-line-down)

(defun nh/transpose-buffers (arg)
  "Transpose the buffers shown in two windows."
  (interactive "p")
  (let ((selector (if (>= arg 0) 'next-window 'previous-window)))
    (while (/= arg 0)
      (let ((this-win (window-buffer))
            (next-win (window-buffer (funcall selector))))
        (set-window-buffer (selected-window) next-win)
        (set-window-buffer (funcall selector) this-win)
        (select-window (funcall selector)))
      ;; (setq arg (if (plusp arg) (1- arg) (1+ arg)))
      (setq arg (if (>= arg 0) (1- arg) (1+ arg)))
      )))
(global-set-key (kbd "C-x 4") 'nh/transpose-buffers)

(defun nh/switch-buffers-between-frames ()
  "switch-buffers-between-frames switches the buffers between the two last frames"
  (interactive)
  (let ((this-frame-buffer nil)
	(other-frame-buffer nil))
    (setq this-frame-buffer (car (frame-parameter nil 'buffer-list)))
    (other-frame 1)
    (setq other-frame-buffer (car (frame-parameter nil 'buffer-list)))
    (switch-to-buffer this-frame-buffer)
    (other-frame 1)
    (switch-to-buffer other-frame-buffer)))
(global-set-key (kbd "C-x 5") 'nh/switch-buffers-between-frames)

(defun nh/toggle-frame-split ()
  "If the frame is split vertically, split it horizontally or vice versa.
Assumes that the frame is only split into two."
  (interactive)
  (unless (= (length (window-list)) 2) (error "Can only toggle a frame split in two"))
  (let ((split-vertically-p (window-combined-p)))
    (delete-window) ; closes current window
    (if split-vertically-p
        (split-window-horizontally)
      (split-window-vertically)) ; gives us a split with the other window twice
    (switch-to-buffer nil))) ; restore the original window in this part of the frame
(global-set-key (kbd "C-x 6") 'nh/toggle-frame-split)

(defun nh/unfill-paragraph ()
  (interactive)
  (let ((fill-column (point-max)))
  (fill-paragraph nil)))
(global-set-key (kbd "M-C-q") 'nh/unfill-paragraph)

(defun nh/copy-region-or-line-other-window ()
  "Copy selected text or current line to other window"
  (interactive)
  (progn (save-excursion
           (if (region-active-p)
               (copy-region-as-kill
                (region-beginning) (region-end))
             (copy-region-as-kill
              (line-beginning-position) (+ (line-end-position) 1)))
           (other-window 1)
           (yank))
         (other-window -1)))

;;* spelling
(defvar nh/enable-flyspell-p "enable flyspell in various modes")

;; use aspell if installed
(if (cond
     ((executable-find "aspell")
      (setq ispell-dictionary "en")
      (setq ispell-program-name "aspell")))
    (progn
      (message "** using %s for flyspell" ispell-program-name)
      (autoload 'flyspell-mode "flyspell" "On-the-fly spelling checker." t)
      (setq flyspell-issue-welcome-flag nil)
      (setq nh/enable-flyspell-p t))
  (setq nh/enable-flyspell-p nil)
  (message "** could not find hunspell or aspell"))

;;* init file utilities
(defvar nh/init-el "~/.emacs.d/init-minimal.el" "My init file")
(defun nh/init-file-edit ()
  "Edit init file specified by `nh/init-el'"
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

;;* Package management
(require 'package)
(setq package-archives
      '(("ELPA" . "https://tromey.com/elpa/")
        ("gnu" . "https://elpa.gnu.org/packages/")
        ("melpa" . "https://melpa.org/packages/")
        ("melpa-stable" . "https://stable.melpa.org/packages/")
        ("marmalade" . "https://marmalade-repo.org/packages/")
	("org" . "https://orgmode.org/elpa/")
        ))

(setq package-archive-priorities
      '(("org" . 30)
        ("elpy" . 30)
        ("melpa-stable" . 20)
        ("marmalade" . 10)
        ("gnu" . 10)
        ("melpa" . 5)))

(setq package-menu-hide-low-priority t)
(setq package-check-signature nil) ;; TODO: fix this properly
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

;;* python

;; https://vxlabs.com/2018/11/19/configuring-emacs-lsp-mode-and-microsofts-visual-studio-code-python-language-server/
(use-package lsp-python-ms
  :ensure t
  :hook (python-mode . (lambda ()
			 (setq indent-tabs-mode nil)
			 (setq tab-width 4)
			 (setq py-indent-offset tab-width)
			 (setq py-smart-indentation t)
			 (push '("SConstruct" . python-mode) auto-mode-alist)
			 (push '("SConscript" . python-mode) auto-mode-alist)
			 (push '("*.cgi" . python-mode) auto-mode-alist)
                         (require 'lsp-python-ms)
                         (lsp)))
  :config
  (setq lsp-python-ms-python-executable-cmd "python3"))

;; TODO: add to hydra
;; lsp-describe-thing-at-point

;; Default 'untabify converts a tab to equivalent number of spaces
;; before deleting a single character.
(setq backward-delete-char-untabify-method "all")

;; autopep8
(defun nh/autopep8-region-or-buffer ()
  "Apply autopep8 to the current region or buffer"
  (interactive)
  (unless (region-active-p)
    (mark-whole-buffer))
  (shell-command-on-region
   (region-beginning) (region-end)      ;; beginning and end of region or buffer
   "autopep8 -"                         ;; command and parameters
   (current-buffer)                     ;; output buffer
   t                                    ;; replace?
   "*autopep8 errors*"                  ;; name of the error buffer
   t)                                   ;; show error buffer?
  (goto-char (region-end))              ;; ... and delete trailing newlines
  (re-search-backward "\n+" nil t)
  (replace-match "" nil t))

(defun nh/autopep8-and-ediff ()
  "Compare the current buffer to the output of autopep8 using ediff"
  (interactive)
  (let ((p8-output
         (get-buffer-create (format "* %s autopep8 *" (buffer-name)))))
    (shell-command-on-region
     (point-min) (point-max)    ;; beginning and end of buffer
     "autopep8 -"               ;; command and parameters
     p8-output                  ;; output buffer
     nil                        ;; replace?
     "*autopep8 errors*"        ;; name of the error buffer
     t)                         ;; show error buffer?
    (ediff-buffers (current-buffer) p8-output)))

;;* javascript/json
(add-hook 'js-mode-hook
          (lambda ()
            (make-local-variable 'js-indent-level)
            (setq js-indent-level 2)))

(add-hook 'json-mode-hook
          (lambda ()
            (make-local-variable 'js-indent-level)
            (setq js-indent-level 2)))

;;* search and navigation (ivy, counsel, and friends)

(use-package ivy
  :ensure t
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t)
  (setq ivy-count-format "%d/%d ")
  (global-set-key (kbd "C-c C-r") 'ivy-resume))

(use-package counsel
  :ensure t
  :config
  (global-set-key (kbd "M-x") 'counsel-M-x)
  (global-set-key (kbd "C-x C-f") 'counsel-find-file)
  (global-set-key (kbd "C-c g") 'counsel-git)
  (global-set-key (kbd "C-c j") 'counsel-git-grep)
  (global-set-key (kbd "C-x l") 'counsel-locate)
  (define-key minibuffer-local-map (kbd "C-r") 'counsel-minibuffer-history))

(use-package swiper
  :ensure t
  :config
  (global-set-key (kbd "C-s") 'swiper))

(use-package projectile
  :ensure t
  :init
  (setq projectile-completion-system 'ivy)
  :config
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (projectile-mode +1))

(use-package avy
  :ensure t
  :bind (("M-'" . avy-goto-word-1)))

;; see https://github.com/ericdanan/counsel-projectile
(use-package counsel-projectile
  :ensure t
  :config
  (counsel-projectile-mode))

(when (boundp 'grep-find-ignored-directories)
  (add-to-list 'grep-find-ignored-directories ".eggs")
  (add-to-list 'grep-find-ignored-directories "src"))

(defun nh/grep-ignore-venv-current-project (&rest args)
  (interactive)
  (let ((venv (find-venv-current-project)))
    (if venv
        (progn
          (setq venv (file-name-nondirectory
                      (replace-regexp-in-string "/$" "" venv)))
          (message "adding '%s' to grep-find-ignored-directories" venv)
          (add-to-list 'grep-find-ignored-directories venv))
      (message "no virtualenv at this location")
      )))

(advice-add 'rgrep :before #'nh/grep-ignore-venv-current-project)
(advice-add 'projectile-grep :before #'nh/grep-ignore-venv-current-project)
(advice-add 'counsel-projectile-grep :before #'nh/grep-ignore-venv-current-project)



;;* hydra

(use-package hydra
  :ensure t
  :config
  (defhydra hydra-launcher (:color teal :columns 4 :post (redraw-display))
    "hydra-launcher"
    ("C-g" redraw-display "<quit>")
    ("RET" redraw-display "<quit>")
    ("b" nh/copy-buffer-file-name "nh/copy-buffer-file-name")
    ("d" nh/insert-date "nh/insert-date")
    ("e" save-buffers-kill-emacs "save-buffers-kill-emacs")
    ("f" nh/fix-frame "fix-frame")
    ("g" hydra-toggle-mode/body "toggle mode")
    ("h" hydra-helm/body "helm commands")
    ("i" hydra-init-file/body "hydra for init file")
    ("n" nh/org-find-index "nh/org-find-index")
    ("N" nh/org-add-entry-to-index "nh/org-add-entry-to-index")
    ("m" magit-status "magit-status")
    ("o" hydra-org-navigation/body "hydra-org-navigation")
    ("O" nh/copy-region-or-line-other-window "copy-region-or-line-other-window")
    ("p" hydra-python/body "python menu")
    ("P" list-processes "list-processes")
    ("s" nh/ssh-refresh "ssh-refresh")
    ("t" org-todo-list "org-todo-list")
    ("T" nh/transpose-buffers "transpose-buffers")
    ("u" untabify "untabify")
    ("w" hydra-web-mode/body "web-mode commands"))
  (global-set-key (kbd "C-\\") 'hydra-launcher/body)

  (defhydra hydra-init-file (:color blue :columns 4 :post (redraw-display))
    "hydra-init-file"
    ("RET" redraw-display "<quit>")
    ("C-g" redraw-display "<quit>")
    ("i" nh/init-file-edit "edit init file")
    ("l" nh/init-file-load "reload init file")
    ("h" nh/init-file-header-occur "occur headers")
    ("H" nh/init-file-header-insert "insert header")
    ("u" nh/init-file-use-package-occur "occur use-package declarations"))

  (defhydra hydra-toggle-mode (:color blue :columns 4 :post (redraw-display))
    "hydra-toggle-mode"
    ("RET" redraw-display "<quit>")
    ;; ("c" csv-mode "csv-mode")
    ("h" html-mode "html-mode")
    ("j" jinja2-mode "jinja2-mode")
    ("k" markdown-mode "markdown-mode")
    ("l" lineum-mode "lineum-mode")
    ("m" moinmoin-mode "moinmoin-mode")
    ("o" org-mode "org-mode")
    ("p" python-mode "python-mode")
    ("r" R-mode "R-mode")
    ("s" sql-mode "sql-mode")
    ("t" text-mode "text-mode")
    ("v" visual-line-mode "visual-line-mode")
    ("w" web-mode "web-mode")
    ("y" yaml-mode "yaml-mode"))

  (defhydra hydra-org-navigation
    (:exit nil :foreign-keys warn :columns 4 :post (redraw-display))
    "hydra-org-navigation"
    ("RET" nil "<quit>")
    ("i" org-previous-item "org-previous-item")
    ("k" org-next-item "org-next-item")
    ("<right>" org-next-block "org-next-block")
    ("<left>" org-previous-block "org-previous-block")
    ("<down>" outline-next-visible-heading "outline-next-visible-heading")
    ("<up>" outline-previous-visible-heading "outline-previous-visible-heading")
    ("S-<down>" org-forward-paragraph "org-forward-paragraph")
    ("S-<up>" org-backward-paragraph "org-backward-paragraph")
    ("s" (org-insert-structure-template "src") "add src block" :color blue)
    ("w" nh/org-element-as-docx "nh/org-element-as-docx" :color blue)
    ("q" nil "<quit>"))

  ) ;; end hydra config

;;* ESS (R language support)

(defun nh/set-inferior-ess-r-program-name ()
  "Set `inferior-ess-r-program-name' as the absolute path to the R
interpreter. On systems using 'modules'
(http://modules.sourceforge.net/), load the R module before defining
the path."
     (interactive)
     (setq inferior-ess-r-program-name
	   (replace-regexp-in-string
	    "\n" ""
	    (shell-command-to-string
	     "which ml > /dev/null && (ml R; which R) || which R"))))

(defun nh/set-inferior-ess-r-program-name ()
    "Set `inferior-ess-r-program-name' as the absolute path to the R
interpreter. On systems using 'modules'
(http://modules.sourceforge.net/), load the R module before defining
the path."
    (interactive)
    (setq inferior-ess-r-program-name
	  (replace-regexp-in-string
	   "\n" ""
	   (shell-command-to-string
	    "which ml > /dev/null && (ml R; which R) || which R"))))

(use-package ess
  :ensure t
  :init (require 'ess-site)
  :config
  (add-hook 'ess-mode-hook
            '(lambda()
               (message "** Loading ess-mode hooks")
               ;; leave my underscore key alone!
               (setq ess-S-assign "_")
               ;; (ess-toggle-underscore nil)
               ;; set ESS indentation style
               ;; choose from GNU, BSD, K&R, CLB, and C++
               (ess-set-style 'GNU 'quiet)
	       (if nh/enable-flyspell-p (flyspell-mode))
	       (nh/set-inferior-ess-r-program-name)
	       )))

;;* org-mode
(use-package org
  :ensure t
  :config
  (push '("\\.org\\'" . org-mode) auto-mode-alist)
  (push '("\\.org\\.txt\\'" . org-mode) auto-mode-alist)
  (add-hook 'org-mode-hook
            '(lambda ()
               (message "Loading org-mode hooks")
               ;; (font-lock-mode)
               (setq org-confirm-babel-evaluate nil)
               (setq org-src-fontify-natively t)
               (setq org-edit-src-content-indentation 0)
               (define-key org-mode-map (kbd "M-<right>") 'forward-word)
               (define-key org-mode-map (kbd "M-<left>") 'backward-word)
               ;; provides key mapping for the above; replaces default
               ;; key bindings for org-promote/demote-subtree
               (define-key org-mode-map (kbd "M-S-<right>") 'org-do-demote)
               (define-key org-mode-map (kbd "M-S-<left>") 'org-do-promote)
               (define-key org-mode-map (kbd "C-c n")  'hydra-org-navigation/body)
               (visual-line-mode)
               ;; org-babel

               ;; enable a subset of languages for evaluation in code blocks
               (setq nh/org-babel-load-languages
                     '((R . t)
                       (latex . t)
                       (python . t)
                       (sql . t)
                       (sqlite . t)
                       (emacs-lisp . t)
                       (dot . t)))

               ;; use "shell" for org-mode versions 9 and above
               (add-to-list 'nh/org-babel-load-languages
                            (if (>= (string-to-number (substring (org-version) 0 1)) 9)
				'(shell . t) '(sh . t)))

               (org-babel-do-load-languages
		'org-babel-load-languages nh/org-babel-load-languages)

               (defadvice org-todo-list (after org-todo-list-bottom ())
		 "Move to bottom of page after entering org-todo-list"
		 (progn (end-of-buffer) (recenter-top-bottom)))
               (ad-activate 'org-todo-list)
               ))
  )

(use-package ox-minutes
  :ensure t
  :after (org))

(defun nh/org-add-entry (filename time-format)
  ;; Add an entry to an org-file with today's timestamp.
  (interactive "FFile: ")
  (find-file filename)
  (end-of-buffer)
  (delete-blank-lines)
  (insert (format-time-string time-format)))

(defvar nh/org-index "~/Dropbox/notes/index.org")
(defun nh/org-add-entry-to-index ()
  (interactive)
  (nh/org-add-entry nh/org-index "\n* <%Y-%m-%d %a> "))

(defun nh/org-find-index ()
  (interactive)
  (find-file nh/org-index))

(defun nh/safename (str)
  "Remove non-alphanum characters and downcase"
  (let ((exprs '(("^\\W+" "") ("\\W+$" "") ("\\W+" "-"))))
    (dolist (e exprs)
      (setq str (replace-regexp-in-string (nth 0 e) (nth 1 e) str)))
    (downcase str)))

(defun nh/org-element-as-docx ()
  "Export the contents of the element at point to a file and
convert to .docx with pandoc"
  (interactive)
  (let* ((sec (car (cdr (org-element-at-point))))
         (header (plist-get sec ':title))
         (fname (nh/safename header))
         (basedir
          (shell-quote-argument (read-directory-name
           "Output directory: " (expand-file-name "~/Downloads"))))
         (orgfile (make-temp-file fname nil ".org"))
         (docx (concat (file-name-as-directory basedir) fname ".docx")))
    (write-region
     (plist-get sec ':begin) (plist-get sec ':end) orgfile)
    (call-process-shell-command (format "pandoc %s -o %s" orgfile docx))
    (if (y-or-n-p "open file?")
        (shell-command (format "open %s" docx)))
    (message "wrote %s" docx)
    ))

;;* sh-mode

(add-to-list 'auto-mode-alist '("\\.zsh\\'" . sh-mode))
(add-to-list 'auto-mode-alist '("\\.bash\\'" . sh-mode))

;;* text-mode

(add-hook 'text-mode-hook
          '(lambda ()
             ;; (longlines-mode)
             (if nh/enable-flyspell-p (flyspell-mode))))
;;* rst-mode

(add-hook 'rst-mode-hook
          '(lambda ()
             (message "Loading rst-mode hooks")
             (if nh/enable-flyspell-p (flyspell-mode))
             (define-key rst-mode-map (kbd "C-c C-a") 'rst-adjust)))

;;* tramp

(condition-case nil
    (require 'tramp)
  (setq tramp-default-method "scp")
  (error (message "** could not load tramp")))

;;* misc packages

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

(use-package magit
  :ensure t)

(use-package smart-mode-line
  :ensure t
  :config
  (setq sml/no-confirm-load-theme t)
  (setq sml/theme 'light)
  (setq sml/name-width 30)
  (setq sml/time-format "%H:%M")
  (sml/setup))

(use-package tex-mode
  :ensure auctex)

;; (use-package auctex
;;   :ensure t)

(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md" . gfm-mode)
         ("\\.md" . markdown-mode)
         ("\\.markdown" . markdown-mode))
  :bind (:map markdown-mode-map
              ;; don't redefine =M-<left>= and =M-<right>= in this mode
              ("M-<right>" . nil)
              ("M-<left>" . nil))
  :init  (setq markdown-command "multimarkdown")
  :config (custom-set-faces
	   '(markdown-code-face
	     ((t (:inherit fixed-pitch :background "lavender"))))))

(use-package groovy-mode
  :ensure t
  :mode ("\\.nf" . groovy-mode))

(use-package discover
  :ensure t
  :config
  (global-discover-mode 1))

;; (use-package company-lsp
;;   :ensure t
;;   :config
;;   (push 'company-lsp company-backends))

;; (use-package which-key
;;   :ensure t
;;   :config (which-key-mode))
