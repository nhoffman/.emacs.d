
(defvar my-alias-prefix "my/")

(defun make-alias (fun &optional prefix)
  "Create an alias for function `fun' by prepending the value of
  `my-alias-prefix' to the symbol name. Use `prefix' to provide
  an alternative prefix string. Example:

  (defun bar () (message \"I am bar\"))
  (make-alias 'bar \"foo-\")
  (foo-bar) => \"I am bar\""

  (interactive)
  (defalias
    (intern (concat (or prefix my-alias-prefix) (symbol-name fun)))
    fun))

(defvar my/init-org "~/.emacs.d/init.org" "org-mode version of init file")
(defvar my/init-el "~/.emacs.d/init.el" "tangled version of `my/init-org'")

(defun init-edit ()
  "Edit org-mode version of init file specified by `my/init-org'"
  (interactive)
  (find-file my/init-org))
(make-alias 'init-edit)

(defun init-load ()
  (interactive)
  (load my/init-el))
(make-alias 'init-load)

(defun init-tangle-and-load ()
  "Tangle `my/init-org' and load the result"
  (interactive)
  (init-edit)
  (org-babel-tangle)
  (init-load)
  (switch-to-buffer "*Messages*"))
(make-alias 'init-tangle-and-load)

(unless (>= emacs-major-version 24)
  (error "Emacs version 24 or higher is required"))

(message "loading ~/.emacs.d/init.el")

(global-auto-revert-mode 1)

;; (setq debug-on-error t)
;; (setq debug-on-signal t)

(setq enable-dir-local-variables nil)

(when (>= emacs-major-version 24)
  (require 'package)
  (setq package-archives
        '(("ELPA" . "http://tromey.com/elpa/")
          ("gnu" . "http://elpa.gnu.org/packages/")
          ("melpa" . "http://melpa.org/packages/")
          ("melpa-stable" . "http://stable.melpa.org/packages/")
          ("marmalade" . "http://marmalade-repo.org/packages/")
          ("org" . "http://orgmode.org/elpa/")
          ("elpy" . "http://jorgenschaefer.github.io/packages/")
          ))

  ;; Check if we're on Emacs 24.4 or newer, if so, use the pinned package feature
  (when (boundp 'package-pinned-packages)
    (setq package-pinned-packages
          '((elpy . "elpy")
            (flycheck . "melpa-stable")
            (helm-descbinds . "melpa-stable")
            (helm-swoop . "melpa-stable")
            (highlight-indentation . "elpy") ;; fixes error in elpy 1.6
            (hydra . "gnu")
            (magit . "melpa-stable")
            (markdown-mode . "melpa-stable")
            (org . "org")
            (smart-mode-line . "melpa-stable")
            (swiper . "melpa-stable")
            (web-mode . "melpa")
            (which-key . "melpa-stable")
            )))

  (package-initialize))

(setq package-archive-priorities
      '(("org" . 30)
        ("elpy" . 30)
        ("melpa-stable" . 20)
        ("marmalade" . 10)
        ("gnu" . 10)
        ("melpa" . 5)))
(setq package-menu-hide-low-priority t)

(unless (package-installed-p 'use-package)
  (if (yes-or-no-p "use-package is not installed yet - install it? ")
      (progn
        ;; bootstrap use-package
        (message "** installing use-package")
        (package-refresh-contents)
        (package-install 'use-package))
    (message "** defining fake use-package macro")
    (defmacro use-package (pkg &rest args)
      (warn
       "use-package is not installed - could not activate %s" (symbol-name pkg))
      )))

(defun package-installed-not-builtin-p (package &optional min-version)
  "Return true if PACKAGE, of MIN-VERSION or newer, is installed
(ignoring built-in versions).  MIN-VERSION should be a version list"

  (unless package--initialized (error "package.el is not yet initialized!"))
(if (< emacs-major-version 4)
    ;; < emacs 24.4
    (let ((pkg-desc (assq package package-alist)))
      (if pkg-desc
          (version-list-<= min-version
                           (package-desc-vers (cdr pkg-desc)))))
  ;; >= emacs 24.4
  (let ((pkg-descs (cdr (assq package package-alist))))
    (and pkg-descs
         (version-list-<= min-version
                          (package-desc-version (car pkg-descs)))))
  ))

(defun package-install-list (pkg-list)
  ;; Install each package in pkg-list if necessary.
  (mapcar
   (lambda (pkg)
     (unless (package-installed-not-builtin-p pkg)
       (package-install pkg)))
   pkg-list)
  (message "done installing packages"))

(defvar my-package-list
  '(auctex
    csv-mode
    discover
    dash-at-point
    edit-server
    elpy
    ess
    expand-region
    flycheck
    gist
    git-timemachine
    helm
    helm-descbinds
    helm-swoop
    helm-projectile
    htmlize
    hydra
    jinja2-mode
    magit
    markdown-mode
    moinmoin-mode
    org
    ox-minutes
    polymode
    projectile
    rainbow-delimiters
    smart-mode-line
    swiper
    visual-regexp
    visual-regexp-steroids
    web-mode
    which-key
    yaml-mode
    yasnippet
    yas-jit))

(defun install-packages ()
  ;; Install packages listed in global 'my-package-list'
  (interactive)
  (package-list-packages)
  (package-install-list my-package-list))
(make-alias 'install-packages)

(if (require 'hydra nil 'noerror)
    (progn
      (defhydra hydra-toggle-mode (:color blue :columns 4 :post (redraw-display))
        "hydra-toggle-mode"
        ("RET" redraw-display "<quit>")
        ("c" csv-mode "csv-mode")
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
        ("y" yaml-mode "yaml-mode")
        ))
  (message "** hydra is not installed"))

(defmacro csetq (variable value)
  `(funcall (or (get ',variable 'custom-set)
                'set-default)
            ',variable ,value))

(global-set-key (kbd "C-x C-b") 'electric-buffer-list)

(defun back-window ()
  (interactive)
  (other-window -1))
(global-set-key (kbd "C-<right>") 'other-window)
(global-set-key (kbd "C-<left>") 'back-window)

(condition-case nil
    (progn
      (require 'helm-config)
      (helm-mode 1)
      (global-set-key (kbd "M-x") 'helm-M-x)
      (global-set-key (kbd "C-x C-f") 'helm-find-files)
      (global-set-key (kbd "M-y") 'helm-show-kill-ring)
      (global-set-key (kbd "C-c h o") 'helm-occur)
      (global-set-key (kbd "C-h SPC") 'helm-all-mark-rings)
      (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)
      (define-key helm-map (kbd "C-i") 'helm-execute-persistent-action)
      (define-key helm-map (kbd "C-z")  'helm-select-action)
      )
  (error (message "** could not activate helm")))

(condition-case nil
    (progn
      (require 'helm-descbinds)
      (global-set-key (kbd "C-h b") 'helm-descbinds))
  (error (message "** could not activate helm-descbinds")))

(if (require 'hydra nil 'noerror)
    (progn
      (defhydra hydra-helm (:color blue :columns 4 :post (redraw-display))
        "hydra-toggle-mode"
        ("RET" redraw-display "<quit>")
        ("b" helm-browse-project "helm-browse-project")
        ("d" helm-descbinds "helm-descbinds")
        ("f" helm-projectile-find-file "helm-projectile-find-file")
        ("g" helm-projectile-grep "helm-projectile-grep")
        ("j" helm-projectile-switch-project "helm-projectile-switch-project")
        ("o" helm-occur "helm-occur")
        ("O" helm-org-in-buffer-headings "helm-org-in-buffer-headings")
        ("p" helm-projectile "helm-projectile")
        ("s" helm-swoop "helm-swoop")
        ))
  (message "** hydra is not installed"))

(if (and (package-installed-p 'projectile) (package-installed-p 'helm-projectile))
    (progn
      (projectile-global-mode)
      (setq projectile-completion-system 'helm)
      (helm-projectile-on))
  (message "** not using projectile or helm-projectile - one or both not installed"))

(when (boundp 'grep-find-ignored-directories)
  (add-to-list 'grep-find-ignored-directories ".eggs")
  (add-to-list 'grep-find-ignored-directories "src"))

(defun grep-ignore-venv-current-project (&rest args)
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

(advice-add 'rgrep :before #'grep-ignore-venv-current-project)
(advice-add 'projectile-grep :before #'grep-ignore-venv-current-project)
(advice-add 'helm-projectile-grep :before #'grep-ignore-venv-current-project)

(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward)

(use-package swiper
             :ensure t
             :bind (("C-s" . swiper)))

(use-package avy
             :ensure t
             :bind (("M-'" . avy-goto-word-1)))

(defun my/mark-inside-quotes ()
  "Mark string between two quote charactes (double, single, or grave accent)"
  (interactive)
  (unless (re-search-forward "[\"'`]" nil t)
    (error "No quote character after the cursor"))
  (backward-char 1)
  (set-mark (point))
  (unless (re-search-backward "[\"'`]" nil t)
    (error "No matching quote character before the cursor"))
  (forward-char 1)
  (exchange-point-and-mark))

(defun my/mark-first-quoted-string ()
  (interactive)
  (re-search-forward "[\"'`]" nil t)
  (my/mark-inside-quotes))

(if (require 'hydra nil 'noerror)
    (defhydra hydra-expand-region (global-map "M-=")
      "hydra-expand-region"
      ("=" er/expand-region "er/expand-region")
      ("-" er/contract-region "er/contract-region")
      ("q" my/mark-inside-quotes "my/mark-inside-quotes" :color blue)
      ("Q" my/mark-first-quoted-string "my/mark-first-quoted-string" :color blue))
  (message "** hydra is not installed"))

(if (require 'hydra nil 'noerror)
    (progn
      (defhydra hydra-launcher (:color teal :columns 4 :post (redraw-display))
        "hydra-launcher"
        ("C-g" redraw-display "<quit>")
        ("RET" redraw-display "<quit>")
        ("b" copy-buffer-file-name "copy-buffer-file-name")
        ("d" insert-date "insert-date")
        ("D" dash-at-point "dash-at-point")
        ("e" save-buffers-kill-emacs "save-buffers-kill-emacs")
        ("f" fix-frame "fix-frame")
        ("g" hydra-toggle-mode/body "toggle mode")
        ("h" hydra-helm/body "helm commands")
        ("i" init-edit "init-edit")
        ("n" my/find-org-index "my/find-org-index")
        ("N" my/org-index-add-entry "my/org-index-add-entry")
        ("m" magit-status "magit-status")
        ("o" copy-region-or-line-other-window "copy-region-or-line-other-window")
        ("p" hydra-python/body "python menu")
        ("P" list-processes "list-processes")
        ("s" ssh-refresh "ssh-refresh")
        ("t" org-todo-list "org-todo-list")
        ("T" transpose-buffers "transpose-buffers")
        ("u" untabify "untabify")
        ("w" hydra-web-mode/body "web-mode commands"))
      (global-set-key (kbd "C-\\") 'hydra-launcher/body))
  (message "** hydra is not installed"))

(use-package which-key
  :ensure t
  :pin melpa-stable  ;; this has no effect but I'll leave it here
                     ;; until the apparent bug in use-package is fixed
                     ;; (in the meantime, the repo is pinned using
                     ;; package-pinned-packages)
  :config (which-key-mode))

(global-set-key (kbd "<f6>") 'linum-mode)
(global-set-key (kbd "<f7>") 'visual-line-mode)
(global-set-key (kbd "<f8>") 'flymake-popup-current-error-menu)

(defalias 'dtw 'delete-trailing-whitespace)

(setq column-number-mode t)
(setq inhibit-splash-screen t)
(setq require-final-newline t)
(setq make-backup-files nil)
(setq initial-scratch-message nil)
(setq suggest-key-bindings 4)
(show-paren-mode 1)

(if (require 'smart-mode-line nil 'noerror)
    (progn
      (setq sml/no-confirm-load-theme t)
      (setq sml/theme 'light)
      (setq sml/name-width 30)
      ;; (setq sml/mode-width 'full)
      (setq sml/time-format "%H:%M")
      (sml/setup))
  (message "** smart-mode-line is not installed"))

(setq frame-title-format
      (list (format "%s %%S: %%j " (system-name))
            '(buffer-file-name "%f" (dired-directory dired-directory "%b"))))

(set-cursor-color "red")
(blink-cursor-mode 1)

(add-to-list 'load-path "~/.emacs.d/elisp/")

(defun ssh-refresh ()
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
(make-alias 'ssh-refresh)

(add-to-list 'exec-path "~/.emacs.d/bin")

(defun prepend-path (path)
  "Add `path' to the beginning of $PATH unless already present."
  (interactive)
  (unless (string-match path (getenv "PATH"))
    (setenv "PATH" (concat path ":" (getenv "PATH")))))

(prepend-path "~/.emacs.d/bin")

;; (when (memq window-system '(mac ns))
;;   (exec-path-from-shell-initialize))

(global-set-key [(control x) (control c)]
                (function
                 (lambda () (interactive)
                   (cond ((y-or-n-p "Quit? (save-buffers-kill-terminal) ")
                          (save-buffers-kill-terminal))))))

(setq delete-trailing-lines nil)
(add-hook 'before-save-hook 'delete-trailing-whitespace)

(defun set-default-font-verbosely (font-name)
  (interactive)
  (message (format "** setting default font to %s" font-name))
  (condition-case nil
      (set-default-font font-name)
    (error (message (format "** Error: could not set to font %s" font-name)))))

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
(global-set-key (kbd "<f2>") 'fix-frame)
(make-alias 'fix-frame)
(fix-frame)

(setq mouse-wheel-scroll-amount '(3 ((shift) . 3))) ;; number of lines at a time
(setq mouse-wheel-progressive-speed nil)            ;; don't accelerate scrolling
(setq mouse-wheel-follow-mosue 't)                  ;; scroll window under mouse
(setq scroll-step 1)                                ;; keyboard scroll one line at a time
(setq scroll-conservatively 1)                      ;; scroll by one line to follow cursor off screen
(setq scroll-margin 2)                              ;; Start scrolling when 2 lines from top/bottom

(global-set-key (kbd "<f5>") 'call-last-kbd-macro)

(csetq ediff-split-window-function 'split-window-horizontally)

(defun desktop-save-no-p ()
  "Save desktop without prompting (replaces `desktop-save-in-desktop-dir')"
  (interactive)
  ;; (message (format "Saving desktop in %s" desktop-dirname))
  (desktop-save desktop-dirname))

(if (member "--no-desktop" command-line-args)
    (message "** desktop auto-save is disabled")
  (progn
    (require 'desktop)
    (desktop-save-mode 1)
    (message "** desktop auto-save is enabled")
    (add-hook 'auto-save-hook 'desktop-save-no-p)))

;; (defun buffer-list-nostar ()
;;     (delq nil (mapcar
;;                (lambda (buf)
;;                  (unless (string-match "^[* ]" (buffer-name buf)) buf))
;;                (buffer-list))))

;; (add-hook 'before-make-frame-hook
;;           (lambda ()
;;             (message "** running 'before-make-frame-hook")
;;             ;; (let ((buf (buffer-file-name (car (buffer-list-nostar)))))
;;             ;;   (print (buffer-list-nostar))
;;             ;;   (when buf
;;             ;;     (setq initial-buffer-choice buf)
;;             ;;     (message "** setting initial buffer to %s" buf)))

;;             (print (buffer-list))
;;             (setq initial-buffer-choice (buffer-file-name (car (delq nil (mapcar
;;                (lambda (buf)
;;                  (unless (string-match "^[* ]" (buffer-name buf)) buf))
;;                (buffer-list))))))
;;             ))

(defun move-line-up ()
  (interactive)
  (transpose-lines 1)
  (previous-line 2))
(global-set-key (kbd "M-<up>") 'move-line-up)

(defun move-line-down ()
  (interactive)
  (next-line 1)
  (transpose-lines 1)
  (previous-line 1))
(global-set-key (kbd "M-<down>") 'move-line-down)

(defun transpose-buffers (arg)
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
(global-set-key (kbd "C-x 4") 'transpose-buffers)

(defun switch-buffers-between-frames ()
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
(global-set-key (kbd "C-x 5") 'switch-buffers-between-frames)

(defun toggle-frame-split ()
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

(global-set-key (kbd "C-x 6") 'toggle-frame-split)

(setq split-height-threshold nil)

(defvar enable-flyspell-p)

;; (setq debug-on-error t)
;; (setq debug-on-signal t)

(if (cond
     ;; ((executable-find "hunspell")
     ;;  (setq ispell-local-dictionary "en_US")
     ;;  (setq ispell-local-dictionary-alist
     ;;        '(("en_US"                                  ;; DICTIONARY-name
     ;;           "[[:alpha:]]"                            ;; CASECHARS
     ;;           "[^[:alpha:]]"                           ;; NOT-CASECHARS
     ;;           "[']"                                    ;; OTHERCHARS
     ;;           nil                                      ;; MANY-OTHERCHARS-P
     ;;           ("-d" "~/.emacs.d/dictionaries/en_US")   ;; ISPELL-ARGS
     ;;           ;; ("-d" "en_US")   ;; ISPELL-ARGS
     ;;           nil                                      ;; EXTENDED-CHARACTER-MODE
     ;;           utf-8                                    ;; CHARACTER-SET
     ;;           )))
     ;;  (setenv "DICPATH" "~/.emacs.d/dictionaries")
     ;;  (setq-default ispell-program-name "hunspell")
     ;;  (setq ispell-really-hunspell t))
     ((executable-find "aspell")
      (setq ispell-dictionary "en")
      (setq ispell-program-name "aspell")))
    (progn
      (message "** using %s for flyspell" ispell-program-name)
      (autoload 'flyspell-mode "flyspell" "On-the-fly spelling checker." t)
      (setq flyspell-issue-welcome-flag nil)
      (setq enable-flyspell-p t))
  (setq enable-flyspell-p nil)
  (message "** could not find hunspell or aspell"))

(add-hook 'find-file-hooks
          '(lambda ()
             (if (equal "pico." (substring (buffer-name (current-buffer)) 0 5))
                 ;; (message "** running hook for pine/alpine")
                 (mail-mode))))

(use-package yasnippet
  :init
  (progn
    (add-hook 'after-save-hook
              (lambda ()
                (when (eql major-mode 'snippet-mode)
                  (yas-reload-all)))))
  :commands (yas-global-mode)
  :mode ("\\.yasnippet" . snippet-mode))

(condition-case nil
    (require 'ess-site)
  (error (message "** could not load ESS")))

(add-hook 'ess-mode-hook
          '(lambda()
             (message "Loading ess-mode hooks")
             ;; leave my underscore key alone!
             (setq ess-S-assign "_")
             ;; (ess-toggle-underscore nil)
             ;; set ESS indentation style
             ;; choose from GNU, BSD, K&R, CLB, and C++
             (ess-set-style 'GNU 'quiet)
             (if enable-flyspell-p (flyspell-mode))
             )
          )

(if (require 'markdown-mode nil 'noerror)
    (use-package markdown-mode
      :commands (markdown-mode gfm-mode)
      :mode (("README\\.md" . gfm-mode)
             ("\\.md" . markdown-mode)
             ("\\.markdown" . markdown-mode))
      :bind (:map markdown-mode-map
                  ;; don't redefine =M-<left>= and =M-<right>= in this mode
                  ("M-<right>" . nil)
                  ("M-<left>" . nil))
      :init  (setq markdown-command "multimarkdown"))
  (message "** markdown-mode is not installed"))

(condition-case nil
    (progn
      (require 'poly-R)
      (require 'poly-markdown)
      (add-to-list 'auto-mode-alist '("\\.Rmd" . poly-markdown+r-mode))
      ;; (add-to-list 'auto-mode-alist '("\\.md" . poly-markdown-mode))
      (define-key polymode-mode-map (kbd "M-n r") 'my/ess-render-rmarkdown))
  (error (message "** could not activate polymode")))

(defun my/ess-render-rmarkdown ()
  "Compile R markdown (.Rmd). Should work for any output type."
  (interactive)
  ;; Check if attached R-session
  (condition-case nil
      (ess-get-process)
    (error
     (ess-switch-process)))
  (let* ((rmd-buf (current-buffer)))
    (save-excursion
      (let* ((sprocess (ess-get-process ess-current-process-name))
             (sbuffer (process-buffer sprocess))
             (buf-coding (symbol-name buffer-file-coding-system))
             (buffer-file-name-html
              (concat (file-name-sans-extension buffer-file-name) ".html"))
             (R-cmd
              (format
               "library(rmarkdown); rmarkdown::render(\"%s\")" buffer-file-name)))
        (save-buffer)
        (message "Running rmarkdown on %s" buffer-file-name)
        (ess-execute R-cmd 'buffer nil nil)
        (switch-to-buffer rmd-buf)
        (ess-show-buffer (buffer-name sbuffer) nil)))))

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
             (setq my/org-babel-load-languages
                   '((R . t)
                     (latex . t)
                     (python . t)
                     (sql . t)
                     (sqlite . t)
                     (emacs-lisp . t)
                     (dot . t)))

             ;; use "shell" for org-mode versions 9 and above
             (add-to-list 'my/org-babel-load-languages
                          (if (>= (string-to-number (substring (org-version) 0 1)) 9)
                              '(shell . t) '(sh . t)))

             (org-babel-do-load-languages
              'org-babel-load-languages my/org-babel-load-languages)

             (require 'ox-minutes nil t)

             ;; (defun org-with-silent-modifications(&rest args)
             ;;   "Replaces function causing error on org-export"
             ;;   (message "Using fake 'org-with-silent-modifications'"))
             (defadvice org-todo-list (after org-todo-list-bottom ())
               "Move to bottom of page after entering org-todo-list"
               (progn (end-of-buffer) (recenter-top-bottom)))
             (ad-activate 'org-todo-list)
             ))

(setq org-agenda-files (list "~/Dropbox/notes/index.org"))
(push '("\\.org\\'" . org-mode) auto-mode-alist)
(push '("\\.org\\.txt\\'" . org-mode) auto-mode-alist)

(if (require 'hydra nil 'noerror)
    (progn
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
        ("q" nil "<quit>")))
  (message "** hydra is not installed"))
;; org-mode-map binds "C-c n" in org-mode-map

(defun insert-date ()
  ;; Insert today's timestamp in format "<%Y-%m-%d %a>"
  (interactive)
  (insert (format-time-string "<%Y-%m-%d %a>")))
(make-alias 'insert-date)

(defun org-add-entry (filename time-format)
  ;; Add an entry to an org-file with today's timestamp.
  (interactive "FFile: ")
  (find-file filename)
  (end-of-buffer)
  (delete-blank-lines)
  (insert (format-time-string time-format)))

(defvar my/org-index "~/Dropbox/notes/index.org")

(defun my/org-index-add-entry ()
  (interactive)
  (org-add-entry my/org-index "\n* <%Y-%m-%d %a> "))

(defun my/find-org-index ()
  (interactive)
  (find-file my/org-index))

(global-set-key
 (kbd "C-x C-j") (lambda () (interactive)
                   (org-add-entry "~/Dropbox/journal/journal.org"
                                  "\n* %A, %B %d, %Y")))

(condition-case nil
    (edit-server-start)
  (error (message "** could not start edit-server (chrome edit with emacs)")))

(add-hook 'python-mode-hook
          '(lambda ()
             (setq indent-tabs-mode nil)
             (setq tab-width 4)
             (setq py-indent-offset tab-width)
             (setq py-smart-indentation t)
             (define-key python-mode-map "\C-m" 'newline-and-indent)))

(push '("SConstruct" . python-mode) auto-mode-alist)
(push '("SConscript" . python-mode) auto-mode-alist)
(push '("*.cgi" . python-mode) auto-mode-alist)

(setq backward-delete-char-untabify-method "all")

(if (require 'flycheck nil 'noerror)
    (use-package flycheck
      :init
      (setq flycheck-flake8rc "~/.emacs.d/flake8.conf"))
  (message "** flycheck is not installed"))

(if (require 'elpy nil 'noerror)
    (use-package elpy
      :bind (:map elpy-mode-map
                  ("C-<right>" . nil)
                  ("C-<left>" . nil)
                  ("M-<right>" . nil)
                  ("M-<left>" . nil)
                  ("M-<right>" . nil)
                  ("M-C-]" . elpy-nav-move-iblock-right)
                  ("M-C-[" . elpy-nav-move-iblock-left))
      :init
      (when (require 'flycheck nil t)
        (add-hook 'elpy-mode-hook 'flycheck-mode))
      :config
      (setq elpy-modules (delq 'elpy-module-django elpy-modules))
      (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
      (setq elpy-rpc-backend "jedi")
      (add-to-list 'elpy-project-ignored-directories "src")
      (add-to-list 'elpy-project-ignored-directories "*-env"))
  (message "** elpy is not installed"))

(condition-case nil
    (progn
      (elpy-enable)
      (setq elpy-modules (delq 'elpy-module-django elpy-modules))
      ;; disable flymake and replace with flycheck
      (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
      (when (require 'flycheck nil t)
        (add-hook 'elpy-mode-hook 'flycheck-mode)
        (setq flycheck-flake8rc "~/.emacs.d/flake8.conf")))
  (error (message "** could not enable elpy")))

(defvar venv-default-py2 "~/.emacs.d/python2-env")
(defvar venv-default-py3 "~/.emacs.d/python3-env")
(defvar venv-default venv-default-py2)

(defun activate-venv-and-reload (venv)
  (interactive)
  (pyvenv-activate venv)
  (elpy-rpc-restart)
  (python-mode))
(make-alias 'activate-venv-and-reload)

(defun activate-venv-default-py2 ()
  (interactive)
  (activate-venv-and-reload venv-default-py2))
(make-alias 'activate-venv-default-py2)

(defun activate-venv-default-py3 ()
  (interactive)
  (setq elpy-rpc-python-command "python3")
  (activate-venv-and-reload venv-default-py3))
(make-alias 'activate-venv-default-py3)

(defun list-venvs (basedir)
  "Return a list of paths to virtualenvs in 'basedir' or nil if
 none can be found"
  (interactive)
  (let ((fstr "find %s -path '*bin/activate' -maxdepth 4")
        (pth (replace-regexp-in-string "/$" "" basedir)))

    (mapcar (lambda (string)
              (replace-regexp-in-string "/bin/activate$" "" string))
            (cl-remove-if
             (lambda (string) (= (length string) 0))
             (split-string (shell-command-to-string (format fstr pth)) "\n")))
    ))

(defun list-venvs-current-project ()
  (if (elpy-project-root)
      (list-venvs elpy-project-root)
    (error "error: there is no project here")))

(defun helm-choose-venv-current-project ()
  (interactive)
  (helm
   :sources (helm-build-sync-source "choose a virtualenv"
              :candidates 'list-venvs-current-project)
   :buffer "*helm choose virtualenvs*"))

(defun activate-venv-current-project ()
  "Activate a virtualenv if one can be found in the current
project; otherwise activate the virtualenv defined in
`venv-default'. Also restarts the elpy rpc process."
  (interactive)
  (let ((venv (helm-choose-venv-current-project)))
    (if venv
        (if (y-or-n-p (format "Activate %s?" venv))
            (progn
              (activate-venv-and-reload venv)
              (message "Using %s" pyvenv-virtual-env)))
      (message "could not find a virtualenv here"))))
(make-alias 'activate-venv-current-project)

(defun elpy-install-requirements ()
  "Install python requirements to the current virtualenv."
  (interactive)
  (unless pyvenv-virtual-env
    (error "Error: no virtualenv is active"))
  (let ((dest "*elpy-install-requirements-output*")
        (install-cmd (format "%s/bin/pip install -U --force '%%s'" pyvenv-virtual-env))
        (deps '("elpy" "jedi" "pyflakes" "pep8" "flake8" "importmagic" "yapf")))
    (generate-new-buffer dest)
    (mapcar
     #'(lambda (pkg)
         (message (format install-cmd pkg))
         (call-process-shell-command (format install-cmd pkg) nil dest)) deps)
    (call-process-shell-command
     (format "%s/bin/pip freeze" pyvenv-virtual-env) nil dest)
    (switch-to-buffer dest))
  (elpy-rpc-restart))
(make-alias 'elpy-install-requirements)

(defun p8 ()
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

(defun p8-and-ediff ()
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

(if (require 'hydra nil 'noerror)
    (progn
      (defhydra hydra-python (:color blue :columns 4 :post (redraw-display))
        "hydra-python"
        ("RET" redraw-display "<quit>")
        ("g" elpy-goto-definition-other-window "elpy-goto-definition-other-window")
        ("e" elpy-config "elpy-config")
        ("f" flycheck-verify-setup "flycheck-verify-setup")
        ("i" elpy-install-requirements "elpy-install-requirements")
        ("v" activate-venv-current-project "activate-venv-current-project")
        ("2" activate-venv-default-py2 "activate-venv-default-py2")
        ("3" activate-venv-default-py3 "activate-venv-default-py3")
        ("y" elpy-yapf-fix-code "elpy-yapf-fix-code")))
  (message "** hydra is not installed"))

(add-to-list 'auto-mode-alist '("\\.zsh\\'" . sh-mode))
(add-to-list 'auto-mode-alist '("\\.bash\\'" . sh-mode))

(defun scons-insert-command ()
  (interactive)
  (insert "output, = env.Command(
    target=,
    source=,
    action=('')
)"))
(make-alias 'scons-insert-command)

(add-hook 'text-mode-hook
          '(lambda ()
             ;; (longlines-mode)
             (if enable-flyspell-p (flyspell-mode))))

(add-hook 'rst-mode-hook
          '(lambda ()
             (message "Loading rst-mode hooks")
             (if enable-flyspell-p (flyspell-mode))
             (define-key rst-mode-map (kbd "C-c C-a") 'rst-adjust)))

(condition-case nil
    (require 'moinmoin-mode)
  (error (message "** could not load moinmoin-mode")))

(if (require 'web-mode nil 'noerror)
    (use-package web-mode
      :mode (("\\.html" . web-mode))
      :bind ("C-c w" . hydra-web-mode/body)
      :init
      (setq web-mode-enable-current-element-highlight t)
      (setq web-mode-engines-alist
            '(("django" . "\\.html")))
      (setq indent-tabs-mode nil)
      (defhydra hydra-web-mode (:color blue :columns 4 :post (redraw-display))
        "hydra-web-mode"
        ("RET" redraw-display "<quit>")
        ("b" web-mode-element-beginning "element-beginning")
        ("e" web-mode-element-beginning "element-end")
        ("/" web-mode-element-close "element-close")))
  (message "** web-mode is not installed"))

(condition-case nil
    (require 'tramp)
  (setq tramp-default-method "scp")
  (error (message "** could not load tramp")))

(require 'vc-git)

(global-set-key (kbd "C-c m") 'magit-status)

(setq sql-sqlite-program "sqlite3")

(setq sql-connection-alist
      '((some-server
         (sql-product 'mysql)
         (sql-server "1.2.3.4")
         (sql-user "me")
         (sql-password "mypassword")
         (sql-database "thedb")
         (sql-port 3307))))

(defun sql-connect-preset (name)
  "Connect to a predefined SQL connection listed in `sql-connection-alist'"
  (eval `(let ,(cdr (assoc name sql-connection-alist))
    (flet ((sql-get-login (&rest what)))
      (sql-product-interactive sql-product)))))

(defun sql-mastermu ()
  (interactive)
  (sql-connect-preset 'mastermu))

;; buffer naming
(defun sql-make-smart-buffer-name ()
  "Return a string that can be used to rename a SQLi buffer.
This is used to set `sql-alternate-buffer-name' within
`sql-interactive-mode'."
  (or (and (boundp 'sql-name) sql-name)
      (concat (if (not(string= "" sql-server))
                  (concat
                   (or (and (string-match "[0-9.]+" sql-server) sql-server)
                       (car (split-string sql-server "\\.")))
                   "/"))
              sql-database)))

(add-hook 'sql-interactive-mode-hook
          (lambda ()
            (setq sql-alternate-buffer-name (sql-make-smart-buffer-name))
            (sql-rename-buffer)))

(require 'epa-file)
(setenv "GPG_AGENT_INFO" nil) ;; suppress graphical passphrase prompt

;; Outline-minor-mode key map
(define-prefix-command 'cm-map nil "Outline-")
;; HIDE
(define-key cm-map "q" 'hide-sublevels)    ; Hide everything but the top-level headings
(define-key cm-map "t" 'hide-body)         ; Hide everything but headings (all body lines)
(define-key cm-map "o" 'hide-other)        ; Hide other branches
(define-key cm-map "c" 'hide-entry)        ; Hide this entry's body
(define-key cm-map "l" 'hide-leaves)       ; Hide body lines in this entry and sub-entries
(define-key cm-map "d" 'hide-subtree)      ; Hide everything in this entry and sub-entries
;; SHOW
(define-key cm-map "a" 'show-all)          ; Show (expand) everything
(define-key cm-map "e" 'show-entry)        ; Show this heading's body
(define-key cm-map "i" 'show-children)     ; Show this heading's immediate child sub-headings
(define-key cm-map "k" 'show-branches)     ; Show all sub-headings under this heading
(define-key cm-map "s" 'show-subtree)      ; Show (expand) everything in this heading & below
;; MOVE
(define-key cm-map "u" 'outline-up-heading)                ; Up
(define-key cm-map "n" 'outline-next-visible-heading)      ; Next
(define-key cm-map "p" 'outline-previous-visible-heading)  ; Previous
(define-key cm-map "f" 'outline-forward-same-level)        ; Forward - same level
(define-key cm-map "b" 'outline-backward-same-level)       ; Backward - same level
;; commands are prefixed with C-c o
(global-set-key (kbd "C-c o") cm-map)

(defun occur-dwim ()
  "Call `occur' with the current region (if active) or word."
  (interactive)
  (push (if (region-active-p)
            (buffer-substring-no-properties
             (region-beginning)
             (region-end))
          (let ((sym (thing-at-point 'symbol)))
            (when (stringp sym)
              (regexp-quote sym))))
        regexp-history)
  (call-interactively 'occur))

(if (package-installed-p 'visual-regexp-steroids)
    (require 'visual-regexp-steroids))

(condition-case nil
    (require 'helm-swoop)
  (error (message "** could not activate helm-swoop")))

(if (require 'hydra nil 'noerror)
    (progn
      (defhydra hydra-search (:color blue :columns 4)
        "hydra-search"
        ("RET" helm-swoop "helm-swoop")
        ("b" helm-swoop-back-to-last-point "helm-swoop-back-to-last-point")
        ("f" file-file-in-project "find-file-in-project")
        ("m" helm-multi-swoop "helm-multi-swoop")
        ("M" helm-multi-swoop-all "helm-multi-swoop-all")
        ("o" occur "occur-dwim")
        ("O" occur-dwim "occur")
        ("r" vr/isearch-backward "vr/isearch-backward")
        ("s" vr/isearch-forward "vr/isearch-forward"))
      (global-set-key (kbd "C-c s") 'hydra-search/body))
  (message "** hydra is not installed"))

(if (require 'hydra nil 'noerror)
    (progn (defhydra hydra-replace (:color blue)
             "hydra-replace"
             ("RET" replace-string "replace-string")
             ("r" vr/replace "vr/replace")
             ("q" query-replace "query-replace")
             ("Q" vr/query-replace "vr/query-replace"))

           (global-set-key (kbd "C-c r") 'hydra-replace/body))
  (message "** hydra is not installed"))

(defun copy-buffer-file-name ()
  "Add `buffer-file-name' to `kill-ring' and echo the value to
the minibuffer"
  (interactive)
  (if buffer-file-name
      (progn
      (kill-new buffer-file-name t)
      (message buffer-file-name))
    (message "no file associated with this buffer")))
(make-alias 'copy-buffer-file-name)

(defun copy-and-comment ()
  "Comment active region and paste uncommented text on the
following line."
  (interactive)
  (kill-new
   (buffer-substring
    (region-beginning)
    (region-end)))
  (comment-region (region-beginning)
                  (region-end))
  (goto-char (region-end))
  (delete-blank-lines)
  (newline 2)
  (yank))

(global-set-key (kbd "M-C-;") 'copy-and-comment)

(defun unfill-paragraph ()
  (interactive)
  (let ((fill-column (point-max)))
  (fill-paragraph nil)))
(global-set-key (kbd "M-C-q") 'unfill-paragraph)
(make-alias 'unfill-paragraph)

(defun copy-region-or-line-other-window ()
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

(make-alias 'copy-region-or-line-other-window)

(condition-case nil
    (require 'elisp-format)
  (error (message "** could not load elisp-format")))

(setq ns-pop-up-frames nil)

(condition-case nil
    (progn
      (require 'discover)
      (global-discover-mode 1))
      (error (message "** could not activate discover")))

(use-package dired-x
  :config
  (progn
    (setq dired-omit-verbose nil)
    (add-hook 'dired-mode-hook #'dired-omit-mode)
    (setq dired-omit-files "^\\.?#")))

(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)

(custom-set-variables
  '(safe-local-variable-values (quote ((toggle-read-only . t)))))

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
