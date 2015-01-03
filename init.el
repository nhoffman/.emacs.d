
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

(defun init-edit ()
  "Edit ~/.emacs.d/init.org"
  (interactive)
  (find-file "~/.emacs.d/init.org"))
(make-alias 'init-edit)

(defun init-tangle-and-load ()
  "Tangle ~/.emacs.d/init.org and load the result"
  (interactive)
  (init-edit)
  (org-babel-tangle)
  (load "~/.emacs.d/init.el")
  (switch-to-buffer "*Messages*"))
(make-alias 'init-tangle-and-load)

(unless (= emacs-major-version 24)
  (error "Emacs version 24 is required"))

(message "loading ~/.emacs.d/init.el")

(global-auto-revert-mode 1)

;; (setq debug-on-error t)
;; (setq debug-on-signal t)

(when (>= emacs-major-version 24)
  (require 'package)
  (package-initialize)
  ;; Original Emacs Lisp Package Archive
  (add-to-list 'package-archives
       '("elpa" . "http://tromey.com/elpa/") t)
  ;; User-contributed repository
  ;; Marmalade is for packages that cannot be uploaded to the official ELPA repository.
  (add-to-list 'package-archives
       '("marmalade" . "http://marmalade-repo.org/packages/") t)
  (add-to-list 'package-archives
       '("melpa" . "http://melpa.org/packages/") t)
  (add-to-list 'package-archives
       '("org" . "http://orgmode.org/elpa/") t)
  (add-to-list 'package-archives
       '("elpy" . "http://jorgenschaefer.github.io/packages/") t)
  )

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
  '(ace-jump-mode
    ace-jump-buffer
    auctex
    edit-server
    elpy
    ess
    expand-region
    gist
    git-timemachine
    helm
    htmlize
    jinja2-mode
    magit
    markdown-mode
    moinmoin-mode
    org
    ;; projectile
    rainbow-delimiters
    ;; smex
    visual-regexp
    visual-regexp-steroids
    yaml-mode
    yas-jit))

(defun install-packages ()
  ;; Install packages listed in global 'my-package-list'
  (interactive)
  (package-list-packages)
  (package-install-list my-package-list))
(make-alias 'install-packages)

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
      (global-set-key (kbd "M-y") 'helm-show-kill-ring)
      (global-set-key (kbd "C-c h o") 'helm-occur)
      (global-set-key (kbd "C-h SPC") 'helm-all-mark-rings)
      (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)
      (define-key helm-map (kbd "C-i") 'helm-execute-persistent-action)
      (define-key helm-map (kbd "C-z")  'helm-select-action)
      )
  (error (message "** could not activate helm")))

;; (if (package-installed-p 'smex)
;;     (progn
;;       (global-set-key (kbd "M-x") 'smex)
;;       (global-set-key (kbd "C-c M-x") 'smex-major-mode-commands)
;;       ;; This is your old M-x.
;;       (global-set-key (kbd "C-x M-x") 'execute-extended-command)))

;; (if (package-installed-p 'projectile)
;;     (projectile-global-mode))

;; (setq ido-enable-flex-matching t)
;; (setq ido-everywhere t)
;; (setq ido-use-virtual-buffers t)
;; (ido-mode 1)

;; (recentf-mode 1)
;; (defun ido-choose-from-recentf ()
;;   "Use ido to select a recently visited file from the `recentf-list'"
;;   (interactive)
;;   (find-file (ido-completing-read "Open file: " recentf-list nil t)))
;; (global-set-key (kbd "C-c f") 'ido-choose-from-recentf)

(require 'ibuffer)
(global-set-key (kbd "C-x C-g") 'ibuffer)
(global-set-key (kbd "C-x M-g") 'ibuffer-switch-to-saved-filter-groups)
(setq ibuffer-show-empty-filter-groups nil)

(defvar my-ibuffer-config-file "~/.emacs.d/ibuffer-config.el")

(defun ibuffer-load-config ()
  ;; load the ibuffer config file
  (interactive)
  (condition-case nil
      (progn
        (message (format "** loading ibuffer config in %s" my-ibuffer-config-file))
        (load my-ibuffer-config-file)
        )
    (error (message (format "** could not load %s" my-ibuffer-config-file))))
  )

(ibuffer-load-config)

(defun ibuffer-show-all-filter-groups ()
  "Show all filter groups"
  (interactive)
  (setq ibuffer-hidden-filter-groups nil)
  (ibuffer-update nil t))

(defun ibuffer-hide-all-filter-groups ()
  "Hide all filter groups"
  (interactive)
  (setq ibuffer-hidden-filter-groups
        (delete-dups
         (append ibuffer-hidden-filter-groups
                 (mapcar 'car (ibuffer-generate-filter-groups
                               (ibuffer-current-state-list)
                               (not ibuffer-show-empty-filter-groups)
                               t)))))
  (ibuffer-update nil t))

(defun ibuffer-reload ()
  ;; kill ibuffer, reload the config file, and return to ibuffer
  (interactive)
  (ibuffer)
  (kill-buffer)
  (ibuffer-load-config)
  (ibuffer)
  )

(defun my-ibuffer-sort-hook ()
  ;; add another sorting method for ibuffer (allow the grouping of
  ;; filenames and dired buffers
  (define-ibuffer-sorter filename-or-dired
    "Sort the buffers by their pathname."
    (:description "filenames plus dired")
    (string-lessp
     (with-current-buffer (car a)
       (or buffer-file-name
           (if (eq major-mode 'dired-mode)
               (expand-file-name dired-directory))
           ;; so that all non pathnames are at the end
           "~"))
     (with-current-buffer (car b)
       (or buffer-file-name
           (if (eq major-mode 'dired-mode)
               (expand-file-name dired-directory))
           ;; so that all non pathnames are at the end
           "~"))))
  (define-key ibuffer-mode-map (kbd "s p")     'ibuffer-do-sort-by-filename-or-dired)
  )

(defun ibuffer-ediff-marked-buffers ()
  "Compare two marked buffers using ediff"
  (interactive)
  (let* ((marked-buffers (ibuffer-get-marked-buffers))
         (len (length marked-buffers)))
    (unless (= 2 len)
      (error (format "%s buffer%s been marked (needs to be 2)"
                     len (if (= len 1) " has" "s have"))))
    (ediff-buffers (car marked-buffers) (cadr marked-buffers))))

(add-hook 'ibuffer-mode-hook
          '(lambda ()
             (ibuffer-auto-mode 1) ;; minor mode that keeps the buffer list up to date
             (ibuffer-switch-to-saved-filter-groups "default")
             (define-key ibuffer-mode-map (kbd "a") 'ibuffer-show-all-filter-groups)
             (define-key ibuffer-mode-map (kbd "z") 'ibuffer-hide-all-filter-groups)
             (define-key ibuffer-mode-map (kbd "e") 'ibuffer-ediff-marked-buffers)
             (my-ibuffer-sort-hook)
             ;; don't accidentally print; see http://irreal.org/blog/?p=2013
             (defadvice ibuffer-do-print (before print-buffer-query activate)
               (unless (y-or-n-p "Print buffer? ")
                 (error "Cancelled")))
             )
          )

(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward)

(define-key global-map (kbd "M-'") 'ace-jump-mode)

(global-set-key (kbd "C-=") 'er/expand-region)
(global-set-key (kbd "C-M-=") 'er/contract-region)

(defvar my-key-map-prefix "C-c l")

(defun my/describe-my-key-map ()
  "List bindings associated with `my-key-map'"
  (interactive)
  (describe-bindings (kbd my-key-map-prefix)))

(define-prefix-command 'my-key-map)
(define-key global-map (kbd my-key-map-prefix) 'my-key-map)

(define-key my-key-map "d" #'insert-date)
(define-key my-key-map "D" #'describe-minor-mode)
(define-key my-key-map "e" #'save-buffers-kill-emacs)
(define-key my-key-map "f" #'fix-frame)
(define-key my-key-map "i" #'init-edit)
(define-key my-key-map "l" #'my/describe-my-key-map)
(define-key my-key-map "n" #'my/find-org-index)
(define-key my-key-map "N" #'my/org-index-add-entry)
(define-key my-key-map "m" #'magit-status)
(define-key my-key-map "o" #'copy-region-or-line-other-window)
(define-key my-key-map "p" #'package-list-packages)
(define-key my-key-map "s" #'ssh-refresh)
(define-key my-key-map "t" #'org-todo-list)

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

(setq display-time-day-and-date t
      display-time-24hr-format t)
(display-time)

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
 "ls -t $(find /tmp/* -user $USER -name Listeners 2> /dev/null)"
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
           (set-default-font-verbosely "Bitstream Vera Sans Mono-14")))
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

(if (package-installed-p 'visual-regexp-steroids)
    (progn (require 'visual-regexp-steroids)
           (define-key global-map (kbd "C-c r") 'vr/replace)
           (define-key global-map (kbd "C-c q") 'vr/query-replace)
           (define-key esc-map (kbd "C-r") 'vr/isearch-backward) ;; C-M-r
           (define-key esc-map (kbd "C-s") 'vr/isearch-forward)) ;; C-M-s
  (global-set-key (kbd "C-c r") 'replace-string)
  (global-set-key (kbd "C-c q") 'query-replace))

(global-set-key (kbd "<f5>") 'call-last-kbd-macro)

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

(define-key global-map (kbd "M-\"") 'ace-jump-buffer)

(setq-default ispell-program-name "aspell")
(setq ispell-dictionary "en")

(autoload 'flyspell-mode "flyspell" "On-the-fly spelling checker." t)
(setq flyspell-issue-welcome-flag nil) ;; fix error message

(add-hook 'find-file-hooks
          '(lambda ()
             (if (equal "pico." (substring (buffer-name (current-buffer)) 0 5))
                 ;; (message "** running hook for pine/alpine")
                 (mail-mode))))

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
             (flyspell-mode)
             )
          )

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
             (visual-line-mode)
             ;; org-babel
             (org-babel-do-load-languages
              'org-babel-load-languages
              '((R . t)
                (latex . t)
                (python . t)
                (sh . t)
                (sql . t)
                (sqlite . t)
                ))
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
  ;;(insert "\n* ")
  (insert (format-time-string time-format))
  (beginning-of-line)
  (forward-char 2))

(defvar my/org-index "~/Dropbox/notes/index.org")

(defun my/org-index-add-entry ()
  (interactive)
  (org-add-entry my/org-index "\n* <%Y-%m-%d %a>"))

(defun my/find-org-index ()
  (interactive)
  (find-file my/org-index))

(global-set-key
 (kbd "C-x C-j") (lambda () (interactive)
                   (org-add-entry "~/Dropbox/journal/journal.org"
                                  "\n* %A, %B %d, %Y")))

(push '("\\.md" . markdown-mode) auto-mode-alist)

(condition-case nil
    (edit-server-start)
  (error (message "** could not start edit-server (chrome edit with emacs)")))

(add-hook 'python-mode-hook
          '(lambda ()
             (message "Loading python-mode hooks")
             (setq indent-tabs-mode nil)
             (setq tab-width 4)
             (setq py-indent-offset tab-width)
             (setq py-smart-indentation t)
             (define-key python-mode-map "\C-m" 'newline-and-indent)
             (setq python-check-command "~/.emacs.d/bin/pychecker")
             ))

(push '("SConstruct" . python-mode) auto-mode-alist)
(push '("SConscript" . python-mode) auto-mode-alist)
(push '("*.cgi" . python-mode) auto-mode-alist)

(setq backward-delete-char-untabify-method "all")

(condition-case nil
    (elpy-enable) ;; install from ELPA
  (error (message "** could not enable elpy")))

(defvar venv-default "~/.emacs.d/emacs-env")
(defun activate-venv-default ()
  (interactive)
  (pyvenv-activate venv-default)
  (elpy-rpc-restart))
(make-alias 'activate-venv-default)

(prepend-path "~/.emacs.d/emacs-env/bin")

(defun activate-venv ()
  "Activate a virtualenv if one can be found in the current
project; otherwise activate the virtualenv defined in
`venv-default'. Also restarts the elpy rpc process."
  (interactive)
  (let ((venv nil)
        (find-pattern "find %s -path '*bin/activate' -maxdepth 4")
        (msg ""))

    (if (elpy-project-root)
        (setq venv
              (replace-regexp-in-string
               "/bin/activate[ \t\n]*" ""
               (shell-command-to-string
                (format find-pattern (elpy-project-root))))))

    (if (< (length venv) 1)
        (progn
          (setq venv venv-default)
          (setq msg "(cound not find a virtualenv here) ")))

    (if (y-or-n-p (format "%sActivate %s?" msg venv))
        (progn
          (pyvenv-activate venv)
          (elpy-rpc-restart)
          (message "Using %s" pyvenv-virtual-env)))))
(make-alias 'activate-venv)

(defun elpy-install-requirements ()
  "Install `elpy' and `jedi' to the current virtualenv. The
version of the `elpy' python package is forced to match the
version of the elisp package, upgrading or downgrading as
necessary."
  (interactive)
  (unless pyvenv-virtual-env
    (error "Error: no virtualenv is active"))
  (let ((dest "*elpy-install-requirements-output*")
        (install-cmd (format "%s/bin/pip install --force '%%s'" pyvenv-virtual-env))
        ;; (deps `(,(format "elpy==%s" elpy-version) "jedi")))
        (deps '("jedi" "pyflakes" "pep8" "flake8")))
    (generate-new-buffer dest)
    (mapcar
     #'(lambda (pkg)
         (message (format install-cmd pkg))
         (call-process-shell-command (format install-cmd pkg) nil dest)) deps)
    (call-process-shell-command
     (format "%s/bin/pip freeze" pyvenv-virtual-env) nil dest)
    (switch-to-buffer dest)
    ))
(make-alias 'elpy-install-requirements)

(add-hook 'elpy-mode-hook
'(lambda ()
   (define-key elpy-mode-map (kbd "C-<right>") nil)
   (define-key elpy-mode-map (kbd "C-<left>") nil)
   (define-key elpy-mode-map (kbd "M-<right>") nil)
   (define-key elpy-mode-map (kbd "M-<left>") nil)
   (define-key elpy-mode-map (kbd "M-<right>") nil)
   (define-key elpy-mode-map (kbd "M-C-]") 'elpy-nav-move-iblock-right)
   (define-key elpy-mode-map (kbd "M-C-[") 'elpy-nav-move-iblock-left)
   (setq elpy-rpc-backend "jedi")
   (add-to-list 'elpy-project-ignored-directories "src")
   (add-to-list 'elpy-project-ignored-directories "*-env")
))

(defun p8 ()
  "Apply autopep8 to the current region or buffer"
  (interactive)
  (unless (region-active-p)
    (mark-whole-buffer))
  (shell-command-on-region
   (region-beginning) (region-end) ;; beginning and end of region or buffer
   "autopep8 -"                    ;; command and parameters
   (current-buffer)                ;; output buffer
   t                               ;; replace?
   "*autopep8 errors*"             ;; name of the error buffer
   t))                             ;; show error buffer?

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
    (ediff-buffers (current-buffer) p8-output)
    ))

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
             (flyspell-mode)
             )
          )

(add-hook 'rst-mode-hook
          '(lambda ()
             (message "Loading rst-mode hooks")
             (flyspell-mode)
             (define-key rst-mode-map (kbd "C-c C-a") 'rst-adjust)
             )
          )

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

(defun copy-buffer-file-name ()
  "Add `buffer-file-name' to `kill-ring'"
  (interactive)
  (kill-new buffer-file-name t))
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

(defun occur-region-or-word-at-point ()
  "Run `occur' using the active region or word at point"
  (interactive)
  (let ((occur-string (if (region-active-p)
                          (buffer-substring (region-beginning) (region-end))
                        (thing-at-point 'word))))
    (unless occur-string
      (error "No active region or word at point"))
    (occur occur-string)
    ))
(global-set-key (kbd "M-s r") 'occur-region-or-word-at-point)

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

(require 'lockstep)

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
