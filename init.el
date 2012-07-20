
(message "loading ~/.emacs.d/init.el")

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

(setq init-file-name "init.el")
(defun init-insert-code-block ()
  ;; Insert a code block that will be tangled into `init-file-name`
  (interactive)
  (backward-paragraph)
  (insert (format "#+BEGIN_SRC elisp :tangle %s" init-file-name))
  (forward-paragraph)
  (insert "#+END_SRC\n"))
(global-set-key (kbd "C-c i") 'init-insert-code-block)

(defun init-load ()
  "Load ~/.emacs.d/init.el"
  (interactive)
  (load "~/.emacs.d/init.el"))
(global-set-key (kbd "M-C-i") 'init-load)

(defun init-compile ()
  "Tangle and export html"
  (interactive)
  (org-babel-tangle)
  (org-export-as-html-and-open 3)
  )

(defalias 'dtw 'delete-trailing-whitespace)

(global-set-key (kbd "<f6>") 'linum-mode)
(global-set-key (kbd "<f7>") 'visual-line-mode)
(global-set-key (kbd "<f8>") 'ns-toggle-fullscreen)

;; (setq debug-on-error t)
;; (setq debug-on-signal t)

(setq column-number-mode t)
(setq inhibit-splash-screen t)
(setq require-final-newline t)
(setq make-backup-files nil) ;; no backup files
(setq initial-scratch-message nil) ;; no instructions in the *scratch* buffer
(setq suggest-key-bindings 4)
(show-paren-mode 1)

(setq display-time-day-and-date t
      display-time-24hr-format t)
(display-time)

(setq frame-title-format
      (list (format "%s %%S: %%j " (system-name))
        '(buffer-file-name "%f" (dired-directory dired-directory "%b"))))

(global-auto-revert-mode 1)

(global-set-key [(control x) (control c)]
                (function
                 (lambda () (interactive)
                   (cond ((y-or-n-p "Quit? (save-buffers-kill-terminal) ")
                          (save-buffers-kill-terminal))))))

(defun fix-frame ()
  (interactive)
  (menu-bar-mode -1) ;; hide menu bar
  (tool-bar-mode -1) ;; hide tool bar
  (cond ((string= "ns" window-system) ;; cocoa
         (progn (message (format "** running %s windowing system" window-system))
         ;; key bindings for mac - see
         ;; http://stuff-things.net/2009/01/06/emacs-on-the-mac/
         ;; http://osx.iusethis.com/app/carbonemacspackage
         (set-keyboard-coding-system 'mac-roman)
         (setq mac-option-modifier 'meta)
         (setq mac-command-key-is-meta nil)
         (setq my-default-font "Bitstream Vera Sans Mono-14")
         ;; enable edit-with-emacs for chrome
         ;; (require 'edit-server)
         ;; (edit-server-start)
         ))
        ((string= "x" window-system)
         (progn 
           (message (format "** running %s windowing system" window-system))
           (setq my-default-font "Liberation Mono-10")
           ;; M-w or C-w copies to system clipboard
           ;; see http://www.gnu.org/software/emacs/elisp/html_node/Window-System-Selections.html
           (setq x-select-enable-clipboard t)
           ;; (set-scroll-bar-mode -1) ;; hide scroll bar
           (scroll-bar-mode -1) ;; hide scroll bar
           ))
        (t
         (progn 
         (message "** running unknown windowing system")
         (setq my-default-font nil)
         (scroll-bar-mode -1) ;; hide scroll bar
         ))
        )

  (unless (equal window-system nil)
    (message (format "** setting default font to %s" my-default-font))
    (condition-case nil
        (set-default-font my-default-font)
      (error (message (format "** could not set to font %s" my-default-font))))
    )
  )

(defun font-dejavu ()
  ;; set default font to dejavu sans mono-11
  (interactive)
  (set-default-font "dejavu sans mono-11")
  )

(fix-frame)

(add-hook 'server-visit-hook 'fix-frame)

(setq mouse-wheel-scroll-amount '(3 ((shift) . 3))) ;; number of lines at a time
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mosue 't) ;; scroll window under mouse
(setq scroll-step 1) ;; keyboard scroll one line at a time
(setq scroll-conservatively 1) ;; scroll by one line to follow cursor off screen
(setq scroll-margin 2) ;; Start scrolling when 2 lines from top/bottom

(transient-mark-mode 1) ;; highlight active region - default in emacs 23.1+
(global-set-key (kbd "M-C-t") 'transient-mark-mode)
(global-set-key (kbd "C-x C-b") 'electric-buffer-list)

(set-cursor-color "red")
(blink-cursor-mode 1)

(iswitchb-mode 1)

(global-set-key (kbd "<f5>") 'call-last-kbd-macro)

(require 'desktop)

(desktop-save-mode 1)
(if (and desktop-save-mode (not (member "--no-desktop" command-line-args)))
    (progn
      (message "Enabling desktop auto-save")
      (add-hook 'auto-save-hook 'desktop-save-in-desktop-dir)))

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

(defun back-window ()
  (interactive)
  (other-window -1))
(global-set-key (kbd "C-<right>") 'other-window)
(global-set-key (kbd "C-<left>") 'back-window)

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

(setq-default ispell-program-name "aspell")
(setq ispell-dictionary "en")

(autoload 'flyspell-mode "flyspell" "On-the-fly spelling checker." t)
(setq flyspell-issue-welcome-flag nil) ;; fix error message

(add-hook 'find-file-hooks
          '(lambda ()
             (if (equal "pico." (substring (buffer-name (current-buffer)) 0 5))
                 ;; (message "** running hook for pine/alpine")
                 (mail-mode))))

(add-to-list 'load-path "~/.emacs.d/")

(condition-case nil
    (require 'tex-site)
  (error (message "** could not load auctex")))

(condition-case nil
    (require 'ess-site "~/.emacs.d/ess/lisp/ess-site")
  (error (message "** could not load local ESS in ~/.emacs.d; trying system ESS")
         (condition-case nil
             (require 'ess-site)
           (error (message "** could not load system ESS")))
         )
  )

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

(add-to-list 'load-path "~/.emacs.d/org-mode/lisp")
(require 'org-install)

(add-hook 'org-mode-hook
          '(lambda ()
             (message "Loading org-mode hooks")
             (turn-on-font-lock)
             (define-key org-mode-map (kbd "M-<right>") 'forward-word)
             (define-key org-mode-map (kbd "M-<left>") 'backward-word)
             ;; provides key mapping for the above; replaces default
             ;; key bindings for org-promote/demote-subtree
             (define-key org-mode-map (kbd "M-S-<right>") 'org-do-demote)
             (define-key org-mode-map (kbd "M-S-<left>") 'org-do-promote)
             (org-indent-mode)
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
                (pygment . t)
                ))
             )
          )

(custom-set-variables
 '(org-confirm-babel-evaluate nil)
 '(org-src-fontify-natively t))

(setq org-agenda-files (list "~/Dropbox/notes/index.org"
                             "~/Dropbox/fredross/notes/plans.org"
                             ))

(push '("\\.org\\'" . org-mode) auto-mode-alist)
(push '("\\.org\\.txt\\'" . org-mode) auto-mode-alist)

(condition-case nil
    (progn
      (require 'ob-pygment)
      (setq org-pygment-path "/usr/local/bin/pygmentize"))
  (error (message "** could not load ob-pygment")))

(defun insert-date ()
  ;; Insert today's timestamp in format "<%Y-%m-%d %a>"
  (interactive)
  (insert (format-time-string "<%Y-%m-%d %a>")))
(global-set-key (kbd "C-c d") 'insert-date)

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

(global-set-key
 (kbd "C-x C-n") (lambda () (interactive)
                   (org-add-entry "~/Dropbox/notes/index.org"
                                  "\n* <%Y-%m-%d %a>")))

(global-set-key
 (kbd "C-x C-m") (lambda () (interactive)
                   (org-add-entry "~/Dropbox/notes/todo.org"
                                  "\n** TODO <%Y-%m-%d %a>")))

(condition-case nil
    (require 'moinmoin-mode)
  (error (message "** could not load moinmon-mode")))

(autoload 'markdown-mode "markdown-mode.el"
   "Major mode for editing Markdown files" t)
(push '("\\.md" . markdown-mode) auto-mode-alist)

(condition-case nil
    (require 'edit-server)
  (error (message "** could not load edit-server (chrome edit with emacs)")))

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
             ;; (hs-minor-mode)
             ;; add function index to menu bar
             ;; (imenu-add-menubar-index)
             ;; (python-mode-untabify)
             ;; (linum-mode)
             )
          )

(push '("SConstruct" . python-mode) auto-mode-alist)
(push '("SConscript" . python-mode) auto-mode-alist)
(push '("*.cgi" . python-mode) auto-mode-alist)

(setq backward-delete-char-untabify-method "all")

(add-to-list 'load-path "~/.emacs.d/python-pylint")
(autoload 'python-pylint "~/.emacs.d/python-pylint")
(autoload 'pylint "~/.emacs.d/python-pylint")

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

(require 'ibuffer)
(global-set-key (kbd "C-x C-g") 'ibuffer)
(global-set-key (kbd "C-x M-g") 'ibuffer-switch-to-saved-filter-groups)
(setq ibuffer-show-empty-filter-groups nil)

(setq ibuffer-config-file "~/.emacs.d/ibuffer-config.el")

(defun ibuffer-load-config ()
  ;; load the ibuffer config file
  (interactive)
  (condition-case nil
      (progn
        (message (format "** loading ibuffer config in %s" ibuffer-config-file))
        (load ibuffer-config-file)
        )
    (error (message (format "** could not load %s" ibuffer-config-file))))
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
             )
          )

(require 'uniquify)

(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(setq ido-use-virtual-buffers t)
(ido-mode 1)

(recentf-mode 1)
(defun ido-choose-from-recentf ()
  "Use ido to select a recently visited file from the `recentf-list'"
  (interactive)
  (find-file (ido-completing-read "Open file: " recentf-list nil t)))
(global-set-key (kbd "C-c f") 'ido-choose-from-recentf)

(require 'vc-git)

(add-to-list 'load-path "~/.emacs.d/magit")
(condition-case nil
    (require 'magit)
  (error (message "** could not load magit")))
(global-set-key (kbd "C-c m") 'magit-status)

(setq sql-sqlite-program "sqlite3")

(setq sql-connection-alist
      '((filemaker-sps
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
;; (epa-file-enable)
;; suppress graphical passphrase prompt
(setenv "GPG_AGENT_INFO" nil)

(condition-case nil
    (require 'rainbow-delimiters)
  (error (message "** could not load rainbow-delimiters")))

(defun copy-buffer-file-name ()
  "Add `buffer-file-name' to `kill-ring'"
  (interactive)
  (kill-new buffer-file-name t)
)

(fset 'copy-and-comment
   "\367\C-x\C-x\273")
(global-set-key (kbd "M-C-;") 'copy-and-comment)

(fset 'get-buffer-file-name
   "\C-hvbuffer-file-name\C-m")

(defun unfill-paragraph ()
  (interactive)
  (let ((fill-column (point-max)))
  (fill-paragraph nil)))
(global-set-key (kbd "M-C-q") 'unfill-paragraph)

(setq ns-pop-up-frames nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; content below was added by emacs ;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(safe-local-variable-values (quote ((toggle-read-only . t))))
 '(uniquify-buffer-name-style (quote post-forward) nil (uniquify)))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 )
