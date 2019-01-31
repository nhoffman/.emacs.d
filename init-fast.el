(unless (>= emacs-major-version 24)
  (error "Emacs version 24 or higher is required"))

(menu-bar-mode -1)   ;; hide menu bar
(tool-bar-mode -1)   ;; hide tool bar
(scroll-bar-mode -1) ;; hide scroll bar

(require 'package)

;; relies on packages already present in ./elpa
(setq package-load-list '(org))
(package-initialize)

(require 'epa-file)
(setenv "GPG_AGENT_INFO" nil) ;; suppress graphical passphrase prompt

(defun insert-date ()
  ;; Insert today's timestamp in format "<%Y-%m-%d %a>"
  (interactive)
  (insert (format-time-string "<%Y-%m-%d %a>")))
(global-set-key (kbd "C-c d") 'insert-date)

(custom-set-variables
  '(safe-local-variable-values (quote ((toggle-read-only . t)))))

;; org-mode
(add-hook 'org-mode-hook
          '(lambda ()
             (message "Loading org-mode hooks")
             ;; (font-lock-mode)
             ;; (setq org-confirm-babel-evaluate nil)
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

             ;; enable a subset of languages for evaluation in code blocks
             (setq my/org-babel-load-languages
                   '((python . t)
                     (sql . t)
                     (sqlite . t)
                     (emacs-lisp . t)))

             ;; use "shell" for org-mode versions 9 and above
             (add-to-list 'my/org-babel-load-languages
                          (if (>= (string-to-number (substring (org-version) 0 1)) 9)
                              '(shell . t) '(sh . t)))

             (org-babel-do-load-languages
              'org-babel-load-languages my/org-babel-load-languages)))
