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
