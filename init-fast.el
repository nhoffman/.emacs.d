(unless (= emacs-major-version 24)
  (error "Emacs version 24 is required"))

(require 'package)

;; relies on packages already present in ./elpa
(package-initialize)

;; ;; Original Emacs Lisp Package Archive
;; (add-to-list 'package-archives
;; 	     '("elpa" . "http://tromey.com/elpa/") t)
;; ;; User-contributed repository
;; ;; Marmalade is for packages that cannot be uploaded to the official ELPA repository.
;; (add-to-list 'package-archives
;; 	     '("marmalade" . "http://marmalade-repo.org/packages/") t)
;; (add-to-list 'package-archives
;; 	     '("melpa" . "http://melpa.milkbox.net/packages/") t)
;; (add-to-list 'package-archives
;; 	     '("org" . "http://orgmode.org/elpa/") t)
;; (add-to-list 'package-archives
;; 	     '("elpy" . "http://jorgenschaefer.github.io/packages/") t)

(require 'epa-file)
(setenv "GPG_AGENT_INFO" nil) ;; suppress graphical passphrase prompt

(custom-set-variables
  '(safe-local-variable-values (quote ((toggle-read-only . t)))))
