;; http://ergoemacs.org/emacs/elisp_hash_table.html
(defun is-option (str)
  ;; return true if string looks like a command line option
  (string-equal (substring str 0 1) "-"))

(defun require-option (opt args)
  ;; Raise an error if option `opt` is not in hash-table `args`
  (if (not (gethash opt args))
      (error (format "Error: option -%s is required" opt))))

(defun replace-all (from-str to-str)
  ;; replace all occurrences of from-str with to-str
  (progn
    (beginning-of-buffer)
    (while (search-forward from-str nil t)
      (replace-match to-str nil t))))

;; allows arbitrary command line arguments
(defun do-nothing () t)
(setq command-line-functions '(do-nothing))

;; store option, value pairs in hash-map `args`
(defvar args (make-hash-table :test 'equal))

;; process command-line-args
(setq clargs command-line-args)
(while clargs
  (setq opt (car clargs))
  (setq val (car (cdr clargs)))
  (if (and (is-option opt) (not (is-option val)))
      (puthash (substring opt 1 nil) val args))
  (setq clargs (cdr clargs)))

(setq make-backup-files nil)

(require 'package)
(package-initialize)

(require 'ob-pygment "~/.emacs.d/ob-pygment.el")

(add-hook 'org-mode-hook
	  '(lambda ()
	     (turn-on-font-lock)
	     (setq org-src-fontify-natively t)
	     (setq org-pygment-path "/usr/local/bin/pygmentize")

	     (setq org-confirm-babel-evaluate nil)
	     (setq org-export-allow-BIND 1)
	     (setq org-export-html-coding-system 'utf-8)
	     (setq org-export-html-postamble nil)
	     (setq org-export-htmlize-output-type 'css)
	     (org-babel-do-load-languages
	      (quote org-babel-load-languages)
	      (quote ((R . t)
		      (latex . t)
		      (python . t)
		      (sh . t)
		      (sql . t)
		      (sqlite . t)
		      (pygment . t)
		      (emacs-lisp . t)
		      )))
	     ))
