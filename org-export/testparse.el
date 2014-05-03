(require 'cli (concat (file-name-directory load-file-name) "cli.el"))

(byte-compile-file (concat (file-name-directory load-file-name) "cli.el"))

(setq options-alist
      '(("--infile" "input file")
	("--outfile" "output file")
	("--package-dir" "Directory containing elpa packages" "~/.org-export")
	("--package-upgrade" "Perform package upgrade" nil)
	))

(setq args (cli-parse-args options-alist))
(defun clarg (name) (gethash name args))

(cli-package-setup
 (clarg "package-dir") '(ess htmlize org) (clarg "package-upgrade"))


