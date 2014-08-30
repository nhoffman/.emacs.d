;; switch between groups using ibuffer-switch-to-saved-filter-groups
(setq ibuffer-saved-filter-groups
      (quote (
	      ("default"
	       ("magit"
	      	(name . "magit"))
	       (".emacs.d"
	      	(filename . "/.emacs.d/"))
	       (".ssh"
	      	(filename . "/.ssh/"))
	       ("zsh"
	      	(filename . "/zsh/"))
	       ("Help"
		(name . "\*help"))
	       ("R processes"
		(name . "\*R"))
	       ("Dired"
	      	(mode . dired-mode))
	       ("Special"
	      	(name . "\*"))
	       ))))
