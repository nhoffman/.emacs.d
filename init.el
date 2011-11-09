;; emacs configuration for Noah Hoffman

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

;; non-default key mappings defined here:
;; f5 ; call-last-kbd-macro
;; shift-f5 ; toggle-kbd-macro-recording-on
;; f6 ; Toggle lineum-mode
;; f7 ; toggle visual-line-mode
;; f8 ; ns-toggle-fullscreen
;; C-M-t ; transient-mark-mode
;; C-M-i ; load-init
;; C-x C-b ; remap to electric-buffer-list
;; C-x m ; remap to imenu (default is compose-mail)
;; C-right ; other-window
;; C-left ; other-window -1 (back-window)
;; C-x C-c ; save-buffers-kill-terminal with confirmation
;; C-M-; ; copy region and comment
;; C-M-q ; unfill-paragraph
;; C-x C-g ; ibuffer
;; C-x M-g ; ibuffer-switch-to-saved-buffer-groups
;; C-x 4; transpose-buffers
;; C-x 5; switch-buffers-between-frames
;; C-cl ; org-store-link
;; C-ca ; org-agenda
;; C-cb; org-iswitchb

(message "loading ~/.emacs.d/init.el")

;; startup, appearance, etc
(setq column-number-mode t)
(setq inhibit-splash-screen t)
(setq require-final-newline t)
(setq make-backup-files nil) ;; no backup files
(setq initial-scratch-message nil) ;; no instructions in the *scratch* buffer
(tool-bar-mode -1)

;; date and time in status bar
;; http://efod.se/writings/linuxbook/html/date-and-time.html
(setq display-time-day-and-date t
      display-time-24hr-format t)
(display-time)

;; file path in title bar
;; http://stackoverflow.com/questions/3669511/the-function-to-show-current-files-full-path-in-mini-buffer
(setq frame-title-format
      (list (format "%s %%S: %%j " (system-name))
        '(buffer-file-name "%f" (dired-directory dired-directory "%b"))))

;; debugging
(setq debug-on-error t)
;; (setq debug-on-signal t)

;; scrolling - see http://www.emacswiki.org/emacs/SmoothScrolling
(setq mouse-wheel-scroll-amount '(3 ((shift) . 3))) ;; number of lines at a time
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mosue 't) ;; scroll window under mouse
(setq scroll-step 1) ;; keyboard scroll one line at a time
(setq scroll-conservatively 1) ;; scroll by one line to follow cursor off screen

;; cursor appearance
(set-cursor-color "red")
(blink-cursor-mode 1)

;; marking
(transient-mark-mode 1) ;; highlight active region - default in emacs 23.1+
(global-set-key (kbd "M-C-t") 'transient-mark-mode)
(global-set-key (kbd "C-x C-b") 'electric-buffer-list)
(iswitchb-mode 1)

;; window splitting
;; see http://en.wikipedia.org/wiki/Emacs_Lisp
(defadvice split-window-vertically
  ;; vertical split contains next (instead of current) buffer
  (after my-window-splitting-advice first () activate)
  (set-window-buffer (next-window) (other-buffer)))

(defadvice split-window-horizontally
  ;; horizontal split contains next (instead of current) buffer
  (after my-window-splitting-advice first () activate)
  (set-window-buffer (next-window) (other-buffer)))

;; imenu
(setq imenu-auto-rescan 1)
(global-set-key (kbd "C-x m") 'imenu) ;; overwrites default sequence for compose-mail

;; shortcuts for keyboard macros
;; see http://www.emacswiki.org/emacs/KeyboardMacros
;; note that default bindings for macros are
;; C-x ( – start defining a keyboard macro
;; C-x ) – stop defining the keyboard macro
;; C-x e – execute the keyboard macro
(global-set-key '[(f5)]          'call-last-kbd-macro)
(global-set-key '[(shift f5)]    'toggle-kbd-macro-recording-on)

;; convenience function to (re)load this file
(defun load-init ()
  "Load ~/.emacs.d/init.el"
  (interactive)
  (load "~/.emacs.d/init.el"))
(global-set-key (kbd "M-C-i") 'load-init)

(defun insert-time ()
  ;; Insert today's timestamp in format "<%Y-%m-%d %a>"
  (interactive)
  (insert (format-time-string "<%Y-%m-%d %a>")))

(defun org-add-entry (filename time-format)
  ;; Add an entry to an org-file with today's timestamp.
  (interactive "FFile: ")
  (find-file filename)
  (end-of-buffer)
  (delete-blank-lines)
  (insert "\n* ")
  (insert (format-time-string time-format))
  (beginning-of-line)
  (forward-char 2))

(global-set-key
 (kbd "C-x C-n") (lambda () (interactive)
		   (org-add-entry "~/Dropbox/notes/index.org" 
				  "<%Y-%m-%d %a>")))

(global-set-key
 (kbd "C-x C-j") (lambda () (interactive)
		   (org-add-entry "~/Dropbox/notes/journal.org" 
				  "%A, %B %d, %Y (%Y%m%d)")))
 
;; setup for emacs desktop
;; http://www.gnu.org/software/emacs/manual/html_node/emacs/Saving-Emacs-Sessions.html
;; http://www.emacswiki.org/emacs/DeskTop
(require 'desktop)

;; should save desktop periodically instead of just on exit, but not
;; if emacs is started with --no-desktop TODO: this disables auto save
;; of desktop entirely - need to check explicity for --no-desktop
(unless (equal desktop-save-mode nil)
  (message "Enabling desktop auto-save")
  (add-hook 'auto-save-hook (lambda () (desktop-save-in-desktop-dir)))
  )

(desktop-save-mode 1)
;; restore only N buffers on startup; restore others lazily
;; (setq desktop-restore-eager 10)

;; TODO - instructions for using this?
(defun toggle-kbd-macro-recording-on ()
  "One-key keyboard macros: turn recording on."
  (interactive)
  (define-key
    global-map
    (events-to-keys (this-command-keys) t)
    'toggle-kbd-macro-recording-off)
  (start-kbd-macro nil))

(defun toggle-kbd-macro-recording-off ()
  "One-key keyboard macros: turn recording off."
  (interactive)
  (define-key
    global-map
    (events-to-keys (this-command-keys) t)
    'toggle-kbd-macro-recording-on)
  (end-kbd-macro))

;; enhance marking in transient mark mode
;; see http://www.masteringemacs.org/articles/2010/12/22/fixing-mark-commands-transient-mark-mode/
;; (defun push-mark-no-activate ()
;;   "Pushes `point' to `mark-ring' and does not activate the region
;; Equivalent to \\[set-mark-command] when \\[transient-mark-mode] is disabled"
;;   (interactive)
;;   (push-mark (point) t nil)
;;   (message "Pushed mark to ring"))
;; (global-set-key (kbd "C-`") 'push-mark-no-activate)

;; (defun jump-to-mark ()
;;   "Jumps to the local mark, respecting the `mark-ring' order.
;; This is the same as using \\[set-mark-command] with the prefix argument."
;;   (interactive)
;;   (set-mark-command 1))
;; (global-set-key (kbd "M-`") 'jump-to-mark)

;; (defun exchange-point-and-mark-no-activate ()
;;   "Identical to exchange-point-and-mark but will not activate the region."
;;   (interactive)
;;   (exchange-point-and-mark)
;;   (deactivate-mark nil))
;; (define-key global-map [remap exchange-point-and-mark] 'exchange-point-and-mark-no-activate)

;; not available before 23.1
(global-set-key (kbd "<f6>") 'linum-mode)
(global-set-key (kbd "<f7>") 'visual-line-mode)
(global-set-key (kbd "<f8>") 'ns-toggle-fullscreen)

;; platform and display-specific settings
(defun fix-frame ()
  (interactive)
  (cond ((string= "ns" window-system) ;; cocoa
	 (message (format "** running %s windowing system" window-system))
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
	 )
	((string= "x" window-system)
	 (message (format "** running %s windowing system" window-system))
	 (setq my-default-font "Liberation Mono-10")
	 ;; M-w or C-w copies to system clipboard
	 ;; see http://www.gnu.org/software/emacs/elisp/html_node/Window-System-Selections.html
	 (setq x-select-enable-clipboard t)
	 )
	(t
	 (message "** running unknown windowing system")
	 (setq my-default-font nil)
	 (menu-bar-mode -1) ;; hide menu bar
	 )
	)

  (unless (equal window-system nil)
    (message (format "** setting default font to %s" my-default-font))
    (condition-case nil
	(set-default-font my-default-font)
      (error (message (format "** could not set to font %s" my-default-font))))
    )
  )

;; apply above settings on startup
(fix-frame)

;; ...and when creating a new connection to emacs server via emacsclient
;; TODO - not sure why this doesn't seem to take effect on frame creation
(add-hook 'server-visit-hook
	  '(lambda ()
	     (message "** applying server-visit-hooks")
	     (fix-frame)
	     )
	  )

;; Copies lines in the absence of an active region
;; see http://emacs-fu.blogspot.com/2009/11/copying-lines-without-selecting-them.html
;; (defadvice kill-ring-save (before slick-copy activate compile) "When
;;   called interactively with no active region, copy a single line
;;   instead."  (interactive (if mark-active (list (region-beginning)
;;   (region-end)) (message "Copied line") (list
;;   (line-beginning-position) (line-beginning-position 2)))))

;; (defadvice kill-region (before slick-cut activate compile) "When
;; called interactively with no active region, kill a single line
;; instead."  (interactive (if mark-active (list (region-beginning)
;; (region-end)) (list (line-beginning-position) (line-beginning-position
;; 2)))))

;; move lines up and down more easily
;; see http://stackoverflow.com/questions/2423834/move-line-region-up-and-down-in-emacs
;; move line up
(defun move-line-up ()
  (interactive)
  (transpose-lines 1)
  (previous-line 2))
(global-set-key (kbd "M-<up>") 'move-line-up)

;; move line down
(defun move-line-down ()
  (interactive)
  (next-line 1)
  (transpose-lines 1)
  (previous-line 1))
(global-set-key (kbd "M-<down>") 'move-line-down)

;; other-window bound by default to `C-x o`
(defun back-window ()
  (interactive)
  (other-window -1))
(global-set-key (kbd "C-<right>") 'other-window)
(global-set-key (kbd "C-<left>") 'back-window)

(defun copy-buffer-file-name ()
  "Add `buffer-file-name' to `kill-ring'"
  (interactive)
  (kill-new buffer-file-name t)
)

;; see http://www.emacswiki.org/emacs/SwitchingBuffers
;; note that original code used function 'plusp', which
;; seems not to be defined
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

;; also from http://www.emacswiki.org/emacs/SwitchingBuffers
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

;; Default 'untabify converts a tab to equivalent number of
;; spaces before deleting a single character.
(setq backward-delete-char-untabify-method "all")
(show-paren-mode 1)
;; Start scrolling when 2 lines from top/bottom
(setq scroll-margin 2)

;; Require prompt before exit on C-x C-c
;; http://www.dotemacs.de/dotfiles/KilianAFoth.emacs.html
(global-set-key [(control x) (control c)]
		(function
		 (lambda () (interactive)
		   (cond ((y-or-n-p "Quit? (save-buffers-kill-terminal) ")
			  (save-buffers-kill-terminal))))))

;; automatically refresh buffers from disk (default is every 5 sec)
;; see http://www.cs.cmu.edu/cgi-bin/info2www?(emacs)Reverting
;; note variable auto-revert-interval
(global-auto-revert-mode 1)

;; set Emacs Load Path
;; (setq load-path (cons "/usr/local/share/emacs/site-lisp/" load-path))
;; (setq load-path (cons "~/.emacs.d" load-path))

(add-to-list 'load-path "~/.emacs.d/")
(add-to-list 'load-path "/usr/local/share/emacs/site-lisp/")

;; word count - http://www.emacswiki.org/emacs/WordCount
(condition-case nil
    (require 'wc)
  (error (message "** could not load wc")))

;; load auctex
(condition-case nil
    (require 'tex-site)
  (error (message "** could not load auctex")))

;; load ess-mode
;; to install locally:
;; export ESS_VER=5.9.1
;; cd ~/src && \
;; wget http://ess.r-project.org/downloads/ess/ess-${ESS_VER}.tgz && \
;; tar -xzvf ess-${ESS_VER}.tgz && \
;; cp -r ess-${ESS_VER} ~/.emacs.d/ess
(condition-case nil
    (require 'ess-site "~/.emacs.d/ess/lisp/ess-site")
  (error (message "** could not load local ESS in ~/.emacs.d; trying system ESS")
	 (condition-case nil
	     (require 'ess-site)
	   (error (message "** could not load system ESS")))
	 )
  )

;; clean up items defining function arguments - place cursor at the
;; start of the line before '\item'
(fset 'rd-clean-item
   "\C-[[B\C-k\C-[[B\C-?\C-?\C-e\C-m\C-[[B\C-a")

;; enable pylit
;; http://www.emacswiki.org/cgi-bin/wiki/pylit.el
;; As of 2007-02-09, the PyLit_ distribution does not include a script
;; to invoke PyLit's functionality from the command line.  On any
;; UNIX-like system, this can be easily worked around by creating a
;; file ``pylit`` somewhere in your executable search path (the
;; ``PATH``) with the following contents:
;;
;; #!/bin/sh
;; exec env PYTHONPATH=/path/to/pylit/repository/src \
;; python /path/to/pylit/repository/src.pylit.py "$@"
;;
;; or perhaps better for my setup:
;; sudo cat > /usr/local/bin/pylit << EOF
;; #!/bin/sh
;; pylit.py "$@"

;; EOF
;; sudo chmod +x /usr/local/bin/pylit

(condition-case nil
    (require 'pylit)
  (error (message "** could not load pylit")))

;; org-mode
(condition-case nil
    (require 'org-install)
  (error (message "** could not load system org-mode")))

(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c b") 'org-iswitchb)

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
	     ;; (icicle-mode)
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
		))
	     )
	  )

;; org-mode file suffix matching
(push '("\\.org\\'" . org-mode) auto-mode-alist)
(push '("\\.org\\.txt\\'" . org-mode) auto-mode-alist)

(setq org-agenda-files (list "~/Dropbox/notes/index.org"
			     "~/Dropbox/fredross/notes/plans.org"
			     ))

;; Set to the location of your Org files on your local system
(setq org-directory "~/Dropbox/notes")
;; Set to the name of the file where new notes will be stored
(setq org-mobile-inbox-for-pull "~/Dropbox/notes/flagged.org")
;; Set to <your Dropbox root directory>/MobileOrg.
(setq org-mobile-directory "~/Dropbox/MobileOrg")

;; moinmoin-mode
;;  wget -U Mozilla -O moinmoin-mode.el "http://moinmoin.wikiwikiweb.de/EmacsForMoinMoin/MoinMoinMode?action=raw"
;; requires http://homepage1.nifty.com/bmonkey/emacs/elisp/screen-lines.el
(condition-case nil
    (require 'moinmoin-mode)
  (error (message "** could not load moinmon-mode")))

;; markdown-mode
;;  see http://jblevins.org/projects/markdown-mode/
;;  wget http://jblevins.org/git/markdown-mode.git/plain/markdown-mode.el
(autoload 'markdown-mode "markdown-mode.el"
   "Major mode for editing Markdown files" t)
(push '("\\.md" . markdown-mode) auto-mode-alist)

;; chrome "edit with emacs"
(condition-case nil
    (require 'edit-server)
  (error (message "** could not load edit-server (chrome edit with emacs)")))

(condition-case nil
    (edit-server-start)
  (error (message "** could not start edit-server (chrome edit with emacs)")))

;;;;;;;; spelling ;;;;;;;
;;use aspell instead of ispell
;;this may need to remain the last line
(setq-default ispell-program-name "aspell")
(setq ispell-dictionary "en")

;;enable on-the-fly spell-check
(autoload 'flyspell-mode "flyspell" "On-the-fly spelling checker." t)

;; support for emacs use within pine/alpine
;; see http://snarfed.org/space/emacs%20font-lock%20faces%20for%20composing%20email
(add-hook 'find-file-hooks
	  '(lambda ()
	     (if (equal "pico." (substring (buffer-name (current-buffer)) 0 5))
		 ;; (message "** running hook for pine/alpine")
		 (mail-mode)
	       )
	     )
	  )


;;;; python-mode configuration
;; see http://jesselegg.com/archives/2010/02/25/emacs-python-programmers-part-1/
(add-hook 'python-mode-hook
	  '(lambda ()
	     (message "Loading python-mode hooks")
	     (setq indent-tabs-mode nil)
	     (setq tab-width 4)
	     (setq py-indent-offset tab-width)
	     (setq py-smart-indentation t)
	     (define-key python-mode-map "\C-m" 'newline-and-indent)
	     (hs-minor-mode)
	     ;; add function index to menu bar
	     (imenu-add-menubar-index)
	     ;; (python-mode-untabify)
	     ;; (linum-mode)
	     )
	  )

;; python-mode file name mappings
(push '("SConstruct" . python-mode) auto-mode-alist)
(push '("SConscript" . python-mode) auto-mode-alist)

;; ess-mode hooks
(add-hook 'ess-mode-hook
	  '(lambda()
	     (message "Loading ess-mode hooks")
	     ;; leave my underscore key alone!
	     (ess-toggle-underscore nil)
	     ;; set ESS indentation style
	     ;; choose from GNU, BSD, K&R, CLB, and C++
	     (ess-set-style 'GNU 'quiet)
	     (flyspell-mode)
	     (ess-imenu-R) ;; add function index to menu bar TODO - why doesn't this work?
	     )
	  )

;; text mode hooks
(add-hook 'text-mode-hook
	  '(lambda ()
	     ;; (longlines-mode)
	     (flyspell-mode)
	     )
	  )

;; tex-mode hooks
;; (add-hook 'tex-mode-hook
;; 	  '(lambda ()
;; 	     (flyspell-mode)
;; 	     (imenu-add-menubar-index) ;; add function index to menu bar
;; 	     )
;; 	  )

;; rst-mode hooks
(add-hook 'rst-mode-hook
	  '(lambda ()
	     (message "Loading rst-mode hooks")
	     (flyspell-mode)
	     (define-key rst-mode-map (kbd "C-c C-a") 'rst-adjust)
	     )
	  )

;; pylit-mode hooks
(add-hook 'pylit-mode-hook
	  '(lambda ()
	     (message "Loading pylit-mode hooks")
	     (flyspell-mode)
	     )
	  )

;; remote file editing using tramp
;; see http://www.gnu.org/software/tramp/
(condition-case nil
    (require 'tramp)
  (setq tramp-default-method "scp")
  (error (message "** could not load tramp")))

;; ibuffer
;; see http://emacs-fu.blogspot.com/2010/02/dealing-with-many-buffers-ibuffer.html
(require 'ibuffer)
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

;; load the config file on startup
(ibuffer-load-config)

(defun ibuffer-reload ()
  ;; kill ibuffer, reload the config file, and return to ibuffer
  (interactive)
  (ibuffer)
  (kill-buffer)
  (ibuffer-load-config)
  (ibuffer)
  )

(global-set-key (kbd "C-x C-g") 'ibuffer)
(global-set-key (kbd "C-x M-g") 'ibuffer-switch-to-saved-filter-groups)

(setq ibuffer-show-empty-filter-groups nil)

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

;; from http://www.emacswiki.org/emacs/IbufferMode
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

;; from http://curiousprogrammer.wordpress.com/2009/04/02/ibuffer/
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

;; icicles
;; http://www.emacswiki.org/emacs/Icicles
;; http://www.emacswiki.org/emacs/Icicles_-_Libraries
;; wget http://www.emacswiki.org/emacs/download/get-icicles.sh
;; sh get-icicles.sh
;; (setq load-path (cons "~/.emacs.d/icicles" load-path))
(add-to-list 'load-path "~/.emacs.d/icicles")
(condition-case nil
    (require 'icicles)  
  (error (message "** could not load icicles")))

(condition-case nil
    (icicle-mode 1)
  (error (message "** could not start icicles")))

;; uniquify - http://www.emacswiki.org/emacs/uniquify
(require 'uniquify)

;; ido mode
;; (condition-case nil
;;     (require 'ido)
;;   (error (message "** could not load ido mode")))

;; keyboard macro copy-and-comment, bound to CM-;
(fset 'copy-and-comment
   "\367\C-x\C-x\273")
(global-set-key (kbd "M-C-;") 'copy-and-comment)

;; define get-buffer-file-name
(fset 'get-buffer-file-name
   "\C-hvbuffer-file-name\C-m")

;; from http://defindit.com/readme_files/emacs_hints_tricks.html
(defun unfill-paragraph ()
  (interactive)
  (let ((fill-column (point-max)))
  (fill-paragraph nil)))
(global-set-key (kbd "M-C-q") 'unfill-paragraph)

;; git support (included in emacs 22.2+)
(require 'vc-git)

;; sql-sqlite
(setq sql-sqlite-program "sqlite3")

;; sql-related enhancements
;; http://atomized.org/2008/10/enhancing-emacs%E2%80%99-sql-mode/
;; support for preset connections
;; TODO: consider using Tom's setup: https://uwmc-labmed.beanstalkapp.com/developers/browse/personal/twe/trunk/EMACS/sql.el
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

;; http://www.emacswiki.org/emacs/EasyPG
(require 'epa-file)
;; (epa-file-enable)
;; suppress graphical passphrase prompt
(setenv "GPG_AGENT_INFO" nil)

;; rainbow-delimiters
;; http://www.emacswiki.org/emacs/RainbowDelimiters
;; setup:
;; cd ~/.emacs.d
;; wget http://www.emacswiki.org/emacs/download/rainbow-delimiters.el
;; emacs -batch -f batch-byte-compile rainbow-delimiters.el
;; or M-x byte-compile-file <location of rainbow-delimiters.el>
;; M-x rainbow-delimiters-mode
(condition-case nil
    (require 'rainbow-delimiters)
  (error (message "** could not load rainbow-delimiters")))

;; gist.el
;; https://github.com/defunkt/gist.el
;; added as a submodule:
;; % git submodule add https://github.com/defunkt/gist.el.git
;; now, to clone .emacs.d elsewhere:
;; % git clone git@github.com:nhoffman/.emacs.d.git
;; % cd .emacs.d
;; % git submodule init && git submodule update
(condition-case nil
    (require 'gist "~/.emacs.d/gist.el/gist.el")
  (error (message "** could not load gist")))

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

