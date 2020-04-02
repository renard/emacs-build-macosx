;;; site-start.el --- 

;; Copyright © 2012 Sébastien Gross <seb•ɑƬ•chezwam•ɖɵʈ•org>

;; Author: Sébastien Gross <seb•ɑƬ•chezwam•ɖɵʈ•org>
;; Keywords: emacs, 
;; Created: 2012-09-21
;; Last changed: 2020-04-03 01:04:05
;; Licence: WTFPL, grab your copy here: http://sam.zoy.org/wtfpl/

;; This file is NOT part of GNU Emacs.

;;; Commentary:
;; 


;;; Code:

(require 'cl-lib)


(defconst emacs-version-git-commit "@@GIT_COMMIT@@"
  "String giving the git sha1 from which this Emacs was built.

SHA1 of git commit that was used to compile this version of
emacs. The source used to compile emacs are taken from Savannah's
git repository at `http://git.savannah.gnu.org/r/emacs.git' or
git://git.savannah.gnu.org/emacs.git

See both `http://emacswiki.org/emacs/EmacsFromGit' and
`http://savannah.gnu.org/projects/emacs' for further
information.")

(defconst user-emacs-directory
  (cl-loop for d in
	;; try to load emacs init file from several version based
	;; directories. Use default directory if not found.
	(list (format "~/.emacs.d-%s/" emacs-version-git-commit)
	      (format "~/.emacs.d-%s/" emacs-version)
	      (format "~/.emacs.d-%d/" emacs-major-version)
	      (format "~/.emacs.d-%d.%d/" emacs-major-version emacs-minor-version)
	      "~/.emacs.d/")
	when (and (file-directory-p d)
		  (file-exists-p (concat d "init.el")))
	do (setq user-init-file
		 (expand-file-name (concat d "init.el")))
	and return d))
(message "Using %s in %s" user-init-file user-emacs-directory)

(provide 'early-site-start)

;; site-start.el ends here
