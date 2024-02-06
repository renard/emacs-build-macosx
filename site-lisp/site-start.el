;;; site-start.el --- 

;; Copyright © 2012 Sébastien Gross <seb•ɑƬ•chezwam•ɖɵʈ•org>

;; Author: Sébastien Gross <seb•ɑƬ•chezwam•ɖɵʈ•org>
;; Keywords: emacs, 
;; Created: 2012-09-21
;; Last changed: 2024-02-06 02:06:23
;; Licence: WTFPL, grab your copy here: http://sam.zoy.org/wtfpl/

;; This file is NOT part of GNU Emacs.

;;; Commentary:
;; 


;;; Code:

(require 'dired)
(require 'files)

(setq source-directory
      (expand-file-name  ".." data-directory))

(defvar emacs-patches-directory
  (expand-file-name  "../patches" data-directory)
  "Directory containing all patches used when Emacs was built.")

(defvar emacs-patches-list
  (condition-case err
      (directory-files emacs-patches-directory nil ".*\\.patch" nil)
    ;; When Emacs is build, the sanity-check target runs emacs and
    ;; requires site-start. If the `emacs-patches-directory' is not
    ;; found the sanity-check target fails.
    ;;
    ;; 408c904d6602cf269c128a5b5e7b9d0e0b4f7d69 fix this but as not
    ;; been merged to the Emacs-29 build cycle. To prevent a build
    ;; failure, return `nil' if `emacs-patches-directory' is not
    ;; found.
    ;;
    ;; See Emacs bug 66721 for further details:
    ;; https://mail.gnu.org/archive/html/bug-gnu-emacs/2023-10/msg01505.html
    (file-missing nil))
  "List of all patches applied when Emacs was built.")

(defun view-emacs-patches ()
  "Open `dired' in `emacs-patches-directory'."
  (interactive)
  (dired emacs-patches-directory))

(defun view-emacs-patch (&optional patch)
  "Find PATCH used when Emacs was built in
`emacs-patches-directory'."
  (interactive)
  (let* ((patch (or patch
		    (completing-read "View patch: "
				     emacs-patches-list nil t)))
	 (patch-file (format "%s/%s" emacs-patches-directory patch)))
    (if (file-exists-p patch-file)
	(find-file patch-file)
      (error "No patch matching %s found in %s."
	     patch emacs-patches-directory))))

(provide 'site-start)

;; site-start.el ends here
