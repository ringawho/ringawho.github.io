(require 'ox-publish)
(setq straight-vc-git-default-clone-depth 1)
(setq straight-process-buffer " *straight-process*")
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))
(straight-use-package 'htmlize)
(when (< emacs-major-version 29)
  (straight-use-package 'csharp-mode))
(setq org-html-htmlize-output-type 'css
      org-html-head-include-scripts t
      org-html-postamble t
      org-html-postamble-format '(("en" "<p class=\"author\">Author: %a</p>\n<p class=\"date\">Date: %d</p>")))

(setq base-directory "./content")
(setq publishing-directory "./public")
(setq html-head (string-join
                 (list
                  "<link rel=\"stylesheet\" href=\"assets/org-src-block.css\" type=\"text/css\" />"
                  "<link rel=\"stylesheet\" href=\"assets/theme.css\" type=\"text/css\" />"
                  "<script type=\"text/javascript\" src=\"assets/theme.js\"></script>")
                 "\n"))
;; :html-head "<link rel=\"stylesheet\" href=\"https://cdn.simplecss.org/simple.min.css\" />"

(defun org-publish-sitemap-with-date (title list)
  "Default site map, as a string.
TITLE is the title of the site map.  LIST is an internal
representation for the files to include, as returned by
`org-list-to-lisp'.  PROJECT is the current project."
  (concat "#+title: " title "\n"
          "#+date: " (format-time-string (org-time-stamp-format nil t) (current-time)) "\n"
	      (org-list-to-org list)))

(setq org-publish-project-alist
      `(("Blog Html"
         :author "ring"
         :base-directory ,base-directory
         :base-extension "org"
         :publishing-directory ,publishing-directory
         :recursive t
         :publishing-function org-html-publish-to-html
         :auto-sitemap t
         :sitemap-filename "index.org"
         :sitemap-function org-publish-sitemap-with-date
         :sitemap-title "ring"
         :sitemap-sort-files anti-chronologically
         ;; :makeindex t
         :with-toc t

         ;; :html-validation-link nil
         :html-checkbox-type html
         :html-head-include-default-style nil
         :html-head ,html-head
         :html-metadata-timestamp-format "%Y-%m-%d")
        ("Blog Stylesheet"
         :base-directory ,base-directory
         :base-extension "css\\|js\\|png"
         :publishing-directory ,publishing-directory
         :recursive t
         :publishing-function org-publish-attachment)
        ("Blog"
         :components ("Blog Html" "Blog Stylesheet"))))

(org-publish-all t)

(message "Build complete!")

