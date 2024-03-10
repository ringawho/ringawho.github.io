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
(setq org-html-htmlize-output-type 'css)

(setq base-directory "./content")
(setq publishing-directory "./public")
(setq html-head (string-join
                 (list
                  "<link rel=\"stylesheet\" href=\"assets/org-src-block.css\" type=\"text/css\" />"
                  "<link rel=\"stylesheet\" href=\"assets/theme.css\" type=\"text/css\" />"
                  "<script type=\"text/javascript\" src=\"assets/theme.js\"></script>")
                 "\n"))
;; :html-head "<link rel=\"stylesheet\" href=\"https://cdn.simplecss.org/simple.min.css\" />"

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
         :sitemap-title "ring"
         ;; :makeindex t
         :with-toc t

         :html-validation-link nil
         :html-checkbox-type html
         :html-head-include-default-style nil
         :html-head ,html-head)
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

