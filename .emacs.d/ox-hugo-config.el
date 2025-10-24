;; ox-hugo configuration for org-roam/denote integration
;; This file should be loaded in your Emacs configuration

(use-package ox-hugo
  :ensure t
  :after org-roam denote
  :config
  ;; Set the Hugo site directory
  (setq org-hugo-base-dir (expand-file-name "~/kDrive/1_Projects/ip-spark/"))
  
  ;; Configure ox-hugo to work with org-roam
  (setq org-hugo-section "posts")
  (setq org-hugo-use-code-for-kbd t)
  (setq org-hugo-prefer-hugo-bindings t)
  
  ;; Custom export settings for org-roam integration
  (setq org-hugo-export-with-toc nil)
  (setq org-hugo-export-with-section-numbers nil)
  
  ;; Configure front matter to include org-roam metadata
  (setq org-hugo-front-matter-format 'yaml)
  
  ;; Custom function to generate Hugo-compatible front matter
  (defun my/org-hugo-front-matter (info)
    "Generate front matter that includes org-roam metadata."
    (let* ((title (org-export-data (plist-get info :title) info))
           (date (org-export-data (plist-get info :date) info))
           (tags (org-export-data (plist-get info :filetags) info))
           (id (org-export-data (plist-get info :id) info))
           (slug (org-export-data (plist-get info :slug) info)))
      (concat
       "---\n"
       "title: \"" title "\"\n"
       "date: " (if date date (format-time-string "%Y-%m-%d")) "\n"
       "draft: false\n"
       (if tags (concat "tags: [" tags "]\n") "")
       (if id (concat "org_roam_id: \"" id "\"\n") "")
       (if slug (concat "slug: \"" slug "\"\n") "")
       "---\n\n")))
  
  ;; Override the default front matter function
  (advice-add 'org-hugo--get-front-matter :override #'my/org-hugo-front-matter)
  
  ;; Custom export function for org-roam nodes
  (defun my/org-roam-export-to-hugo ()
    "Export current org-roam node to Hugo format."
    (interactive)
    (when (org-roam-node-at-point)
      (let* ((node (org-roam-node-at-point))
             (file-path (org-roam-node-file node))
             (title (org-roam-node-title node))
             (tags (org-roam-node-tags node)))
        ;; Set buffer to the org-roam file
        (find-file file-path)
        ;; Export to Hugo
        (org-hugo-export-wim-to-md)
        (message "Exported %s to Hugo" title))))
  
  ;; Key binding for quick export
  (define-key org-mode-map (kbd "C-c C-e H h") #'my/org-roam-export-to-hugo)
  
  ;; Auto-export on save (optional)
  (defun my/auto-export-to-hugo-on-save ()
    "Automatically export to Hugo when saving org-roam files."
    (when (and (org-roam-file-p)
               (string-match-p "posts/" (buffer-file-name)))
      (my/org-roam-export-to-hugo)))
  
  ;; Uncomment the line below to enable auto-export
  ;; (add-hook 'after-save-hook #'my/auto-export-to-hugo-on-save)
  )

;; Custom org-roam capture template for blog posts
(defun my/org-roam-blog-capture-template ()
  "Create a blog post capture template that integrates with Hugo."
  (setq org-roam-capture-templates
        (append org-roam-capture-templates
                '(("b" "Blog Post" plain "%?"
                   :target (file+head
                            (concat
                             (format-time-string "%Y%m%dT%H%M%S")
                             "--${slug}"
                             "__%(my/denote-org-roam-keywords)"
                             ".org")
                            (concat
                             ":PROPERTIES:\n"
                             ":ID: %<%Y%m%dT%H%M%S>\n"
                             ":ROAM_EXCLUDE: nil\n"
                             ":END:\n"
                             "#+title: ${title}\n"
                             "#+date: [%<%Y-%m-%d %a %H:%M:%S>]\n"
                             "#+filetags: %^G\n"
                             "#+hugo_section: posts\n"
                             "#+hugo_draft: false\n\n"
                             "%?"))
                   :immediate-finish t
                   :unnarrowed t)))))

;; Initialize the blog capture template
(my/org-roam-blog-capture-template)

(provide 'ox-hugo-config)
