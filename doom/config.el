;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
;; (setq user-full-name "John Doe"
;;       user-mail-address "john@doe.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-symbol-font' -- for symbols
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;;
;;(setq doom-font (font-spec :family "Fira Code" :size 12 :weight 'semi-light)
;;      doom-variable-pitch-font (font-spec :family "Fira Sans" :size 13))
;;
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-one)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/kDrive/2_Areas/org/")


;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

(setq org-reveal-root "file:///.config/doom/reveal.js")

(use-package ox-reveal
  :ensure t
  :config
  (setq org-reveal-root "file:///.config/doom/reveal.js"))

(setq org-refile-targets '((org-agenda-files :maxlevel . 3)))
(setq org-refile-use-outline-path 'file)

(setq org-agenda-custom-commands
  '(("d" "My Daily Review" 
     ;; Block 1: Immediate Tasks (High Priority or Overdue)
     ((tags-todo "+PRIORITY=\"A\"|DEADLINE<\"<+1d>\"" 
       ((org-agenda-overriding-header "🚨 PRIORITY A & DEADLINE Tasks")))

      ;; Block 2: Today's Schedule and Habits
      (agenda "" 
       ((org-agenda-span 'day)
        (org-agenda-overriding-header "🗓 Today's Schedule")))

      ;; Block 3: Next Actions (TODOs tagged with next)
      (tags-todo "+NEXT-WAITING" 
       ((org-agenda-overriding-header "✅ Next Actions (Not Waiting)")))

      ;; Block 4: All Waiting Items
      (todo "WAITING" 
       ((org-agenda-overriding-header "⏳ Waiting For...")))
      )

     ;; Global options for the block agenda
     ((org-agenda-compact-blocks t))
    )))
;; ~/.doom.d/config.el

;; ----------------------------------------
;; A. Shared Directory & Denote Setup
;; ----------------------------------------

;; 1. Define the directory where all notes will be stored.
(setq org-roam-directory (expand-file-name "~/kDrive/2_Areas/org/roam_notes/"))
(setq denote-directory org-roam-directory)

;; 2. Set Denote's file type to org.
(setq denote-file-type 'org)
;; ~/.doom.d/config.el - PURE ORG-ROAM V2
 
;; 2. Enable automatic database sync
(org-roam-db-autosync-mode)

;; 3. Set the standard capture template (as above)
(setq org-roam-capture-templates
      '(("d" "default" plain "%?"
         :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                            "#+title: ${title}\n")
         :unnarrowed t)
        
        ("p" "Public/Publish" plain "%?"
         :target (file+head
                  ;; 1. File Naming Convention
                  "%(concat (format-time-string \"%Y%m%d%H%M%S-\") (org-hugo-slug ${title}) \".org\")"
                  
                  ;; 2. File Header (Crucial for ox-hugo)
                  "%(concat
                   \"#+TITLE: ${title}\\n\"
                   \"#+HUGO_SECTION: notes\\n\"  
                   \"#+HUGO_TAGS: public\\n\"
                   \"#+HUGO_PUBLISH_DATE: %U\\n\"
                   \"#+HUGO_LASTMOD: %u\\n\"
                   \"#+ROAM_KEY: ${id}\\n\"
                   )"
                  )
         :unnarrowed t)))


;; 4. Org-roam-UI for visualization (Ensure this block remains intact)
(use-package org-roam-ui
  :ensure t
  :after org-roam
  :config
  (org-roam-ui-mode +1) 
  (setq org-roam-ui-follow t)
  (setq org-roam-ui-update-on-save t)
  )

;; 5. Optional Keybind (usually handled by Doom's :lang org +roam)
(map! :leader
      :desc "Org-roam node find" "n r f" #'org-roam-node-find)
  ;; Customize the server port if needed
  ;; (setq org-roam-ui-server-port 35901)
  
