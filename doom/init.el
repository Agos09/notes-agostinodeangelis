;;; init.el -*- lexical-binding: t; -*-

;; This file controls what Doom modules are enabled and what order they load
;; in. Remember to run 'doom sync' after modifying it!

;; NOTE Press 'SPC h d h' (or 'C-h d h' for non-vim users) to access Doom's
;;      documentation. There you'll find a link to Doom's Module Index where all
;;      of our modules are listed, including what flags they support.

;; NOTE Move your cursor over a module's name (or its flags) and press 'K' (or
;;      'C-c c k' for non-vim users) to view its documentation. This works on
;;      flags as well (those symbols that start with a plus).
;;
;;      Alternatively, press 'gd' (or 'C-c c d') on a module to browse its
;;      directory (for easy access to its source code).

(doom! :input
       ;;bidi              ; (tfel ot) thgir etirw uoy gnipleh
       ;;chinese
       ;;japanese
       ;;layout            ; auie,ctsrnm is the superior home row

       :completion
       ;;company           ; the ultimate code completion backend
       (corfu +orderless)  ; complete with cap(f), cape and a flying feather!
       ;;helm              ; the *other* search engine for love and life
       ;;ido               ; the other *other* search engine...
       ;;ivy               ; a search engine for love and life
       vertico           ; the search engine of the future

       :ui
       ;;deft              ; notational velocity for Emacs
       doom              ; what makes DOOM look the way it does
       doom-dashboard    ; a nifty splash screen for Emacs
       ;;doom-quit         ; DOOM quit-message prompts when you quit Emacs
       ;;(emoji +unicode)  ; 🙂
       hl-todo           ; highlight TODO/FIXME/NOTE/DEPRECATED/HACK/REVIEW
       ;;indent-guides     ; highlighted indent columns
       ;;ligatures         ; ligatures and symbols to make your code pretty again
       minimap           ; show a map of the code on the side
       modeline          ; snazzy, Atom-inspired modeline, plus API
       ;;nav-flash         ; blink cursor line after big motions
       ;;neotree           ; a project drawer, like NERDTree for vim
       ophints           ; highlight the region an operation acts on
       (popup +defaults)   ; tame sudden yet inevitable temporary windows
       ;;smooth-scroll     ; So smooth you won't believe it's not butter
       ;;tabs              ; a tab bar for Emacs
       treemacs          ; a project drawer, like neotree but cooler
       ;;unicode           ; extended unicode support for various languages
       (vc-gutter +pretty) ; vcs diff in the fringe






       vi-tilde-fringe   ; fringe tildes to mark beyond EOB
       ;;window-select     ; visually switch windows
       workspaces        ; tab emulation, persistence & separate workspaces
       ;;zen               ; distraction-free coding or writing

       :editor
       (evil +everywhere); come to the dark side, we have cookies
       file-templates    ; auto-snippets for empty files
       fold              ; (nigh) universal code folding
       ;;(format +onsave)  ; automated prettiness
       ;;god               ; run Emacs commands without modifier keys
       ;;lispy             ; vim for lisp, for people who don't like vim
       ;;multiple-cursors  ; editing in many places at once
       ;;objed             ; text object editing for the innocent
       ;;parinfer          ; turn lisp into python, sort of
       ;;rotate-text       ; cycle region at point between text candidates
       snippets          ; my elves. They type so I don't have to
       ;;word-wrap         ; soft wrapping with language-aware indent

       :emacs
       dired             ; making dired pretty [functional]
       electric          ; smarter, keyword-based electric-indent
       eww               ; the internet is gross
       ;;ibuffer           ; interactive buffer management
       undo              ; persistent, smarter undo for your inevitable mistakes
       vc                ; version-control and Emacs, sitting in a tree

       :term
       ;;eshell            ; the elisp shell that works everywhere
       ;;shell             ; simple shell REPL for Emacs
       ;;term              ; basic terminal emulator for Emacs
       vterm             ; the best terminal emulation in Emacs

       :checkers
       syntax              ; tasing you for every semicolon you forget
       ;;(spell +flyspell) ; tasing you for misspelling mispelling
       ;;grammar           ; tasing grammar mistake every you make

       :tools

      (org               ; Org-mode features
       +attach           ; Attach files to notes
       +babel            ; Execute code blocks
       +roam2)           ; <<< Make sure this is present for Org-roam V2


       (use package denote)
       ;;ansible
       biblio            ; Writes a PhD for you (citation needed)
       ;;collab            ; buffers with friends
       ;;debugger          ; FIXME stepping through code, to help you add bugs
       ;;direnv
       ;;docker
       ;;editorconfig      ; let someone else argue about tabs vs spaces
       ;;ein               ; tame Jupyter notebooks with emacs
       (eval +overlay)     ; run code, run (also, repls)
       lookup              ; navigate your code and its documentation
       ;;lsp               ; M-x vscode
       magit             ; a git porcelain for Emacs
       ;;make              ; run make tasks from Emacs
       ;;pass              ; password manager for nerds
       pdf               ; pdf enhancements
       ;;terraform         ; infrastructure as code
       ;;tmux              ; an API for interacting with tmux
       ;;tree-sitter       ; syntax and parsing, sitting in a tree...
       ;;upload            ; map local to remote projects via ssh/ftp
       (doom-nib anki)

       :os
       ;;(:if (featurep :system 'macos) macos)  ; improve compatibility with macOS
       ;;tty               ; improve the terminal Emacs experience

       :lang
       ;;agda              ; types of types of types of types...
       beancount         ; mind the GAAP
       ;;(cc +lsp)         ; C > C++ == 1
       ;;clojure           ; java with a lisp
       ;;common-lisp       ; if you've seen one lisp, you've seen them all
       ;;coq               ; proofs-as-programs
       ;;crystal           ; ruby at the speed of c
       ;;csharp            ; unity, .NET, and mono shenanigans
       ;;data              ; config/data formats
       ;;(dart +flutter)   ; paint ui and not much else
       ;;dhall
       ;;elixir            ; erlang done right
       ;;elm               ; care for a cup of TEA?
       emacs-lisp        ; drown in parentheses
       ;;erlang            ; an elegant language for a more civilized age
       ;;ess               ; emacs speaks statistics
       ;;factor
       ;;faust             ; dsp, but you get to keep your soul
       ;;fortran           ; in FORTRAN, GOD is REAL (unless declared INTEGER)
       ;;fsharp            ; ML stands for Microsoft's Language
       ;;fstar             ; (dependent) types and (monadic) effects and Z3
       ;;gdscript          ; the language you waited for
       ;;(go +lsp)         ; the hipster dialect
       ;;(graphql +lsp)    ; Give queries a REST
       ;;(haskell +lsp)    ; a language that's lazier than I am
       ;;hy                ; readability of scheme w/ speed of python
       ;;idris             ; a language you can depend on
       ;;json              ; At least it ain't XML
       ;;(java +lsp)       ; the poster child for carpal tunnel syndrome
       ;;javascript        ; all(hope(abandon(ye(who(enter(here))))))
       ;;julia             ; a better, faster MATLAB
       ;;kotlin            ; a better, slicker Java(Script)
       latex             ; writing papers in Emacs has never been so fun
       lean              ; for folks with too much to prove
       ledger            ; be audit you can be
       ;;lua               ; one-based indices? one-based indices
       markdown          ; writing docs for people to ignore
       ;;nim               ; python + lisp at the speed of c
       ;;nix               ; I hereby declare "nix geht mehr!"
       ;;ocaml             ; an objective camel
       org               ; organize your plain life in plain text
       (org +roam)        ; Ensures org and the org-roam features are loaded
       (org +hugo)       ; ensure hugo is loaded
       ;;php               ; perl's insecure younger brother
       ;; plantuml          ; diagrams for confusing people more
       ;;graphviz          ; diagrams for confusing yourself even more
       ;;purescript        ; javascript, but functional
       ;;python            ; beautiful is better than ugly
       ;;qt                ; the 'cutest' gui framework ever
       ;;racket            ; a DSL for DSLs
       ;;raku              ; the artist formerly known as perl6
       ;;rest              ; Emacs as a REST client
       ;;rst               ; ReST in peace
       ;;(ruby +rails)     ; 1.step {|i| p "Ruby is #{i.even? ? 'love' : 'life'}"}
       ;;(rust +lsp)       ; Fe2O3.unwrap().unwrap().unwrap().unwrap()
       ;;scala             ; java, but good
       ;;(scheme +guile)   ; a fully conniving family of lisps
       sh                ; she sells {ba,z,fi}sh shells on the C xor
       ;;sml
       ;;solidity          ; do you need a blockchain? No.
       ;;swift             ; who asked for emoji variables?
       ;;terra             ; Earth and Moon in alignment for performance.
       ;;web               ; the tubes
       yaml              ; JSON, but readable
       ;;zig               ; C, but simpler

       :email
       (mu4e +org +gmail)
       ;;notmuch
       ;;(wanderlust +gmail)

       :app
       calendar
       ;;emms
       ;;everywhere        ; *leave* Emacs!? You must be joking
       ;;irc               ; how neckbeards socialize
       (rss +org)        ; emacs as an RSS reader

       :config
       emacs-lisp     ; Allows for the elisp setup required
       ;;literate
       (default +bindings +smartparens))



;(use-package! calfw-gcal)

;; --- Protocol  Function ---

;;;
;;;
;; --- Timestamp Function ---
(defun my-protocol-get-timestamp ()
  "Get the current timestamp in a specific format."
  (format-time-string "%Y%m%d-%H%M%S"))

;; --- Protocol Function ---
(defvar protocol-last-number 0 "The last protocol number used.") ; Global variable

(defun generate-protocol-number-with-timestamp (prefix)
  "Generates a protocol number with the given prefix and timestamp."
  (let* ((timestamp (my-protocol-get-timestamp))
         (new-number (1+ protocol-last-number)) ; Define new-number here
         (formatted-number (format "%05d" new-number)) ; Pad with zeros
         (protocol-number (concat prefix timestamp "-" formatted-number)))
    (setq protocol-last-number new-number)
    protocol-number))

(defun insert-protocol-number-with-prefix-and-timestamp (prefix)
  "Inserts a protocol number with the given prefix and timestamp at the cursor."
  (interactive "sEnter prefix: ") ; Prompt for the prefix
  (let ((protocol-number (generate-protocol-number-with-timestamp prefix)))
    (insert protocol-number)))

;; --- Key Binding (Optional) ---
;; You might want to bind this new function to a key
(global-set-key (kbd "C-c a p n") 'insert-protocol-number-with-prefix-and-timestamp)


;;; Denote Configuration

;(use-package! denote)
;; Set your base denote directory on the cloud.
;; We'll decide on the specific path later after discussing cloud options.
(setq denote-directory "~/kDrive/2_Areas/denote")


;; Configure the allowed metadata keys. This is crucial for tags and project IDs.
(setq denote-known-keywords
      '("title"         ; Standard title
        "tags"          ; For your tags
        "project"       ; For your project ID
        "created"       ; Automatically added creation timestamp
        "modified"))     ; Automatically added modification timestamp

;; Define the prefixes for your files.
;; Here, %t will insert the title, %k the keywords (tags), and %p the project.
(setq denote-file-name-format
      '((:type . org)
        (:format . "%t--%k--%p")
        (:function . denote-org-file-name)))

(setq denote-file-name-format-functions
      '((org . denote-org-file-name)))

(defun denote-org-file-name (title keywords project)
  (let ((base (concat title
                       (when keywords (format "--%s" (mapconcat #'identity keywords "--")))
                       (when project (format "--%s" project)))))
    (replace-regexp-in-string "[[:space:]]+" "-" (downcase base))))

;; Optional: Configure how Denote handles different file types.
(setq denote-default-type "org") ;; I recommend org-mode for its tagging and properties.

;; Optional: Automatically add date to the filename if you prefer.
;; (setq denote-file-name-format '((:type . org) (:format . "%Y%m%d-%H%M%S--%t--%k--%p")))

;; For easy tag and project ID entry when creating new notes.
(setq denote-prompter 'completing-read) ;; Use Emacs' built-in completion. You could also use 'ivy' or 'vertico' if you have them.

(setq denote-add-keywords-function
      (lambda (prompt initial)
        (completing-read prompt denote-known-keywords
                         nil t initial nil nil)))

(setq denote-add-project-function
      (lambda (prompt initial)
        (completing-read prompt (mapcar #'identity '("project1" "project2" "important")) ; Add some example projects
                         nil t initial nil nil)))

;; To quickly find notes by keywords or project.
; (require 'denote)
(define-key global-map (kbd "C-c n f") 'denote-find-note)
(define-key global-map (kbd "C-c n k") 'denote-find-by-keyword)
(define-key global-map (kbd "C-c n p") 'denote-find-by-project)



;;; feed rss
; (require 'elfeed)

; (setq elfeed-feeds
  ;     '(
    ;     "https://theregister.com/headlines.atom"
      ;   "https://www.iptechblog.com/category/europe/feed/"
        ; "https://www.techcentral.ie/feed/"
;         "ipwatchdog.com/feed" :title "Ip watchdog"
  ;       "patentlyo.com/feed"
    ;     "https://feeds.feedburner.com/theipkat"
      ;   "https://www.managingip.com/rssfeeds/ip-strategy"
        ; "https://www.managingip.com/rssfeeds/patents"
;         "https://www.managingip.com/rssfeeds/copyright"
  ;       "https://www.managingip.com/rssfeeds/trademarks"
    ;     "https://www.iprights.it/feed/"
      ;   "https://pluralistic.net/feed/"
        ; "https://www.wipo.int/pct/en/newslett/rss.xml"
;         "https://www.wipo.int/econ_stat/en/economics/rss.xml"
  ;       "https://www.wipo.int/patentscope/en/news/pctdb/rss.xml"
    ;     "https://www.wipo.int/wipo_magazine/en/rss"
      ;   "https://www.wipo.int/news/en/wipolex/rss.xml"
        ; "https://wipo.taleo.net/careersection/feed/joblist.rss?lang=en&portal=101430233&searchtype=3&f=null&s=2|A&a=null&multiline=true")
;       )


;; Optional: Configure the update interval (in seconds)
; (setq elfeed-update-interval 3600) ;; Update every hour

;; Optional: Configure the directory where elfeed stores its data
; (setq elfeed-db-directory "~/.elfeed.db")

;; Optional: Automatically update feeds on startup
; (add-hook 'after-init-hook 'elfeed-update)

;; Optional: Keybinding to open the elfeed dashboard
; (global-set-key (kbd "C-x g") 'elfeed)
; ```

;; Ensure MELPA is added to your package archives if not already
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(use-package djvu)

;; Install anki-editor and its dependencies
(unless (package-installed-p 'anki-editor)
  (package-refresh-contents)
  (package-install 'anki-editor))

;; Basic configuration for anki-editor
(use-package anki-editor
  :ensure t ; Ensure it's installed if not already
  :after org ; Load after org-mode is loaded
  :config
  ;; Optional: Create decks in Anki automatically if they don't exist
  (setq anki-editor-create-decks t)

  ;; Optional: Default deck and note type
  (setq anki-editor-default-deck "My Default Deck")
  (setq anki-editor-default-note-type "Basic")

  ;; Optional: Customize default tags
  ;; (setq anki-editor-default-tags '("emacs" "org-mode"))

  ;; Set up keybindings (example)
  (define-key org-mode-map (kbd "C-c a n") 'anki-editor-insert-note)
  (define-key org-mode-map (kbd "C-c a p") 'anki-editor-push-notes)
  (define-key org-mode-map (kbd "C-c a c") 'anki-editor-cloze-dwim)
  (define-key org-mode-map (kbd "C-c a d") 'anki-editor-delete-notes)
  )


; (use-package copilot
;   :ensure t
;   :hook (prog-mode . copilot-mode)
;   :bind (("c-c c a" . copilot-accept-completion)
;          ("c-c c p" . copilot-previous-completion)
;          ("c-c c n" . copilot-next-completion)
;          ("c-c c c" . copilot-chat)
;          ("c-c c s" . copilot-accept-completion-by-word)
;          ("c-c c d" . copilot-accept-completion-by-line)))

