# configuration org roam


Org-roam and Denote are two popular Emacs packages for non-hierarchical, plain-text note-taking (often referred to as a Zettelkasten or Personal Wiki).

While both help you manage a large collection of notes and easily link them, they are built on fundamentally different architectural principles.

Here is a comparison of their key features and philosophies:

Feature	Org-roam	Denote
Core Philosophy	Networked Thought (Zettelkasten). Focuses on creating a web of interconnected files and headings (nodes).	Structured Files. Focuses on a simple, predictable, and robust file-naming scheme for organization.
Architecture	Database-Centric. Uses an SQLite database (built into Emacs 29+) to index all files, links, titles, and tags.	File-Centric (Database-less). Relies solely on the file system and file-naming conventions.
Node Granularity	File and Heading. Can treat an entire Org file OR a specific headline within a file as a standalone node in the network.	File Only. A note is always a single file.
Identifier/Linking	Uses the standard Org-mode id: link type and a UUID (a long string of numbers/letters) stored in the file's properties drawer (:ID:).	Uses a custom denote: link type and a simple, date-based ID from the filename.
File Naming	Flexible. Does not enforce a strict naming scheme, but the default template uses a slug of the title and the date.	Strictly Enforced. Files are named: TIMESTAMP--TITLE--TAG1_TAG2.org. This makes it highly portable and searchable via simple file-system tools.
Search Experience	Feature-Rich & Fast. The database is optimized for quick searching and filtering by title, tag, alias, and backlinks via commands like org-roam-node-find.	Simple & Tool-Agnostic. Searches are typically done via Dired commands or external Emacs packages like consult-notes that leverage the structured filename.
Visualizations	Native Graph View. Integrates with tools like org-roam-ui to generate a dynamic, interactive visual graph of your note network (a major selling point).	No Native Graph. Requires third-party tools or scripts to generate a graph based on links.
File Format Support	Org-mode Only. Fully integrated into the Org-mode ecosystem.	Any Format. The naming scheme works for .org, .md, .txt, .pdf, etc., allowing for management of all files, not just notes.

Export to Sheets

Key Takeaways for Choosing
Choose Org-roam if:

Your primary goal is to build a highly interconnected network of ideas (a Zettelkasten).

You want the ability to make headlines into atomic nodes.

You value visualizing the relationships between your notes (the graph view).

You prefer a feature-rich, integrated search experience driven by a database.

Choose Denote if:

You prefer minimalism and simplicity with no external database dependency.

You value the longevity and portability of a strictly enforced file-naming scheme (the name contains the date, title, and tags).

You want a system that can organize non-Org files (like PDFs or Markdown) using the same structure.

You find the idea of Org-mode file IDs and properties drawers unnecessary or intrusive.

Note: It is possible to use both packages together by configuring your org-roam-capture-templates to use Denote's file-naming convention while still having Org-roam manage the IDs and the database for graph and backlink features.

The most common way to use Org-roam and Denote together is to leverage Org-roam's database, visualization, and linking features while adopting Denote's clean, date-and-keyword-based file-naming scheme.

This configuration requires setting up Denote's directory and file type, and then customizing Org-roam's capture template to conform to Denote's file-naming convention.

1. Prerequisite Setup
Ensure you have both packages installed and configured with the same directory for notes.

Code snippet

;; ----------------------------------------
;; 1. Denote Configuration
;; ----------------------------------------
(use-package denote
  :init
  ;; Set the directory where ALL your notes will live
  (setq denote-directory (expand-file-name "~/Notes/Roam/"))
  
  ;; Ensure files are created as Org-mode files
  (setq denote-file-type 'org)

  ;; Configure the front matter to include the Org-roam ID property
  ;; This ensures Org-roam can index the file.
  ;; %1$s: Title, %2$s: Date, %3$s: Keywords, %4$s: Identifier
  (setq denote-org-front-matter
        (concat
         ":properties:\n"
         ":ID: %4$s\n" ; Crucial for Org-roam to find the node
         ":END:\n"
         "#+title: %1$s\n"
         "#+date: %2$s\n"
         "#+filetags: %3$s\n\n"))

  :config
  (setq denote-prompts '(title keywords)) ; Prompt for Title and Keywords
  (setq denote-infer-keywords t)
  )

;; ----------------------------------------
;; 2. Org-roam Configuration
;; ----------------------------------------
(use-package org-roam
  :after denote ; Ensure Denote is loaded first
  :init
  ;; Crucially, set Org-roam to use the SAME directory as Denote
  (setq org-roam-directory denote-directory)
  
  :config
  ;; Enable automatic sync for the database
  (org-roam-db-autosync-mode)

  ;; BINDINGS (Optional but recommended)
  (define-key global-map (kbd "C-c n f") #'org-roam-node-find)
  )
2. The Hybrid Capture Template
For a truly seamless experience, you want to use Org-roam's command (org-roam-node-find) to create a new note, but have that new note be named using Denote's convention.

You can achieve this by setting Org-roam's capture template to use Denote's file-naming logic.

The example below uses the standard Org-roam capture template but modifies the :target to follow the TIMESTAMP--SLUG__KEYWORDS.org format.

Code snippet

(defun my/denote-org-roam-keywords ()
  "Prompt for keywords and format them for Denote's filename."
  (interactive)
  ;; This prompts for a comma-separated list of keywords and converts them
  ;; into the double-underscore-separated format used by Denote.
  (replace-regexp-in-string
   ",\\s-*" "_"
   (denote-read-keywords)
   t t))

(setq org-roam-capture-templates
      '(("d" "default" plain "%?"
         :target (file+head
                  ;; The Denote Naming Convention: TIMESTAMP--SLUG__KEYWORDS.org
                  ;; The filename is dynamically generated upon capture completion.
                  (concat
                   ;; TIMESTAMP (e.g., 20251019T100141)
                   (format-time-string "%Y%m%dT%H%M%S")
                   "--${slug}"
                   "__%(my/denote-org-roam-keywords)" ; Insert keywords here
                   ".org")
                  
                  ;; The Content for the new file
                  (concat
                   ":properties:\n"
                   ":ID: %<%Y%m%dT%H%M%S>\n" ; Org-roam ID must match the timestamp for consistency
                   ":END:\n"
                   "#+title: ${title}\n"
                   "#+date: [%<%Y-%m-%d %a %H:%M:%S>]\n"
                   "#+filetags: %^G\n\n" ; %^G prompts for tags in the capture buffer
                   "%?"
                   ))
         :immediate-finish t
         :unnarrowed t)))
3. Usage and Benefits
With this hybrid configuration:

Creating a Note:

You call M-x org-roam-node-find.

A capture buffer opens, prompting you for a title and keywords (due to the custom function).

The file is created with the Denote naming scheme (e.g., 20251019T100141--my-new-idea__emacs_config.org).

The file includes the Org-roam ID property, allowing it to be indexed by the database.

Linking Notes:

You use the standard Org-roam command M-x org-roam-node-insert to link between files. This uses the robust id: link type.

Renaming/Refactoring:

You use Denote's renaming commands (M-x denote-rename-file) to safely change the title or keywords, which updates the filename.

Since the Org-roam ID property remains untouched, Org-roam links will not break, and you simply run M-x org-roam-db-sync (or let autosync handle it) to update the database with the new filename/title.

Benefits Gained: You get the best of both:

Org-roam: Backlinks, graph visualization, and advanced querying.

Denote: Clean, portable file-naming scheme and reliable file management tools.

Yes, the Org-roam visualization is highly interactive and clickable, but it requires installing the companion package org-roam-ui (ORUI), which runs a web application alongside Emacs.1You do not interact with the graph directly within the Emacs buffer itself, but through a web browser window.2Interactivity ComparisonFeatureOrg-roam-UI (The Graph)Obsidian Graph ViewNode Click/VisitYes. Clicking a node in the browser will open the corresponding Org file in your Emacs window.Yes. Clicking a node in the graph opens the note in the Obsidian window.Hover PreviewYes. Hovering over a node displays a quick preview of the note's content in the sidebar or a popup.Yes. Hovering over a node previews the note content.Filtering/ColoringYes. Highly customizable filtering and coloring based on tags, number of links, centrality, file type, and more.Yes. Robust filtering by tags, file names, folders, and connection type, with color coding.Graph ContextLocal View: You can select a node and instantly switch to a local view showing only its immediate neighbors (and further degrees of separation).Local Graph: Available, showing the connections around the currently active note.Real-time UpdatesYes. The graph updates in real-time via a WebSocket connection as you edit and save files in Emacs.Yes. The graph updates dynamically as you make changes to your vault.How Org-roam's Visualization WorksBackend (Emacs): The main org-roam package runs inside Emacs and maintains the SQLite database of all your nodes, links, and metadata.Frontend (org-roam-ui): This separate package starts a lightweight web server (typically at http://127.0.0.1:35901/).Visualization: The browser renders the graph using web technologies, allowing for smooth, performant, and sophisticated interactive features.Emacs Integration: The web app uses Emacs's built-in server features to send commands back. When you click a node in the browser, ORUI tells Emacs to open that node's file, making the experience seamless.In short, while the implementation is different (browser vs. native application), the end-user experience of using the Org-roam graph is just as interactive and clickable as Obsidian's.


# configuration guide

Configuring a hybrid **Org-roam** (for graphing and linking) and **Denote** (for file naming) setup in Doom Emacs, complete with the **`org-roam-ui`** visualization tool, requires modifications to your `init.el` and `config.el` files.

Here is a step-by-step guide.

-----

## 1\. Enable Required Modules

First, ensure you have the core Org-mode, Org-roam, and Emacs Lisp modules enabled in your `~/.doom.d/init.el` file.

In the `(doom! ...)` block, confirm these lines are present and uncommented:

```emacs-lisp
;; ~/.doom.d/init.el

:lang
(org +roam) ; Ensures org and the org-roam features are loaded

:config
emacs-lisp    ; Allows for the elisp setup required
```

Run `doom sync` in your terminal after making these changes.

-----

## 2\. Configure Org-roam, Denote, and the Hybrid Template

All the configuration logic will go into your `~/.doom.d/config.el` file.

This setup defines the common directory, sets up Denote's file format, and then uses a custom function to make Org-roam's capture template follow the Denote naming scheme.

```emacs-lisp
;; ~/.doom.d/config.el

;; ----------------------------------------
;; A. Shared Directory & Denote Setup
;; ----------------------------------------

;; 1. Define the directory where all notes will be stored.
(setq org-roam-directory (expand-file-name "~/Notes/Roam/"))
(setq denote-directory org-roam-directory)

;; 2. Set Denote's file type to org.
(setq denote-file-type 'org)

;; 3. Crucially, set the Org front matter to include the Org-roam ID.
;; This allows Org-roam's database to find and index the files created by Denote.
;; %4$s is the unique identifier (timestamp in this case).
(setq denote-org-front-matter
      (concat
       ":PROPERTIES:\n"
       ":ID: %4$s\n"
       ":END:\n"
       "#+title: %1$s\n"
       "#+date: %2$s\n"
       "#+filetags: %3$s\n\n"))

;; 4. Set which prompts Denote will ask for.
(setq denote-prompts '(title keywords))
(setq denote-infer-keywords t)

;; ----------------------------------------
;; B. Hybrid Org-roam Capture Logic
;; ----------------------------------------

;; Function to get keywords for the Denote filename format.
(defun my/denote-org-roam-keywords ()
  "Prompt for keywords and format them for Denote's filename: tag1_tag2."
  (interactive)
  (let ((keywords (denote-read-keywords)))
    (if keywords
        (replace-regexp-in-string ",\\s-*" "_" keywords t t)
      "")))

;; Set Org-roam's capture template to use the Denote file-naming scheme.
(setq org-roam-capture-templates
      '(("d" "denote-note" plain "%?"
         :target (file+head
                  ;; The Denote Naming Convention: TIMESTAMP--SLUG__KEYWORDS.org
                  (concat
                   (format-time-string "%Y%m%dT%H%M%S")
                   "--${slug}"
                   "__%(my/denote-org-roam-keywords)"
                   ".org")
                  
                  ;; The actual content template uses the front matter defined above
                  (concat
                   ":PROPERTIES:\n"
                   ":ID: %<%Y%m%dT%H%M%S>\n" ; ID is the same timestamp for consistency
                   ":END:\n"
                   "#+title: ${title}\n"
                   "#+date: [%<%Y-%m-%d %a %H:%M:%S>]\n"
                   "#+filetags: %^G\n\n" ; %^G prompts for tags in the capture buffer
                   "%?"))
         :immediate-finish t
         :unnarrowed t)))

;; ----------------------------------------
;; C. Org-roam UI (Graph Visualization) Setup
;; ----------------------------------------

(use-package org-roam-ui
  :ensure t
  :after org-roam
  :config
  ;; Enable org-roam-ui when Emacs starts
  (org-roam-ui-mode +1) 
  
  ;; Configure to follow the active node in Emacs
  (setq org-roam-ui-follow t)
  
  ;; Real-time update
  (setq org-roam-ui-update-on-save t)
  
  ;; Customize the server port if needed
  ;; (setq org-roam-ui-server-port 35901)
  )
```

-----

## 3\. Usage Guide

After adding the configuration above and restarting Emacs, you can start using your hybrid setup:

### A. Create a New Note (Hybrid Flow)

1.  Use the Org-roam command to create a node: **`M-x org-roam-node-find`** (or its bound shortcut, typically **`SPC n r f`** in Doom).
2.  Type the **Title** of your new note.
3.  The custom function will prompt you for **Keywords**. Enter them separated by commas (e.g., `emacs, doom, config`).
4.  The note will be created with the **Denote naming convention**:
      * Example Filename: `20251019T100720--config-guide__emacs_doom_config.org`
      * This file is immediately indexed by the Org-roam database for linking and graphing.

### B. Visualize the Graph

1.  Start the Org-roam UI server: **`M-x org-roam-ui-open`**.
2.  This command will **open your default web browser** to the graph view.
3.  The graph is highly interactive:
      * **Click a node** in the graph to **open the corresponding Org file in Emacs**.
      * **Hover over a node** to see a preview of its contents.
      * Use the controls to filter by **tags**, **file type**, or switch to a **local view** around a specific node.
      
      That is an excellent question that highlights the core difference between **visual canvas tools** like Heptabase and **text-centric tools** like Org-roam.

While the Org-roam/Denote setup doesn't have a direct "drag-and-drop grouping" feature like Heptabase's Whiteboard sections, you can create the functional equivalent of "groups of ideas in a block" using several methods in Org-mode and Org-roam.

-----

## Org-roam Methods for Grouping Ideas

In Org-roam, grouping is achieved through structured text and database queries rather than spatial arrangement.

### 1\. The Map of Content (MOC) File

The most robust way to create a thematic grouping is by establishing a **Map of Content (MOC)** file, which functions identically to a Heptabase Whiteboard that organizes many related cards.

  * **How it Works:** You create a new Org-roam file named after the complex topic (e.g., `20251019T101500--evolutionary-psychology-MOC__concepts.org`). This file acts as your **"block"** or organizational hub.
  * **Grouping:** You organize the file by creating **Org headlines** (e.g., `* Core Principles`, `* Criticisms`, `* Key Researchers`). Under each headline, you simply insert **Org-roam links** to all the relevant atomic notes.
  * **Analogy:** The MOC file is the **Whiteboard**, and the links are the **cards arranged into sections**.

### 2\. The Headline Node (Sub-Grouping)

Since Org-roam can treat any **Org headline** as a separate node in the database, you can use these headlines to define sub-groups within a single MOC file.

  * **Benefit:** If you link to the headline node `[[ID-of-MOC::*Core Principles]]`, the link is precise. In the `org-roam-ui` graph, this specific headline will appear as a distinct node, showing its connections separate from the main MOC file.

### 3\. Dynamic Blocks (Query-Based Grouping)

For groups defined by metadata (like all notes with a certain tag), you can use Org-mode's powerful query features to create a **dynamic, automatically updated block** within your MOC file. This is the **most direct analog to filtering/grouping** in Heptabase.

| Heptabase Action | Org-roam Action (Dynamic Block) |
| :--- | :--- |
| **Filter by Tag** | Create an Org dynamic block using an `org-roam-query` to pull all notes with a specific tag. |
| **Example Code:** | `#+BEGIN: org-roam-query :tag "epigenetics"`  <br> `<List of relevant links is automatically generated here>`  <br> `#+END:` |

When you execute this query (`C-c C-x C-u`), Org-roam fills the block with a list of linked notes that meet your criteria. This block then becomes your curated, up-to-date group.

-----

## Visualizing Grouped Structure

When using **`org-roam-ui`**, these grouping techniques become visible:

1.  **MOC Files** often become **high-centrality nodes** (large, well-connected) in the graph, making the structure of the complex topic immediately obvious.
2.  You can **color-code** nodes by **tag** to visually delineate your groups, even if they aren't directly linked to a MOC.
3.  The **Org-roam buffer** (`SPC n r b`) next to your current MOC file will instantly show you all the notes that link *to* it (the atomic notes you are grouping), confirming your connections.

In essence, you substitute Heptabase's spatial organization with Org-mode's **textual and programmatic organization**, gaining automation and structured rigor in return for losing the free-form canvas feel.
