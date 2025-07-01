(setq org-directory "~/org/org"
      org-agenda-files '("~/org/org/code.org" "~/org/org/emacs.org" "~/org/org/interpretability.org"
                         "~/org/org/projects.org" "~/org/org/reading.org" "~/org/org/reading_misc.org"
                         "~/org/org/research.org" "~/org/org/research_projects.org" "~/org/org/robot.org"
                         "~/org/org/students.org" "~/org/org/teaching.org"
                         "~/org/org/work.org" "~/org/org/ets.org")
      org-default-notes-file "~/org/org/refile.org"
      org-agenda-start-day "-1d"
      org-agenda-span 10
      org-agenda-start-on-weekday nil
      org-duration-format '((special . h:mm))
      org-agenda-window-setup 'other-window
      org-agenda-restore-windows-after-quit t
      org-use-fast-todo-selection 'expert
      my/org-research-files '("~/org/org/research.org" "~/org/org/projects.org"
                              "~/org/org/reading.org" "~/org/org/students.org"
                              "~/org/org/interpretability.org" "~/org/org/reading_misc.org")
      util/org-collect-headings-files my/org-research-files
      util/org-insert-link-always-add-refs t
      org-imgs-dir (f-join org-directory "imgs"))

(defvar my/org-pdfs-dir nil
  "Where pdfs linked in org files are kept.")

(defvar my/org-new-books-dir nil
  "Where new ebooks are stored by default.")

(setq org-adapt-indentation t)

(setq auto-mode-alist
      (a-assoc (a-dissoc auto-mode-alist "\\.org\\'") "\\.org\\'" 'org-mode))

(defun org-agenda-and-misc ()
  (-concat org-agenda-files '("~/org/org/misc.org")))

;; org-todo-keywords
(setq org-todo-keywords
      (quote ((sequence "TODO(t)" "IN_PROGRESS(p)" "CURRENT(c)" "|" "FINISHED(f!)" "TAKEAWAY(w)")
              (sequence "QUEUED(q!)" "ALMOST_DONE(a)" "KOMATOSE(k)" "|" "REASSIGNED(s!)" "DEAD(d!)")
              (sequence "IDEA(i)" "|" "IMPLEMENTED(l!)" "FAIL(x!)")
              (sequence "CHECKOUT(o)" "READ(r)" "|" "NOUSE(u!)" "MEH(m!)" "NICE(n!)" "CRAP(h!)"))))

;; org todo key faces
(setq org-todo-keyword-faces
      (quote (("TODO" :foreground "red" :weight bold)
              ("IN_PROGRESS" :foreground "turquoise4" :weight bold)
              ("IDEA" :foreground "blue" :weight bold)
              ("CURRENT" :foreground "blue" :weight bold)
              ("READ" :foreground "purple" :weight bold)
              ("MEH" :foreground "lawn green" :weight bold)
              ("NEXT" :foreground "#00bfff" :weight bold)
              ("NICE" :foreground "forest green" :weight bold)
              ("CRAP" :foreground "yellow green" :weight bold)
              ("CHECKOUT" :foreground "cyan" :weight bold)
              ("FINISHED" :foreground "forest green" :weight bold)
              ("NOUSE" :foreground "yellow green" :weight bold)
              ("REASSIGNED" :foreground "yellow green" :weight bold)
              ("IMPLEMENTED" :foreground "forest green" :weight bold)
              ("FAIL" :foreground "yellow green" :weight bold)
              ("QUEUED" :foreground "orange" :weight bold)
              ("ALMOST_DONE" :foreground "magenta" :weight bold)
              ("TAKEAWAY" :foreground "blue" :weight bold)
              ("KOMATOSE" :foreground "grey" :weight bold)
              ("DEAD" :foreground "yellow green" :weight bold))))

(setq org-capture-templates
  (quote
   (("r" "Templates for Research stuff")
    ("rr" "File a paper/entry for reading" entry (file+olp+datetree "~/org/org/refile.org")
     "* READ %? :research:\n%U\n%a\n" :clock-in t :clcok-resume t)
    ("rt" "Other Research TODO item " entry (file+olp+datetree "~/org/org/refile.org")
     "* TODO %? :research:\n%U\n%a\n" :clock-in t :clcok-resume t)
    ("i" "idea" entry (file+olp+datetree "~/org/org/refile.org")
     "* IDEA %? %^g\n%U\n%a\n" :clock-in t :clcok-resume t)
    ("j" "journal entry" entry (file+olp+datetree "~/org/org/refile.org")
     "* DIARY %?\n%U\n%a\n" :clock-in t :clcok-resume t)
    ("w" "todo work" entry (file+olp+datetree "~/org/org/refile.org")
     "* TODO %? :work:%^g\n%U\n%a\n" :clock-in t :clcok-resume t)
    ("a" "adtech" entry (file+olp+datetree "~/org/org/refile.org")
     "* TODO %? :work:adtech:%^g\n%U\n%a\n" :clock-in t :clcok-resume t)
    ("s" "todo students" entry (file+olp+datetree "~/org/org/refile.org")
     "* TODO %? :students:\n%U\n%a\n" :clock-in t :clcok-resume t)
    ("m" "todo misc" entry (file+olp+datetree "~/org/org/refile.org")
     "* TODO %? :misc:\n%U\n%a\n" :clock-in t :clcok-resume t)
    ("p" "paper to read" entry (file+olp+datetree "~/org/org/refile.org")
     "* READ %? \n%U\n%a\n" :clock-in t :clcok-resume t))))

;; Exclude table from fontify
(setq org-protecting-blocks '("src" "example" "export" "table"))
(add-to-list 'org-structure-template-alist '("t" . "table"))
(add-to-list 'org-structure-template-alist '("p" . "src python"))
(add-to-list 'org-structure-template-alist '("i" . "src ipython"))
(add-to-list 'org-structure-template-alist '("b" . "src bash"))
(add-to-list 'org-structure-template-alist '("L" . "src lisp"))
(add-to-list 'org-structure-template-alist '("r" . "src rust"))

;; Fontify source blocks
(setq org-src-fontify-natively t)

;; Use the current window for indirect buffer display
(setq org-indirect-buffer-display 'current-window)

;; Use full outline paths for refile targets - we file directly with IDO
(setq org-refile-use-outline-path t)

;; Targets complete directly with IDO
(setq org-goto-interface 'outline-path-complition)
(setq org-outline-path-complete-in-steps nil)

;; Emphasis alist
(setq org-emphasis-alist
      '(("*" bold) ("/" italic) ("_" underline) ("=" org-verbatim verbatim)
        ("~" markdown-inline-code-face verbatim) ("+" (:strike-through t))))

;; Hide ** // etc
(setq org-hide-emphasis-markers nil)
(setq org-link-file-path-type 'absolute)
(setq org-agenda-dim-blocked-tasks nil)

;; Customizations to org-lists
(setq org-list-allow-alphabetical t)
(setq org-list-demote-modify-bullet '(("-" . "+") ("+" . "*") ("*" . "-")))

;; Don't use actual image width
(setq org-image-actual-width nil)

;; use ido completion
(setq org-completion-use-ido t)

;; Increase imenu depth. Was 2 originally and 5 later.
(setq org-imenu-depth 10)

;; Allow refile to create parent tasks with confirmation
(setq org-refile-allow-creating-parent-nodes 'confirm)

;; org clock settings
(setq org-clock-persist 'history)
(org-clock-persistence-insinuate)
(setq org-clock-history-length 24)


;; Babel
;; NOTE: `ditaa' converts ascii to diagrams http://ditaa.sourceforge.net/
(setq org-babel-load-languages '((emacs-lisp . t) (python . t) (ditaa . t)))
(setq org-babel-python-command "python3")
(setq org-ditaa-jar-path "/usr/share/java/ditaa.jar")
(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t) (python . t) (ditaa . t)))
