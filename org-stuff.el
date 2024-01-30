;; TODO Add functions for parsing research TODO, CHECKOUT, READ etc.
;; based on certain timestamps and properties on how they're imported
;; global org variables files
(load "~/emacs_config/org-vars.el")

(require 'util/org "util-org")
(require 'util/helm-org "util-helm-org")
(unless (or util/org-collect-buffers
            util/org-collect-files-modtimes)
  (util/org-collect-setup))

;; Global org related key bindings
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "C-c C-x C-j") 'org-clock-goto)
(global-set-key (kbd "C-c l j") 'org-store-link)
(global-set-key (kbd "M-g h") 'my/helm-org-headings)
;; keybindings for org-mode

;; Local bindings for `org-mode'. Modifies `org-mode-map'
(defun org-mode-keyset-hook ()
  (smartscan-mode -1)
  (define-key org-mode-map (kbd "M-n") #'my/org-forward-symbol-heading-or-link)
  (define-key org-mode-map (kbd "M-p") #'my/org-backward-symbol-heading-or-link)
  (define-key org-mode-map (kbd "C-c C-h") #'my/helm-org-headings)
  (define-key org-mode-map (kbd "C-c C-n") #'my/org-next-heading-or-error)
  ;; (define-key org-mode-map (kbd "C-c C-l") nil)
  (define-key org-mode-map (kbd "C-c C-k") nil)
  (define-key org-mode-map (kbd "C-c C-k C-k") #'my/org-copy-link)
  (define-key org-mode-map (kbd "C-c C-k C-p") #'my/org-copy-link-to-pdf-if-present)
  (define-key org-mode-map (kbd "C-c l") nil)
  (define-key org-mode-map (kbd "C-c l l") #'my/linkify-words-as-files)
  (define-key org-mode-map (kbd "C-c l d") #'my/linkify-words-as-files-with-description)
  (define-key org-mode-map (kbd "C-c l p") #'my/linkify-pdf-org-words-as-files)
  (define-key org-mode-map (kbd "C-c l i") #'org-insert-link)
  (define-key org-mode-map (kbd "C-c l f") #'ref-man-org-insert-full-heading)
  (define-key org-mode-map (kbd "C-c l s") #'ref-man-org-insert-short-heading)
  (define-key org-mode-map (kbd "C-c l c") #'ref-man-org-insert-citation)
  (define-key org-mode-map (kbd "C-c l r") #'ref-man-org-insert-reference)
  (define-key org-mode-map (kbd "C-c l h") #'my/org-insert-link-to-heading)
  ;; (define-key org-mode-map (kbd "C-c b") nil)
  (define-key org-mode-map (kbd "C-c b") #'my/org-bibtex-insert-headline-as-bib-to-file)
  ;; (define-key org-mode-map (kbd "C-c b b") 'org-iswitchb)
  (define-key org-mode-map (kbd "C-c s") nil)
  (define-key org-mode-map (kbd "C-c s k") #'my/org-kill-subtree)
  (define-key org-mode-map (kbd "C-c s w") #'org-copy-subtree)
  (define-key org-mode-map (kbd "C-c s a") #'util/org-kill-new-or-append-subtree)
  (define-key org-mode-map (kbd "C-c t") #'org-todo)
  (define-key org-mode-map (kbd "C-c <") #'my/org-insert-or-update-today)
  (define-key org-mode-map (kbd "C-c .") #'my/org-time-stamp-inactive)
  (define-key org-mode-map (kbd "C-c ;") #'my/org-code)
  (define-key org-mode-map (kbd "C-c C-x C-c") #'org-clock-cancel)
  (define-key org-mode-map (kbd "C-c C-x C-n") #'my/org-next-link)
  (define-key org-mode-map (kbd "C-c C-x C-p") #'my/org-previous-link)
  (define-key org-mode-map (kbd "C-c C-x C-v") #'my/org-toggle-inline-images)
  ;; (define-key org-mode-map (kbd "C-c e") nil)
  ;; (define-key org-mode-map (kbd "C-c e s") 'ref-man-org-search-heading-on-gscholar-with-eww)
  ;; (define-key org-mode-map (kbd "C-c e c") 'org-search-heading-on-crossref-with-biblio)
  (define-key org-mode-map (kbd "C-c !") #'org-time-stamp)
  (define-key org-mode-map (kbd "C-c f") nil)
  (define-key org-mode-map (kbd "C-c f f") #'my/restart-flycheck)
  (define-key org-mode-map (kbd "C-c f a") #'auto-fill-mode)
  (define-key org-mode-map (kbd "C-c f g") #'org-fragtog-mode)
  (define-key org-mode-map (kbd "C-c k") nil)
  (define-key org-mode-map (kbd "C-c k s") #'org-kanban/shift)
  (define-key org-mode-map (kbd "C-c k i") #'org-kanban//initialize-mirrored-kanban-at-point))


(defun my/org-occur-bold-keywords ()
  (interactive)
  (let* ((case-fold-search t)
         (words (mapcar 'downcase '("TODO" "CHECK" "Problems" "NOTE" "Idea" "Question" "Method" "Quote" "Roadmap" "Regularization" "Deferred" "IDEA" "However" "note" "IMPORTANT" "UPDATE")))
         ;; (regexp (format "\\*\\(?:\\[\\)?\\(%s\\)\\(?:\\]\\)?\\*"
         ;;                 (string-join (-concat words (mapcar 'upcase words)) "\\|")))
         (regexp (format "\\*\\(?:\\[\\)?\\(%s\\)\\(?:\\]\\)?\\*"
                         (string-join words "\\|")))
         result)
    ;; NOTE: In case we just want to return the values
    ;; (save-excursion
    ;;   (goto-char (point-min))
    ;;   (while (re-search-forward regexp nil t)
    ;;     (push (substring-no-properties (match-string 0)) result)))
    ;; result
    (occur regexp)))

(defun my/org-table-calc-all-TBLFM ()
  (when (org-at-TBLFM-p)
    (org-table-calc-all-TBLFM)
    t))
(add-hook 'org-ctrl-c-ctrl-c-hook 'my/org-table-calc-all-TBLFM)

;; TODO: Consolidate all such 'org-apply-to-subtree or similar functions
;; Got annoyed at missing links from references
(defun my/org-get-references-not-in-list (prop)
  "Get list of heading properties in subtree or DOC_ROOT if not in list under point.

A plist of of :heading and :custom-id is returned.

DOC_ROOT is determined with `util/org-get-tree-prop'.

If DOC_ROOT then headings under \"References\" are checked
regardless of whether they have property PROP.

In any case org subtree headings which have PROP are also gathered.

Difference between subtree+references and list are returned.

See also `util/org-copy-subtree-elems-with-property' and
`util/org-copy-subtree-elems'."
  (save-excursion
    (let* ((doc-root (util/org-get-tree-prop "DOC_ROOT"))
           (level (if doc-root
                      (save-excursion
                        (goto-char doc-root)
                        (org-current-level))
                    (org-current-level)))
           (have-references (util/save-mark-and-restriction
                             (when doc-root
                               (goto-char doc-root))
                             (org-narrow-to-subtree)
                             (re-search-forward
                              (concat "^" "\\*\\{" (format "%s" (+ 1 level)) "\\}" " References$") nil t)))
           (link-re util/org-fuzzy-or-custom-id-link-re)
           (el (save-excursion
                 (beginning-of-line)
                 (skip-chars-forward " \r\t\n")
                 (org-element-context)))
           (list-items (-filter (lambda (x) (string-equal (nth 2 x) "- "))
                                (plist-get (cadr el) :structure)))
           (list-links (progn
                         (unless list-items
                           (user-error "Not at an org list"))
                         (mapcar (lambda (item)
                                   (let ((bounds `(,(car item) ,(-last-item item))))
                                     (goto-char (car bounds))
                                     (beginning-of-line)
                                     (re-search-forward link-re (cadr bounds) t)
                                     (substring-no-properties (match-string 1))))
                                 list-items)))
           (same-file-links (-filter (lambda (x)
                                     (let ((path (car (split-string x "::"))))
                                       (string-match "\\(:?file:\\)?\\(.+\\)" path)
                                       (f-equal? (match-string 2 path) (buffer-file-name))))
                                     list-links))
           (subtree-refs (util/save-mark-and-restriction
                          (outline-back-to-heading)
                          (org-narrow-to-subtree)
                          (let (refs)
                            (while (outline-next-heading)
                              (when (org-entry-get (point) prop)
                                (let ((heading (substring-no-properties (org-get-heading t t t t)))
                                      (cid (org-entry-get (point) "CUSTOM_ID")))
                                  (push `(:heading ,heading :custom-id ,cid) refs))))
                            refs)))
           (references (when have-references
                         (util/save-mark-and-restriction
                          (goto-char have-references)
                          (org-narrow-to-subtree)
                          (let ((case-fold-search t))
                            (util/org-apply-to-subtree-headings
                             (lambda ()
                               (let ((heading (substring-no-properties (org-get-heading t t t t)))
                                     (cid (org-entry-get (point) "CUSTOM_ID")))
                                 `(:heading ,heading :custom-id ,cid)))
                             nil)))))
           (need-links (when (or have-references subtree-refs)
                         (-filter (lambda (x)
                                    (not (-any
                                          (lambda (y)
                                            (let ((link (cadr (split-string y "::"))))
                                              (if (string-prefix-p "#" link)
                                                  (string= (string-remove-prefix "#" link) (plist-get x :custom-id))
                                                (string= (string-remove-prefix "*" link) (plist-get x :heading)))))
                                          same-file-links)))
                                  (-concat subtree-refs references)))))
      (list (or subtree-refs have-references) need-links))))

(defun my/org-copy-references-not-in-list ()
  (interactive)
  (pcase-let ((`(,have-links ,links) (my/org-get-references-not-in-list "PAPERID")))
    (unless have-links
      (user-error "No references separately or in subtree found in current subtree or DOC"))
    (if (not links)
        (message "Could not generate any new list of links")
      (kill-new
       (format "[[file:%s::#%s][%s]]\n"
               (buffer-file-name)
               (plist-get (car links) :custom-id)
               (plist-get (car links) :heading)))
      (seq-do
       (lambda (x)
         (kill-append
          (format "[[file:%s::#%s][%s]]\n"
                  (buffer-file-name)
                  (plist-get x :custom-id)
                  (plist-get x :heading))
          nil))
       (cdr links))
      (message "Copied %s links" (length links)))))

;; For flycheck mode
(defun my/org-next-heading-or-error ()
  (interactive)
  (if flycheck-mode
      (condition-case nil
          (flycheck-next-error)
        (error (org-next-visible-heading 1)))
    (org-next-visible-heading 1)))


;; Insert link to heading
(defun my/org-insert-link-to-heading ()
  (interactive)
  (util/org-insert-link-to-heading 'identity))


;; Convenience functions to jump across PDF_FILE links
(defun my/org-next-link ()
"Like `org-next-link' but goto PDF link wihout prefix arg."
  (interactive)
  (if current-prefix-arg
      (when (re-search-forward org-link-any-re)
        (backward-char)
        (org-reveal))
      (when (re-search-forward " +:PDF_FILE: +\\[.+]\\|\\[file://.+pdf]\\|\\[/home/.+pdf]\\|\\[~/.+pdf]")
        (org-reveal))))

(defun my/org-previous-link ()
  "Like `org-previous-link' but goto PDF link wihout prefix arg."
  (interactive)
  (if current-prefix-arg
      (when (re-search-backward org-link-any-re)
        (forward-char)
        (org-reveal))
    (when (re-search-backward " +:PDF_FILE: +\\[.+]\\|\\[file://.+pdf]\\|\\[/home/.+pdf]\\|\\[~/.+pdf]")
      (org-reveal))))

(defun my/org-code (&optional delimiter)
  "Surround current word with code block delimiter ~."
  (interactive)
  (pcase-let ((`(,beg . ,end) (my/bounds-of-word-at-point))
              (delimiter (or delimiter "~")))
    (cond ((region-active-p)
           (goto-char beg)
           (insert delimiter)
           (goto-char (1+ end))
           (insert delimiter))
          ((thing-at-point 'symbol)
           (goto-char beg)
           (insert delimiter)
           (goto-char (1+ end))
           (insert delimiter))
          ((thing-at-point 'whitespace)
           (insert delimiter delimiter)
           (backward-char)))))

(defun my/org-toggle-inline-images (&optional include-linked)
  "Toggle the display of inline images.

This is a hacked version of `org-toggle-inline-images' with
INCLUDE-LINKED always t.

INCLUDE-LINKED is passed to `org-display-inline-images'."
  (interactive "P")
  (if org-inline-image-overlays
      (progn
	(org-remove-inline-images)
	(when (called-interactively-p 'interactive)
	  (message "Inline image display turned off")))
    (org-display-inline-images t)
    (when (called-interactively-p 'interactive)
      (message (if org-inline-image-overlays
		   (format "%d images displayed inline"
			   (length org-inline-image-overlays))
		 "No images to display inline")))))

(defun my/org-kill-subtree (prefix-arg)
  (interactive "p")
  (org-copy-subtree prefix-arg t))

(if (not (file-exists-p (expand-file-name (concat my/emacs-libdir "/" "grammarly"))))
    (warn "grammarly not found")
  (add-to-list 'load-path (expand-file-name (concat my/emacs-libdir "/" "grammarly")))
  (use-package request
    :ensure t
    :defer t)
  (use-package websocket
    :ensure t
    :defer t)
  (require 'grammarly)
  (require 'flycheck-grammarly)
  (flycheck-grammarly-setup))

(defun my/restart-flycheck ()
  (interactive)
  (if current-prefix-arg
      (if flycheck-mode
          (flycheck-mode -1)
        (flycheck-mode 1))
    (flycheck-mode -1)
    (flycheck-mode 1)))

;; `org-dir' key bindings
(add-to-list 'load-path (concat my/emacs-libdir "/" "org-dir"))
(require 'org-dir)
(defun org-dir-load-hook ()
  (require 'org-dir)
  (define-key org-mode-map (kbd "C-c o") nil)
  (define-key org-mode-map (kbd "C-c o d") 'org-dir-insert-dirs-from-path)
  (define-key org-mode-map (kbd "C-c o s") 'org-dir-insert-subdirs-from-heading)
  (define-key org-mode-map (kbd "C-c o f") 'org-dir-insert-files-from-heading))

;; Find or create today's date in work
(defvar my/org-daily-log-file
  (f-join org-directory "work.org")
  "File where daily logs may be kept.")

(defun my/org-find-today (&optional when)
  "Find or create today's date in `my/org-daily-log-file'.

Optionally insert date after asking WHEN if a prefix-argument is
given."
  (interactive)
  (let ((buf (find-file my/org-daily-log-file))
        (when (if current-prefix-arg
                  (read-from-minibuffer "When? " "now")
                "now")))
    (with-current-buffer buf
      (goto-char (point-min))
      (re-search-forward (regexp-quote "* Daily"))
      (org-datetree-find-date-create
       (org-date-to-gregorian
        (org-read-date t nil when))
       'subtree-at-point)
      (org-reveal))))

;; Custom convenience search functions for `org-link-search'
(add-to-list 'org-execute-file-search-functions
             #'util/org-execute-simple-regexp-search)
(add-to-list 'org-execute-file-search-functions
             #'util/org-execute-customid-or-max-heading-match-search)
(setq util/org-execute-search-prefix-arg-behaviour-alist
      '((4 . util/org-execute-search-find-pdf-file)
        (16 . util/org-execute-search-other-window)))

;; `util/org-insert-citation-to-heading' behaviour
(setq util/org-citation-function 'util/short-heading-with-authors)

(defun my/org-sort-entries (prop asc)
  "Sort entries by for numeric property PROP in descending order.
With a \\[universal-argument] sort in ascending order."
  (org-sort-entries
   nil (if asc ?F ?f)
   (lambda () (or (org-entry-get (point) prop) "0"))
   (lambda (x y) (> (string-to-number x) (string-to-number y)))))


(defun my/org-sort-entries-citations ()
  "Sort entries by citationcount in descending order.
With a \\[universal-argument] sort in ascending order."
  (interactive)
  (my/org-sort-entries "CITATIONCOUNT" current-prefix-arg))

(defun my/org-sort-entries-year ()
  "Sort entries by year in descending order.
With a \\[universal-argument] sort in ascending order."
  (interactive)
  (my/org-sort-entries "YEAR" current-prefix-arg))

(defalias 'my/org-insert-or-update-today 'my/insert-or-update-today)

(defvar my/org-fuzzy-or-custom-id-link-re
  (rx "[" "["
      (seq (group (opt
                   (opt "file:")
                   (opt "//")
                   (or "/" "~")
                   (regexp ".+?::")))
           (group (or "*" "#") (+? any)))
      "]" "[" (group (+? any)) "]" "]")
  "Regexp for matching org fuzzy or custom-id link.

Unlike `util/org-fuzzy-or-custom-id-link-re' it has three groups.
First matches filename if present.
Second matches the link.
Third matches the description.")

(defun my/org-forward-symbol-or-link ()
  (interactive)
  (if (org-at-heading-p)
      (my/org-search-link-to-heading)
    (unless (my/org-search-for-link)
      (smartscan-symbol-go-forward))))

(defun my/org-backward-symbol-or-link ()
  (interactive)
  (if (org-at-heading-p)
      (my/org-search-link-to-heading t)
    (unless (my/org-search-for-link t)
      (smartscan-symbol-go-backward))))

(defun my/org-goto-symbol-heading-or-link-from-link (&optional backward)
  (let ((maybe-link (save-excursion
                      (my/org-search-for-link backward t)))
        (maybe-heading (save-excursion
                         (my/org-search-heading-from-link backward))))
    (cond ((and maybe-link maybe-heading)
           (goto-char (if backward (max maybe-link maybe-heading)
                        (min maybe-link maybe-heading))))
          ((or maybe-link maybe-heading)
           (goto-char (or maybe-link maybe-heading)))
          ((and (not (org-at-heading-p)) (not (eq (org-element-type (org-element-context)) 'link))
                (thing-at-point 'symbol))
           (if backward
               (smartscan-symbol-go-backward)
             (smartscan-symbol-go-forward)))
          (t nil))))

(defun my/org-forward-symbol-heading-or-link ()
  (interactive)
  (unless (if (org-at-heading-p)
              (my/org-search-link-to-heading)
            (my/org-goto-symbol-heading-or-link-from-link))
    (message "At last match")))

(defun my/org-backward-symbol-heading-or-link ()
  (interactive)
  (unless (if (org-at-heading-p)
              (my/org-search-link-to-heading t)
            (my/org-goto-symbol-heading-or-link-from-link t))
    (message "At first match")))

(defun my/org-search-heading-from-link (&optional backward)
  "Search for a heading from a link at point.

If optional BACKWARD is non-nil then search backward, else search
forward.  A single \\[universal-argument] prefix switches
BACKWARD to t."
  (interactive)
  (let* ((backward (or backward current-prefix-arg))
         (el (org-element-context))
         (at-link (eq (org-element-type el) 'link))
         (link (and at-link
                    (not (my/org-maybe-http-url el))
                    (pcase-let ((`(,beg . ,end) (util/org-get-bracket-link-bounds)))
                      (goto-char beg)
                      (looking-at util/org-fuzzy-or-custom-id-link-re)
                      (-last-item (split-string (match-string 1) "::")))))
         (regexp (and link (if (string-prefix-p "#" link)
                               (concat " *?:CUSTOM_ID: *?" (string-remove-prefix "#" link))
                             (concat "\\*+ \\(?:[a-zA-Z]+ \\)?" (string-remove-prefix "*" link)))))
         (case-fold-search t))
    (when regexp
      (if backward
          (let ((found (re-search-backward regexp nil t)))
            (when found
              (org-reveal)
              (outline-back-to-heading)
              (point)))
        (let ((found (re-search-forward regexp nil t)))
          (when found
            (org-reveal)
            (outline-back-to-heading)
            (point)))))))


(defun my/org-search-for-link (&optional backward noerror)
  "Search for a link at point.

If optional BACKWARD is non-nil then search backward, else search
forward.  A single \\[universal-argument] prefix switches
BACKWARD to t.

Optional NOERROR is passed on to `re-search-forward' and
`re-search-backward'."
  (interactive)
  (let* ((backward (or backward current-prefix-arg))
         (el (org-element-context))
         (at-link (eq (org-element-type el) 'link))
         (link-regexp (and at-link
                           (not (my/org-maybe-http-url el))
                           (pcase-let ((`(,beg . ,end) (util/org-get-bracket-link-bounds)))
                             (goto-char beg)
                             (looking-at util/org-fuzzy-or-custom-id-link-re)
                             ;; HACK: Dirty hack to remove file:\|~/\|/home/etc abberviations
                             ;;       We assume org files in same directory
                             (prog1 (f-filename (match-string 1))
                               (unless backward
                                 (goto-char end))))))
         (regexp (and link-regexp (regexp-quote link-regexp)))
         (case-fold-search t))
    (when regexp
      (if backward
          (let ((found (re-search-backward regexp nil noerror)))
            (prog1 found
              (when found (org-reveal))))
        (let ((found (re-search-forward regexp nil noerror)))
          (prog1 found
            (when found (org-reveal))))))))

(defun my/org-search-link-to-heading (&optional backward)
  "Search forward for a link to current heading in buffer.

With a prefix argument or non-nil optional BACKWARD, search
backwards."
  (interactive)
  (let* ((backward (or backward current-prefix-arg))
         (heading (org-get-heading t t t t))
         (custom-id (org-entry-get (point) "CUSTOM_ID"))
         (regexp (if custom-id
                     (concat (regexp-quote (concat "*" heading)) "\\|#" custom-id)
                   (concat (regexp-quote (concat "*" heading)))))
         (case-fold-search t))
    (if backward
        (let ((found (re-search-backward regexp nil t)))
          (prog1 found
            (when found
              (org-reveal))))
        (let ((found (re-search-forward regexp nil t)))
          (prog1 found
            (when found
              (org-reveal)))))))

(defun my/org-get-one-link (shorten)
  "Return org link according to context.

If at a heading, then return link to that.  If on an org internal
link, then return that.  If a region covers multiple
headings (only headings for now and at the same level also) then
return links to those.  If at an HTTP URL, then return that."
  (let* ((el (org-element-context))
         (link (or (my/org-maybe-http-url el t)
                   ;; NOTE: Don't return link org heading when not at the heading
                   ;; (util/org-get-link-to-heading-under-point shorten)
                   (when (org-at-heading-p)
                     (util/org-get-link-to-heading-under-point shorten))
                   (pcase-let ((`(,beg . ,end) (util/org-get-bracket-link-bounds)))
                     (and beg end (buffer-substring-no-properties beg end))))))
    link))

(defun my/org-copy-link (&optional pref-arg)
  "Copy link to something.

Something depends on context.  See `my/org-get-one-link'.

With single \\[universal-argument] append the link to last kill
instead of copying new.

With two \\[universal-argument] pass the flag `shorten' to
`util/org-get-link-to-heading-under-point' if at org heading.

With three \\[universal-argument] both append and shorten if applicable."
  (interactive "p")
  (pcase-let* ((`(,beg ,end) (when (region-active-p) (list (region-beginning) (region-end))))
               (append (pcase pref-arg (1 nil) (4 t) (16 nil) (_ t)))
               (shorten (pcase pref-arg (1 nil) (4 nil) (16 t) (_ t)))
               (link nil))
    (cond ((and beg end)
           (when (save-excursion (goto-char beg) (org-at-heading-p))
             (let ((links (util/org-get-links-to-multiple-headings shorten t)))
               (when mark-active
                 (deactivate-mark))
               (if append
                   (kill-append (concat "\n" (string-join (reverse links) "\n")) nil)
                 (kill-new (string-join (reverse links) "\n"))))))
          ((progn (setq link (my/org-get-one-link shorten)) link)
           (if append
               (progn
                 (kill-append (concat "\n" link) nil)
                 (message "Appended link..."))
             (kill-new link)
             (message "Copied link...")))
          (t (user-error "Cannot do anything here")))))

(defun my/org-copy-link-to-pdf-if-present ()
  "Copy the path to PDF file if present under point.

Element under point can be link or heading."
  (interactive)
  (let* ((link (util/org-link-get-target-for-internal))
         (path (when link (plist-get link :file)))
         (buf (when path (find-file-noselect path)))
         (pt (when path (plist-get link :point))))
    (unless link
      (user-error "Copy link from heading not implemented yet"))
    (if (and buf pt)
        (with-current-buffer buf
          (save-excursion
            (goto-char pt)
            (let* ((pdf-prop (org-entry-get pt "PDF_FILE"))
                   (pdf-file (when (and pdf-prop (string-match util/org-file-link-re pdf-prop))
                               (match-string 1 pdf-prop))))
              (if pdf-file
                  (progn (kill-new (format "%s" pdf-file))
                         (message "Copied path %s" pdf-file))
                (user-error "No PDF file found under link")))))
      (user-error "Could not follow link"))))

(defun my/org-copy-heading-text (&optional pref-arg)
  "Copy an org heading's text.

If at a link to an org heading, then retrieve the text for that
heading."
  (interactive)
  (let ((heading (if (org-at-heading-p)
                     (org-get-heading t t t t)
                   (let* ((link (util/org-link-get-target-for-internal))
                          (path (when link (plist-get link :file)))
                          (buf (when path (find-file-noselect path)))
                          (pt (when path (plist-get link :point))))
                     (when (and buf pt)
                       (save-excursion
                         (with-current-buffer buf
                           (goto-char pt)
                           (org-get-heading t t t t))))))))
    (if heading
        (kill-new heading)
      (user-error "No heading or link here"))))

(defun my/org-find-link-in-targets (file lnk buf-a buf-b)
  (or (when-let ((target file)
                 (found (with-current-buffer (get-file-buffer file)
                          (util/org-execute-customid-or-max-heading-match-search lnk t t t))))
        (list target found))
      (when-let ((target (buffer-file-name buf-a))
                 (found (with-current-buffer buf-a
                          (util/org-execute-customid-or-max-heading-match-search lnk t t t))))
        (list target found))
      (when-let ((target (buffer-file-name buf-b))
                 (found (with-current-buffer buf-b
                          (util/org-execute-customid-or-max-heading-match-search lnk t t t))))
        (list target found))
      (let* ((matches-in-cache (util/org-filter-from-headings-cache
                                nil
                                (lambda (x) (string-match-p
                                             (substring lnk 1)
                                             (if (string-prefix-p "*" lnk)
                                                 (car x)
                                               (nth 3 x))))))
             (prompt (when matches-in-cache
                       "Didn't find link in given file but link exists in cache for files. Choose one: "))
             (selected (when prompt
                         (ido-completing-read prompt
                                              (mapcar (lambda (x)
                                                        (concat (car x) " in " (nth 2 x)))
                                                      matches-in-cache)
                                              nil t)))
             (target (when selected
                       (expand-file-name
                        (car (-filter (lambda (x)
                                        (string-match-p (-last-item
                                                         (split-string selected))
                                                        x))
                                      util/org-collect-headings-files))))))
        (list target target))))

(defun my/org-search-link-backward-subr (&optional no-plist)
  (let* ((beg (match-beginning 0))
         (end (match-end 0))
         (match (substring-no-properties (match-string 1)))
         (link (substring-no-properties (match-string 2)))
         (desc (substring-no-properties (match-string 3)))
         (file (if (not (string-empty-p match))
                   (->> match
                        (string-remove-suffix "::" )
                        (string-remove-prefix "file:")
                        (string-remove-prefix "//")
                        (expand-file-name))
                 match))
         (no-file (string-empty-p file)))
    (if no-plist
        `(,beg ,end ,match ,link ,desc ,file ,no-file)
      `(:beg ,beg :end ,end :match ,match :link ,link
             :desc ,desc :file ,file :no-file ,no-file))))

;; TODO: These should be transferred to `util'
(defun my/org-fix-broken-links (other-buf &optional subtreep)
  "Fix broken org links in current org buffer.

All org links are checked including local links.  If the target
is not found then OTHER-BUF is checked for that link first.  If
the `file' component of that link is non-nil then the current
buffer is also checked for that link and if not found in
OTHER-BUF that the links are searched in
`util/org-filter-from-headings-cache'.

With optional non-nil SUBTREEP, fix in subtree instead
of complete buffer.

Like `my/org-fix-links-to-buf', this function is for fixing links
when a subtree is moved to another file.  This one checks in the
newly created file."
  (let* ((buf-a (current-buffer))
         (buf-b (get-buffer other-buf))
         (file-a (buffer-file-name buf-a))
         (link-re my/org-fuzzy-or-custom-id-link-re)
         links)
    (with-current-buffer buf-a
      (util/save-mark-and-restriction
       (when subtreep
         (org-narrow-to-subtree))
       (goto-char (point-max))
       (while (re-search-backward link-re nil t)
         (unless (eq (save-match-data (org-element-type (org-element-context))) 'src-block)
           (push (my/org-search-link-backward-subr) links)))
       (mapc (lambda (link)
               (pcase-let* ((no-file (plist-get link :no-file))
                            (beg (plist-get link :beg))
                            (file (if no-file file-a (plist-get link :file)))
                            (lnk (plist-get link :link))
                            (`(,target ,found) (my/org-find-link-in-targets file lnk buf-a buf-b))
                            (broken (not (string= file target))))
                 (when broken
                   (if found
                       (if no-file
                           (my/replace-once lnk (concat "file:" target  "::" lnk) beg)
                         (my/replace-once (plist-get link :match) (concat "file:" target  "::") beg))
                     (error "Couldn't find link for %s anywhere" lnk)))))
             (reverse links))
       nil))))

(defun my/org-fix-links-to-buf (buf-a buf-b)
  "Fix links from org buffer BUF-A to org buffer BUF-B.

Check for broken links in BUF-A with either:

1. `file' org link is in BUF-A but actually points to BUF-B.
2. fuzzy or custom-id org link with no `file' prefix but is in BUF-B.

The function is for fixing links when a subtree is moved to
another file and there can be broken links.  This function would
run on the original file and check if any broken links are
referenced in the file which now exist in the new file."
  (let ((file-a (buffer-file-name buf-a))
        (file-b (buffer-file-name buf-b))
        (link-re my/org-fuzzy-or-custom-id-link-re))
    (with-current-buffer buf-a
      (save-excursion
        (save-restriction
          (goto-char (point-max))
          (while (re-search-backward link-re nil t)
            (unless (eq (save-match-data (org-element-type (org-element-context))) 'src-block)
              (pcase-let* ((`(,beg ,end ,match ,link ,desc ,file ,no-file)
                            (my/org-search-link-backward-subr t))
                           (link-to-current-file (or no-file (string= file file-a))))
                (when link-to-current-file
                  (let* ((match-in-buf-a (with-current-buffer buf-a
                                          (util/org-execute-customid-or-max-heading-match-search link t t t)))
                         (match-in-buf-b (unless match-in-buf-a
                                           (with-current-buffer buf-b
                                             (util/org-execute-customid-or-max-heading-match-search link t t t)))))
                    (cond ((and match-in-buf-a file (string= file file-b))
                           (my/replace-once match "" beg)
                           (message "Replaced %s at point %s in buffer %s" link beg (buffer-name buf-a)))
                          (match-in-buf-b
                           (if (string-empty-p file)
                               (my/replace-once link (concat "file:" file-b  "::" link) beg)
                             (if (string= file file-a)
                                 (my/replace-once (concat match link) (concat "file:" file-b  "::" link) beg)
                               (warn "Found match in %s but link points to " file-b file)))
                           (message "Replaced %s at point %s in buffer %s" link beg (buffer-name buf-a)))
                          (nil t))))))))))))

(defun my/replace-once (string replacement &optional beg)
  (save-excursion
    (let ((case-fold-search nil))       ; match case
      (when beg
        (goto-char beg))
      (search-forward string nil t)
      (delete-region (match-beginning 0) (match-end 0))
      (insert replacement))))

(defun my/org-fix-links-between-buffers ()
  "Fix links between two org buffers.

First search and fix links in `current-buffer' then other buffer
which is asked at a prompt."
  (interactive)
  (let ((other-buf (ido-completing-read "Other org buffer: "
                                      (mapcar #'buffer-name (-filter
                                                             (lambda (x)
                                                               (eq 'org-mode
                                                                   (with-current-buffer x major-mode)))
                                                             (buffer-list))))))
    (my/org-fix-links-to-buf (current-buffer) (get-buffer other-buf))
    (my/org-fix-links-to-buf (get-buffer other-buf) (current-buffer))))

(defun my/org-fix-links (files)
  "Given a list of files, search for and fix broken links."
  ;; say if a subtree was detached from a file-a.org
  ;; and made into a new file file-b.org
  ;; While detaching, for all headings in file-al.org
  ;; 1. check if any file in org files references that heading and fix
  ;; 2. Fix local links from file-a.org that point to file-b.org
  ;; 3. Fix local links from file-b.org that point to file-a.org
  )


(defun my/org-search-first-custom-id-or-fuzzy-link (str)
  "Return the point for first occurence of fuzzy or custom-id link."
  (let* ((case-fold-search t)
         (words (split-string
                 (string-remove-prefix "*"
                                       (replace-regexp-in-string "file:.+reading.org::" "" str))
                 " "))
         (todo-comment-re (if (derived-mode-p 'org-mode)
                              (format "\\(?:%s\\|%s\\)?" org-todo-regexp org-comment-string)
                            (format "\\(?:%s\\)?" org-comment-string)))
         (custom-id-re (when (pcase (string-match-p "^#[a-zA-Z0-9_-]+$" str)
                               (0 t)
                               (_ nil))
                         (concat " *?:CUSTOM_ID: *?" (string-remove-prefix "#" str))))
         (title (regexp-quote (string-join words " ")))
         (title-re
          (format "^\\*+ \\(?:\\(?:%s\\) \\)?\\(%s\\)\\(?: +:.+:\\)?$"
                  todo-comment-re title))
         (cookie-re "\\[[0-9]*\\(?:%\\|/[0-9]*\\)\\]")
         (comment-re org-comment-regexp))
    (save-excursion
      (goto-char (point-min))
      (when (re-search-forward (or custom-id-re title-re) nil t)
        (point)))))

(defun my/org-get-all-paperids ()
  "Get all \"PAPERID\" property from org entries where \"PAPERID\" property exists.

This uses `util/org-filter-from-headings-cache'."
  (-keep (lambda (ent)
           (with-current-buffer (get-buffer (nth 2 ent))
             (when-let ((paperid (org-entry-get (nth 4 ent) "PAPERID")))
               paperid)))
         (util/org-filter-from-headings-cache
          nil
          (lambda (x)
            (string-match-p "[a-zA-Z]+?[0-9]\\{4\\}[a-zA-Z]+" (nth 3 x))))))

;; NOTE: Wrote this function thinking to separate the reading_plan to a separate file
(defun my/org-convert-subtree-to-separate-file ()
  "Convert a subtree to a separate org file.

Care has to be taken so that there are no broken links.  For that
each link in the subtree that points to not in subtree must be
changed and vice versa.

In addition org file: links and local org links without the file:
prefix must also be handled correctly in both files."
  (let* ((bounds (save-restriction
                   (org-narrow-to-subtree)
                   `(,(point-min) . ,(point-max))))
         local-links file-links)
    ;; TODO: subtree links which point to subtree should point become local
    ;; TODO: subtree links which point outside subtree should point to current file
    ;; TODO: non-subtree links which point to subtree should point to new file
    (goto-char (point-min))
    (while (re-search-forward util/org-fuzzy-or-custom-id-link-re nil t)
      (when (string-match-p ".+reading.org::.+" (match-string 1))
        (push `(,(substring-no-properties (match-string 1)) . ,(point)) file-links))
      (when (string-match-p "^[#\\*].+" (match-string 1))
        (push `(,(substring-no-properties (match-string 1)) . ,(point)) local-links)))
    (list (length file-links) (length local-links))
    (seq-do (lambda (str)
              (let ((pt (my/org-search-first-custom-id-or-fuzzy-link str)))
                (when (and pt (< (car bounds) pt) (< pt (cdr bounds)))
                  (debug))))
            (mapcar 'car file-links))))

(defun my/move-pdf-to-docs-dir (call-method)
  "Move file under point to docs dir.
Also copy the new file path.

Works with `dired-mode', `pdf-view-mode' and `org-mode'."
  (interactive "p")
  (let ((fname
         (pcase major-mode
           ('org-mode
            (util/org-move-file-under-point call-method ref-man-documents-dir))
           ('dired-mode (dired-get-filename))
           ('pdf-view-mode (buffer-file-name))
           (_ (message "Nothing to do here") nil))))
    (when (and fname
               (y-or-n-p
                (format "Move %s to %s? " fname ref-man-documents-dir)))
      (dired-rename-file fname (f-join ref-man-documents-dir
                                       (f-filename fname))
                         nil))
    (kill-new (format "[[%s]]" (f-join ref-man-documents-dir (f-filename fname))))))

(defun my/move-pdf-to-new-books-dir (call-method)
  "Move file under point to docs dir.
Also copy the new file path.

Works with `dired-mode', `pdf-view-mode' and `org-mode'."
  (interactive "p")
  (let ((target-dir my/org-new-books-dir)
        (fname
         (pcase major-mode
           ('org-mode
            (util/org-move-file-under-point call-method target-dir))
           ('dired-mode (dired-get-filename))
           ('pdf-view-mode (buffer-file-name))
           (_ (message "Nothing to do here") nil))))
    (when (and fname
               (y-or-n-p
                (format "Move %s to %s? " fname target-dir)))
      (dired-rename-file fname (f-join target-dir (f-filename fname))
                         nil))
    (kill-new (format "[[%s]]" (f-join target-dir (f-filename fname))))))

(defun my/org-insert-pdf-from-docs-dir ()
  "Insert a pdf file from docs-dir as an org link."
  (interactive)
  (util/with-org-mode
   (let* ((files (directory-files ref-man-documents-dir))
          (prompt "File to insert: ")
          (file (ido-completing-read "File: " (-slice files 2)))
          (link (format "[[%s][%s]]" (f-join ref-man-documents-dir file) file)))
     (if current-prefix-arg
         (insert link)
       (org-set-property "PDF_FILE" link)))))

(defun my/org-search-dups (arg)
  "Search for duplicate custom-id and fuzzy links.

Current heading and custom-id if it exists is used to search and
displayed in `occur' buffer."
  (interactive "p")
  (let* ((cid-re (when (org-entry-get (point) "CUSTOM_ID")
                   (concat "^ *?:CUSTOM_ID: *?" (org-entry-get (point) "CUSTOM_ID"))))
         (todo-comment-re (if (derived-mode-p 'org-mode)
                              (format "\\(?:%s\\|%s\\)?" org-todo-regexp org-comment-string)
                            (format "\\(?:%s\\)?" org-comment-string)))
         (title-re (format "^\\*+ \\(?:\\(?:%s\\) \\)?\\(%s\\)$" todo-comment-re
                           (substring-no-properties (org-get-heading t t t t))))
         (case-fold-search t)
         (regexp (string-join (-filter 'identity (list cid-re title-re)) "\\|"))
         (bufs (mapcar (lambda (x)
                         (find-file-noselect x))
                       my/org-research-files))
         (arg (max arg 5)))
    (occur-1 regexp arg bufs)))

;; TODO Not sure what this does
(defun my/org-filter-publication-props-or-link-in-text (el)
  (let ((el (cadr el)))
    (or (-reduce-from (lambda (x y) (or x (member y '(:AUTHOR :TITLE :PDF_FILE)))) nil el)
        (save-restriction
          (org-narrow-to-subtree)
          (let ((start (point))
                (end (save-excursion (outline-next-heading) (point))))
            (string-match-p "\\[\\[.+]\\[.*]]" (buffer-substring-no-properties start end)))))))
(setq util/org-heading-props-filter-p #'my/org-filter-publication-props-or-link-in-text)


(defun my/org-generate-default-custom-id-subr ()
  "Subroutine for generating default custom-id."
  (let ((existing (org-entry-get (point) "CUSTOM_ID"))
        (heading (substring-no-properties (org-get-heading t t t t)))
        (parent (save-excursion
                  (condition-case nil
                      (progn
                        (outline-up-heading 1)
                        (substring-no-properties (org-get-heading t t t t)))
                    (error "")))))
    ;; When it's only two words also prepend the parent's parent
    (when (<= (length (apply #'-concat (mapcar #'split-string `(,heading ,parent)))) 2)
      (save-excursion
        (outline-up-heading 2)
        (setq parent (concat (substring-no-properties (org-get-heading t t t t))
                             " " parent))))
    (if (or (not existing)
            (y-or-n-p "Replace custom-id at point? "))
        (org-entry-put (point) "CUSTOM_ID" (mapconcat
                                            (lambda (x)
                                              (replace-regexp-in-string
                                               " +" "_"
                                               ;; remove all characters except the following class
                                               (replace-regexp-in-string "[^ a-zA-Z0-9_-]" ""
                                                                         (downcase x))))
                                            (list parent heading)
                                            "_"))
      (message "Not replacing existing custom-id"))))

;; TODO: check for existing custom-id if it exists in links in any org files
;;       Need a cache of custom-id's preferably for that.
(defun my/org-generate-default-custom-id ()
  "Generate custom-id for org heading xbased on outline path.

With \\[universal-argument] generate IDs for all subheadings in
subtree one level down."
  (interactive)
  (save-excursion
    (if current-prefix-arg
        (util/org-get-headings-at-level 1 #'my/org-generate-default-custom-id-subr)
      (my/org-generate-default-custom-id-subr))))

(use-package company-bibtex
    :ensure t
    :defer t)
(require 'company-bibtex)
;; NOTE: Change the following also in company-bibtex.el in case it ever updates
(setq company-bibtex-bibliography (f-files "~/org/bibs/"))
(setq bibtex-completion-bibliography (f-files "~/org/bibs/"))
(defconst company-bibtex-pandoc-citation-regex "@"
  "Regex for pandoc citation prefix.")

;; NOTE: Modified company-bibtex
;;       The package itself hasn't been maintained for a while so
;;       placing the function here instead of hacks.el
(defun company-bibtex (command &optional arg &rest ignored)
  "`company-mode' completion backend for bibtex key completion.

This backend activates for citation styles used by `pandoc-mode' (@),
`latex-mode' (\cite{}), and `org-mode' (ebib:), and reads from a
bibliography file or files specified in `company-bibtex-bibliography'.
COMMAND, ARG, and IGNORED are used by `company-mode'."

  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'company-bibtex))
    (prefix (cond ((derived-mode-p 'latex-mode)
		   (company-grab (format "%s\\(%s,\\)*\\(%s\\)"
					 company-bibtex-latex-citation-regex
					 company-bibtex-key-regex
					 company-bibtex-key-regex)
				 2))
		  ((derived-mode-p 'org-mode)
		   (company-grab (format "%s\\(%s,\\)*\\(%s\\)"
					 (rx (or (eval company-bibtex-pandoc-citation-regex)
                                                 (eval company-bibtex-org-citation-regex)))
					 company-bibtex-key-regex
					 company-bibtex-key-regex)
				 2))
		  ((derived-mode-p 'markdown-mode)
		   (company-grab (format "%s\\(%s\\)"
					 company-bibtex-pandoc-citation-regex
					 company-bibtex-key-regex)
				 1))
		  ))
    (candidates
     (cl-remove-if-not
      (lambda (c) (string-prefix-p arg c))
      (company-bibtex-candidates arg)))
    (annotation (company-bibtex-get-annotation arg))
    (meta (company-bibtex-get-metadata arg))
    (duplicates t)))

(use-package cdlatex
  :ensure t
  :defer t)

(add-to-list 'load-path (expand-file-name (concat my/emacs-libdir "/" "org-fragtog")))
(defun my/org-minor-modes ()
  (auto-revert-mode 1)
  (auto-fill-mode 1)
  (org-cdlatex-mode 1)
  (require 'org-fragtog)
  (org-fragtog-mode 1))

(defun my/org-company-settings ()
  (setq-local company-backends company-backends)
  (add-hook 'company-backends #'company-bibtex)
  ;; company-capf gives error when trying to complete on a line after a fuzzy link
  ;; NOTE: Perhaps it's fixed [2021-10-05 Tue 02:48]
  ;; (remove-hook 'company-backends 'company-capf)
  )

(add-hook 'org-mode-hook #'org-mode-keyset-hook)
(add-hook 'org-mode-hook #'my/org-minor-modes)
(add-hook 'org-mode-hook #'my/org-company-settings)

;; Remove useless org-ref-* hooks
(setq org-mode-hook
      (-filter (lambda (x) (if (symbolp x)
                               (not (string-prefix-p "org-ref-" (symbol-name x)))
                             x))
               org-mode-hook))
;; (add-hook 'org-mode-hook 'org-dir-load-hook)

;; Increase scale for inline latex
(plist-put org-format-latex-options :scale 1.4)

(use-package org-pdftools
  :ensure t
  :defer t)
(require 'org-pdftools)
(setq org-file-apps '((auto-mode . emacs)
                      (directory . emacs)
                      ("\\.mm\\'" . default)
                      ("\\.x?html?\\'" . default)
                      ("\\.pdf\\'" . default)
                      ;; ("\\.pdf\\'" . (lambda (file link)
                      ;;                  (org-pdfview-open link)))
                      ("\\.djvu\\'" . "evince %s"
                       ;; NOTE: alternative way
                       ;; (lambda (file link)
                       ;;   (my/org-open-djvu))
                       )))

(use-package org-noter
  :ensure t
  :defer t)
(require 'org-noter)
(add-to-list 'org-noter-notes-search-path (expand-file-name "~/org/"))

(use-package org-kanban
  :ensure t
  :defer t)
(require 'org-kanban)
(defun org-kanban//initialize-mirrored-kanban-at-point ()
  "Create an org-kanban dynamic block at the point."
  (interactive)
  (save-excursion
    (insert "#+BEGIN: kanban :mirrored nil :scope tree\n#+END:\n"))
  (org-ctrl-c-ctrl-c))

;; ====== Org Agenda settings ======
;; (setq org-agenda-compact-blocks nil)

;; For the next operation.
;; 1. For every todo add a "next" property
;; 2. Ensure there are no more than one "next" at the depth
;; 3. Show entries with parents with "next" at lowest depth

;; (defun org-check-misformatted-subtree ()
;;   "Check misformatted entries in the current buffer."
;;   (interactive)
;;   (save-excursion)
;;   (show-all)
;;   (org-map-entries
;;    (lambda ()
;;      (when (and (move-beginning-of-line 2)
;;                 (not (looking-at org-heading-regexp)))
;;        (if (or (and (org-get-scheduled-time (point))
;;                     (not (looking-at (concat "^.*" org-scheduled-regexp))))
;;                (and (org-get-deadline-time (point))
;;                     (not (looking-at (concat "^.*" org-deadline-regexp)))))
;;            (when (y-or-n-p "Fix this subtree? ")
;;              (message "Call the function again when you're done fixing this subtree.")
;;              (recursive-edit))
;;          (message "All subtrees checked."))))))



;; Gives each heading with the value of the NEXT property
;; (defun org-bleh () (interactive)
;;        (mapcar 'message
;;                (org-map-entries
;;                 '(concat (org-get-heading t t) " " (org-entry-get nil "NEXT"))
;;                 nil 'tree)))
;; Gives each entry with level and heading
;; (mapcar (lambda (x) (message (concat (number-to-string (car x)) " " (nth 2 x))))
;;  (org-map-entries 'org-heading-components "//!" nil))

;; Another set of org custom agenda commands
;; (setq org-agenda-custom-commands
;;       '(("O" "Office block agenda"
;;          ((agenda "" ((org-agenda-span 1)))
;;                       ;; limits the agenda display to a single day
;;           (tags-todo "+PRIORITY=\"A\"")
;;           (tags-todo "computer|office|phone")
;;           (tags "project+CATEGORY=\"elephants\"")
;;           (tags "review" ((org-agenda-files '("~/org/org/circuspeanuts.org"))))
;;                           ;; limits the tag search to the file circuspeanuts.org
;;           (todo "WAITING"))
;;          ((org-agenda-compact-blocks t))) ;; options set here apply to the entire block
;;         ;; ...other commands here
;;         ))

(defun org-agenda-subtree-or-region (prefix)
  "Display an agenda view for the current subtree or region.
With prefix, display only TODO-keyword items."
  (interactive "p")
  (let (header)
    (if (use-region-p)
        (progn
          (setq header "Region")
          (put 'org-agenda-files 'org-restrict (list (buffer-file-name (current-buffer))))
          (setq org-agenda-restrict (current-buffer))
          (move-marker org-agenda-restrict-begin (region-beginning))
          (move-marker org-agenda-restrict-end
                       (save-excursion
                         (goto-char (1+ (region-end))) ; If point is at pos 0, include heading on that line
                         (org-end-of-subtree))))
      (progn
        ;; No region; restrict to subtree
        (setq header "Subtree")
        (org-agenda-set-restriction-lock 'subtree)))

    ;; sorting doesn't seem to be working, but the header is
    (let ((org-agenda-sorting-strategy '(priority-down timestamp-up))
          (org-agenda-overriding-header header))
      (org-search-view (if (>= prefix 4) t nil) "*"))
    (org-agenda-remove-restriction-lock t)
    (message nil)))

;; For now, next will search for the next tag in projects
;; Each project is in a separate file
(setq org-agenda-custom-commands
      '((" " "Agenda" ((agenda "Agenda" nil)
                       (tags "//CURRENT"
                                  ((org-agenda-overriding-header "CURRENT Tasks")
                                   (org-tags-match-list-sublevels 'indented)))
                       (tags-todo "NEXT=\"t\""
                                  ((org-agenda-overriding-header "NEXT Tasks")
                                   (org-tags-match-list-sublevels 'indented)))
                       (tags-todo "CATEGORY=\"refile\"/!"
                                  ((org-agenda-overriding-header "To do (and refile)")
                                   (org-tags-match-list-sublevels t)))
                       (tags "//FINISHED|DEAD|REASSIGNED"
                             ((org-agenda-overriding-header "Tasks to Archive")
                              (org-tags-match-list-sublevels 'indented)))))
        ;; ("p" . "Project Classes")
        ("E" "Everything" ((tags-todo "NEXT=\"t\""
                                        ((org-agenda-overriding-header "NEXT Tasks")
                                         (org-tags-match-list-sublevels 'indented)))
                             (tags "CATEGORY=\"refile\"//IDEA"
                                   ((org-agenda-overriding-header "Ideas to refile")
                                    (org-tags-match-list-sublevels nil)))
                             (tags "//IDEA"
                                   ((org-agenda-overriding-header "All Ideas")
                                    (org-tags-match-list-sublevels nil)))
                             (tags "//!"
                                   ((org-agenda-overriding-header "All Project TODOs")
                                    (org-tags-match-list-sublevels nil)))))
        ("R" "Readings" ((tags-todo "NEXT=\"t\""
                                    ((org-agenda-overriding-header "NEXT Tasks")
                                     (org-tags-match-list-sublevels 'indented)))
                         (tags "//IDEA"
                               ((org-agenda-overriding-header "All Ideas")
                                (org-tags-match-list-sublevels nil)))
                         (tags "//!"
                               ((org-agenda-overriding-header "All Project TODOs")
                                (org-tags-match-list-sublevels nil))))
         ((org-agenda-files '("~/org/pubs_org"))))
        ("r" "Research Projects" ((agenda "Research"
                                          ((org-agenda-files
                                            '("~/org/org/research.org" "~/org/org/experiment.org"))
                                           (org-agenda-text-search-extra-files nil)
                                           (org-agenda-use-tag-inheritance nil)
                                           ;; (org-agenda-skip-function '(org-agenda-skip-entry-if 'regexp ))
                                           ))
                                  (tags "//CURRENT"
                                        ((org-agenda-overriding-header "Current Research Tasks")
                                         (org-tags-match-list-sublevels 'indented)
                                         (org-agenda-use-tag-inheritance nil)))
                                  (tags "//IN_PROGRESS"
                                        ((org-agenda-overriding-header "In Progress Tasks")
                                         (org-tags-match-list-sublevels 'indented)
                                         (org-agenda-use-tag-inheritance nil)))
                                  ;; (tags "+research|CATEGORY=\"research\"+NEXT=\"t\""
                                  ;;       ((org-agenda-overriding-header "NEXT Research Tasks")
                                  ;;        (org-tags-match-list-sublevels 'indented)))
                                  ;; (tags "+research|CATEGORY=\"refile\"+research//IDEA"
                                  ;;       ((org-agenda-overriding-header "Research Ideas to refile")
                                  ;;        ;; (org-tags-match-list-sublevels nil)
                                  ;;        ))
                                  (tags "//IDEA"
                                        ((org-agenda-overriding-header "All Research Ideas")
                                         (org-tags-match-list-sublevels 'indented)
                                         (org-agenda-use-tag-inheritance nil)))
                                  (tags "//TODO"
                                        ((org-agenda-overriding-header "All Research TODOs")
                                         (org-tags-match-list-sublevels 'indented)
                                           (org-agenda-use-tag-inheritance nil))))
         ((org-agenda-files'("~/org/org/research.org" "~/org/org/experiment.org"))
          (org-agenda-text-search-extra-files nil)
          (org-agenda-use-tag-inheritance nil)))
        ("w" "Current Work" ((agenda "Current Work"
                                     ((org-agenda-files
                                       '("~/org/org/research.org" "~/org/org/work.org"))
                                      (org-agenda-text-search-extra-files nil)))
                             (tags "//TODO"
                                  ((org-agenda-overriding-header "All Work TODOs")
                                   (org-tags-match-list-sublevels 'indented)))
                             (tags "//CURRENT"
                                  ((org-agenda-overriding-header "Current Work Tasks")
                                   (org-tags-match-list-sublevels 'indented)))
                             (tags "//NEXT"
                                  ((org-agenda-overriding-header "Next Work Tasks")
                                   (org-tags-match-list-sublevels 'indented)))
                             (tags "//IDEA"
                                  ((org-agenda-overriding-header "All Work Ideas")
                                   (org-tags-match-list-sublevels 'indented)))
                             (tags "//READ"
                                  ((org-agenda-overriding-header "Things to Read")
                                   (org-tags-match-list-sublevels 'indented)))
                             (tags "//CHECKOUT"
                                  ((org-agenda-overriding-header "Things to Checkout")
                                   (org-tags-match-list-sublevels 'indented))))
         ((org-agenda-files '("~/org/org/research.org" "~/org/org/work.org"))))
        ("p" "Projects" ((agenda "Projects"
                                     ((org-agenda-files
                                       '("~/org/org/projects.org"))
                                      (org-agenda-text-search-extra-files nil)))
                             (tags "//TODO"
                                  ((org-agenda-overriding-header "All Project TODOs")
                                   (org-tags-match-list-sublevels 'indented)))
                             (tags "//CURRENT"
                                  ((org-agenda-overriding-header "Current Project Tasks")
                                   (org-tags-match-list-sublevels 'indented)))
                             (tags "//NEXT"
                                  ((org-agenda-overriding-header "Next Project Tasks")
                                   (org-tags-match-list-sublevels 'indented)))
                             (tags "//IN_PROGRESS"
                                  ((org-agenda-overriding-header "In Progress Project Tasks")
                                   (org-tags-match-list-sublevels 'indented))))
         ((org-agenda-files '("~/org/org/projects.org"))))
        ("P" "Pubs/PDFs" ((agenda "Pubs entries with PDFs"
                                  ((org-agenda-files
                                    '("~/org/org/projects.org"))
                                   (org-agenda-text-search-extra-files nil)))
                          (tags "PDF_FILE={.*pdf}"
                                ((org-agenda-overriding-header "PDF Files?")
                                 (org-tags-match-list-sublevels 'indented)
                                 (org-agenda-text-search-extra-files nil))))
         ((org-agenda-text-search-extra-files nil)
          (org-agenda-entry-text-mode t)
          (org-agenda-entry-text-maxlines 10)
          (org-agenda-files '("~/org/org/projects.org" "~/org/org/research.org"))))))

;; Only changing the background by certain tags
;; Can customize this function to change by various options like:
;; category, tags,
(defun color-org-header (tag backcolor) ;; forecolor)
  (goto-char (point-min))
  (while (re-search-forward tag nil t)
    (add-face-text-property (point-at-bol) (point-at-eol)
                            `(face (:background, backcolor))))) ;;, :foreground, forecolor)))))

;; ultimately all the tagged items should be refiled under the file
;; names which are the same as the tags
(defun my-org-tags-custom-faces ()
  (save-excursion
    (color-org-header "adtech:" "#ff89bc")
    (color-org-header "research:" "#ffdbef")
    (color-org-header "paperwork:" "#cdcdcd")
    (color-org-header "students:" "#fffacd")
    (color-org-header "emacs:" "#cefbff")
    (color-org-header "misc:" "#f2f2ff")))
    ;; (color-org-header "FINISHED" "#cdffcf")))

(add-hook 'org-agenda-finalize-hook 'my-org-tags-custom-faces)

;; NOTE: Refile Stuff
;;       Adapted from Norang's refile settings
(defun my/org-refile-agenda-files ()
  (interactive)
  (let ((org-refile-targets '((org-agenda-files . (:maxlevel . 5)))))
    (org-refile)))
(defun my/org-refile-pubs-org-files ()
  (interactive)
  (let ((org-refile-targets '((my/pubs-org-files :maxlevel . 2))))
    (org-refile)))
(defun my/org-refile-current-file ()
  (interactive)
  (let ((org-refile-targets '((nil . (:maxlevel . 5))))
        (org-refile-use-outline-path t))
    (org-refile)))
(defun my/org-refile-reading ()
  "Refile to only those org files in which research papers exist."
  (interactive)
  (let ((org-refile-targets '((my/research-files :maxlevel . 5)))
        (org-refile-use-outline-path t)
        (org-outline-path-complete-in-steps t)
        (org-olpath-completing-read
         (lambda (prompt collection &rest args)
           "Read an outline path like a file name."
           (let ((thetable collection))
             (apply #'ido-completing-read
	            prompt
	            (lambda (string predicate &optional flag)
	              (cond
	               ((eq flag nil) (try-completion string thetable))
	               ((eq flag t)
	                (let ((l (length string)))
		          (mapcar (lambda (x)
			            (let ((r (substring x l))
				          (f (if (string-match " ([^)]*)$" x)
					         (match-string 0 x)
				               "")))
			              (if (string-match "/" r)
				          (concat string (substring r 0 (match-end 0)) f)
			                x)))
			          (all-completions string thetable predicate))))
	               ;; Exact match?
	               ((eq flag 'lambda) (assoc string thetable))))
	            args)))))
    (org-refile)))
(setq org-refile-targets (quote ((org-agenda-files :maxlevel . 5))))
(setq org-refile-use-cache t)

;; unwind-protect wrap to make sure the variable is reset
;; Where is this used?
;; Oh I think if I want to use file names for refile or not
(defun my/org-refile ()
  (interactive)
  (setq org-refile-use-outline-path 'file)
  (unwind-protect
      (org-refile)
    (setq org-refile-use-outline-path t)))

(defun my/count-matches (str re)
  (save-match-data
    (let ((count 0)
          (start 0))
      (while (string-match re str start)
        (setq start (match-end 0))
        (setq count (+ count 1)))
      count)))

(defun my/org-copy-link-description ()
  (interactive)
  (let ((desc (util/org-get-bracket-link-description)))
    (kill-new desc)))

;; TODO: Shift this to org-directory
;; TODO: Fix this. Broken :-(
;; TODO: There's a case where a link to heading also is created.
;;       I'm not sure that's how I intended it

;; This stupid function is still wrong, though it can be used to insert links
;; ORG LINK HANDLING
;; (while (re-search-forward "foo[ \t]+bar" nil t)
;;   (replace-match "foobar" nil nil))
;; NOTE: Works both on region and lines now
;; NOTE: Fixed for mulitple files on line from point to EOL when not using region
(defun my/linkify-words-as-files (&optional prefix suffix description)
  "Convert words to org links.

If no region is active, words beginning from the one under cursor
till end of line is wrapped in a [[]] so that it becomes an org
link.

If a region is active on a single line, that region is
converted to a link.

If a region spans multiple lines, then each of those lines
interesecting with the region becomes a link.

If a set of words or region to be converted doesn't start with a
/ or ~, it's prepended with a * so that it refers to an org
heading, else it's assumed to a be a file link."
  (interactive)
  ;; NOTE: The below regexp replaces ^space also
  ;; (replace-regexp "\\([^[:space:]|^\n]+\\)" "[[\\1]]" nil
  ;; (replace-regexp "\\([^\n]+\\)" "[[\\1]]" nil
  ;; (replace-regexp "\\([[:space:]]*\\)\\([^[:space:]]+\\)[[:space:]]*\\(\n*\\)" "\\1[[\\2]]\\3" nil
  ;; (replace-regexp "\\([[:blank:]]*\\)" "\\([^\n]+\\)" "\\1[[\\2]]")
  (let* ((cursor (point))
         (match "\\([[:blank:]]*\\)\\([^\n]+\\)")
         (description (when description (concat "[" description "]")))
         (sub (if prefix
                  (if (not (progn (beginning-of-line-text) (search-forward prefix (point-at-eol) t)))
                      (concat "\\1[[" prefix "\\2" suffix "]" description "]")
                    (concat "\\1[[\\2]" description "]"))
                (if (called-interactively-p 'any)
                    (concat "\\1[[*\\2][\\2]]")
                  (concat "\\1[[*\\2]" description "]"))))
         (file-re (rx (seq (group (opt "file:") (or "/" "~") (+? (not " "))) (regexp "[ \n]"))))
         (reg-begin (if (use-region-p) (region-beginning)))
         (reg-end (if (use-region-p) (region-end))))
    (util/save-mark-and-restriction
      (if (use-region-p)
          (progn (narrow-to-region reg-begin reg-end)
                 (delete-trailing-whitespace reg-begin reg-end)
                 (goto-char (point-min))
                 (while (re-search-forward match nil t)
                   (when (member (char-after (match-beginning 2)) '(47 126))
                     (setq sub (replace-regexp-in-string "\\*" "" sub)))
                   (replace-match sub nil nil)))
        (goto-char cursor) (skip-chars-backward "^ ")
        (let ((file-begin (point)))
          (when (re-search-forward match (point-at-eol) t)
            (when (> (my/count-matches
                      (concat (buffer-substring-no-properties file-begin (point-at-eol)) "\n")
                      file-re)
                     1)
              (user-error "Multiple files found from point to end of line"))
            (delete-trailing-whitespace cursor (point-at-eol))
            (when (member (char-after (match-beginning 2)) '(47 126))
              (setq sub (replace-regexp-in-string "\\*" "" sub)))
            (replace-match sub nil nil)))))))

(defun my/linkify--replace (cursor match sub &optional end)
  (goto-char cursor)
  (unless (bolp)
    (forward-whitespace -1)
    (forward-char))
  (when (re-search-forward match (if end end (point-at-eol)) t)
    (replace-match sub nil nil)))

(defun my/linkify-pdf-org-words-as-files ()
  (interactive)
  (my/linkify-words-as-files "~/org/pubs_org/" ".org"))

;; TODO: This is more difficult as file-name-nondirectory can only be found
;;       once match is made
(defun my/linkify-words-as-files-with-default-description ()
  "default description is `file-name-nondirectory`"
  (interactive)
  (my/linkify-words-as-files nil nil "bleh"))

;; TODO: Should make sure region is nil
(defun my/linkify-words-as-files-with-description (description)
  (interactive "sDescription: ")
  (my/linkify-words-as-files nil nil description))

(defun my/org-copy-http-pdf-link-before-follow ()
  (let* ((text-props (text-properties-at (point)))
         (uri (if (assoc :uri text-props)
                  (nth 1 (assoc :uri text-props)))))
    (if (and uri (eq (string-match-p "http" uri) 0)
             (string-match-p "pdf" uri))
        (prog1 t (setq my/org-pdf-followed-url uri)
               (eww-open-in-new-buffer)))))
;; (add-hook 'org-open-at-point-functions 'my/org-copy-http-pdf-link-before-follow)

(defun my/org-maybe-http-url (el &optional description)
  "Return HTTP URL if present under point"
  (pcase (car el)
    ('link (let ((link (plist-get (cadr el) :type)))
             (when (or (equal link "http") (equal link "https"))
               (if (and description (plist-get (cadr el) :contents-begin)
                        (plist-get (cadr el) :contents-end))
                   (format "[[%s][%s]]"(plist-get (cadr el) :raw-link)
                           (buffer-substring-no-properties (plist-get (cadr el) :contents-begin)
                                                           (plist-get (cadr el) :contents-end)))
                 (plist-get (cadr el) :raw-link)))))
    ('node-property (and
                     (string-match-p "url"
                                     (plist-get (cadr el) :key))
                     (string-match-p "^\\(\\[+\\)?https?://.+\\(]+\\)?" (plist-get (cadr el) :value))
                     (if description
                         (plist-get (cadr el) :value)
                       (replace-regexp-in-string "^\\(\\[+\\)?\\(https?://.+\\\\)(]+\\)?" "\\1"
                                                 (plist-get (cadr el) :value)))))
    (_ nil)))

(defun my/org-any-link-p (el)
  (pcase (car el)
    ('link t)
    ('node-property (string-match-p org-link-any-re (plist-get (cadr el) :value)))
    (_ nil)))

(add-hook 'org-open-at-point-functions 'my/org-browse-paper-doi)
(defun my/org-browse-paper-doi (&rest args)
  "Browse via `ref-man-org-browse-paper-via-doi' if at DOI property."
  (interactive)
  (let* ((el (org-element-context)))
    (when (and (eq (car el) 'node-property) (equal (plist-get (cadr el) :key) "DOI"))
      (ref-man-org-browse-paper-via-doi)
      t)))

;; (setq org-open-at-point-functions nil)
(add-hook 'org-open-at-point-functions 'my/org-browse-url-webmacs)
(defun my/org-browse-url-webmacs (&rest args)
  "Open http or https url at point in webmacs instead of emacs."
  (interactive)
  (let* ((el (org-element-context))
         (maybe-link (my/org-maybe-http-url el)))
    (when (and current-prefix-arg maybe-link)
      (browse-url-webmacs maybe-link))))

(add-hook 'org-open-at-point-functions 'my/org-open-pdf)
(defvar my/org-open-pdf-external-viewer "evince"
  "Command name of external viewer for pdf files.
See function `my/org-open-pdf'")
(defun my/org-open-pdf (&rest args)
  "Open pdf file in emacs or external viewer."
  (interactive)
  (pcase-let* ((el (org-element-context))
               (cmd my/org-open-pdf-external-viewer)
               (`(,beg . ,end) (org-in-regexp org-link-any-re))
               (maybe-link (and beg end (buffer-substring-no-properties beg end)))
               (link (when maybe-link (save-excursion
                                        (goto-char beg)
                                        (org-element-link-parser))))
               (path (when (and link (equal (org-element-property :type link) "file"))
                       (org-element-property :path link)))
               (proc-name (when path (format "%s %s" cmd (file-name-nondirectory path)))))
    (when (and path
               (string-match-p "pdf" (shell-command-to-string (format "file %s" path))))
      (when current-prefix-arg
        (start-process proc-name (concat "*" proc-name "*") cmd path) t))))

;; (defun my/org-sort-by-citation-count-descending ()
;;   "Sort subtree by CITATIONCOUNT property."
;;   (interactive)
;;   (org-sort-entries nil ?f
;;                     (lambda () (org-entry-get (point) "CITATIONCOUNT"))
;;                     (lambda (x y) (> (string-to-number x) (string-to-number y)))))

;; NOTE: This removed in lieu of `org-file-apps' above
;; (add-hook 'org-open-at-point-functions 'my/org-open-djvu)
(defun my/org-open-djvu (&rest args)
  "Open djvu file at point at point in evince instead of emacs."
  (interactive)
  (unless current-prefix-arg
    (let* ((async-shell-command-buffer 'new-buffer)
           (async-shell-command-display-buffer nil)
           (type (org-element-context))
           (maybe-file (pcase (car type)
                         ('link (let ((link (plist-get (cadr type) :type)))
                                  (when (equal link "file")
                                    (plist-get (cadr type) :raw-link))))
                         ('node-property (let ((match (string-match-p "^\\(?:\\[\\[\\).+"
                                                                      (plist-get (cadr type) :value))))
                                           (if (and match (= match 0))
                                             (progn  (string-match "^\\[\\[\\(/home.+?\\)].*\\|^\\[\\[\\(~/.+?\\)].*"
                                                                   (plist-get (cadr type) :value))
                                                     (match-string 1 (plist-get (cadr type) :value)))
                                           (plist-get (cadr type) :raw-link))))
                         (_ ""))))
      (when (string-suffix-p ".djvu" maybe-file)
        (async-shell-command (format "evince \"%s\"" maybe-file))))))

(defun my/org-table-two-time-durations-add-or-sub-as-string (time1 time2 &optional sub)
  "Subtract two time TIME1 and TIME2 durations in HH:MM format.

With optional SUB, subtract instead."
  (let ((time (my/add-or-subtract-two-time-durations time1 time2 sub)))
    (concat (number-to-string (car time)) ":" (number-to-string (cdr time)))))

(defun my/add-or-subtract-two-time-durations (time1 time2 &optional subtract)
  "Add or subtract two time durations TIME1 and TIME2.

Seconds are ignored in this calculation.

Add the durations by default and subtract only if optional
SUBTRACT is non-nil."
  (let* ((t1 (-take 2 (split-string time1 ":")))
         (t2 (-take 2 (split-string time2 ":")))
         (op (if subtract '- '+))
         (hours (apply op (mapcar (lambda (t) (string-to-number (car t))) (list t1 t2))))
         (mins (apply op (mapcar (lambda (t) (string-to-number (cadr t))) (list t1 t2)))))
    (if subtract
        (progn
          (setq hours (- hours (/ mins 60)))
          (setq mins (% mins 60))
          (when (> mins 0)
            (setq hours (- hours 1))
            (setq mins (- 60 mins)))
          `(,hours . ,mins))
      `(,(+ hours (/ mins 60)) . ,(% mins 60)))))


;; Following three functions for inserting and calculating work and idle time
(defun my/org-table-insert-row (name)
  (let* ((org-table-fix-formulas-confirm 0)
         (_ (condition-case nil (org-table-insert-row t) (error "")))
         (_ (org-table-insert-hline t)))
    name))

(defun my/org-job-cols (jobfile cols)
  (let ((cols (-filter (lambda (x)
                         (string-match-p (concat "^" jobfile ".org$") (car x)))
                       cols)))
    (mapcar 'cdr cols)))

(defun my/org-work-cols (cols)
  (let ((cols (-filter (lambda (x)
                          (string-match-p "^work.org$\\|^projects.org$\\|^research.org$\\|^reading.org\\|students.org$" (car x)))
                        cols)))
    (mapcar 'cdr cols)))

(defun my/org-misc-cols (cols)
  (let ((cols (-filter (lambda (x)
                         (not (string-match-p "^work.org$\\|^projects.org$\\|^research.org$\\|^reading.org$\\|^File$" (car x))))
                       cols)))
    (mapcar 'cdr cols)))

;; NOTE: the original `org-match-line' is buggy as case-fold-search isn't
;;       handled correctly
;; [[file:~/.emacs.d/elpa/org-9.5/org-macs.el::(defun org-match-line (regexp)][here]]
(defun org-match-line (regexp)
  "Match REGEXP at the beginning of the current line."
  (let (case-fold-search)
    (save-excursion
      (beginning-of-line)
      (looking-at regexp))))

;; Maybe I'll add more later
(defun my/helm-org-rifle ()
  "Rifle current buffer.
With \\[current-prefix-arg] rifle through agenda files."
  (interactive)
  (if current-prefix-arg
      (let (current-prefix-arg)
        (helm-org-rifle-agenda-files))
    (helm-org-rifle-current-buffer)))

;; helm functions
(defun helm-org-rifle-buffer (buf)
  "Rifle through given buffer BUF.
BUF is read from user.  Uses completing read to select buffer.
Very un-`helm' like."
  (interactive (list (ido-completing-read "Org buffer: "
                                          (mapcar
                                           (lambda (x) (format "%s" x))
                                           (remove-if-not
                                            (lambda (x) (with-current-buffer x
                                                          (eq major-mode 'org-mode)))
                                            (buffer-list))))))
  (if (get-buffer buf)
      (helm-org-rifle-files (buffer-file-name (get-buffer buf)))
    (message (format "Invalid buffer %s" buf))))

(defun org-table-calc-all-TBLFM (&optional arg)
  "Apply the #+TBLFM on all formulas for the table."
  (interactive "P")
  (unless (org-at-TBLFM-p) (user-error "Not at a #+TBLFM line"))
  (let (formulas)
    (save-excursion
      (goto-char (org-table-TBLFM-begin))
      (while (org-at-TBLFM-p)
        (push (buffer-substring-no-properties
               (point-at-bol) (point-at-eol))
              formulas)
        (forward-line)))
    (setq formulas (reverse formulas))
    (seq-do
     (lambda (formula)
       (save-excursion
         ;; Insert a temporary formula at right after the table
         (goto-char (org-table-TBLFM-begin))
         (let ((s (point-marker)))
	   (insert formula "\n")
	   (let ((e (point-marker)))
	     ;; Recalculate the table.
	     (beginning-of-line 0)		; move to the inserted line
	     (skip-chars-backward " \r\n\t")
	     (unwind-protect
	         (org-call-with-arg #'org-table-recalculate (or arg t))
	       ;; Delete the formula inserted temporarily.
	       (delete-region s e)
	       (set-marker s nil)
	       (set-marker e nil))))))
     formulas)))

(defun my/helm-org-dups ()
  "Show duplicate headings in an org buffer with `helm'."
  (interactive)
  (util/helm-org-show-duplicate-headings
   (lambda (x) (> (length (split-string x " ")) 1)) t))

(defun my/helm-org-headings ()
  "Naviage headings with `helm'."
  (interactive)
  (if (eq major-mode 'org-mode)
      (if current-prefix-arg
          (util/helm-org-headings-agenda-files)
        (util/helm-org-headings))
    (util/helm-org-headings-agenda-files)))

(defun my/image-mode-hook ()
  (define-key image-mode-map (kbd "C-c C-s") 'my/org-save-img))

(add-hook 'image-mode-hook 'my/image-mode-hook)

(defun my/org-save-img (&optional no-prompt)
  "Save current `image-mode' buffer as img to `org-imgs-dir'.
With optional non-nil NO-PROMPT or `current-prefix-arg', don't
ask for description or copy to kill ring.  Default is to ask for
a description and copy as org link to kill ring."
  (interactive)
  (if (derived-mode-p 'image-mode)
      (let ((filename (f-join org-imgs-dir (concat (md5 (buffer-string)) ".png")))
            (desc (if (or no-prompt current-prefix-arg)
                       ""
                     (read-from-minibuffer "Description: " filename))))
        (write-file filename)
        (unless (or no-prompt current-prefix-arg)
          (kill-new (format "[[%s][%s]]" filename desc))
          (message "Copied org link for %s" (or desc filename))))
    (message "Not in image mode")))

(add-to-list 'load-path (f-join my/emacs-libdir "ox-ipynb"))
(setq my/ox-ipynb-lib-path (f-join my/emacs-libdir "ox-ipynb"))
(setq my/ox-dash-lib-path (file-name-directory (find-library-name "dash")))
(setq my/ox-s-lib-path (file-name-directory (find-library-name "s")))
(setq my/ox-ipynb-content nil)
(require 'ox-ipynb)
(defun ox-ipynb-export-to-file-async (&optional subtreep)
  "Export current buffer or subtree to ipynb.
With optional SUBTREEP, export only subtree."
  (interactive)
  (let ((ipynb (ox-ipynb-notebook-filename))
	(content (save-restriction (when subtreep (org-narrow-to-subtree))
                                   (buffer-string)))
        (buf (current-buffer))
        (export-buf (get-buffer-create "*Org IPynb Export*"))
        (heading-regex (format "^\*+ %s" (org-get-heading t t t t))))

    (setq ox-ipynb-export-file (buffer-file-name))
    ;; (org-org-export-as-org async subtreep visible-only body-only info)
    ;;    (with-current-buffer "*Org ORG Export*"
    (with-current-buffer export-buf
      (erase-buffer)
      (insert content)
      (org-mode)

      ;; NOTE: `ox-ipynb' does not export python cells only ipython
      (goto-char (point-min))
      (while (re-search-forward (regexp-quote "#+begin_src python") nil t)
        (replace-match "#+begin_src ipython"))

      ;; Reminder to self. This is not a regular kind of exporter. We have to
      ;; build up the json document that represents a notebook, so some things
      ;; don't work like a regular exporter that has access to the whole data
      ;; structure for resolving links. We have to handle org-ref separately. At
      ;; this point, they are no longer org-ref links, and have been converted
      ;; to custom-id links. They don't render because they are turned to md as
      ;; isolated strings.
      (let ((links (cl-loop for link in (org-element-map
					    (org-element-parse-buffer) 'link 'identity)
			    if (string= "custom-id" (org-element-property :type link))
			    collect link)))
	(cl-loop for link in (reverse links)
		 do
		 (setf (buffer-substring (org-element-property :begin link)
					 (org-element-property :end link))
		       (format "[%s]" (org-element-property :path link)))))
      ;; The bibliography also leaves something to be desired. It gets turned
      ;; into an org-bibtex set of headings. Here we convert these to something
      ;; just slightly more palatable.
      (let ((bib-entries (cl-loop for hl in (org-element-map
						(org-element-parse-buffer) 'headline 'identity)
				  if (org-element-property :=KEY= hl)
				  collect hl)))
	(cl-loop for hl in (reverse bib-entries)
		 do
		 (setf (buffer-substring (org-element-property :begin hl)
					 (org-element-property :end hl))
		       (s-format "[${=KEY=}] ${AUTHOR}. ${TITLE}. https://dx.doi.org/${DOI}\n\n"
				 (lambda (arg &optional extra)
				   (let ((entry (org-element-property (intern-soft (concat ":"arg)) hl)))
				     (substring
				      entry
				      (if (s-starts-with? "{" entry) 1 0)
				      (if (s-ends-with? "}" entry) -1 nil))))))))
      (setq ox-ipynb-content (buffer-string)))

    (async-start
     `(lambda ()
        ,(async-inject-variables "ox-ipynb-export-file\\|ox-ipynb-content\\|my/ox-ipynb-lib-path\\|my/ox-dash-lib-path\\|my/ox-s-lib-path")
        (add-to-list 'load-path my/ox-dash-lib-path)
        (add-to-list 'load-path my/ox-s-lib-path)
        (add-to-list 'load-path my/ox-ipynb-lib-path)
        (require 'ox-ipynb)
        (let ((buf (get-buffer-create "*Org IPynb Export*")))
          (with-current-buffer buf
            (insert ox-ipynb-content)
            (org-mode)
            (with-current-buffer (ox-ipynb-export-to-buffer)
              (buffer-string)))))
     (lambda (result)
       (let* ((filename (with-current-buffer (find-file-noselect ox-ipynb-export-file)
                          (ox-ipynb-notebook-filename)))
              (out-buf (find-file-noselect filename)))
         (with-current-buffer out-buf
           (delete-region (point-min) (point-max))
           (insert result)
           (save-buffer))
         (message "Saved buffer %s" filename))))))

(defun my/orgtbl-to-csv (table params &optional sep)
  "Convert the `orgtbl-mode' TABLE to CSV material.
This does take care of the proper quoting of fields with comma or quotes."
  (orgtbl-to-generic table
		     (org-combine-plists `(:sep ,(or sep ",") :fmt org-quote-csv-field)
					 params)))

(defun my/org-table-to-csv ()
  "Convert `org-table' at point to CSV and display."
  (interactive)
  (let* ((org-export-with-broken-links 'mark)
        (buf (get-buffer-create "*Org Table CSV Export*"))
        org-export-show-temporary-export-buffer
        table
        (csv (my/orgtbl-to-csv
              (org-table-to-lisp
               (buffer-substring-no-properties
                (org-table-begin) (org-table-end)))
              nil (and current-prefix-arg ";"))))
    (with-current-buffer buf
      (insert csv))
    (pop-to-buffer buf)))

(defun my/ox-ipynb-export-doc-async ()
  "Export the the DOC_ROOT to ipynb aysnc."
  (interactive)
  (save-excursion
    (let ((doc-root (util/org-get-tree-prop "DOC_ROOT" nil t)))
      (goto-char doc-root)
      (ox-ipynb-export-to-file-async t))))

