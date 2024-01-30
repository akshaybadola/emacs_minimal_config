(require 'org)
;; HACK: for inserting org heading instead of file+custom_id path
;;       when org link is stored
;;       See comment ;; Store a link using the CUSTOM_ID property. below
(defun org-store-link (arg &optional interactive?)
  "Store a link to the current location.
\\<org-mode-map>
This link is added to `org-stored-links' and can later be inserted
into an Org buffer with `org-insert-link' (`\\[org-insert-link]').

For some link types, a `\\[universal-argument]' prefix ARG is interpreted.  \
A single
`\\[universal-argument]' negates `org-context-in-file-links' for file links or
`org-gnus-prefer-web-links' for links to Usenet articles.

A `\\[universal-argument] \\[universal-argument]' prefix ARG forces \
skipping storing functions that are not
part of Org core.

A `\\[universal-argument] \\[universal-argument] \\[universal-argument]' \
prefix ARG forces storing a link for each line in the
active region.

Assume the function is called interactively if INTERACTIVE? is
non-nil."
  (interactive "P\np")
  (org-load-modules-maybe)
  (if (and (equal arg '(64)) (org-region-active-p))
      (save-excursion
	(let ((end (region-end)))
	  (goto-char (region-beginning))
	  (set-mark (point))
	  (while (< (point-at-eol) end)
	    (move-end-of-line 1) (activate-mark)
	    (let (current-prefix-arg)
	      (call-interactively 'org-store-link))
	    (move-beginning-of-line 2)
	    (set-mark (point)))))
    (setq org-store-link-plist nil)
    (let (link cpltxt desc description search custom-id agenda-link)
      (cond
       ;; Store a link using an external link type, if any function is
       ;; available. If more than one can generate a link from current
       ;; location, ask which one to use.
       ((and (not (equal arg '(16)))
	     (let ((results-alist nil))
	       (dolist (f (org-store-link-functions))
		 (when (funcall f)
		   ;; XXX: return value is not link's plist, so we
		   ;; store the new value before it is modified.  It
		   ;; would be cleaner to ask store link functions to
		   ;; return the plist instead.
		   (push (cons f (copy-sequence org-store-link-plist))
			 results-alist)))
	       (pcase results-alist
		 (`nil nil)
		 (`((,_ . ,_)) t)	;single choice: nothing to do
		 (`((,name . ,_) . ,_)
		  ;; Reinstate link plist associated to the chosen
		  ;; function.
		  (apply #'org-link-store-props
			 (cdr (assoc-string
			       (completing-read
				"Which function for creating the link? "
				(mapcar #'car results-alist)
				nil t (symbol-name name))
			       results-alist)))
		  t))))
	(setq link (plist-get org-store-link-plist :link))
	(setq desc (or (plist-get org-store-link-plist :description)
		       link)))

       ;; Store a link from a remote editing buffer.
       ((org-src-edit-buffer-p)
	(let ((coderef-format (org-src-coderef-format))
	      (format-link
	       (lambda (label)
		 (if org-src-source-file-name
		     (format "file:%s::(%s)" org-src-source-file-name label)
		   (format "(%s)" label)))))
	  (cond
	   ;; Code references do not exist in this type of buffer.
	   ;; Pretend we're linking from the source buffer directly.
	   ((not (memq (org-src-source-type) '(example-block src-block)))
	    (with-current-buffer (org-src-source-buffer)
	      (org-store-link arg interactive?))
	    (setq link nil))
	   ;; A code reference exists.  Use it.
	   ((save-excursion
	      (beginning-of-line)
	      (re-search-forward (org-src-coderef-regexp coderef-format)
				 (line-end-position)
				 t))
	    (setq link (funcall format-link (match-string-no-properties 3))))
	   ;; No code reference.  Create a new one then store the link
	   ;; to it, but only in the function is called interactively.
	   (interactive?
	    (end-of-line)
	    (let* ((label (read-string "Code line label: "))
		   (reference (format coderef-format label))
		   (gc (- 79 (length reference))))
	      (if (< (current-column) gc)
		  (org-move-to-column gc t)
		(insert " "))
	      (insert reference)
	      (setq link (funcall format-link label))))
	   ;; No code reference, and non-interactive call.  Don't know
	   ;; what to do.  Give up.
	   (t (setq link nil)))))

       ;; We are in the agenda, link to referenced location
       ((equal (bound-and-true-p org-agenda-buffer-name) (buffer-name))
	(let ((m (or (get-text-property (point) 'org-hd-marker)
		     (get-text-property (point) 'org-marker))))
	  (when m
	    (org-with-point-at m
	      (setq agenda-link (org-store-link nil interactive?))))))

       ((eq major-mode 'calendar-mode)
	(let ((cd (calendar-cursor-to-date)))
	  (setq link
		(format-time-string
		 (car org-time-stamp-formats)
		 (apply 'encode-time
			(list 0 0 0 (nth 1 cd) (nth 0 cd) (nth 2 cd)
			      nil nil nil))))
	  (org-link-store-props :type "calendar" :date cd)))

       ((eq major-mode 'help-mode)
	(let ((symbol (replace-regexp-in-string
		       ;; Help mode escapes backquotes and backslashes
		       ;; before displaying them.  E.g., "`" appears
		       ;; as "\'" for reasons.  Work around this.
		       (rx "\\" (group (or "`" "\\"))) "\\1"
		       (save-excursion
			 (goto-char (point-min))
			 (looking-at "^[^ ]+")
			 (match-string 0)))))
	  (setq link (concat "help:" symbol)))
	(org-link-store-props :type "help"))

       ((eq major-mode 'w3-mode)
	(setq cpltxt (if (and (buffer-name)
			      (not (string-match "Untitled" (buffer-name))))
			 (buffer-name)
		       (url-view-url t))
	      link (url-view-url t))
	(org-link-store-props :type "w3" :url (url-view-url t)))

       ((eq major-mode 'image-mode)
	(setq cpltxt (concat "file:"
			     (abbreviate-file-name buffer-file-name))
	      link cpltxt)
	(org-link-store-props :type "image" :file buffer-file-name))

       ;; In dired, store a link to the file of the current line
       ((derived-mode-p 'dired-mode)
	(let ((file (dired-get-filename nil t)))
	  (setq file (if file
			 (abbreviate-file-name
			  (expand-file-name (dired-get-filename nil t)))
		       ;; otherwise, no file so use current directory.
		       default-directory))
	  (setq cpltxt (concat "file:" file)
		link cpltxt)))

       ((setq search (run-hook-with-args-until-success
		      'org-create-file-search-functions))
	(setq link (concat "file:" (abbreviate-file-name buffer-file-name)
			   "::" search))
	(setq cpltxt (or description link)))

       ((and (buffer-file-name (buffer-base-buffer)) (derived-mode-p 'org-mode))
	(org-with-limited-levels
         (cond
	  ;; Store a link using the target at point.
	  ((org-in-regexp "[^<]<<\\([^<>]+\\)>>[^>]" 1)
	   (setq cpltxt
		 (concat "file:"
			 (abbreviate-file-name
			  (buffer-file-name (buffer-base-buffer)))
			 "::" (match-string 1))
		 link cpltxt))
          ;; NOTE: Store a link using the CUSTOM_ID property.
          ((setq custom-id (org-entry-get nil "CUSTOM_ID"))
           ;; NOTE: This is not there in ol.el
           (setq desc (org-link--normalize-string (org-get-heading t t t t)))
           (setq cpltxt
		 (concat "file:"
			 (abbreviate-file-name
			  (buffer-file-name (buffer-base-buffer)))
			 "::#" custom-id)
		 link cpltxt))
          ;; Store a link using (and perhaps creating) the ID property.
	  ((and (featurep 'org-id)
		(or (eq org-id-link-to-org-use-id t)
		    (and interactive?
			 (or (eq org-id-link-to-org-use-id 'create-if-interactive)
			     (and (eq org-id-link-to-org-use-id
				      'create-if-interactive-and-no-custom-id)
				  (not custom-id))))
		    (and org-id-link-to-org-use-id (org-entry-get nil "ID"))))
	   (setq link (condition-case nil
			  (prog1 (org-id-store-link)
			    (setq desc (or (plist-get org-store-link-plist
						      :description)
					   "")))
			(error
			 ;; Probably before first headline, link only to file.
			 (concat "file:"
				 (abbreviate-file-name
				  (buffer-file-name (buffer-base-buffer))))))))
	  (t
	   ;; Just link to current headline.
	   (setq cpltxt (concat "file:"
				(abbreviate-file-name
				 (buffer-file-name (buffer-base-buffer)))))
	   ;; Add a context search string.
	   (when (org-xor org-link-context-for-files (equal arg '(4)))
	     (let* ((element (org-element-at-point))
		    (name (org-element-property :name element))
		    (context
		     (cond
		      ((let ((region (org-link--context-from-region)))
			 (and region (org-link--normalize-string region t))))
		      (name)
		      ((org-before-first-heading-p)
		       (org-link--normalize-string (org-current-line-string) t))
		      (t (org-link-heading-search-string)))))
	       (when (org-string-nw-p context)
		 (setq cpltxt (format "%s::%s" cpltxt context))
		 (setq desc
		       (or name
			   ;; Although description is not a search
			   ;; string, use `org-link--normalize-string'
			   ;; to prettify it (contiguous white spaces)
			   ;; and remove volatile contents (statistics
			   ;; cookies).
			   (and (not (org-before-first-heading-p))
				(org-link--normalize-string
				 (org-get-heading t t t t)))
			   "NONE")))))
	   (setq link cpltxt)))))

       ((buffer-file-name (buffer-base-buffer))
	;; Just link to this file here.
	(setq cpltxt (concat "file:"
			     (abbreviate-file-name
			      (buffer-file-name (buffer-base-buffer)))))
	;; Add a context search string.
	(when (org-xor org-link-context-for-files (equal arg '(4)))
	  (let ((context (org-link--normalize-string
			  (or (org-link--context-from-region)
			      (org-current-line-string))
			  t)))
	    ;; Only use search option if there is some text.
	    (when (org-string-nw-p context)
	      (setq cpltxt (format "%s::%s" cpltxt context))
	      (setq desc "NONE"))))
	(setq link cpltxt))

       (interactive?
	(user-error "No method for storing a link from this buffer"))

       (t (setq link nil)))

      ;; We're done setting link and desc, clean up
      (when (consp link) (setq cpltxt (car link) link (cdr link)))
      (setq link (or link cpltxt)
	    desc (or desc cpltxt))
      (cond ((not desc))
	    ((equal desc "NONE") (setq desc nil))
	    (t (setq desc (org-link-display-format desc))))
      ;; Store and return the link
      (if (not (and interactive? link))
	  (or agenda-link (and link (org-link-make-string link desc)))
	(if (member (list link desc) org-stored-links)
	    (message "This link already exists")
	  (push (list link desc) org-stored-links)
	  (message "Stored: %s" (or desc link))
	  (when custom-id
	    (setq link (concat "file:"
			       (abbreviate-file-name
				(buffer-file-name (buffer-base-buffer)))
			       "::#" custom-id))
	    (push (list link desc) org-stored-links)))
	(car org-stored-links)))))

(defun my/org-link-open-local-subr (path &optional arg type)
  "Called by hacked version of `org-link-open'.

Open local file links in the current window instead of switching
to another window."
  ;; Don't run hooks when type is "file"
  (unless (and (not (equal type "file"))
               (run-hook-with-args-until-success 'org-open-link-functions path))
    (if (not arg) (org-mark-ring-push)
      (switch-to-buffer-other-window (org-link--buffer-for-internals)))
    (let ((destination
	   (org-with-wide-buffer
	    (if (equal type "radio")
		(org-link--search-radio-target path)
	      (org-link-search
	       (pcase type
		 ("custom-id" (concat "#" path))
		 ("coderef" (format "(%s)" path))
		 (_ path))
	       ;; Prevent fuzzy links from matching themselves.
	       (and (equal type "fuzzy")
		    (+ 2 (org-element-property :begin link)))))
	    (point))))
      (unless (and (<= (point-min) destination)
		   (>= (point-max) destination))
	(widen))
      (goto-char destination))))

;; HACK: to not open a file link in new buffer if it's the same file
(defun org-link-open (link &optional arg)
  "Open a link object LINK.

ARG is an optional prefix argument.  Some link types may handle
it.  For example, it determines what application to run when
opening a \"file\" link.

Functions responsible for opening the link are either hard-coded
for internal and \"file\" links, or stored as a parameter in
`org-link-parameters', which see."
  (let ((type (org-element-property :type link))
	(path (org-element-property :path link)))
    (pcase type
      ;; Opening a "file" link requires special treatment since we
      ;; first need to integrate search option, if any.
      ("file"
       (let* ((option (org-element-property :search-option link))
	      (path (if option (concat path "::" option) path)))
         ;; NOTE: The code differing from [[file:/usr/local/share/emacs/29.0.50/lisp/org/ol.el.gz::defun org-link-open (link &optional arg][bleh]]
         ;; ((string= (car (split-string path "::")) (buffer-file-name))
         ;;                 (my/org-link-open-local-subr option arg type))
         ;;                ((and (string-match-p my/org-pdfs-dir path)
         ;;                      (not (f-exists? path)))
         ;;                 (util/org-delete-file-under-point)
         ;;                 )
         (if (string= (car (split-string path "::")) (buffer-file-name))
             (my/org-link-open-local-subr option arg type)
	   (org-link-open-as-file path
				  (pcase (org-element-property :application link)
				    ((guard arg) arg)
				    ("emacs" 'emacs)
				    ("sys" 'system))))))
      ("custom-id"
       (when (not (string-prefix-p "#" path))
         (setq path (concat "#" path)))
       (my/org-link-open-local-subr path arg))
      ;; Internal links.
      ((or "coderef"  "fuzzy" "radio")
       (my/org-link-open-local-subr path arg))
      (_
       ;; Look for a dedicated "follow" function in custom links.
       (let ((f (org-link-get-parameter type :follow)))
	 (when (functionp f)
	   ;; Function defined in `:follow' parameter may use a single
	   ;; argument, as it was mandatory before Org 9.4.  This is
	   ;; deprecated, but support it for now.
	   (condition-case nil
	       (funcall (org-link-get-parameter type :follow) path arg)
	     (wrong-number-of-arguments
	      (funcall (org-link-get-parameter type :follow) path)))))))))

;; HACK: For clearing the cache for a particular org latex segment
(defun org-latex-preview (&optional arg)
  "Toggle preview of the LaTeX fragment at point.

If the cursor is on a LaTeX fragment, create the image and
overlay it over the source code, if there is none.  Remove it
otherwise.  If there is no fragment at point, display images for
all fragments in the current section.

With a `\\[universal-argument]' prefix argument ARG, clear images \
for all fragments
in the current section.

With a `\\[universal-argument] \\[universal-argument]' prefix \
argument ARG, display image for all
fragments in the buffer.

With a `\\[universal-argument] \\[universal-argument] \
\\[universal-argument]' prefix argument ARG, clear image for all
fragments in the buffer."
  (interactive "p")
  (cond
   ((not (display-graphic-p)) nil)
   ;; Clear whole buffer.
   ((equal arg 64)
    (org-clear-latex-preview (point-min) (point-max))
    (message "LaTeX previews removed from buffer"))
   ;; Preview whole buffer.
   ((equal arg 16)
    (message "Creating LaTeX previews in buffer...")
    (org--latex-preview-region (point-min) (point-max))
    (message "Creating LaTeX previews in buffer... done."))
   ;; Clear current section.
   ;; NOTE: Different here
   ((equal arg 5)
    (org-clear-latex-preview
     (if (org-before-first-heading-p) (point-min)
       (save-excursion
	 (org-with-limited-levels (org-back-to-heading t) (point))))
     (org-with-limited-levels (org-entry-end-position))))
   ;; Toggle preview on LaTeX code at point.
   ((let ((datum (org-element-context)))
      (and (memq (org-element-type datum) '(latex-environment latex-fragment))
	   (let ((beg (org-element-property :begin datum))
		 (end (org-element-property :end datum)))
	     (if (org-clear-latex-preview beg end)
		 (message "LaTeX preview removed")
	       (message "Creating LaTeX preview...")
               ;; NOTE: Different here
	       (org--latex-preview-region beg end (equal arg 4))
	       (message "Creating LaTeX preview... done."))
	     t))))
   ;; Preview current section.
   (t
    (let ((beg (if (org-before-first-heading-p) (point-min)
		 (save-excursion
		   (org-with-limited-levels (org-back-to-heading t) (point)))))
	  (end (org-with-limited-levels (org-entry-end-position))))
      (message "Creating LaTeX previews in section...")
      ;; NOTE: Different here
      (org--latex-preview-region beg end (equal arg 4))
      (message "Creating LaTeX previews in section... done.")))))

(defun org--latex-preview-region (beg end &optional clear-cache)
  "Preview LaTeX fragments between BEG and END.
BEG and END are buffer positions.

CLEAR-CACHE clears cached image."
  (let ((file (buffer-file-name (buffer-base-buffer))))
    (save-excursion
      (org-format-latex
       (concat org-preview-latex-image-directory "org-ltximg")
       beg end
       ;; Emacs cannot overlay images from remote hosts.  Create it in
       ;; `temporary-file-directory' instead.
       (if (or (not file) (file-remote-p file))
	   temporary-file-directory
	 default-directory)
       ;; NOTE: Different here
       'overlays nil 'forbuffer org-preview-latex-default-process clear-cache))))

(defun org-format-latex
    (prefix &optional beg end dir overlays msg forbuffer processing-type clear-cache)
  "Replace LaTeX fragments with links to an image.

The function takes care of creating the replacement image.

Only consider fragments between BEG and END when those are
provided.

When optional argument OVERLAYS is non-nil, display the image on
top of the fragment instead of replacing it.

PROCESSING-TYPE is the conversion method to use, as a symbol.

With non-nil optional CLEAR-CACHE the latex image for the
particular call is cleared, be it segment, section or buffer. See
`org-latex-preview'.

Some of the options can be changed using the variable
`org-format-latex-options', which see."
  (when (and overlays (fboundp 'clear-image-cache)) (clear-image-cache))
  (unless (eq processing-type 'verbatim)
    (let* ((math-regexp "\\$\\|\\\\[([]\\|^[ \t]*\\\\begin{[A-Za-z0-9*]+}")
	   (cnt 0)
	   checkdir-flag)
      (goto-char (or beg (point-min)))
      ;; Optimize overlay creation: (info "(elisp) Managing Overlays").
      (when (and overlays (memq processing-type '(dvipng imagemagick)))
	(overlay-recenter (or end (point-max))))
      (while (re-search-forward math-regexp end t)
	(unless (and overlays
		     (eq (get-char-property (point) 'org-overlay-type)
			 'org-latex-overlay))
	  (let* ((context (org-element-context))
		 (type (org-element-type context)))
	    (when (memq type '(latex-environment latex-fragment))
	      (let ((block-type (eq type 'latex-environment))
		    (value (org-element-property :value context))
		    (beg (org-element-property :begin context))
		    (end (save-excursion
			   (goto-char (org-element-property :end context))
			   (skip-chars-backward " \r\t\n")
			   (point))))
		(cond
		 ((eq processing-type 'mathjax)
		  ;; Prepare for MathJax processing.
		  (if (not (string-match "\\`\\$\\$?" value))
		      (goto-char end)
		    (delete-region beg end)
		    (if (string= (match-string 0 value) "$$")
			(insert "\\[" (substring value 2 -2) "\\]")
		      (insert "\\(" (substring value 1 -1) "\\)"))))
		 ((eq processing-type 'html)
		  (goto-char beg)
		  (delete-region beg end)
		  (insert (org-format-latex-as-html value)))
		 ((assq processing-type org-preview-latex-process-alist)
		  ;; Process to an image.
		  (cl-incf cnt)
		  (goto-char beg)
		  (let* ((processing-info
			  (cdr (assq processing-type org-preview-latex-process-alist)))
			 (face (face-at-point))
			 ;; Get the colors from the face at point.
			 (fg
			  (let ((color (plist-get org-format-latex-options
						  :foreground)))
                            (if forbuffer
                                (cond
                                 ((eq color 'auto)
                                  (face-attribute face :foreground nil 'default))
                                 ((eq color 'default)
                                  (face-attribute 'default :foreground nil))
                                 (t color))
                              color)))
			 (bg
			  (let ((color (plist-get org-format-latex-options
						  :background)))
                            (if forbuffer
                                (cond
                                 ((eq color 'auto)
                                  (face-attribute face :background nil 'default))
                                 ((eq color 'default)
                                  (face-attribute 'default :background nil))
                                 (t color))
                              color)))
			 (hash (sha1 (prin1-to-string
				      (list org-format-latex-header
					    org-latex-default-packages-alist
					    org-latex-packages-alist
					    org-format-latex-options
					    forbuffer value fg bg))))
			 (imagetype (or (plist-get processing-info :image-output-type) "png"))
			 (absprefix (expand-file-name prefix dir))
			 (linkfile (format "%s_%s.%s" prefix hash imagetype))
			 (movefile (format "%s_%s.%s" absprefix hash imagetype))
			 (sep (and block-type "\n\n"))
			 (link (concat sep "[[file:" linkfile "]]" sep))
			 (options
			  (org-combine-plists
			   org-format-latex-options
			   `(:foreground ,fg :background ,bg))))
		    (when msg (message msg cnt))
		    (unless checkdir-flag ; Ensure the directory exists.
		      (setq checkdir-flag t)
		      (let ((todir (file-name-directory absprefix)))
			(unless (file-directory-p todir)
			  (make-directory todir t))))
                    ;; NOTE: Different here
		    (when (or clear-cache (not (file-exists-p movefile)))
		      (org-create-formula-image
		       value movefile options forbuffer processing-type))
		    (if overlays
			(progn
			  (dolist (o (overlays-in beg end))
			    (when (eq (overlay-get o 'org-overlay-type)
				      'org-latex-overlay)
			      (delete-overlay o)))
			  (org--make-preview-overlay beg end movefile imagetype)
			  (goto-char end))
		      (delete-region beg end)
		      (insert
		       (org-add-props link
			   (list 'org-latex-src
				 (replace-regexp-in-string "\"" "" value)
				 'org-latex-src-embed-type
				 (if block-type 'paragraph 'character)))))))
		 ((eq processing-type 'mathml)
		  ;; Process to MathML.
		  (unless (org-format-latex-mathml-available-p)
		    (user-error "LaTeX to MathML converter not configured"))
		  (cl-incf cnt)
		  (when msg (message msg cnt))
		  (goto-char beg)
		  (delete-region beg end)
		  (insert (org-format-latex-as-mathml
			   value block-type prefix dir)))
		 (t
		  (error "Unknown conversion process %s for LaTeX fragments"
			 processing-type)))))))))))

;; HACK: versions which also operate on timestamps outside logbook
(defun org-shiftcontrolup (&optional n)
  "Change timestamps synchronously up in CLOCK log lines.
Optional argument N tells to change by that many units."
  (interactive "P")
  (if (org-at-timestamp-p 'lax)
      (let (org-support-shift-select)
        (if (org-at-clock-log-p)
            (org-clock-timestamps-up n)
          (org-timestamp-up n)))
    (user-error "Not at a clock log or timestamp")))

(defun org-shiftcontroldown (&optional n)
  "Change timestamps synchronously up in CLOCK log lines.
Optional argument N tells to change by that many units."
  (interactive "P")
  (if (org-at-timestamp-p 'lax)
      (let (org-support-shift-select)
        (if (org-at-clock-log-p)
            (org-clock-timestamps-down n)
          (org-timestamp-down n)))
    (user-error "Not at a clock log or timestamp")))


(defun org-list-count-blanks-backward ()
  "Count blank lines above beginning of line."
  (save-excursion
    (count-lines (goto-char (point-at-bol))
		 (progn (skip-chars-backward " \r\t\n")
			(forward-line)
			(point)))))

(defun org-list-count-blanks-forward ()
  "Count blank lines above beginning of line."
  (save-excursion
    (count-lines (point-at-bol)
		 (progn
                   (goto-char (point-at-eol))
                   (skip-chars-forward " \r\t\n")
                   (beginning-of-line)
		   (forward-line -1)
		   (point)))))

;; HACK: Don't add blank line if parent struct doesn't have blank lines
(defun org-list-separating-blank-lines-number (pos struct prevs)
  "Return number of blank lines that should separate items in list.

POS is the position of point where `org-list-insert-item' was called.

STRUCT is the list structure.  PREVS is the alist of previous
items, as returned by `org-list-prevs-alist'.

Assume point is at item's beginning.  If the item is alone, apply
some heuristics to guess the result."
  (save-excursion
    (let ((item (point))
	  (insert-blank-p
	   (cdr (assq 'plain-list-item org-blank-before-new-entry)))
	  usr-blank)
      (cond
       ;; Trivial cases where there should be none.
       ((not insert-blank-p) 0)
       ;; When `org-blank-before-new-entry' says so, it is 1.
       ((eq insert-blank-p t) 1)
       ;; `plain-list-item' is 'auto.  Count blank lines separating
       ;; neighbors' items in list.
       ;; NOTE: This is where the differences are
       (t (pcase-let* ((next-p (org-list-get-next-item item struct prevs))
                       (`(,next-blanks ,next-indent)
                        (when next-p
                          (save-excursion
                            (goto-char next-p)
		            `(,(org-list-count-blanks-backward)
                              ,(progn (beginning-of-line)
                                      (current-indentation))))))
                       (prev-p (org-list-get-prev-item item struct prevs))
                       (`(,prev-blanks ,prev-indent)
                        (when prev-p
                          (save-excursion
                            (goto-char item)
		            `(,(org-list-count-blanks-backward)
                              ,(progn (beginning-of-line)
                                      (current-indentation))))))
                       (parent-p (org-list-get-parent item struct nil))
                       (`(,parent-blanks ,parent-indent)
                        (when parent-p
                          (save-excursion
                            (goto-char parent-p)
		            `(,(org-list-count-blanks-backward)
                              ,(progn (beginning-of-line)
                                      (current-indentation)))))))
	    (cond
	     ;; Is there a next item?
	     (next-p next-blanks
                     ;; (save-excursion
                     ;;   (goto-char next-p)
                     ;;   (funcall count-blanks))
                     )
	     ;; Is there a previous item?
	     (prev-p prev-blanks)
	     ;; Parent?
             ((and (not (or next-p prev-p)) parent-p)
              (save-excursion
                (beginning-of-line)
                ;; (goto-char parent-p)
	        (org-list-count-blanks-backward)))
	     ;; User inserted blank lines, trust him.
	     ((and (> pos (org-list-get-item-end-before-blank item struct))
		   (> (save-excursion (goto-char pos)
				      (setq usr-blank (org-list-count-blanks-backward)))
		      0))
	      usr-blank)
	     ;; Are there blank lines inside the list so far?
	     ((save-excursion
		(goto-char (org-list-get-top-point struct))
		;; Do not use `org-list-search-forward' so blank lines
		;; in blocks can be counted in.
		(re-search-forward
		 "^[ \t]*$" (org-list-get-item-end-before-blank item struct) t))
	      1)
	     ;; Default choice: no blank line.
	     (t 0))))))))

(defun pos-tip-show
    (string
     &optional tip-color pos window timeout width frame-coordinates dx dy font)
  "Show STRING in a tooltip, which is a small X window, at POS in WINDOW
using frame's default font with TIP-COLOR.

Return pixel position of tooltip relative to top left corner of frame as
a cons cell like (X . Y).

TIP-COLOR is a face or a cons cell like (FOREGROUND-COLOR . BACKGROUND-COLOR)
used to specify *only* foreground-color and background-color of tooltip. If
omitted, use `pos-tip-foreground-color' and `pos-tip-background-color' or the
foreground and background color of the `tooltip' face instead.

Omitting POS and WINDOW means use current position and selected window,
respectively.

Automatically hide the tooltip after TIMEOUT seconds. Omitting TIMEOUT means
use the default timeout of 5 seconds. Non-positive TIMEOUT means don't hide
tooltip automatically.

WIDTH, if non-nil, specifies the width of filling each paragraph.

If FRAME-COORDINATES is omitted or nil, automatically obtain the absolute
coordinates of the top left corner of frame which WINDOW is on. Here,
`top left corner of frame' represents the origin of `window-pixel-edges'
and its coordinates are essential for calculating the absolute coordinates
of the tooltip. If a cons cell like (LEFT . TOP), specifies the frame
absolute location and makes the calculation slightly faster, but can be
used only when it's clear that frame is in the specified position. Users
can get the latest values of frame coordinates for using in the next call
by referring the variable `pos-tip-saved-frame-coordinates' just after
calling this function. Otherwise, FRAME-COORDINATES `relative' means use
the pixel coordinates relative to the top left corner of the frame for
displaying the tooltip. This is the same effect as
`pos-tip-use-relative-coordinates' is non-nil.

DX specifies horizontal offset in pixel.

DY specifies vertical offset in pixel. This makes the calculations done
without considering the height of object at POS, so the object might be
hidden by the tooltip.

See also `pos-tip-show-no-propertize'."
  (unless window
    (setq window (selected-window)))
  (let* ((frame (window-frame window))
	 (max-width (pos-tip-x-display-width frame))
	 (max-height (pos-tip-x-display-height frame))
	 (w-h (pos-tip-string-width-height string))
         (fg (pos-tip-compute-foreground-color tip-color))
         (bg (pos-tip-compute-background-color tip-color))
         (frame-font (or font (find-font (font-spec :name (frame-parameter frame 'font)))))
         (tip-face-attrs (list :font frame-font :foreground fg :background bg)))
    (cond
     ((and width
	   (> (car w-h) width))
      (setq string (pos-tip-fill-string string width nil 'none nil max-height)
	    w-h (pos-tip-string-width-height string)))
     ((or (> (car w-h) max-width)
	  (> (cdr w-h) max-height))
      (setq string (pos-tip-truncate-string string max-width max-height)
	    w-h (pos-tip-string-width-height string))))
    (pos-tip-show-no-propertize
     (propertize string 'face tip-face-attrs)
     tip-color pos window timeout
     (pos-tip-tooltip-width (car w-h) (frame-char-width frame))
     (pos-tip-tooltip-height (cdr w-h) (frame-char-height frame) frame)
     frame-coordinates dx dy)))
