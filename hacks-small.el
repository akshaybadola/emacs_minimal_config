(defun replace-in-string (in what with)
  (replace-regexp-in-string
   (regexp-quote what) with in nil 'literal))


(require 'electric)
;; Custom `electric-indent-post-self-insert-function' because the original
;; definition doesn't respect `electric-quote-string' or `electric-quote-comment'
(defun electric-indent-post-self-insert-function ()
  "Function that `electric-indent-mode' adds to `post-self-insert-hook'.
This indents if the hook `electric-indent-functions' returns non-nil,
or if a member of `electric-indent-chars' was typed; but not in a string
or comment."
  ;; FIXME: This reindents the current line, but what we really want instead is
  ;; to reindent the whole affected text.  That's the current line for simple
  ;; cases, but not all cases.  We do take care of the newline case in an
  ;; ad-hoc fashion, but there are still missing cases such as the case of
  ;; electric-pair-mode wrapping a region with a pair of parens.
  ;; There might be a way to get it working by analyzing buffer-undo-list, but
  ;; it looks challenging.
  (let (pos)
    (when (and
           electric-indent-mode
           ;; Don't reindent while inserting spaces at beginning of line.
           (or (not (memq last-command-event '(?\s ?\t)))
               (save-excursion (skip-chars-backward " \t") (not (bolp))))
           (setq pos (electric--after-char-pos))
           (save-excursion
             (goto-char pos)
             (let ((act (or (run-hook-with-args-until-success
                             'electric-indent-functions
                             last-command-event)
                            (memq last-command-event electric-indent-chars))))
               (not
                (or (memq act '(nil no-indent))
                    ;; In a string or comment, but ignore if `electric-quote-comment'
                    ;; or `electric-quote-string' is non-nil
                    (unless (or (eq act 'do-indent)
                                electric-quote-string electric-quote-comment)
                      (nth 8 (syntax-ppss))))))))
      ;; If we error during indent, silently give up since this is an
      ;; automatic action that the user didn't explicitly request.
      ;; But we don't want to suppress errors from elsewhere in *this*
      ;; function, hence the `condition-case' and `throw' (Bug#18764).
      (catch 'indent-error
        ;; For newline, we want to reindent both lines and basically
        ;; behave like reindent-then-newline-and-indent (whose code we
        ;; hence copied).
        (let ((at-newline (<= pos (line-beginning-position))))
          (when at-newline
            (let ((before (copy-marker (1- pos) t)))
              (save-excursion
                (unless
                    (or (memq indent-line-function
                              electric-indent-functions-without-reindent)
                        electric-indent-inhibit)
                  ;; Don't reindent the previous line if the
                  ;; indentation function is not a real one.
                  (goto-char before)
                  (condition-case-unless-debug ()
                      (indent-according-to-mode)
                    (error (throw 'indent-error nil))))
                (unless (eq electric-indent-inhibit 'electric-layout-mode)
                  ;; Unless we're operating under
                  ;; `electric-layout-mode' (Bug#35254), the goal here
                  ;; will be to remove the trailing whitespace after
                  ;; reindentation of the previous line because that
                  ;; may have (re)introduced it.
                  (goto-char before)
                  ;; We were at EOL in marker `before' before the call
                  ;; to `indent-according-to-mode' but after we may
                  ;; not be (Bug#15767).
                  (when (and (eolp))
                    (delete-horizontal-space t))))))
          (unless (and electric-indent-inhibit
                       (not at-newline))
            (condition-case-unless-debug ()
                (indent-according-to-mode)
              (error (throw 'indent-error nil)))))))))


(defun replace-string (from-string to-string &optional delimited start end backward
                       region-noncontiguous-p)
  "..."
  (declare (interactive-only
            "use `search-forward' and `replace-match' instead."))
  (interactive
   (let ((common
          (query-replace-read-args
           (concat "Replace"
                   (if current-prefix-arg
                       (if (eq current-prefix-arg '-) " backward" " word")
                     "")
                   " string"
                   (if (use-region-p) " in region" ""))
           nil)))
     (list (nth 0 common) (nth 1 common) (nth 2 common)
           (if (use-region-p) (region-beginning))
           (if (use-region-p) (region-end))
           (nth 3 common)
           (if (use-region-p) (region-noncontiguous-p)))))
  (perform-replace
   from-string to-string nil nil delimited nil nil start end backward region-noncontiguous-p))

;; custom line-move to remove line-move-partial
(defun hacks/line-move (arg &optional noerror _to-end try-vscroll)
  "Move forward ARG lines.
If NOERROR, don't signal an error if we can't move ARG lines.
TO-END is unused.
TRY-VSCROLL controls whether to vscroll tall lines: if either
`auto-window-vscroll' or TRY-VSCROLL is nil, this function will
not vscroll.

This particular version has been hacked so that vscroll never
moves."
  (if noninteractive
      (line-move-1 arg noerror)
    (unless (and auto-window-vscroll try-vscroll
		 ;; Only vscroll for single line moves
		 (= (abs arg) 1)
		 ;; Under scroll-conservatively, the display engine
		 ;; does this better.
		 (zerop scroll-conservatively)
		 ;; But don't vscroll in a keyboard macro.
		 (not defining-kbd-macro)
		 (not executing-kbd-macro)
                 ;; NOTE: HACK commented out `line-move-partial' Though I think
                 ;;       vscroll was needed for long images so may need to
                 ;;       revisit this later.
                 ;;
                 ;; (line-move-partial arg noerror)
                 )
      (set-window-vscroll nil 0 t)
      (if (and line-move-visual
	       ;; Display-based column are incompatible with goal-column.
	       (not goal-column)
	       ;; When the text in the window is scrolled to the left,
	       ;; display-based motion doesn't make sense (because each
	       ;; logical line occupies exactly one screen line).
	       (not (> (window-hscroll) 0))
	       ;; Likewise when the text _was_ scrolled to the left
	       ;; when the current run of vertical motion commands
	       ;; started.
	       (not (and (memq last-command
			       `(next-line previous-line ,this-command))
			 auto-hscroll-mode
			 (numberp temporary-goal-column)
			 (>= temporary-goal-column
			    (- (window-width) hscroll-margin)))))
	  (prog1 (line-move-visual arg noerror)
	    ;; If we moved into a tall line, set vscroll to make
	    ;; scrolling through tall images more smooth.
	    (let ((lh (line-pixel-height))
		  (edges (window-inside-pixel-edges))
		  (dlh (default-line-height))
		  winh)
	      (setq winh (- (nth 3 edges) (nth 1 edges) 1))
	      (if (and (< arg 0)
		       (< (point) (window-start))
		       (> lh winh))
		  (set-window-vscroll
		   nil
		   (- lh dlh) t))))
	(line-move-1 arg noerror)))))

;; smartscan hacks
(require 'smartscan)
(defvar smartscan-ignore-contexts '(comment string)
  "Ignore scanning symbols when in these contexts.

Default values are to ignore the symbols when in a comment or
string.  Use `smartscan-ignore-symbol-name-in-comments' and
`smartscan-ignore-symbol-name-in-strings' to change or set
directly.")
(defun smartscan-ignore-symbol-name-in-comments (&optional arg)
  (interactive)
  (when current-prefix-arg
    (setq arg t))
  (if (or (not arg) (and (integerp arg) (< 0 arg)))
      (add-to-list 'smartscan-ignore-contexts 'comment)
    (setq smartscan-ignore-contexts
          (remove 'comment smartscan-ignore-contexts))))
(defun smartscan-ignore-symbol-name-in-strings (&optional arg)
  (interactive)
  (when current-prefix-arg
    (setq arg t))
  (if (or (not arg) (and (integerp arg) (< 0 arg)))
      (add-to-list 'smartscan-ignore-contexts 'string)
    (setq smartscan-ignore-contexts
          (remove 'string smartscan-ignore-contexts))))

;; Change the function, LOL
(defun smartscan-symbol-goto (name direction)
  "Jumps to the next NAME in DIRECTION in the current buffer.

DIRECTION must be either `forward' or `backward'; no other option
is valid."

  ;; if `last-command' did not contain
  ;; `smartscan-symbol-go-forward/backward' then we assume it's a
  ;; brand-new command and we re-set the search term.
  (unless (memq last-command '(smartscan-symbol-go-forward
                               smartscan-symbol-go-backward))
    (setq smartscan-last-symbol-name name)
    (push-mark))
  (setq smartscan-symbol-old-pt (point))
  (message (format "%s scan for symbol \"%s\""
                   (capitalize (symbol-name direction))
                   smartscan-last-symbol-name))
  (smartscan-with-symbol
    (unless (catch 'done
              (while (funcall (cond
                               ((eq direction 'forward) ; forward
                                're-search-forward)
                               ((eq direction 'backward) ; backward
                                're-search-backward)
                               (t (error "Invalid direction"))) ; all others
                              (concat "\\<" smartscan-last-symbol-name "\\>") nil t)
                (unless (memq (syntax-ppss-context (syntax-ppss (point)))
                              smartscan-ignore-contexts)
                  (throw 'done t))))
      (goto-char smartscan-symbol-old-pt))))
