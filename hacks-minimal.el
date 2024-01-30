(require 'eldoc)
(use-package eldoc-overlay
  :ensure t
  :defer t)

(use-package quick-peek
  :ensure t
  :defer t)

(defun eldoc-overlay-quick-peek (format-string &rest args)
  "Quick-peek backend function to show FORMAT-STRING and ARGS."
  ;; Don't show empty strings
  (when (and format-string (not (string-empty-p format-string)))
    (quick-peek-show
     (apply 'format format-string args) (point) 1)))


(require 'bytecomp)
(defun byte-recompile-directory (directory &optional arg force follow-symlinks no-descend)
  "Recompile every `.el' file in DIRECTORY that needs recompilation.
This happens when a `.elc' file exists but is older than the `.el' file.
Files in subdirectories of DIRECTORY are processed also.

If the `.elc' file does not exist, normally this function *does not*
compile the corresponding `.el' file.  However, if the prefix argument
ARG is 0, that means do compile all those files.  A nonzero
ARG means ask the user, for each such `.el' file, whether to
compile it.  A nonzero ARG also means ask about each subdirectory
before scanning it.

If the third argument FORCE is non-nil, recompile every `.el' file
that already has a `.elc' file.

This command will normally not follow symlinks when compiling
files.  If FOLLOW-SYMLINKS is non-nil, symlinked `.el' files will
also be compiled.

NO-DESCEND will not recurse in subdirs, LOL."
  (interactive "DByte recompile directory: \nP")
  (if arg (setq arg (prefix-numeric-value arg)))
  (if noninteractive
      nil
    (save-some-buffers
     nil (lambda ()
           (let ((file (buffer-file-name)))
             (and file
                  (string-match-p emacs-lisp-file-regexp file)
                  (file-in-directory-p file directory)))))
    (force-mode-line-update))
  (with-current-buffer (get-buffer-create byte-compile-log-buffer)
    (setq default-directory (expand-file-name directory))
    ;; compilation-mode copies value of default-directory.
    (unless (derived-mode-p 'compilation-mode)
      (emacs-lisp-compilation-mode))
    (let ((directories (list default-directory))
	  (default-directory default-directory)
	  (skip-count 0)
	  (fail-count 0)
	  (file-count 0)
	  (dir-count 0)
	  last-dir)
      (displaying-byte-compile-warnings
       (while directories
	 (setq directory (car directories))
	 (message "Checking %s..." directory)
         (dolist (source (if no-descend
                             (directory-files directory t "\\.el$")
                           (directory-files directory t)))
           (let ((file (file-name-nondirectory source)))
	     (if (file-directory-p source)
		 (and (not (member file '("RCS" "CVS")))
		      (not (eq ?\. (aref file 0)))
                      (or follow-symlinks
		          (not (file-symlink-p source)))
		      ;; This file is a subdirectory.  Handle them differently.
		      (or (null arg) (eq 0 arg)
			  (y-or-n-p (concat "Check " source "? ")))
		      (setq directories (nconc directories (list source))))
               ;; It is an ordinary file.  Decide whether to compile it.
               (if (and (string-match emacs-lisp-file-regexp source)
			;; The next 2 tests avoid compiling lock files
                        (file-readable-p source)
			(not (string-match "\\`\\.#" file))
                        (not (auto-save-file-name-p source))
                        (not (member source (dir-locals--all-files directory))))
                   (progn (cl-incf
                           (pcase (byte-recompile-file source force arg)
                             ('no-byte-compile skip-count)
                             ('t file-count)
                             (_ fail-count)))
                          (or noninteractive
                              (message "Checking %s..." directory))
                          (if (not (eq last-dir directory))
                              (setq last-dir directory
                                    dir-count (1+ dir-count)))
                          )))))
	 (setq directories (cdr directories))))
      (message "Done (Total of %d file%s compiled%s%s%s)"
	       file-count (if (= file-count 1) "" "s")
	       (if (> fail-count 0) (format ", %d failed" fail-count) "")
	       (if (> skip-count 0) (format ", %d skipped" skip-count) "")
	       (if (> dir-count 1)
                   (format " in %d directories" dir-count) "")))))

(defun python-nav--beginning-of-defun (&optional arg)
  "Internal implementation of `python-nav-beginning-of-defun'.
With positive ARG search backwards, else search forwards."
  (when (or (null arg) (= arg 0)) (setq arg 1))
  (let* ((re-search-fn (if (> arg 0)
                           #'re-search-backward
                         #'re-search-forward))
         (line-beg-pos (line-beginning-position))
         (line-content-start (+ line-beg-pos (current-indentation)))
         (pos (point-marker))
         (min-indentation (if (python-info-current-line-empty-p)
                              most-positive-fixnum
                            (current-indentation)))
         (body-indentation
          (save-excursion
            (python-nav-beginning-of-statement)
            (and (> arg 0)
                 (or (and (python-info-looking-at-beginning-of-defun nil t)
                          (+ (save-excursion
                               (python-nav-beginning-of-statement)
                               (current-indentation))
                             python-indent-offset))
                     (save-excursion
                       (while
                           (and
                            (python-nav-backward-block)
                            (or (not (python-info-looking-at-beginning-of-defun))
                                (>= (current-indentation) min-indentation))
                            (setq min-indentation
                                  (min min-indentation (current-indentation)))))
                       (or (and (python-info-looking-at-beginning-of-defun)
                                (+ (current-indentation) python-indent-offset))
                           0))))))
         (found
          (progn
            (when (and (python-info-looking-at-beginning-of-defun nil t)
                       (or (< arg 0)
                           ;; If looking at beginning of defun, and if
                           ;; pos is > line-content-start, ensure a
                           ;; backward re search match this defun by
                           ;; going to end of line before calling
                           ;; re-search-fn bug#40563
                           (and (> arg 0)
                                (or (python-info-continuation-line-p)
                                    (> pos line-content-start)))))
              (python-nav-end-of-statement))

            (while (and (funcall re-search-fn
                                 python-nav-beginning-of-defun-regexp nil t)
                        (or (python-syntax-context-type)
                            ;; Handle nested defuns when moving
                            ;; backwards by checking indentation.
                            (and (> arg 0)
                                 (not (= (current-indentation) 0))
                                 (>= (current-indentation) body-indentation)))))
            (and (python-info-looking-at-beginning-of-defun nil t)
                 (or (not (= (line-number-at-pos pos)
                             (line-number-at-pos)))
                     (and (>= (point) line-beg-pos)
                          (<= (point) line-content-start)
                          (> pos line-content-start)))))))
    (if found
        (progn
          (when (< arg 0)
            (python-nav-beginning-of-statement))
          (beginning-of-line 1)
          t)
      (and (goto-char pos) nil))))
