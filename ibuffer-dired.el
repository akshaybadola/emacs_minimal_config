(global-set-key (kbd "C-c i") 'ibuffer)

(defun my/ibuffer-copy-full-filenames-as-kill ()
  "Copy full buffer filename at point to kill ring without marking it.
Only copies buffer at point even if region is set.  Defaults to
copying marked buffers if there are any marked buffers."
  (interactive)
  (if (zerop (ibuffer-count-marked-lines))
      (kill-new (buffer-file-name (ibuffer-current-buffer t)))
    (ibuffer-copy-filename-as-kill 0)))

;; I've hacked ibuffer-visit-buffer-other-window as the noselect
;; option was not working correctly with the code that was there.
;; Must keep track of ibuffer thingy as if their code changes, this also
;; will have to be modified
(defun my/ibuffer-mode-hook ()
  (define-key ibuffer-mode-map (kbd "w") #'my/ibuffer-copy-full-filenames-as-kill)
  (define-key ibuffer-mode-map (kbd "M-o") nil)
  (bind-key (kbd "o") #'ibuffer-visit-buffer-other-window-noselect ibuffer-mode-map)
  (bind-key (kbd "e") #'ibuffer-visit-buffer-other-window ibuffer-mode-map))

(defun my/get-or-create-window-on-side ()
  "Get the window on side if it exists else create it."
  (let* ((orig-win (selected-window))
         (win (cond ((window-in-direction 'right orig-win)
                     (window-in-direction 'right orig-win))
                    ((window-in-direction 'left orig-win)
                     (window-in-direction 'left orig-win))
                    (t (split-window-horizontally)))))
    win))

(defun ibuffer-visit-buffer-other-window (&optional noselect)
    "Visit the buffer on this line in another window."
    (interactive)
    (let ((buf (ibuffer-current-buffer t))
          (win (my/get-or-create-window-on-side)))
      (bury-buffer (current-buffer))
      (set-window-buffer win buf)
      (unless noselect
        (pop-to-buffer buf))))

(add-hook 'ibuffer-mode-hook 'my/ibuffer-mode-hook)
(setq ibuffer-saved-filter-groups
      `(("default"
         ("dired" (mode . dired-mode))
         ("perl" (mode . cperl-mode))
         ("erc" (mode . erc-mode))
         ("planner" (or
                     (name . "^\\*Calendar\\*$")
                     (name . "^diary$")
                     (mode . muse-mode)))
         ("emacs" (or
                   (name . "^\\*scratch\\*$")
                   (name . "^\\*Messages\\*$")))
         ("gnus" (or
                  (mode . message-mode)
                  (mode . bbdb-mode)
                  (mode . mail-mode)
                  (mode . gnus-group-mode)
                  (mode . gnus-summary-mode)
                  (mode . gnus-article-mode)
                  (name . "^\\.bbdb$")
                  (name . "^\\.newsrc-dribble"))))
        ;; ("project" ,(ibuffer-project-generate-filter-groups))
        ))

;; dired stuff
;; Dired filter functions already exist in "dired-filter"
;; TODO: setup dired-filter and dired-narrow
;; TODO: add highlight to dired-narrow
(setq dired-listing-switches "-alh --group-directories-first")
(setq dired-recursive-deletes 'always)
(setq dired-recursive-copies 'always)
(setq dired-deletion-confirmer #'y-or-n-p)
(setq dired-show-hidden-p nil)
(setq dired-toggle-extension-list nil)
(setq pdf-file-warning-threshold 40000000)
(setq dired-clean-confirm-killing-deleted-buffers nil)

(when (my/install-for 'compact)
  ;; dired-rainbow
  (use-package dired-rainbow
    :ensure t
    :defer t
    :config
    (progn
      (dired-rainbow-define-chmod directory "#6cb2eb" "d.*" t)
      (dired-rainbow-define html "#eb5286"
                            ("css" "less" "sass" "scss" "htm" "html" "jhtm" "mht" "eml" "mustache" "xhtml") t)
      (dired-rainbow-define xml "#f090f4"
                            ("xml" "xsd" "xsl" "xslt" "wsdl" "bib" "json" "msg" "pgn" "rss" "yaml" "yml" "rdata") t)
      (dired-rainbow-define document "#9561e2"
                            ("doc" "docm" "docx" "odb" "odt" "pdb" "pdf" "ps" "rtf" "djvu" "epub" "odp" "ppt" "pptx" "PDF" "EPUB") t)
      (dired-rainbow-define text "#424242"
                            ("org" "etx" "info" "markdown" "md" "mkd" "nfo" "pod" "rst" "tex" "textfile" "txt") t)
      (dired-rainbow-define database "#6574cd"
                            ("xls" "xlsx" "csv" "accdb" "db" "mdb" "sqlite" "nc") t)
      (dired-rainbow-define music "#de751f"
                            ("mp3" "MP3" "ogg" "mov" "mid" "midi" "wav" "aiff" "flac") t)
      (dired-rainbow-define video "#ff2f8f"
                            ("mkv" "mp4" "MP4" "avi" "mpeg" "mpg" "flv" "mov") t)
      (dired-rainbow-define image "#f66d9b"
                            ("tiff" "tif" "cdr" "gif" "ico" "jpeg" "jpg" "png" "psd" "eps" "svg") t)
      (dired-rainbow-define log "#c17d11"
                            ("log") t)
      (dired-rainbow-define shell "#f6993f"
                            ("awk" "bash" "bat" "sed" "sh" "zsh" "vim") t)
      (dired-rainbow-define interpreted "#38c172"
                            ("py" "ipynb" "rb" "pl" "t" "msql" "mysql" "pgsql" "sql" "r" "clj" "cljs" "scala" "js") t)
      (dired-rainbow-define compiled "#4dc0b5"
                            ("asm" "cl" "lisp" "el" "c" "h" "c++" "h++" "hpp" "hxx" "m" "cc" "cs" "cp" "cpp" "go" "f" "for" "ftn" "f90" "f95" "f03" "f08" "s" "rs" "hi" "hs" "pyc" ".java") t)
      (dired-rainbow-define winexec "#8cc4ff"
                            ("exe" "msi") t)
      (dired-rainbow-define compressed "#539800" ; "#51d88a"
                            ("7z" "zip" "bz2" "tgz" "txz" "gz" "xz" "z" "Z" "jar" "war" "ear" "rar" "sar" "xpi" "apk" "xz" "tar" "pkl") t)
      (dired-rainbow-define packaged "#faad63"
                            ("deb" "rpm" "apk" "jad" "jar" "cab" "pak" "pk3" "vdf" "vpk" "bsp") t)
      (dired-rainbow-define encrypted "#2f2df4"
                            ("gpg" "pgp" "asc" "bfe" "enc" "signature" "sig" "p12" "pem") t)
      (dired-rainbow-define fonts "#6cb2eb"
                            ("afm" "fon" "fnt" "pfb" "pfm" "ttf" "otf") t)
      (dired-rainbow-define partition "#e3342f"
                            ("img" "dmg" "iso" "bin" "nrg" "qcow" "toast" "vcd" "vmdk" "bak") t)
      (dired-rainbow-define vc "#0074d9"
                            ("git" "gitignore" "gitattributes" "gitmodules") t)
      (dired-rainbow-define-chmod executable "#006400" "-.*x.*" t) ; "#28cc82" "#38c172" "dark green"
      )))

;; FIXME: Some of these functions have been obsoleted by dired-filter
;; (defun dired--toggle-helper ()
;;   (revert-buffer)
;;   (when (not dired-show-hidden-p)
;;     (dired-mark-files-regexp "^\\\.")
;;     (dired-do-kill-lines)
;;     (message "Hiding dot files"))
;;   (when dired-toggle-extension-list
;;     (revert-buffer)
;;     (mapcar (lambda (x) (dired-mark-files-regexp x)) dired-toggle-extension-list)
;;     (dired-toggle-marks)
;;     (dired-do-kill-lines)))

;; (defun dired-toggle-extension (arg)
;;   "Show/hide dot-files"
;;   (interactive "P")
;;   (if current-prefix-arg
;;       (setq dired-toggle-extension-list nil)
;;     (when (eq major-mode 'dired-mode)
;;       (unless (boundp 'dired-toggle-extension-list)
;;         (setq dired-toggle-extension-list nil)) ;; dot files should be currently visible
;;       (let* ((ext (read-string
;;                    (concat "Current extensions: " (if dired-toggle-extension-list
;;                                                       (string-join dired-toggle-extension-list ",") "")
;;                            " ")))
;;              (ext-regexp (if (not (string-empty-p ext))
;;                              (concat "\\." (car (last (split-string ext "\\."))) "$"))))
;;         (if (not arg)
;;             (when ext-regexp
;;               (add-to-list 'dired-toggle-extension-list ext-regexp))
;;           (if ext-regexp (setq dired-toggle-extension-list
;;                                (delete ext-regexp dired-toggle-extension-list))
;;             (pop dired-toggle-extension-list))))))
;;   (dired--toggle-helper))

;; (defun dired-toggle-dot ()
;;   (interactive)
;;   (when (eq major-mode 'dired-mode)
;;     ;; (when (not (boundp 'dired-show-hidden-p)) (setq dired-show-hidden-p t))
;;     (if dired-show-hidden-p
;;         (setq dired-show-hidden-p nil)
;;       (setq dired-show-hidden-p t))
;;     (dired--toggle-helper)))

;; (defun dired-revert-with-hidden-toggled ()
;;   (interactive)
;;   (setq dired-toggle-extension-list nil)
;;   (dired--toggle-helper))


(when (my/install-for 'minimal)
  (use-package dired-filter
    :ensure t
    :defer t
    )
  (add-hook 'dired-mode-hook 'dired-filter-mode))

(add-hook 'dired-mode-hook 'my/dired-mode-hook)

(defun my/dired-mode-hook ()
  (when (my/install-for 'compact)
    (define-key dired-mode-map (kbd "s") nil)
    (define-key dired-mode-map (kbd "w")
                'util/dired-copy-full-filename-as-kill)
    (define-key dired-mode-map (kbd "s s") 'util/dired-custom-sort-size)
    (define-key dired-mode-map (kbd "s t") 'util/dired-custom-sort-time)
    (define-key dired-mode-map (kbd "s n") 'util/dired-custom-sort-name))
  (define-key dired-mode-map (kbd "B") 'dired-up-directory)
  (define-key dired-mode-map (kbd "N") 'dired-find-file))

(defun files/abort-if-file-too-large (size op-type filename &optional offer-raw)
  "If file SIZE larger than `large-file-warning-threshold', allow user to abort.
OP-TYPE specifies the file operation being performed (for message
to user).  If OFFER-RAW is true, give user the additional option
to open the file literally.  If the user chooses this option,
`abort-if-file-too-large' returns the symbol `raw'.  Otherwise,
it returns nil or exits non-locally."
  (let ((choice (and large-file-warning-threshold size
	             (> size large-file-warning-threshold)
                     ;; No point in warning if we can't read it.
                     (file-readable-p filename)
                     (files--ask-user-about-large-file
                      size op-type filename offer-raw))))
    (when (eq choice 'abort)
      (user-error "Aborted"))
    choice))

;; Open large pdf files
;; Custom overridden function
(defun abort-if-file-too-large (size op-type filename &optional offer-raw)
  "If file SIZE larger than `large-file-warning-threshold', allow user to abort.

This function is overridden from the one in `files' to ignore PDF
files for large file size warning. See
`files/abort-if-file-too-large' for the original in case
`find-file' breaks.

OP-TYPE specifies the file operation being performed (for message
to user).  If OFFER-RAW is true, give user the additional option
to open the file literally.  If the user chooses this option,
`abort-if-file-too-large' returns the symbol `raw'.  Otherwise,
it returns nil or exits non-locally."
  ;; (let ((retval (my/dired-get-file-type (dired-get-filename t t) t)))
  ;;   (if retval
  ;;       (when (and large-file-warning-threshold size
  ;;                  (not (string-match-p "pdf document" retval))
  ;;                  (> size large-file-warning-threshold)
  ;;                  (not (y-or-n-p (format "File %s is large (%s), really %s? "
  ;;       			          (file-name-nondirectory filename)
  ;;       			          (file-size-human-readable size) op-type))))
  ;;         (user-error "Aborted"))))
  (when (file-exists-p filename)
    (let* ((filetype (my/dired-get-file-type (expand-file-name filename) t))
           (case-fold-search t)
           (large-pdf-file (and size pdf-file-warning-threshold
                                (> size pdf-file-warning-threshold)))
           (large-file (and size large-file-warning-threshold
                            (> size large-file-warning-threshold)))
           ;; No point of doing anything if we can't read it.
           (choice (if (file-readable-p filename)
                       (and (if (and filetype (string-match-p "pdf document" filetype))
                                large-pdf-file
                              large-file)
                            (files--ask-user-about-large-file
                             size op-type filename offer-raw))
                     (user-error "Cannot read file %s" filename))))
      (when (eq choice 'abort)
        (user-error "Aborted"))
      choice)))

(defun my/dired-get-file-type (file deref-symlinks)
  "Get the type of FILE, according to the `file' command.
If you give a prefix to this command, and FILE is a symbolic
link, then the type of the file linked to by FILE is printed
instead."
  (when file
    (let (process-file-side-effects)
      (with-temp-buffer
        (if deref-symlinks
            (process-file "file" nil t t "-L" "--" file)
          (process-file "file" nil t t "--" file))
        (when (bolp)
          (backward-delete-char 1))
        (buffer-string)))))

(defun my/kill-async-shell-buffers ()
  "Kill async shell command buffers.

With one \\[universal-argument] kill alive buffers also.

Useful for cleaning residual *Async Shell Command* buffers lying
around."
  (interactive)
  (let ((case-fold-search t))
    (seq-do (lambda (x)
              (when (and (buffer-name x) (string-match-p "\\*async shell command\\*" (buffer-name x))
                         (or current-prefix-arg
                             (not (get-buffer-process x))
                             (let ((case-fold-search nil))
                               (string-match-p "Shell" (format "%s" (get-buffer-process x))))))
                (if current-prefix-arg
                    (let (kill-buffer-query-functions)
                      (kill-buffer x))
                  (kill-buffer x))))
            (buffer-list))))
