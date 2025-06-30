;; Webkit browser only for gui mode
;; The python programs are buggy and not very useful
;; Maybe one day I'll fix that code.
;; (add-to-list 'load-path (expand-file-name "~/.emacs.d/webkit/"))
;; (require 'webkit)

;; Eaf based pyqt browser
;; I had totally forgotten about this!!
;;
;; Though the keybindings and other commands will have to be
;; configured separately.
;; (add-to-list 'load-path (expand-file-name "~/.emacs.d/eaf/"))
;; (require 'eaf)

(setq-default menu-bar-mode nil
              tool-bar-mode nil
              scroll-bar-mode nil)
(setq menu-bar-mode nil
      tool-bar-mode nil
      scroll-bar-mode nil)

(use-package desktop+
  :ensure t
  :defer t)
(require 'desktop+)
(setq desktop-auto-save-timeout nil)
(desktop-save-mode 1)
(setq gc-cons-threshold (* 100 1024 1024))
(defun my-desktop-save ()
  (interactive)
  ;; Don't call desktop-save-in-desktop-dir, as it prints a message.
  (if (eq (desktop-owner) (emacs-pid))
      (desktop-save desktop-dirname)))
(add-hook 'auto-save-hook 'my-desktop-save)
(if (server-running-p)
    (if (y-or-n-p "Emacs Server is already running. Should we continue? ")
        (message "Continuing with startup")
      (kill-emacs))
  (server-start))

(when (package-installed-p 'ace-window)
  (require 'ace-window)
  (global-set-key (kbd "M-o") 'ace-window))

(use-package pdf-tools
  :ensure t
  :defer t)
;; pdf tools install
(pdf-tools-install)

(defun my/pdf-view-copy-file-name ()
  (interactive)
  (if (buffer-file-name)
      (progn (kill-new (buffer-file-name))
             (message "Copied %s" (buffer-file-name)))
    (when (string-equal (buffer-name) "*eww pdf*")
      (with-current-buffer (get-buffer "*eww*") (setq my/last-pdf-url (plist-get (nth 0 eww-history) :url))
                           (setq j 1)
                           (while (and (not (string-match-p "pdf" my/last-pdf-url))
                                       (< j (length eww-history)))
                             (setq my/last-pdf-url (plist-get (nth j eww-history) :url))
                             (setq j (+ 1 j))))
      (when (string-match-p "pdf" my/last-pdf-url)
        (let ((filename (my/filename-from-url my/last-pdf-url)))
          (kill-new filename)
          (write-file filename)
          (message "Copied %s and written to file" filename))))))

(setq my/pdf-view-external-viewer "mupdf")
(setq my/pdf-view-external-viewer-alt "evince")
(defun my/pdf-view-open-with-external-viewer ()
  (interactive)
  (let* ((command (if current-prefix-arg
                      my/pdf-view-external-viewer-alt
                    my/pdf-view-external-viewer))
         (fname (buffer-file-name))
         (name (format "%s %s" command (file-name-nondirectory fname))))
    (if (or current-prefix-arg
            (y-or-n-p (format "Open file with %s? " command)))
        (progn
          (message "Opening %s with %s" (file-name-nondirectory fname) command)
          (start-process name (concat "*" name "*") command fname))
      (message "Aborted"))))

(defvar browse-url-webmacs-program
  "webmacs"
  "Command name for `webmacs' browser.")
(setq browse-url-webmacs-program (executable-find "webmacs"))
(defun browse-url-webmacs (url &optional _new-window)
  "Ask the webmacs browser to load URL.
Default to the URL around or before point.  The optional argument
NEW-WINDOW is not used."
  (interactive (browse-url-interactive-arg "URL: "))
  (setq url (browse-url-encode-url url))
  (let* ((process-environment (browse-url-process-environment)))
    ;; Check if process is already running
    (apply 'start-process
	   (concat "webmacs " url) nil
	   browse-url-webmacs-program
	   (list url))))

(defun my-pdf-view-mode-hook ()
  (smartscan-mode -1)                   ; no need for smartscan for pdf-view-mode
  (define-key pdf-view-mode-map (kbd "n")
    'pdf-view-next-line-or-next-page)
  (define-key pdf-view-mode-map (kbd "p")
    'pdf-view-previous-line-or-previous-page)
  (define-key pdf-view-mode-map (kbd "h")
    'image-backward-hscroll)
  (define-key pdf-view-mode-map (kbd "l")
    'image-forward-hscroll)
  (define-key pdf-view-mode-map (kbd "v")
    'my/pdf-view-open-with-external-viewer)
  (define-key pdf-view-mode-map (kbd "j")
    'pdf-view-next-line-or-next-page)
  (define-key pdf-view-mode-map (kbd "k")
    'pdf-view-previous-line-or-previous-page)
  (define-key pdf-view-mode-map (kbd "w")
    'my/pdf-view-copy-file-name)
  (define-key pdf-view-mode-map (kbd "M-n")
    'pdf-view-next-page-command)
  (define-key pdf-view-mode-map (kbd "M-p")
    'pdf-view-previous-page-command)
  (define-key pdf-view-mode-map (kbd "C-c r r") 'ref-man-get-references))

(add-hook 'pdf-view-mode-hook 'my-pdf-view-mode-hook)
(add-hook 'pdf-view-mode-hook 'auto-revert-mode)

(defun my-image-mode-hook ()
  (define-key image-mode-map (kbd "Q") 'kill-this-buffer))
(add-hook 'image-mode-hook 'my-image-mode-hook)

(load "~/emacs_config/org-stuff.el")
