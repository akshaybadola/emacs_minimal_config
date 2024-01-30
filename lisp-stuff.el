(defun my/cask-build ()
  (interactive)
  (shell-command "rm *.elc")
  (if current-prefix-arg (byte-compile-file (buffer-file-name))
    (byte-recompile-directory default-directory 0 t nil t)))

;; Add orglink to a few modes
;; keybinding for python mode is also C-c C-o and already added in python-stuff.el
;; (require 'orglink)
;; (global-orglink-mode)
;; (setq orglink-activate-in-modes '(emacs-lisp-mode python-mode))

(defun my/elisp-mode-hook ()
  (setq-local company-idle-delay 0.2)
  (setq-local company-tooltip-idle-delay 0.2)
  (define-key emacs-lisp-mode-map (kbd "C-c C-c") #'my/cask-build)
  (define-key emacs-lisp-mode-map (kbd "C-c C-d") #'edebug-defun)
  (define-key emacs-lisp-mode-map (kbd "C-c C-o") #'org-open-at-point))

(add-hook 'lisp-mode-hook 'flycheck-mode)
(defun my/lisp-mode-hook ()
  (when (my/install-for 'minimal)
    (define-key prog-mode-map (kbd "C-c C-n")
                #'flycheck-next-error)
    (define-key prog-mode-map (kbd "C-c C-p")
                #'flycheck-previous-error)))
(add-hook 'lisp-mode-hook 'my/lisp-mode-hook)
(add-hook 'emacs-lisp-mode-hook 'my/lisp-mode-hook)
(add-hook 'emacs-lisp-mode-hook 'my/elisp-mode-hook)
;; FIXME: Somehow C-c C-d doesn't `edebug-defun' in *scratch*
(add-hook 'lisp-interaction-mode-hook 'my/elisp-mode-hook)
