(require 'package)
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")))
(setq package-check-signature nil)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  (when no-ssl (warn "\
Your version of Emacs does not support SSL connections,
which is unsafe because it allows man-in-the-middle attacks.
There are two things you can do about this warning:
1. Install an Emacs version that does support SSL and be safe.
2. Remove this warning from your init file so you won't see it again."))
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  (when (< emacs-major-version 24)
    (add-to-list 'package-archives (cons "gnu" (concat proto "://elpa.gnu.org/packages/")))))
(package-initialize)
(when (and (eq emacs-major-version 26) (eq emacs-minor-version 2))
  (setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3"))
(setq vc-follow-symlinks t)

(require 'package)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(yasnippet-snippets company-math auctex dired-rainbow string-inflection yaml-mode eldoc-overlay helm idle-highlight-mode elpy projectile find-file-in-project company flycheck yafolding iedit forge magit json-mode yaml a hl-todo smartparens smartscan dired-filter use-package cmake-mode))
 '(idle-highlight-idle-time 0.3)
 '(sp-override-key-bindings
   '(("C-M-f" . sp-forward-sexp)
     ("C-M-t" . sp-transpose-sexp)
     ("C-M-b" . sp-backward-sexp)
     ("C-M-u" . sp-backward-up-sexp)
     ("C-M-d" . sp-down-sexp)
     ("C-M-n" . sp-next-sexp)
     ("C-M-p" . sp-previous-sexp)
     ("C-M-k" . sp-kill-sexp)
     ("C-M-<backspace>" . sp-backward-kill-sexp)
     ("C-M-w" . sp-copy-sexp)
     ("C-M-i" . sp-splice-sexp-killing-backward)
     ("C-M-g" . sp-forward-barf-sexp)
     ("C-M-j" . sp-forward-slurp-sexp)
     ("C-M-q" . sp-unwrap-sexp))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((((type tty) (background dark)) (:background "black")) (t (:family "Liberation Mono" :background "ivory" :slant normal :weight normal :height 110 :width normal))))
 '(dired-mark ((t (:background "white" :foreground "black" :weight ultra-bold))))
 '(org-block ((t (:inherit fixed-pitch))))
 '(org-scheduled-today ((t (:foreground "DarkGreen" :weight bold)))))

;; Add Windows specific default fonts
(when (eq window-system 'w32)
  (custom-theme-set-faces
   'user
   '(default ((((type tty) (background dark)) (:background "black"))
              (t (:family "Cascadia Mono"
                          :background "ivory"
                          :slant normal
                          :weight normal
                          :height 100
                          :width normal)))))
  (set-face-attribute 'fixed-pitch nil :font "Cascadia Mono"))

(defvar emacs-minimal-config-style 'micro
  "Set the emacs minimal config style.
It is one of \\='(micro mini minimal compact more).

\\='micro means to install no external packages and only configure inbuilt
variables and add hacks to inbuilt packages.

\\='small means to install SOME packages only from melpa and add hacks for them.

\\='minimal means to install MORE packages only from melpa and add hacks for them.

\\='compact means to add custom packages (via git or other means) also.

\\='more means to add even MORE packages.")

(defvar emacs-minimal-config-style-to-num
  '((micro . 1) (small . 2) (minimal . 3) (compact . 4) (more . 5))
  "Symbol to number mapping for `emacs-minimal-config-style'.")

(setq emacs-minimal-config-style 'compact)

(load "~/emacs_config/set-vars.el")

(defun my/install-for (symb)
  (>= (assg emacs-minimal-config-style emacs-minimal-config-style-to-num)
      (assg symb emacs-minimal-config-style-to-num)))

(defvar my/emacs-libdir nil
  "Directory where custom emacs libraries are stored")

(defvar my/python-libdir nil
  "Directory where custom python libraries are stored")

(load "~/emacs_config/init-vars.el")
(load "~/emacs_config/misc-funcs-and-commands.el")


(load "~/emacs_config/ibuffer-dired.el")
; (load (concat (file-name-directory load-file-name) "ibuffer-dired.el"))

(when (my/install-for 'small)
  (setq confirm-kill-emacs 'y-or-n-p)
  (load "~/emacs_config/smartparens-smartscan-stuff.el") ; (expand-file-name "~/.emacs.d/smartparens-stuff.el")
  (load "~/emacs_config/hl-todo-stuff.el")
  (use-package a
    :ensure t
    :defer t)

  (use-package f
    :ensure t
    :defer t)
  (require 'f)

  (use-package yaml
    :ensure t
    :defer t)
  (load "~/emacs_config/json-yaml.el")
  (setq my/load-org-git nil)
  (load "~/emacs_config/git-stuff.el")
  (load "~/emacs_config/hacks-small.el"))

(when (my/install-for 'minimal)
  (use-package iedit
    :ensure t
    :defer t)
  (require 'iedit)

  (use-package yafolding
    :ensure t
    :defer t)

  (use-package flycheck
    :ensure t
    :defer t)

  (if window-system
      (global-set-key (kbd "C-.") #'iedit-mode)
    (global-set-key (kbd "C-x .") #'iedit-mode))
  (use-package company
    :ensure t
    :defer t
    :init (add-hook 'after-init-hook 'global-company-mode)
    :config
    (setq company-idle-delay              0.1
          company-tooltip-idle-delay      0.1
	  company-minimum-prefix-length   2
	  company-show-numbers            t
	  company-tooltip-limit           20
	  company-dabbrev-downcase        nil
          company-backends                '(company-capf company-files
                                                         company-gtags company-etags
                                                         company-keywords))
    :bind ("C-;" . company-complete-common))

  (use-package find-file-in-project
    :ensure t
    :defer t)

  (use-package projectile
    :ensure t
    :defer t)

  (load "~/emacs_config/lisp-stuff.el")

  (load "~/emacs_config/python-stuff.el")

  (use-package helm
    :ensure t
    :defer t)
  (when (package-installed-p 'helm)
    (setq helm-M-x-show-short-doc t)
    (setq helm-move-to-line-cycle-in-source nil)
    (global-set-key (kbd "M-x") 'helm-M-x))
  (load "~/emacs_config/hacks-minimal.el"))

(when (my/install-for 'compact)
  (load "~/emacs_config/latex-stuff.el")

  (use-package string-inflection
    :ensure t
    :defer t)

  ; Emacs util
  (let ((util-dir (f-join my/emacs-libdir "emacs-util")))
    (when (file-exists-p util-dir)
      (add-to-list 'load-path util-dir))
    (require 'util/all "util-all")
    (load "~/emacs_config/util-vars.el"))

  (when window-system
    (load "~/emacs_config/init-gui.el"))
  (load "~/emacs_config/hacks-compact.el"))

(when (my/install-for 'more)
  ; add mail mu4e?
  ; big library
  (when window-system
    (load "~/emacs_config/ref-man-stuff.el")))
