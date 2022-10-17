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

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(a string-inflection f find-file-in-project dash flycheck-pycheckers flycheck helm util-ffip util-core jedi-core magit idle-highlight-mode hl-anything hl-todo yasnippet-snippets smartscan yafolding sphinx-doc virtualenvwrapper smartparens magit-todos jedi-direx iedit elpygen elpy company-jedi))
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
 )

(setq default-truncate-lines t)
(setq truncate-partial-width-windows nil)

;; And the command that caches long lines
(setq cache-long-line-scans t)
(global-set-key [f12] 'toggle-truncate-lines)

(setq select-active-regions nil)
(setq save-interprogram-paste-before-kill nil)
(setq x-select-enable-clipboard t)
(setq x-select-enable-primary t)

;; iedit set key
(require 'iedit)
(if window-system
    (global-set-key (kbd "C-.") #'iedit-mode)
  (global-set-key (kbd "C-x .") #'iedit-mode)
  (global-set-key (kbd "C-x ;") #'comment-line))


(setq-default indent-tabs-mode nil)

;; fill-column has to be 80, not whatever else
(setq-default fill-column 80)
;; (setq request-backend (quote url-retrieve))
(setq scroll-bar-mode (quote right))
(show-paren-mode t)

(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)

(setq ido-everywhere t)
(setq ido-max-directory-size 100000)
(ido-mode 'both)

; Use the current window when visiting files and buffers with ido
(setq ido-default-file-method 'selected-window)
(setq ido-default-buffer-method 'selected-window)
(setq ido-enable-flex-matching t)

;; org 9 ido or actually ido everywhere enhancement?
(setq ido-ubiquitous-mode t)

;; smartparens
;; Should set keybindings also
(smartparens-global-mode t)

(add-hook 'python-mode-hook 'hl-todo-mode)
(add-hook 'emacs-lisp-mode-hook 'hl-todo-mode)
(add-hook 'js-mode-hook 'hl-todo-mode)
(setq hl-todo-keyword-faces
      (quote (("TODO" :foreground "red" :weight bold)
              ("FIXED" :foreground "forest green" :weight bold)
              ("DONE" :foreground "forest green" :weight bold)
              ("HOLD" :foreground "#d0bf8f" :weight bold)
              ("NEXT" :foreground "#dca3a3" :weight bold)
              ("THEM" :foreground "#dc8cc3" :weight bold)
              ("PROG" :foreground "#7cb8bb" :weight bold)
              ("OKAY" :foreground "#7cb8bb" :weight bold)
              ("DONT" :foreground "#5f7f5f" :weight bold)
              ("FAIL" :foreground "#8c5353" :weight bold)
              ("DONE" :foreground "#afd8af" :weight bold)
              ("NOTE" :foreground "#d0bf8f" :weight bold)
              ("KLUDGE" :foreground "#d0bf8f" :weight bold)
              ("HACK" :foreground "#d0bf8f" :weight bold)
              ("TEMP" :foreground "#d0bf8f" :weight bold)
              ("FIXME" :foreground "red" :weight bold)
              ("XXX+" :foreground "#cc9393" :weight bold)
              ("\\?\\?\\?+" :foreground "#cc9393" :weight bold))))
;; (seq-do (lambda (x) (package-install x)) '(hl-anything hl-todo yasnippet-snippets smartscan yafolding sphinx-doc virtualenvwrapper smartparens magit-todos jedi-direx iedit elpy company-jedi idle-highlight-mode))


;; hideshow enabled in python-mode
(add-hook 'python-mode-hook #'hs-minor-mode)
(add-hook 'python-mode-hook #'yafolding-mode)
(add-hook 'python-mode-hook #'smartparens-mode)
; (add-hook 'python-mode-hook #'sphinx-doc-mode)

(condition-case nil
    (require 'idle-highlight-mode)
  (defface idle-highlight
    '((t (:background "grey90")))
    "Face used to highlight other occurrences of the word at point.")
  (defun idle-highlight-word-at-point ()
    "Highlight the word under the point."
    (if idle-highlight-mode
        (let ((target (and (thing-at-point 'symbol)
                           (substring-no-properties (thing-at-point 'symbol)))))
          (idle-highlight-unhighlight)
          (when (and target
                     (not (in-string-p))
                     ;; (looking-at-p "\\s_\\|\\sw") ;; Symbol characters
                     (not (member target idle-highlight-exceptions)))
            ;; NOTE: Was (concat "\\<" (regexp-quote target) "\\>")
            (setq idle-highlight-regexp (concat "\\_<" (regexp-quote target) "\\_>"))
            (highlight-regexp idle-highlight-regexp 'idle-highlight)))))
  (error nil))

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

(use-package smartscan
  :ensure t
  :defer t)
(require 'smartscan)
(global-smartscan-mode 1)
(setq smartscan-use-extended-syntax t)
(setq smartscan-symbol-selector "symbol")
(defun my/disable-smartscan ()
  (smartscan-mode -1))
(add-hook 'comint-mode-hook 'my/disable-smartscan)

(require 'smartparens-config)
(smartparens-global-mode t)
(sp-with-modes '(markdown-mode gfm-mode)
  (sp-local-pair "$" "$")
  (sp-local-pair "$$" "$$"))

(sp-with-modes 'org-mode
  (sp-local-pair "$" "$")
  (sp-local-pair "$$" "$$"))

(sp-with-modes 'rst-mode
  (sp-local-pair ":" ":")
  (sp-local-pair "<" ">"))

(sp-with-modes 'python-mode
  (sp-local-pair ":" ":" :when '(sp-in-docstring-p)))

(setq python-shell-interpreter "python3")
(require 'elpy)
(elpy-enable)
(setq elpy-rpc-python-command "python3")
(setq elpy-rpc-backend "jedi")

(defun my/elpy-shell-send-region-or-buffer-and-step ()
  (interactive)
  (when current-prefix-arg
    (elpy-shell-kill t))
  (elpy-shell-send-region-or-buffer-and-step))

(setq elpy-rpc-timeout 10)

(setq python-shell-completion-native-enable nil)
(when (require 'flycheck nil t)
    (setq elpy-modules (delq 'elpy-module-flymake elpy-modules)))
(remove-hook 'flymake-diagnostic-functions 'flymake-proc-legacy-flymake)

(defun my/elpy-mode-hook ()
  (auto-fill-mode -1)
  (add-hook 'elpy-mode-hook 'flycheck-mode)
  (setq flycheck-pycheckers-checkers '(flake8 mypy3))
  (setq flycheck-pycheckers-max-line-length 100)
  (setq flycheck-pycheckers-ignore-codes (split-string "E226,E303,H306,W503,W504" "," t))
  (with-eval-after-load 'flycheck
    (add-hook 'flycheck-mode-hook #'flycheck-pycheckers-setup))
  (define-key python-mode-map (kbd "C-c C-n")
    #'flycheck-next-error)
  (define-key python-mode-map (kbd "C-c C-p")
    #'flycheck-previous-error)
  (define-key elpy-mode-map (kbd "C-c C-n")
    #'flycheck-next-error)
  (define-key elpy-mode-map (kbd "C-c C-p")
    #'flycheck-previous-error)
  ;; (setq-local current-word-highlight-context nil)
  (highlight-indentation-mode 1)
  (setq elpy-formatter 'autopep8)
  (define-key elpy-mode-map (kbd "C-c C-c")
    'my/elpy-shell-send-region-or-buffer-and-step)
  (define-key elpy-mode-map (kbd "C-c C-h") #'helm-semantic-or-imenu)
  (define-key elpy-mode-map (kbd "C-c C-o") #'org-open-at-point)
  (define-key elpy-mode-map (kbd "C-M-i") #'sp-splice-sexp-killing-backward))

(add-hook 'elpy-mode-hook 'flycheck-mode)
(add-hook 'elpy-mode-hook 'my/elpy-mode-hook)

(defun elpy--fix-code-with-formatter (method)
  "Common routine for formatting python code."
  (let ((line (line-number-at-pos))
        (col (current-column))
        (directory (if (elpy-project-root)
                       (expand-file-name (elpy-project-root))
                     default-directory)))
    (if (use-region-p)
        (let* ((beg (region-beginning))
               (end (region-end))
               (offset (progn (save-excursion
                                (save-restriction
                                  (narrow-to-region beg end)
                                  (goto-char beg)
                                  (while (string-match-p "^ *$" (buffer-substring-no-properties
                                                                 (line-beginning-position)
                                                                 (line-end-position)))
                                    (forward-line))
                                  (beginning-of-line)
                                  (setq beg (point))
                                  (prog1 (string-join (-repeat (current-indentation) " "))
                                    (when (re-search-forward "^ *$" nil t)
                                      (forward-line -1)
                                      (end-of-line)
                                      (setq end (point))))))))
               (new-block (elpy-rpc method
                                    (list (replace-regexp-in-string
                                           (concat "^" offset) ""
                                           (buffer-substring-no-properties beg end))
                                          directory))))
          (setq new-block (replace-regexp-in-string
                           "^\\(.+\\)"
                           (concat offset "\\1") new-block))
          (elpy-buffer--replace-region
           beg end (string-trim-right new-block))
          (goto-char end)
          (deactivate-mark))
      (let ((new-block (elpy-rpc method
                                 (list (elpy-rpc--buffer-contents)
                                       directory)))
            (beg (point-min))
            (end (point-max)))
        (elpy-buffer--replace-region beg end new-block)
        (when (bobp)
          (forward-line (1- line))
          (forward-char col))))))

(use-package f
  :ensure t
  :defer t)
(require 'f)
(let ((util-dir (expand-file-name "~/lib/emacs-util")))
  ;; (if (f-exists-p util-dir)
  ;;     (let ((default-directory (expand-file-name "~/emacs-util")))
  ;;       (async-shell-command "git pull --rebase"))
  ;;   (let ((default-directory (expand-file-name "~")))
  ;;     (async-shell-command "git clone https://github.com/akshaybadola/emacs-util")))
  ;; (when (and (f-exists? util-dir) (not (package-installed-p 'util)))
  ;;   (package-install-file (f-join util-dir "util.el")))
  ;; (when (package-installed-p 'util)
  ;;   (require 'util))

  (when (f-exists? util-dir)
    (add-to-list 'load-path util-dir))
  (require 'util/all "util-all")
  )


;; dired

(setq dired-listing-switches "-alh --group-directories-first")
(setq dired-recursive-deletes 'always)
(setq dired-recursive-copies 'always)
(setq dired-deletion-confirmer #'y-or-n-p)
(setq dired-show-hidden-p nil)
(setq dired-toggle-extension-list nil)
