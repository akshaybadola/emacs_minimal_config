(require 'f)

(add-hook 'python-mode-hook #'hs-minor-mode)
(add-hook 'python-mode-hook #'yafolding-mode)
(add-hook 'python-mode-hook #'smartparens-mode)

(defvar ob-ipython-python-path "/usr/bin/python"
  "Path for the `python' executable.")

(remove-hook 'flymake-diagnostic-functions 'flymake-proc-legacy-flymake)

(when (my/install-for 'compact)
  (if (not (file-exists-p (expand-file-name (concat my/emacs-libdir "/" "flycheck-pycheckers"))))
      (warn "flycheck-pycheckers not found")
    (add-to-list 'load-path (expand-file-name (concat my/emacs-libdir "/" "flycheck-pycheckers")))
    (require 'flycheck-pycheckers)
    (flycheck-pycheckers-setup))
  (setq flycheck-pycheckers-checkers '(flake8 mypy3)) ; Change to '(flake8 mypy3) as required
  (setq flycheck-pycheckers-max-line-length 100)
  (setq flycheck-pycheckers-ignore-codes (split-string
                                          (string-join '("E226,E303,H306,W503,W504" ; flake8 rules
                                                         "Q00,PTH,D,ANN,T201,ERA001,UP006")
                                                       ",") ; ruff rules
                                          "," t))
  (with-eval-after-load 'flycheck
    (add-hook 'flycheck-mode-hook #'flycheck-pycheckers-setup)))

(use-package elpy
  :ensure t
  :defer t)
(require 'elpy)
(elpy-enable)
(when (load "flycheck" t t)
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  (add-hook 'elpy-mode-hook 'flycheck-mode)
  (setq flycheck-python-flake8-executable "flake8")
  (define-key elpy-mode-map (kbd "C-c C-n")
    #'flycheck-next-error)
  (define-key elpy-mode-map (kbd "C-c C-p")
    #'flycheck-previous-error))
(setq elpy-rpc-timeout 10)

;; NOTE: kill-buffer at the end of `elpy-multiedit-stop'
(defun elpy-multiedit-stop ()
  "Stop editing multiple places at once."
  (interactive)
  (dolist (ov elpy-multiedit-overlays)
    (delete-overlay ov))
  (setq elpy-multiedit-overlays nil)
  (if (get-buffer "*Elpy Edit Usages*")
      (kill-buffer "*Elpy Edit Usages*")))


;; NOTE: Fix overlapping strings detected while navigation
(setq elpy-eldoc-show-current-function nil)
(setq elpy-rpc-ignored-buffer-size 1024000)
;; (setq elpy-rpc-backend "jedi")

(setq elpy-test-runner 'elpy-test-pytest-runner)

(defun my/python-test-file ()
  (and (string-prefix-p "test" (f-filename (buffer-file-name)))
    (let ((buf-string (buffer-string))
          (case-fold-search nil))
      (or (string-match-p "^import pytest" buf-string)
          (string-match-p "^import unittest" buf-string)
          (string-match-p "^from unittest import TestCase" buf-string)
          (string= (f-base (f-dirname (buffer-file-name))) "tests")))))

(defun my/elpy-test-run (working-directory command &rest args)
  "Run COMMAND with ARGS in WORKING-DIRECTORY as a test command."
  (let ((default-directory (f-parent working-directory)))
    (funcall elpy-test-compilation-function
             (mapconcat #'shell-quote-argument
                        (cons command args)
                        " "))))

(defun my/elpy-test-pytest-runner (top file module test &optional debug)
  "Test the project using the py.test test runner.

This requires the pytest package to be installed."
  (interactive (elpy-test-at-point))
  (let ((pytest-runner-command
         (if debug
             (list (car elpy-test-pytest-runner-command) "--pdb")
           elpy-test-pytest-runner-command))
        (elpy-test-compilation-function (if debug 'pdb 'compile)))
    (cond
     (test
      (let ((test-list (split-string test "\\.")))
        (apply #'my/elpy-test-run
               top
               (append pytest-runner-command
                       (list (mapconcat #'identity
                                        (cons file test-list)
                                        "::"))))))
     (module
      (apply #'my/elpy-test-run top (append pytest-runner-command
                                         (list file))))
     (t
      (apply #'my/elpy-test-run top pytest-runner-command)))))

(setq elpy-test-runner 'my/elpy-test-pytest-runner)
(setq elpy-test-compilation-function 'compile)

(defun my/elpy-test (&optional test-whole-project debug)
  "Run tests on the current test, or the whole project.

If there is a test at point, run that test. If not, or if a
prefix is given, run all tests in the current project."
  (interactive "P")
  (let ((gud-pdb-command-name "pytest --pdb")
        (current-test (elpy-test-at-point)))
    (if test-whole-project
        ;; With prefix arg, test the whole project.
        (funcall elpy-test-runner
                 (car current-test)
                 nil nil nil)
      ;; Else, run only this test
      (apply elpy-test-runner (-concat current-test current-prefix-arg)))))

(defun my/elpy-shell-send-region-or-buffer-and-step ()
  (interactive)
  (save-excursion
    (cond ((my/python-test-file)
           (my/elpy-test))
          (current-prefix-arg
           (elpy-shell-kill t)
           (elpy-shell-send-region-or-buffer-and-step))
          (t (elpy-shell-send-region-or-buffer-and-step)))))

(defun my/python-type-ignore ()
  (interactive)
  (comment-indent)
  (insert "type: ignore"))

(unless (version< emacs-version "27")
  (use-package idle-highlight-mode
    :ensure t
    :defer t))
(defun my/elpy-mode-hook ()
  (auto-fill-mode -1)
  ;; (setq-local current-word-highlight-context nil)
  (setq-local flycheck-checker-error-threshold 2000)
  (unless (version< emacs-version "27")
    (idle-highlight-mode 1))
  (highlight-indentation-mode 1)
  (define-key elpy-mode-map (kbd "C-c C-c")
    'my/elpy-shell-send-region-or-buffer-and-step)
  (define-key elpy-mode-map (kbd "C-c d")
    #'my/pydoc-at-point)
  (setq elpy-formatter 'autopep8)
  (define-key elpy-mode-map (kbd "C-c C-h") #'helm-semantic-or-imenu)
  (define-key elpy-mode-map (kbd "C-c C-o") #'org-open-at-point)
  (define-key elpy-mode-map (kbd "C-M-i") #'sp-splice-sexp-killing-backward)
  (when (load "flycheck" t t)
    (define-key elpy-mode-map (kbd "C-c C-n")
      #'flycheck-next-error)
    (define-key elpy-mode-map (kbd "C-c C-p")
      #'flycheck-previous-error))
  (define-key elpy-mode-map (kbd "C-c t") #'my/python-type-ignore))
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
                                  (string-join (-repeat (current-indentation) " "))
                                  ;; NOTE: (string-join (-repeat (current-indentation) " "))
                                  ;;       is returned and end is changed, which has become buggy
                                  ;;
                                  ;; (prog1 (string-join (-repeat (current-indentation) " "))
                                  ;;   (when (re-search-forward "^ *$" nil t)
                                  ;;     (forward-line -1)
                                  ;;     (end-of-line)
                                  ;;     (setq end (point))))
                                  ))))
               (new-block (elpy-rpc method
                                    (list (replace-regexp-in-string
                                           (concat "^" offset) ""
                                           (elpy-rpc--region-contents))
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

(defun my/python-occur-fstrings ()
  (interactive)
  (occur "[^f]\"[^\"\\w]+?{[^\"]+?}.*?\""))

(when (my/install-for 'more)
  (load (f-expand "~/.emacs.d/python-more.el")))
