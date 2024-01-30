(use-package auctex
    :ensure t
    :defer t)
(use-package company-math
    :ensure t
    :defer t)

(defun my/helm-texdoc (pkg)
  "Search and display TeX documentation with `helm'."
  (interactive (let ((pkg (read-from-minibuffer "Package Name? ")))
                 (if (string-empty-p pkg)
                     (user-error "pkg name cannot be empty")
                   (list pkg))))
  (helm :sources
        (helm-build-sync-source "Test"
          :candidates (let ((files (-keep (lambda (x)
                                            (when (and x (string-match-p "\\.pdf" x))
                                              (let ((it (nth 2 (split-string (string-trim x)))))
                                                (and it (not (string-match-p "news\\|readme\\|bugs\\|changes" it)) it))))
                                          (split-string
                                           (shell-command-to-string (format "texdoc -l -M %s" pkg)) "\n"))))
                        (mapcar (lambda (f) `(,f . ,f)) files))
          :action (helm-make-actions
                   "View File" (lambda (file) (interactive) (find-file file))))))

(defun my/LaTeX-env-frame (env)
  (LaTeX-env-label env)
  (LaTeX-env-label "block")
  (TeX-insert-braces nil)
  (insert  "\\center "))

(defun my/LaTeX-env-image (env)
  (LaTeX-env-args "center")
  (LaTeX-env-label "figure")
  (insert  "\\centering\n")
  (insert "\\includegraphics[]{}")
  (indent-according-to-mode)
  (insert "\n\\caption{}")
  (indent-according-to-mode)
  (previous-line)
  (goto-char (- (pos-eol) 1)))

(defun my/latex-include-graphics ()
  (interactive)
  (LaTeX-env-args "center")
  (LaTeX-env-label "figure")
  (insert  "\\centering\n")
  (insert "\\includegraphics[]{}")
  (indent-according-to-mode)
  (insert "\n\\caption{}")
  (indent-according-to-mode)
  (previous-line)
  (goto-char (- (pos-eol) 1)))

(defun my/LaTeX-env-block (env)
  (LaTeX-env-label "block")
  (TeX-insert-braces nil)
  (insert  "\\center "))

(defun beamer-footfullcite (args)
  (interactive
   (list (read-string (format "Citekeys [%s]: " args-beamer-footfullcite)
                      nil nil args-beamer-footfullcite)))
  (setq args-beamer-footfullcite args)
  (setq args-list (split-string args))
  (if (= (length args-list) 1)
      (insert (format "\\footfullcite{%s}" args-beamer-footfullcite))
    (insert (string-join (mapcar (lambda (arg)
                                   (format "\\footfullcite{%s}" arg))
                                 (split-string args)) "$^,$"))))

;; Can modify this so the block is inserted only at the end of the current frame
;; Should be easy enough.
(defun beamer-insert-frame-with-title (args)
  (interactive
   (list (read-string (format "Frametitle [%s]: " args-beamer-frame-title)
                      nil nil args-beamer-frame-title)))
  (setq args-beamer-frame-title args)
  (insert (format "\n\\begin{frame}{%s}\n\\begin{block}\n{\\center }\n\\end{block}\n\\begin{itemize}\n\\item\n\\end{itemize}\n\\end{frame}\n\n" args-beamer-frame-title))
  (previous-line 3)
  ;; (LaTeX-mark-environment)
  (LaTeX-mark-environment)
  (indent-region (region-beginning) (region-end))
  (next-line 5)
  (move-end-of-line 1)
  (insert " ")
  (previous-line 3)
  (move-end-of-line 1)
  (backward-char)
  (deactivate-mark))

(defun add-latex-math-to-company ()
  (setq-local company-backends
              (append '(company-math-symbols-latex company-latex-commands)
                      company-backends)))

(defun my/run-tikz-tex ()
  (call-process-shell-command (format "cd .. && pdflatex tex/%s" (buffer-name))))

(defun my/compile-latex (biber bibtex)
  (call-process-shell-command
   (cond (biber
          (format "pdflatex -nonstopmode %s && biber %s && pdflatex -nonstopmode %s" (buffer-name)))
         (bibtex
          (format "pdflatex -nonstopmode %s && bibtex %s && pdflatex -nonstopmode %s" (buffer-name)))
         (t (format "pdflatex -nonstopmode %s" (buffer-name))))))

(defun my/compile-latex-only ()
  (interactive)
  (my/compile-latex nil nil))

(defun my/compile-latex-with-biber ()
  (interactive)
  (my/compile-latex t nil))

(defun my/compile-latex-with-bibtex ()
  (interactive)
  (my/compile-latex nil t))

(defun my/LaTeX-mode-hook ()
  (LaTeX-add-environments '("frame" my/LaTeX-env-frame))
  (LaTeX-add-environments '("block" my/LaTeX-env-block))
  (LaTeX-add-environments '("image" my/LaTeX-env-image))
  (yas-minor-mode-on)
  (add-latex-math-to-company)
  (bind-key (kbd "C-c C-x C-l") 'org-latex-preview LaTeX-mode-map)
  (bind-key (kbd "C-c C-n") 'flycheck-next-error LaTeX-mode-map)
  (bind-key (kbd "C-c C-p") 'flycheck-previous-error LaTeX-mode-map)
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward "^\\\\usepackage{tikz}" nil t)
      (add-hook 'after-save-hook 'my/run-tikz-tex nil t))))

(add-hook 'LaTeX-mode-hook 'my/LaTeX-mode-hook)

;; some math stuff
(defun latex-math-insert-diff ()
  (interactive)
  (insert "\\frac{\\text{d}}{\\text{d}x}")
  (backward-char 12))

(defun my/latex-increment-all-argument-numbers ()
  "Increment all the macro argument numbers in active region by 1.

If `\\[universal-argument]' is given, then decrement."
  (interactive)
  (when (region-active-p)
    (save-excursion
      (save-restriction
        (narrow-to-region (region-beginning) (region-end))
        (goto-char (point-min))
        (while (re-search-forward "{#\\([0-9]+?\\)}" nil t)
          (let ((repl (number-to-string
                       (if current-prefix-arg
                           (1- (string-to-number (match-string 1)))
                         (1+ (string-to-number (match-string 1)))))))
            (replace-match (format "{#%s}" repl))))))))
