(use-package 'pydoc
    :ensure t
    :defer t)

(defun my/elpy-mode-hook-more ()
  (define-key elpy-mode-map (kbd "C-c l") nil)
  (define-key elpy-mode-map (kbd "C-c l r") #'my/python-fix-docstring)
  (define-key elpy-mode-map (kbd "C-c l a") #'my/python-doc-sphinx-attr)
  (define-key elpy-mode-map (kbd "C-c l c") #'my/python-doc-sphinx-code)
  (define-key elpy-mode-map (kbd "C-c l C") #'my/python-doc-sphinx-class)
  (define-key elpy-mode-map (kbd "C-c l m") #'my/python-doc-sphinx-method)
  (define-key elpy-mode-map (kbd "C-c l f") #'my/python-doc-sphinx-func)
  (define-key elpy-mode-map (kbd "C-c l M") #'my/python-doc-sphinx-mod)
  (define-key elpy-mode-map (kbd "C-c M-d") #'my/sphinx-doc))
(add-hook 'elpy-mode-hook 'my/elpy-mode-hook-more)

(defun my/pydoc-at-point ()
  "Try to get `pydoc' for thing at point.
Returns the full html rendering of the pydoc instead of a brief
summary. Requires `elpy', python package jedi and `pydoc' to be
available as a command.

The result is pulled from the source and is fetched from jedi
using `elpy', so it relies on `elpy' for now."
  (interactive)
  (let* ((docstring (elpy-rpc-get-docstring))
         (name (and (string-match "Documentation for \\(.+\\):" docstring)
                    (match-string 1 docstring))))
    (unless (string-empty-p name)
      (pydoc-with-help-window (pydoc-buffer)
        (call-process-shell-command
         (concat pydoc-command " " name)
         nil standard-output)))))

(defvar my/python-indentation 4)
(defun my/sphinx-doc ()
  "Insert docstring for the Python function definition at point.
This is an interactive function and the docstring generated is as
per the requirement of Sphinx documentation generator."
  (interactive)
  (unless (python-info-looking-at-beginning-of-defun)
    (python-nav-beginning-of-defun))
  (beginning-of-line)
  (if (looking-at "^ *async def")
      (forward-word 2)
    (forward-word))
  (let* ((context (my/jedi-get-context (buffer-file-name)
                                       (line-number-at-pos)
                                       (current-column)))
         (func-name (car context))
         (params
          (mapcar (lambda (x)
                    (pcase-let* ((`(,name ,type) (split-string x ";"))
                                 (type (if type
                                           (split-string
                                            (replace-regexp-in-string "^: +" "" (string-trim type)) "=")
                                         '(""))))
                      (make-sphinx-doc-arg :name (string-trim name)
                                           :default (nth 1 type)
                                           :type (unless (string-empty-p (car type))
                                                   (car type)))))
                  (cdr context)))
         (fd (make-sphinx-doc-fndef
              :name func-name
              :args params))
         (indent (+ (save-excursion
                      (python-nav--beginning-of-defun)
                      (sphinx-doc-current-indent))
                    my/python-indentation))
         (old-ds (sphinx-doc-existing))
         (new-ds (sphinx-doc-fndef->doc fd)))
    (when old-ds (sphinx-doc-kill-old-doc indent))
    ;; FIXME: There's a bug here where fields aren't merged
    ;;        correctly. Basically the sections have be parsed with (or
    ;;        like) napoleon
    ;;
    ;;        There's a package [[~/.emacs.d/elpa/python-docstring-20190716.921/python-docstring.el][python-docstring]] which fontifies the python
    ;;        docstring that may also be helpful. But again has to be changed
    ;;        for google style doc strings.
    (sphinx-doc-insert-doc
     (if old-ds (sphinx-doc-merge-docs old-ds new-ds) new-ds) t)
    (sphinx-doc-indent-doc indent)))

(defun my/python-sphinx-quotes-to-code ()
  (interactive)
  (my/sphinx-quotes-to-code-in-region #'sphinx-doc-util-python-docstring-bounds))

(defun my/python-doc-sphinx-code ()
  (interactive)
  (my/sphinx-doc-identifier ":code:"))

(defun my/python-doc-sphinx-class ()
  (interactive)
  (my/sphinx-doc-identifier ":class:"))

(defun my/python-doc-sphinx-method ()
  (interactive)
  (my/sphinx-doc-identifier ":meth:"))

(defun my/python-doc-sphinx-func ()
  (interactive)
  (my/sphinx-doc-identifier ":func:"))

(defun my/python-doc-sphinx-mod ()
  (interactive)
  (my/sphinx-doc-identifier ":mod:"))

(defun my/python-doc-sphinx-attr ()
  (interactive)
  (my/sphinx-doc-identifier ":attr:"))

(defun my/python-fix-docstring ()
  (interactive)
  (my/python-sphinx-quotes-to-code)
  (my/python-doc-fix-alignment))

(defun my/python-doc-fix-alignment ()
  (interactive)
  (pcase-let ((`(,start . ,end) (sphinx-doc-util-python-docstring-bounds)))
    (save-excursion
      (save-restriction
        (narrow-to-region start end)
        (goto-char (point-min))
        (let ((prev-line (buffer-substring-no-properties (point-at-bol) (point-at-eol)))
              flag spaces)
          (while (not (eobp))
            (forward-line)
            (when (string-match "^\\( +[a-z][a-zA-Z0-9_]+\\): \\(.+\\)" prev-line)
              (setq spaces (make-string (+ 2 (- (match-end 1) (match-beginning 1))) ? ))
              (setq flag t))
            (when flag
              (when (looking-at "^\\( +\\)\\([a-z][a-zA-Z0-9_]+\\): \\(.+\\)" t)
                (setq flag nil)))
            (when  (string-match-p "^$" prev-line)
              (setq spaces nil))
            (when (and flag (looking-at "^\\( +\\)\\(.+[^\"\"\"]\\)$") spaces)
              (replace-match (format "%s\\2" spaces)))
            (setq prev-line (buffer-substring-no-properties (point-at-bol) (point-at-eol)))))))))

(defvar elpy-enable-debug-output nil)
(defvar elpy-debug-buffer "*elpy-debug*")

(defun elpy-debug-enable (&optional arg)
  (interactive)
  (cond ((or arg current-prefix-arg)
         (message "Elpy debug output disabled")
         (setq elpy-enable-debug-output nil))
        (t
         (message "Elpy debug output enabled")
         (setq elpy-enable-debug-output t))))

(defun elpy-maybe-debug (str)
  (when (and elpy-enable-debug-output elpy-debug-buffer)
    (with-current-buffer (get-buffer-create elpy-debug-buffer)
      (goto-char (point-max)))
      (insert (format (time-stamp-string "[%Y-%m-%d %a %H:%M:%S]") "%s\n%s\n") str)))

(setq python-shell-completion-native-enable nil)
