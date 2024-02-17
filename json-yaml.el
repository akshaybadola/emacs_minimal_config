(require 'a)
(require 'yaml)
(require 'json)

(when (my/install-for 'minimal)
  (use-package json-mode
    :ensure t
    :defer t)
  (require 'json-mode)
  (use-package yaml-mode
    :ensure t
    :defer t)
  (require 'yaml-mode))

(add-hook 'yaml-mode-hook 'highlight-indentation-mode)
(add-hook 'yaml-mode-hook 'my/yaml-mode-hook)

(defun my/yaml-mode-hook ()
  (define-key yaml-mode-map (kbd "TAB") #'my/yaml-indent-forward)
  (define-key yaml-mode-map (kbd "<backtab>") #'my/yaml-indent-backward)
  (define-key yaml-mode-map (kbd "M-RET") #'my/yaml-insert-list-item))

(defun my/yaml-insert-list-item ()
  (interactive)
  (let ((at-sequence (save-excursion
                       (goto-char (point-at-bol))
                       (looking-at "^\\( *\\)-.*$")
                       (and (match-string 1) (length (match-string 1))))))
    (when at-sequence
      (insert "\n-")
      (indent-line-to at-sequence)
      (end-of-line))))

(defun my/yaml-indent-forward ()
  (interactive)
  (my/yaml-electric-indent))

(defun my/yaml-indent-backward ()
  (interactive)
  (my/yaml-electric-indent t))

;; TODO: point should move according to indent
(defun my/yaml-electric-indent (&optional backtab)
  (interactive)
  (let* ((back-indent (or backtab current-prefix-arg))
         (pos (point))
         (beg (if (region-active-p)
                  (region-beginning)
                (point-at-bol)))
         (end (if (region-active-p)
                  (region-end)
                (point-at-eol)))
         (computed (save-mark-and-excursion
                       (goto-char beg)
                       (yaml-compute-indentation)))
         (cur-indent (save-mark-and-excursion
                       (goto-char beg)
                       (current-indentation)))
         (prev-indent (save-mark-and-excursion
                        (goto-char beg)
                        (previous-line)
                        (current-indentation))))
    (cond ((= (- cur-indent prev-indent)
              yaml-indent-offset)
           (if (region-active-p)
               (indent-rigidly beg end (- yaml-indent-offset))
             (indent-line-to prev-indent)))
          (back-indent
           (if (region-active-p)
               (indent-rigidly beg end (- yaml-indent-offset))
             (indent-line-to (- cur-indent 2))))
          ((< cur-indent prev-indent)
           (if (region-active-p)
               (indent-rigidly beg end (- computed cur-indent))
             (indent-line-to computed)))
          ((> cur-indent (+ prev-indent yaml-indent-offset))
           (if (region-active-p)
               (indent-rigidly beg end (- computed cur-indent))
             (indent-line-to (+ computed yaml-indent-offset))))
          (t
           (if (region-active-p)
               (indent-rigidly beg end yaml-indent-offset)
             (indent-line-to (+ cur-indent 2)))))
    (setq deactivate-mark nil)))

(setq auto-mode-alist
      (a-assoc
       (a-dissoc auto-mode-alist "\\.\\(e?ya?\\|ra\\)ml\\'")
       "\\.\\(e?ya?\\|ra\\)ml\\'" 'yaml-mode))
