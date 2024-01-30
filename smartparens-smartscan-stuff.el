(require 'org)
(require 'org-element)

(use-package smartscan
  :ensure t
  :defer t)

(require 'smartscan)

(use-package smartparens
  :ensure t
  :defer t)

(require 'smartparens)
(smartparens-global-mode t)

(global-smartscan-mode 1)
(setq smartscan-use-extended-syntax t)
(setq smartscan-symbol-selector "symbol")
;; disable smartscan in comint
(defun my/disable-smartscan ()
  (smartscan-mode -1))
(add-hook 'comint-mode-hook 'my/disable-smartscan)

(require 'smartparens-config)
(smartparens-global-mode t)
(sp-with-modes '(markdown-mode gfm-mode)
  (sp-local-pair "$" "$")
  (sp-local-pair "$$" "$$"))

(sp-with-modes 'org-mode
  (sp-local-pair "\"" "\"" :unless '(sp-point-after-word-p) :post-handlers '(("[d1]" "SPC")))
  (sp-local-pair "$" "$" :unless '(sp-point-after-word-p) :post-handlers '(("[d1]" "SPC")))
  (sp-local-pair "$$" "$$" :unless '(sp-point-after-word-p) :post-handlers '(("[d1]" "SPC")))
  (sp-local-pair "\\[" "\\]" :unless '(sp-point-after-word-p) :post-handlers '(("[d1]" "SPC")))
  (sp-local-pair "+" "+" :unless '(sp-point-after-word-p sp-in-math-p) :post-handlers '(("[d1]" "SPC")))
  (sp-local-pair "=" "=" :unless '(sp-point-after-word-p sp-in-math-p) :post-handlers '(("[d1]" "SPC")))
  (sp-local-pair "_" "_" :unless '(sp-point-after-word-p sp-in-math-p) :post-handlers '(("[d1]" "SPC")))
  (sp-local-pair "*" "*" :unless '(sp-point-after-word-p sp-in-math-p) :post-handlers '(("[d1]" "SPC")))
  (sp-local-pair "\\left(" "\\right)"
                 :trigger "\\l(" :post-handlers '(("SPC")))
  (sp-local-pair "\\left[" "\\right]"
                 :trigger "\\l[" :post-handlers '(("SPC")))
  (sp-local-pair "\\left\\{" "\\right\\}"
                 :trigger "\\l{" :post-handlers '(("SPC")))
  (sp-local-pair "\\left|" "\\right|"
                 :trigger "\\l|" :post-handlers '(("SPC"))))
(setq sp-autoinsert-pair t)

(sp-with-modes 'rst-mode
  (sp-local-pair ":" ":")
  (sp-local-pair "<" ">"))

(setq sp-ignore-modes-list '(minibuffer-inactive-mode))

(sp-with-modes 'python-mode
  (sp-local-pair ":" ":" :when '(sp-in-docstring-p)))
