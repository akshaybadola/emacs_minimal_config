(defalias 'assg 'assoc-default)

(global-set-key [f12] 'toggle-truncate-lines)

(setq select-active-regions nil)
(setq save-interprogram-paste-before-kill nil)
(setq x-select-enable-clipboard t)
(setq x-select-enable-primary t)

(setq-default default-truncate-lines t
              truncate-partial-width-windows nil
              cache-long-line-scans t          ; And the variable that caches long lines
              ;; Kill saves to primary selection even from keyboard and updates it
              select-active-regions nil
              save-interprogram-paste-before-kill nil
              select-enable-clipboard t
              select-enable-primary t
              ;; Disable menu bar, tool bar and scroll-bar
              menu-bar-mode nil
              tool-bar-mode nil
              scroll-bar-mode nil
              ;; imenu settings
              imenu-max-items 100
              imenu-max-item-length 120
              ;; For some reason it doesn't work after desktop restore
              indent-tabs-mode nil
              fill-column 80
              show-trailing-whitespace t
              shell-file-name "/bin/bash"
              print-length nil
              print-level 16
              eval-expression-print-level 16
              eval-expression-print-length nil
              case-fold-search t)

(setq indent-tabs-mode nil
      fill-column 80
      show-trailing-whitespace t
      print-length nil
      print-level 16
      eval-expression-print-length nil
      eval-expression-print-level 16
      edebug-print-length 1000
      edebug-print-level 50
      case-fold-search t)

;; save bookmarks after every edit
(setq bookmark-save-flag 1)

;; (setq request-backend (quote url-retrieve))
(setq scroll-bar-mode (quote right))
(show-paren-mode t)

(setq ido-everywhere t)
(setq ido-max-directory-size 100000)
(ido-mode 'both)

; Use the current window when visiting files and buffers with ido
(setq ido-default-file-method 'selected-window)
(setq ido-default-buffer-method 'selected-window)
(setq ido-enable-flex-matching t)

;; org 9 ido or actually ido everywhere enhancement?
(setq ido-ubiquitous-mode t)
(global-set-key (kbd "M-g s") 'ido-goto-symbol)

(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)

;; Redirect all gppg pin entries to remacs
(setf epg-pinentry-mode 'loopback)

;; Use electric quoting
(setq electric-quote-string t)

(unless window-system (global-set-key (kbd "C-x ;") #'comment-line))
