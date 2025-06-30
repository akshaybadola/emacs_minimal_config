(require 'a)
(unless (version< emacs-version "27")
  (use-package magit
    :ensure t
    :defer t)
  (use-package forge
    :ensure t
    :defer t)
  (require 'magit)
  (require 'forge)

  (defun my/magit-process-password-auth-source (key)
    "Use `auth-source-search' to get a password.
A hacked version of `magit-process-password-auth-source'.  See
that for definition of KEY."
    (require 'auth-source)
    (and (string-match "\\`\\(.+\\)@\\([^@]+\\)\\'" key)
         (let* ((user (match-string 1 key))
                (host (match-string 2 key))
                (secret
                 (plist-get
                  (car (or (auth-source-search :max 1 :host host :user user)
                           (auth-source-search :host host :user user)
                           (auth-source-search :max 1 :host key)))
                  :secret)))
           (if (functionp secret)
               (funcall secret)
             secret))))

  (add-to-list 'magit-process-find-password-functions #'my/magit-process-password-auth-source)
  (when my/load-org-git
    (load "~/emacs_config/org-git.el")))
