(defun my/repeat-last-extended-command (&optional pref-arg)
  (interactive "p")
  (let* ((pref-arg (if pref-arg (- pref-arg 1) 0))
         (cmd (nth pref-arg extended-command-history)))
    (if cmd
        (command-execute (intern cmd))
      (user-error "Given index %s out of bounds of command history" pref-arg))))
(global-set-key (kbd "C-x z") #'my/repeat-last-extended-command)


(when (my/install-for 'compact)
  (defun my/org-time-stamp-inactive ()
    (interactive)
    (org-time-stamp '(16) 'inactive))

  (defun my/insert-or-update-time-stamp-at-point ()
    (interactive)
    (if current-prefix-arg
        (util/insert-or-update-time-stamp-at-point)
      (my/org-time-stamp-inactive)))

  (global-set-key (kbd "C-c .") #'my/insert-or-update-time-stamp-at-point)
  (global-set-key (kbd "C-c <") #'my/insert-or-update-today))
