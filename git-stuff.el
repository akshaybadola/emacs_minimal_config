(require 'a)
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

(defun org-git-forge-repo ()
  "Get `forge-repository' from context.
In `org-mode', the repository is obtained from the GITHUB_URL
property for the org entry. In `magit-mode' and its derivatives
obtained with `forge'."
  (cond ((and (eq major-mode 'org-mode)
              (org-entry-get (point) "GITHUB_URL" t))
         (forge-get-repository (org-entry-get (point) "GITHUB_URL" t)))
        ((string-match-p "^magit.*-mode$" (symbol-name major-mode))
         (forge-get-repository t))
        (t nil)))

(defun org-git-forge--label-names (repo)
  (mapcar (lambda (x) (nth 1 x)) (oref repo labels)))

(defun org-git-forge-parse-labels (repo)
  (let* ((labels (org-git-forge--label-names repo))
         (tags (-concat (org-get-tags)
                        (and (org-get-todo-state)
                             (list (downcase (substring-no-properties (org-get-todo-state)))))))
         (not-in-labels (-difference tags labels))
         (valid-labels (-intersection tags labels)))
    valid-labels))


(defun my/remove-useless-stuff-from-org-git-export-buffer ()
  ;; operate on current buffer
  (let ((state-re (concat "^ *- +State *\"" org-todo-regexp))
        (notes-re "^ *- +\\[note\\]"))
    (util/org-remove-list-items-matching-re-from-buffer
     (string-join (list state-re notes-re) "\\|"))
    (util/org-remove-subtrees-matching-re "Notes" t)))

(defvar org-git-export-preprocess-hooks nil
  "Hooks to run before exporting the org content as gfm.")

(add-to-list 'org-git-export-preprocess-hooks #'my/remove-useless-stuff-from-org-git-export-buffer)

(defun org-git-export-entry (buf-string)
  "Export the current org entry as `gfm'."
  (with-current-buffer (get-buffer-create "*org-git-export-buffer*")
    (erase-buffer)
    (org-mode)
    (insert buf-string)
    (goto-char (point-min))
    (run-hooks 'org-git-export-preprocess-hooks)
    (let ((org-export-filter-timestamp-functions
           (list #'my/empty-filter))
          (org-export-with-toc nil)
          (org-export-with-clocks nil)
          (org-export-with-properties nil)
          (org-export-with-todo-keywords nil)
          (org-export-with-date nil)
          (org-export-with-tags nil)
          (org-export-with-timestamps nil)
          (org-export-with-drawers nil))
      (org-export-as 'gfm))))

(defun org-git-forge-convert-and-post-issue (&optional prompt view)
  "Export current `org-mode' entry to a `forge-issue' issue.
Store the issue url received as property ISSUE_URL in the current
org entry. Optional PROMPT and VIEW are not used."
  (interactive)
  (when (eq major-mode 'org-mode)
    (let* ((repo (org-git-forge-repo))
           (view (or view current-prefix-arg))
           (pt (point))
           (title (s-trim (substring-no-properties (org-get-heading t t t t))))
           (buf-string (save-restriction (org-narrow-to-subtree)
					 (buffer-string)))
           (labels (if repo
                       (org-git-forge-parse-labels repo)
                     (user-error "No forge repo for current entry found.")))
           ;; TODO: Add assignees (maybe)
           body assignees)
      (setq body (org-git-export-entry buf-string))
      (cond ((org-entry-get (point) "ISSUE_URL")
             (if (y-or-n-p "ISSUE_URL already exists. Update? ")
                 ;; CHECK: Should it be update or sync
                 ;;        `convert-and-post' should be update
                 ;;        `sync-from-repo' should be sync
                 (let* ((issue-num (org-git-try-read-issue-num-in-entry))
                        (git-body (if issue-num
                                      (oref (forge-get-issue repo issue-num) body)
                                    (user-error "No issue url for current entry found.")))
                        (org-body body)
                        (tmp-git-buf (make-temp-name "*tmp-org-git"))
                        (tmp-org-buf (make-temp-name "*tmp-org-git")))
                   ;; (with-current-buffer (get-buffer-create tmp-git-buf)
                   ;;   (insert git-body))
                   (with-current-buffer (get-buffer-create tmp-org-buf)
                     (insert org-body)
                     (markdown-mode))
                   ;; TODO: setup ediff
                   ;; (ediff-buffers tmp-git-buf tmp-org-buf)
                   (forge-visit (forge-get-issue repo issue-num))
                   ;; NOTE: For now basically look at the exported text on the
                   ;;       side and edit yourself, LOL.
                   (let ((win (util/get-or-create-window-on-side)))
                     (set-window-buffer win tmp-org-buf)))
               (user-error "Aborted")))
            ((s-blank-p title)
             (user-error "Title is required"))
            (t
             (when (and prompt (cl-typep repo 'forge-repository))
               (when-let ((-labels (magit-completing-read-multiple*
                                    "Labels: "
                                    (mapcar #'cadr (oref repo labels))
                                    nil t
                                    (mapconcat #'car (closql--iref topic 'labels) ","))))
                 (setq labels (-concat -labels labels))))
             (forge--ghub-post repo "/repos/:owner/:repo/issues"
               `((title . ,title)
                 (body  . ,body)
                 ,@(and labels (list (cons 'labels labels)))
                 ,@(and assignees (list (cons 'assignees assignees))))
               :host (oref repo apihost)
               ;; NOTE: Changed from org-git-forge--issue-post-submit-callback
               :callback  (-partial #'org-git-forge--submit-message repo
                                    (list (cons 'buffer (current-buffer))
                                          (cons 'pt pt))
                                    view)
               :errorback #'org-git-forge--post-submit-errorback))))))

(defun org-git-forge-set-issue-url (buf pt url &optional heading)
  (with-current-buffer buf
    (goto-char pt)
    (org-set-property "ISSUE_URL" url)
    (when (string-match "^https://api.github.com/repos/\\(.+?\\)/issues/\\([0-9]+\\)$" url)
      (org-set-property
       "BROWSER_ISSUE_URL"
       (format "https://github.com/%s/issues/%s" (match-string 1 url) (match-string 2 url))))))

(defun org-git-forge-insert-browser-issue-url ()
  (interactive)
  (let ((url (org-entry-get (point) "ISSUE_URL")))
    (when url
      (string-match "^https://api.github.com/repos/\\(.+?\\)/issues/\\([0-9]+\\)$" url)
      (org-set-property
       "BROWSER_ISSUE_URL"
       (format "https://github.com/%s/issues/%s" (match-string 1 url) (match-string 2 url))))))

(defun org-git-forge--submit-message (repo org view &rest args)
  "Callback after successful `org-forge-convert-and-post-issue'.
Because of the unstable nature of `forge--update-issue', this
function only messages that the issue was posted and inserts the
issue url into the org entry ORG.

REPO is a type of `forge-repository', ORG is an alist of org
buffer and point and VIEW is whether to pull the REPO and view
the issue post submit."
  (pcase-let* ((`(,value ,headers ,status ,req) args)
               (url (a-get value 'url))
               (buf (a-get org 'buffer))
               (pt (a-get org 'pt)))
    (org-git-forge-set-issue-url buf pt url)
    (when view
      (forge--zap-repository-cache repo)
      (forge-pull repo)
      (forge-visit
       (forge-get-issue repo
                        (string-to-number (-last-item (split-string url "/"))))))))

(defun org-git-forge--issue-post-submit-callback (repo org view &rest args)
  "Callback after successful `org-forge-convert-and-post-issue'.
REPO is a type of `forge-repository', ORG is an alist of org
buffer and point and VIEW is whether to view the issue post
submit.

This function is given as a partial function with fixed REPO, ORG, VIEW.

ARGS are added by the callback."
  (pcase-let* ((`(,value ,headers ,status ,req) args)
               (url (a-get value 'url))
               (buf (a-get org 'buffer))
               (pt (a-get org 'pt))
               (fixed-value (mapcar
                             (-compose
                              (lambda (x) (cond ((eq 'labels (car x))
                                                 `(labels . ,(list (a-assoc (cadr x) 'id
                                                                            (a-get (cadr x) 'node_id)))))
                                                ((and (eq 'comments (car x)) (= (cdr x) 0))
                                                 `(comments . ,nil))
                                                ((eq 'state (car x)) `(state . ,(upcase (cdr x))))
                                                ((eq 'user (car x)) `(author . ,(cdr x)))
                                                (t x)))
                              (lambda (x) (if (string-match-p "_" (symbol-name (car x)))
                                              `(,(intern (string-inflection-camelcase-function
                                                          (symbol-name (car x))))
                                                . ,(cdr x))
                                            x)))
                             value)))
    (org-git-forge-set-issue-url buf pt url)
    (forge--update-issue repo fixed-value t)
    (forge--zap-repository-cache repo)
    (when view
      (forge-visit
       (forge-get-issue repo
                        (string-to-number (-last-item (split-string url "/"))))))))

(defun org-git-update-issue (&optional issue-num)
  "Update issue number ISSUE-NUM for a repo.
The repo is obtained by `org-git-forge-repo'"
  (interactive)
  (let ((repo (org-git-forge-repo))
        (issue-num (or issue-num (read-number "Issue Number: "))))
    (ghub-fetch-issue (oref repo owner)
                      (oref repo name)
                      issue-num
                      (lambda (data)
                        (forge--update-issue repo data t)
                        (forge--zap-repository-cache repo)))))

(defun magit-refresh-zapping-cache ()
  (interactive)
  (forge--zap-repository-cache)
  (magit-refresh))

(defun org-git-forge--post-submit-errorback (error &rest _)
  (error "Failed to submit post: %S" error))

;; TODO: Sync issue back to org
;;       - Either sync from git to org, or org to git depending on which is newer.
;;       - Maybe give user options to check the diff before syncing.
;;       - In any case I'll have to use pandoc
(defun org-git-forge-sync-issue ())

(defun org-git-forge-delete-issue ()
  ;; Delete from github and archive in org
  )

(defun org-git-forge-occur-all-issues ()
  (interactive)
  (save-restriction
    (let ((case-fold-search nil))
      (org-occur "https://.*github.com.*issues/.+")
      ;; (occur ts-regexp)
      ;; (other-window 1)
      ;; (read-only-mode -1)
      ;; (goto-char 0)
      ;; (forward-line)
      ;; NOTE: This may not be generally valid but We use this as the time stamp
      ;;       is formatted YYYY-MM-DD first for us.
      ;;       Can use `util/time-stamp-less-p'
      ;;       I think the sorting mechanism may also have to change in that case.
      ;; (sort-regexp-fields -1 "^.*$" ts-regexp (point) (buffer-end 1))
      ;; (read-only-mode)
      )))

(defun org-git-forge-close-issue ()
  ;; close the issue in remote without visiting
  )


(defun org-git-try-read-issue-num-in-entry ()
  (when (org-entry-get (point) "ISSUE_URL")
    (string-to-number
     (-last-item
      (split-string
       (org-entry-get (point) "ISSUE_URL") "/" t)))))

(defun org-git-forge-get-issue-details (repo issue-num)
  "Get issue details for a forge-repository REPO and issue number ISSUE-NUM."
  (ghub-graphql (format "query {
      repository(owner:\"%s\", name:\"%s\") {
        issue(number:%s) {
        author {login}
        milestone {id}
        assignees (first:10) {edges {node {name}}}
        comments (first:20) {edges {node {databaseId author {login} createdAt updatedAt body}}}
        title id url number state createdAt updatedAt closedAt locked body
      }
     }
    }" (oref repo owner) (oref repo name) issue-num)))

(defun org-git-forge-goto-repo ()
  "Visit the magit buffer for forge repo at point"
  (interactive)
  (let ((repo (org-git-forge-repo)))
    (if repo
        (magit-status (oref repo worktree))
      (user-error "Repo %s not found" (org-entry-get (point) "GITHUB_URL" t)))))

(defun org-git-forge-visit-issue ()
  "Visit the issue for current entry if possible."
  (interactive)
  (when (eq major-mode 'org-mode)
    (let* ((repo (org-git-forge-repo))
           (issue-num (org-git-try-read-issue-num-in-entry)))
      (if issue-num (forge-visit (forge-get-issue repo issue-num))
        (user-error "No issue url for current entry found.")))))

(defun org-git-get-remote-hash (&optional remote short)
  "Return the hash of the current commit"
  (nth 1 (split-string
          (car (split-string
                (shell-command-to-string
                 (format "git show -s %s" (if short "--abrev-commit" "")))
                "\n" t)))))

(defun org-git-get-hash (&optional short)
  "Return the hash of the current commit"
  (nth 1 (split-string
          (car (split-string
                (shell-command-to-string
                 (format "git show -s %s" (if short "--abrev-commit" "")))
                "\n" t)))))
