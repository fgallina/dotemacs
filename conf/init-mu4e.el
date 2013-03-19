(require 'mu4e-actions)
(require 'mu4e-headers)

(defvar my:mu4e-last-read-account "")

(defun my:mu4e-minibuffer-read-account (&optional account-list)
  "Read account name from minibuffer."
  (let ((account-list
         (or account-list
             (mapcar
              #'(lambda (var)
                  (car var))
              my:mu4e-account-alist))))
    (setq
     my:mu4e-last-read-account
     (completing-read
      (format "Compose with account: (%s) "
              (mapconcat
               #'(lambda (acc)
                   (if (not (string= acc my:mu4e-last-read-account))
                       acc
                     (format "[%s]" acc)))
               account-list "/"))
      account-list nil t nil nil my:mu4e-last-read-account))))

(defun my:mu4e-get-account (&optional msg account)
  "Get mail account.
When MSG is non-nil, its :MAILDIR is extracted and used for
account detection.  When ACCOUNT is non-nil is checked against
the available list of accounts, if it happens to not match any of
these it will use MSG or ask for one using completing read."
  (let* ((maildir)
         (account-list
          (mapcar
           #'(lambda (var)
               (car var))
           my:mu4e-account-alist))
         (account
          (cond ((and account (car (member account account-list))))
                ((and msg
                      (setq maildir (mu4e-msg-field msg :maildir))
                      (string-match
                       (concat
                        "/\\(" (regexp-opt
                                (mapcar 'regexp-quote account-list)) "\\)/?")
                       maildir)
                      (match-string-no-properties 1 maildir)))
                (t (my:mu4e-minibuffer-read-account account-list)))))
    (message account)))

(defun my:mu4e-set-folder (var msg)
  "Set account folder VAR using MSG as detection element."
  (let ((varval
         (assoc
          var (cdr
               (assoc (my:mu4e-get-account msg)
                      my:mu4e-account-alist)))))
    (if varval
        (set var (cdr varval))
      (mu4e-error "Account not found"))))

;; Inspired from: https://github.com/wunki/wunki-dotfiles
(defun my:mu4e-set-account ()
  "Set the account for composing by looking at the maildir."
  (interactive)
  (let* ((msg (or mu4e-compose-parent-message
                  (ignore-errors
                    (mu4e-message-at-point))))
         (account (my:mu4e-get-account msg))
         (account-vars (cdr (assoc account my:mu4e-account-alist))))
    (when account-vars
      (mapc #'(lambda (var)
                (set (car var) (cdr var)))
            account-vars))
    (message "Using account %s" account)))

(defun my:smtpmail-set-account ()
  "Set the account for msmtp.
This hack extracts the account from `my:mu4e-account-alist' by
searching the alist for a match on the email given in the 'from'
field.  Note that all msmtp accounts should defined in the
~/.msmtprc file and names should be matching those of the
`my:mu4e-account-alist'."
  (setq message-sendmail-extra-arguments
        (list
         "-a"
         (catch 'exit
           (let* ((from (message-fetch-field "from"))
                  (email (and from
                              (string-match thing-at-point-email-regexp from)
                              (replace-regexp-in-string
                               "[<>]" ""
                               (match-string-no-properties 0 from)))))
             (if email
                 (dolist (alist my:mu4e-account-alist)
                   (when (string= email (cdr (assoc 'user-mail-address (cdr alist))))
                     (throw 'exit (car alist))))
               (catch 'exit (my:mu4e-minibuffer-read-account))))))))

(setq mu4e-sent-folder (apply-partially #'my:mu4e-set-folder 'mu4e-sent-folder)
      mu4e-drafts-folder (apply-partially #'my:mu4e-set-folder 'mu4e-draft-folder)
      mu4e-trash-folder (apply-partially #'my:mu4e-set-folder 'mu4e-trash-folder)
      mu4e-refile-folder (apply-partially #'my:mu4e-set-folder 'mu4e-refile-folder)
      mu4e-attachment-dir (expand-file-name "~/Downloads")
      mu4e-compose-complete-addresses t
      mu4e-confirm-quit nil
      mu4e-get-mail-command "offlineimap -o"
      mu4e-headers-date-format "%b, %d %Y. %H:%M"
      mu4e-headers-fields '((:maildir . 12)
                            (:human-date . 12)
                            (:flags . 6)
                            (:mailing-list . 10)
                            (:from . 20)
                            (:subject . nil))
      mu4e-headers-leave-behavior 'apply
      mu4e-html2text-command nil
      mu4e-headers-include-related t
      mu4e-maildir (expand-file-name "~/Maildir")
      mu4e-org-contacts-file (expand-file-name "~/Org/contacts.org")
      mu4e-sent-messages-behavior 'delete
      mu4e-use-fancy-chars t
      mu4e-view-image-max-width 800
      mu4e-view-show-addresses t
      mu4e-view-show-images t
      mail-user-agent 'mu4e
      message-kill-buffer-on-exit t
      message-send-mail-function 'message-send-mail-with-sendmail
      message-sendmail-envelope-from 'header
      sendmail-program (executable-find "msmtp"))

(add-to-list 'mu4e-headers-actions '("org-contact-add" . mu4e-action-add-org-contact) t)
(add-to-list 'mu4e-view-actions '("org-contact-add" . mu4e-action-add-org-contact) t)
(add-hook 'mu4e-compose-pre-hook 'my:mu4e-set-account)
(add-hook 'message-send-mail-hook 'my:smtpmail-set-account)


;;; Vars that need to be customized.

(defvar my:mu4e-account-alist nil
  "Alist containing all information of email accounts.
Here's an example two accounts:

    '((\"account-1\"
       (user-mail-address . \"user1@server.com\")
       (mu4e-sent-folder . \"/account-1/Sent Mail\")
       (mu4e-drafts-folder . \"/account-1/Drafts\")
       (mu4e-refile-folder . \"/account-1/Archive\")
       (mu4e-trash-folder . \"/account-1/Trash\"))
      (\"account-2\"
       (user-mail-address . \"user2@server.com\")
       (mu4e-sent-folder . \"/account-2/Sent Mail\")
       (mu4e-drafts-folder . \"/account-2/Drafts\")
       (mu4e-refile-folder . \"/account-2/Archive\")
       (mu4e-trash-folder . \"/account-2/Trash\")))

In this example account-1 and account-2 match the accounts
defined in ~/.offlineimaprc and ~/.msmtprc")


;;; You should set this stuff.

;; (setq user-full-name "Your username"
;;       user-mail-address "your default email address")
