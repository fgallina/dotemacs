;; http://www.emacswiki.org/emacs/JorgenSchaefersEmacsConfig
;; Credits to Jorgen Schaefers
(defun insert-date (prefix)
  "Insert the current date.
With PREFIX, use ISO format.  With two PREFIX arguments, write
out the day and month name."
  (interactive "P")
  (let ((format (cond
                 ((not prefix) "%d.%m.%Y")
                 ((equal prefix '(4)) "%Y-%m-%d")
                 ((equal prefix '(16)) "%A, %d. %B %Y"))))
    (insert (format-time-string format))))

(defun su ()
  "Reopen current file as root."
  (interactive)
  (when buffer-file-name
    (find-alternate-file
     (concat "/su::" buffer-file-name))))


(defun sudo ()
  "Reopen current file as sudoer."
  (interactive)
  (when buffer-file-name
    (find-alternate-file
     (concat "/sudo:root@localhost:" buffer-file-name))))

;; http://www.emacswiki.org/emacs/DefaultKillingAndYanking
(defun yank-pop-backwards ()
  "Yank backwards."
  (interactive)
  (yank-pop -1))

;; Modified to work with positives prefix args.
;; http://www.emacswiki.org/emacs/InsertFileName.
(defun insert-file-name (filename &optional arg)
  "Insert name of file FILENAME into buffer after point.

  With \\[universal-argument] ARG <= 1, insert filename's
  relative path.  See `file-relative-name' for details.

  With 1 < \\[universal-argument] ARG <= 4, insert filename's
  fully canocalized path.  See `expand-file-name'.

  With \\[universal-argument] ARG > 4, insert the file name
  exactly as it appears in the minibuffer prompt."
  ;; Based on insert-file in Emacs -- ashawley 20080926
  (interactive "*fInsert file name: \np")
  (cond ((<= 1 arg)
         (insert (file-relative-name filename)))
        ((<= 4 arg)
         (insert (expand-file-name filename)))
        (t
         (insert filename))))
