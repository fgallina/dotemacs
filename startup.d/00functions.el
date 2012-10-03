;; By Fabián Ezequiel Gallina
(defun copy-line ()
  "Copy line from point"
  (interactive)
  (save-excursion
    (let ((start (point-marker)))
      (end-of-line)
      (kill-ring-save start (point-marker)))))

;; http://www.emacswiki.org/emacs/BackwardDeleteWord
(defun delete-word (arg)
  "Delete characters forward until encountering the end of a word.
With argument, do this that many times."
  (interactive "p")
  (delete-region (point) (progn (forward-word arg) (point))))

;; http://www.emacswiki.org/emacs/BackwardDeleteWord
(defun backward-delete-word (arg)
  "Delete characters backward until encountering the end of a word.
With argument, do this that many times."
  (interactive "p")
  (delete-word (- arg)))

;; http://www.emacswiki.org/emacs/JorgenSchaefersEmacsConfig
;; Credits to Jorgen Schaefers
(defun insert-date (prefix)
  "Insert the current date. With prefix-argument, use ISO format. With
   two prefix arguments, write out the day and month name."
  (interactive "P")
  (let ((format (cond
                 ((not prefix) "%d.%m.%Y")
                 ((equal prefix '(4)) "%Y-%m-%d")
                 ((equal prefix '(16)) "%A, %d. %B %Y"))))
    (insert (format-time-string format))))

(defun su ()
  "Reopen current file as root"
  (interactive)
  (when buffer-file-name
    (find-alternate-file
     (concat "/su::" buffer-file-name))))


(defun sudo ()
  "Reopen current file as sudoer"
  (interactive)
  (when buffer-file-name
    (find-alternate-file
     (concat "/sudo:root@localhost:" buffer-file-name))))

;; http://www.emacswiki.org/emacs/DefaultKillingAndYanking
(defun yank-pop-backwards ()
  "Yank backwards"
  (interactive)
  (yank-pop -1)
  (message "yanking backwards"))

;; Replace regex all buffers (query) By Fabián Ezequiel Gallina
(defun query-replace-regexp-all-buffers (regexp replace)
  "Runs query replace regexp in all open buffers"
  (interactive
   (list
    (read-string "Regexp to replace in all buffers: " "" nil "")
    (read-string "Replace: " "" nil "")))
  (let ((buffers ()))
    ;; Get all the buffers we are interested in
    (dolist (buffer (buffer-list))
      (if (and (not (string= (substring (buffer-name buffer) 0 1) " "))
               (not (null (buffer-file-name buffer))))
          (add-to-list 'buffers buffer)))
    ;; Run query replace in all buffers...
    (save-excursion
      (dolist (buffer buffers)
        (save-restriction
          (widen)
          (beginning-of-buffer)
          (query-replace-regexp regexp replace)
          (switch-to-buffer buffer))))))

;; http://www.emacswiki.org/emacs/InsertFileName
(defun insert-file-name (arg filename)
  "Insert name of file FILENAME into buffer after point.
  Set mark after the inserted text.

  Prefixed with \\[universal-argument], expand the file name to
  its fully canocalized path.

  See `expand-file-name'."
  ;; Based on insert-file in Emacs -- ashawley 2008-09-26
  (interactive "*P\nfInsert file name: ")
  (if arg
      (insert (expand-file-name filename))
    (insert filename)))
